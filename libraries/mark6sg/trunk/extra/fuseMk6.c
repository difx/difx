#define FUSE_USE_VERSION 26

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <memory.h>
#include <stdlib.h>
#include <pthread.h>

#include <mark6sg.h>

#define MAX_DISKS       32  // maximum number of disks mounted under /mnt/disks/, same as the max. number of files per Mark6 SG scan
#define USE_JSON_INFOS   1  // non-zero if JSON metadata infos should be used (these are: scan create time, approximate scan size in bytes)

void usage(void)
{
	printf("\n"
		"Mark6 Scatter-Gather data interpretation layer   v1.10  Jan Wagner 24062015\n"
		"\n"
		"Usage: fuseMk6 [-v] <mountpoint>\n"
		"\n"
		"Presents Mark6 scatter-gather mode (SG) recordings as single files.\n"
		"The SG disks are assumed to be already mounted (/mnt/disks/[1-4]/[0-7]/)\n"
		"\n"
		"The Mark6 SG recording mode stripes data of a VLBI recording across multiple\n"
                "files, each of them generally placed on its own disk or file system.\n"
		"These SG files contain metadata and the actual VLBI data striped out in a\n"
		"somewhat random time order. The fuseMk6 layer uses the 'libmark6sg' library\n"
                "to hide the SG striping and metadata.\n"
		"\n"
                "Options:\n"
                "   -v    verbose mode (puts fuseMk6 into 'foreground' mode),\n"
                "         repeat to increase verbosity\n\n"
	);
}


//////////////////////////////////////////////////////////////////////////////////
// Local
//////////////////////////////////////////////////////////////////////////////////

// Directory entries
static char**       m_scannamelist;
static struct stat* m_scanstatlist;
static int          m_nscans;
static m6sg_slistmeta_t* m_json_scanlist;


//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : reading of scatter-gather files via the "mark6sg" libary
//////////////////////////////////////////////////////////////////////////////////

static int fusem6_open(const char *path, struct fuse_file_info *fi)
{
	int rc, i;
	struct stat st;

	if ((fi->flags & O_RDONLY) != O_RDONLY)
		return -EACCES;

	rc = mark6_sg_open(path, O_RDONLY);
	fi->fh = (uint64_t)rc;
	if (rc < 0)
		return -EACCES;

#if 1	// During mark6_sg_open() the actual VLBI data length is calculated.
        // Even the Mark6 JSON metadata (if used) does not have the actual correct file size.
	// We should thus update the FUSE file info here accordingly with the now known *true* data length.
	rc = mark6_sg_fstat(fi->fh, &st);
	for (i=0; i<m_nscans; i++)
	{
		if (strcmp(path+1, m_scannamelist[i]) == 0)
		{
			memcpy(&m_scanstatlist[i], &st, sizeof(struct stat));
		}
	}
#endif

	return fi->fh;
}

static int fusem6_release(const char *path, struct fuse_file_info *fi)
{
	return mark6_sg_close(fi->fh);
}

static int fusem6_read(const char *path, char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
	ssize_t nrd;
	static pthread_mutex_t rdlock = PTHREAD_MUTEX_INITIALIZER;

	pthread_mutex_lock(&rdlock);
	nrd = mark6_sg_pread(fi->fh, buf, size, offset);
	if (nrd != size)
		printf("pread()=%ld != req=%lu\n", nrd, size);
	pthread_mutex_unlock(&rdlock);

	return (nrd > 0) ? nrd : 0;
}


//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : directory representation of scatter-gather filesets, non-trivial
//////////////////////////////////////////////////////////////////////////////////

// Simplest implementation:
//    We have only a root directory (<mountpoint>/), no subdirectories,
//    and all scans are listed in the root directory.

static int fusem6_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
			 off_t offset, struct fuse_file_info *fi)
{
	int i;

	struct stat dir_st;
	dir_st.st_mode = 0555 | S_IFDIR;
	dir_st.st_uid  = 1000;
	dir_st.st_gid  = 1000;
	dir_st.st_nlink = 2 + m_nscans;

	/* No directories except root */
	if (strcmp(path, "/") != 0)
	{
		return -ENOENT;
	}

	/* Add directory entries */
	filler(buf, ".",  &dir_st, 0);
	filler(buf, "..", &dir_st, 0);
	for (i=0; i<m_nscans; i++)
	{
		filler(buf, m_scannamelist[i], &m_scanstatlist[i], 0);
	}
	return 0;
}

static int fusem6_getattr(const char *path, struct stat *stbuf)
{
	int i;

	/* Root directory */
	if (strcmp(path, "/") == 0) {
		memset(stbuf, 0, sizeof(struct stat));
		stbuf->st_uid   = 1000;
		stbuf->st_gid   = 1000;
		stbuf->st_ctime = time(NULL);
		stbuf->st_mode  = S_IFDIR | 0555;
		stbuf->st_nlink = 2 + m_nscans;
		return 0;
	}

	/* Files in root directory */
	for (i=0; i<m_nscans; i++)
	{
		if (strcmp(path+1, m_scannamelist[i]) == 0)
		{
			memcpy(stbuf, &m_scanstatlist[i], sizeof(struct stat));
			return 0;
		}
	}

	return -ENOENT;
}


//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : entry point
//////////////////////////////////////////////////////////////////////////////////

static struct fuse_operations fusem6_oper = {
	.open		= fusem6_open,
	.release	= fusem6_release,
	.read		= fusem6_read,
	.getattr	= fusem6_getattr,
	.readdir	= fusem6_readdir
};

int main(int argc, char *argv[])
{
	int rc = 0, verbosity = 0, i = 0;
	int fuse_argc;
	char** fuse_argv;
	char* mountpoint = NULL;
	m6sg_slistmeta_t* json_slist_ptr;

	/* Arguments */
	mountpoint = argv[argc-1];
	for (i=1; i<argc; i++)
	{
		if (strcmp(argv[i], "-v") == 0)
		{
			verbosity++;
		}
	}
	if ((argc < 2) || (mountpoint[0] == '-'))
	{
		usage();
		return -1;
	}

	/* Set library verbosity */
	mark6_sg_verbositylevel(verbosity);

	/* Get scan list from Mark6sg library */
	m_nscans = mark6_sg_list_all_scans(&m_scannamelist);

	/* Prepare entries to be shown in the FUSE root directory */
	m_scanstatlist = malloc(m_nscans*sizeof(struct stat));
	memset(m_scanstatlist, 0, m_nscans*sizeof(struct stat));
	for (i=0; i<m_nscans; i++)
	{
		// NOTE 1: file time stamps are not easily known without
		// inspecting the JSON metadata files (those contain some kind of timestamp) 
		// NOTE 2: file size is not well known until opening all the files associated
		// with the SG recording, although the JSON metadata has an estimate in GByte of
		// the total file size (based on the assumption that no disks failed)
		// Due to the above reasons for now we use fake file size and time...
		time_t t0 = time(NULL);
		m_scanstatlist[i].st_nlink = 1;
		m_scanstatlist[i].st_uid   = 1000;
		m_scanstatlist[i].st_gid   = 1000;
		m_scanstatlist[i].st_atime = t0;
		m_scanstatlist[i].st_mtime = t0;
		m_scanstatlist[i].st_ctime = t0;
		m_scanstatlist[i].st_mode  = 0444 | S_IFREG;
		m_scanstatlist[i].st_size  = 1234;
	}

        /* Augment the scan information using the Mark6 JSON metadata files */
#if (USE_JSON_INFOS)
	rc = mark6_sg_collect_metadata(&m_json_scanlist);
	json_slist_ptr = m_json_scanlist;
	for (i=0; i<rc; i++)
	{
		int j;
		for (j=0; j<m_nscans; j++)
		{
			char* s1 = json_slist_ptr->scanname;  // usually shorter as it has no ".vdif" at the end
			char* s2 = m_scannamelist[j];         // real files end with ".vdif" or other suffix
			int match = strncmp(s1, s2, strlen(s1));
			// printf(" JSON:%s =? dirlist:%s  : %d\n", s1, s2, match);
			if (match == 0)
			{
				m_scanstatlist[j].st_size  = json_slist_ptr->size;
				m_scanstatlist[j].st_atime = json_slist_ptr->starttime;
				m_scanstatlist[j].st_mtime = json_slist_ptr->starttime;
				m_scanstatlist[j].st_ctime = json_slist_ptr->starttime;
				break;
			}
		}
		json_slist_ptr = json_slist_ptr->next;
	}
#endif

	/* Start FUSE */
	fuse_argc = 0;
	fuse_argv = malloc(16*sizeof(char**));
	fuse_argv[fuse_argc++] = argv[0];
	fuse_argv[fuse_argc++] = "-oallow_other";
	fuse_argv[fuse_argc++] = "-odirect_io";
        if (verbosity > 0)
        {
		fuse_argv[fuse_argc++] = "-f";     // foreground mode to show libmark6sg printouts
	}
	if (verbosity > 3)
	{
		fuse_argv[fuse_argc++] = "-d";     // show FUSE debug infos
	}
	fuse_argv[fuse_argc++] = mountpoint;

	rc = fuse_main(fuse_argc, fuse_argv, &fusem6_oper, NULL);
	return rc;
}
