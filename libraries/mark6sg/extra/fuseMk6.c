/***************************************************************************
 *   Copyright (C) 2014 by Jan Wagner                                      *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//
// fuseMk6 [-v] [-r \"pattern\"] <mountpoint>
//
// Example program for the mark6sg library. Allows normal file access to
// scatter-gather recordings on a module.
//
///============================================================================

#include <mark6sg.h>

#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <memory.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/inotify.h>
#endif

#define FUSE_USE_VERSION 26
#include <fuse.h>

#define MAX_DISKS       32  // maximum number of disks mounted under /mnt/disks/, same as the max. number of files per Mark6 SG scan
#define USE_JSON_INFOS   1  // non-zero if JSON metadata infos should be used (these are: scan create time, approximate scan size in bytes)
#define USE_FILESIZE_EST 1  // non-zero if total length of file fragments should be used as scan size in bytes

//////////////////////////////////////////////////////////////////////////////////
// Local
//////////////////////////////////////////////////////////////////////////////////

static gid_t m_gid = 1000;
static uid_t m_uid = 1000;

// Directory entries
static char**       m_scannamelist;
static struct stat* m_scanstatlist;
static int          m_nscans;
static m6sg_slistmeta_t* m_json_scanlist;
static const char*  m_root;

static pthread_mutex_t dirlock = PTHREAD_MUTEX_INITIALIZER;

void usage(void)
{
	printf("\n"
		"Mark6 Scatter-Gather data interpretation layer   v1.13  Jan Wagner 15032016\n"
		"\n"
		"Usage: fuseMk6 [-v] [-r \"pattern\"] <mountpoint>\n"
		"\n"
		"Provides access to Mark6 scatter-gather (SG) recordings as single sequential files.\n"
		"The scattered data are gathered exactly as recorded. FuseMk6 does not perform\n"
		"reordering of individual VDIF frames on the fly. If reordering is helpful, sometimes\n"
		"the case for multi-threaded VDIF recordings, consider 'mark6gather' or 'vdifuse'.\n"
		"\n"
		"Each Mark6 SG recording consists of VLBI scan data striped across multiple\n"
		"files on separate disks of a module. Mark6 modules can be auto-mounted by the\n"
		"mk5daemon service that is part of DiFX. FuseMk6 relies on the 'libmark6sg' library\n"
		"to access the data in the files sequentially and presents them as a single file.\n\n"
		"Options:\n"
		"   -v    verbose mode (puts fuseMk6 into 'foreground' mode),\n"
		"         repeat to increase verbosity\n"
		"   -r    set root pattern (default: \"%s\")\n\n"
		"Environment variables:\n"
		"    MARK6_ROOT       search path for Mark 6 data, with wildcards\n"
		"    MARK6_META_ROOT  path containing Mark 6 metadata\n\n",
		MARK6_SG_ROOT_PATTERN
	);
}

//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : reading of scatter-gather files via the "mark6sg" libary
//////////////////////////////////////////////////////////////////////////////////

static void *fusem6_init(struct fuse_conn_info *conn)
{
	struct fuse_context *ctx = fuse_get_context();
	if (NULL != ctx)
	{
		m_gid = ctx->gid;
		m_uid = ctx->uid;
	}
	return NULL;
}

static int fusem6_open(const char *path, struct fuse_file_info *fi)
{
	int rc, i;
	struct stat st;

	if ((fi->flags & O_RDONLY) != O_RDONLY)
		return -EACCES;

	rc = mark6_sg_open(path, O_RDONLY);
	fi->fh = (uint64_t)rc;
	if (rc < 0)
		return -errno;

#if 1	// During mark6_sg_open() the actual VLBI data length is calculated.
        // Even the Mark6 JSON metadata (if used) does not have the actual correct file size.
	// We should thus update the FUSE file info here accordingly with the now known *true* data length.
	rc = mark6_sg_fstat(fi->fh, &st);
	pthread_mutex_lock(&dirlock);
	for (i=0; i<m_nscans; i++)
	{
		if (strcmp(path+1, m_scannamelist[i]) == 0)
		{
			m_scanstatlist[i].st_size = st.st_size;
			m_scanstatlist[i].st_blksize = st.st_blksize;
		}
	}
	pthread_mutex_unlock(&dirlock);
#endif

	// Invent a "MSN" label for DifxMessageMark6Activity messages,
	// to distinguish rate reports when multiple fuseMk6 mounts are active
	// Note: "MSN" must have strlen() of 8, else difxmessage reverts MSN to "none"
	// TODO: somehow determine actual MSN(s) list of the accessed module(s),
	//       or the original MSN of off-module raw data
	char fakeMSN[9] = { '\0' };
	snprintf(fakeMSN, sizeof(fakeMSN), "fuse%04d", getpid() % 10000);
	mark6_sg_active_msn(fi->fh, fakeMSN);

	return 0;
}

static int fusem6_create(const char *path, mode_t mode_ignored, struct fuse_file_info *fi)
{
	int rc, framesize;
	char *scanname, *s;

        // Check that the scan does not exist
	rc = mark6_sg_open(path, O_RDONLY);
	if (rc >= 0) {
		mark6_sg_close(rc);
		return -EEXIST;
	}

	// Check if scan name contains frame size hint (e.g., "/path/name.vdif:8032")
	scanname = strdup(path);
	if ((s = strrchr(scanname, ':')) != NULL) {
		framesize = atoi(s+1);
		*s = '\0';
		if ((framesize > 64) && (framesize < 16*1024*1024)) {
			mark6_sg_packetsize_hint(framesize);
		}
		printf("fusem6_creat(): filename %s had trailing frame size hint for %d bytes\n", scanname, framesize);
	}

	// Create
	rc = mark6_sg_creat(path, mode_ignored); // note: use 'path' instead of 'scanname' since 'dd' and others expect filename doesn't change during create()...
	if (rc < 0) {
		printf("fusem6_creat(): create of %s failed\n", path);
		perror("mark6_sg_creat");
		free(scanname);
		return -errno;
	}
	free(scanname);

	// Set infos
	fi->fh = (uint64_t)rc;
	fi->nonseekable = 1;

	return 0;
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
	//if (nrd != size)
	//	printf("pread()=%ld != req=%lu\n", nrd, size);
	pthread_mutex_unlock(&rdlock);

	return (nrd > 0) ? nrd : 0;
}

static int fusem6_write(const char *path, const char *buf, size_t size, off_t offset,
		      struct fuse_file_info *fi)
{
	ssize_t nwr;
	nwr = mark6_sg_write(fi->fh, buf, size);
	return (nwr > 0) ? nwr : 0;
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
	dir_st.st_uid  = m_uid;
	dir_st.st_gid  = m_gid;
	dir_st.st_nlink = 2 + m_nscans;

	/* No directories except root */
	if (strcmp(path, "/") != 0)
	{
		return -ENOENT;
	}

	/* Add directory entries */
	filler(buf, ".",  &dir_st, 0);
	filler(buf, "..", &dir_st, 0);
	pthread_mutex_lock(&dirlock);
	for (i=0; i<m_nscans; i++)
	{
		if (filler(buf, m_scannamelist[i], &m_scanstatlist[i], 0) != 0)
		{
			pthread_mutex_unlock(&dirlock);
			return -ENOMEM;
		}
	}
	pthread_mutex_unlock(&dirlock);

	return 0;
}

static int fusem6_getattr(const char *path, struct stat *stbuf)
{
	int i;
	memset(stbuf, 0, sizeof(struct stat));

	/* Root directory */
	if (strcmp(path, "/") == 0) {
		memset(stbuf, 0, sizeof(struct stat));
		stbuf->st_uid   = m_uid;
		stbuf->st_gid   = m_gid;
		stbuf->st_ctime = time(NULL);
		stbuf->st_mode  = S_IFDIR | 0555;
		stbuf->st_nlink = 2 + m_nscans;
		stbuf->st_size  = 4096;
		stbuf->st_blksize = 512;
		for (i=0; i<m_nscans; i++)
		{
			stbuf->st_blocks += m_scanstatlist[i].st_size / stbuf->st_blksize;
		}
		return 0;
	}

	/* Files in root directory */
	pthread_mutex_lock(&dirlock);
	for (i=0; i<m_nscans; i++)
	{
		if (strcmp(path+1, m_scannamelist[i]) == 0)
		{
			memcpy(stbuf, &m_scanstatlist[i], sizeof(struct stat));
			stbuf->st_blksize = 512;
			stbuf->st_blocks = stbuf->st_size / stbuf->st_blksize;
			pthread_mutex_unlock(&dirlock);
			return 0;
		}
	}
	pthread_mutex_unlock(&dirlock);

	return -ENOENT;
}

//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : scan list refresh helper
//////////////////////////////////////////////////////////////////////////////////

void fusem6_make_scanlist(void)
{
	int rc = 0, i = 0;
	m6sg_slistmeta_t* json_slist_ptr;

	pthread_mutex_lock(&dirlock);

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
		m_scanstatlist[i].st_uid   = m_uid;
		m_scanstatlist[i].st_gid   = m_gid;
		m_scanstatlist[i].st_atime = t0;
		m_scanstatlist[i].st_mtime = t0;
		m_scanstatlist[i].st_ctime = t0;
		m_scanstatlist[i].st_mode  = 0444 | S_IFREG;
		m_scanstatlist[i].st_size  = 0;
	}

	/* Augment the scan information using the Mark6 JSON metadata files */
#if (USE_JSON_INFOS)
	rc = mark6_sg_collect_metadata(&m_json_scanlist);
	printf("Found %d scans, and %d entries in JSON\n", m_nscans, rc);
	json_slist_ptr = m_json_scanlist;
	for (i=0; i<rc && json_slist_ptr!=NULL; i++)
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

#if (USE_FILESIZE_EST)
	for (i=0; i<m_nscans; i++)
	{
		off_t totalsize = 0, nfragments;
		char **filepathlist, **filenamelist;
		int j;

		nfragments = mark6_sg_filelist_from_name(m_scannamelist[i], &filepathlist, &filenamelist);

		for (j=0; j<nfragments; j++)
		{
			struct stat st;
			if (stat(filepathlist[j], &st) == 0)
			{
				// Rough guess of total payload size
				totalsize += st.st_size * (9998760.0/(9998760.0 + 4.0)); // 4-byte SG Version 2 Block Header + default payload
				totalsize -= 5*4; // SG File Header
			}
			free(filepathlist[j]);
			free(filenamelist[j]);
		}
		if (NULL != filepathlist) { free(filepathlist); }
		if (NULL != filenamelist) { free(filenamelist); }
		if (totalsize < 0) { totalsize = 0; }

		m_scanstatlist[i].st_size = totalsize;
	}
#endif

	pthread_mutex_unlock(&dirlock);

	return;
}

#ifdef __linux__
void *fusem6_dir_watcher(void* thread_arg)
{
	char buf[1024*(sizeof(struct inotify_event) + 16)];
	ssize_t nrd;
	int fd, rc, i;
	glob_t g;

	/* Use Linux inotify API to listen for changes in Mark6 directories */
	fd = inotify_init();
	rc = glob(m_root, GLOB_MARK|GLOB_BRACE|GLOB_ONLYDIR, NULL, &g);
	if (rc != 0)
	{
		printf("fusem6_dir_watcher: glob failed, rc=%d\n", rc);
		close(fd);
		return NULL;
	}

	for (i = 0; i < g.gl_pathc; i++)
	{
		struct stat st;
		stat(g.gl_pathv[i], &st);
		if (S_ISDIR(st.st_mode))
		{
			inotify_add_watch(fd, g.gl_pathv[i], IN_CLOSE_WRITE|IN_CREATE|IN_DELETE|IN_DELETE_SELF);
		}
	}
	globfree(&g);

	/* Receive inotify events and refresh our scan list */
	while (1)
	{
		nrd = read(fd, buf, sizeof(buf));
		if (nrd < 0)
		{
			close(fd);
			return NULL;
		}
		printf("inotify: Mark6 files changed, refreshing the scan list\n");
		fusem6_make_scanlist();
	}
}
#endif

//////////////////////////////////////////////////////////////////////////////////
// FUSE Layer : entry point
//////////////////////////////////////////////////////////////////////////////////

static struct fuse_operations fusem6_oper = {
	.init		= fusem6_init,
	.open		= fusem6_open,
	.create		= fusem6_create,
	.release	= fusem6_release,
	.read		= fusem6_read,
	.write		= fusem6_write,
	.getattr	= fusem6_getattr,
	.readdir	= fusem6_readdir
};

int main(int argc, char *argv[])
{
	int rc = 0, verbosity = 0, i = 0;
	int fuse_argc;
	char** fuse_argv;
	char* mountpoint = NULL;
	pthread_t tid;

	/* Arguments */
	mountpoint = argv[argc-1];
	if (getenv("MARK6_ROOT") != NULL)
	{
		m_root = strdup(getenv("MARK6_ROOT"));
		mark6_sg_set_rootpattern(m_root);
	}
	if (getenv("MARK6_META_ROOT") != NULL)
	{
		mark6_sg_set_metapath(getenv("MARK6_META_ROOT"));
	}
	for (i=1; i<argc; i++)
	{
		if (strcmp(argv[i], "-v") == 0)
		{
			verbosity++;
		}
		else if ((strcmp(argv[i], "-h") == 0) || (strcmp(argv[i], "--help") == 0))
		{
			usage();
			return -1;
		}
		else if ((strcmp(argv[i], "-r") == 0) && ((i+1) < argc))
		{
			m_root = strdup(argv[i+1]);
			mark6_sg_set_rootpattern(m_root);
			i++;
		}
		else
		{
			break;
		}
	}
	m_root = mark6_sg_get_rootpattern();

	if (((argc-i) != 1) || (mountpoint[0] == '-'))
	{
		usage();
		return -1;
	}

	/* Set library verbosity */
	mark6_sg_verbositylevel(verbosity);

	/* Get scan list from Mark6sg library and update it with JSON and file fragment data */
	fusem6_make_scanlist();
	if (m_nscans <= 0)
	{
		printf("No scans found. Exiting.\n");
		return -1;
	}

#ifdef __linux__
	/* Start a thread to refresh the scan list when files change */
	pthread_create(&tid, NULL, fusem6_dir_watcher, NULL);
#endif

	/* Start FUSE */
	fuse_argc = 0;
	fuse_argv = malloc(16*sizeof(char**));
	fuse_argv[fuse_argc++] = argv[0];
	fuse_argv[fuse_argc++] = "-oallow_other";
	fuse_argv[fuse_argc++] = "-odirect_io";
	fuse_argv[fuse_argc++] = "-oattr_timeout=0";
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
