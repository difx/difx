/*****************************************************************************
*  Utility for merging two correlator scans to be parsed by fourfit          *
*                                                                            *
*       Input:  two root files to be merged                                  *
*                                                                            *      
*       Output: new merged root file, as well as type-1 and -3 files         *
*               with current six-letter timestamp suffix                     *
*                                                                            *
*  Written June-July 2009 by T. Cappallo                                     *
*                                                                            *
*  (c) 2009  MIT Haystack Observatory                                        *
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>

#include "fourmer.h"

                                //required by libutil
int msglev = 2;
char progname[] = "fourmer";

                                // from root_id.c: returns
                                //   a six-letter timestamp
char *root_id(int, int, int, int, int);
char* root_id_break(time_t now, int year, int day, int hour, int min, int sec);
                                // TODO: comment
int do_record_merge(char *, char *, char *, char *);


char *field(char *, char *, int);
void merge(int *, int *, int *, const int, const char *);
static void relabel_channels(int, int, int);
static void revise_expn(int);
static char *make_outname(char *, char *, char *);

char lineA[MAX_LINES][MAX_CHARS],       // contents of input file A, by line
     lineB[MAX_LINES][MAX_CHARS],       // contents of input file B, by line
     buffer[MAX_BUFFER][MAX_CHARS];     // buffer in which to merge a section


int main(int argc, char **argv)
    {
    FILE *fileA,
         *fileB,
         *fileOut;
    
    char *rcode;                // new six-letter timecode suffix
    char pwd[MAX_FPATH];        // present working directory
    char fileOutName[MAX_FPATH];// output root file name, generated from
                                // present working directory and new timecode
    
    int numLinesA = -1,         // stores length of file A in lines
        numLinesB = -1,         // stores length of file B in lines
        numBufferLines = 0;     // stores length of buffer in lines
    
    int n = 0,                  // used as line index for file A
        m = 0,                  // used as line index for file B
        i = 0;
    
    int special = 0;            // non-zero indicates a section
                                //   to be merged
    
    int flagged = 0;            // whether a mismatch has been noted
                                //   in current section
    
    char section[30];           // name of current section
    
                                // list of sections to be merged
    const char NOTICE_LIST[5][8] =     {"", 
                                    "$BBC", 
                                    "$FREQ", 
                                    "$EXPER",
                                    "$TRACKS"    };
    
                                // list of keywords to identify
                                //   lines to be merged
    const char sortKey[5][11] =     {"",
                                 "BBC_assign",
                                 "chan_def",
                                 "exper_num",
                                 "fanout_def"    };
    
				// which field to sort by
    const int sortField[5] =     {0, 2, 6, 0, 5    };
    
    if (getcwd (pwd, MAX_FPATH) == NULL)
        {
        msg ("error getting current working directory", 2);
        return (1);
        }


    if (argc != 3 && argc != 4)         // check argument count
        {                           
        printf("Usage: %s <file A> <file B>\n", argv[0]);
        printf(" or    %s <file A> <file B> <msglev>\n\n", argv[0]);
        return 1;
        }
    
    
    fileA = fopen(argv[1], "r");        // open root files for input
    fileB = fopen(argv[2], "r");
    if (!fileA || !fileB)               // bail if problem with an input file
        {
        fprintf(stderr, "ERROR: could not open one or more input files\n");
        return (1);
        }
    if (argc == 4)
        msglev = atoi (argv[3]);

				// and open it for writing
    rcode = make_outname(argv[1], pwd, fileOutName);
    if (!rcode)
	{
	fprintf(stderr, "Unable to generate new root file\n");
	return(2);
	}
    fileOut = fopen(fileOutName, "w");
    
                                // read each input file into its
                                //   appropriate array
    while (fgets(lineA[++numLinesA], MAX_CHARS, fileA))
        lineA[numLinesA][strlen(lineA[numLinesA])-1] = '\0';
    
    while (fgets(lineB[++numLinesB], MAX_CHARS, fileB))
        lineB[numLinesB][strlen(lineB[numLinesB])-1] = '\0';

    msg("Read %d lines from %s", 2, numLinesA, argv[1]);
    msg("read %d lines from %s", 2, numLinesB, argv[2]);
    msg("Writing output rootfile to %s", 2, fileOutName);
    msg("Processing... (any discrepancies noted will"
	" default to version A in output file)\n", 0);
    
    scan_name_edit(lineA, numLinesA, lineB, numLinesB);
                                        
    for (n = 0; n < numLinesA; n++)     // loop through every line in file A
        {
        if (lineA[n][0] == '$')     // wait until we hit a section header
            {   
            flagged = 0;
            fprintf(fileOut, "%s\n", lineA[n]);     // write section header
                                // parse section name from line
            strncpy(section, lineA[n], strchr(lineA[n], ';') - lineA[n]);
            section[strchr(lineA[n], ';') - lineA[n]] = '\0';
            
                                // search file B for matching section
            for (m = 0; m < numLinesB; m++)
                if (!strncmp(lineA[n], lineB[m], MAX_CHARS))
                    break;

                                // if section is in the NOTICE_LIST,
                                //   set special to its index in list
            special = 0;
            for (i = 1; i <= 3; i++)
                if (!strncmp(section, NOTICE_LIST[i], 30))
                    {
                    special = i;
                    break;
                    }
            
                                // loop through each line in section
            for (n++, m++; n < numLinesA; n++, m++)
                {
                if (lineA[n][0] == '$')     // if new section, exit loop
                    {
                    n--;  m--;
                    break;
                    }
                
                                // if current section is in NOTICE_LIST,
                if (special && strstr(lineA[n], sortKey[special]))
                    {
                                // collate the relevant contents
                    merge(&n, &m, &numBufferLines,
                          sortField[special], sortKey[special]);
                    
                                // write our merged section
                    for (i = 0; i < numBufferLines; i++)
                        fprintf(fileOut, "%s\n", buffer[i]);
                    
                    numBufferLines = 0;
                    }
                
                                // otherwise, write line from file A
                                //   into output file; if it doesn't
                                //   match file B, print a warning
                if (strncmp(lineA[n], lineB[m], MAX_CHARS))
                    {
                    if (!flagged)
                        msg("======Discrepancy in %s======", 1, section);
                    msg("  [A:%4d] %s", 1, n+1, lineA[n]);
                    msg("  [B:%4d] %s\n", 1, m+1, lineB[m]);
                    flagged = 1;
                    }
                
                fprintf(fileOut, "%s\n", lineA[n]);
                }
            }
        }
    
                                // cleanup
    fclose(fileA);
    fclose(fileB);
    fclose(fileOut);
    if (getenv("HOPS_FOURMER_ROOT_ONLY")) return(0);
    
                                // write merged type-1 and -3 files
    if ((n = do_record_merge(argv[1], argv[2], fileOutName, rcode)))
        {
        fprintf(stderr, "Error %d processing files.\n", n);
        return n;
        }
    else
        {
        msg("Scans merged successfully.", 2);
        }
    
    return 0;
    }


                                // returns the n'th field when given
                                //   a colon-delimited string
                                //   (n must >= 2)
char *field(char *ret, char *line, int n)
    {
    int i;
    char s[MAX_CHARS];
    
    strcpy(s, line);
    strtok(s, ":");
    for (i = 2; i <= n; i++)
        strcpy(ret, strtok((char *)0, ":"));
    
    return ret;
    }


                                // merges a portion of a section,
                                //   sorting and eliminating duplicates
void merge(int *n, int *m, int *numBufferLines,
           const int sortField, const char *sortKey)
    {
    int i, j;
    int swapped = 1, n0s, nAs, nBs;
    
    char ret[MAX_CHARS],
         temp[MAX_CHARS];
         
                                // read every line with the keyword
                                //   we're looking for into a buffer,
                                //   concatenating A and B
    n0s = *numBufferLines;
    while (strstr(lineA[*n], sortKey))
        strcpy(buffer[(*numBufferLines)++], lineA[(*n)++]);
    nAs = (*numBufferLines);
    while (strstr(lineB[*m], sortKey))
        strcpy(buffer[(*numBufferLines)++], lineB[(*m)++]);
    nBs = (*numBufferLines);

    if (*sortKey == 'c'/*han_def*/)
	relabel_channels(n0s, nAs, nBs);
    if (*sortKey == 'e'/*xper_num*/)
        revise_expn(nBs);

                                // bubble sort the buffer on
                                //   the appropriate field
    if (sortField > 1) while (swapped)
        {
        swapped = 0;
        for (i = 0; i < *numBufferLines-1; i++)
            {
            char a[MAX_CHARS], b[MAX_CHARS];
            strcpy(a, field(ret, buffer[i], sortField));
            strcpy(b, field(ret, buffer[i+1], sortField));
            if (strcmp(a, b) > 0) 
                {
                strcpy(temp, buffer[i]);
                strcpy(buffer[i], buffer[i+1]);
                strcpy(buffer[i+1], temp);
                swapped = 1;
                }
            }
        }
    
                                // eliminate duplicate lines from buffer
    for (i = *numBufferLines-1; i > 0; i--)
        if (!strcmp(buffer[i], buffer[i-1]))
            {
            for (j = i; j < *numBufferLines; j++)
                strcpy(buffer[j-1], buffer[j]);
            (*numBufferLines)--;
            }
        
    return;
    }

/*
 * Do the relabelling on the affected lines in the buffer array.
 */
static void relabel_channels(int n0s, int nAs, int nBs)
    {
    int nn = n0s;
    for ( ; nn < nAs; nn++) relabel_chan_def(buffer[nn], 1);
    for ( ; nn < nBs; nn++) relabel_chan_def(buffer[nn], 0);
    }

/*
 * Do the revision on the experiment number
 */
static void revise_expn(int n0s)
{
    char *eq, *en = getenv("HOPS_FOURMER_EXPN");
    /* fprintf(stderr, "en = %s %d lines\n", en, n0s); */
    if (!en) return;
    while (n0s--) {
        fputs(buffer[n0s], stderr);
        eq = strchr(buffer[n0s], '=');
        if (!eq) continue;
        sprintf(eq, "= %s;", en);
    }
}

/*
 * Generate a new output (root) filename and root code.  Since
 * the old root might be in the current working directory, we
 * want to be careful not to overwrite it.
 */
static char *make_outname(char *rootA, char *pwd, char *rootC)
    {
    time_t now;
    struct tm *t;
    char *pchar;
    char *rcode = 0;            // new six-letter timecode suffix
    struct stat buf;

    while (!rcode)
	{
	now = time((time_t *)NULL);
	t = localtime(&now);    // generate new 6-char rootcode timestamp
	// rcode = root_id(t->tm_year, t->tm_yday+1,
	// 		t->tm_hour, t->tm_min, t->tm_sec);
    rcode = root_id_break(now, t->tm_year, t->tm_yday+1,
			t->tm_hour, t->tm_min, t->tm_sec);
				// rootA & rootC differ by timestamp & path
	pchar = strrchr (rootA, '/');
	if (pchar != NULL)
	    pchar++;            // basename is last component of path
	else
	    pchar = rootA;      // no / in the string

	strcpy (rootC, pwd);
	strcat (rootC, "/");
	strcat (rootC, pchar);
                                // replace old root code with new
	rootC[strlen(rootC) - strlen(rcode)] = 0;
	strcat(rootC, rcode);

	if (0 == stat(rootC, &buf))
	    {
	    msg("Sleeping 4 secs to avoid root file overwrite.", 2);
	    sleep(4);
	    rcode = 0;
	    }
	else if (errno != ENOENT)
	    {
	    perror("stat: make_outname");
	    msg("Some permission problem with %s", -1, rootC);
	    return(0);
	    }
	}

    return(rcode);
    }

/*
 * eof
 */
