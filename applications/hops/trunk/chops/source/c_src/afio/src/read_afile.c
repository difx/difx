//Wed Dec  7 12:39:47 EST 2016  jpb
//based off of read_data.c in the aedit postproc library
//this function is only used to fill a struct with afile data
//so it can be read out into python through the ctypes library


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include "adata.h"
#include "afile_structure.h"

extern void clear_afile_structure(afile_structure*);

int read_afile(char *filename, afile_structure* adata_element)
{
    FILE *fp;
    struct stat statbuf;
    int i, nfilt, bad, nkbr, nkbc, nkbf, nkbt, nkbq, year_ok, pret, frac, ck[12];
    int nadded[5], expand, type;
    char line[512];

    if(stat(filename,&statbuf) != 0)
    {
        if(errno == ENOENT) msg("File '%s' does not exist",2,filename);
        else msg("Problem accessing file '%s'",2,filename);
        return(-1);
    }

    if((fp=fopen(filename,"r")) == NULL)
    {
        msg("Problem opening '%s'",2,filename);
        return(-1);
    }

    //create and init our adata structure
    //we assume all memory it points to has already been freed or is unalloc'ed
    clear_afile_structure(adata_element);

    //first scan the file and determine the number of lines of each type
    while(fgets(line,511,fp) != NULL)
    {
        if (afile_comment(line)) continue;	/* Ignore comment lines */

        /* What type of line is this? */
        if (isdigit (line[0])) sscanf (line, "%*d %*s %d", &type);
        else sscanf (line, "%*s %d", &type);
        type %= 50;				/* In case of old format */
        if ((type < 0) || (type > 4))		/* Better be 0, 1, 2, 3 or 4 */
        {
            msg ("Found line with incomprehensible format, stopping.",2);
            break;
        }
        //count the stuctures
        switch (type)
        {
            case 0:
            adata_element->nroot++;
            break;

            case 1:
            adata_element->ncorel++;
            break;

            case 2:
            adata_element->nfringe++;
            break;

            case 3:
            adata_element->ntriangle++;
            break;

            case 4:
            adata_element->nquad++;
        }
    }

    //now malloc the space we need
    if(adata_element->nroot != 0){ adata_element->rootdata = malloc( (adata_element->nroot)*sizeof(rootsum) ); };
    if(adata_element->ncorel != 0){ adata_element->coreldata = malloc( (adata_element->ncorel)*sizeof(corelsum) ); };
    if(adata_element->nfringe != 0){ adata_element->fringedata = malloc( (adata_element->nfringe)*sizeof(fringesum) ); };
    if(adata_element->ntriangle != 0){ adata_element->triangledata = malloc( (adata_element->ntriangle)*sizeof(trianglesum) ); };
    if(adata_element->nquad != 0){ adata_element->quaddata = malloc( (adata_element->nquad)*sizeof(quadsum) ); };

    //reset the file pointer back to the begin
    rewind(fp);

    msg("Reading data from file '%s' ...",2,filename);

    for (i=0; i<5; i++){ nadded[i] = 0; };

    while(fgets(line,511,fp) != NULL)
    {
        if (afile_comment(line)) continue;	/* Ignore comment lines */

        /* What type of line is this? */
        if (isdigit (line[0])) sscanf (line, "%*d %*s %d", &type);
        else sscanf (line, "%*s %d", &type);
        type %= 50;				/* In case of old format */
        if ((type < 0) || (type > 4))		/* Better be 0, 1, 2, 3 or 4 */
        {
            msg ("Found line with incomprehensible format, stopping.",2);
            break;
        }
        /* call proper parser */
        /* Remember to clear elements of */
        /* aedit array structures */
        switch (type)
        {
            case 0:
            pret = parse_rsumm(line, &( (adata_element->rootdata)[ nadded[type] ]) );
            nadded[type]++;
            break;

            case 1:
            pret = parse_csumm(line, &( (adata_element->coreldata)[ nadded[type] ]) );
            nadded[type]++;
            break;

            case 2:
            pret = parse_fsumm(line, &( (adata_element->fringedata)[ nadded[type] ]) );
            nadded[type]++;
            break;

            case 3:
            pret = parse_tsumm(line, &( (adata_element->triangledata)[ nadded[type] ]) );
            nadded[type]++;
            break;

            case 4:
            break;
            //amp closure not working...
            //why is the following line commented out in the aedit read_data function?
            /*		pret = parse_qsumm (line, &((data->qdata)[qscan].data));  */
            //aeclr_quad( &( (adata_element->quaddata)[ nadded[type] ]) );
            //nadded[type]++;
        }
    }				/* End of main read loop */

    fclose(fp);

    return 0;
}
