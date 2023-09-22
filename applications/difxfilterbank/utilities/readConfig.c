/*****
simple code to extract some essential info from a DiFX configuration (.input) file.

Randall Wayth. 2009/2010. This forms part of the fb_reorder utility.

*****/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "readConfig.h"

#define MAX_LINE 1024

/*****************************
Utility to search the a "config" file for the key part of key-value pair, and return pointer to
string with the value part of the pair. The VLBA config file has colons (:) after the key (or as part
of it), so when the text part of a key is found, advance through any additional non-space chars until
we get to a space. That way we move past colons, equals signs etc. There does have to be white space
between the key and value.

The resulting pointer might have leading white space, but since various atoX functions don't care, i don't.
******************************/
char *findKey(char *config, char *key) {
    char *p;
    // search for the string "key" in the big string "config"
    p = strstr(config,key);
    if (p==NULL) return NULL;

    // found the string, now offset until we find white space
    p += strlen(key);
    if (!isspace(*p)) p++;
    return p;
}


/*****************************
print an error message and exit if a key was not found in the config file
******************************/
void readConfigMissingKeyErr(char *key) {
    fprintf(stderr,"ERROR: key <%s> not found in config file. It is required\n",key);
    exit(EXIT_FAILURE);
}


/*****************************
Load the config file into an array, removing blank lines and comment lines
******************************/
char *loadConfigFile(char *filename) {
    int n_read=0;
    char *config=NULL,*p;
    FILE *fp;
    struct stat fileinfo;

    // load the entire config file into mem, so that we can scan it many times looking for keys
    fp = fopen(filename,"r");
    if (fp==NULL) {
        fprintf(stderr,"loadConfigFile: Cannot open file <%s>\n",filename);
        return NULL;
    }
    // stat the file to get file size
    fstat(fileno(fp),&fileinfo);

    // allocate a buffer to read the entire file, plus one for the string termination
    config = (char *) calloc(fileinfo.st_size + 1,sizeof(char));

    while (!feof(fp)) {
        p = fgets(config+n_read,fileinfo.st_size-n_read+1,fp);

        if (p==NULL || p[0] == '\0' || p[0] == '\n' || p[0] == '#') continue;  // skip blank and comment lines
	
        n_read += strlen(p);
    }

    fclose(fp);     // done with the file now.

    return config;
}


/*****************************
 read a flag file to flag combinations of stream/band/channels.
******************************/
int readFlagsFile(char *filename, FB_Config *fb_config) {
    int stream,band,chan;
    char line[MAX_LINE];
    FILE *fp;
    int returnValue = 0;

    fp = fopen(filename,"r");
    if (fp==NULL) {
        fprintf(stderr,"readFlagsFile: Cannot open file <%s>\n",filename);
        return 1;
    }
    while (fgets(line,MAX_LINE-1,fp)!=NULL) {
        if (line[0] == '\0' || line[0] == '\n' || line[0] == '#') continue;  // skip blank and comment lines

        if(sscanf(line,"ALLCHAN %d",&chan)==1) {
            if (chan < 0 || chan >= fb_config->n_chans) {
                fprintf(stderr,"ERROR: ALLCHAN flag specified for chan %d. Max %d\n",chan,fb_config->n_chans);
                returnValue = 1;
                break;
            }
            fprintf(stderr,"Flagging channel %d on all stream/bands\n",chan);
            // flag the same chan on all streams/bands
            for (stream=0; stream < fb_config->n_streams; stream++) {
                for (band=0; band < fb_config->n_bands; band++) {
                    fb_config->flags[stream][band][chan] = 1;
                }
            }
        }

        if (sscanf(line,"STREAM_BAND %d %d",&stream,&band)==2) {
            if(stream < 0 || stream >= fb_config->n_streams) {
                fprintf(stderr,"ERROR: bad stream id for stream/band flagging: %d. \n",stream);
                returnValue = 1;
                break;
            }
            if(band < 0 || band >= fb_config->n_bands) {
                fprintf(stderr,"ERROR: bad band id for stream/band flagging: %d.\n",band);
                returnValue = 1;
                break;
            }
            fprintf(stderr,"Flagging all chans for stream/band %d/%d\n",stream,band);
            for (chan=0; chan < fb_config->n_chans; chan++) {
                fb_config->flags[stream][band][chan] = 1;
            }
        }

        if (sscanf(line,"STREAM_ALL %d",&stream)==1) {
            if(stream < 0 || stream >= fb_config->n_streams) {
                fprintf(stderr,"ERROR: bad stream id for full-stream flagging: %d.\n",stream);
                returnValue = 1;
                break;
            }
            fprintf(stderr,"Flagging all bands/chans for stream %d\n",stream);
            for(band=0; band<fb_config->n_bands; band++) {
                for(chan=0; chan < fb_config->n_chans; chan++) {
                    fb_config->flags[stream][band][chan] = 1;
                }
            }
        }
        if (sscanf(line,"STREAM_BAND_CHAN %d %d %d",&stream,&band,&chan)==3) {
            if(stream < 0 || stream >= fb_config->n_streams) {
                fprintf(stderr,"ERROR: bad stream id for STREAM_BAND_CHAN: %d.\n",stream);
                returnValue = 1;
                break;
            }
            if(band < 0 || band >= fb_config->n_bands) {
                fprintf(stderr,"ERROR: bad band id for STREAM_BAND_CHAN: %d.\n",band);
                returnValue = 1;
                break;
            }
            if (chan < 0 || chan >= fb_config->n_chans) {
                fprintf(stderr,"ERROR: bad chan for STREAM_BAND_CHAN: %d. Max %d\n",chan,fb_config->n_chans);
                returnValue = 1;
                break;
            }
            fprintf(stderr,"Flagging chan %d, in band %d for stream %d\n",chan,band,stream);
            fb_config->flags[stream][band][chan] = 1;
        }
        if (sscanf(line,"BAND_CHAN %d %d",&band,&chan)==2) {
            if(band < 0 || band >= fb_config->n_bands) {
                fprintf(stderr,"ERROR: bad band id for STREAM_BAND_CHAN: %d.\n",band);
                returnValue = 1;
                break;
            }
            if (chan < 0 || chan >= fb_config->n_chans) {
                fprintf(stderr,"ERROR: bad chan for STREAM_BAND_CHAN: %d. Max %d\n",chan,fb_config->n_chans);
                returnValue = 1;
                break;
            }
            fprintf(stderr,"Flagging chan %d, in band %d for all streamsd\n",chan,band);
            for (stream=0; stream < fb_config->n_streams; stream++) {
                fb_config->flags[stream][band][chan] = 1;
            }
        }
    }
    fclose(fp);
    return returnValue;
}


