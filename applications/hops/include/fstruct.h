#ifndef FSTRUCT_DONE
#define FSTRUCT_DONE

#define MAXFILES 500000

#define BADSTRING 0x01
#define BADFORM   0x02
#define BADBASE   0x04
#define BADFNUM   0x08
#define BADFREQ   0x10
#define BADSRC    0x20
#define BADROOT   0x40
#define BADSTAT   0x80


#ifndef FALSE
#define TRUE 1
#define FALSE 0
#endif

typedef struct
    {
    int order;		/* For sorting operations */
    char *name;		/* Full name of file */
    short type;		/* File type according to name 0...4 */
    char source[32];	/* Source name (root files only) */
    char baseline[3];	/* Baseline id (fringe and corel files only) */
    char station;	/* Type 3 (pcal) files only */
    char freq_code;	/* Frequency id code (fringe files only) */
    int filenum;	/* File number in fileset */
    char rootcode[7];	/* root id code 'abcdef' */
    short done;		/* Flag to indicate that this file has been done already */
    int intparm[4];	/* Auxiliary info if needed */
    float floatparm[4];	/* Auxiliary info if needed */
    } fstruct;

extern void clear_fstruct();

#endif

