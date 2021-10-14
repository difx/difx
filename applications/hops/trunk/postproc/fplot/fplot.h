#define NONE     0
#define XWINDOW  1
#define HARDCOPY 2
#define PRINTLPR 3
#define GSDEVICE 4
#define DISKFILE 5
#define PSTOPDF  6

extern int parse_cmdline (
    int argc, char** argv, fstruct** files, int* display, char** file_name);
extern void msg (char *, int, ...);
#ifdef MK4_DATA
extern char display_221 (struct type_221 *, int);
#endif /* MK4_DATA */
