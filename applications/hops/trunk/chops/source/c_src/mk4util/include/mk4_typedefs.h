#ifndef MK4_TYPEDEF			/* Allow multiple includes */
#define MK4_TYPEDEF

typedef struct sky_coord
    {
    short       ra_hrs;                 /* Self-explanatory */
    short       ra_mins;
    float       ra_secs;
    short       dec_degs;
    short       dec_mins;
    float       dec_secs;
    } sky_coord_struct;

typedef struct date
    {
    short	year;
    short	day;
    short	hour;
    short	minute;
    float	second;
    } date_struct;

typedef struct
    {
    int		whatever;
    /* ?????? */
    } modpoly;


typedef unsigned char       U8;
typedef unsigned short int U16;
typedef unsigned int U32;
// changed following declaration for use on 64 bit systems rjc 2007.2.9
// typedef unsigned long  int U32;

#endif
