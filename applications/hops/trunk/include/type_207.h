#ifndef T207_VERSION
#define T207_VERSION 2


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_207_v2 type_207

struct sbandf
    {
    float               lsb;
    float               usb;
    };

struct type_207_v0 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct sbandf       ref_pcamp[16];          /* Phasecal amp for ref station */
    struct sbandf       rem_pcamp[16];          /* Phasecal amp for rem station */
    struct sbandf       ref_pcphase[16];        /* Phasecal phase for ref station */
    struct sbandf       rem_pcphase[16];        /* Phasecal phase for rem station */
    struct sbandf       ref_pcfreq[16];         /* Phasecal freq for ref station */
    struct sbandf       rem_pcfreq[16];         /* Phasecal freq for rem station */
    float               ref_pcrate;             /* Phasecal rate for ref station */
    float               rem_pcrate;             /* Phasecal rate for rem station */
    float               ref_errate[16];         /* Mean error rate for ref station */
    float               rem_errate[16];         /* Mean error rate for rem station */
    };

struct type_207_v1
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    int                 pcal_mode;              /* 10*ant1+ant2, meanings in control.h */
    int                 unused2;                /* padding */
    struct sbandf       ref_pcamp[16];          /* Phasecal amp for ref station */
    struct sbandf       rem_pcamp[16];          /* Phasecal amp for rem station */
    struct sbandf       ref_pcphase[16];        /* Phasecal phase for ref station */
    struct sbandf       rem_pcphase[16];        /* Phasecal phase for rem station */
    struct sbandf       ref_pcoffset[16];       /* Phasecal offset for ref station */
    struct sbandf       rem_pcoffset[16];       /* Phasecal offset for rem station */
    struct sbandf       ref_pcfreq[16];         /* Phasecal freq for ref station */
    struct sbandf       rem_pcfreq[16];         /* Phasecal freq for rem station */
    float               ref_pcrate;             /* Phasecal rate for ref station */
    float               rem_pcrate;             /* Phasecal rate for rem station */
    float               ref_errate[16];         /* Mean error rate for ref station */
    float               rem_errate[16];         /* Mean error rate for rem station */
    };

struct type_207
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    int                 pcal_mode;              /* 10*ant1+ant2, meanings in control.h */
    int                 unused2;                /* padding */
    struct sbandf       ref_pcamp[64];          /* Phasecal amp for ref station */
    struct sbandf       rem_pcamp[64];          /* Phasecal amp for rem station */
    struct sbandf       ref_pcphase[64];        /* Phasecal phase for ref station */
    struct sbandf       rem_pcphase[64];        /* Phasecal phase for rem station */
    struct sbandf       ref_pcoffset[64];       /* Phasecal offset for ref station */
    struct sbandf       rem_pcoffset[64];       /* Phasecal offset for rem station */
    struct sbandf       ref_pcfreq[64];         /* Phasecal freq for ref station */
    struct sbandf       rem_pcfreq[64];         /* Phasecal freq for rem station */
    float               ref_pcrate;             /* Phasecal rate for ref station */
    float               rem_pcrate;             /* Phasecal rate for rem station */
    float               ref_errate[64];         /* Mean error rate for ref station */
    float               rem_errate[64];         /* Mean error rate for rem station */
    };

#endif
