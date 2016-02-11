#define MAXFREQ 64                      /* Max number of VC frequencies allowed */
#define MAXBANDS 10                     /* Max number of wavebands allowed */
#define MAXGROUPS 20			/* Max number of freq groups in ccfile */
#define MAXSTEXP 40                     /* Max stations allowed per experiment */
#define MAXSTTOT 40                     /* Max stations allowed throughout data */
#define MAXSTBAND 16                    /* Max stations allowed in 1 band */
#define MAXEXPTS 40                     /* Max number of experiments allowed */
#define MAXSRC 500                      /* Max number of sources allowed */
#define MAXVERSION 5                    /* Highest A-format version # supported */


#define MAXBASE ((MAXSTEXP * (MAXSTEXP - 1)) / 2)       /* Baseline limit */
#define MAXCLOSE  ((MAXBASE * (MAXSTEXP - 2)) / 3)      /* Closure phase limit */
