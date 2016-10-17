					/* Operational modes for edit_close() */
#define MODE_TRI     (1<<0)
#define MODE_BASE    (1<<1)

#define DUPLICATE    (1<<0)              /* Bit specifications for flag field of */
#define ZAPPED       (1<<1)              /* data structure */
#define BADQF        (1<<2)
#define BADSNR       (1<<3)
#define BADBSNR      (1<<4)
#define BADTIME      (1<<5)
#define BADSTATION   (1<<6)
#define BADBASELN    (1<<7)
#define BADTRNGL     (1<<8)
#define BADQUAD      (1<<9)
#define BADEXPT      (1<<10)
#define BADFREQA     (1<<11)
#define BADTYPE      (1<<12)
#define BADSOURCE    (1<<13)
#define BADLENGTH    (1<<14)
#define BADFRAC      (1<<15)
#define BADNFREQ     (1<<16)
#define BADPARAM     (1<<17)
#define BADPROC      (1<<18)
#define CHILDLESS    (1<<19)
#define ORPHAN       (1<<20)
#define NO_TRIANGLE  (1<<21)
#define NO_BASELINE  (1<<22)
#define TAGGED       (1<<23)
#define BADPOL       (1<<24)

#define ERROR        (1<<25)

					/* filter ids used by active_filter() */
#define F_TIMETAG    1
#define F_PROCDATE   2
#define F_STATION    3
#define F_BASELINE   4
#define F_TRIANGLE   5
#define F_QUAD       6
#define F_FREQUENCY  7
#define F_EXPERIMENT 8
#define F_QCODE      9
#define F_SOURCE     10
#define F_TYPE       11
#define F_SNR        12
#define F_BSNR       13
#define F_LENGTH     14
#define F_FRACTION   15
#define F_NFREQ      16
#define F_PARAMETER  17
#define F_POLARIZATION  18
