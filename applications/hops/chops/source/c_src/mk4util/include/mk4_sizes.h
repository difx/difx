/**************************************************************************
*                                                                         *
*  global size file for the Mark IV correlator on-line software           *
*                                                                         *
**************************************************************************/

#ifndef MK4_SIZES
#define MK4_SIZES

                                        /* maximum sizes of various strings */
                                        
#define MAX_LEN_KEY     32              /* maximum key length (incl. null) */
#define MAX_LEN_FILNAM 128              /*   "  filename length (full path, w/ null */
#define MAX_LEN_ERR    128              /* max text length in Error_Message   */



                                        /* various hardware limits */
                                        
#define MAX_NUM_SU      16              /* max # of SU's */
#define MAX_NUM_STN     32              /* max # of stations within a task */
#define MAX_NUM_TAPES   32              /* max # of tape request lines */

#define MAX_CHAN       128              // max # data channels (= difx bands)
#define MAX_POLY_CHAN   16              // maximum number of polynomials

#define MAXFREQ         64              // max # of freq channels

#endif

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif
