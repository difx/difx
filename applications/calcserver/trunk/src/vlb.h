/*  @(#)vlb.h  version 1.15  created 00/03/09 14:43:43
    %% function prototypes for VLB component
    LANGUAGE: C
    ENVIRONMENT: Any
*/


/*************************************************************************/
/*                     EXCLUSIVELY Unix FUNCTIONS                        */
/*************************************************************************/


#ifndef NULL
#define NULL  0
#endif

#ifndef EOF
#define EOF  (-1)
#endif

#ifndef TRUE
#define TRUE  1
#endif

#ifndef FALSE
#define FALSE  0
#endif

#ifndef STD_IN
#define STD_IN  0
#endif

#ifndef STD_OUT
#define STD_OUT  1
#endif

#ifndef STD_ERR
#define STD_ERR  2
#endif

#ifndef FOREVER
#define FOREVER  for(;;)
#endif

#ifndef NONE
#define NONE  (-1)	/* for times when NULL won't do */
#endif

#ifndef EOS
#define EOS   '\0'	/* C string terminator */
#endif

#ifndef NUL
#define NUL   '\0'      /*  nul character, do not confuse with NULL pointer  */
#endif

#ifndef ERROR
#define ERROR  (-1)
#endif

#ifndef OK
#define OK     0
#endif

typedef int BOOL, VOID, STATUS;

/***************************************************/ 
/*  macros for character testing and manipulation  */
/***************************************************/ 

#ifndef isalpha
#define isalpha(c) (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
#define isdigit(c) ( '0' <= c  &&  c <= '9' )
#define islower(c) ( 'a' <= c  &&  c <= 'z' )
#define isupper(c) ( 'A' <= c  &&  c <= 'Z' )
#define iswhite(c) (c == ' ' || c == '\t' || c == '\n')
#define isalnum(c) (isalpha(c) || isdigit(c))
#endif
#ifndef tolower
#define tolower(c) (isupper(c) ? (c - 'A' + 'a') : c)
#define toupper(c) (islower(c) ? (c - 'a' + 'A') : c)
#endif

