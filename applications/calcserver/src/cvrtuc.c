/*  @(#)cvrtuc.c  version 1.5  created 95/11/24 08:09:20
    %% functions to convert string upper or lower case
    LANGUAGE: C
    ENVIRONMENT: Any
*/

/* includes */

/*******************************************************************************
*/
char *cvrtuc		/* convert to upper case */
    (
    char *s		/* pointer to string to be converted */
    )
/*
 * RETURNS original string pointer 
 *
 * This function converts its input string to all upper case.  The input string 
 * must be terminated by '\0'.
 */ 
{
    char *pRtn = s;

    while (*s != '\0')
	{
        if ((*s >= 'a') && (*s <= 'z'))
	    *s += ('A'-'a');
	s++;
	}

    return (pRtn);
}

/*******************************************************************************
*/
char *cvrtlc		/* convert to lower case */
    (
    char *s		/* pointer to string to be converted */
    )
/*
 * RETURNS original string pointer 
 *
 * This function converts its input string to all lower case.  The input string 
 * must be terminated by '\0'.
 */
{
    char *pRtn = s;

    while (*s != '\0')
        {
        if ((*s >= 'A') && (*s <= 'Z'))
            *s += ('a' - 'A');
        s++;
        }

    return (pRtn);
}
