/**********************************************************************/
int             sindex (s, t)
    char            s[];	       /* The character string to be searched */
    char            t[];	       /* The character string to search for  */
 /* Returns character position within string */
/**********************************************************************/

/*
%% Finds the position of one string within another
---
  Taken from K & R p67

LANGUAGE: C
ENVIRONMENT: Versatek
:: parse string
++$ AUDIT TRAIL
1.0  86nov30 R.T. Duquet  Initial Submission
--$
-*/

{
    int             i, j, k;

    for (i = 0; s[i] != '\0'; i++) {
	for (j = i, k = 0; t[k] != '\0' && s[j] == t[k]; j++, k++);
	if (t[k] == '\0')
	    return (i);
    }
    return (-1);
}
