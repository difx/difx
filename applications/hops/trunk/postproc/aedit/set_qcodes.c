/********************************************************************************/
/*                                                                              */
/* This routine parses the user's typed quality code selection.  Two            */
/* formats are accepted.  They are a simple string of quality codes, and        */
/* a range specified by "A-B" where A is lower than B in the (arbitrary)        */
/* quality code "sequence".  Any unrecognized character causes failure.         */
/* Any number of fields can be entered - they are ORed together to produce      */
/* a final list which goes into the input structure as a simple string          */
/* of consecutive codes.  There is an option NOT, as the first argument,        */
/* which simply means select all except the specified codes.                    */
/*                                                                              */
/*      Inputs          arg1,arg2,remarg        User input                      */
/*                                                                              */
/*      Output:         inp.qcodes              List of acceptable codes        */
/*                                                                              */
/* Created April 5 1989 by CJL                                                  */
/*                                                                              */
/********************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int set_qcodes(char *arg1, char *arg2, char *remarg)
{
        extern struct inputs inp;
        int i, j, k, n, not, lq, first, last;
	char *p1, *p2;
        char c, qlist[20], buf[50], *string, *strtok(), allargs[200], allq[20];

        sprintf(allq,"ABCDEFGH0123456789");
        qlist[0] = '\0';

        n = strlen(arg1);
        if(n == 0) {                    /* Check for blank (means clear) */
            inp.qcodes[0] = '\0';
            return(0);
        }
                                        /* Check for "not" string in 1st arg */
        for(i=0;i<n;i++) {              /* Convert to lower case */
            c = arg1[i];
            if(isupper(c)) c = tolower(c);
            buf[i] = c;
        }
        buf[n] = '\0';
        if(strcmp(buf,"not") == 0) {
            not = TRUE;
            sprintf(allargs,"%s,%s",arg2,remarg);       /* Concatenate strings */
        }
        else {
            not = FALSE;
            sprintf(allargs,"%s,%s,%s",arg1,arg2,remarg);
        }
        n = strlen(allargs);
        for(i=0;i<n;i++) {              /* Convert to upper case */
            c = allargs[i];
            if(islower(c)) c = toupper(c);
            allargs[i] = c;
        }
            
        string = strtok(allargs," ,");  /* Parse "," or " " separated fields */
        while(TRUE) {
            if(strchr(string,'-') != NULL) {    /* Range ... check it carefully */
                if(strlen(string) != 3) {       /* A-B is 3 chars */
                    msg("Bad argument %s",2,string);
                    return(-1);
                }
                else if(string[1] != '-') {     /* Malformed */
                    msg("Misplaced '-' character",2);
                    return(-1);
                }                               /* Check for valid codes */
                else if((p1=strchr(allq,string[0])) == NULL ||  
                                (p2=strchr(allq,string[2])) == NULL) {
                    msg("Unrecognized qcodes in '%s'",2,string);
                    return(-1);
                }                               /* Bad order */
                else if(p1 > p2) {
                    msg("Qcode %c comes after %c!",2,string[0],string[2]);
                    return(-1);
                }
                else {                          /* OK, we do it */
                    lq = strlen(qlist);
                    j = 0; first = FALSE; last = FALSE;
                    for(j=0;j<strlen(allq);j++) {
                        if(allq[j] == string[0]) first = TRUE;
                        if(strchr(qlist,allq[j]) == NULL && first && (! last)) {
                            qlist[lq] = allq[j];
                            qlist[++lq] = '\0';
                        }
                        if(allq[j] == string[2]) last = TRUE;
                    }
                }
            }
            else {                              /* Simple list */
                lq = strlen(qlist);
                j = 0;
                while((c = string[j++]) != '\0') {
                    if(strchr(allq,c) == NULL) {
                        msg("Unrecognized qcode %c in '%s'",2,c,string);
                        return(-1);
                    }
                    else if(strchr(qlist,c) == NULL) {
                            qlist[lq] = c;
                            qlist[++lq] = '\0';
                    }
                }
            }
            if((string = strtok(NULL," ,")) == NULL) break;     /* Ends loop */
        }

        j = 0; k = 0;
        if(not) {                               /* Invert code list */
            while((c=allq[j++]) != '\0') {
                if(strchr(qlist,c) == NULL) inp.qcodes[k++] = c;
            }
            inp.qcodes[k] = '\0';
        }
        else strcpy(inp.qcodes,qlist);
        return(0);
}
