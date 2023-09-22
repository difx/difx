/************************************************************************/
/*									*/
/* Given a string enclosed in quotes, this routine strips the quotes	*/
/* and substitutes escaped characters according to the VEX rules, which	*/
/* are in fact just the normal C-language rules for characters		*/
/*									*/
/*	Inputs:		qstring		Quoted string, including quotes	*/
/*									*/
/*	Output:		string		Processed string		*/
/*									*/
/* Created 12 November 1998 by CJL, ideas and code borrowed from WEH	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "mk4_vex.h"
#include "mk4_util.h"

#define isodigit(x) ((x) >= '0' && (x) <= '7')
#define hextoint(x) (isdigit((x)) ? (x) - '0'\
                                  : ((x) - 'A') + 10)

int
process_qstring (char *qstring,
                 char *string)
    {
    int i, j, len, temp;
    char c, str[256], outstr[256];
					/* If this is not quoted string, */
					/* we just return */
    if (qstring[0] != '\"') return (0);
					/* Validate string and strip quotes */
    len = strlen (qstring);
    if (len >= 256)
	{
	msg ("String passed to process_qstring() is too long", 2);
	return (-1);
	}
    if (qstring[len-1] != '\"')
	{
	msg ("Invalid quoted string '%s' passed to process_qstring()", 2,
		qstring);
	return (-1);
	}
    len = len - 2;
    strncpy (str, qstring+1, len);
    str[len] = '\0';

    j = 0;
    for (i=0; i<len; i++)
	{
	c = str[i];
	if (c == '\\')
	    {
	    i++;
	    c = str[i];
	    switch (c)
		{
		case 'a':
		    c = '\a';
		    break;
		case 'b':
		    c = '\b';
		    break;
		case 'v':
		    c = '\v';
		    break;
		case 'f':
		    c = '\f';
		    break;
		case 'r':
		    c = '\r';
		    break;
		case '\\':
		    c = '\\';
		    break;
		case '\"':
		    c = '\"';
		    break;
		case '\?':
		    c = '\?';
		    break;
		case '\'':
		    c = '\'';
		    break;
		case 'o':
		    i++;
		    c = str[i];
		    if (isodigit (c))
			{
			temp = c - '0';
			i++;
			c = str[i];
			if (isodigit (c))
			    {
			    temp = (temp << 3) + (c - '0');
			    i++;
			    c = str[i];
			    if (isodigit (c)) temp = (temp << 3) + (c - '0');
			    else i--;
			    }
			else i--;
			}
		    else
			{
			msg ("Illegal '\\o' specifier in quoted string", 2);
			return (-1);
			}
		    c = temp;
		    break;
		case 'x':
		    i++;
		    c = str[i];
		    if (isxdigit (c))
			{
			temp = hextoint (toupper (c));
			i++;
			c = str[i];
			if (isxdigit (c))
			    temp = (temp << 4) + hextoint (toupper (c));
			else i--;
			}
		    else
			{
			msg ("Illegal '\\x' specifier in quoted string", 2);
			return (-1);
			}
		    c = temp;
		    break;
		default:
		    msg ("Unrecognized or illegal escape in quoted string", 2);
		    return (-1);
		}
	    }

	outstr[j] = c;
	j++;
	}
    outstr[j] = '\0';

    strcpy (string, outstr);

    return (0);
    }
