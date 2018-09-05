/*******************************************************************************
*                                                                              *
*  lex performs the lexical analysis by analyzing each token in the input      *
*      run string, determining what token it is, and what (if any) its         *
*      value is.                                                               *
*                                                  rjc  92.10.8                *
*******************************************************************************/
#include "parser.h"
#include "general.h"
#include "mk4_sizes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char fchars[64] =
	{'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
     'q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F',
     'G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V',
     'W','X','Y','Z','0','1','2','3','4','5','6','7','8','9','$','%'};

 /* string contains conditioned input from control file */
int lex (char* input_string)
    {
    extern struct token_struct *tokens;  /* output structure of token symbols
                                            and values */
    extern double *float_values;         /* array of actual fl. pt. values    */
    extern char *char_values;            /* pointer to array of actual strings*/
    extern int token_cat[],
               *line_end;
         
    int nold = 1,
        n,
        nchars = 0,
        i_value,
        nfl = 0,
        cv_off = 0,                   /* start with no char strings in buffer */
        itok = 0;

    double f_value;

    char *read_point,
         *next_token;                   /* next token represented as a string */

                            /* initialize to null pointers for realloc's sake */
    tokens         = (struct token_struct *) malloc(1);
    float_values   = (double *) malloc(1);
    char_values    = (char *) malloc(1);

    read_point = input_string;            /* start reading at start of string */

                                           /* get next token, if there is one */
    while ((next_token = strtok (input_string," ")) != NULL)
        {
        input_string = NULL;               /* keep strtok parsing same string */

        tokens = (struct token_struct *) 
                  realloc (tokens, (itok + 1) * sizeof (struct token_struct));

        if (n = is_keyword (next_token))         /* see if token is a keyword */
            {
            if (token_cat[n] < INT_CONST)     
                tokens[itok].symbol = n;             /* just a normal keyword */

            else
                {               /* special integer constant, substitute value */
                tokens[itok].symbol = INT_;
                tokens[itok].value  = token_cat[n] - INT_CONST;
                }
            }

        else if (is_timeval (next_token,&i_value))
            {
            tokens[itok].symbol = TIME_VAL_;
            tokens[itok].value  = i_value;     /* stuff time into value array */
            }

        else if (is_integer (next_token,&i_value))
            {
            tokens[itok].symbol = INT_;
            tokens[itok].value  = i_value; /* stuff integers into value array */
            }

        else if (is_float (next_token,&f_value))
            {
            tokens[itok].symbol = FLOAT_;
            float_values = (double *) realloc (float_values, (nfl+1) * sizeof (double));
            float_values[nfl] = f_value;
            tokens[itok].value  = nfl++;       /* save index into float array */
            }

        else if (is_char (next_token))       /* general string of 1-16 chars? */
            {
            n = strlen (next_token);               /* examine string's length */
            switch (n)
                {
                case 1:                       /* token type depends on length */
                    tokens[itok].symbol = ONE_CHAR_;
                    break;
                case 2:
                    tokens[itok].symbol = TWO_CHAR_;
                    break;
                default:
                    tokens[itok].symbol = MANY_CHAR_;
                    break;
                }
                                         /* append this string to char buffer */
                                                 /* first allocate more memory*/
            char_values = (char *) realloc (char_values, cv_off + n + 1);
            strcpy (char_values + cv_off, next_token);             /* copy it */

            tokens[itok].value  = cv_off;      /* save offset into char array */
            cv_off += n + 1;                     /* get ready for next string */
            }
        else
            {
            msg ("Lexical analysis error interpreting token %s",2,next_token);
            return (-1);
            }

                                                             /* save category */
        tokens[itok].category = token_cat[tokens[itok].symbol];
        for (n=nold; n<line_end[0]; n++)
            if (line_end[n] > nchars+1)
                break;                    /* found line that this token is in */
        tokens[itok].line = n;                     /* save it in token struct */
        msg ("token %d: <%s> line %d category %hd symbol %hd value %d", -3, 
              itok, next_token, tokens[itok].line, tokens[itok].category,
              tokens[itok].symbol, tokens[itok].value);
        nold = (n > 1) ? n - 1 : 1;
        nchars += strlen (next_token) + 1;   /* total chars to start of token */
        itok++;
        }
    tokens = (struct token_struct *) 
             realloc (tokens, (itok + 1) * sizeof (struct token_struct));
    tokens[itok].category = 0;                     /* mark end of token array */

    return (0);                                        /* signal AOK from lex */
    }



/*******************************************************************************
*                                                                              *
*  is_keyword scans the list of keywords, looking for one to match the         *
*             input string, which is called next_token. Returns = 0 if         *
*             no match is found; else it returns an index number into          *
*             the keyword array.                                               *
*                                                   rjc  92.11.06              *
*******************************************************************************/

int is_keyword (next_token)
char *next_token;
    {
    extern char *token_string[];                   /* array of defined tokens */

    int i,number;

    number = 0;                          /* number == 0 means token not found */

    for (i=0; i<MAX_TOKENS; i++)
        {
        if (strcmp (next_token,token_string[i]) == 0)
            {
            number = i;                               /* success, this is it! */
            break;
            }
        }
    return (number);
    }


/*******************************************************************************
*                                                                              *
*  is_timeval checks the input token, called next_token, to see if it is       *
*             a valid time-value, which must be in the rather rigid format     *
*             of ddd-hhmmss. Each of the component fields are checked to be    *
*             in a valid range as follows: d(001-366), hh(00-23), mm(00-59),   *
*             and ss(00-59).                                                   *
*                                                   rjc  94.05.04              *
*******************************************************************************/

int is_timeval (next_token,i_value)
char *next_token;
int *i_value;
    {
    int i,
        r_code,
        iddd,
        ihh,
        imm,
        iss;

    if (strlen (next_token) != 10)          /* wrong length - not even close! */
        r_code = 0;                      /* r_code == 0 means token not found */
    else
        {
        r_code = 1;      /* good length, assume for now it will pass the test */
        for (i=0; i<10; i++)                 /* check characters for validity */
            {
            if (i == 3)                      /* hyphen should be fourth digit */
                {
                if (next_token[i] != '-')
                    r_code = 0;
                }
            else if (isdigit (next_token[i]) == 0)
                r_code = 0;                    /* not a digit; signal failure */
            }

        if (r_code)          /* if OK so far, check that fields are in bounds */
            {
            sscanf (next_token,"%3d-%2d%2d%2d",&iddd,&ihh,&imm,&iss);
            if (iddd < 1 || iddd > 366 ||
                ihh  < 0 || ihh  > 23  ||
                imm  < 0 || imm  > 59  ||
                iss  < 0 || iss  > 59)

                r_code = 0;             /* something amiss, this isn't a time */
            }

        }
    if (r_code)                  /* everything AOK, convert to secs since BOY */
        *i_value = (((iddd - 1) * 24 + ihh) * 60 + imm) * 60 + iss;

    return (r_code);
    }


/*******************************************************************************
*                                                                              *
*  is_integer checks the input token, called next_token, to see if it is       *
*             a valid base-10 integer, which consists of a (optional)          *
*             minus sign, followed by digits from [0-9]. If no invalid         *
*             character is found, 1 is returned; otherwise 0 is returned.      *
*                                                   rjc  92.11.06              *
*******************************************************************************/

int is_integer (next_token,i_value)
char *next_token;
int *i_value;
    {
    int i,sign,value;
    char c;

    value = 0;
    sign = 1;

    for (i = 0; i < strlen (next_token); i++)
        {
        c = next_token[i];                            /* fetch next character */
        if (c == '-' && i == 0)
            sign = -1;                                    /* valid minus sign */
        
        else if (isdigit (c))
            value = 10 * value + c - '0';                      /* valid digit */

        else
            return (0);                           /* indicate invalid integer */
        }
    *i_value = sign * value;
    return (1);                                     /* indicate valid integer */
    }


/*******************************************************************************
*                                                                              *
*  is_float checks the input string, called next_token, to see if it is a      *
*           valid floating point number. Will accept any floating point        *
*           constant that is acceptable to scanf.                              *
*                                                   rjc  92.11.06              *
*             modified to use sscanf                rjc  94.1.27               *
*******************************************************************************/


int is_float (next_token,f_value)
char *next_token;
double *f_value;
    {
    int nfields,nchars;
    double val;

    nfields = sscanf (next_token,"%le%n",&val,&nchars);

    if (nfields != 1 || nchars != strlen (next_token))
        return (0);
    else                            /* valid float detected, indicate success */
        {
        *f_value = val;
        return (1);
        }
    }


/*******************************************************************************
*                                                                              *
*  is_char checks for a string of 1 to 16 contiguous characters, all of        *
*          which appear in a valid-character list.                             *
*                                                                              *
*                                                   rjc  92.12.17              *
*******************************************************************************/

int is_char (next_token)
char *next_token;
   {
   int n,
       i,
       validity;

   n = strlen (next_token);

// if (n < 1 || n > MAXFREQ)
//    validity = 0;                          /* wrong length, post as invalid */
//
// else
//    {
      validity = 1;
      for (i=0; i<n; i++)
         if (isgraph (next_token[i]) == 0)
            validity = 0;           /* non-printing character, post not valid */
//    }
//
   return (validity);
   }
   

int fcode(char c)
	{
	int i;
//	char fchars[64];
	
	for (i = 0; i < MAXFREQ; i++)
		if (c == fchars[i]) return i;
	}
	
char get_fchar_by_index(int i)
	{
		if(i < MAXFREQ)
		{
			return fchars[i];
		}
		else
		{
			return '\0';
		}
	}
