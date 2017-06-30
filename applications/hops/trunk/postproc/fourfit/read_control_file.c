/*******************************************************************************
*                                                                              *
* read_control_file reads the specified control file into memory. The contents *
*                   of the file are conditioned as follows:                    *
*                                                                              *
*                   1) white space (CRs, LFs, tabs, multiple spaces) is        *
*                      collapsed into a single space.                          *
*                   2) comments (COMCHAR through EOL) are stripped             *
*                   3) parentheses are delimited by a single space             *
*                                                                              *
*                   If the control_file_name starts with the string "if ",     *
*                   this indicates that the c_f_n isn't really a file name;    *
*                   instead the c_f_n string itself contains the desired       *
*                   control statements. In this case, all reads are done from  *
*                   the control_file_name.                                     *
*                                                              rjc  92.12.17   *
*                   Added memory file hidden in c_f_n feature  rjc  94.1.26    *
*                   Added flag argument to distinguish between file and in     *
*                   and in memory control string reads         jpb  17.4.7     *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "control.h"
                                             /* the 3 states of this tiny fsm */
#define goodstuff 1
#define white     2
#define comment   3

                                                 /* a couple of useful macros */
#define isparen(cc) c == '(' || c == ')'
#define append(cc) buffer = (char *)realloc (buffer,++num_chars);  \
                            buffer[num_chars - 1] = cc      

#define IS_CONTROL_FILE 1
#define IS_SET_STRING 2

int read_control_file (char* control_file_name, char** input_string, int* flag)
   {
   int state,
       num_chars,
       num_line,
       c,
       n,
       *buf_line;
   char *buffer;
   extern int *line_end;
   FILE *fp;

   buffer = (char *) malloc (1);         /* initially, output string is empty */
   buf_line = (int *) malloc (4);      /* Added malloc() call to make realloc */
                                        /* happy on the Suns, CJL May 26 1993 */
   num_chars = 0;
   num_line = 1;

   if (memcmp (control_file_name, "if ", 3))   /* special memory file option? */
      {
      *flag = IS_CONTROL_FILE;
      n = -1;                                  /* no, signify input from file */
      fp = fopen (control_file_name, "r");               /* open control file */
      if (fp == NULL)
         return (-1);                              /* return on opening error */
      c = getc (fp);                                /* prime the reading pump */
      }
   else
      {
      *flag = IS_SET_STRING;
      n = 0;                                 /* yes, point to first character */
      c = control_file_name[n++];                              /* and read it */
      }

   state = goodstuff;

   while (c != EOF)       /* process another character, so long as it's there */
      {
      if (c == '\n')                         /* newline - keep track of lines */
         {                                       /* for syntax error messages */
         buf_line = realloc (buf_line, sizeof (int) * (num_line + 1));
         buf_line[num_line++] = num_chars;
         }
      switch (state)                      /* figure out which state we are in */
         {
         case goodstuff:                       /* in the midst of useful text */
            if (c == COMCHAR)
               {
               state = comment;
               c = ' ';
               }
            else if (isspace (c))                     /* is this white space? */
               {
               state = white;
               c = ' ';
               }
            else if (isparen (c))         /* surround parentheses with spaces */
               {
               append (' ');
               append (c);
               c = ' ';
               state = white;
               }
            break;

         case white:                                      /* into white space */
            if (c == COMCHAR)
               {
               state = comment;
               c = 0;                                      /* reject comments */
               }
            else if (isspace (c))
               c = 0;
            else if (isparen (c))
               {                  /* on parentheses, append space, stay white */
               append (c);
               c = ' ';
               }
            else
               state = goodstuff;
            break;

         case comment:
            if (c == '\n')            /* comments only ended by newline char. */
               state = white;
            c = 0;
            break;
         }                                          /* end of switch on state */

      if (c)                             /* append this character if non-null */
         {
         append (c);                     /* beware - macro is multiple lines! */
         }

      if (n < 0)                       /* get next character from file or RAM */
         c = getc (fp);
      else
         {
         c = control_file_name[n++];
         if (c == '\0')              /* simulate end of file at end of string */
            c = EOF;
         }
      }                             /* end of while on single character reads */

   append ('\0');                               /* terminate string with null */

   if (n < 0)
      fclose (fp);                            /* all done, close control file */

   *input_string = buffer;                     /* pass address back to caller */
   line_end = buf_line;
   *line_end = num_line - 1;          /* save number of lines in 0th position */

   msg ("condensed string=%s",-1,buffer);
   return (0);
   }
