/********************************************************************************/
/* 										*/
/* This routine accepts a typed line of input and parses it into commands and	*/
/* arguments.  Commands and their arguments are separated from other commands	*/
/* by semicolons, whilst arguments are separated from their command and each	*/
/* other by either spaces or commas.  Only the first two arguments are 		*/
/* explicitly parsed, and the rest are dumped into "remarg" for the use of the	*/
/* one or two commands that actually take more than two arguments.  The results */
/* of the parsing operation are placed in the structure array "commands".	*/
/* It is an error for more than 10 commands to be entered on one line.		*/
/*										*/
/*	Input:		line		User typed input line			*/
/*										*/
/*	Output:		commands	Structure array with parsed info	*/
/*			n		Number of parsed commands in "commands"	*/
/*										*/
/* Created March 30 1989 by CJL							*/
/********************************************************************************/
#include <string.h>
#include <stdio.h>
#include "aedit.h"

int
parse_commands (line,commands,n)
struct com commands[10];
char *line;
int *n;
{
	char *field[10];
	char *string, *strtok();
	int i, nfields;

/****************************************************************************/
/* Parse into strings separated by semicolons *******************************/
/****************************************************************************/
	field[0] = strtok(line,";");
	if(field[0] == NULL) {		/* Nothing on line - quit */
	    *n = 0;
	    return(0);
	}
	for(i=1;i<10;i++) {		/* Step through fields */
	    if((field[i] = strtok(NULL,";")) == NULL) break;
	}
	if(i == 10) {			/* User overzealous, chastize */
	    msg("Too many commands on one line", 2);
	    *n = 0;
	    return(0);
	}
	nfields = i;

/****************************************************************************/
/* Loop over strings, splitting each into words *****************************/
/****************************************************************************/

	for(i=0;i<nfields;i++) {
	    commands[i].cmnd[0] = '\0'; commands[i].arg1[0] = '\0';
	    commands[i].arg2[0] = '\0'; commands[i].remarg[0] = '\0';
	    string = strtok(field[i]," ,");
	    if(string == NULL) strcpy(commands[i].cmnd,"NOOP");
	    else strcpy(commands[i].cmnd,string);
	    commands[i].narg = 0;
	    if((string = strtok(NULL," ,")) != NULL) {
		strcpy(commands[i].arg1,string);
		commands[i].narg = 1;
	    }
	    if((string = strtok(NULL," ,")) != NULL) {
		strcpy(commands[i].arg2,string);
		commands[i].narg = 2;
	    }
	    if((string = strtok(NULL,"")) != NULL) {
		strcpy(commands[i].remarg,string);
		commands[i].narg = 3;
	    }
	}
	*n = nfields;
	return(0);
}
