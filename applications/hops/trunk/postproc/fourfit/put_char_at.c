/************************************************/
/*  put_char_at is a simplified procedure for   */
/* placing single characters inside a string 	*/
/* matrix.  It is almost impossible to break,	*/
/* and is faster than either print_at or 	*/
/* PrintLeft.					*/
/*						*/
/*	Author: Tony Lower			*/
/*		Created: June 26, 1991		*/
/*		Last Edit: June 26, 1991	*/
/*						*/
/************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include "print_page.h"

//#define BOUNDS_ERROR		FALSE
#define BOUNDS_ERROR		0

void 
put_char_at ( matrix, x, y, c )
char **matrix;
int x, y;
char c;
	
    {
    int lower_bound = 0 ;

    if ((c == '\n')||(c == '\t'))
	{
	fprintf( stderr," Character error in put_char_at.\n");
	} 			

    for(;matrix[lower_bound+1]!=NULL; lower_bound++);

    if ( y > lower_bound )
	{
	if (BOUNDS_ERROR)
	    fprintf(stderr,"Bounds error in put_char_at.\n");
	return ;			
	} 

    if (x >= strlen( matrix[y] ))
	{
	if (BOUNDS_ERROR)
	    fprintf(stderr,"Bounds error in put_char_at.\n");
	return ;
	} 

    matrix[y][x] = c ;

    } /* put_char_at */		
