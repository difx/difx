/********************************/
/*				*/
/*   grid is a procedure which	*/
/* draws a single point in on	*/
/* a graph.			*/
/*   It deals with points that	*/
/* exceed the limits of the	*/
/* graph, and also with search	*/
/* windows.			*/
/*				*/
/********************************/
#include <stdio.h>

#include <math.h>

#ifndef irint
#define irint(a) 	(a)
#endif

void 
grid(matrix, pos_x, pos_y, width, height, xscale, yscale, 
					winstart, winstop, x, y, ch, altch)
char **matrix;
int pos_x, pos_y, x, winstart, winstop;
double xscale, yscale;
double y;
char ch, altch;
    {
	
    int tempx, tempy;
    char out;

    out = ch;
    if (((winstart < winstop) && (x < winstart || x > winstop))
     || ((winstart > winstop) && (x > winstop  && x < winstart)))
	out = altch;
    tempx = (int)((width*x)/xscale + 0.5);
    tempy = (int)((height*y)/yscale + 0.5);
    if (tempy > height)
	{
	out = '^';
	tempy = height;
	}
    put_char_at(matrix, pos_x+tempx, pos_y+height-tempy, out);
}
