#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"

char single_code (char *station)
    {
    int i;
    char c;
    static char code_table[52][4] =
        {"A Ai", "B Bd", "C Sh", "D 13", "E Wf", "F Eb", "G Gb", "H Ho", "I Ma",
         "J Cc", "K Kk", "L xx", "M Mc", "N Ny", "O Kb", "P Oh", "Q Tc", "R Zc",
         "S Nt", "T Ts", "U Ur", "V Wz", "W xx", "X On", "Y Yb", "Z Mh",
         "a Ap", "b Br", "c Cm", "d Cn", "e xx", "f Fd", "g xx", "h Hn", "i xx",
         "j Jc", "k Kp", "l La", "m Mk", "n Nl", "o Ov", "p Pt", "q Qb", "r Ro",
         "s Sc", "t Ti", "u Ur", "v Pv", "w Wb", "x xx", "y Y ", "z xx"};

    update_stations(getenv("HOPS_STATION_CODE"), code_table);

    c = 0;
                                    // find station in code table
    for (i=0; i<52; i++)
        {
        if (strncmp (station, &code_table[i][2], 2) == 0)
            {
            c = code_table[i][0];
            break;
            }
        }
                                    // if we didn't find station, must assign
                                    // a code from an unused slot
    if (c == 0)
        {
        for (i=0; i<52; i++)
            if (strncmp ("xx", &code_table[i][2], 2) == 0)
                {
                strncpy (&code_table[i][2], station, 2);
                c = code_table[i][0];
                printf ("      Invented new code %c for station %c%c\n",
                         c, station[0], station[1]);
                break;
                }
        }
                                    // if no usunsed slot was found, c will be 0
    return c;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
