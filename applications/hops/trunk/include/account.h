/*
 * $Id$
 */

#ifndef ACCOUNT_H
#define ACCOUNT_H

#define MAX_PSEGS 50
#define NAME_LEN 31
#define TRUE 1
#define FALSE 0

struct time_account
    {
    char    segment_name[NAME_LEN + 1];
    int     namlen;
    double  real_time;
    double  user_time;
    double  system_time;
    int     times_called;
    };

#endif

extern char *account(char *segment_name);

/*
 * eof
 */
