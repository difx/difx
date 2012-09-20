#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"

#ifndef STANDALONE_CHECK
#define STANDALONE_CHECK 0
#else /* STANDALONE_CHECK */
static int verb = 0;
#endif /* STANDALONE_CHECK */
static int update_stations_done = 0;

/*
 * Normally invoked with file = getenv("HOPS_STATION_CODE")
 * which should be a file of 2-letter id <=> 1-letter id edit pairs.
 * The pairs may be in either order and must be white-space separated.
 * If "debug" is present in the file name, then before and after
 * versions of the file will be generated.
 */

static void dump_stations(char *file, char *suffix, char code_table[52][4])
{
    char *name = malloc(strlen(file) + strlen(suffix) + 3);
    FILE *ofp;
    int	i;
#if STANDALONE_CHECK
    if (verb>0) printf("Dumping code_table to %s.%s\n", file, suffix);
#endif /* STANDALONE_CHECK */
    if (!name) { perror("dump_stations:malloc"); return; }
    sprintf(name, "%s.%s", file, suffix);
    ofp = fopen(name, "w");
    if (!ofp) { perror("dump_stations:fopen"); free(name); return; }
    for (i=0; i<52 && code_table[i]; i++)   
	fprintf(ofp, "%4.4s\n", code_table[i]);
    fflush(ofp);
    fclose(ofp);
    free(name);
}

/*
 * xx is a special case here.
 */
static void edit_stations(char *one, char *two, char code_table[52][4])
{
    int i;
    for (i=0; i<52; i++) {
	if (code_table[i][0] == *one) {
	    code_table[i][2] = two[0];
	    code_table[i][3] = two[1];
	}
	if (code_table[i][2] == two[0] && two[0] != 'x' &&
	    code_table[i][3] == two[1] && two[1] != 'x') {
	    code_table[i][0] = one[0];
	}
    }
}

void update_stations(char *file, char code_table[52][4])
{
    FILE *ifp;
    int debug, three;
    char this[4], that[4];
    if (update_stations_done) return;	/* once is normally enough */
    update_stations_done = 1;
#if STANDALONE_CHECK
    if (verb>0) printf("Updating code_table with %s\n", file);
#endif /* STANDALONE_CHECK */
    if (!file) return;
    debug = strstr(file, "debug") ? 1 : 0;
    if (debug) dump_stations(file, "default", code_table);
    if (!(ifp = fopen(file, "r"))) return;
    while (2 == fscanf(ifp, "%3s %3s%n", this, that, &three)) {
	if (strlen(this) == 1 && strlen(that) == 2)
	    edit_stations(this, that, code_table);
	else if (strlen(this) == 2 && strlen(that) == 1)
	    edit_stations(that, this, code_table);
	else if (three == 3) /* possibly ambiguous */
	    edit_stations(this, that, code_table);
	else
	    fprintf(stderr, "Bad station pairing %s <==> %s\n", this, that);
    }
    fclose(ifp);
    if (debug) dump_stations(file, "editted", code_table);
}

/*
 * Test code follows
 */
#if STANDALONE_CHECK
/* all this for stat? */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/* copied from single_code.c */
static char mct[52][4] =
    {"A Ai", "B Bd", "C Sh", "D 13", "E Wf", "F Eb", "G Gb", "H Ho", "I Ma",
     "J Cc", "K Kk", "L xx", "M Mc", "N Ny", "O Kb", "P Oh", "Q Tc", "R Zc",
     "S Nt", "T Ts", "U Ur", "V Wz", "W xx", "X On", "Y Yb", "Z Mh",
     "a Ap", "b Br", "c Cm", "d Cn", "e xx", "f Fd", "g xx", "h Hn", "i xx",
     "j Jc", "k Kp", "l La", "m Mk", "n Nl", "o Ov", "p Pt", "q Qb", "r Ro",
     "s Sc", "t Ti", "u Ur", "v Pv", "w Wb", "x xx", "y Y ", "z xx"};

/*
 * This is perhaps as easily done via an check script, but
 * I do this to show how it can be done in the code itself.
 */
static int run_tests(void)
{
    char hack_one[] = AWK
	" '$2 ~ /xx/ {$2=sprintf(\"%02s\",NR);print}'"
	" < debug.default > debug.hacks";
    char hack_two[] = AWK
	" '$2 ~ /[0-9][0-9]/ {$2=sprintf(\"xx\",NR);print}'"
	" < debug.hacks.editted > debug.fixed";
    char hack_tre[] = "diff debug.fixed.editted debug.default > debug.diffs";
    char hack_fur[] = "cp debug.hacks debug.envir";
    char hack_fiv[] = "cmp debug.envir.editted debug.hacks.editted";
    struct stat sb;

    if (strlen(AWK) == 0) return(0);	/* no AWK to hack with */

    update_stations("debug", mct);
    update_stations_done = 0;		/* for next test */
    if (stat("debug.default", &sb) || sb.st_size != 260) return(13);

    if (verb>2 && system("ls -l debug.*")) return(21);
    if (verb>1) fprintf(stdout, "%s\n", hack_one);
    if (system(hack_one)) return(22);
    update_stations("debug.hacks", mct);
    update_stations_done = 0;		/* for next test */
    if (stat("debug.hacks.editted", &sb) || sb.st_size != 260) return(23);

    if (verb>2 && system("ls -l debug.hacks*")) return(31);
    if (verb>1) fprintf(stdout, "%s\n", hack_two);
    if (system(hack_two)) return(32);
    update_stations("debug.fixed", mct);
    update_stations_done = 0;		/* for next test */
    if (stat("debug.fixed.editted", &sb) || sb.st_size != 260) return(33);

    if (!system(hack_tre)) return(42);
    if (stat("debug.diffs", &sb) || sb.st_size != 22) return(43);

    mct[3][2] = '1';
    mct[3][3] = '3';
    if (system(hack_fur)) return(52);
    if (verb>0) fprintf(stdout, "HSC = %s\n", getenv("HOPS_STATION_CODE"));
    update_stations(getenv("HOPS_STATION_CODE"), mct);
    update_stations_done = 0;		/* for next test */
    if (stat("debug.envir.editted", &sb) || sb.st_size != 260) return(53);
    if (system(hack_fiv)) return(54);

    return(0);
}

int main(int argc, char **argv)
{
    int ers = 0;
    verb = getenv("testverb") ? atoi(getenv("testverb")) : 0;
    if (argc == 2) update_stations(argv[1], mct);
    else if (getenv("srcdir")) ers = run_tests();
    if (ers) fprintf(stderr, "Error code %d\n", ers);
    return(ers);
}
#endif /* STANDALONE_CHECK */
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
