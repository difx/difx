#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FILE *fopen_trials(char **name, char *mode)
    {
    FILE *fp = fopen(*name, mode);
    if (fp) return(fp);
    *name = "./fourmakes";
    system( "cat Makefile Makefile Makefile Makefile > ./fourmakes");
    fp = fopen(*name, mode);
    return(fp);
    }

main (argc, argv)
int argc;
char *argv[];
    {
    /* char teststr[]=
	"This is a test - This is a test This is a test This is a test"; */
    char input[100001], output[100001], icopy[100001];
    char workspace[32768], *tvb = getenv("testverb");
    char *name = "/usr/share/dict/words";
    int i, len, len2, verb;
    FILE *fp;

    input[100000]  = 'A';
    icopy[100000]  = 'B';
    output[100000] = 'C';

    verb = tvb ? atoi(tvb) : 0;

    /* strcpy (input, teststr); */
    /* doa: fp = fopen ("/tmp/28825aaa", "r"); */
    fp = fopen_trials (&name, "r");
    if (!fp)
	{
	    perror("fopen");
	    fprintf(stderr, "trying to open %s\n", name);
	    return(0);
	}
    if (verb>0) printf("Opened %s as FILE %p\n", name, fp);
    fread (input, 1, 99999, fp);
    memcpy(icopy, input, 100000);

    compress_compress (workspace, input, strlen(input), output, &len);

    if (verb>0)
	printf("length of output, input = %d, %d\n", len, strlen(input));

    for (i=0; i<256; i++) input[i] = '\0';

    compress_decompress (workspace, output, len, input, &len2);
    i = memcmp(input, icopy, 100000);

    if (verb>0)
	{
	printf("Output length = %d\n",len2);
	if (verb>1)
	    printf("Output string = '%100.100s...'\n", input);
	printf("Memcmp status = %d\n", i);
	printf("Guard bytes   = %c %c %c\n",
	    input[100000], icopy[100000], output[100000]);
	}

    return (
	input[100000] == 'A' &&
	icopy[100000] == 'B' &&
	output[100000] == 'C' &&
	i == 0 && len > 0 && strlen(input) > 0
	    ? 0 : 1 );
    }
