#include <cstdio>

char *fgetsNoCR(char *line, int MaxLineLength, FILE *in)
{
	char *v;
	unsigned int i;

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		return v;
	}

	// strip CR/LF chars from end
	for(i = 0; line[i]; ++i)
	{
		if(line[i] < ' ')
		{
			line[i] = 0;

			break;
		}
	}

	return v;
}
