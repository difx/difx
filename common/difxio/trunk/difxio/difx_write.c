#include "difxio/difx_write.h"

int writeDifxLine(FILE *out, const char *key, const char *value)
{
	char line[1024];
	int i;

	for(i = 0; i < 32 && key[i]; i++)
	{
		line[i] = key[i];
	}
	line[i++] = ':';
	while(i < 20)
	{
		line[i++] = ' ';
	}
	value -= i;
	for(; i < 1023 && value[i]; i++)
	{
		line[i] = value[i];
	}
	line[i++] = 0;
	fprintf(out, "%s\n", line);

	return 0;
}

int writeDifxLineInt(FILE *out, const char *key, int value)
{
	char v[32];

	sprintf(v, "%d", value);

	return writeDifxLine(out, key, v);
}

int writeDifxLineInt1(FILE *out, const char *key, int i1, int value)
{
	char v[32];
	char k[128];

	sprintf(v, "%d", value);

	sprintf(k, key, i1);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLine1(FILE *out, const char *key, int i1, const char *value)
{
	char k[128];

	sprintf(k, key, i1);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine2(FILE *out, const char *key, int i1, int i2, 
	const char *value)
{
	char k[128];

	sprintf(k, key, i1, i2);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine3(FILE *out, const char *key, int i1, int i2, int i3, 
	const char *value)
{
	char k[128];

	sprintf(k, key, i1, i2, i3);
	
	return writeDifxLine(out, k, value);
}
