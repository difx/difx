#include "other.h"

/* copy the first single-quote enclosed string portion of src into dest.
 * pad out to n characters with spaces or truncate string longer than n
 * characters */
void copyQuotedString(char *dest, const char *src, int n)
{
	int i = 0;

	dest[0] = 0;

	if(*src == 0)
	{
		return;
	}
	while(*src != '\'')
	{
		src++;
		if(*src == 0)
		{
			return;
		}
	}
	src++;
	while(*src != '\'')
	{
		*dest = *src;
		dest++;
		i++;
		if(i > n)
		{
			return;
		}
		if(*src == 0)
		{
			*dest = 0;
			return;
		}
		src++;
	}
	*dest = 0;
	for(; i < n; ++i)
	{
		*dest = ' ';
		dest++;
	}
}
