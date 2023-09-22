#include <stdio.h>
#include <unistd.h>
#include <string.h>

int difxiowriteint0_(int *fd, char *f1, int *v1, int s1)
{
	char format[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	snprintf(line, 200, "%-20s%d\n", format, *v1);

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowriteint1_(int *fd, char *f1, int *i1, int *v1, int s1)
{
	char format[100];
	char lhs[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	snprintf(lhs, 100, format, *i1);
	snprintf(line, 200, "%-20s%d\n", lhs, *v1);

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowriteint2_(int *fd, char *f1, int *i1, int *i2, int *v1, int s1)
{
	char format[100];
	char lhs[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	snprintf(lhs, 100, format, *i1, *i2);
	snprintf(line, 200, "%-20s%d\n", lhs, *v1);

	write(*fd, line, strlen(line));

	return 0;
}

static void stripspaces(char *s, int l)
{
	int i;

	for(i = l-1; i > 0; --i)
	{
		if(s[i] == ' ')
		{
			s[i] = 0;
		}
		else
		{
			break;
		}
	}
}

int difxiowritestring0_(int *fd, char *f1, char *v1, int s1, int s2)
{
	char format[100];
	char value[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	strncpy(value, v1, s2);
	value[s2] = 0;
	stripspaces(value, s2);
	snprintf(line, 200, "%-20s%s\n", format, value);

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowritestring1_(int *fd, char *f1, int *i1, char *v1, int s1, int s2)
{
	char format[100];
	char value[100];
	char lhs[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	strncpy(value, v1, s2);
	value[s2] = 0;
	stripspaces(value, s2);
	snprintf(lhs, 100, format, *i1);
	snprintf(line, 200, "%-20s%s\n", lhs, value);

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowritestring2_(int *fd, char *f1, int *i1, int *i2, char *v1, int s1, int s2)
{
	char format[100];
	char value[100];
	char lhs[100];
	char line[200];

	strncpy(format, f1, s1);
	format[s1] = 0;
	strncpy(value, v1, s2);
	value[s2] = 0;
	stripspaces(value, s2);
	snprintf(lhs, 100, format, *i1, *i2);
	snprintf(line, 200, "%-20s%s\n", lhs, value);

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowritepoly2_(int *fd, char *f1, int *i1, int *i2, double *v1, int v1l, int s1)
{
	char format[100];
	char lhs[100];
	char line[1000];
	int i, n;

	strncpy(format, f1, s1);
	format[s1] = 0;
	snprintf(lhs, 100, format, *i1, *i2);
	n = snprintf(line, 1000, "%-20s", lhs);
	for(i = 0; i < v1l; ++i)
	{
		n += snprintf(line+n, 1000-n, "%22.15e%c", v1[i], ((i == v1l-1) ? '\n' : ' '));
	}

	write(*fd, line, strlen(line));

	return 0;
}

int difxiowritepoly26_(int *fd, char *f1, int *i1, int *i2, double *p0, double *p1, double *p2, double *p3, double *p4, double *p5, int s1)
{
	char format[100];
	char lhs[100];
	char line[1000];

	strncpy(format, f1, s1);
	format[s1] = 0;
	snprintf(lhs, 100, format, *i1, *i2);
	snprintf(line, 1000, "%-20s%22.15e\t%22.15e\t%22.15e\t%22.15e\t%22.15e\t%22.15e\n", lhs, *p0, *p1, *p2, *p3, *p4, *p5);

	write(*fd, line, strlen(line));

	return 0;
}
