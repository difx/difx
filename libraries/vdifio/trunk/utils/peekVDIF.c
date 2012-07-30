#include <stdio.h>
#include <stdlib.h>

#define MAX 100000000

int main(int argc, char **argv)
{
	FILE *in;
	unsigned char *data, *d;
	int n;
	int l, l0;
	int t, t0 = 0;
	int i, j;
	int nv = 0;
	int nb = 0;

	if(argc < 2)
	{
		exit(0);
	}
	in = fopen(argv[1], "r");

	data = (unsigned char *)malloc(MAX);

	n = fread(data, 1, MAX, in);

	fclose(in);

	printf("%d bytes read\n", n);

	t = t0 = data[0] + (data[1]<<8) + (data[2]<<16) + (data[3]<<24);

	l = l0 = (data[8] + (data[9]<<8) + (data[10]<<16)) << 3;


	for(i = 0; i < n;)
	{
		d = data + i;

		t = d[0] + (d[1]<<8) + (d[2]<<16) + (d[3]<<24);
		l = (d[8] + (d[9]<<8) + (d[10]<<16)) << 3;

		if(l == l0 && (t == t0 || t == t0+1))
		{
			++nv;

			printf("[%d %d] Timecode = %d l = %d t = %d\n", nv, nb, t, l,  d[14] | ((d[15] & 0x03) << 8));
			t0 = t;

			i += l;
		}
		else
		{
			++nb;

			for(j = 0; j < 9000; ++j)
			{
				d = data + i + j;
				t = d[0] + (d[1]<<8) + (d[2]<<16) + (d[3]<<24);
				l = (d[8] + (d[9]<<8) + (d[10]<<16)) << 3;

				if(l == l0 && (t == t0 || t == t0+1))
				{
					i += j;
					t0 = t;
		
					printf("[%d %d]  Len = %d\n", nv, nb, j);

					j = 1000000;
				}

			}
		}
	}

	return 0;
}
