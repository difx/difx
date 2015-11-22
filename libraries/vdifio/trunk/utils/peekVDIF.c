#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX 100000000

int main(int argc, char **argv)
{
	FILE *in;
	unsigned char *data, *d;
	int n, f;
	int l, l0;
	int t, t0 = 0;
	int i, j;
	int nv = 0;
	int nb = 0;
	int lastf = -1;
	int lastt = -1;
	int nf = 0;
	long long B = 0;

	int T[4] = {-1,-1,-1,-1}; 

	

	if(argc != 2)
	{
		fprintf(stderr, "Usage: %s <inputVDIFFile>\n", argv[0]);

		exit(0);
	}
	in = fopen(argv[1], "r");
	if(!in)
	{
		fprintf(stderr, "Error: file '%s' cannot be opened\n", argv[1]);

		return EXIT_FAILURE;
	}

	data = (unsigned char *)malloc(MAX+1000);

	n = fread(data, 1, MAX, in);


	printf("%d bytes read\n", n);

	lastt = t = t0 = data[0] + (data[1]<<8) + (data[2]<<16) + (data[3]<<24);

	l = l0 = (data[8] + (data[9]<<8) + (data[10]<<16)) << 3;

	for(i = 0; ;)
	{
		if(i > MAX-10000 || i > n)
		{
			memmove(data, data + i, MAX-i);
			fprintf(stderr, "B=%lld Read %d\n", B, i);
			B += i;
			n = fread(data + MAX - i, 1, i, in);
			if(n < 1)
			{
				break;
			}
			n += i;
			i = 0;
		}

		d = data + i;

		t = d[0] + (d[1]<<8) + (d[2]<<16) + (d[3]<<24);
		l = (d[8] + (d[9]<<8) + (d[10]<<16)) << 3;
		
		f = d[4] + (d[5]<<8) + (d[6]<<16);

		if(f == lastf)
		{
			++nf;
		}
		else
		{
			if(t != lastt)
			{
				if(t != lastt + 1)
				{
					printf("Error: seconds change %d to %d  B = %lld f = %d\n", lastt, t, B + i, f);
				}
				if(f != 0)
				{
					printf("Error: frame number not reset on second change  B = %lld t = %d f = %d\n", B + i, t, f);
				}

				lastt = t;
			}
			if(f >= 0 && f != lastf + 1 && (f != 0 || lastf != 12799))
			{
				printf("Error: frame number change %d to %d  B = %lld t = %d\n", lastf, f, B + i, t);
			}
			if(nf != 4 && lastf >= 0)
			{
				printf("Warning: t = %d f = %d B = %lld nf = %d T = [%d %d %d %d]\n", t, f, B + i, nf, T[0], T[1], T[2], T[3]);
			}
			nf = 1;
			lastf = f;
			T[0] = T[1] = T[2] = T[3] = -1;
		}

		if(nf > 0 && nf <= 4)
		{
			T[nf-1] = d[14] | ((d[15] & 0x03) << 8);
		}
		else
		{
			printf("Weird: nf=%d   ", nf);
			printf("t = %d f = %d B = %lld nf = %d T = [%d %d %d %d]\n", t, f, B + i, nf, T[0], T[1], T[2], T[3]);
			exit(0);
		}

		

		if(l == l0 && (t == t0 || t == t0+1))
		{
			++nv;

			printf("[%lld %d %d] Timecode = %d f = %d l = %d t = %d\n", B+i, nv, nb, t, f, l,  d[14] | ((d[15] & 0x03) << 8));
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
		
					printf("Warning: [%lld %d %d] %d %d  Len = %d\n", B+i, nv, nb, t, f, j);

					j = 1000000;
				}

			}
		}
	}

	fclose(in);

	return 0;
}
