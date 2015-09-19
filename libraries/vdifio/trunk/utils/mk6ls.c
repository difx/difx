#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifmark6.h>

int main(int argc, char **argv)
{
	char **fileList;
	int n;
	int a;
	int longPrint = 0;
	int fullPrint = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-l") == 0 || strcmp(argv[a], "--long") == 0)
		{
			longPrint = 1;
		}
		else if(strcmp(argv[a], "-f") == 0 || strcmp(argv[a], "--full") == 0)
		{
			fullPrint = 1;
		}
	}

	n = getMark6FileList(&fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in %s\n", getMark6Root());
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			if(fullPrint)
			{
				Mark6Gatherer *G;
				long long int size;
				int f;

				G = openMark6GathererFromTemplate(fileList[i]);
				size = getMark6GathererFileSize(G);
				printf("%s   %d  %lld  %s\n", fileList[i], G->nFile, size, isMark6GatherComplete(G) ? "complete" : "incomplete");
				for(f = 0; f < G->nFile; ++f)
				{
					printMark6File(&(G->mk6Files[f]));
				}
				closeMark6Gatherer(G);
			}
			else if(longPrint)
			{
				Mark6Gatherer *G;
				long long int size;

				G = openMark6GathererFromTemplate(fileList[i]);
				size = getMark6GathererFileSize(G);
				printf("%s   %d  %lld  %s\n", fileList[i], G->nFile, size, isMark6GatherComplete(G) ? "complete" : "incomplete");
				closeMark6Gatherer(G);
			}
			else
			{
				printf("%s\n", fileList[i]);
			}
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}


	return 0;
}
