#ifndef __PARSEDIFX__
#define __PARSEDIFX__

#define MAX_DIFX_KEY_LEN	32

typedef struct
{
	char *line;	/* entire, unmodified line of text */
	char *key;	/* before colon, trailing whitespace removed */
			/* also can hold comment */
	char *value;	/* after colon, leading/trailing whitespace removed */
			/* NULL pointer if no colon on row */
} DifxRow;

typedef struct
{
	int num_rows;	/* number of rows populated */
	int alloc_rows;	/* number of rows allocated */
	DifxRow *rows;	/* pointer to allocated row structures */
} DifxParameters;

/* Allocate new structure with given number of rows */
DifxParameters *newDifxParameters();

/* Free from memory existing structure */
void deleteDifxParameters(DifxParameters *dp);

/* Load difx file, allocating memory as needed, return pointer to struct */
DifxParameters *newDifxParametersfromfile(const char *filename);

/* Delete all the parameters */
void resetDifxParameters(DifxParameters *dp);

/* Mainly used internally -- used to double the number of allocated rows */
void growDifxParameters(DifxParameters *dp);

/* Add another line of text to structure and parse at same time */
int DifxParametersaddrow(DifxParameters *dp, const char *line);

/* Print contents of structure to stdout */
void printDifxParameters(const DifxParameters *dp);

/* Return index of first match to "key" starting from start_row */
int DifxParametersfind(const DifxParameters *dp, int start_row,
	const char *key);

/* Same as above, but allow key with a particular index value.  for example
 * to match "TELESCOPE 1 INDEX" set key = "TELESCOPE %d INDEX" and index = 1 */
int DifxParametersfind1(const DifxParameters *dp, int start_row,
	const char *key, int index1);

/* Same as above, but allow two indexes */
int DifxParametersfind2(const DifxParameters *dp, int start_row,
	const char *key, int index1, int index2);

/* Safely get the value of a row */
const char *DifxParametersvalue(const DifxParameters *dp, int row);

/* find row indicies for several keys */
int DifxParametersbatchfind(const DifxParameters *dp, int start,
	const char keys[][MAX_DIFX_KEY_LEN], int n, int rows[]);
int DifxParametersbatchfind1(const DifxParameters *dp, int start,
	const char keys[][MAX_DIFX_KEY_LEN], int index1, int n, int rows[]);

#endif
