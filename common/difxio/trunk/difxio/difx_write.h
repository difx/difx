#ifndef __DIFX_WRITE_H__
#define __DIFX_WRITE_H__

#include <stdio.h>
#include "difxio/difx_input.h"

int writeDifxLine(FILE *out, const char *key, const char *value);

int writeDifxLineInt(FILE *out, const char *key, int value);

int writeDifxLineInt1(FILE *out, const char *key, int i1, int value);

int writeDifxLine1(FILE *out, const char *key, int i1, const char *value);

int writeDifxLine2(FILE *out, const char *key, int i1, int i2, 
	const char *value);

int writeDifxLine3(FILE *out, const char *key, int i1, int i2, int i3, 
	const char *value);

#endif
