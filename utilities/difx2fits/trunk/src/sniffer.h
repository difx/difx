#ifndef __SNIFFER_H__
#define __SNIFFER_H__

#include <stdio.h>
#include <complex.h>
#include <fftw3.h>
#include "fitsUV.h"

struct _Sniffer;

typedef struct _Sniffer Sniffer;

Sniffer *newSniffer(const DifxInput *D, int nComplex, 
	const char *filebase, double solint);

void deleteSniffer(Sniffer *S);

int feedSnifferFITS(Sniffer *S, const struct UVrow *data);

#endif
