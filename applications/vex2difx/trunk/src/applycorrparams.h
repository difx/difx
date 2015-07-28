#ifndef __APPLYCORRPARAMS_H__
#define __APPLYCORRPARAMS_H__

#include "vex_data.h"
#include "corrparams.h"

int applyCorrParams(VexData *V, const CorrParams &params, int &nWarn, int &nError);

#endif
