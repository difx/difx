//============================================================================
// Name        : xcTaskTools.h
// Author      : John Roberts
// Version     :
// Copyright   : (C) Copyright 2010 XCube Research and Development
//               All rights reserved.
//
// Description : Tools to deal with task names and addresses
//
// HISTORY     :
//       01/14/2011 - initial Version
//============================================================================
#ifndef XCTASKTOOL_H_
#define XCTASKTOOL_H_

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sched.h>
#include <errno.h>
#include <sys/resource.h>
#include <unistd.h>
#include "xcTypes.h"

// These values are to be used with the 'nice()' system call
// The default nice priority is 0
//
typedef enum _X3C_TASK_PRIORITY_
{
	X3C_TASK_PRIORITY_MIN = 0,
	X3C_TASK_PRIORITY_0 =  0,
	X3C_TASK_PRIORITY_1 = -2,
	X3C_TASK_PRIORITY_2 = -4,
	X3C_TASK_PRIORITY_3 = -6,
	X3C_TASK_PRIORITY_4 = -8,
	X3C_TASK_PRIORITY_5 = -10,
	X3C_TASK_PRIORITY_6 = -12,
	X3C_TASK_PRIORITY_7 = -14,
	X3C_TASK_PRIORITY_MAX = -20
} X3C_TASK_PRIORITY;

UINT32  addr_to_type(UINT32 addr);
UINT32 addr_to_enum(UINT32 addr);
UINT32 type_enum_to_addr(UINT32 ttype, UINT32 tenum);

int setAffinity(unsigned int cpuMask);
int getAffinity(unsigned int * currentCpuMask);
int setPriority(X3C_TASK_PRIORITY priority);
int getPriority(int  * priority);

#endif /* XCTASKTOOL_H_ */
