/*
 * bogus.h
 *
 *  Created on: Feb 15, 2012
 *  Author: jtrier
 *  Description: contains strutures needed for external apps
 */

#ifndef LF1API_H_
#define LF1API_H_

#include <sys/time.h>
#include "xcTypes.h"

typedef struct _LF1_STREAM_INFO
{
        UINT32 taskType;        // task type ID
        UINT32 taskEnum;        // task run number
        INT32  thisZone;         // time zone
        struct timeval tv;      // timestamp for the project
} LF1_STREAM_INFO;

#endif /* LF1API_H_ */
