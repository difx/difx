/* Jungo Confidential. Copyright (c) 2011 Jungo Ltd.  http://www.jungo.com */

/* 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License version 2 as published by
 * the Free Software Foundation.
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License v2 for
 * more details.
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, contact Jungo Ltd. at
 * http://www.jungo.com/openrg/opensource_ack.html
 */

#ifndef _LINUX_COMMON_H_
#define _LINUX_COMMON_H_
#include <linux/version.h>

#ifndef VERSION_CODE
#  define VERSION_CODE(vers,rel,seq) ( ((vers)<<16) | ((rel)<<8) | (seq) )
#endif

/* only allow major releases */
#if LINUX_VERSION_CODE < VERSION_CODE(2,0,0) /* not < 2.0 */
#  error "This kernel is too old: not supported by this file"
#endif
#if LINUX_VERSION_CODE > VERSION_CODE(2,7,0) /* not > 2.6, by now */
#  error "This kernel is too recent: not supported by this file"
#endif
#if (LINUX_VERSION_CODE & 0xff00) == 1 /* not 2.1 */
#  error "Please don't use linux-2.1, use 2.2 or 2.4 instead"
#endif
#if (LINUX_VERSION_CODE & 0xff00) == 3 /* not 2.3 */
#  error "Please don't use linux-2.3, use 2.4 or 2.6 instead"
#endif
#if (LINUX_VERSION_CODE & 0xff00) == 5 /* not 2.5 */
#  error "Please don't use linux-2.5, use 2.6 instead"
#endif

/* remember about the current version */
#if LINUX_VERSION_CODE < VERSION_CODE(2,1,0)
#  define LINUX_20
#elif LINUX_VERSION_CODE < VERSION_CODE(2,3,0)
#  define LINUX_22
#elif LINUX_VERSION_CODE < VERSION_CODE(2,5,0)
#  define LINUX_24
#elif LINUX_VERSION_CODE < VERSION_CODE(2,7,0)
#  define LINUX_26
#else
#error "unsupported linux kernel version"
#endif

#if defined(LINUX_22) || defined(LINUX_24) || defined(LINUX_26)
    #define LINUX_22_24_26
#endif
#if defined(LINUX_26) || defined(LINUX_24)
    #define LINUX_24_26
#endif
#if defined(LINUX_20) || defined(LINUX_22)
    #define LINUX_20_22    
#endif 

/* Starting kernel 2.6.18, autoconf.h is included in the build
 * command line */
#if LINUX_VERSION_CODE < VERSION_CODE(2,6,18)
    #include <linux/config.h>
#endif

#if defined(LINUX_24)
#if defined(CONFIG_MODVERSIONS) && !defined(MODVERSIONS)
    #define MODVERSIONS
#endif
#if defined(MODVERSIONS)
    #include <linux/modversions.h>
#endif
#include <linux/init.h>
#include <linux/kernel.h>
#endif

#endif

