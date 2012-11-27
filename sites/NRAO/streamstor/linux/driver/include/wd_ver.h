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

/* 
 * Alternately, if compiling for Microsoft Windows, Windows CE, Mac, this
 * file is licensed under the WinDriver commercial license provided with the
 * Software.
 */

#define WD_MAJOR_VER 10
#define WD_MINOR_VER 3
#define WD_SUB_MINOR_VER 1

#define WD_MAJOR_VER_STR "10"
#define WD_MINOR_VER_STR "3"
#define WD_SUB_MINOR_VER_STR "1"

/* %% Replace with empty string for non-beta version %% */
#define WD_VER_BETA_STR "" 

#define WD_VERSION_MAC_STR WD_MAJOR_VER_STR "." WD_MINOR_VER_STR "." \
    WD_SUB_MINOR_VER_STR " " WD_VER_BETA_STR
#define WD_VERSION_STR WD_MAJOR_VER_STR "." WD_MINOR_VER_STR \
    WD_SUB_MINOR_VER_STR WD_VER_BETA_STR
#define WD_VER (WD_MAJOR_VER * 100 + WD_MINOR_VER * 10 + WD_SUB_MINOR_VER)
#define WD_VER_ITOA WD_MAJOR_VER_STR WD_MINOR_VER_STR WD_SUB_MINOR_VER_STR
#define WD_VERSION_STR_DOTS WD_MAJOR_VER_STR "." WD_MINOR_VER_STR "." \
    WD_SUB_MINOR_VER_STR WD_VER_BETA_STR
