/***************************************************************************
 *   Copyright (C) 2011-2012 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the Lesser GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   Lesser GNU General Public License for more details.                   *
 *                                                                         *
 *   You should have received a copy of the Lesser GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/vsis.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __VSIS_COMMANDS_H__
#define __VSIS_COMMANDS_H__

#include "mk5daemon.h"

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int packet_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int packet_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int filter_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int bank_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int bank_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int SS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int OS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int fill_pattern_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int fill_pattern_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int protect_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int protect_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int error_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_model_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_model_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_serial_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_size_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int dir_info_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int pointers_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int personality_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int personality_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int bank_info_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int net_protocol_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int net_protocol_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_state_mask_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int disk_state_mask_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int get_stats_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int start_stats_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int start_stats_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int mode_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int mode_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int rtime_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int disk_state_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int disk_state_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int scan_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int scan_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int record_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int record_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int reset_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int recover_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int recover_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int status_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int VSN_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int VSN_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int MAC_list_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int MAC_list_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

#endif
