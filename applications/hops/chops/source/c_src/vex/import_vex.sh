#!/bin/bash

#check if we were passed the flag --checksum-only, if so, we only need to
#compare the files, and return 0 (1) if they are the same (different)

CHKSUM=0
if [ "$1" == "--checksum-only" ]; then
	CHKSUM=1
fi

ret_val=0

if [ -z ${HOPS_SRC_DIR} ] && [ -z ${CHOPS_SRC_DIR} ]; then
    echo "Need to set HOPS_SRC_DIR and CHOPS_SRC_DIR"
else  

    #list of header files we want to import from hops
    declare -a header_list=(
        "cvex.h"
        "evex.h"
        "ivex.h"
        "lvex.h"
        "mk4_vex.h"
        "ovex.h"
        "svex.h"
        "vex.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/include"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/vex/include"

    for i in "${header_list[@]}"
    do
        if [ -f "${header_src_dir}/${i}" ]
        then
            if [ "${CHKSUM}" -eq "0" ]
            then
                cp "${header_src_dir}/${i}" "${header_dest_dir}/${i}"
            else
                SOURCE_HASH=$( md5sum "${header_src_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                SOURCE_HASH="${SOURCE_HASH%% *}" 
                DEST_HASH=$( md5sum "${header_dest_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                DEST_HASH="${DEST_HASH%% *}" 
                if [ "${SOURCE_HASH}" != "${DEST_HASH}" ]
                then
                    ret_val=1
                    echo "${header_src_dir}/${i}" " has changed and longer matches " "${header_dest_dir}/${i}"
                fi
            fi
        fi
    done

    declare -a source_list=(
        "block_params.c"
        "check_intrange.c"
        "check_realrange.c"
        "check_scan.c"
        "check_stloc.c"
        "check_strrange.c"
        "cvex_info.c"
        "decode_pval.c"
        "do_antenna.c"
        "do_bbc.c"
        "do_clock.c"
        "do_das.c"
        "do_eop.c"
        "do_exper.c"
        "do_freq.c"
        "do_head_pos.c"
        "do_if.c"
        "do_pass_order.c"
        "do_phase_cal_detect.c"
        "do_roll.c"
        "do_site.c"
        "do_source.c"
        "do_track.c"
        "evex_info.c"
        "fill_deflists.c"
        "fill_scanlist.c"
        "fill_station_parms.c"
        "find_statements.c"
        "get_block_mode.c"
        "get_chip_mode.c"
        "get_corr_bd_parms.c"
        "get_corr_mode.c"
        "get_def.c"
        "get_drive_init.c"
        "get_global_deflist.c"
        "get_logscan.c"
        "get_mode_deflist.c"
        "get_pbs_init.c"
        "get_pcm_config.c"
        "get_pcm_tables.c"
        "get_section_mode.c"
        "get_statement.c"
        "get_station_deflist.c"
        "get_su_chan_out.c"
        "get_su_connect.c"
        "get_trm_config.c"
        "get_val_list.c"
        "get_version.c"
        "get_vex.c"
        "in_comment.c"
        "init_scan.c"
        "in_quote.c"
        "ivex_info.c"
        "locate_blocks.c"
        "locate_cq.c"
        "lvex_info.c"
        "nextchar.c"
        "param_formats.c"
        "parse_date.c"
        "parse_dec.c"
        "parse_pval.c"
        "parse_ra.c"
        "parse_ref.c"
        "parse_units.c"
        "parse_vexfile.c"
        "print_location.c"
        "process_qstring.c"
        "read_file.c"
        "scan_info.c"
        "strip_text.c"
        "svex_info.c"
        "vex_init.c"
        "write_vexfile.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/sub/vex"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/vex/src"

    for i in "${source_list[@]}"
    do
        if [ -f "${source_src_dir}/${i}" ]
        then
            if [ "${CHKSUM}" -eq "0" ]
            then
                cp "${source_src_dir}/${i}" "${source_dest_dir}/${i}"
            else
                SOURCE_HASH=$( md5sum "${source_src_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                SOURCE_HASH="${SOURCE_HASH%% *}" 
                DEST_HASH=$( md5sum "${source_dest_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                DEST_HASH="${DEST_HASH%% *}" 
                if [ "${SOURCE_HASH}" != "${DEST_HASH}" ]
                then
                    ret_val=1
                    echo "${source_src_dir}/${i}" " has changed and longer matches " "${source_dest_dir}/${i}"
                fi
            fi
        fi
    done

fi

return ${ret_val}
