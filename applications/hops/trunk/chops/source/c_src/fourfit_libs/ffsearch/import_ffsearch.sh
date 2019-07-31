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
        "adhoc_flag.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/postproc/fourfit"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/fourfit_libs/ffsearch/include"

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
        "adhoc_flag.c"
        "apply_filter.c"
        "apply_notches.c"
        "apply_passband.c"
        "calc_normalization.c"
        "calc_rms.c"
        "compute_model.c"
        "compute_qf.c"
        "freq_spacing.c"
        "fringe_search.c"
        "delay_rate.c"
        "gate_off.c"
        "interp.c"
        "ion_search.c"
        "norm_fx.c"
        "norm_xf.c"
        "organize_data.c"
        "est_pc_manual.c"
        "precorrect.c"
        "pcalibrate.c"
        "report_actions.c"
        "rotate_pcal.c"
        "sampler_delays.c"
        "search.c"
        "search_windows.c"
        "update.c"
        "create_fname.c"
        "parse_cmdline.c"
        "apply_video_bp.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/postproc/fourfit"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/fourfit_libs/ffsearch/src"

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
