#!/bin/bash
#ffcore is responsible for managing the initialization and organization
#of the pass and param structures

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
    "filter.h"
    "freqlist.h"
    "param_struct.h"
    "pass_struct.h"
    "plot_struct.h"
    "refringe.h"
    "statistics.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/postproc/fourfit"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/fourfit_libs/ffcore/include"

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

    #list of header files we want to import from hops
    declare -a alt_header_list=(
        "fourfit_signal_handler.h"
        "write_lock_mechanism.h"
    )

    alt_header_src_dir="${HOPS_SRC_DIR}/include"
    alt_header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/fourfit_libs/ffcore/include"

    for i in "${alt_header_list[@]}"
    do
        if [ -f "${alt_header_src_dir}/${i}" ]
        then
            if [ "${CHKSUM}" -eq "0" ]
            then
                cp "${alt_header_src_dir}/${i}" "${alt_header_dest_dir}/${i}"
            else
                SOURCE_HASH=$( md5sum "${alt_header_src_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                SOURCE_HASH="${SOURCE_HASH%% *}"
                DEST_HASH=$( md5sum "${alt_header_dest_dir}/${i}" | awk '{print $1}' | tr -d '\n')
                DEST_HASH="${DEST_HASH%% *}"
                if [ "${SOURCE_HASH}" != "${DEST_HASH}" ]
                then
                    ret_val=1
                    echo "${alt_header_src_dir}/${i}" " has changed and longer matches " "${alt_header_dest_dir}/${i}"
                fi
            fi
        fi
    done

    declare -a source_list=(
    "create_lockfile.c"
    "fourfit_signal_handler.c"
    "wait_for_write_lock.c"
    "clear_pass.c"
    "make_flist.c"
	"make_passes.c"
    "fill_param.c"
    "read_sdata.c"
    "time_range.c"
    "get_corel_data.c"
    "set_defaults.c"
    "clear_freq_corel.c"
    "check_rflist.c"
    "refringe_list.c"
    "set_pointers.c"
    "generate_cblock.c"
    "vrot.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/postproc/fourfit"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/fourfit_libs/ffcore/src"

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
