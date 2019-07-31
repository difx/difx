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
        "adata.h"
        "afile_structure.h"
        "mk4_afio.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/include"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/afio/include"

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
        "afile_header.c"
        "aline_id.c"
        "check_sizes.c"
        "clear_afile_structure.c"
        "clear_csumm.c"
        "clear_fsumm.c"
        "clear_rsumm.c"
        "clear_tsumm.c"
        "corelname.c"
        "fringename.c"
        "get_unique_name.c"
        "parse_csumm.c"
        "parse_fsumm.c"
        "parse_rsumm.c"
        "parse_tsumm.c"
        "read_afile.c"
        "rootname.c"
        "write_csumm.c"
        "write_fsumm.c"
        "write_rsumm.c"
        "write_tsumm.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/sub/afio"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/afio/src"

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
