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

    declare -a source_list=(
        "alist.c"
        "open_output.c"
        "parse_cmdline.c"
        "summarize_mk4fringe.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/postproc/alist"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/applications/alist"

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
