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
        "account.h"
        "fileset.h"
        "fstruct.h"
        "general.h"
        "mk4_sizes.h"
        "mk4_typedefs.h"
        "mk4_util.h"
	"adler32_checksum.h"
        "hops_complex.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/include"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/mk4util/include"

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
        "account.c"
		"adler32_checksum.c"
        "check_name.c"
        "clear_date.c"
        "clear_fstruct.c"
        "confirm.c"
        "datec_to_datef.c"
        "datef_to_datec.c"
        "day_of_datef.c"
        "environment.c"
        "extract_filenames.c"
        "fileset.c"
        "get_abs_path.c"
        "get_filelist.c"
        "hptoie4.c"
        "hptoie8.c"
        "hptoie.c"
        "int_to_time.c"
        "ismk4.c"
        "minmax.c"
        "msg.c"
        "report_times.c"
        "root_belong.c"
        "root_id.c"
        "sexigesimal2hrdeg.c"
        "sort_names.c"
        "swap.c"
        "syntax.c"
        "time_to_double.c"
        "time_to_int.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/sub/util"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/mk4util/src"

    
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
