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
        "bytflp.h"
        "type_000.h"
        "type_100.h"
        "type_101.h"
        "type_110.h"
        "type_120.h"
        "type_200.h"
        "type_201.h"
        "type_202.h"
        "type_203.h"
        "type_204.h"
        "type_205.h"
        "type_206.h"
        "type_207.h"
        "type_208.h"
        "type_210.h"
        "type_212.h"
        "type_220.h"
        "type_221.h"
        "type_222.h"
        "type_230.h"
        "type_300.h"
        "type_301.h"
        "type_302.h"
        "type_303.h"
        "type_304.h"
        "type_305.h"
        "type_306.h"
        "type_307.h"
        "type_308.h"
        "type_309.h"
        "T1.h"
        "mk4_records.h"
        "mk4_data.h"
        "mk4_dfio.h"
    )

    header_src_dir="${HOPS_SRC_DIR}/include"
    header_dest_dir="${CHOPS_SRC_DIR}/source/c_src/dfio/include"

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

    #The file "alloc_t120_array.c" has not been included 
    #as it doesn't appear to be used anywhere

    declare -a source_list=(
        "addr_100.c"
        "addr_101.c"
        "addr_110.c"
        "addr_120.c"
        "addr_200.c"
        "addr_201.c"
        "addr_202.c"
        "addr_203.c"
        "addr_204.c"
        "addr_205.c"
        "addr_206.c"
        "addr_207.c"
        "addr_208.c"
        "addr_210.c"
        "addr_212.c"
        "addr_220.c"
        "addr_221.c"
        "addr_222.c"
        "addr_230.c"
        "addr_300.c"
        "addr_301.c"
        "addr_302.c"
        "addr_303.c"
        "addr_304.c"
        "addr_305.c"
        "addr_306.c"
        "addr_307.c"
        "addr_308.c"
        "addr_309.c"
        "clear_100.c"
        "clear_101.c"
        "clear_110.c"
        "clear_120.c"
        "clear_200.c"
        "clear_201.c"
        "clear_202.c"
        "clear_203.c"
        "clear_204.c"
        "clear_205.c"
        "clear_206.c"
        "clear_207.c"
        "clear_208.c"
        "clear_210.c"
        "clear_212.c"
        "clear_220.c"
        "clear_221.c"
        "clear_222.c"
        "clear_230.c"
        "clear_300.c"
        "clear_301.c"
        "clear_302.c"
        "clear_303.c"
        "clear_304.c"
        "clear_305.c"
        "clear_306.c"
        "clear_307.c"
        "clear_308.c"
        "clear_309.c"
        "clear_coord.c"
        "clear_mk4corel.c"
        "clear_mk4fringe.c"
        "clear_mk4sdata.c"
        "compress.c"
        "copy_100.c"
        "copy_101.c"
        "copy_110.c"
        "copy_120.c"
        "copy_200.c"
        "copy_201.c"
        "copy_202.c"
        "copy_203.c"
        "copy_204.c"
        "copy_205.c"
        "copy_206.c"
        "copy_207.c"
        "copy_208.c"
        "copy_210.c"
        "copy_212.c"
        "copy_220.c"
        "copy_221.c"
        "copy_222.c"
        "copy_230.c"
        "copy_300.c"
        "copy_301.c"
        "copy_302.c"
        "copy_303.c"
        "copy_304.c"
        "copy_305.c"
        "copy_306.c"
        "copy_307.c"
        "copy_308.c"
        "copy_309.c"
        "corel_alloc.c"
        "display_221.c"
        "init_000.c"
        "open_datafile.c"
        "read_mk4corel.c"
        "read_mk4file.c"
        "read_mk4fringe.c"
        "read_mk4sdata.c"
        "test_compress.c"
        "write_err.c"
        "write_mk4corel.c"
        "write_mk4fringe.c"
        "write_mk4sdata.c"
        "write_record.c"
    )

    source_src_dir="${HOPS_SRC_DIR}/sub/dfio"
    source_dest_dir="${CHOPS_SRC_DIR}/source/c_src/dfio/src"

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

    #copy bytflip.h
    if [ "${CHKSUM}" -eq "0" ]
    then
        cp "${source_src_dir}/bytflp.h" "${header_dest_dir}/bytflp.h"
    else
        SOURCE_HASH=$( md5sum "${source_src_dir}/bytflp.h" | awk '{print $1}' | tr -d '\n')
        SOURCE_HASH="${SOURCE_HASH%% *}" 
        DEST_HASH=$( md5sum "${header_dest_dir}/bytflp.h" | awk '{print $1}' | tr -d '\n')
        DEST_HASH="${DEST_HASH%% *}" 
        if [ "${SOURCE_HASH}" != "${DEST_HASH}" ]
        then
            ret_val=1
            echo "${source_src_dir}/bytflp.h" " has changed and longer matches " "${header_dest_dir}/bytflp.h"
        fi
    fi

fi

return ${ret_val}
