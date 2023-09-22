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

    #copy fourfit
    if [ "${CHKSUM}" -eq "0" ]
    then
        cp "${HOPS_SRC_DIR}/postproc/fourfit/fourfit.c" "${CHOPS_SRC_DIR}/source/c_src/applications/fourfit/fourfit.c"
    else
        SOURCE_HASH=$( md5sum "${HOPS_SRC_DIR}/postproc/fourfit/fourfit.c" | awk '{print $1}' | tr -d '\n')
        SOURCE_HASH="${SOURCE_HASH%% *}" 
        DEST_HASH=$( md5sum "${CHOPS_SRC_DIR}/source/c_src/applications/fourfit/fourfit.c" | awk '{print $1}' | tr -d '\n')
        DEST_HASH="${DEST_HASH%% *}" 
        if [ "${SOURCE_HASH}" != "${DEST_HASH}" ]
        then
            ret_val=1
            echo "${HOPS_SRC_DIR}/postproc/fourfit/fourfit.c" " has changed and longer matches " "${CHOPS_SRC_DIR}/source/c_src/applications/fourfit/fourfit.c"
        fi
    fi

fi

return ${ret_val}
