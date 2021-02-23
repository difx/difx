#!/bin/bash

if [ $# != 2 ]; then
	echo "Usage: amon.sh <.alist file name>  <.vex file name>"
	exit
fi

export AMON_PATH=$DIFXROOT/svn/sites/MPIfR/AMON-gmva

# Config files(/dir) is expected in current working directory. Use defaults if missing.
if [ ! -d "amon_config" ]; then
        ln -s $AMON_PATH/amon_config ./amon_config
        # cp -arv $AMON_PATH/amon_config .
fi

PYTHONPATH=${PYTHONPATH}:${AMON_PATH} /usr/bin/python2 ${AMON_PATH}/amon.py $1 $2
