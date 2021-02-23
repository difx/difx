export AMON_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/"

if [ ! -d "amon_config" ]; then
	ln -s $AMON_PATH/amon_config ./amon_config
	#cp -arv $AMON_PATH/amon_config .
fi

PYTHONPATH=${PYTHONPATH}:${AMON_PATH} /usr/bin/python ${AMON_PATH}/amon.py alist_v6.out  *.vex
