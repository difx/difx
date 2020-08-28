#!/bin/bash
#
# parFourfit.sh <fourfit control file> <scan1> [<scan2> <scan3> ...]
#
# Runs several fourfit instances in parallel, per baseline, for each
# of the given scan(s). Fringe fits visibility data, excluding autos.
# The scripts waits if more than 20 fourfit instances
# are active on the computer.
#
# Example: parFourfit.sh cf_eht 1234/100-0243 1234/100-0517 1234/100-0525
#
# Optionally define env var FF_EXTRA_ARGS with fourfit arguments (e.g. EXTRA_ARGS=-PXL),
# or FF_POST_ARGS with commands to append to fourfit (e.g. FF_POST_ARGS="set freqs a").

set -m

MAX_FF_INSTANCES=20

if [ "$1" == "" ]; then
    echo "Usage: parFourfit.sh <fourfit control file> <scan1> [<scan2> <scan3> ...]"
fi

for indir in "${@:2}"; do
	echo $indir
	rm -f ./$indir/*.lock

	blines=()
	for visfile in `ls -1 ./$indir/??..*`; do
		bl=${visfile##*/}
		bl=${bl%..*}
		if [ "${bl:0:1}" == "${bl:1:1}" ]; then
			# skip autocorrs and auto cross-pols?
			continue  # to skip
			#echo     # to keep
		fi
		blines+=("$bl")
		# echo ${visfile##*/}  $bl
	done

	for bl in "${blines[@]}"; do
		if compgen -G "./$indir/$bl..*" > /dev/null; then
			solcount=`find ./$indir/ -name "$bl.[A-Z].*" | wc -l`
			if [ "$solcount" != "4" ] && [ "$solcount" != "2" ]; then
				echo "Interrupted fourfit in scan $indir baseline $bl, got $solcount instead of 4 solutions, redoing!"
				rm -f ./$indir/$bl.?.*
			fi
			if compgen -G "./$indir/$bl.[A-Z].*" > /dev/null; then
				echo "Scan $indir baseline $bl fourfit already done"
			else
				export cnt=`ps axuf | grep fourfit | wc -l`
				while (( $cnt > $MAX_FF_INSTANCES )); do
					echo "Waiting $indir baseline $bl : still too many ($cnt > 20) fourfit processes running..."
					sleep 2
					export cnt=`ps axuf | grep fourfit | wc -l`
				done
				echo "Scan $indir baseline $bl : fourfit"
				echo "fourfit -m1 -c $1 -b$bl $FF_EXTRA_ARGS -u $indir $FF_POST_ARGS"
				# fourfit -m1 -c $1 -b$bl $FF_EXTRA_ARGS -u $indir $FF_POST_ARGS &
				fourfit -m1 -c $1 -b$bl $FF_EXTRA_ARGS -u $indir $FF_POST_ARGS 2>&1 | grep 'sbd\|SNR' &
				err=$?
				if [  "$err" != "0" ]; then
					echo "Fourfit returned error $err! Stopping."
					exit $err
				fi
			fi
		fi
	done
	wait
done
