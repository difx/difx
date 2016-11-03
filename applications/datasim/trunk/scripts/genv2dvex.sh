#!/bin/bash
#
#

[ $# -eq 2 ] || { echo "Usage: genv2dvex tdir obs_info" ;
echo "(e.g. genv2dvex g3 anoise.inp.7000)" ; exit 1 ; }
tdir=$1 ; shift
file="$@"
fdir=`dirname $file`
#echo $fdir

verb=${VERB-'false'}

export ENUM=7000

[ -n $file -a -f $file ] && . $file && 
	echo sourced $file ||
	echo failed to source $file

# read station source time information from inputfile
target=$SOURCE
scanStart=$START_TIME
scanStop=$END_TIME
fftres=$FFTRES
#echo $target
#echo $STATION

oIFS=$IFS
# read string and put the value into an array
IFS=', ' read -ra antennas <<< "$STATION"
IFS=', ' read -ra band <<< "$BAND"
IFS=', ' read -ra gap <<< "$GAP"
IFS=', ' read -ra freq_shift <<< "$SHIFT"

freq_obs=$FREQ_OBS

# read station information from station.cat
st_ids=()
site_pos=()
for a in ${antennas[@]}
do
	line=`sed -n '/'$a'/p' $fdir/station.cat`
#	eval $line
	IFS=';' read -ra sinfo <<< "$line"
	sid=${sinfo[1]}
	spos=${sinfo[2]}
	id=${sid#*=}
	site_position=${spos#*=}
	echo $id $site_position
	st_ids+=("$id")
	site_pos+=("$site_position")	
done
#echo ${site_pos[0]}

stlist=''
for i in ${st_ids[@]}
do
	[ -z "$stlist" ] && stlist=$i || stlist="$stlist,$i"
	# echo $stlist
done

# read source information from target.cat
info=`sed -n '/'$target'/p' $fdir/target.cat`
IFS=';' read -ra tinfo <<< "$info"
tiau=${tinfo[1]}
tra=${tinfo[2]}
tdec=${tinfo[3]}
tref_coord=${tinfo[4]}

iau=${tiau#*=}
ra=${tra#*=}
dec=${tdec#*=}
ref_coord=${tref_coord#*=}

IFS=$oIFS

# calculate format info for each antenna
numAnt=${#antennas[@]}
fmts=()
for i in $(eval echo {0..$((numAnt-1))})
do
  str=${band[$i]}
  bw=${str%x*}
  ch=${str#*x}
  fnum=$(echo "8*$ch*$bw*2" | bc)
  fnum=${fnum%.*}
  while [ $fnum -gt 8192 ]
  do
    fnum=$((fnum/2))
    #echo $fnum
  done
  fnum=$((fnum+32))
  fmts+=("VDIF/$fnum/2")
  echo $str $bw $ch $fnum ${fmts[$i]}
done

# calculated by hand
# starttime 2012y075d05h58m00s
# stoptime 2012y075d05h58m05s
# export mjdStart=56001.248611111
# export mjdStop=56001.248668981

# strip leading zeros of string
function slz {
  str=$(echo $1 | sed 's/^0*//')
  [ -z $str ] && str=0
  echo $str
}

# calculate mjd
function tomjd {
  yy=`expr $1 : '\(....\)y...d..h..m.*'`
  dd=`expr $1 : '....y\(...\)d..h..m.*'`
  hh=`expr $1 : '....y...d\(..\)h..m.*'`
  mm=`expr $1 : '....y...d..h\(..\)m.*'`
  ss=`expr $1 : '....y...d..h..m\(..\)s'`

  dd=`slz $dd`
  hh=`slz $hh`
  mm=`slz $mm`
  ss=`slz $ss`

  mjds=$((($yy - 1901) / 4 * 1461 + ($yy - 1901) % 4 * 365 + $dd -1 + 15385))
  # fraction of day
  fods=$(echo "scale=9; (($ss/60 + $mm)/60 + $hh)/24" | bc -l)
  echo $(echo "$mjds+$fods" | bc)
}

export mjdStart=`tomjd $scanStart`
export mjdStop=`tomjd $scanStop`
echo $mjdStart
echo $mjdStop

export scanName=''
export force='--force'

[ -n "$FFTRES"    ] || export FFTRES=$fftres
[ -n "$SPECRES"   ] || export SPECRES=0.50
[ -n "$NINT"      ] || export NINT=0.8
[ -n "$SUBINT"    ] || export SUBINT=32000000

comment="
  # Variables:
  mjdStart=$mjdStart
  mjdStop=$mjdStop
  scanStart=$scanStart
  scanStop=$scanStop
  scanName=$scanName
  target=$target
  FFTRES=$FFTRES
  SPECRES=$SPECRES
  NINT=$NINT
  SUBINT=$SUBINT
  force=$force
"
# echo "$comment"
# $verb && echo "# St_info: $st_info"

#
# specifics for this dataset
#
jobn=$tdir
exper=$tdir
num=$ENUM
# pick up station, mjdStart, mjdStop, scanStart, scanfile
# [ -n "$comment" ] && echo "$comment"
[ -z "$scanStart" ] && { echo scanStart undefined ; exit 2; }

cepoch=`expr "$scanStart" : '\(....y...d\).*'`00h00m

#
# make a place to work
#
while [ -d $tdir-$num ] ; do num=`expr $num + 1` ; done
#if [ -d $tdir-$num ] ; then
#	rm -rf $tdir-$num
#fi
td=$tdir-$num
mkdir $td && cd $td && echo In $td || { echo Cannot make $td ; exit 2; }
wdir=`pwd`
job=${jobn}.$num
sob=${jobn}_$num

# construct scan name and target
dd=`expr $scanStart : '....y\(...\)d..h..m.*'`
hh=`expr $scanStart : '....y...d\(..\)h..m.*'`
mm=`expr $scanStart : '....y...d..h\(..\)m.*'`
[ -n "$scanName" ] || scanName=$dd-$hh$mm
scan=$scanName
hopsrc=`echo $target | tr . _`

etime=${ETIME-'4'}

#
# set formats information by hands at the moment
#

# st_info:
# id	name	fmt					bw		das
# SP	SMA 	VDIF/2032/2 32.0	4 
st_info=''
for idx in $(eval echo {0..$((numAnt-1))})
do
  str=${band[$idx]}
  st_info="$st_info ${st_ids[$idx]} ${antennas[$idx]} ${fmts[$idx]} ${str%x*} ${str#*x}"
done
echo $st_info
st_shift=5

temp=()
set -- $st_info
while [ $# -ge $st_shift ]
do
    st=$1 name=$2 fmt=$3 bw=$4 das=$5
    shift $st_shift
    fmt=`echo $fmt | tr / _`
    temp+=($fmt)
done
formats=$(echo ${temp[@]} | tr ' ' '\n' | sort -u)
echo ${formats[@]}

# create input v2d
echo '' ; echo '%%% manufacturing vex input' ; echo ''
cat > $job.v2d <<-EOF
vex = $exper.vex.obs
#mjdStart = $mjdStart
#mjdStop  = $mjdStop
mjdStart = $START_TIME
mjdStop = $END_TIME
antennas = $stlist
startSeries = $num
dataBufferFactor = ${DBFAC-16}
visBufferLength = ${VBLEN-80}
nDataSegments = ${NDSEG-8}
minLength = 0.5

SETUP default
{
   tInt = ${NINT-0.48}
   # subintNS = ${SUBINT-240000000}
   guardNS = 2000
   FFTSpecRes = ${FFTRES-0.25}
   specRes = ${SPECRES-0.50}
   xmacLength = 1
   strideLength = 1
}
EOF
nstn=0 set -- $st_info
while [ $# -ge $st_shift ]
do
    [ -z "$nstn" ] && nstn=0
		st=$1 name=$2 fmt=$3 bw=$4 das=$5
		file=$st.vdif
    cat >> $job.v2d <<-....EOF

ANTENNA $st
{
   file = $file
   format = $fmt
   phaseCalInt = ${PHASECALINT-0}
....EOF
    cat >> $job.v2d <<-....EOF
}
....EOF
    nstn=`expr $nstn + 1`
    file=''
    shift $st_shift
done

#[ -n "$v2dcomment" ] && echo "$v2dcomment" >> $job.v2d
#[ -n "$formats" ] && echo "$formats" | sed 's/^/# /' >> $job.v2d

#
# Manufacture a suitable zero-baseline test vex file.
#

rm -f $exper.vex.obs
cat >> $exper.vex.obs <<EOF
VEX_rev = 1.5;
\$GLOBAL;
    ref \$EXPER = AVzoom;
    ref \$EOP = EOP001;
*-----------------------   end \$GLOBAL             ----------------------*
\$EXPER;
  def AVzoom;
    exper_name = $exper;
    exper_num = $num;
    exper_description = AVzoom;
    PI_name = GBC;
    target_correlator = DiFX;
  enddef;
*-----------------------   end \$EXPER              ----------------------*
*----------------------- begin \$MODE               ----------------------*
\$MODE;
  def MINE;
EOF
set -- $st_info
while [ $# -ge $st_shift ]
do
		st=$1 name=$2 fmt=$3 bw=$4 das=$5
    fmt=`echo $fmt | tr / _`
    cat >> $exper.vex.obs <<....EOF
    * station $st
    ref \$FREQ = $fmt-FREQ_$st:$st;
    ref \$BBC = $fmt-BBC:$st;
    ref \$IF = $fmt-IF:$st;
    ref \$TRACKS = $fmt-TRACKS_$st:$st;
....EOF
    shift $st_shift
done
cat >> $exper.vex.obs <<EOF
  enddef;
*-----------------------   end \$MODE               ----------------------*
*----------------------- begin \$EOP                ----------------------*
\$EOP;
 def EOP001;
* TAI-UTC = 34 sec;
  TAI-UTC = 35 sec;
  A1-TAI = 0.0 sec;
  eop_ref_epoch = $cepoch;
  * min of 5 are required, apparently
  num_eop_points = 5;
  eop_interval = 24 hr;
  ut1-utc  = 0.0 sec  : 0.0 sec  : 0.0 sec  : 0.0 sec  : 0.0 sec  ;
  x_wobble = 0.0 asec : 0.0 asec : 0.0 asec : 0.0 asec : 0.0 asec ;
  y_wobble = 0.0 asec : 0.0 asec : 0.0 asec : 0.0 asec : 0.0 asec ;
 enddef;
*-----------------------   end \$EOP                ----------------------*
*----------------------- begin \$CLOCK              ----------------------*
\$CLOCK;
EOF
set -- $st_info
while [ $# -ge $st_shift ]
do
	st=$1 name=$2 fmt=$3 bw=$4 das=$5
  eval cearly=\$${1}_early
  [ -z $cearly ] && cearly=0
  cat >> $exper.vex.obs <<..EOF
  def $st; clock_early = $cepoch : $cearly usec: $cepoch : 0; enddef;
..EOF
  shift $st_shift
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$CLOCK             ----------------------*
*----------------------- begin \$STATION            ----------------------*
\$STATION;
EOF
set -- $st_info
while [ $# -ge $st_shift ]
do
	st=$1 name=$2 fmt=$3 bw=$4 das=$5
  cat >> $exper.vex.obs <<..EOF
  def $st;
    ref \$SITE = $name;
    ref \$ANTENNA = $name;
    ref \$CLOCK = $name;
    ref \$DAS = FakeDAS;
  enddef;
..EOF
  shift $st_shift
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$STATION            ----------------------*
*----------------------- begin \$ANTENNA            ----------------------*
\$ANTENNA;
EOF
set -- $st_info
while [ $# -ge $st_shift ]
do
	st=$1 name=$2 fmt=$3 bw=$4 das=$5
	cat >> $exper.vex.obs <<..EOF
  def $name;
    antenna_diam =  10.00 m;
    axis_type = az : el;
    axis_offset = 0 m;
    pointing_sector = &n : az : 0 deg : 360 deg : el : 0 deg : 90 deg;
  enddef;
..EOF
	shift $st_shift
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$ANTENNA            ----------------------*
*----------------------- begin \$DAS                ----------------------*
\$DAS;
  def FakeDAS;
*   record_transport_type = VDIF;
  enddef;
*-----------------------   end \$DAS                ----------------------*
EOF
cat >> $exper.vex.obs <<EOF
*----------------------- begin \$BBC                ----------------------*
\$BBC;
EOF
for fmt in ${formats[@]}
do
    cat >> $exper.vex.obs <<....EOF
    def $fmt-BBC;
			* vanilla_bbc
		BBC_assign = &BBC01 : 01 : &IF_XR;
    enddef;
....EOF
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$BBC                ----------------------*
*----------------------- begin \$FREQ               ----------------------*
\$FREQ;
EOF
set -- $st_info
idx=0
while [ $# -ge $st_shift ]
do
		freq=$freq_obs
		st=$1 name=$2 fmt=$3 bw=$4 das=$5
		fmt=`echo $fmt | tr / _`
    cat >> $exper.vex.obs <<..EOF
    def $fmt-FREQ_$st;
      *
..EOF
  for i in $(eval echo {1..$((das))})
  do
  if [ $i -eq 1 ]; then
    freq=$(echo "$freq+${freq_shift[$idx]}" | bc)
  fi
	[ $i -lt 10 ] && ch=0$i || ch=$i
	cat >> $exper.vex.obs <<....EOF
	chan_def = &B : $freq MHz: U : $bw MHz : &Ch$ch : &BBC01 : &cp;  
....EOF
  if [ `expr ${gap[$idx]} \= 0.0` -eq 1 ]; then
    freq=$(echo "$freq+$bw+${gap[$idx]}" | bc)
  else
    chgap=(${gap[$idx]//:/ })
    freq=$(echo "$freq+$bw+${chgap[$(($i-1))]}" | bc)    
  fi
  done
  cat >> $exper.vex.obs <<..EOF
	sample_rate = $(echo "$bw*2" | bc) Ms/sec;
    enddef;
..EOF
  idx=$idx+1
    shift $st_shift
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$FREQ               ----------------------*
*----------------------- begin \$IF                 ----------------------*
\$IF;
EOF
for fmt in ${formats[@]}
do
    cat >> $exper.vex.obs <<....EOF
    def $fmt-IF;
			* vanilla_if
		if_def = &IF_XR : XX : R : $freq_obs MHz : U : 0 MHz : 0 Hz;
    enddef;
....EOF
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$IF                 ----------------------*
*-----------------------   begin \$TRACKS            ----------------------*
\$TRACKS;
EOF
set -- $st_info
while [ $# -ge $st_shift ]
do
    st=$1 name=$2 fmt=$3 bw=$4 das=$5
    nfmt=`echo $fmt | tr / _`
    cat >> $exper.vex.obs <<....EOF
def $nfmt-TRACKS_$st;
    * station $st
    track_frame_format = $fmt;
....EOF
    for i in $(eval echo {1..$((das))})
    do
    [ $i -lt 10 ] && ch=0$i || ch=$i
    cat >> $exper.vex.obs <<....EOF

    fanout_def =  : &Ch$ch : sign : 1: $i;
    fanout_def =  : &Ch$ch : mag : 1: $i;
....EOF
    done
    cat >> $exper.vex.obs <<....EOF
enddef;
....EOF
    shift $st_shift
done
cat >> $exper.vex.obs <<EOF

*-----------------------   end \$TRACKS              ----------------------*
*-----------------------   begin \$SCHED            ----------------------*
\$SCHED;
* 2010y001d20h58m21.3503s 2010y001d20h58m25.3501s
* i have no idea what the format of station is...
  scan $scanName;
    start = $scanStart;
    mode = MINE;
    * hopefully one of these is visible
    source = $target;
EOF
[ "$etime" -lt 20 ] && et=20 || et=$etime
set -- $st_info
while [ $# -ge $st_shift ]
do
	st=$1 name=$2 fmt=$3 bw=$4 das=$5
  cat >> $exper.vex.obs <<..EOF
    * real time or 20, whichever is greater, no idea why.
    station = $st : 0 sec : $et sec : 0 ft : 1A : &n : 1;
..EOF
  shift $st_shift
done
cat >> $exper.vex.obs <<EOF
  endscan;
*-----------------------   end \$SCHED              ----------------------*
*----------------------- begin \$SITES              ----------------------*
\$SITE;
* Westford antenna location
EOF
idx=0
set -- $st_info
while [ $# -ge $st_shift ]
do
	st=$1 name=$2 fmt=$3 bw=$4 das=$5
  cat >> $exper.vex.obs <<..EOF
  def $name;
    site_type = fixed;
    site_name = $name;
    site_ID = $st;
    site_position =  ${site_pos[$idx]};
  enddef;
..EOF
	idx=$idx+1
  shift $st_shift
done
cat >> $exper.vex.obs <<EOF
*-----------------------   end \$SITES              ----------------------*
*----------------------- begin \$SOURCE             ----------------------*
\$SOURCE;
* totally bogus source always visible at Westford
  def $target;
    source_type = galaxy;
    source_name = $target;
    IAU_name = $iau;
    dec = $dec;
    ra = $ra;
    ref_coord_frame = $ref_coord;
  enddef;
*-----------------------   end \$SOURCE             ----------------------*
EOF

#
# more or less boilerplate from here on down
#

echo '' ; echo '%%% creating inputs' ; echo ''
#echo \
#vex2difx $force $job.v2d
#vex2difx $force $job.v2d
#
#[ -f $sob.input ] || {
#    echo vex2difx did not like your v2d/vex file combination
#    echo $wdir/$exper.vex.obs
#    echo $wdir/$job.v2d
#    exit 1
#}
#
#echo \
#calcif2 $sob.input
#calcif2 $sob.input

#
# eof
#
