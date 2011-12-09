#!/bin/bash
# gloPut7T.sh  Copies files in a designated directory to a remote server.
#              Requires threaded globus-url-copy; uses sshftp.
#              Graham.Jenkins@arcs.org.au  April 2009. Rev: 20100519
#              Modified by Cormac Reynolds (c.reynolds@curtin.edu.au) for use on pbstore SAM/QFS: 20100527

# Default-batch-size, environment
BATCH=16       # Adjust as appropriate
#[ -d /opt/globus-5.0.1/bin ] && export GLOBUS_LOCATION=/opt/globus-5.0.1 \
#                         || export GLOBUS_LOCATION=/opt/globus-4.2.1
export GLOBUS_LOCATION=/opt/globus/
#export GLOBUS_LOCATION=/opt/globus-4.2.1/
#export GLOBUS_LOCATION=/opt/globus-5.0.1/
#export GLOBUS_LOCATION=/opt/gt5.0.3/

export PATH=$GLOBUS_LOCATION/bin:$PATH

# Usage, alias
#Params="-p 4"
Params=""
Skip="A"
Match="."
maxwait=15
while getopts b:m:w:usRr Option; do
  case $Option in
    b) BATCH=$OPTARG;;
    u) Params="-udt -p 2";;
    s) Skip=;;
    m) Match=$OPTARG;;
    R) Recurse="Y";;
    r) Order="-r";;
    w) maxwait=$OPTARG;;
    \?) Bad="Y";;
  esac
done
shift `expr $OPTIND - 1`
[ \( -n "$Bad" \) -o \( $# -ne 3 \) ] &&
  ( echo "  Usage: `basename $0` directory remote-userid remote-directory"
    echo "   e.g.: `basename $0` /data/xraid0/v252l" \
                 "accumulator@arcs-df.ivec.org" \
                 "/data/ASTRO-TRANSFERS/February09/v252l/Mopra"
    echo "Options: -b n .. use a batch-size of 'n' (default 16)"
    echo "         -r   .. reverse order"
    echo "         -s   .. skip files whose names begin with a period"
    echo "         -m <pattern>  .. specify a pattern to grep for filenames"
    echo "         -R   .. transfer subdirectories recursively"
    echo "         -w n .. maximum waiting time (minutes) for files to stage on cortex"
    echo "         -u   .. use 'udt' protocol"                ) >&2 && exit 2
#alias ssu='ssh -o"UserKnownHostsFile /dev/null" -o"StrictHostKeyChecking no"'
ssu() {
    ssh -o"UserKnownHostsFile /dev/null" -o"StrictHostKeyChecking no" $@
}

# Failure/cleanup function; parameters are exit-code and message
fail() {
  Code=$1; shift
  rm -f $LisFil
  echo "$@"; exit $Code
}

# Globus-URL-Copy function; file list is 1st param
doGlobus() {
  echo "`date '+%a %T'` .. Pid: $$ .. Files:"
#wc -c `awk '{print $1}' < $1 | cut -c 8-`
  du -h `awk '{print $1}' < $1 | cut -c 8-`
  #globus-url-copy -q $Params -cc 2 -fast -f $1
  globus-url-copy -q $Params -cc 1 -fast -f $1
  echo
  >$1
  [ -x "$1" ]                             || fail 0 "Graceful Termination"
}


stage_and_wait() {
  # Files on the pbstore need to be staged before they can be transferred.
  # Compile a list of files to stage, waiting for them to complete if necessary
  EndPrestage=$(( BATCH * 4 ))
  [ "`cat $LisFil 2>/dev/null | wc -l`" -eq 0 ] && prestage $InDir ${FileList[@]:StartPrestage:EndPrestage}
  
  # wait for the next file due to be transferred to finish staging. This
  # should generally only be seen at the beginning of a transfer.
  #stage -r -w $1/$File
  #sfind $InDir/$File -offline -exec echo 'staging' {} \; -exec stage -w {} \;
  # actually stage -w seems to put a load on pbstore, so:
  if [ -n "`sfind $InDir/$File -offline `" ] ; then
    echo -n "staging $InDir/$File "
    waittime=0
    while [ -n "`sfind $InDir/$File -offline `" -a $waittime -lt $maxwait ]; do
        waittime=$(( waittime+1 ))
        stage $InDir/$File
        #echo -n "$nwait."
        echo -n "."
        sleep $(( nwait * 60 ))
    done
    echo
  fi
  # update the access time so it is not top of the release queue. (touch -a
  # does not work on other people's files!)
  #head -1 $InDir/$File > /dev/null
  #touch $InDir/$File
}

# stage a set of files. First param is the directory path. This is followed by
# a list of files to stage
prestage() {
    dirpath="${1}"
    shift
    files=("${@}")
    local filelist2
    unset filelist2
    for stageFil in "${files[@]}"; do
        #echo "pre-staging $dirpath/$stageFil"
        if [ -e $dirpath/$stageFil ]; then
            # stage the file, 
            #stage "$dirpath/$stageFil"
            filelist2[${#filelist2[*]}]="$dirpath/$stageFil"
        fi
    done;
    stage -V ${filelist2[@]}
}

# Create destination directory if required, ensure that we can write to it 
echo "ssu $2 /bin/date</dev/null>/dev/null 2>&1 "
ssu $2 /bin/date</dev/null>/dev/null 2>&1 || fail 1 "Remote-userid is invalid"
ssu $2 "mkdir -p -m 775 $3"   2>/dev/null || fail 1 "Remote-directory problem"
ssu $2 "test -w         $3"   2>/dev/null || fail 1 "Remote-directory problem"

# Create temporary file, set traps
LisFil=`mktemp` && chmod a+x $LisFil      || fail 1 "Temporary file problem"
trap "chmod a-x $LisFil ; echo Break detected .. wait"     TERM
trap 'Params="     -p 4"; echo Switched to TCP..'      USR1
trap 'Params="-udt -p 2"; echo Switched to UDT..'      USR2

# get list of directories to be transferred.
InDirList=( "$1" )
OutDirList=( "$3" )
if [ -n "$Recurse" ] ; then
    cd $1
    for Dir in `find ./* -type d`; do
        InDirList[${#InDirList[*]}]="$1/$Dir"
        OutDirList[${#OutDirList[*]}]="$3/$Dir"
        ssu $2 "mkdir -p -m 775 $3/$Dir"   2>/dev/null || fail 1 "Remote-directory problem: $3/$Dir"
    done
fi


# loop through each directory to be transferred.
for (( idir=0; idir < ${#InDirList[@]}; idir++ )); do
  InDir="${InDirList[@]:idir:1}"
  OutDir="${OutDirList[@]:idir:1}"
  echo "Transferring $InDir to $OutDir"
  # Loop until no more files need to be copied
  echo "To Terminate gracefully,  enter: kill -TERM $$"
  echo "To switch to TCP/UDT mode enter: kill -USR1/USR2 $$"
  Flag=Y
  while [ -n "$Flag" ] ; do
    Flag=
    unset FileList
    echo "Finished at `date '+%a %T'`"
    echo "Generating a list of files to be copied .. `date '+%a %T'` .. wait .."
    # List filename/size couplets in remote and local directories; if a couplet
    # appears once then it hasn't been copied properly, so add filename to list
  
    for File in `( ssu $2 "ls -lL$Skip $OutDir 2>/dev/null"
                           ls -lL$Skip $InDir 2>/dev/null ) |
        awk '{print \$NF, \$5}' |  sort $Order | uniq -u | awk '{print \$1}' | uniq | grep $Match`; do

      #FileList=( "${FileList[@]}" "$File" )
      #FileList+=( $File )
      FileList[${#FileList[*]}]=$File

    done
  
    nwait=1
    StartPrestage=-1
    for File in ${FileList[@]}; do
      StartPrestage=$(( StartPrestage + 1 ))
      
      [ \( ! -f "$InDir/$File" \) -o \( ! -r "$InDir/$File" \) ] && continue
      Flag=Y
  
      # on pbstore start the next few batches of files staging in advance
      if [ $HOSTNAME == 'pbstore' -o $HOSTNAME == 'cortex' ] ; then
          stage_and_wait
          if [ -n "`sfind $InDir/$File -offline `" ] ; then
              echo "skipping $InDir/$File - staging is too slow"
			  continue
          fi
      fi
  
      echo "file://$InDir/$File sshftp://$2$OutDir/" >> $LisFil
  
  
      [ "`cat $LisFil 2>/dev/null | wc -l`" -eq $BATCH ] && doGlobus $LisFil
    done
    [ "`cat $LisFil 2>/dev/null | wc -l`" -ne 0 ] && doGlobus $LisFil
  done
done

# All done, adjust permissions and exit
ssu $2 "chmod -R g+rw $3" 2>/dev/null
echo "Finished at `date '+%a %T'`"
fail 0 "No more files to be copied!"
