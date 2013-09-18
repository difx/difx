#!/bin/bash +x

# vex2script automated test script

#=============================================================================
# compare function
# param 1: project; param 2:antenna param 3: vex2script params
function compare {
  SCRIPT=`echo ${1} | tr '[:lower:]' '[:upper:]'`
  ${VEX2S} ${1}.vex ${3} &> /dev/null
  # remove unused output
  for A in SC HN NL FD LA PT KP OV BR MK GB; do
    if [ $2 != $A ]; then
      rm -f ${SCRIPT}.${A}.py
    fi
  done
  # remove comments
  cat ${SCRIPT}.${2}.py.out | `which grep` -v "^#" > /tmp/project.py.orig
  cat ${SCRIPT}.${2}.py     | `which grep` -v "^#" > /tmp/project.py
  #compare
  if [ "x`diff -q /tmp/project.py.orig /tmp/project.py`" != "x" ]; then
    echo "  ATTENTION - output has changed!  <--------------"
    diff ${SCRIPT}.${2}.py ${SCRIPT}.${2}.py.out &> /tmp/${SCRIPT}.${2}.diff
    echo "  Check /tmp/${SCRIPT}.${2}.diff"
  else
    echo "  Output unchanged"
  fi
  rm -f /tmp/project.py.orig /tmp/project.py
}

#=============================================================================
# Verified outputs have a ".out" extensions. Current build outputs have the regular
# output names, <filename>.<ant>.py. Compared these two to find deviations. If
# any deviations are found and verified to be by design, copy the new output
# files to ".out".
echo""
echo""
echo "==== vex2script Automated Test Script (v0.1) ========="
echo""

if [ "x" != "x${1}" ]; then
	VEX2S=${1}
else
	VEX2S=`which vex2script`
fi
echo "  remove old output files"
rm -f *.py
echo""
echo "Using: " ${VEX2S}
echo""

#=============================================================================
# GB test: xh000.vex
echo -n "GB Test 1 - DDC, VDIF                           "
compare xh000 GB

#=============================================================================
# GB test: bm352g0.vex
echo -n "GB Test 2 - PFB, 2IF                            "
compare bm352g0 GB

#=============================================================================
# USNO test: n3204.vex
echo -n "USNO Test (PFB, 2IF (4/12))                     "
compare n3204 PT

#=============================================================================
# PFB - 1 IF
echo -n "PFB 1IF rec (1 and 2 IF)                        "
compare bl175cv SC
   
#=============================================================================
# PFB - 2 IF: bl188b.vex
echo -n "PFB 2IF rec                                     "
compare bl188b MK
   
#=============================================================================
# PFB - 1 IF (mix of 1 and 2 IF) - non-recording: br145ya.vex
echo -n "PFB 1IF nonrec                                  "
compare br145ya KP --mark5a

#=============================================================================
# PFB - 2 IF - non-recording: bd170ar.vex 
echo -n "PFB 2IF nonrecc (8/8)                           "
compare bd170ar LA --mark5a

#=============================================================================
echo -n "pointing - PFB                                  "
compare tjul28p KP --mark5a

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 1 IF - 1 channel         "
compare bn048c.1Ch.1 FD

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 1 IF - 2 channel         "
compare bn048c.2Ch.2 MK

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 1 IF - 3 channel         "
compare bn048c.3Ch.3 LA

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 1 (and 2) IF - 4 channel "
compare bn048c LA

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 2 IF (1/1) - 2 channel   "
compare bm353o.2Ch.1.1 KP

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 2 IF (2/1) - 3 channel   "
compare bm353o.3Ch.2.1 FD

#=============================================================================
echo -n "DDC - 1 DBE - Mark5B - 2 IF (2/2) - 4 channel   "
compare bm353o OV

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 1 IF - 1 channel           "
compare tt001c.1ch NL

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 1 IF - 2 channel           "
compare tt001c.2ch NL

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 1 IF - 3 channel           "
compare tt001c.3ch NL

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 1 IF - 4 channel           "
compare tt001c.4ch NL

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 2 IF - 2 channel           "
compare tt001c.2ch.1.1 BR

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 2 IF (1/2) - 3 channel     "
compare tt001c.3ch.1.2 FD

#=============================================================================
echo -n "DCC - 1 DBE - VDIF - 2 IF (2/1) - 3 channel     "
compare tt001c.3ch.2.1 SC

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 2 IF (1/3) - 4 channel     "
compare tt001c.4ch.1.3 OV

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 2 IF (2/2) - 4 channel     "
compare tt001c.4ch PT

#=============================================================================
echo -n "DDC - 1 DBE - VDIF - 2 IF (3/1) - 4 channel     "
compare tt001c.4ch.3.1 HN

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 1 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 2 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 3 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 4 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 5 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 6 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 7 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 1 IF - 8 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 1 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 2 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 3 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 4 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 5 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 6 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 7 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 2 IF - 8 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 3 IF - 3 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 3 IF - 4 channel

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 3 IF (2/2/1) - 5 channel   "
compare tx006c.5ch.2.2.1 HN

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 3 IF (3/1/2) - 6 channel   "
compare tx006c.6ch.3.1.2 OV

#=============================================================================
# DDC - 2 DBE - VDIF - 3 IF - 7 channel

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 3 IF (5/1/2) - 8 channel   "
compare tx006c.8ch.5.1.2 HN

#=============================================================================
# DDC - 2 DBE - VDIF - 4 IF (1/1/1/1) - 4 channel

#=============================================================================
# DDC - 2 DBE - VDIF - 4 IF - 5 channel

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 4 IF (2/1/2/1)- 6 channel  "
compare tx006c.6ch.2.1.2.1 HN

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 4 IF (2/2/1/1)- 6 channel  "
compare tx006c.6ch.2.2.1.1 HN

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 4 IF (2/2/2/1)- 7 channel  "
compare tx006c.7ch.2.2.2.1 HN

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 4 IF (2/2/2/2)- 8 channel  "
compare tx006c.8ch.2.2.2.2 HN

#=============================================================================
echo -n "DDC - 2 DBE - VDIF - 4 IF (1/3/2/2)- 8 channel  "
compare tx006c.8ch.1.3.2.2 NL

#=============================================================================
echo -n "Some antennas excluded from particular modes 1  "
compare dq338 SC

#=============================================================================
echo -n "Some antennas excluded from particular modes 2  "
compare gs032a1 FD
