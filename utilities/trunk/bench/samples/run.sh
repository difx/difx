#!/bin/bash

threads='threads1 threads2 threads3 threads4 threads5 threads6 threads7 threads8'
nps='6 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23'

#iohosts='apsr00 apsr01 apsr02 apsr03 apsr04 apsr05'
iohosts='pam1 pam2 pam3 pam4'

for np in $nps
do
 for thread in $threads 
 do

   \rm -f threads 
   ln -s $thread threads

   log=${np}-${thread}-`date -u +%d-%b-%y-%T`
   for host in $iohosts
   do
# Use this for bash on the i/o node
#     xterm -exec "ssh $host ./vstart.sh 2>&1 | tee ${host}-${log}.log" &
# Use this for csh on the i/o node
     xterm -exec "ssh $host ./vstart.sh |& tee ${host}-${log}.log" &
   done

   \rm -f vc082.rpf
   sleep 2
   echo stopmpifxcorr | at now+5min
   startcorr.pl -machinefile machines -np $np /home/vlbi/difx/bin/mpifxcorr vc082.input 2>&1 | tee corr-${log}.log

   sleep 5
    
 done
done
