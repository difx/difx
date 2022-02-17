#!/bin/bash -
#//=======================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $Author$
# $HeadURL$
# $LastChangedRevision$
# $LastChangedBy$ 
# $LastChangedDate$
#
#=========================================================================
#=========================================================================
#
#          FILE: shao_mark6_unmount
#
#         USAGE: ./shao_mark6_mount
#
#=========================================================================

set -o nounset                                  # Treat unset variables as an error

# get all the disks
# [0:0:8:0]    disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdb 
# [0:0:9:0]    disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdc 
# [0:0:10:0]   disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdd 
# [0:0:11:0]   disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sde 
# [0:0:12:0]   disk    ATA      ST4000NM0035-1V4 TN03  /dev/sdj 
# [0:0:13:0]   disk    ATA      ST4000NM0035-1V4 TN03  /dev/sdk 
# [0:0:14:0]   disk    ATA      ST4000NM0035-1V4 TN03  /dev/sdl 
# [0:0:15:0]   disk    ATA      ST4000NM0035-1V4 TN02  /dev/sdm 
# [1:0:0:0]    cd/dvd  TSSTcorp CDDVDW SH-224DB  SB01  /dev/sr0 
# [2:0:0:0]    disk    ATA      WDC WD5000YS-01M 2E07  /dev/sda 
# [16:0:0:0]   process Marvell  Console          1.01  -        
# [17:0:8:0]   disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdf 
# [17:0:9:0]   disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdg 
# [17:0:10:0]  disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdh 
# [17:0:11:0]  disk    ATA      WDC WD4003FZEX-0 1A01  /dev/sdi 
# [17:0:12:0]  disk    ATA      ST4000NM0035-1V4 TN02  /dev/sdn 
# [17:0:13:0]  disk    ATA      ST4000NM0035-1V4 TN02  /dev/sdo 
# [17:0:14:0]  disk    ATA      ST4000NM0035-1V4 TN02  /dev/sdp 
# [17:0:15:0]  disk    ATA      ST4000NM0035-1V4 TN02  /dev/sdq 
aa=`lsscsi| grep -v cd | grep -v -w sda| grep disk| awk '{print $NF}' `

echo "There are disks in the following:"
echo $aa
index=1
for i in $aa
do
    arr[$index]=$i
    let "index+=1"
done

let module_no=index/8

echo 'There are total ' $index ' disks, '${module_no} 'module(s) in total'

echo 'Begin mounting...'

for ((module=1;module<=${module_no};module++))
do
    for ((disk=0;disk<=7;disk++))
    do
        let no=(module-1)*8+disk+1
        echo $no ': mount' $module'/'$disk
        echo 'mounting ' ${arr[$no]}'1' ' => /mnt/disks/'$module/$disk
        mount ${arr[$no]}1 /mnt/disks/$module/$disk
        echo 'mounting ' ${arr[$no]}'2' ' => /mnt/disks/.meta/'$module/$disk
        mount ${arr[$no]}2 /mnt/disks/.meta/$module/$disk
    done
done
