################################
# Script for running hammer.sh
# for all modules loaded on the
# Mark6 machine
# Can be run as user oper
################################
echo Stoping mark5daemon
stopMk5daemon
sleep 5
echo Starting mark5daemon
startMk5daemon
sleep 10
echo Verifying number of disks
count=($(mount | grep disks | wc -l))
count=$(($count / 2))
n=$(($count%8))

until [ $n -eq 0 ]
do
    echo "Found $count disks (incomplete number of modules). Trying again."
    sleep 5
    count=($(mount | grep disks | wc -l))
    count=$(($count / 2))
    n=$(($count%8))
done
numMod=$(($count / 8))
echo Found $count disks = $numMod modules.
printf "\\n\\n"
echo "---------------------------------------"
echo "Ready for conditioning the modules"
echo "---------------------------------------"
echo "By proceeding you will be conditioning:"
echo $numMod modules on host `hostname`
echo "---------------------------------------"
read -p "Do you want to proceed? [y/n] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    printf "\n"
    echo "Remounting all Mark6 disks in rw mode"
    `sudo /home/oper/bin/remountMk6rw`
    cd /home/oper/logs
    echo "Starting conditioning"
    echo "Check LED activity on $numMod modules on host `hostname`"
    echo Starting nohup sudo /home/oper/bin/hammer.dbg.sh purge=true $* &
    nohup sudo /home/oper/bin/hammer.sh purge=true $* &
    echo "Log files are written in /home/oper/logs"
fi
printf "\n"
