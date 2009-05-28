#!/bin/sh -f
echo "Starting on `hostname` at `date`"
sleep 25
/home/vlbi/bin/vlbi_fake -year 2009 -dayno 60 -time 01:00:00 -f 2 -n 8 -dur 121 -p 52100
sleep 5
