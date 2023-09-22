#!/bin/sh -f
echo "Starting on `hostname` at `date`"
sleep 7
echo "Running: vlbi_fake $@"
vlbi_fake $@
sleep 5
