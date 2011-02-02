#!/usr/bin/perl -wpi

use strict;

s/^(CALC FILENAME:\s*)\/.*\/([^\/]+)$/$1$2/;
s/^(CORE CONF FILENAME:\s*)\/.*\/([^\/]+)$/$1$2/;
s/^(OUTPUT FILENAME:\s*)\/.*\/([^\/]+)$/$1$2/;
s/^(VEX FILE:\s*)\/.*\/([^\/]+)$/$1$2/;
s/^(IM FILENAME:\s*)\/.*\/([^\/]+)$/$1$2/;
s/^(FLAG FILENAME:\s*)\/.*\/([^\/]+)$/$1$2/;
