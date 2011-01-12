#!/usr/bin/perl -w

use blib;

use strict;

use DIFX::Input;

my $filename = shift @ARGV;

my $input = new DIFX::Input($filename);

