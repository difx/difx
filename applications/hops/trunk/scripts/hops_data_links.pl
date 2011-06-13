#!/usr/bin/perl
#!/usr/bin/perl
#
use warnings;           # turns on optional warnings
use diagnostics;        # and makes them not terse
use strict;             # makes unsafe constructs illegal
use Getopt::Std;	# for standard option processing
#
# A generalized version of Mike's new_make_links.prl which takes an
# alist file as input and create sufficient links to allow work on
# the data in some new location.
#
my %opts;
my $VERSION='$Id: hops_data_links.pl.in 326 2011-06-09 18:58:18Z gbc $'."\n";
my $USAGE="Usage: $0 [options]

where the options are

    -a <file>	to specify an alist file for the data to be linked
    -l <file>	is the script file to be created to make the links
    -d <dir>    destination data directory
    -s <dir>    source data directory

The alist file (-a) is required, and the source and destination
directories cannot be the same.  Either may be specified relative to
the current directory and are automatically converted to absolute paths
for the linkages.  The current directory (.) is the default for both.

If the -l option is omitted, a temporary file is used and deleted
after the links are made.  If present, a script file is created which
you can review and then run yourself.  (Use this if you are chicken.)

In any case the script creates a directory hierarchy starting with
the experiment number in the destination directory with symbolic links
to files of the same name in the source data directory heirarchy.

";
if ( $#ARGV < 0 || $ARGV[0] eq "--help" ) { print "$USAGE"; exit(0); }
if ( $ARGV[0] eq "--version" ) { print "$VERSION" . "\n"; exit(0); }
$opts{'s'} = '.';
$opts{'d'} = '.';
$opts{'l'} = './ln_temp.sh';
getopts('a:e:l:s:d:', \%opts);

# parse arguments
my ($alist, $linkr, $src, $dst);
$alist=$opts{'a'};	    # alist file name
$linkr=$opts{'l'};	    # linker script
$src=$opts{'s'};	    # source directory
$dst=$opts{'d'};	    # dest directory

my ($ref, $rem, $exp, $bnd, $num, $dir, $dat, @aline, %did);

die "Required alist ($alist) is missing\n" if ( ! -f $alist );
die "Source directory ($src) does not exist\n" if ( ! -d $src );

chomp($src = `cd $src ; pwd`) || die "cannot determine source directory\n";

die "Source ($src) and destination ($dst) are the same\n" if ( $src eq $dst );

unlink($linkr) if ( -f $linkr );
open(FILIN,$alist) || die "Alist file \"$alist\" not found\n";
open(FILOUT,">$linkr") || die "Cannot create \"$linkr\" \n";

sub slinky {
    my ($src,$dir,$dat) = @_;
    $did{"$dir/$dat"} = $src;
    return " [ -h $dst/$dir/$dat ] ||\n" .
	   "  ln -s $src/$dir/$dat $dst/$dir/$dat\n";
}

# process certain fields of every line of the alist file
# root[1]...num[3]...expn[7] scan[8]...target[13] baseline[14]...band[16]
while (<FILIN>) {
    chomp;
    next if (/^[\*]/);		    # skip comments
    @aline = split();
    next if ($#aline lt 14);	    # skip partial lines
    print FILOUT "# line " . $. . " of $alist\n";
    $ref = substr($aline[14],0,1);  # reference station of baseline
    $rem = substr($aline[14],1,1);  # remote station of baseline
    $bnd = substr($aline[16],0,1);  # the S,X,... band identifier
    $num = $aline[3];		    # the fourfit file number
    $dir = "$aline[7]/$aline[8]";   # local exp. directory
    # make directory
    print FILOUT "[ -d $dst/$dir ] || mkdir -p $dst/$dir\n";
    # skip missing scans
    print FILOUT "[ -d $src/$dir ] && {\n";
    # target root file
    $dat = "$aline[13].$aline[1]";
    print FILOUT &slinky($src,$dir,$dat) if (not exists($did{"$dir/$dat"}));
    # baseline root file
    $dat = "$aline[14]..$aline[1]";
    print FILOUT &slinky($src,$dir,$dat) if (not exists($did{"$dir/$dat"}));
    # reference station file
    $dat = "$ref..$aline[1]";
    print FILOUT &slinky($src,$dir,$dat) if (not exists($did{"$dir/$dat"}));
    # remote station file
    $dat = "$rem..$aline[1]";
    print FILOUT &slinky($src,$dir,$dat) if (not exists($did{"$dir/$dat"}));
    # fourfit file
    $dat = "$aline[14].$bnd.$num.$aline[1]";
    print FILOUT &slinky($src,$dir,$dat) if (not exists($did{"$dir/$dat"}));
    print FILOUT "}\n"
}
close(FILIN);
close(FILOUT);

if ($linkr eq './ln_temp.sh') {
    chmod(0755,$linkr);
    system($linkr);
    if ($? eq 0) {
	unlink($linkr);
	print "Links made without issues\n";
    } else {
	print "There were issues making the links review $linkr\n";
    }
} else {
    chmod(0755,$linkr);
    print "Created $linkr for you to review and run\n";
}

#
# eof
#
