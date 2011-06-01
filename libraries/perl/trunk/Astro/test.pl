#!/usr/local/perl -w

use Astro::Time;
use Astro::Coord;
use Astro::Misc;

use strict;

my ($day, $month, $dayno, $year, $ut, $mjd, $dUT1, $gst, $lmst, $i);
my ($turn, $deg, $rad, $hour, $minute, $sec, $l, $b);
my ($str, $ra, $dec, $x, $y, $az, $el, $ha, $nra, $ndec);

$day = 17;
$month = 11;
$year = 1997;

my $longitude = 0.417;
my $latitude = 0.141;

$str = '12:34:45.4';
$turn = str2turn($str, 'H');
printf "str2turn:   $str ==> %.4f\n", $turn;
$str = turn2str($turn, 'H', 1);
printf "turn2str:   %.4f ==> $str\n", $turn;

$str = '-45 34 12.9';
$turn = str2turn($str, 'D');
printf "str2turn:   $str ==> %.4f\n", $turn;
$Astro::Time::StrSep = ' ';
$str = turn2str($turn, 'D', 1);
printf "turn2str:   %.4f ==> $str\n", $turn;

$str = '13h23m45.3s';
$turn = str2turn($str, 'H');
printf "str2turn:   $str ==> %.4f\n", $turn;
$Astro::Time::StrSep = 'hms';
$str = turn2str($turn, 'H', 1);
printf "turn2str:   %.4f ==> $str\n", $turn;

$str = '13d03\'45.3"';
$turn = str2turn($str, 'H'); # Note 'H' gets ignored
printf "str2turn:   $str ==> %.4f\n", $turn;
$str = turn2str($turn, 'D', 1, 'deg');
printf "turn2str:   %.4f ==> $str\n", $turn;

$hour = 12;
$minute = 33;
$sec = 47;
$ut = hms2time($hour, $minute, $sec);
printf("hms2time:   %02d/%02d/%02d ==> %s\n", $hour, $minute, $sec, 
       turn2str($ut, 'H', 0, ':'));

$rad = 0.45;
$deg = rad2deg($rad);
printf "rad2deg:    $rad Radians ==> %.2f degrees\n", $deg;
$turn = deg2turn($deg);
printf "deg2turn:   %.2f degrees  ==> %.3f Turns\n", $deg, $turn;

$dayno = cal2dayno($day, $month, $year);
print "cal2dayno:  $day/$month/$year ==> $dayno/$year\n";

($day, $month) = dayno2cal($dayno, $year);
print "dayno2cal:  $dayno/$year ==> $day/$month/$year\n";

print "yesterday:  The day before $day/$month/$year is ";
($day, $month, $year) = yesterday($day, $month, $year);
print "$day/$month/$year\n";

print "Tomorrow:   The day after $dayno/$year is ";
($dayno, $year) = tomorrow($dayno, $year);
print "$dayno/$year\n";

print "leap:\n";
for ($i=1996; $i<2001; $i++) {
  if (leap($i)) {
    print " $i is a leap year\n";
  } else {
    print " $i is NOT a leap year\n";
  }
}

$mjd = now2mjd();
printf "now2mjd:    Current MJD = %.2f\n", $mjd;

$Astro::Time::StrSep = 'hms';

($day, $month, $year, $ut) = mjd2cal($mjd);
printf("mjd2cal:    MJD %.2f ==> %02d/%02d/$year (%s)\n", $mjd, $day,
       $month, turn2str($ut, 'H', 0));

$mjd = cal2mjd($day, $month, $year, $ut);
printf("cal2mjd:    %02d/%02d/$year (%s) ==> MJD %.2f \n", $day, $month,
       turn2str($ut, 'H', 0), $mjd);

$Astro::Time::StrSep = ':';
($dayno, $year, $ut) = mjd2dayno($mjd);
printf("mjd2dayno:  MJD %.2f ==> %03d/$year (%s)\n", $mjd, $dayno,
       turn2str($ut,'H',0));

printf("mjd2time:   MJD %.2f ==> %s\n", $mjd, mjd2time($mjd));
printf("mjd2vextime:MJD %.2f ==> %s\n", $mjd, mjd2vextime($mjd));

$mjd = dayno2mjd($dayno, $year, $ut);
printf("dayno2mjd:  %03d/$year (%s) ==> MJD %.2f\n", $dayno,
       turn2str($ut,'H',0), $mjd);

my $mstr = month2str($month);
printf("month2str:  $month is $mstr\n");

$dUT1 = -0.2;
$gst = gst($mjd, $dUT1);
printf("gst:        MJD %.2f (dUT1=$dUT1) ==> GMST %s\n", $mjd,
       turn2str($gst,'H',0));

$Astro::Time::StrSep = 'hms';

$lmst = cal2lst($day, $month, $year, $ut, $longitude);
printf("cal2lst:    $day/$month/$year (%s) at %s ==> LMST %s\n",
       turn2str($ut, 'H', 0), turn2str($longitude, 'D', 1, ':'),
       turn2str($lmst, 'H', 0));

$lmst = dayno2lst($dayno, $year, $ut, $longitude);
printf("dayno2lst:  $dayno/$year (%s) at %s ==> LMST %s\n",
       turn2str($ut, 'H', 0), turn2str($longitude, 'D', 1, ':'),
       turn2str($lmst, 'H', 0));

$lmst = mjd2lst($mjd, $longitude, $dUT1);
printf("mjd2lst:    MJD %.6f at %s ==> LMST %s\n", $mjd,
       turn2str($longitude, 'D', 1, ':'), turn2str($lmst, 'H', 2));

$mjd = lst2mjd($lmst, $dayno, $year, $longitude);
printf("lst2mjd:    LST %s on $dayno/$year \@ %s ==> MJD %.6f\n",
       turn2str($lmst,'H',2,'hms'), turn2str($longitude,'D',0,':'),
       $mjd);

$Astro::Time::StrSep = ':';

$ra = 0.5356;
$dec = 0.1025;

my ($lst_rise, $lst_set) = rise($ra, $dec, $latitude, deg2turn(15));
printf("rise:       %s,%s at %s rise between %s - %s lst\n",
       turn2str($ra, 'H', 0), turn2str($dec, 'D', 0),
       turn2str($latitude, 'D', 0),
       turn2str($lst_rise, 'H', 0), turn2str($lst_set, 'H', 0));

$x = deg2turn(-15.585);
$y = deg2turn(+11.507);
($az, $el) = xy2azel($x, $y);
printf("xy2azel:    %s,%s ==> %s,%s\n", turn2str($x,'D',0),
       turn2str($y,'D',0), turn2str($az,'D',0), turn2str($el,'D',0));

($x, $y) = azel2xy($az, $el);
printf("azel2xy:    %s,%s ==> %s,%s\n", turn2str($az,'D',0), 
       turn2str($el,'D',0), turn2str($x,'D',0), turn2str($y,'D',0));

($ha, $dec) = eqazel($az, $el, $latitude);
printf("eqazel:     %s,%s ==> %s,%s (azel->hadec)\n", turn2str($az,'D',0),
       turn2str($el,'D',0), turn2str($ha,'H',0), turn2str($dec,'D',0));

($az, $el) = eqazel($ha, $dec, $latitude);
printf("eqazel:     %s,%s ==> %s,%s (hadec->azel)\n", turn2str($ha,'H',0),
       turn2str($dec,'D',0), turn2str($az,'D',0), turn2str($el,'D',0));

($nra, $ndec) = fk4fk5($ra, $dec);
printf("fk4fk5:     %s,%s => %s,%s\n", turn2str($ra,'H',0),
       turn2str($dec,'D',0), turn2str($nra,'H',0), turn2str($ndec,'D',0));

($ra, $dec) = fk5fk4($nra, $ndec);
printf("fk5fk4:     %s,%s => %s,%s\n", turn2str($nra,'H',0),
       turn2str($ndec,'D',0), turn2str($ra,'H',0), turn2str($dec,'D',0));

($l, $b) = fk4gal($ra, $dec);
printf("fk4gal:     %s,%s => %s,%s\n", turn2str($ra,'H',0),
       turn2str($dec,'D',0), turn2str($l,'D',0), turn2str($b,'D',0));

($ra, $dec) = galfk4($l, $b);
printf("galfk4:     %s,%s => %s,%s\n", turn2str($l,'D',0),
       turn2str($b,'D',0), turn2str($ra,'H',0), turn2str($dec,'D',0));



$dec = 0.2;
($az, $el) = coord_convert($ra, $dec, 4, 1, $mjd, $longitude, $latitude,
			   0.00005);
printf("coord_convert:  Source %s, %s\n",
       turn2str($ra,'H',0), turn2str($dec,'D',0));
printf("                   MJD %.2f\n", $mjd);
printf("           Observatory %s, %s\n",
       turn2str($longitude,'D',0,'deg'), turn2str($latitude,'D',0,'deg'));
printf("             ==> az,el %s, %s\n",
       turn2str($az,'D',0), turn2str($el,'D',0));


