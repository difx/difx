use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	   => 'Astro::VexParser',
    'VERSION_FROM' => 'VexParser.pm', # finds $VERSION
    'LIBS'	   => ["-L$ENV{DIFXROOT}/lib -lvex -lfl"],
    'DEFINE'	   => '',
    'INC'	   => '-I../vexlib',
    'dist'         => { COMPRESS => "gzip -9f"},
);
