use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	   => 'DIFX::Input',
    'VERSION_FROM' => 'DIFX/Input.pm', # finds $VERSION
    'dist'         => { COMPRESS => "gzip -9f", SUFFIX => 'gz'},
);


