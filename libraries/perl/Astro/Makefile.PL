use ExtUtils::MakeMaker;

WriteMakefile(
    NAME      => 'Astro',
    VERSION   => '0.78',
    DISTNAME  => 'Astro',
    dist      => { COMPRESS => 'gzip -9f', SUFFIX => 'gz'}
);
