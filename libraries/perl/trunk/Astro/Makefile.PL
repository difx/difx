use ExtUtils::MakeMaker;

WriteMakefile(
    NAME      => 'Astro',
    VERSION   => '0.77',
    DISTNAME  => 'Astro',
    dist      => { COMPRESS => 'gzip -9f', SUFFIX => 'gz'}
);
