#### CHECKOUT ##################

See https://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/installation.
E.g. for current trunk, a tagged release or everything:
    svn co https://svn.atnf.csiro.au/difx/virtualtrunk
    svn co https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.6.2
    svn co https://svn.atnf.csiro.au/difx

If you checked out everything and want to recover space for nolonger
needed releases:

    cd ...path-to.../master_tags
    svn co https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.6.1 --depth empty

#### INSTALLATION ##############

To install the latest trunk version of a clean SVN checkout of DiFX, follow
these simple instructions:

1) Ensure you have a working installation of IPP and MPI
2) cd setup
3) Open setup.bash or setup.csh in your favourite text editor and change the 
DIXFROOT, IPPROOT MPICXX and PGPLOT paths and the perl versions to suit your system
4) source setup/setup.bash (or .csh). You will probably want to add
this to your .bashrc or .cshrc files
5) Run "./install-difx

Help on options for that last bit are available with ./install-difx --help.

Thats it. DiFX should compile - if not, check out the troubleshooting
area on http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/start

#### TESTING ####################

See http://www.atnf.csiro.au/dokuwiki/doku.php/difx/benchmarks
