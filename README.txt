#### CHECKOUT ##################

See https://github.com/difx/difx/wiki/Installation


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
area on https://github.com/difx/difx/wiki/troubleshooting

#### TESTING ####################

See https://github.com/difx/difx/wiki/benchmarks
