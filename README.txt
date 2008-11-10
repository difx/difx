#### INSTALLATION ##############

To install the latest trunk version of a clean SVN checkout of DiFX, follow
these simple instructions:

1) Ensure you are using bash
2) Ensure you have a working installation of IPP and MPI
3) Open setup-difx.trunk in your favourite text editor and change the 
***ROOT and CXX paths and the perl versions to suit your system
4) If you have not checked-out SVN into a subdirectory called 
32 or 64 (meaning the number of bits), comment out the lines
that alter the SVNROOT path
5) Add "source setup-difx.trunk" to your .bashrc file
6) Execute "source setup-difx.trunk" to get things set up for the open
session
7) Run "./make-difx.trunk"

Thats it. DiFX should compile - if not, check out the troubleshooting
area on http://cira.ivec.org/dokuwiki/doku.php/difx/index.

#### TESTING ####################

See http://cira.ivec.org/dokuwiki/doku.php/difx/benchmarks
