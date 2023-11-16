# DiFX

The official repo for DiFX.

## Installation

DiFX requires MPI, PGPLOT and IPP.

Details see https://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/installation.

### For DiFX Users

At the time of writing, the current stable version of DiFX is DiFX-2.8.1. You can look the DiFX tags for more version.

Then we recommend the user to check out stable version right now. If you want to check out v2.8.1, just type :

```bash
$ git clone https://github.com/difx/difx -b v2.8.1
```

### For DiFX Developers

To install the latest dev branch of DiFX, follow these simple instructions:

```bash
$ git clone https://github.com/difx/difx
$ cd difx
# now there are just one main branch
$ git checkout -b dev origin/dev
# Now you are in dev branch, 
# we will do the development based on dev branch
# until we want do a release tag version
# do coding now
```



### Compiling DiFX



```bash
# make sure the setting in setup.bash is correct
$ source setup.bash # (or .csh). 
#You will probably want to add this to your .bashrc or .cshrc files

# Help on options for that last bit are available with ./install-difx --help.
$ ./install-difx
```



The troubleshooting area on http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/start

#### TESTING ####################

See http://www.atnf.csiro.au/dokuwiki/doku.php/difx/benchmarks

## Usage

See the userguide of DiFX.



## Maintainer

[@adamdeller](https://github.com/adamdeller)



## Contributing

Feel free to join us!  [Open an issue](https://github.com/difx/difx/issues/new) or submit PRs is always welcome.

Find more details on [CONTRIBUTION](https://github.com/difx/difx/CONTIBUTION.md) .



Please follows the [Contributor Covenant](http://contributor-covenant.org/version/1/3/0/) Code of Conduct.

### Contributors

This project exists thanks to all the people who contribute. 
<a href="https://github.com/difx/difx/graphs/contributors"><img src="https://opencollective.com/difx/contributors.svg?width=890&button=false" /></a>

## License





