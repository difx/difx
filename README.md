# DiFX

The official repo for DiFX.

## Installation

DiFX requires MPI, PGPLOT and IPP.

Details see https://github.com/difx/difx/wiki/Installation

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
$ git checkout dev
# Now you are in dev branch, 
# we will do the development based on dev branch
# until we want do a release tag version
# do coding now
```



### Compiling DiFX



```bash
# make sure the setting in setup.bash is correct (you will probably need to modify e.g. IPPROOT, MPICXX, etc)
$ source setup.bash # (or .csh). 
#You will probably want to add this to your .bashrc or .cshrc files

# Help on options for that last bit are available with ./install-difx --help.
$ ./install-difx --help

# It is highly recommended to build difx in an area other that the source tree (to avoid adding many untracked files)
$ cd /some/directory/in/which/to/build
$ /path/to/difx/repo/install-difx
```



The troubleshooting area on https://github.com/difx/difx/wiki/Documentation

## TESTING

See https://github.com/difx/difx/wiki/Benchmarks

## Usage

See the userguide of DiFX.



## Maintainer

[@adamdeller](https://github.com/adamdeller)



## Contributing

Feel free to join us!  [Open an issue](https://github.com/difx/difx/issues/new) or submit PRs is always welcome.

Find more details on [CONTRIBUTION](https://github.com/difx/difx/blob/main/CONTRIBUTION.md) .



Please follows the [Contributor Covenant](http://contributor-covenant.org/version/1/3/0/) Code of Conduct.

### Contributors

This project exists thanks to all the people who contribute. 
<a href="https://github.com/difx/difx/graphs/contributors"><img src="https://opencollective.com/difx/contributors.svg?width=890&button=false" /></a>


## Citation

If you use DiFX in your research, please consider citing our following papers:

1. DiFX-2: Deller, A. T., W. F. Brisken, C. J. Phillips, John Morgan, W. Alef, R. Cappallo, E. Middelberg et al. "DiFX-2: a more flexible, efficient, robust, and powerful software correlator." Publications of the Astronomical Society of the Pacific 123, no. 901 (2011): 275.

2. DiFX : Deller, Adam T., S. J. Tingay, Matthew Bailes, and C. West. "DiFX: a software correlator for very long baseline interferometry using multiprocessor computing environments." Publications of the Astronomical Society of the Pacific 119, no. 853 (2007): 318.

## License

[GPL3](LICENSE.md) .
