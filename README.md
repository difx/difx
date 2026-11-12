# DiFX
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16949346.svg)](https://doi.org/10.5281/zenodo.16949346)

The official repo for DiFX.

## Installation

DiFX requires MPI, PGPLOT and IPP.

Details see https://github.com/difx/difx/wiki/difx-installation

### For DiFX Users

At the time of writing, the current stable version of DiFX is DiFX-2.9.1. You can look the DiFX tags for more versions.

We recommend the user to check out the stable version right now. If you want to make a fresh checkout of v2.9.1, just type:

```bash
$ git clone https://github.com/difx/difx
```

as the main branch is on version 2.9.

To update if you had previously cloned the DiFX repository prior to the release of DiFX-2.9.1, update your local repository with git pull and then change to the main branch if necessary:

```bash
$ git pull
$ git checkout main
```

### For DiFX Developers

To install the latest dev branch of DiFX, follow these simple instructions:

```bash
$ git clone https://github.com/difx/difx
$ cd difx
# now there is just one main branch
$ git checkout dev
# Now you are in the dev branch, 
# we will do the development based on the dev branch
# until we want to do a release tag version
# do coding now
```



### Compiling DiFX



#### Site-specific settings: `setup.local.bash`

`setup.bash` (and `setup.csh`) hold generic defaults and are tracked by git, so **do not
edit them for your machine**. Put your own settings in `setup.local.bash` next to
`setup.bash` instead. `setup.bash` reads it if it exists, and git ignores it, so your
local paths cannot be committed or pushed by accident:

```bash
$ cp setup.local.bash.example setup.local.bash
$ $EDITOR setup.local.bash      # e.g. DIFXROOT, IPPROOT, DIFXMPIDIR, CUDAROOT, NVCCFLAGS
```

Set only what differs from the defaults. `DIFX_PREFIX` and `MPICXX` follow `DIFXROOT` and
`DIFXMPIDIR` unless you set them too. To keep the file elsewhere (for example one per
machine, outside the repository), point `DIFX_SETUP_LOCAL` at it before sourcing
`setup.bash`. csh users do the same with `setup.local.csh` (see `setup.local.csh.example`).
Because csh cannot tell where a sourced script lives, either source `setup.csh` from its
own directory or set `DIFX_SETUP_LOCAL`.

```bash
$ source setup.bash # (or .csh).
# You will probably want to add this to your .bashrc or .cshrc files

# Help on options for that last bit are available with ./install-difx --help.
$ ./install-difx --help

# It is highly recommended to build difx in an area other than the source tree (to avoid adding many untracked files)
$ cd /some/directory/in/which/to/build
$ /path/to/difx/repo/install-difx
```



The troubleshooting area is on https://github.com/difx/difx/wiki/Documentation

## TESTING

See https://github.com/difx/difx/wiki/Benchmarks

## Usage

See the user guide of DiFX.



## Maintainer

[@adamdeller](https://github.com/adamdeller)



## Contributing

Feel free to join us!  [Open an issue](https://github.com/difx/difx/issues/new) or submit PRs are always welcome.

Find more details on [CONTRIBUTION](https://github.com/difx/difx/blob/main/CONTRIBUTION.md).



Please follow the [Contributor Covenant](http://contributor-covenant.org/version/1/3/0/) Code of Conduct.

### Contributors

This project exists thanks to all the people who contribute. 
<a href="https://github.com/difx/difx/graphs/contributors"><img src="https://opencollective.com/difx/contributors.svg?width=890&button=false" /></a>


## Citation

If you use DiFX in your research, please consider citing it via DOI [10.5281/zenodo.16949346](https://doi.org/10.5281/zenodo.16949346) and our following papers:

1. DiFX-2: Deller, A. T., W. F. Brisken, C. J. Phillips, John Morgan, W. Alef, R. Cappallo, E. Middelberg et al. "DiFX-2: a more flexible, efficient, robust, and powerful software correlator." Publications of the Astronomical Society of the Pacific 123, no. 901 (2011): 275.

2. DiFX: Deller, Adam T., S. J. Tingay, Matthew Bailes, and C. West. "DiFX: a software correlator for very long baseline interferometry using multiprocessor computing environments." Publications of the Astronomical Society of the Pacific 119, no. 853 (2007): 318.

## License

[GPL3](LICENSE.md) .
