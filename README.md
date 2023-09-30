# DiFX

The official repo for DiFX.

## Installation

DiFX requires MPI, PGPLOT and IPP.

Details see https://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/installation.

### Users

At the time of writing, the current stable version of DiFX is DiFX-2.8.1. You can look the DiFX tags for more version.

Then we recommend the user to check out stable version right now. If you want to check out v2.8.1, just type :

```bash
$ git clone https://github.com/difx/difx -b v2.8.1
```

### Developers

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
$ source setup/setup.bash # (or .csh). 
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

The preferred way to contribute to DiFX is to fork the [DiFX](https://github.com/difx/difx) on Github, then submit a "pull request"(PR). You can do this by cloning a copy of the DiFX repository to your own computer, or alternatively using [Codespaces](https://docs.github.com/codespaces), a cloud-based in-brower development environment that comes with the appropriated setup to contribute to DiFX.

A brief overview of the workflow is as following:

1. Create an account on Github if you do not have one yet.
2. Fork the [DiFX](https://github.com/difx/difx) on Github by clicking on the **Fork** button near the top of the page. This creates a copy of the code under your account on the GitHub server.
3. Set up a development environment:
   1. `git clone https://github.com/<your github usename>/difx.git`
4. Follow the installation of DiFX.
5. Create a branch to hold your changes, and start making changes. **Never** work in the 'main' branch!!
   1. `git checkout -b feature-xxx origin/main` based on main branch or
   2. `git checkout -b feature-xxx origin/dev` based on dev branch
6. Work on this task using Git to do the version control, push them to your DiFX fork using:
   1. `git add file_changed`
   2. `git commit -m 'messges'`
   3. `git push -u origin feature-xxx`
7. Finally, go to the web page of your fork of DiFX, and click 'Pull request' to send your changes to the maintainers for review.

Please follows the [Contributor Covenant](http://contributor-covenant.org/version/1/3/0/) Code of Conduct.

### Contributors

This project exists thanks to all the people who contribute. 
<a href="https://github.com/difx/difx/graphs/contributors"><img src="https://opencollective.com/difx/contributors.svg?width=890&button=false" /></a>

## License





