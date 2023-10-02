# Contribution Guide

Github makes it easy to contribute to the DiFX codebase. For frequent contributors, it is probably desirable to join the DiFX project - you can do this by subscribing to the difx-developers mailing list hosted at https://listmgr.nrao.edu/mailman/listinfo/difx-developers and asking to be added to the project. This will enable you to create branches directly on the main DiFX github project.

While we strongly encourage people to join the project directly, it is also possible to fork [DiFX](https://github.com/difx/difx) on Github and then create branches in your own project.  You can do this by cloning a copy of the DiFX repository to your own computer, or alternatively using [Codespaces](https://docs.github.com/codespaces), a cloud-based in-brower development environment that comes with the appropriated setup to contribute to DiFX.

In either case, once you have implemented and tested a new feature, it can be merged back into the main DiFX codebase by submitting a "pull request".  A brief overview of the workflow is as following:

_Initial steps to get a working DiFX installation:_
1. ***For occasional contributors not part of the main DiFX project only:*** Create an account on Github if you do not have one yet.
2. ***For occasional contributors not part of the main DiFX project only:***  Fork the [DiFX](https://github.com/difx/difx) on Github by clicking on the **Fork** button near the top of the page. This creates a copy of the code under your account on the GitHub server.
3. Set up a development environment:
   1. ***If you forked DiFX:*** `git clone https://github.com/<your github usename>/difx.git` ***Otherwise:*** `git clone https://github.com/difx/difx.git`
4. Follow the installation of DiFX.

_Then each time you want to make a contribution:_
5. Create a branch to hold your changes, and start making changes. **Never** work in the 'main' branch!!
   1. `git checkout -b feature-xxx origin/main` based on main branch or
   2. `git checkout -b feature-xxx origin/dev` based on dev branch
6. Work on this task using Git to do the version control, push them to github using:
   1. `git add file_changed`
   2. `git commit -m 'messges'`
   3. `git push -u origin feature-xxx`
7. Finally, go to the github page of the project you are working under (the main DiFX project for regular contributors, or your own project if you forked it), and click 'Pull request' to send your changes to the maintainers for review.
8. You probably then want to delete your feauture branch once the pull request is approved (which can be done on the command line or on github)
