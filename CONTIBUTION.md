# Contribution Guide for user not belong to DiFX developers

The preferred way to contribute to DiFX is to fork the [DiFX](https://github.com/difx/difx) on Github, then submit a "pull request"(PR). You can do this by cloning a copy of the DiFX repository to your own computer, or alternatively using [Codespaces](https://docs.github.com/codespaces), a cloud-based in-brower development environment that comes with the appropriated setup to contribute to DiFX.

A brief overview of the workflow is as following(this if for the user who do not belong the DiFX developer group):

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