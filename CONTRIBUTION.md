# Contribution Guide

GitHub makes it easy to contribute to the DiFX codebase. 
For frequent contributors, it is probably desirable to join the DiFX project - you can do this by subscribing to the difx-developers mailing list hosted at https://listmgr.nrao.edu/mailman/listinfo/difx-developers and asking to be added to the project (please contact @adamdeller @shaoguangleo @walterfb). 
This will enable you to create branches directly on the main DiFX github project,
otherwise you can fork the project and create branches in your own project.

While we strongly encourage people to join the project directly, it is also possible to fork [DiFX](https://github.com/difx/difx) on Github  then create branches in your own project.  You can do this by cloning a copy of the DiFX repository to your own computer or using [Codespaces](https://docs.github.com/codespaces), a cloud-based in-browser development environment that comes with the appropriate setup to contribute to DiFX.

In either case, once you have implemented and tested a new feature, it can be merged back into the main DiFX codebase by submitting a "Pull Request" to the maintainers for review.  
You probably then want to delete your feature branch once the pull request is approved (which can be done on the command line or GitHub)

A brief overview of the workflow is as follows:

## Initial steps to get a working DiFX installation:

1. *For occasional contributors not part of the main DiFX project only:* Create an account on GitHub if you do not have one yet.
2. *For occasional contributors not part of the main DiFX project only:*  Fork the [DiFX](https://github.com/difx/difx) on GitHub by clicking on the **Fork** button near the top of the page. This creates a copy of the code under your account on the GitHub server.
3. Set up a development environment:
   1. If you forked DiFX: `git clone https://github.com/<your github usename>/difx.git` 
   2. Otherwise: `git clone https://github.com/difx/difx.git`
4. Follow the installation of DiFX.

## Contributing to DiFX

DiFX using the gitflow workflow. Gitflow is a legacy Git workflow that was originally a disruptive and novel strategy for managing Git branches, and DiFX repo will hold two main branches with an infinite lifetime name main and dev.

In the gitflow workflow, there are five different branch types:

- main : contains production-ready code, can be tagged at various commits to signify different versions or releases of the code, and other branches will be merged into the main branch after they have been sufficiently vetted and tested
- dev : contains pre-production code with newly developed features that are in the process of being tested
- feature: most common type, we should start a feature branch off the dev branch, and then merge changes back into dev branch when the features are completed and properly reviewed
- release: preparing new production releases. Typically, the work being performed on release branches concerns finishing touches and minor bugs specific to releasing new code, with code that should be addressed separately from the main development branch. And remember large new features are strictly prohibited.
- hotfix: used to quickly address necessary changes in the main branch. The base of the hotfix branch should be the main branch and should then be merged back into both the main and dev branches. Merging the changes from your hotfix branch back into the develop branch is critical to ensure the fix persists the next time the main branch is released.

Each time you want to contribute, please follow these steps:

### Feature branch:

> Never working in the 'main' branch!!, PR will not be accepted to main branch from a feature branch.

```bash
$ git checkout -b feature-xxx dev
  Switch to a new branch 'feature-xxx'
# coding ....
$ git add file_changed
$ git commit -m 'feature added'
$ git push -u origin feature-xxx

# Then you can create a PR to the main branch
```

### Hotfix branch:

The hotfix branch off from `main` branch, and merge back to `main` and `dev` branch.

Assuming now the main version is `2.8.1`, remember to bump the version to `2.8.2` or `2.8.1.1` after the hotfix.

```bash
$ git checkout -b hotfix-x.x.x.x main
  Switch to a new branch 'hotfix-x.x.x.y'
# bump version to x.x.x.y
# coding ....
$ git commit -am 'bumped version to x.x.x.y'
# coding ....
# fix bug
$ git commit -m 'fixed what problem'

# Then you can create a PR to the main branch

# For the maintainers only
# Finishing a hotfix branch
$ git checkout main
  Switch to branch 'main'
$ git merge --no-ff hotfix-x.x.x.y
  Merge made by recursive.
$ git push origin main
$ git tag -a x.x.x.y
$ git push origin main --tags

# Merge back to dev branch
$ git checkout dev
  Switch to branch 'dev'
$ git merge --no-ff hotfix-x.x.x.y
  Merge made by recurive
  ......
# fix merge conflict
$ git push origin dev

# delete the hotfix-branch
$ git branch --delete hotfix-x.x.x.y
```

### Release branch

> This is for the maintainers only.

The release branch off from `dev` branch, and merge back to `main` and `dev` branch.


```bash
# Create the release branch
$ git checkout -b release-x.x.x dev
 Switched to a new branch 'release-x.x.x'

# Coding for bumped version x.x.x
$ .....

$ git commit -am 'Bumped version to x.x.x'


###############################################
# The following can be done in CLI or Github
# I recommend to do it in Github through PR
###############################################

# Finishing a release branch
$ git checkout main
  Switch to branch 'main'
$ git merge --no-ff release-x.x.x
  Merge made by recursive.
$ git push origin main
$ git tag -a x.x.x
$ git push origin main --tags

# Merge back to dev branch
$ git checkout dev
  Switch to branch 'dev'
$ git merge --no-ff release-x.x.x
  Merge made by recursive
  ......
# fix merge conflict
$ git push origin dev

# delete the release-branch
$ git branch --delete release-x.x.x
```
