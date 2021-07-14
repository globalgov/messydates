# Contributing

Contributions to `qData`, whether in the form of issue identification, bug fixes, new code or documentation are encouraged and welcome, both from research assistants and (early) users of the package:

* [Submit an issue](#issues)
* [Fix a bug or implement new features](#adding-new-code)
* [Document existing code](#documentation)

This outlines how to propose a change to a package from the Global Governance Observatory's ecosystem. Please note that the `qData` project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Issues
Please use the issue tracker on GitHub to identify problems or suggest new functionality, before submitting changes to the code.
We use issues to identify bugs and tasks, discuss feature requests, and to track implementation of changes.

When submitting an issue, please provide at least a 'Type' label that best describes what the issue is about.
The most useful issues are ones that precisely identify a bug, or propose a test that should pass but instead fails.

## Adding new code
Independent or assigned code contributions are most welcome.
When writing new code, please follow [tidyverse style guide](https://style.tidyverse.org/index.html) which is based on
[standard R guidelines](https://google.github.io/styleguide/Rguide.xml).

It can help to use packages such as `lintr` and `goodpractice` to ensure these are followed. 
These packages are incorporated into the checks made when a pull request is made. 
The `styler` package fixes in a non-invasive way the code to adhere to the tidyverse formatting rules, and it also provides an RStudio Addins to help with this task.
To run the `lintr` and `goodpractice` checks or use `styler` in a file run:

```r
# basic lintr checking
lintr::lint_package(path = "qData/")

# goodpractices checks. Exclude length 80
goodpractice::gp(path = "qData/",
   checks = all_checks()[-c(8)])

# styler fix some of the styling issues
styler::style_file("filePath")
```

## Pull request process
The title of your PR should briefly describe the change. 
Please include a summary of the changes and which issues are fixed and the relevant motivation and context. 
List any dependencies that are required for this change, indicating whether this is a major (breaking), minor, or patch change. 
The body of your PR should contain `Fixes #issue-number`.
A checklist is provided to check off the cases as the different elements listed have been completed to make sure all the steps have been respected.
Make sure the package passes R CMD check by running `devtools::check()` before commiting changes to a pull request. 

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed before openning a pull request. If you’ve found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex).

Please follow the qData pull request guideline (https://github.com/globalgov/qData/blob/main/.github/pull_request_template.md)  

### Branches
We use two **main branches** in this project:

1. The `origin/main` branch is reserved for fully functional releases of the model. 
When the `develop` branch reaches a stable point, a code maintainer merges it back into to the `master` branch, and tags it with a release number there. 

2. The `origin/develop` branch reflects the latest model development stage.
Contributers are encouraged to submit minor changes to this branch that enhance existing functionality.
New features that may break existing functionality should be committed to supporting branches.

We use two types of **supporting branches**:

3. *Feature branches* are used to develop new functionality. They exist as long as the feature is developed, and are then either merged into the `develop` branch for incorporation in a release, or deleted if the feature is abandoned. Feature branches should branch off from `origin/develop`.

4. *Hotfix branches* are used to provide fixes to severe bugs in the `main` branch. That way, the code maintainer does not have to incorporate (potentially unstable) changes from the `develop` branch to fix an issue. Branch names should be prefixed with `hotfix-`.

This branching model is based on: https://nvie.com/posts/a-successful-git-branching-model/.

### Main Branch (code maintainer only)
To create a release version of the code:

1. Ensure that the repository is up-to-date: `git pull`.
2. Switch to the **main** branch: `git checkout master`.
3. Merge changes to the **develop** branch: `git merge --no-ff develop`.
4. Tag release version: `git tag -a VX.Y.Z -m "VERSION-NAME"`.
5. Push changes to this repository `git push origin master --tags`.

### Develop Branch (minor changes to existing functionality)
To make minor changes directly to the `develop` branch, follow standard git procedures:

1. Make sure you switched to the **develop** branch of the project: `git checkout -b develop`.
2. Make sure your local version of the code is up-to-date: `git pull origin develop`.
3. Make your changes
4. Stage your changes for a commit: `git add PATH-TO-CHANGED-FILE`.
5. Commit your changes [using an appropriate message](#commit-messages): `git commit -m "DESCRIPTION"`.
6. Push your commit: `git push origin develop`.

### Feature Branches (new functionality)
To create a new feature branch: `git checkout -b myfeature develop`.

To merge a feature branch back into `develop`:
```
git checkout develop
git merge --no-ff myfeature
git branch -d myfeature
git push origin develop
```

### Hotfix Branches (to fix critical bugs in release versions)
To create a new hotfix branch: `git checkout -b hotfix-VERSION master`.

To merge a hotfix back into `master` (code maintainer only):
```
git checkout master
git merge --no-ff hotfix-VERSION 
git tag -a VERSION
git push origin develop
```
And into develop:
```
git checkout develop
git merge --no-ff hotfix-VERSION
```

Every hotfix should increment the [PATCH digit of the version number](#versioning): a hotfix branch for `V1.3.0` is named `hotfix-V1.3.1`, and the new release is tagged as `V1.3.1`.

Once merged into `master` and `develop`, the hotfix branch can be deleted: `git branch -d hotfix-VERSION`.

### Commit messages
Commits that relate to existing issues should reference the updated status of those issues, and mention the issue number (preceded by a hash symbol: #) in the commit description:

``` Resolved #31 by adding a new function that does things, also updated documentation ```

Where the issue hash (i.e. #31) is preceded by `resolve`, `resolves`, `resolved`, `close`, `closes`, `closed`, `fix`, `fixes`, or `fixed` (capitalised or not), the status of the issue(s) mentioned is updated automatically.
Our current syntactical standard is to mention the issue first and then provide a short description of what the committed changes do in relation to that issue.
Any ancillary changes can be mentioned after a comma.

It should all be written in a single line, like so: #`{verb} {issue} {describe main action/changes}, {additional actions/changes}`.

Note that it is important to write a structured commit message to improve efficiency in collaboration.
Please make sure of:
* Making the title clear and concise with correct reference to an issue as described above. 
* Using a subject line in the description part when the commit message outlines many different changes.
* Explaining in details the changes made and why you made them by using bullet points.
* Separating each paragraph with a blank line.

### Testing 
We use the [testthat](https://testthat.r-lib.org/) package to write unit tests.
By convention, tests are located in [testthat/tests/](qData/tests/testthat).

You should verify that all tests pass before issuing a commit to existing code.
To run all tests for the latest version manually:
```
git pull
library("testthat")
testthat::test_dir("tests/testthat")
```

When writing a new function, consider writing a unit test for that function. 
We follow several conventions for writing tests:

- A unit test file should test one or more aspects of a single function. This makes it easier to identify the source of bugs, and prevents lower-level tests from failing when higher-level functions change.

- The [naming convention](https://www.tidyverse.org/articles/2019/04/testthat-2-1-0/) for test files is: ``test-FILENAME_IN_R_DIRECTORY-FUNCTION_NAME.R``, i.e. test files are named after the file containing the original function in the [R](qData/R) directory, pre-fixed with "test", and optionally post-fixed with the name of the function that is being tested.

- If a test requires auxiliary functions from the package, e.g. to initialize a network with sample data, these belong in a helper file. There should be only one helper file for each `R` file, named ``helper-FILENAME_IN_R_DIRECTORY-FUNCTION_NAME.R``. Re-using existing test data is preferable to creating new data for every test.

## Documentation
A final way of contributing to the package is in developing the vignettes/articles that illustrate the value added in the package. 
Please contact us directly with proposals for updating the documentation, or submit an issue if existing documentation is unclear.

## Versioning
Note that the package is versioned according to [semantic versioning](https://www.jvandemo.com/a-simple-guide-to-semantic-versioning/).
This means that versions follow the Major.Minor.Patch semantic format.

Each minor or major level version is also given a new version name, which should be updated in the `zzz.R` file.

## For developers using MacOS
Develops using MacOS might meet problems compiling the packages since the compiling configuration of R in MacOS is usually incorrect. If one meet an error with error info "/usr/bin/ld: cannot find -lgfortran", then he can either correct the configuration himself or follows the following steps to solve the problem:
1. Run ".libPaths()" command in R and get a path, e.g. one might get "/Library/Frameworks/R.framework/Versions/3.6/Resources/library"
2. If the path one get in the first step is "something/library",  then open the file "something/etc/Makeconf" and comment out the line starting with "FLIBS".  e.g. one might open the file  " /Library/Frameworks/R.framework/Versions/3.6/Resources/etc/Makeconf" and change the line "FLIBS =  -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm" to "#FLIBS =  -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath - lm"