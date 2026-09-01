# Contributing

Contributions to `messydates`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Git

`globalgov` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

## Fork

### Cloning
Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 
This command allows you to `pull` changes from the remote repository to your local repository.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose main or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

## Issues and tests

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.

The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

## Bug fixing or adding new code

Independent or assigned code contributions are most welcome.
When writing new code, please follow 
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/). 
It can help to use packages such as `lintr`, `goodpractice` and `formatR` 
to ensure these are followed.

## Documentation

A final way of contributing to the package is in developing the 
vignettes/articles that illustrate the value added in the package. 
Please contact me with any proposals here.

Please note that the `messydates` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Package architecture

### Project overview

`messydates` is an R package implementing ISO 8601-2:2019(E) "messy" dates and times.
It introduces an `mdate` S3 class (a character vector under the hood) that can represent
unspecified components (`X`), approximate (`~`) and uncertain (`?`, or both `%`) annotations,
ranges (`..`), open-ended ranges (`..2019-01-01` / `2009-01-01..`), sets (`{}`/`[]`),
and sub-day times of day with the same annotation system.

The design principle is: retain and reason about imprecision,
and only resolve to a single precise date/time when the user explicitly asks for it
(via `min`/`max`/`mean`/`median`/`random` resolution).
New functionality should preserve that principle rather than resolving imprecision early
or discarding annotations.

### Consistency

We are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`as_*()`, `year()`/`month()`/`day()`, `expand()`/`contract()`, etc.)
should share argument order and naming, so that behaviour is guessable across the family.

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Install dependencies / load for development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "time-parse")` (matches `test-time-parse.R`),
  or `testthat::test_file("tests/testthat/test-time-parse.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Code coverage: `covr::package_coverage()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.

Note that `README.md` is generated from `README.Rmd` — edit `README.Rmd` and re-knit
(`devtools::build_readme()`), never edit `README.md` directly.

`DESCRIPTION` sets `Config/testthat/parallel: true` for CI.
Some local setups cannot spawn the parallel testthat subprocesses;
if the suite fails to start rather than failing a test, force sequential execution
with `Sys.setenv(TESTTHAT_PARALLEL = "false")` before `testthat::test_dir("tests/testthat")`
rather than changing the `DESCRIPTION` setting.
`devtools::check()` is unaffected.

### File organization (file naming = verb/purpose)

`R/` files are grouped by verb/purpose, not by exported function,
so related functions across the public API often live in different files that share a prefix:

| Prefix | Contains |
|---|---|
| `class_*.R` | the `mdate` class itself: `class_mdate.R` holds the constructor/validator (`new_messydate()`/`validate_messydate()`/`make_messydate()`) *and* the S3 dispatch methods (`print`/`format`/`Ops`/comparison); `class_methods.R` covers basic vector semantics (`[`, `[<-`, `c()`, `rep()`) so that `mdate` behaves like an ordinary vector; duration handling lives in `class_mduration.R` |
| `coerce_to_messydate.R` | the largest file: `as_messydate()`, the main user-facing entry point that parses character strings, `Date`, `POSIXct`/`POSIXlt` into `mdate`, including the prose parser (Roman numerals, "circa", "the Ides of March", decades/centuries, "before X", comma-separated prose lists, etc. — see the roxygen `@section Parsing prose` there for the full list of supported conventions) |
| `coerce_from_messydate.R` | the inverse: `mdate` → `Date`/`POSIXct`/`POSIXlt` |
| `validate_input.R` | input validation shared across the coercion pipeline, and `md_problems()`, which reports what could not be parsed |
| `resolve_extrema.R`, `resolve_tendency.R` | resolution functions (`vmin()`/`vmax()`/`vmedian()`/`vmean()`/`random()`, etc.) used to collapse a messy date/range/set to one precise value |
| `component_extract.R`, `component_annotate.R` | pulling out or tagging individual date/time components (year, month, day, hour, …) and their annotation state |
| `convert_expand.R`, `convert_contract.R`, `convert_sequence.R` | expanding an `mdate` (range/set/approximate window) out to all compatible concrete dates, contracting a vector of dates back into the most compact messy representation, and generating `seq()`-style sequences over `mdate`s. `convert_expand.R` is the second-largest file and encodes the "imputation window" logic (default 3 years for year-only approximation, 3 years + 3 months for year-month approximation, etc.) for approximate/censored dates |
| `operate_*.R` | arithmetic on mdates and durations (`operate_arithmetic.R`), inequality/comparison operators (`operate_comparison.R`), set operations on mdate sets/ranges (`operate_set.R`), proportional overlap (`operate_proportional.R`), and higher-level statement helpers (`operate_statements.R`) |
| `data_battles.R` | a bundled example dataset (`battles`, in `data/battles.rda`) used in docs and examples |
| `messydates-defunct.R` | deprecated/removed function stubs kept for a graceful transition |

### Time-of-day support

Times are appended to a date with a space, e.g. `2019-03-01 14:30:00`
(a space is preferred for readability but `T` is still accepted on input,
per ISO 8601-1/RFC 3339).
Because `:` is also a range separator, the parser detects and protects time components
before range-splitting occurs, so `2009-01-01:2019-01-01` still parses as a range
while `2019-03-01 14:30:00` parses as a time.
Time components accept the same `~`/`?`/`%`/`X` annotations as date components.
This logic is spread across the coercion pipeline (`coerce_to_messydate.R`),
component extraction, and the arithmetic/sequence files —
when changing time parsing, check all three.

### Tests

Tests in `tests/testthat/` mirror the `R/` files closely by topic
(e.g. `test-coerce_to.R`, `test-convert_expand.R`, `test-operate_set.R`),
alongside `test-time-*.R` files that cover time-of-day parsing, annotation,
expansion and extraction across those same stages.
When adding a feature to one of the `R/*.R` files above,
the corresponding `test-*.R` file is almost always the right place to add coverage.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`).

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings
so that a reader finds a change where they find the function.
Lead with `## Package` (dependencies, build, branding, website, infrastructure),
then the function families in overview order:
`## Class`, `## Coercion`, `## Resolution`, `## Annotation`, `## Extraction`,
`## Expand/Contract`, `## Operations`.
Some of these name the family rather than the website title, because the site
splits or spells a few of them differently:
`## Coercion` and `## Resolution` stand for "Coerce to" and "Coerce from",
`## Annotation` and `## Extraction` for "Components",
and `## Expand/Contract` for "Manipulation".
Put `## Tests`, `## Vignettes` and `## Data` near the end.
Each heading appears at most once per version.
Reuse a heading that already fits rather than inventing a new one,
and never leave a bullet directly under the version heading.

A version takes a top-level heading, `# messydates X.Y.Z`.
Entries for a version that is not yet released accumulate under the existing top
heading, rather than each opening a heading of its own.
Update `NEWS.md` in the same commit or PR as the change it documents.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closes #123)`
- `Renamed ... to ...` — function or argument name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes
- `Removed ...` / `Dropped ...` — functionality or dependencies taken out
- `Moved ...` / `Migrated ...` — functionality relocated to another package or file
- `Split ...` — one function or file divided into several

Any of these verbs can also lead a sub-bullet.

Name a function by the generic, e.g. `as_messydate()`, where the change reaches
every class it dispatches on.
Where it reaches only one method, spell that method out in full,
e.g. `as_messydate.character()`,
so that a reader knows which classes the change applies to.

Spell the issue suffix `closes`, not `closed` or `closing`.
If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the same parentheses, e.g. `(closes #94, thanks @njbart)`.

#### Grouping

Group first, and only then write the bullets.
The more entries a version holds, the more this matters.

- Cluster related changes as indented sub-bullets under a lead bullet.
- Where several changes concern one function, lead with an `Improved ...` bullet naming
  the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
  so the cluster groups by function rather than by change type.
- Under such a lead bullet, do not name the function again in the sub-bullets,
  since the lead bullet already carries it.
- Where one decision runs across many functions, lead with the decision rather than
  with each function.
- Sub-bullets indent by two spaces, and nest at most one level further (four spaces).

#### Writing the bullets

`NEWS.md` is read by users scanning for what changed, not by reviewers reading prose,
so each bullet is a headline rather than a sentence.
Avoid over-punctuation and over-explanation.
Details belong in the function documentation, if anywhere.

- No full stop at the end of a bullet
- Keep every bullet to one line of fewer than 81 characters ideally
  (a few more or less is fine)
  - If a bullet wraps, it holds too much: shorten it,
    or split it into a lead bullet and sub-bullets
- One clause where possible, and at most one comma
  - Use a semicolon for a short second clause, e.g. "the old spelling still warns"
  - Use a sub-bullet where the second clause needs more room than that
- Name the function or object in backticks and say what changed to it,
  dropping scaffolding like "This change ...", "In order to ...", or "as part of"
- Keep the *what*, and add the *why* only where the behaviour would otherwise look
  arbitrary
- No trailing rationale, no restating the same change twice in different words,
  and no marketing adjectives such as "comprehensive" or "robust"
- A sub-bullet does not need a verb: it can state the consequence,
  the previous behaviour, or an example call
- Cut a sub-bullet that only restates what the lead bullet already implies
- Where several bullets describe parallel changes, reuse the sentence structure,
  so that a reader sees the parallelism at a glance
- Use one word for one thing throughout a version's entries,
  rather than varying the wording for effect

For example, instead of:

> Fixed how `expand()` applies unspecified-component rules to each member of a set
> separately, fixing over-expansion of sets whose members had an unspecified month:
> `"{2008-XX-31,2009-XX-31}"` gave 671 dates and now gives 24.

write:

> Fixed `expand()` applying unspecified-component rules to a whole set
>   - `"{2008-XX-31,2009-XX-31}"` gave 671 dates and now gives 24

and instead of:

> Added `unique()` and `duplicated()` methods for `mdate` objects, which are useful
> because previously `unique()` fell through to the character method and silently
> dropped the class.

write:

> Added `unique()` and `duplicated()` methods for `mdate` (closes #106)
>   - `unique()` fell through to the character method and dropped the class

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- Commenting `/document` or `/style` on a PR triggers
  [prcommands.yml](workflows/prcommands.yml), which runs `roxygen2::roxygenise()`
  or `styler::style_pkg()` and pushes the result back to the PR branch.
- Commits should reference an existing GitHub issue number (`#123`), see below.

### Versioning

The package is versioned according to
[semantic versioning](https://www.jvandemo.com/a-simple-guide-to-semantic-versioning/),
i.e. Major.Minor.Patch.
Every PR into `main` must bump the `Version` field in `DESCRIPTION` by the appropriate
increment, and say the new version number in the PR title; CI checks both.
