# Contributing

Contributions to `messydates`,
whether in the form of issue identification, bug fixes, new code or documentation,
are encouraged and welcome.

Please note that the `messydates` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Contents

Each entry says when to read the section.
Update this list whenever a heading below is added, renamed, or moved.

- [Identifying issues](#identifying-issues): before you report a bug or ask for a feature
- [Package architecture](#package-architecture)
  - [Project overview](#project-overview): to decide whether a change keeps the design principle
- [Design principles](#design-principles)
  - [Pleasant predictability](#pleasant-predictability): before you choose an argument order
  - [Naming](#naming): before you name a new function or operator
  - [Vocabulary](#vocabulary): before you write documentation, NEWS, or the paper
- [Developing](#developing)
  - [Common commands](#common-commands): to load, document, test, or check the package
  - [File organization](#file-organization-file-naming--verbpurpose): to find a function,
    or the file for a new one
  - [Time-of-day support](#time-of-day-support): before you change how times are parsed
  - [Tests](#tests): before you add a test, or when a change must not alter output
  - [Deprecation](#deprecation): before you rename or remove a function or an argument
- [Documenting](#documenting)
  - [README, vignettes, and website](#readme-vignettes-and-website): before you edit the README
    or a vignette, and whenever you export a function
  - [`NEWS.md` conventions](#newsmd-conventions): before you write a `NEWS.md` entry
    or a commit message
- [Releasing](#releasing)
  - [Branching and CI](#branching-and-ci): to choose a branch, or to read a failed check
  - [Versioning](#versioning): before you open a pull request into `main`
  - [Checking downstream packages](#checking-downstream-packages): before you change the class,
    or what a function returns
  - [Releasing to CRAN](#releasing-to-cran): before a submission to CRAN
- [Git for new contributors](#git-for-new-contributors): if git is new to you

## Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.

## Package architecture

### Project overview

`messydates` is an R package implementing ISO 8601-2:2019(E) "messy" dates and times.
It introduces an `mdate` class, a `vctrs` vector over a character vector, that can represent
unspecified components (`X`), approximate (`~`) and uncertain (`?`, or both `%`) annotations,
ranges (`..`), open-ended ranges (`..2019-01-01` / `2009-01-01..`), sets (`{}`/`[]`),
and sub-day times of day with the same annotation system.
Durations have their own class, `mduration`, built the same way.

The design principle is: retain and reason about imprecision,
and only resolve to a single precise date/time when the user explicitly asks for it
(via `min`/`max`/`mean`/`median`/`random` resolution).
New functionality should preserve that principle rather than resolving imprecision early
or discarding annotations.

## Design principles

### Pleasant predictability

We are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`as_*()`, `year()`/`month()`/`day()`, `expand()`/`contract()`, etc.)
should share argument order and naming, so that behaviour is guessable across the family.

### Naming

Use these rules to name a new exported function or operator.
The rules apply to new names.
Existing names that do not obey them stay as they are until a deprecation cycle renames them
(see `R/messydates-defunct.R`), and each rename gets a NEWS entry.

1. **Use an existing generic** (from base R, `stats`, or `lubridate`)
   only when the meaning and the return type agree with that generic.
   For example, `min()` reduces a vector to one value, `<` returns a logical vector,
   and `year()` returns a number for each element.
   Do not write a method that gives a familiar generic a different meaning.
2. **Use `v` + the generic name** (`vmin()`, `vmax()`, `vmean()`, `vmedian()`, `vmodal()`, `vrandom()`)
   only for the element-wise form of a generic that reduces a vector to one value.
   The result has the same length as the input.
   Do not use the `v` prefix for any other purpose.
3. **Use `%word%`** (`%union%`, `%intersect%`) for a binary set operation.
   **Use `%letters%`** (`%l%`, `%g%`, `%le%`, `%ge%`, `%><%`, `%>=<%`) only for an operator that returns a proportion.
   Do not add new letter operators.
4. **Use `date_is_` + adjective** for a new predicate, for example `date_is_empty()`.
   A predicate is element-wise and returns a logical vector of the same length as its input.
   A missing input gives `NA`.
   The `date_` stem keeps the name clear of common predicates in other packages
   (for example `rlang::is_empty()` and `purrr::is_empty()`).
   In 1.2.0, `is_precise()`, `is_uncertain()`, `is_approximate()`, `is_bce()` and `is_similar()`
   became `date_is_precise()`, `date_is_uncertain()`, `date_is_approximate()`, `date_is_bce()` and `date_is_similar()`.
   The old names are soft-deprecated aliases in `R/messydates-defunct.R`.
   Inside the package, use only the new names.
   `is_messydate()` is a class test (see rule 6), and it keeps its name.
   `is_intersecting()` and `is_subset()` are not element-wise yet.
   They keep their names until the set operations become element-wise.
5. **Use `md_` + verb** for a new exported function that has no generic,
   for example `md_problems()`.
   Use this prefix in particular when the bare word is a common English word
   or when another common package exports it (`expand()` is an example, as `tidyr` exports it too).
6. **Use the `messydate` or `messyduration` stem** only for class infrastructure:
   `new_messydate()`, `validate_messydate()`, `make_messydate()`, `as_messydate()`, and `is_messydate()`.
   Do not use this stem for an operation on a messy date.
7. **Use the verb form** for a function that adds an annotation, for example `approximate()` and `uncertain()`.
   The function returns an `mdate` of the same length.
8. **Check each new name** against `base`, `stats`, `lubridate`, the tidyverse, and `rlang`
   before you export it.
   If one of these packages exports the name, use the `md_` prefix (rule 5) or choose a different name.

### Vocabulary

Use these terms with these meanings in documentation, vignettes, NEWS, and the paper.
Use one term for one idea, and do not swap in a synonym for variety.

**The record**

- **True date**: when the event happened. Usually unknown.
- **Record**: what the source says about the date.
- **Compatible set**: every date consistent with the record.
  `expand()` returns it; `vmin()` and `vmax()` return its ends.
- **Messy date**: a record whose compatible set holds more than one date.
  Messiness is a property of the record alone.
- **Imprecision**: the general phenomenon, in prose only.
  In the paper, the statistical name is *coarsened data* (Heitjan and Rubin 1991).
- **Precision**: the finest component a record specifies (day, month, year), as `precision()` reports it.

**Annotations** (use the ISO 8601-2 names)

- **Unspecified** (`X`): a component that the record does not give, such as the century in `XX29`.
- **Approximate** (`~`): the source gives the value as an estimate.
- **Uncertain** (`?`): the source itself is in doubt.
  Reserve "uncertain" for this annotation, and use "uncertainty" only for the uncertainty of an estimate.
- **Range** (`..`), **open-ended range** (`..2019` or `2019..`), and **set** (`{}` for all of, `[]` for one of).
- **Empty set** (`{}`): a record that is known to contain no date,
  for example the intersection of two dates that do not overlap.
  It is not a missing value: `NA` is a date that is not known.
  ISO 8601-2 requires a set to have at least one member,
  so the empty set is a messydates extension to the standard.
  The element-wise resolution functions (`vmin()` and the others) give `NA` for it,
  because it holds no date to resolve to.
  Other element-wise functions, such as `approximate()`, `contract()`, and `+`, give `{}` again.
  Functions that reduce a vector to one value (`min()` and the others) leave it out,
  because it adds no date to the union.

**Two different operations on precision**

These two are opposites, and they must not be confused.

- **False precision**: a record shown at a finer precision than the evidence supports,
  so that the compatible set is replaced by one date and then discarded.
  For example, `2020` becomes `2020-01-01`.
  We call the step that does this **flattening**, and it is what the package exists to prevent.
- **Temporal aggregation**: precise dates grouped into coarser units for an analysis.
  For example, `2020-01-01` becomes `2020`.
  This loses information too, but on purpose, and the analyst knows it.
  It is the subject of an older literature (Freeman 1989; Shellman 2004).

**The analysis**

- **Level of aggregation**: the unit of time an analysis uses (day, week, month, year).
  Always write "level of aggregation" (or "aggregation level") in full,
  because a bare "level" is ambiguous with precision and with factor levels in R.
- **Cell**: one unit at that level, such as May 1997 in a monthly analysis.
- **Resolution**: choosing one date from the compatible set at analysis time.
  Never use "resolution" to mean the level of aggregation.
- **Rule**: the way resolution chooses a date: earliest (`vmin()`), latest (`vmax()`),
  midpoint (`vmean()`), median (`vmedian()`), or random (`vrandom()`).
- **Corner cases**: the estimates under the earliest and latest rules.
  These are not bounds on the estimate.
- **Labile**: a messy date is labile for an analysis when its compatible set spans more than one cell,
  or more than one outcome of a comparison.
  Lability is a property of the record *and* the analysis:
  `1997-05` is labile at the week level but not at the year level.
  In the language of rough sets (Pawlak 1982), a labile date lies in the boundary region of the partition.
- **Exposure**: the share of labile dates in an analysis, optionally weighted by their influence on the estimate.
- **Coarsening mechanism**: how the compatible set relates to the true date.
  It is *at random* when the true date is equally likely anywhere in the set,
  and *informative* otherwise, as under a reporting lag.
- **Dating effect**: the difference in an estimate across rules.

## Developing

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
- Time the parser and the resolution functions: `Rscript data-raw/benchmark.R`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no build system outside R: no `package.json` and no `Makefile`.

`DESCRIPTION` sets `Config/testthat/parallel: true` for CI.
Some local setups cannot spawn the parallel testthat subprocesses;
if the suite fails to start rather than failing a test, force sequential execution
with `Sys.setenv(TESTTHAT_PARALLEL = "false")` before `testthat::test_dir("tests/testthat")`
rather than changing the `DESCRIPTION` setting.
`devtools::check()` is unaffected.

When writing new code, please follow
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/).
Packages such as `lintr`, `goodpractice` and `formatR` help to check this.

### File organization (file naming = verb/purpose)

`R/` files are grouped by verb/purpose, not by exported function,
so related functions across the public API often live in different files that share a prefix.
The prefix also gives the `NEWS.md` heading for a change (see [Grouping](#grouping)).

| Prefix | Contains |
|---|---|
| `class_*.R` | the `mdate` class itself: `class_mdate.R` holds the constructor/validator (`new_messydate()`/`validate_messydate()`/`make_messydate()`) *and* the S3 dispatch methods (`print`/`format`/`Ops`/comparison); `class_methods.R` holds the `vctrs` methods (type names, `vec_ptype2()`, `vec_cast()`) that give `mdate` ordinary vector behaviour for `[`, `[<-`, `c()`, `rep()`, and `unique()`; duration handling lives in `class_mduration.R` |
| `coerce_to_messydate.R` | the largest file: `as_messydate()`, the main user-facing entry point that parses character strings, numbers, `Date`, `POSIXct`/`POSIXlt` into `mdate`, including the prose parser (Roman numerals, "circa", "the Ides of March", decades/centuries, "before X", comma-separated prose lists, etc.; see the roxygen `@section Parsing prose` there for the full list of supported conventions) |
| `coerce_from_messydate.R` | the inverse: `mdate` to `Date`/`POSIXct`/`POSIXlt`, and `as.list.mdate()` |
| `validate_input.R` | input validation shared across the coercion pipeline, and `md_problems()`, which reports what could not be parsed |
| `resolve_extrema.R`, `resolve_tendency.R`, `resolve_weights.R` | resolution functions (`vmin()`/`vmax()`/`vmedian()`/`vmean()`/`random()`, etc.) used to collapse a messy date/range/set to one precise value; `resolve_weights.R` holds the weights that `random()` uses for approximate and uncertain values |
| `component_extract.R`, `component_annotate.R` | pulling out or tagging individual date/time components (year, month, day, hour, and so on), their annotation state, and `precision()` |
| `convert_expand.R`, `convert_contract.R`, `convert_sequence.R` | expanding an `mdate` (range/set/approximate window) out to all compatible concrete dates, contracting a vector of dates back into the most compact messy representation, and generating `seq()`-style sequences over `mdate`s |
| `operate_*.R` | arithmetic on mdates and durations (`operate_arithmetic.R`), inequality/comparison operators (`operate_comparison.R`), set operations on mdate sets/ranges (`operate_set.R`), proportional overlap (`operate_proportional.R`), and the `date_is_*()` predicates (`operate_statements.R`) |
| `data_battles.R` | a bundled example dataset (`battles`, in `data/battles.rda`) used in docs and examples |
| `messydates-defunct.R` | deprecated/removed function stubs kept for a graceful transition (see [Deprecation](#deprecation)) |

### Time-of-day support

Times are appended to a date with a space, e.g. `2019-03-01 14:30:00`
(a space is preferred for readability but `T` is still accepted on input,
per ISO 8601-1/RFC 3339).
Because `:` is also a range separator, the parser detects and protects time components
before range-splitting occurs, so `2009-01-01:2019-01-01` still parses as a range
while `2019-03-01 14:30:00` parses as a time.
Time components accept the same `~`/`?`/`%`/`X` annotations as date components.
This logic is spread across the coercion pipeline (`coerce_to_messydate.R`),
component extraction, and the arithmetic and sequence files.
When you change time parsing, check all three.

### Tests

Tests in `tests/testthat/` mirror the `R/` files closely by topic
(e.g. `test-coerce_to.R`, `test-convert_expand.R`, `test-operate_set.R`),
alongside `test-time-*.R` files that cover time-of-day parsing, annotation,
expansion and extraction across those same stages.
When adding a feature to one of the `R/*.R` files above,
the corresponding `test-*.R` file is almost always the right place to add coverage.

`test-equivalence.R` compares the output of parsing, expansion, and resolution
on a fixed set of inputs to a reference in `tests/testthat/fixtures/equivalence.rds`.
Its inputs and helpers are in `tests/testthat/helper-equivalence.R`.
Use it to show that a change made for speed or tidiness does not change any result:

1. Make the change, and run `test-equivalence.R`.
2. If it fails, the change altered what users get back.
   Fix the change, or, only if the new output is intended,
   rebuild the reference with `Rscript data-raw/equivalence_reference.R`.
3. Record every intended change to output in `NEWS.md`.

Run `Rscript data-raw/benchmark.R` before and after a change made for speed,
and give the result in `NEWS.md` (see [`NEWS.md` conventions](#newsmd-conventions)).
Add a row to the benchmark for any new path that is found to be slow.

### Deprecation

A function that is renamed or removed keeps working under its old name for a time,
through a shim in [R/messydates-defunct.R](../R/messydates-defunct.R):

- Document the shim with `#' @describeIn defunct Deprecated in messydates X.Y.Z.`,
  name the new function on the next line, and export it.
- For a rename, call `.soft_deprecate("<old>()", "<new>()")` and then the new function,
  so that the old name still returns a result.
  The message shows once in each session.
- Older shims call `.Deprecated()`, which warns each time the old name is used.
  Use `.soft_deprecate()` for a new shim.
- Inside the package, use only the new name.
- Record the change as `Renamed ... to ...` or `Removed ...` in `NEWS.md`,
  and say that the old name still works.

Shims are removed at a later minor release, which `NEWS.md` names.
A shim needs no tests beyond one that the old name gives the message and still works.

## Documenting

### README, vignettes, and website

`README.md` is generated from `README.Rmd`.
Edit `README.Rmd` and knit it again with `devtools::build_readme()`.
Never edit `README.md` directly.

Vignettes and articles show the value that the package adds.
Please contact the maintainer with any proposals here.

Every exported function must appear in a `reference:` section of `pkgdown/_pkgdown.yml`,
or the website build fails.
The sections follow the file prefixes in [File organization](#file-organization-file-naming--verbpurpose).

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings
so that a reader finds a change where they find the function.
Lead with `## Package` (dependencies, build, branding, website, infrastructure),
then the function families in the order of the website overview:

| Heading | Website title | Files |
|---|---|---|
| `## Class` | Class construction | `class_*.R` |
| `## Coercion` | Coerce to | `coerce_to_messydate.R`, `validate_input.R` |
| `## Resolution` | Coerce from | `coerce_from_messydate.R`, `resolve_*.R` |
| `## Expand/Contract` | Manipulation | `convert_*.R` |
| `## Annotation` | Components | `component_annotate.R` |
| `## Extraction` | Components | `component_extract.R` |
| `## Operations` | Operations | `operate_*.R` |

Put a change under the heading for the file that holds the function,
even where the change came from work on another family.
Put `## Tests`, `## Vignettes` and `## Data` near the end.
Each heading appears at most once per version.
Use only the headings above; do not invent a new one, such as `## Statements`.
Never leave a bullet directly under the version heading.

A version takes a top-level heading, `# messydates X.Y.Z`.
Entries for a version that is not yet released accumulate under the existing top
heading, rather than each opening a heading of its own.
Update `NEWS.md` in the same commit or PR as the change it documents.

Start each bullet with a verb matching the change type:

- `Added ...`: new functionality
- `Fixed ...`: bug fixes
- `Renamed ... to ...`: function or argument name migrations
- `Improved ...`: functional updates to existing behaviour
- `Accelerated ...`: the same behaviour, made faster;
  give the speed-up in a sub-bullet (see below)
- `Updated ...`: documentation changes, including this file
- `Removed ...` / `Dropped ...`: functionality or dependencies taken out
- `Moved ...` / `Migrated ...`: functionality relocated to another package or file
- `Split ...`: one function or file divided into several

Any of these verbs can also lead a sub-bullet.
A change to a workflow or another build file is `Improved ...`, not `Updated ...`.

An `Accelerated ...` bullet says how the function was made faster,
and a sub-bullet says by how much and for what input, e.g.:

```
- Accelerated `as_messydate()` (closes #105)
  - Prose and time steps run only on elements that can hold prose or a time
  - ~9x faster for 6,000 plain ISO dates
```

Time the old and the new version on the same input (see [Tests](#tests)),
and give a rounded figure as `~40x`, a range as `~40-140x`,
or a lower bound as `>1000x`.
Name the input the figure was measured on,
since the speed-up usually grows with the size of the vector.
Do not add a sub-bullet that says the results are unchanged:
`Accelerated` already says so.

Name a function by the generic, e.g. `as_messydate()`, where the change reaches
every class it dispatches on.
Where it reaches only one method, spell that method out in full,
e.g. `as_messydate.character()`,
so that a reader knows which classes the change applies to.

Cite a GitHub issue at the end of the bullet, in parentheses:

- `(closes #123)` where the change completes the issue
- `(#123)` where the change does only part of it, and the issue stays open
- Spell the suffix `closes`, not `closed` or `closing`
- GitHub closes the issue only where the same phrase is in a commit message
  or the pull request description, so put it there too
- If the issue was **not** authored by @jhollway, thank the author with an
  `@`-tag in the same parentheses, e.g. `(closes #94, thanks @njbart)`

Commits should also cite the issue that they work on, where there is one.

#### Grouping

Group first, and only then write the bullets.
The more entries a version holds, the more this matters.

- Cluster related changes as indented sub-bullets under a lead bullet.
- Where several changes concern one function, lead with an `Improved ...` bullet naming
  the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
  so the cluster groups by function rather than by change type.
- Under such a lead bullet, do not name the function again in the sub-bullets,
  since the lead bullet already carries it.
- Where one decision runs across many functions in one family, lead with the decision
  rather than with each function, e.g. "Fixed missing values in `expand()` and `contract()`".
  Where it runs across families, give each heading its own bullet with the same wording.
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

## Releasing

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a reverse-dependency
  check, and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- Commenting `/document` or `/style` on a PR triggers
  [prcommands.yml](workflows/prcommands.yml), which runs `roxygen2::roxygenise()`
  or `styler::style_pkg()` and pushes the result back to the PR branch.

### Versioning

The package is versioned according to
[semantic versioning](https://www.jvandemo.com/a-simple-guide-to-semantic-versioning/),
i.e. Major.Minor.Patch.
Every PR into `main` must bump the `Version` field in `DESCRIPTION` by the appropriate
increment, and say the new version number in the PR title; CI checks both.
The newest heading in `NEWS.md` must give the same version.

### Checking downstream packages

`{manydata}` has `{messydates}` under `Depends`,
and `{autograph}`, `{manystates}`, and `{poldis}` have it under `Suggests`.
A change to the `mdate` class, to what a function returns, or to a name
can break them until their own next release.
Before committing such a change:

1. Build the package from a clean copy and install it into a separate library:
   `R CMD INSTALL --library=<lib> messydates_<version>.tar.gz`.
2. Run the test suite of each downstream package against that library, e.g.
   `R_LIBS=<lib> Rscript -e "devtools::test('<path to manydata>', stop_on_failure = FALSE)"`,
   and their examples and vignettes, since those are checked on CRAN too.
3. Compare against the same run on an unchanged `develop`,
   so that a failure that was already there is not taken for a new one.
4. Read the backtraces rather than the counts.
   A failure inside a downstream package's own `R/` code is a real break:
   keep a shim for the old behaviour (see [Deprecation](#deprecation)),
   or wait until that package is fixed and released.
   A failure in its `tests/` is usually an expectation to update there.

The `Reverse dependencies` job in [prchecks.yml](workflows/prchecks.yml) only checks
that the CRAN releases still find every `{messydates}` object they use.
It does not run their tests or examples, so it does not replace this check.

### Releasing to CRAN

Once the pull request into `main` passes its checks:

1. Check that the `Version` in `DESCRIPTION` equals the newest heading in `NEWS.md`,
   and that every bullet under it describes something a user can see.
2. Build a tarball from a clean copy of the repository and run
   `R CMD check --as-cran --run-donttest` on it. Read every note.
3. Check the *CRAN releases* of all reverse dependencies against the new build:
   download their source tarballs and run `R CMD check` on each,
   with the new `{messydates}` first in the library path.
   CRAN runs the same check, and a failure there stops the submission.
4. Where a downstream package had to change to work with this release,
   its own release must be on CRAN first, and must require the `{messydates}` version
   it needs (`messydates (>= x.y.z)`), so that CRAN never pairs it with an older one.
5. Write `cran-comments.md` from the results of these checks, not from the last release:
   the environments that were really tested, the counts of errors, warnings, and notes,
   the full text of each note with a sentence on why it is acceptable,
   and the result for each reverse dependency.
6. CRAN notes a package with more than six releases in the past 180 days
   ("Number of updates in past 6 months"). Count the releases on CRAN and in its
   archive before submitting, and say why the release cannot wait if the note will occur.
7. Submit with `devtools::submit_cran()`, and tag the release only once CRAN has
   accepted it, since the version may have to move if CRAN asks for changes.

## Git for new contributors

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

### Fork

#### Cloning
Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

#### Pull
This command allows you to `pull` changes from the remote repository to your local repository.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose main or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 

#### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the
issue reference (see [`NEWS.md` conventions](#newsmd-conventions)) and commit.
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.
