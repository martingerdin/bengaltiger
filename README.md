# bengaltiger :tiger:
A R-package to streamline data analysis and manuscript writing for medical
research, primarily on the Towards Improved Trauma Care Outcomes (TITCO) in
India cohort. Very much work in progress at this stage.

[![Automated Release Notes by gren](https://img.shields.io/badge/%F0%9F%A4%96-release%20notes-00B2EE.svg)](https://github-tools.github.io/github-release-notes/)

## Installation
```r
install.packages("devtools")
library("devtools")
install_github("martingerdin/bengaltiger")
```

## Use
```r
library("bengaltiger")
```

## Contributing
1. Read this document.
2. Fork.
3. Contribute. New functions should be contributed as standalone files.

## Versioning
We use Semantic Versioning, see [semver.org](https://semver.org/) for details,
as interpreted by Hadley Wickham [here](http://r-pkgs.had.co.nz/release.html). A
version number is MAJOR.MINOR.PATCH, for example 1.0.9. Only master releases are
versioned, with the exception of this pre-release of first master release
version, which is versioned as 0.0.X.9000.

> Increment patch, e.g. 0.8.2 for a patch: you’ve fixed bugs without adding any
> significant new features. I’ll often do a patch release if, after release, I
> discover a show-stopping bug that needs to be fixed ASAP. Most releases will
> have a patch number of 0.

> Increment minor, e.g. 0.9.0, for a minor release. A minor release can include
> bug fixes, new features and changes in backward compatibility. This is the
> most common type of release. It’s perfectly fine to have so many minor
> releases that you need to use two (or even three!) digits, e.g. 1.17.0.
 
> Increment major, e.g. 1.0.0, for a major release. This is best reserved for
> changes that are not backward compatible and that are likely to affect many
> users. Going from 0.b.c to 1.0.0 typically indicates that your package is
> feature complete with a stable API.
 
> In practice, backward compatibility is not an all-or-nothing threshold. For
> example, if you make an API-incompatible change to a rarely-used part of your
> code, it may not deserve a major number change. But if you fix a bug that many
> people depend on, it will feel like an API breaking change. Use your best
> judgement.

## Git workflow
See
[Vincent Driessen's Git branching model](https://nvie.com/posts/a-successful-git-branching-model/) for
details. In short, we use the following branches (note that we don't use
specific branches to finalise releases, not yet at least):

### `master`
Main release branch. Stable version. 

### `develop`
Branched from `master`. Merged into master to bump version and form a new
release when stable.

### `hotfix-*`
Branched from `master`. Merged into `master` and `develop`. Used only to fix
critical bugs. A hotfix must fix a github issue and result in a patch release.

For example:
```shell
git checkout master
# Create issue if one does not exist.
hub create -m "Fix ImportStudyData throws uncaught error" -l bug
git checkout -b hotfix-iss12 # Where 12 is a reference to the github issue 
# Write fix
git add --all 
git commit -m "Fix #12 - ImportStudyData throws uncaught error" 
# In the line above 12 is, again, a reference to a github issue and 
# ImportStudyData ... is the title of that issue
git push origin hotfix-iss12
# Create pull request
```

### `fix-*`
Branched from `develop`. Merged into `develop`. Used for less critical bugs. A
fix must fix a github issue. See [`hotfix-*`](#hotfix-) for workflow.

### `feature-*`
Branched from `develop`. Merged into `develop`. Used to add new features, such
as new functions. A feature should come from a github issue.

For example:
```shell
git checkout develop
hub create -m "ApplyExclusionCriteria" -l function
git checkout -b feature-add-ApplyExclusionCriteria
# Work on feature
git add --all
git commit -m "Close #12 - Add ApplyExclusionCriteria" # Where 12 is the issue 
                                                       # number
git checkout develop
git merge --no-ff feature-add-ApplyExclusionCriteria -m "Merge feature-add-ApplyExclusionCriteria into develop"
git branch -d feature-add-ApplyExclusionCriteria
```

## Issues
We use github [issues](https://github.com/martingerdin/bengaltiger/issues) to
track bugs, new features and produce release notes and changelogs using
`gren`. To make sure `gren` produces beautiful release notes and changelogs for
us, follow these advice:

> 1. Start the title with a verb (e.g. Change header styles)
> 2. Use the imperative mood in the title (e.g. Fix, not Fixed or Fixes header styles)
> 3. Use labels wisely and assign one label per issue

Bugs should be labelled `bug`, enhancements should be labelled `enhancement`,
and new functions should be labelled `function`. 

EXCEPTION 

When you create an issue to reflect that you're working on a new function the
title should just be the function name:

```shell
hub create -m "ApplyExclusionCriteria" -l function
git checkout develop
git checkout -b feature-feature-add-ApplyExclusionCriteria
```

### Commit messages
Should be written in sentence case, be informative, and make sense. Please
follow `gren`'s advice:

> 1. Start the subject line with a verb (e.g. Change header styles)
> 2. Use the imperative mood in the subject line (e.g. Fix, not Fixed or Fixes header styles)
> 3. Limit the subject line to about 50 characters
> 4. Do not end the subject line with a period
> 5. Separate subject from body with a blank line
> 6. Wrap the body at 72 characters
> 7. Use the body to explain what and why not how

Further, we encourage you
to
["Commit Often, Perfect Later"](https://sethrobertson.github.io/GitBestPractices/).

### Tagging
Tags should only be used to mark new master releases. Master releases should be
tagged with version number. Only annoted tags should be used. The annoted tag
message should read "Version MAJOR.MINOR.PATCH released".

For example:
```shell
git tag -a v1.0.1 -m "Version 1.0.1"
git push --tags
```

EXCEPTION 

This pre-release version is tagged 0.0.0.9000. Changes before 1.0.0 will be
incremented as stated above in versioning.

### Merging
Merging should be done using the `--no-ff` flag to make sure that commit
history is not lost.

For example:
```shell
git checkout develop
git merge --no-ff fix-iss18
git push origin develop
git branch -d fix-iss18
```
### Complete workflow example
```shell
git checkout develop
hub create -m "ApplyExclusionCriteria" -l function
git checkout -b feature-feature-add-ApplyExclusionCriteria
# Work on feature
git add ApplyExclusionCriteria.R
git commit -m "Write function template"
# Continue work on featue
git add ApplyExclusionCriteria.R
git commit -m "Close #12 - Add ApplyExclusionCriteria"
git checkout develop
git merge --no-ff -m "Add ApplyExclusionCriteria function"
# If new feature should be merged with master and result in new release
git checkout master
# Change version number in DESCRIPTION, increment MINOR as new feature was added
git merge --no-ff -m "Add ApplyExclusionCriteria function"
git tag -a v1.1.0 -m "Version 1.1.0"
git push
git push --tags
gren release 
gren changelog --override
```

## Code style guide
We use [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)
with the exception that functions should be documented
using [roxygen2](https://github.com/yihui/roxygen2) comments. An example
function:

For example:
```r
#' Import study data
#'
#' Imports the study data.
#' @param path A character vector of length 1. Path to study data. No default.
#' @export
ImportStudyData <- function(path) {
	## Function code here
	return(study.data)
}
```

Additional notes:
- `@param` definitions, i.e. "A character vector of length 1. Path to study
  data. No default." in the example above, have four parts, A. B. C. D. C is
  optional. A defines the parameter value and should ideally be enforced by
  adequate error handling (e.g. `if(!is.character(path) | isLength1(path))
  stop("path has to be a character vector of length 1")`). B is a plain text
  description of the parameter. C may be a continuation of B or any other text
  that one may want to include. D is a description of the default value, if
  any. If there is no default value this should be clearly stated as "No
  default".
- [roxygen2](https://github.com/yihui/roxygen2) comments and in code comments
  should be written using sentence case with correct punctuation. In other words:
  
  GOOD
  ```r
  ## Import study data, modify and save to disk. Get path from .session.variables.
  ```
  
  BAD
  ```r
  ## import study data, modify and save to disk. get path from .session.variables.
  
  ## import study data, modify and save to disk
  ## get path from .session.variables.
  ```
- Short code can be followed by a short comment on the same line. Long comments
  should be placed above the code they refer to. 
  
  GOOD
  ```r
  study.data <- ImportStudyData() # Import study data
  
  ## Import study data, modify and save to disk. Get path from
  ## .session.variables.
  study.data <- ImportStudyData(path = .session.variables$data.path)
  ```
  
  BAD
  ```r
  study.data <- ImportStudyData(path = .session.variables$data.path) # Import study data, modify and save to disk. Get path from .session.variables.
  
  study.data <- ImportStudyData(path = .session.variables$data.path)
  ## Import study data, modify and save to disk. Get path from
  ## .session.variables.
  ```
- Comments placed on their own line should be preceded by two hashtags
  (##). Comments placed on the same line as code should be preceded by one
  hashtag (#). See above for examples.
