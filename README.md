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
critical bugs. A hotfix should result in a patch release.

For example:
```shell
git checkout master
git checkout -b hotfix-iss12 # Where 12 is a reference to a github issue
# Write fix
git add --all 
git commit -m "Fix #12 - ImportStudyData throws uncaught error" 
# In the line above 12 is, again, a reference to a github issue and 
# ImportStudyData ... is the title of that issue
git push origin hotfix-iss12
# Create pull request
```

### `fix-*`
Branched from `develop`. Merged into `develop`. Used for less critical
bugs. See [`hotfix-*`](#hotfix-) for workflow.

### `feature-*`
Branched from `develop`. Merged into `develop`. Used to add new features, such
as new functions.

### Commit messages
Should be written in sentence case, be informative, and make sense. To make sure
that `gren` produces beautiful relase notes and changelogs for us, follow the
advice below:

> 1. Start the subject line with a verb (e.g. Change header styles)
> 2. Use the imperative mood in the subject line (e.g. Fix, not Fixed or Fixes header styles)
> 3. Limit the subject line to about 50 characters
> 4. Do not end the subject line with a period
> 5. Separate subject from body with a blank line
> 6. Wrap the body at 72 characters
> 7. Use the body to explain what and why not how

Further, we encourage you
to
["Commit Often, Perfect Later"](https://sethrobertson.github.io/GitBestPractices/),
however, we don't want all commit messages in the release notes and
changelog. Therefore, all commits that are temporary, representing work in
progress, and are made just to make sure important changes are "saved", should
be commited with a commit message that starts with the string "--di--", meaning
"Don't include". Our `gren` configuration file will then make sure that those
commits are omitted from the release notes and changelog.

For example:
```shell
git checkout develop
git checkout -b feature-add-ApplyExclusionCriteria
## You work on the new feature but are not done at the time when you make your
## first commit. We call this a "temporary" commit. Such a commit should be 
## marked with "TMP" in the commit message.
git add ApplyExclusionCriteria.R 
git commit -m "TMP Draft ApplyExclusionCriteria"
## You continue to work on your new feature and after some additional temporary 
## commits the feature is finalised. You therefore omit TMP from your commit 
## message
git add ApplyExclusionCriteria.R 
git commit -m "Add ApplyExclusionCriteria"
## Now, only the commit message without TMP will be included in release notes 
## and the changelog.
```

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
git checkout -b feature-add-ApplyExclusionCriteria
# Work on feature
git add ApplyExclusionCriteria.R
git commit -m "TMP Write function template"
# Continue work on featue
git add ApplyExclusionCriteria.R
git commit -m "Add ApplyExclusionCriteria"
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
  adequate error handling (e.g. `if(!is.character(path) | length(path) > 1)
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
