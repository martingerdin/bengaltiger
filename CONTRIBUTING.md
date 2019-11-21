# Contributing
1. Read this document.
2. Fork.
3. Contribute. All new branches should correspond to an issue. New functions should be contributed as standalone files.

# Tools
## [ghi](https://github.com/stephencelis/ghi)
Wrapper for git that allows for command line editing of github issues. 
## [hub](https://hub.github.com/)
Used to create pull-requests from the command line.
## [gren](https://github-tools.github.io/github-release-notes/)
Automatic release notes. 

# Versioning
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

# Git workflow
Inspired by [Vincent Driessen's Git branching
model](https://nvie.com/posts/a-successful-git-branching-model/). In short, we
use the following branches:

## `master`
Main release branch. Stable version. 

## `release-*`
Release branch for release *. The release branch corresponding to an upcoming
version 1.0.5 would be called release-v1.0.5. Branched from develop. Merged into
develop and master to bump version when stable.

## `develop`
Branched from `master`. Bleeding edge version. 

## `hotfix-*`
Branched from `master`. Merged into `master` and `develop`. Used only to fix
critical bugs. A hotfix must fix a github issue and result in a patch release.

For example:

```shell
git checkout master
# Create issue if one does not exist
ghi open -m "Fix ImportStudyData throws uncaught error" -L bug -u martingerdin
git checkout -b hotfix-iss12 # Where 12 is a reference to the github issue 
# Write fix
git add --all 
git commit -m "Fix #12 - ImportStudyData throws uncaught error" 
# In the line above 12 is, again, a reference to a github issue and 
# ImportStudyData ... is the title of that issue
git push origin hotfix-iss12
# Create pull request
hub pull-request -m "Fix #12"
```

## `iss*`
Feature and less critical bug fixes branches, where * references a GitHub
issue. Branched from develop. Merged into develop.

For example:
```shell
git checkout develop
ghi open -m "ApplyExclusionCriteria" -L function -u martingerdin
git checkout -b iss15 # Where 15 is a reference to the github issue
# Work on feature
git add --all
git commit -m "Close #15 - Add ApplyExclusionCriteria" # Where 15 is the issue 
                                                       # number
git checkout develop
git merge iss15 -m "Merge feature-add-ApplyExclusionCriteria into develop"
git branch -d iss15
```

# Issues
We use github [issues](https://github.com/martingerdin/bengaltiger/issues) to
track bugs, new features and produce release notes and changelogs using
`gren`. To make sure `gren` produces beautiful release notes and changelogs for
us, follow these advice:

> 1. Start the title with a verb (e.g. Change header styles)
> 2. Use the imperative mood in the title (e.g. Fix, not Fixed or Fixes header styles)
> 3. Use labels wisely and assign one label per issue

Bugs should be labelled `bug`, enhancements should be labelled `enhancement`,
and new functions should be labelled `function`. 

## Commit messages
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

## Tagging
Tags should only be used to mark new master releases. Master releases should be
tagged with version number. Only annoted tags should be used. The annoted tag
message should read "Version MAJOR.MINOR.PATCH".

For example:
```shell
git tag -a v1.0.1 -m "Version 1.0.1"
git push --tags
```

EXCEPTION 

This pre-release version is tagged 0.0.0.9000. Changes before 1.0.0 will be
incremented as stated above in versioning.

## Merging
Merging should be done without using the `--no-ff` flag.

## Complete workflow example
```shell
git checkout develop
ghi open -m "ApplyExclusionCriteria" -L function
git checkout -b iss15
# Work on feature
git add ApplyExclusionCriteria.R
git commit -m "Write function template"
# Continue work on featue
git add ApplyExclusionCriteria.R
git commit -m "Close #15 - Add ApplyExclusionCriteria"
git checkout develop
git merge iss15 -m "Add ApplyExclusionCriteria function"
# If new feature should be merged with master and result in new release
git checkout release-v1.1.0
git merge develop -m "Add ApplyExclusionCriteria function"
# Change version number in DESCRIPTION, increment MINOR as new feature was added
git add DESCRIPTION
git commit -m "Update version number"
git tag -a v1.1.0 -m "Version 1.1.0" # See note below
git push 
git push --tags
gren release 
gren changelog --override
```

To make sure that `gren` includes closed issues in the correct release notes
it's important that there is a separate "tag commit" that is later than the
merge from
develop. See
[this issue](https://github.com/github-tools/github-release-notes/issues/181)
for a discussion on this.

# Code style guide
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
	# Function code here
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
  # Import study data, modify and save to disk. Get path from .session.variables.
  ```
  
  BAD
  ```r
  # import study data, modify and save to disk. get path from .session.variables.
  
  # import study data, modify and save to disk
  # get path from .session.variables.
  ```
- Short code can be followed by a short comment on the same line. Long comments
  should be placed above the code they refer to. 
  
  GOOD
  ```r
  study.data <- ImportStudyData() # Import study data
  
  # Import study data, modify and save to disk. Get path from
  # .session.variables.
  study.data <- ImportStudyData(path = .session.variables$data.path)
  ```
  
  BAD
  ```r
  study.data <- ImportStudyData(path = .session.variables$data.path) # Import study data, modify and save to disk. Get path from .session.variables.
  
  study.data <- ImportStudyData(path = .session.variables$data.path)
  # Import study data, modify and save to disk. Get path from
  # .session.variables.
  ```
- Comments placed on their own line should be preceded by two hashtags
  (##). Comments placed on the same line as code should be preceded by one
  hashtag (#). See above for examples.
