### rrepo

[![Build Status](https://travis-ci.org/leondutoit/rrepo.svg)](https://travis-ci.org/leondutoit/rrepo)

Tools to analyse an organisation's github repos.

```R
install.packages("devtools")
devtools::install_github("leondutoit/rrepo")
```

Quick example:

```R
library(rrepo)
url <- "https://api.github.com/orgs/{org_name}/repos" # substitue {org_name} with name
auth <- list(user = "user", pw = "pw") # use your github auth
repo_data <- get_org_repo_data(url, auth)

# Repo level info
repo_language(repo_data)

# Commit level info
repos <- clone_org_repos(repo_data)
commits <- get_all_commit_data(repos)
author_contributions(commits)

# Pretty picture
project_heatmap(projects_over_time(commits))

# cleaup
remove_local_clones(repos)
```
