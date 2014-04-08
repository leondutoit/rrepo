rrepo
=====

Tools to analyse an organisation's github repos. This is made primarily for my own enjoyment although it is certainly also useful for other people who are curious about their organisation's git repos.

```R
install.packages("devtools")
devtools::install_github("rrepo", "leondutoit")
```

Quick example:
```R
library(rrepo)
url <- "https://api.github.com/orgs/{org_name}/repos" # substitue {org_name} with name
auth <- "{username}:{password}" # substitue with your github auth
repo_data <- get_repo_data(url, auth)

# Repo level info
repo_language(repo_data)

# Commit level info
clone_repos(repo_data)
commits <- get_all_commit_data(repo_data, api = FALSE) # use local repos
author_contributions(commits)

```

For more in depth examples see the `examples` folder.
