### rrepo

Tools to analyse an organisation's github repos. This is made primarily for my own enjoyment although it is certainly also useful for other people who are curious about their organisation's git repos.

```R
install.packages("devtools")
devtools::install_github("rrepo", "leondutoit")
```

Quick example:

```R
library(rrepo)
url <- "https://api.github.com/orgs/{org_name}/repos" # substitue {org_name} with name
auth <- list(user = "user", pw = "pw") # substitue with your github auth
repo_data <- get_repo_data(url, auth)

# Repo level info
repo_language(repo_data)

# Commit level info
clone_repos(repo_data)
commits <- get_all_commit_data(repo_data, api = FALSE) # use local repos
author_contributions(commits)

# Pretty picture
project_heatmap(projects_over_time(commits))

```
There are two ways to get information about commits: 1) clone repos from github and extract info locally; or 2) use the github REST API to download commits in batches. Both methods work just fine.

I recommend method #1 if there are many commits and if you're interested in deeper details - the local extraction gives information about files changed, insertions and deletions per commit, while the github API does not. Of course the information is available but getting it via HTTP would mean making crazy amounts of requests. Method #1 is also less error prone due to possible network disturbances when making lots of REST calls.

For more in depth examples see the `examples` folder.
