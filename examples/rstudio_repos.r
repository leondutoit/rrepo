# a look at rstudio's github repos
# example as of 2014-04-02
library(dplyr)
library(ggplot2)

url <- "https://api.github.com/orgs/rstudio/repos?page=1&per_page=20"
auth <- "your_username:your_pw" # add your auth
rstudio_repo_data <- get_repo_data(url, auth)

# size grouped by language
repo_language(rstudio_repo_data)

#     language   size
# 1          C  13602
# 2        C++    103
# 3        CSS   1521
# 4       Java 303760
# 5 JavaScript 120782
# 6          R  16751
# 7        TeX    399
# 8         NA    298

# limit it to the top 10
top10 <- rstudio_repo_data %>%
  arrange(desc(updated_at)) %>%
  select(clone_url, name, updated_at) %>%
  top_n(11) # bug in dplyr; fix pending

# using github's REST api would make _many_ REST calls
# so cloning is more efficient
# and less error prone w.r.t. random network issues
clone_repos(top10)

# extract commit data from local repos
commits <- get_all_commit_data(top10, api = FALSE)

commits %>%
  group_by(author) %>%
  summarise(commits = n()) %>%
  arrange(desc(commits)) %>%
  top_n(11)

# Needs better name parsing still...
# Also, many rstudio projects are not under the org's github account
# And this is only for 10 out of the 30ish repos in the org
# But you get the idea anyways

#                author commits
# 1          JJ Allaire    5589
# 2           Joe Cheng    2176
# 3       Winston Chang     855
# 4  Jonathan McPherson     846
# 5              hadley     492
# 6           Yihui Xie     258
# 7         trestletech     132
# 8       jeffreyhorner     116
# 9             unknown     115
# 10         JJ Allarie      88

# And lastly, a look at the commit frequency over time
over_time <- ggplot(
  commits_per_week(commits),
  aes(weekdate, commits)) +
    geom_bar(stat = "identity") +
    stat_smooth()

over_time_per_project <- project_heatmap(projects_over_time(commits))
