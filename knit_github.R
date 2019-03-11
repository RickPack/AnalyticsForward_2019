library(knitr)
user <- "RickPack"
repo <- "AnalyticsForward_2019"

create_gitpath <- function(user, repo, branch = 'master'){
  paste0(paste('https://github.com', user, repo, 'raw', branch, sep = '/'),'/')
}

my_repo <- create_gitpath(user, repo)

knit.github <- function(..., git_url  ){
  old_url <- opts_knit$get('base.url')
  on.exit(opts_knit$set(base.url = old_url))
  opts_knit$set(base.url  = git_url)
  knit(..., envir = parent.frame())
}

knit.github("README.Rmd", git_url = my_repo)
