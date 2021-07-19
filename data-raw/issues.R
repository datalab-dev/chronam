# documents getting the dataset as well as the post processing applied to it
library(chronam)
library(usethis)

issues = chronam::get_issues_all(update=TRUE)

saveRDS(issues, "issues.rds")
usethis::use_data(issues, overwrite=TRUE)
