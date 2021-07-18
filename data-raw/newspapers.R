library(chronam)
library(usethis)

newspapers = chronam::get_newspapers()

saveRDS(newspapers, "newspapers.rds")
usethis::use_data(newspapers, overwrite=TRUE)
