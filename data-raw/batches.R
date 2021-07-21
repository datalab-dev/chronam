library(chronam)
library(usethis)

batches = chronam::get_batches()

saveRDS(batches, "batches.rds")
usethis::use_data(batches, overwrite=TRUE)
