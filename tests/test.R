library(svdc)

#test_1

ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"))
ppn_dataset <- ppn_sample_dataset[, -c(42:43)]
test_ppn1 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset, file = test_ppn1, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = "data/SVASSS.alarms.data_sample.RData",
                ppn_dataset =  test_ppn1,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("The number of columns in the PPN dataset should be 43",
                     res[[1]]$message)) > 0)

#test_2

rm(list = ls())


ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"))
ppn_dataset <- ppn_sample_dataset
names(ppn_dataset)[1] <- "foo"
test_ppn1 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset, file = test_ppn1, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = "data/SVASSS.alarms.data_sample.RData",
                ppn_dataset =  test_ppn1,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("The columns names in the PPN dataset do not match the ordinary PPN columns names",
                      res[[1]]$message)) > 0)
res
