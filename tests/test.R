library(svdc)

ppn_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"))
ppn_dataset <- ppn_dataset[,c(1:7)]
test_ppn1 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset, file = test_ppn1, row.names = FALSE)
#writeLines(ppn_dataset, test_ppn1)

data_cleaning(test_ppn1)

res <- tools::assertError(
                          data_cleaning(svasss_dataset = "data/SVASSS.alarms.data_sample.RData",
                                        ppn_dataset =  test_ppn1,
                                        movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("The number of columns in the PPN dataset should be 43",
                     res[[1]]$message)) > 0)

