library(svdc)

#test_1

ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
ppn_sample_dataset$Postnummer<- as.character(ppn_sample_dataset$Postnummer)
ppn_dataset <- ppn_sample_dataset[, -c(41)]
test_ppn1 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset, file = test_ppn1, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  test_ppn1,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("The number of columns in the PPN dataset should be 41",
                     res[[1]]$message)) > 0)

#test_2

rm(list = ls())

ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
ppn_sample_dataset$Postnummer<- as.character(ppn_sample_dataset$Postnummer)
ppn_dataset2 <- ppn_sample_dataset
colnames(ppn_dataset2)[5] <- "notX"
colnames(ppn_dataset2)[6] <- "notY"
test_ppn2 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset2, file = test_ppn2, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  test_ppn2,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("Column name X or Y is changed. Check if JBV fixed the error in X and Y coordinates",
                      res[[1]]$message)) > 0)

#test_3

rm(list = ls())

ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
ppn_dataset3 <- ppn_sample_dataset
names(ppn_dataset3)[1] <- "foo"
test_ppn3 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset3, file = test_ppn3, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  test_ppn3,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("The columns names in the PPN dataset do not match the ordinary PPN columns names",
                      res[[1]]$message)) > 0)

#test_4

rm(list = ls())

ppn_sample_dataset <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
ppn_dataset4 <- ppn_sample_dataset
ppn_dataset4$Ppn[1] <- "B2029" # introduced to make the write.csv2 to change class
test_ppn4 <- tempfile(fileext = ".csv")
write.csv2(ppn_dataset4, file = test_ppn4, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  test_ppn4,
                movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"))
)

stopifnot(length(grep("Columns class has changed",
                      res[[1]]$message)) > 0)


#test5

rm(list = ls())

movements_sample_dataset = read.csv2(system.file("extdata/ani_move_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
movements_dataset <- movements_sample_dataset
names(movements_dataset)[1] <- "foo"
test_mov1 <- tempfile(fileext = ".csv")
write.csv2(movements_dataset, file = test_mov1, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  system.file("extdata/ppn_sample.csv", package = "svdc"),
                movements_dataset = test_mov1)
)

stopifnot(length(grep("The columns names in the movements dataset do not match the ordinary movements columns names",
                      res[[1]]$message)) > 0)

#test 6

rm(list = ls())

movements_sample_dataset = read.csv2(system.file("extdata/ani_move_sample.csv", package = "svdc"), stringsAsFactors = FALSE)
movements_dataset2 <- movements_sample_dataset
movements_dataset2$Ppn[1] <- "B2029" # introduced to make the write.csv2 to change class
test_mov2 <- tempfile(fileext = ".csv")
write.csv2(movements_dataset2, file = test_mov2, row.names = FALSE)

res <- tools::assertError(
  data_cleaning(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                ppn_dataset =  system.file("extdata/ppn_sample.csv", package = "svdc"),
                movements_dataset = test_mov2)
)

stopifnot(length(grep("Columns class of movements dataset has changed",
                      res[[1]]$message)) > 0)
