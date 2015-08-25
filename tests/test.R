library(svdc)

stopifnot(identical(add(3,3), 6))

res <- tools::assertError(add("a","b"))
stopifnot(length(grep("Both arguments must be numeric",
                      res[[1]]$message)) > 0)

res <- tools::assertError(add(1,"b"))
stopifnot(length(grep("Both arguments must be numeric",
                      res[[1]]$message)) > 0)

res <- tools::assertError(add("a",1))
stopifnot(length(grep("Both arguments must be numeric",
                      res[[1]]$message)) > 0)

#Need to check if the errors are the correct erros too...
