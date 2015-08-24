library(svdc)

stopifnot(identical(add(3,3), 6))

tools::assertError(add("a","b"))
tools::assertError(add("1","b"))
tools::assertError(add("a","1"))

#Need to check if the errors are the correct erros too...
