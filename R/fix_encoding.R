##' @title Convert the encoding of a dataframe from latin-1 to UTF-8
##' @description A method for changing the encoding of a dataframe to UTF-8
##' @param df A dataframe
##' @return A dataframe
##' @author Thomas Rosendal


fix_enc <- function(df) {

    if(class(df) != "data.frame") {
        stop("input must be a dataframe")
    }

    if(ncol(df) < 1){
        stop("dataframe has no columns")
    }

    for(x in seq_len(ncol(df))){
      if(identical(is.factor(df[, x]), TRUE)) {
        Encoding(levels(df[, x])) <- "latin1"
        df[, x] <- enc2utf8(levels(df[, x]))
      }
      if(identical(is.character(df[,x]), TRUE)) {
        Encoding(df[,x]) <- "latin1"
        df[, x] <- enc2utf8(df[, x])
      }
    }
    return(df)
  }
