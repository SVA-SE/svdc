#' Check filename argument
#'
#' Check the filename and return normalized path.
#' @param filename The filename to check
#' @return character vector with normalized path
#' @keywords internal
check_file_argument <- function(filename)
{
    stopifnot(all(is.character(filename),
                  identical(length(filename), 1L)))
    normalizePath(filename, mustWork = TRUE)
}

#' Load ppn data
#'
#' Load and prepare ppn data.
#' @param filename The filename to the dataset
#' @return A data.frame
#' @keywords internal
load_ppn_data <- function(filename)
{
    PPN <- read.csv(file = filename, sep=";", header=T, stringsAsFactors = FALSE, dec=",",
                    encoding = "UTF-8")
    
    if(!(length(names(PPN)) == 41)){
        stop("The number of columns in the PPN dataset should be 41")
    }

    ## OBS! X and Y are inverted in the original dataset of JBV
    if(!colnames(PPN)[5] == "X" | !colnames(PPN)[6] == "Y"){
        stop("Column name X or Y is changed. Check if JBV fixed the error in X and Y coordinates")
    }
    
    ## Load sample data to compare column names. We expect the column
    ## names in the sample data and real data to be identical. NOTE: that
    ## this check fails if the data structure change.
    ppn_sample <- read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc"),
                            stringsAsFactors=FALSE)
    
    if(!identical(names(ppn_sample), names(PPN))) {
        stop("The columns names in the PPN dataset do not match the ordinary PPN columns names")
    }

    ## Also check that the type of each column is identical to the sample
    ## data.
    if(!identical(sapply(PPN, "class"), sapply(ppn_sample, "class"))){
        stop("Columns class has changed")
    }
    
    colnames(PPN)[5] <- "Y"
    colnames(PPN)[6] <- "X"
    
    ## Exclude PPN with status 'Upph?rt'
    PPN <- subset(PPN, PPN$Platsstatuskod == "G" | PPN$Platsstatuskod == "O")

    ## Add column to sum total number of animals per each PPN
    PPN$tot_anim <- rowSums(PPN[, c("Antal",
                                    "Antalslaktplatser",
                                    "Antalsuggplatser",
                                    "CDB.Antal",
                                    "Maxkapacitet")], na.rm = TRUE)

    ## The total number of animals can be identical to zero for two
    ## reasons:
    ##  1) All numbers for one PPN equal to NA
    ##  2) At least one number specified to 0
    ## We want to make sure the 1) case is NA
    i <- which(is.na(PPN$Antal) &
               is.na(PPN$Antalslaktplatser) &
               is.na(PPN$Antalsuggplatser) &
               is.na(PPN$CDB.Antal) &
               is.na(PPN$Maxkapacitet))
    if (length(i))
        PPN$tot_anim[i] <- NA

    ## create a new column to list the species present in each PPN
    z <- PPN[c("Ppn", "Typ")]
    z1 <- split(z$Typ, z$Ppn)
    a <- unlist(lapply(z1, function(x){
        paste(unique(x), collapse = ", ")
    }))

    final <- data.frame("Ppn" = labels(a),
                        "Species" = a,
                        stringsAsFactors = FALSE)

    final$Ppn <- as.integer(final$Ppn)

    PPN<-cbind(PPN,
               "Species" = final$Species[match(PPN$Ppn, final$Ppn)],
               stringsAsFactors=FALSE)

    PPN$Species[PPN$Species == ""] <- "Unknown"

    return(PPN)
}

#' Load movement data
#'
#' Load and prepare movement data.
#' @param filename The filename to the dataset
#' @return A \code{data.frame}
#' @keywords internal
load_movement_data <- function(filename)
{
    ## Import movement dataset and format according to EpiContactTrace
    ## (Thomas Rosendal code)
    ani_move <- read.csv2(filename, as.is=TRUE)
    ani_move_sample <- read.csv2(system.file("extdata/ani_move_sample.csv", package = "svdc"),
                                 stringsAsFactors=FALSE)

    if(!identical(names(ani_move_sample), names(ani_move))) {
        stop("The columns names in the movements dataset do not match the ordinary movements columns names")
    }

    if(!identical(sapply(ani_move, "class"), sapply(ani_move_sample, "class"))) {
        stop("Columns class of movements dataset has changed")
    }

    file.info("ani_move")
    ani_move <- ani_move[,c(5,6,8,7)]
    names(ani_move) <- c('source', 'destination', 'Type', 't')

    ## We are only interested in movement dataset last 365 days
    ani_move$t <- as.Date(ani_move$t)
    ani_move <- ani_move[ani_move$t > Sys.Date() - 365,]

    ani_move2 <- subset(ani_move, subset = Type == 2 | Type == 4)
    ani_move3 <- subset(ani_move, subset = Type == 1 | Type == 5)
    ani_move4 <- ani_move3
    ani_move4$source <- ani_move3$destination
    ani_move4$destination <- ani_move3$source
    ani_move <- rbind(ani_move2, ani_move4)
    ani_move <- subset(ani_move, select = -Type)

    return(ani_move)
}


#' Read in the PPN data from jordbruksverket
#' 
#' Change all projected coordinated from sweref99 to RT90
#' @param df dataset of PPNs
#' @return a dataframe of the PPNs
#' @import sp
#' @import rgdal
#' @keywords internal

rt90_sweref99 <- function(df) 
{
  
  ##Change all projected coordinated from sweref99 to RT90. The
  ##coordinate systems do not overlap. All those points with a
  ##Y-coordinate (the horizontal axis) of less than 1083427.290 are
  ##sweref99 and should be reprojected to RT90. The other points are
  ##already in RT90. The reason for the choice of RT90 is that the
  ##polygon data that I have for Sweden is in RT90. 
  
  colnames(df)[colnames(df) == "Y"] <- "X.1"
  colnames(df)[colnames(df) == "X"] <- "Y"
  colnames(df)[colnames(df) == "X.1"] <- "X"
  
  if (length(df$Y[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]) == 0) {return(df); break}
    
  points_sweref99 <- SpatialPoints(cbind(df$Y[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)],
                                         df$X[df$Y < 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]))
  ##
  proj4string(points_sweref99) <- "+init=epsg:3006 +proj=utm +zone=33 +ellps=GRS80
  +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  points_RT90 <- SpatialPoints(cbind(df$Y[df$Y >= 1083427.2970 & !is.na(df$Y) & !is.na(df$X)],
                                     df$X[df$Y >= 1083427.2970 & !is.na(df$Y) & !is.na(df$X)]))
  
  proj4string(points_RT90) <- "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0
                                 +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs"
  
  points_RT90_2<- spTransform(points_sweref99, "+init=epsg:3021 +proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0
                                                  +ellps=bessel +towgs84=414.1,41.3,603.1,-0.855,2.141,-7.023,0 +units=m +no_defs")
  
  
  df$X1[df$Y<1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90_2@coords[,"coords.x2"]
  df$Y1[df$Y<1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90_2@coords[,"coords.x1"]
  
  df$X2[df$Y>=1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90@coords[,"coords.x2"]
  df$Y2[df$Y>=1083427.2970 & !is.na(df$Y) & !is.na(df$X)] <- points_RT90@coords[,"coords.x1"]
  
  df$X <- df$X1
  df$X[is.na(df$X)] <- df$X2[is.na(df$X)]
  
  df$Y <- df$Y1
  df$Y[is.na(df$Y)] <- df$Y2[is.na(df$Y)]
  
  df <- subset(df, select = -c(X1, Y1, X2, Y2))
  
  colnames(df)[colnames(df) == "Y"] <- "X.1"
  colnames(df)[colnames(df) == "X"] <- "Y"
  colnames(df)[colnames(df) == "X.1"] <- "X"
  
  return(df)
}


#' Data cleaning of data for use with SVAMP
#'
#' A data cleaning process needed to create data to be used in SVAMP
#' @param svasss_dataset Path to dataset of SVASSS data
#' @param ppn_dataset Path to dataset of farm ppns
#' @param movements_dataset Path to animal movement dataset
#' @param svala_dataset Path to SVALA dataset 
#' @return A compound list of datasets
#' @export
#' @include fix_encoding.R
#' @import sp
#' @import rgdal
data_cleaning <- function(svasss_dataset = system.file("extdata/SVASSS.alarms.data_sample.rda", package = "svdc"),
                          ppn_dataset =  system.file("extdata/ppn_sample.csv", package = "svdc"),
                          movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc"),
                          svala_dataset = system.file("extdata/svala.data_sample.rda", package = "svdc"),
                          sjv_dataset = system.file("extdata/sjv.data_sample.rda", package = "svdc"),
                          urax_dataset = system.file("extdata/urax_sample.csv", package = "svdc"))
{
  # Check arguments
  svasss_dataset <- check_file_argument(svasss_dataset)
  ppn_dataset <- check_file_argument(ppn_dataset)
  movements_dataset <- check_file_argument(movements_dataset)
  svala_dataset <- check_file_argument(svala_dataset)
  sjv_dataset <- check_file_argument(sjv_dataset)
  urax_dataset <- check_file_argument(urax_dataset)
  
  
  # NOTE: data have been already converted from ETRS89 to RT90
  data(NUTS_03M, package = "svdc", envir = environment())
  data(postnummer, package = "svdc", envir = environment())

  # SVASSS data
  load(file = svasss_dataset)
  
  # SVALA data
  load(file = svala_dataset)

  # SJV data
  load(file = sjv_dataset)
  
  # URAX data.
  urax <- read.csv2 ("//UBUNTU1/share/urax.csv", header=T, stringsAsFactors = FALSE,
                     encoding = "UTF-8")

  # Encoding of SVASSS data
  SVASSS.alarms.data <- fix_enc(SVASSS.alarms.data)
  SVASSS.CDB.alarms.data <- fix_enc(SVASSS.CDB.alarms.data)
  SVASSS.SJV.alarms.data <- fix_enc(SVASSS.SJV.alarms.data)

  PPN <- load_ppn_data(ppn_dataset)
  PPN <- rt90_sweref99(PPN)
  
  ani_move <- load_movement_data(movements_dataset)

  # PPN with (PPN_is.na) and without missing coordinates (PPN_xy)
  # Spatial objects, i.e. spdf not created if is.na coords
  PPN_is.na <- PPN[is.na(PPN$X) | is.na(PPN$Y),]
  PPN_xy <- PPN[!is.na(PPN$X) | !is.na(PPN$Y),]

  # Spdf of Postnummers containing PPN with missing coordinates
  PPN_is.na_ND <- PPN_is.na[!duplicated(PPN_is.na$Postnummer),]
  i <- postnummer@data$POSTALCODE %in% PPN_is.na_ND$Postnummer
  i2 <- !postnummer@data$POSTALCODE %in% PPN_is.na_ND$Postnummer
  postnum_miss <- postnummer[i,]
  postnum_not_miss <- postnummer[i2,]
  postnum_miss <- spTransform(postnum_miss, CRS("+init=epsg:3021"))
  postnum_not_miss <-spTransform(postnum_not_miss, CRS("+init=epsg:3021"))
  
  # PPN not duplicated
  farms_RT90 <- subset(PPN_xy, !duplicated(PPN_xy$Ppn))
  coordinates(farms_RT90) <- c("X","Y")
  proj4string(farms_RT90) <- CRS("+init=epsg:3021")

  ## Veterinary disctrict dataset
  data(district_geo_RT90, package = "svdc", envir = environment())
  
  # Labels for Län of static outbreak map
  data(nuts_label, package = "svdc", envir = environment())

  # List of PPN, X and Y not duplicated
  ppnlist <- PPN[c("Ppn","X","Y","Kommun", "Adress", "Postnummer", "Postadress")]
  ppnlist <- ppnlist[!duplicated(ppnlist$Ppn),]

  # Save file

  result <- list(PPN = PPN,
                 farms_RT90 = farms_RT90,
                 PPN_is.na = PPN_is.na,
                 postnum_miss = postnum_miss,
                 postnum_not_miss = postnum_not_miss,
                 ani_move = ani_move,
                 district_geo_RT90 = district_geo_RT90,
                 nuts_label = nuts_label,
                 SVASSS.alarms.data = SVASSS.alarms.data,
                 SVASSS.SJV.alarms.data = SVASSS.SJV.alarms.data,
                 SVASSS.CDB.alarms.data = SVASSS.CDB.alarms.data,
                 sjv.data = sjv.data,
                 urax = urax,
                 ppnlist = ppnlist)

  return(result)
}

result <- data_cleaning(ppn_dataset = "//UBUNTU1/share/PPN_records.csv",
movements_dataset = "//UBUNTU1/share/Notforflyttningar.csv",
svasss_dataset = "//UBUNTU1/share/SVASSS.alarms.data.RData",
sjv_dataset = "//UBUNTU1/share/sjv.data.RData", 
urax_dataset = "//UBUNTU1/share/urax.csv")
# 
save(result, file = "//UBUNTU1/share/result.rda")



