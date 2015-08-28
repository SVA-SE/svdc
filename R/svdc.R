#' Data cleaning of data for use with SVAMP
#'
#' @title data_cleaning
#'
#' @param svasss_dataset path of dataset of svass report
#' @param ppn_dataset Path of dataset of farm ppns
#' @param movements_dataset path of animal movement dataset
#'
#' @return A compound list of datasets
#'
#' @description A data cleaning process needed to create data to be used in SVAMP
#'
#' @export
#'
#' @import
#' sp
#'


#load PPN data from Rapportportalen

data_cleaning <-function(svasss_dataset = "data/SVASSS.alarms.data_sample.RData",
                         ppn_dataset =  system.file("extdata/ppn_sample.csv", package = "svdc"),
                         movements_dataset = system.file("extdata/ani_move_sample.csv", package = "svdc")){
  
#    load boundary data of Sweden from SVAR package: OBS data have been already converted from ETRS89 to RT90
#    load(system.file("extdata/NUTS_03M.rda", package = "svdc", mustWork = TRUE))
     data(NUTS_03M, package = "svdc", envir = environment())
     data(postnummer, package = "svdc", envir = environment())

  #postnummer <- spTransform(postnummer, CRS("+init=epsg:3021"))

  #SVASSS data
  load(file = svasss_dataset)
  
  
  #URAX data. Those are toy data. As soon as we'll have true urax data change the path
  # urax <- read.csv("C:/project/R/proj/gis/data/URAX/prover.csv", sep=";",
  #                  header=T, stringsAsFactors = FALSE, dec=",", encoding='latin1')

  #Function to encode factors and characters
  fix_enc<-function(df){
    for(x in 1:ncol(df)){
      if(identical(is.factor(df[,x]),TRUE)) {
        Encoding(levels(df[,x]))<-"latin1"
        df[,x]<-enc2utf8(levels(df[,x]))
      }
      if(identical(is.character(df[,x]),TRUE)) {
        Encoding(df[,x])<-"latin1"
        df[,x]<-enc2utf8(df[,x])
      }
    }
    return(df)
  }

  #Encoding of SVASSS data
  SVASSS.alarms.data<-fix_enc(SVASSS.alarms.data)
  SVASSS.CDB.alarms.data<-fix_enc(SVASSS.CDB.alarms.data)
  SVASSS.SJV.alarms.data<-fix_enc(SVASSS.SJV.alarms.data)

  #load PPN data from Rapportportalen
  PPN <- read.csv(file = ppn_dataset, sep=";", header=T, stringsAsFactors = FALSE, dec=",", encoding='UTF-8')
  
  if(!(length(names(PPN)) == 43)){
    stop("The number of columns in the PPN dataset should be 43")
  }
  
  ppn_sample_names <- names(read.csv2(system.file("extdata/ppn_sample.csv", package = "svdc")))
  ppn_names <- names(ppn_dataset)
  
  if(!identical(ppn_sample_names, ppn_names)){
    stop("The columns names in the PPN dataset do not match the ordinary PPN columns names")

  }
  
  PPN <- subset(PPN, PPN$Platsstatuskod == "G" |
                  PPN$Platsstatuskod == "O")

  #Add column to sum total number of animals per each PPN
  PPN$tot_anim <- apply(PPN[c("Antal",
                              "Antalslaktplatser",
                              "Antalsuggplatser",
                              "CDB.Antal",
                              "Maxkapacitet")], 1, sum, na.rm=TRUE)

  PPN$tot_anim[(is.na(PPN$Antal) &
                  is.na(PPN$Antalslaktplatser) &
                  is.na(PPN$Antalsuggplatser) &
                  is.na(PPN$CDB.Antal) &
                  is.na(PPN$Maxkapacitet))] <- NA

  #OBS! X and Y are inverted in the original dataset of JBV
  colnames(PPN)[5] <- "Y"
  colnames(PPN)[6] <- "X"

  #create a new column to list the species present in each PPN
  z<-PPN[c("Ppn","Typ")]
  z1<- split(z$Typ, z$Ppn)
  a<-unlist(lapply(z1,function(x){
    paste(unique(x), collapse=", ")
  }))

  final<-data.frame("Ppn"=labels(a), "Species"=a, stringsAsFactors=FALSE)
  final$Ppn <- as.integer(final$Ppn)

  PPN<-cbind(PPN,
             'Species'=final$Species[match(PPN$Ppn, final$Ppn)],
             stringsAsFactors=FALSE)

  PPN$Species[PPN$Species==""]<-"Unknown"


  # Import movement dataset and format according to EpiContactTrace (Thomas Rosendal code)
  ani_move <- read.csv2(movements_dataset, as.is=TRUE, encoding='UTF-8')
  file.info("ani_move")
  ani_move <- ani_move[,c(5,6,8,7)]
  names(ani_move) <- c('source', 'destination', 'Type', 't')
  ani_move$t <- as.Date(ani_move$t)
  ani_move2 <- subset(ani_move, subset=Type==2|Type==4)
  ani_move3 <- subset(ani_move, subset=Type==1|Type==5)
  ani_move4 <- ani_move3
  ani_move4$source <- ani_move3$destination
  ani_move4$destination <- ani_move3$source
  ani_move <- rbind(ani_move2, ani_move4)
  ani_move <- subset(ani_move, select=-Type)

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

  # PPN not duplicated
  farms_RT90 <- subset(PPN_xy, !duplicated(PPN_xy$Ppn))
  coordinates(farms_RT90) <- c("X","Y")
  proj4string(farms_RT90) <- CRS("+init=epsg:3021")

  #movement dataset last 100 days
  ani_move <- ani_move[ani_move$t>Sys.Date() - 100,]

  #Veterinary disctrict dataset
  data(district_geo_RT90, package = "svdc", envir = environment())
  Encoding(names(district_geo_RT90@data))<-"latin1"
  names(district_geo_RT90@data)<-enc2utf8(names(district_geo_RT90@data))
  district_geo_RT90@data$Lan <- as.character(district_geo_RT90@data$Lan)
  district_geo_RT90@data$Distriktsveterinar <- as.character(district_geo_RT90@data$Distriktsveterinar)
  district_geo_RT90@data$Adress <- as.character(district_geo_RT90@data$Adress)
  district_geo_RT90@data$Kommun <- as.character(district_geo_RT90@data$Kommun)
  Encoding(district_geo_RT90@data$Lan) <- "latin1"
  Encoding(district_geo_RT90@data$Distriktsveterinar) <- "latin1"
  Encoding(district_geo_RT90@data$Adress) <- "latin1"
  Encoding(district_geo_RT90@data$Kommun) <- "latin1"
  district_geo_RT90@data$Lan <- enc2utf8(district_geo_RT90@data$Lan)
  district_geo_RT90@data$Distriktsveterinar <- enc2utf8(district_geo_RT90@data$Distriktsveterinar)
  district_geo_RT90@data$Adress <- enc2utf8(district_geo_RT90@data$Adress)
  district_geo_RT90@data$Kommun <- enc2utf8(district_geo_RT90@data$Kommun)

  #Labels for Län of static outbreak map
  data(nuts_label, package = "svdc", envir = environment())

  #List of PPN, X and Y not duplicated
  ppnlist<-PPN[c("Ppn","X","Y","Kommun", "Adress", "Postnummer", "Postadress")]
  ppnlist <- ppnlist[!duplicated(ppnlist$Ppn),]

  #Save file

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
                 ppnlist = ppnlist)
  
  return(result)

  # Save function for urax's toy data.
  # As soon as you'll have acces to real data save the object in ppn.rdata

  # save(urax, file = "map_report/data/urax.rdata")

}
