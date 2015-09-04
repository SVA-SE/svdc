##' Postal codes
##'
##' @name postnummer
##' @docType data
##' @import sp
##' @keywords datasets
##' @include svdc.R
NULL

##' Administrative (NUTS) units in Sweden (1 : 3 Million)
##'
##' The dataset contains administrative units (NUTS) in Sweden in
##' European terminology in scale 1 : 3 Million.
##'
##' @name NUTS_03M
##' @docType data
##' @source
##' \url{http://epp.eurostat.ec.europa.eu/portal/page/portal/gisco_Geographical_information_maps/popups/references/administrative_units_statistical_units_1}
##' @import sp
##' @keywords datasets
##'
##'
NULL

##' Veterinary districts spatial point dataframe
##'
##' The dataset contains point locations head offices of the
##' veterinary districts in Sweden
##'
##' @name district_geo_RT90
##' @docType data
##' @source
##' Swedish veterinary districts webpage
##' @format A spatial point data frame with 81 observations on the following 8 variables.
##' \describe{
##'   \item{\code{Lan}}{
##'     The county where the location lies. (\code{character})
##'   }
##'
##'   \item{\code{Distriktsveterinar}}{
##'     The name of the district. (\code{character})
##'   }
##'
##'   \item{\code{Adress}}{
##'     The adress of the district. (\code{character})
##'   }
##'
##'   \item{\code{Kommun}}{
##'     The kommun of the district. (\code{character})
##'   }
##'
##'   \item{\code{Telefon}}{
##'     The phone number to the district. (\code{character})
##'   }
##'
##'   \item{\code{Website}}{
##'     The url to the homepage of the district. (\code{character})
##'   }
##'
##'   \item{\code{X}}{
##'     The x coordinate in RT90 of the district. (\code{integer})
##'   }
##'
##'   \item{\code{Y}}{
##'     The y coordinate in RT90 of the district. (\code{integer})
##'   }
##' }
NULL

##' The name of the counties in Sweden (Län)
##'
##' A dataframe of names of counties, and the NUTS3 code
##'
##' @name nuts_label
##' @docType data
##' @source
##' Extractions from Eurostat boundaries (\url{http://epp.eurostat.ec.europa.eu/portal/page/portal/gisco_Geographical_information_maps/popups/references/administrative_units_statistical_units_1})
NULL
