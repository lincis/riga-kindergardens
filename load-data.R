library(magrittr)
library(tidyverse)

loadDataFromApi <- function(link, skip = 0) {
  link <- paste0(link, "?$skip=", skip)
  df.list <- list()
  while(TRUE){
    message("Parse link ", link)
    json.data <- jsonlite::fromJSON(link)
    current.df <- json.data[["value"]]
    if(is.null(current.df))
      break
    if(nrow(current.df) < 1)
      break
    df.list <- c(df.list, list(current.df))
    message(nrow(current.df), " records obtained")
    link <- json.data[["odata.nextLink"]]
    if(length(link) < 1)
      break
  }
  dplyr::bind_rows(df.list)
}

all.applications <- loadDataFromApi("https://opendata.riga.lv/odata/service/KgApplications2")
all.admissions <- loadDataFromApi("https://opendata.riga.lv/odata/service/AdmissionStatistics")
all.private.kg <- loadDataFromApi("https://opendata.riga.lv/odata/service/KgEstimates")

public.kgs <- all.admissions %>%
  dplyr::select(starts_with("institution")) %>%
  dplyr::distinct()

.queryNominatim <- function(address) {
  message(address)
  result <- tryCatch({
    httr::GET(
      paste0("https://nominatim.openstreetmap.org/?format=json&q=", address, "&format=json&limit=1&countrycodes=lv")
    ) %>% httr::content()
    } , error = function(e) { NULL }
  )
  return(result)
}

queryNominatim <- function(address) {
  result <- .queryNominatim(address)
  while (!is.list(result)){
    message("sleep .25 secs")
    Sys.sleep(.25)
    result <- .queryNominatim(address)
  }
  result
}

# lapply(public.kgs$institution_name %>% head(5), queryNominatim) %>% View()

public.kgs %<>%
  dplyr::mutate(
    all.address.data = lapply(institution_name, queryNominatim)
    , latitud
  )
