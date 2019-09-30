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
