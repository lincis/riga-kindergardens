library(magrittr)
library(tidyverse)
library(ggmap)

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

register_google(Sys.getenv("GOOGLE_API_KEY"))

public.kgs %<>%
  dplyr::mutate(
    all.address.data = lapply(institution_name, geocode, output = "all")
    # , latitude = lapply(all.address.data, function(x) x[[1]]$geometry$location$lat)
  )

public.kgs %<>%
  dplyr::mutate(
    latitude = sapply(all.address.data, function(x){
      tryCatch(
        x$results[[1]]$geometry$location$lat
        , error = function(e) NA_real_
      )
    })
    , longitude = sapply(all.address.data, function(x){
      tryCatch(
        x$results[[1]]$geometry$location$lng
        , error = function(e) NA_real_
      )
    })
    , address = sapply(all.address.data, function(x){
      tryCatch(
        x$results[[1]]$formatted_address
        , error = function(e) NA_character_
      )
    })
  )

# # Rīgas pirmsskolas izglītības iestāde "Pienenītes"
# public.kgs$address[153] <- "Mores iela 8, Ziemeļu rajons, Rīga, LV-1034"
# public.kgs$latitude[153] <- 57.0178339
# public.kgs$longitude[153] <- 24.1336834
# 
# # Rīgas 259. pirmsskolas izglītības iestāde
# public.kgs$address[43] <- "Jāņa Grestes iela 3, Latgales priekšpilsēta, Rīga, LV-1021"
# public.kgs$latitude[43] <- 56.9368019
# public.kgs$longitude[43] <- 24.2059763

saveRDS(public.kgs, here::here("r-data/public.kgs.rds"))
