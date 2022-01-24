## Script to download SST for OCC sites
## Specifically the "NOAA Coral Reef Watch Daily 5 km" data
## Juliette Verstaen
## Jan 2022


# Packages

library(readxl)
library(httr)
library(here)
library(tidyverse)

# Pathway to the OCC file on the server to save
gold_path <- "//PICGOLDFISH/GENERAL/Oceanography/CNMI_OCC_SST/"

# Read in the sites file, we will need this for the lat, long, and site name
site_locations <- readxl::read_xlsx("occ_sites/CNMI-OCC-sites.xlsx") %>%
  filter(STATUS == "Active")

# Some data checks
## 1. are any site IDs listed twice? There should be 101

length(unique(site_locations$OCC_SITEID)) ## there are 100. SAI-017 is listed twice, but with different lats/log and depths. Since this is a one off download script I'm going to skip it in the looped script and then go and download it manually for the two coordinates after. However if this becomes something that we do again I will need to come up with streamlined and reproducable approach to this.

# Create a vector with all the sites we are interested in
site_list <- c(site_locations$OCC_SITEID)

## didn't finish and computer went to sleep at site SAI-016. will start at this site and do over in case it didn't finish all the way and then finish the rest.
site_list <- site_list[93:101]


for(s in site_list) {

  ## skip SAI-017 because there are two sets of coordinates associated with it; downloaded     manually after loop
  if(s == "OCC-SAI-017") next

  ## grab just the coordinates for the site id we're interested in
  filtered_row <- filter(site_locations, OCC_SITEID == s)

  lon <- filtered_row$LONGITUDE
  lat <- filtered_row$LATITUDE

  ## url for the data
  url_link <- httr::GET(paste("https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.csv?analysed_sst%5B(1985-01-01T12:00:00Z):(2021-12-31T12:00:00Z)%5D%5B(", lat, ")%5D%5B(", lon, ")%5D&.draw=lines&.vars=time%7Canalysed_sst%7C&.color=0x000000&.bgColor=0xffffffff", sep = ""))

  ## get csv data from the url
  bin <- content(url_link, "raw")
  writeBin(bin, "sst.csv")
  sst <- read_csv("sst.csv")
  sst <- sst[-1,] ## there's some extra unnecessary info in the first line under the header    we can just get rid of
  sst <- sst %>%
    mutate(OCC_SITEID = s)

  ## save the file
  write_csv(sst, paste(gold_path, "SST-", s, ".csv", sep = ""))


  ## We want to check and make sure that there aren't a bunch of NAs. If that is the case      that means that we are on/near land (some of the points are close to shore so they might     accidentally land there in the 5 km). We want to document which ones these are so we can     change the centroid

  na_counts_file <- read_csv("occ_sites/sst_na_counts_location.csv") %>%
   mutate(LATITUDE = as.numeric(LATITUDE),
          LONGITUDE = as.numeric(LONGITUDE),
          NA_COUNT = as.numeric(NA_COUNT))

  count_na <- sum(is.na(sst$analysed_sst))
  na_counts_file <- tibble::add_row(na_counts_file,
                 OCC_SITEID = s,
                  LATITUDE = lat,
                  LONGITUDE = lon,
                  NA_COUNT = count_na)

## save it and overwrite old file
  write_csv(na_counts_file, "occ_sites/sst_na_counts_location.csv")


}


# Grabbing the data associated with the duplicate site

## Need to grab the two coordinate rows associated with OCC-SAI-017. The following is just a repeat of the code in the loop above, but specifying the site, and lat/longs. The first set of coordinates associated with SAI-017 will be called lon_1/lat_1 and the second lon_2/lat_2.

### First one
s <- "OCC-SAI-017"

lon_1 <- as.numeric('145.7679')
lat_1 <- as.numeric('15.25618')

## url for the data
url_link <- httr::GET(paste("https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.csv?analysed_sst%5B(1985-01-01T12:00:00Z):(2021-12-31T12:00:00Z)%5D%5B(", lat_1, ")%5D%5B(", lon_1, ")%5D&.draw=lines&.vars=time%7Canalysed_sst%7C&.color=0x000000&.bgColor=0xffffffff", sep = ""))

## get csv data from the url
bin <- content(url_link, "raw")
writeBin(bin, "sst.csv")
sst <- read_csv("sst.csv")
sst <- sst[-1,] ## there's some extra unnecessary info in the first line under the header we can just get rid of
sst <- sst %>%
  mutate(OCC_SITEID = s)


## save the file
write_csv(sst, paste(gold_path, "SST-", s, "-1.csv", sep = ""))

## Na counts check
na_counts_file <- read_csv("occ_sites/sst_na_counts_location.csv") %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE),
         NA_COUNT = as.numeric(NA_COUNT))

count_na <- sum(is.na(sst$analysed_sst))
na_counts_file <- tibble::add_row(na_counts_file,
                                  OCC_SITEID = s,
                                  LATITUDE = lat_1,
                                  LONGITUDE = lon_1,
                                  NA_COUNT = count_na)

## save it and overwrite old file
write_csv(na_counts_file, "occ_sites/sst_na_counts_location.csv")


### Second one
s <- "OCC-SAI-017"

lon_2 <- as.numeric('145.7676')
lat_2 <- as.numeric('15.25614')

## url for the data
url_link <- httr::GET(paste("https://oceanwatch.pifsc.noaa.gov/erddap/griddap/CRW_sst_v3_1.csv?analysed_sst%5B(1985-01-01T12:00:00Z):(2021-12-31T12:00:00Z)%5D%5B(", lat_2, ")%5D%5B(", lon_2, ")%5D&.draw=lines&.vars=time%7Canalysed_sst%7C&.color=0x000000&.bgColor=0xffffffff", sep = ""))

## get csv data from the url
bin <- content(url_link, "raw")
writeBin(bin, "sst.csv")
sst <- read_csv("sst.csv")
sst <- sst[-1,] ## there's some extra unnecessary info in the first line under the header we can just get rid of
sst <- sst %>%
  mutate(OCC_SITEID = s)


## save the file
write_csv(sst, paste(gold_path, "SST-", s, "-2.csv", sep = ""))

## Na counts check
na_counts_file <- read_csv("occ_sites/sst_na_counts_location.csv") %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE),
         NA_COUNT = as.numeric(NA_COUNT))

count_na <- sum(is.na(sst$analysed_sst))
na_counts_file <- tibble::add_row(na_counts_file,
                                  OCC_SITEID = s,
                                  LATITUDE = lat_2,
                                  LONGITUDE = lon_2,
                                  NA_COUNT = count_na)

## save it and overwrite old file
write_csv(na_counts_file, "occ_sites/sst_na_counts_location.csv")

