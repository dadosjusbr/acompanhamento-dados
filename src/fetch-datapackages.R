#| label: libs
library(tidyverse)
library(frictionless)
library(here)
library(jsonlite)

# URL package
pkgURL <- "https://dadosjusbr.org/download/datapackage/tjsc/tjsc-2021.zip"
orgaos <- fromJSON("https://api.dadosjusbr.org/v1/orgaos") %>% as_tibble()

orgaos %>% 
  unnest(collecting) %>% 
  unnest(description) %>% 
  mutate(timestamp = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))

# read from zip
read_dadosjus <- function(pkgURL, myDir) {
  
  pkgDest <- str_extract(pkgURL, "(?<=datapackage\\/).*") 
  pkgDir <- str_remove(pkgDest, "\\/.*$")
  
  pkgDest <- here(myDir, pkgDest)
  pkgDir <- here(myDir, pkgDir)
  
  dir.create(pkgDir, recursive = TRUE)
  
  message(str_glue("get {pkgURL}\n\n"))
  download.file(pkgURL, mode = "wb", destfile = pkgDest)
  Sys.sleep(2)
  
  message(str_glue("unzip {pkgDest} to {pkgDir}\n\n"))
  unzip(pkgDest, exdir = pkgDir)
  Sys.sleep(2)
  
  # leitura do pacote
  message("read DadosJusBr datapackage\n")
  package <- frictionless::read_package(here(pkgDir, 'datapackage.json'))
  
  unlink(here("R/data"), recursive = TRUE)
  
  return(package)
  
}