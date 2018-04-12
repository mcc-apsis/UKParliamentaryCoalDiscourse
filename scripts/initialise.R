# Load libraries
library(rvest)
library(httr)
library(stringi)
#library(XML)

# Load own functions (from functions folder)
dump <- lapply(list.files("../functions", pattern = ".*\\.R"), source)
rm("dump")