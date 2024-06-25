################################################################################
##################### DENMARK HOUSESTOCK #######################################
################################################################################


# Dwellings downloaded from
# https://www.statbank.dk/statbank5a/SelectVarVal/Define.asp?MainTable=BOL102&PLanguage=1&PXSId=0&wsid=cftree
# via API.
# 
# Averagem2 downloaded from https://www.statbank.dk/20064 via API
#
# Dwellings and averagem2 are divided into subsections based on type, which is used to calculate
# totalm2 together with the corresponding averagem2 of the type, since no direct data on totalm2 is found. 

#install.packages("httr")
#install.packages("readr")

# Load the packages
library(httr)
library(readr)

mydict = c("Detached houses/farmhouses" = "house",
           "Terraced, linked or semi-detached houses" = "house",
           "Cottages" = "house",
           "Multi-dwelling houses" = "flat",
           "Residential buildings for communities" = "flat",
           "Student hostels" = "flat",
           "Other" = "flat"
)

################################################################################
##################### AVERAGE M2 ###############################################
################################################################################

# Define the API URL
url <- "https://api.statbank.dk/v1/data/BOL106/CSV?lang=en&OMR%C3%85DE=000&ENHED=GNSAB&ANVENDELSE=125%2C130%2C140%2C150%2C160%2C565%2C570&Tid=*"

# Make the GET request to the API
response <- GET(url)

# Check if the request was successful
if(status_code(response) == 200) {
  # Read the CSV content
  averagem2 <- fread(content(response, "text"))
} else {
  # Print an error message if the request was not successful
  print(paste("Error:", status_code(response)))
}

names(averagem2)[names(averagem2) == "ANVENDELSE"] <- "type"
names(averagem2)[names(averagem2) == "TID"] <- "year"
names(averagem2)[names(averagem2) == "INDHOLD"] <- "averagem2"
averagem2 <- averagem2[, c("type", "year", "averagem2")]

# Change NAs to 0s
averagem2 <- averagem2[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

################################################################################
##################### DWELLINGS ################################################
################################################################################

# Define the API URL
url <- "https://api.statbank.dk/v1/data/BOL102/CSV?lang=en&AMT=000&BEBO=*&ANVENDELSE=*&Tid=*"

# Make the GET request to the API
response <- GET(url)

# Check if the request was successful
if(status_code(response) == 200) {
  # Read the CSV content
  dwellings <- fread(content(response, "text"))
} else {
  # Print an error message if the request was not successful
  print(paste("Error:", status_code(response)))
}

names(dwellings)[names(dwellings) == "AMT"] <- "region"
names(dwellings)[names(dwellings) == "BEBO"] <- "unit"
names(dwellings)[names(dwellings) == "ANVENDELSE"] <- "type"
names(dwellings)[names(dwellings) == "TID"] <- "year"
names(dwellings)[names(dwellings) == "INDHOLD"] <- "dwellings"

# Change NAs to 0s
dwellings <- dwellings[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Aggregate unoccupied and occupied dwellings together
dwellings <- dwellings[, .(dwellings = sum(dwellings)), by =  c("type", "year")]

################################################################################
##################### CREATE TOTALM2 ###########################################
################################################################################

# Make sure the two tables align
setorder(dwellings, type, year)
setorder(averagem2, type, year)

# Combine them into one data table
stock <- cbind(dwellings, averagem2$averagem2)
names(stock)[names(stock) == "V2"] <- "averagem2"

# Multiply the average m2 of a dwelling with number of dwellings, based on the different types
stock[, totalm2 := dwellings*averagem2]

# Rename property types to simplified names
stock$type  <- mydict[stock$type]

# Aggregate together houses and flats
stock <- stock[, .(totalm2 = sum(totalm2), dwellings = sum(dwellings)), by = .(type, year)]

# Aggregate together totalm2 of the different types for each year
stock_all <- stock[, .(totalm2 = sum(totalm2), dwellings = sum(dwellings)), by = .(year)]
stock_all[, type := "all"]
setcolorder(stock_all, c("type", setdiff(names(stock_all), "type")))

# Bind together
stock <- rbind(stock, stock_all)

################################################################################
##################### MAPADOMO LOADING #########################################
################################################################################


# Load MAPADOMO data in case it can be used to extend
mapadomo_full <- fread("C://Users//hecttob//git//housingstocks//mapadomo_data//EU_national.csv", header = TRUE)
mapadomo_dwellings <- mapadomo_full[nuts == "DK" & indicator == "dwellings"]
mapadomo_m2 <- mapadomo_full[nuts == "DK" & indicator == "totalm2"]