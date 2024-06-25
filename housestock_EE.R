################################################################################
##################### ESTONIA HOUSESTOCK #######################################
################################################################################


# Number of dwellings downloaded directly from 
# https://andmed.stat.ee/en/stat/majandus__kinnisvara__elamumajandus/KVE01/table/tableViewLayout2
# via API
#
# Total M2 unavailable directly so average M2 for newly constructed buildings is used as a proxy to calculate totalm2
# downloaded from https://andmed.stat.ee/en/stat/majandus__ehitus__ehitus-ja-kasutusload/EH06/table/tableViewLayout2
# via API
#
# Completions downloaded directly for house, flats and total from 
# https://andmed.stat.ee/en/stat/majandus__ehitus__ehitus-ja-kasutusload/EH06/table/tableViewLayout2
# via API


#install.packages("MDecfin")
#install.packages("pxweb")

library(MDecfin)
library(pxweb)

mydict = c("Number of dwellings" = "all",
           "Dwellings, total" = "all",
           "Number of dwellings in one-dwelling houses" = "house",
           "Number of dwellings in two-dwelling houses" = "house",
           "Number of dwellings in terraced houses" = "house",
           "Number of dwellings in blocks of flats" = "flat",
           "Average floor area of dwelling, m²" = "averagem2"
)

################################################################################
##################### DWELLINGS ################################################
################################################################################
 

# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//dwellings_EE.json")
pxd <- pxweb_get("https://andmed.stat.ee/api/v1/en/stat/KVE01", query = pxq)
dwellings <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Rename column names to be more simplified
names(dwellings)[names(dwellings) == "Type of dwelling"] <- "type"
names(dwellings)[names(dwellings) == "KVE01: DWELLINGS, 1 JANUARY"] <- "dwellings"
names(dwellings)[names(dwellings) == "Year"] <- "year"


# Rename property types to simplified names
dwellings$type  <- mydict[dwellings$type]

# Change NAs to 0s
dwellings <- dwellings[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
dwellings <- dwellings[, c("year", "type", "dwellings")]

################################################################################
##################### TOTALM2 & COMPLETIONS ####################################
################################################################################


# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//totalm2_compl_EE.json")
pxd <- pxweb_get("https://andmed.stat.ee/api/v1/en/stat/EH06", 
                 query = pxq)
completions <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Rename column names to be more simplified, and convert totalm2 from 1000sqm to 1sqm
names(completions)[names(completions) == "Reference period"] <- "year"
names(completions)[names(completions) == "EH06: DWELLING COMPLETIONS (NEW CONSTRUCTION)"] <- "completions"
names(completions)[names(completions) == "Indicator"] <- "type"

# Rename property types to simplified names
completions$type  <- mydict[completions$type]

# Change NAs to 0s
completions <- completions[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Aggregate house-type subsections into house and flats
completions <- completions[, .(completions = sum(completions)), by =  c("type", "year")]

# Use average m2 for new completions as a proxy to calculate total m2 for all dwellings

stock <- merge(dwellings, completions[type == "averagem2"], by = "year")
stock[, totalm2 := dwellings*completions]
stock <- stock[, c("year", "type.x", "dwellings", "completions", "totalm2")]
names(stock)[names(stock) == "type.x"] <- "type"

################################################################################
##################### MAPADOMO LOADING #########################################
################################################################################


# Load MAPADOMO data in case it can be used to extend
mapadomo_full <- fread("C://Users//hecttob//git//housingstocks//mapadomo_data//EU_national.csv", header = TRUE)
mapadomo_dwellings <- mapadomo_full[nuts == "EE" & indicator == "dwellings"]
mapadomo_m2 <- mapadomo_full[nuts == "EE" & indicator == "totalm2"]