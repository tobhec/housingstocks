################################################################################
##################### SWEDEN HOUSESTOCK ########################################
################################################################################


# Dwellings downloaded  from
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T04/
# via API.
#
# Totalm2 downloaded (only every fifth year) from
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__MI__MI0810__MI0810B/BostLokiTatortReg/
# via API.
#
# Interpolation is done on totalm2 using number of dwellings. (Currently it is using imputena())
# from the package MDecfin to do this. However, with the current approach it simply uses dwellings as the proxy,
# effectively assuming the same averagem2/dwelling as the last OBSERVED (totalm2) year.
# A more accurate approach should be to assume a linear trend in averagem2/dwelling between the observed years,
# and use this as the proxy (since averagem2/dwelling does not show a constant trend over time for Sweden, 
# but rather an increasing trend). I have not had time so far to implement this, but when it is done,
# this text should be changed.
#
# Completions (only one/two dwellings and multidwellings buildings found) are downloaded from
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0101__BO0101A/LghReHustypAr/
# via API (Note that it is the table called 
# Completed dwellings and number of rooms including kitchen in newly constructed buildings by region and type of building. Year 1975 - 2023 [2024-05-15]
# and not
# Completed dwellings in newly constructed buildings by region, size of dwelling and type of building. Year 1975 - 2023 [2024-05-15]
# For some reason I have not been able to figure out some years show different values. I chose the one 
# closer to Mapadomo
#
# Demolitions downloaded from
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0101__BO0101F/LagenhetRivAkBpLtRAr/
# and from 
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0101__BO0101F/LagenhRivAkBpLtRArN/
# via API, and then binded together (since they give different year periods)
#
# Conversions taken from 
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0101__BO0101D/LghTillOmbUpplatArN/
# via API

#install.packages("MDecfin")
#install.packages("pxweb")

library(MDecfin)
library(pxweb)

mydict = c("one- or two-dwelling buildings" = "house",
           "one- or two dwelling buildings" = "house",
           "multi-dwelling buildings" = "flat",
           "multi-dwelling and commercial buildings" = "flat",
           "industrial buildings" = "flat",
           "other buildings" = "flat",
           "special housing" = "flat"
)

################################################################################
##################### DWELLINGS ################################################
################################################################################


# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//json_queries//dwellings_SE.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/BO/BO0104/BO0104D/BO0104T04", 
                  query = pxq)
dwellings <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Rename column names to be more simplified
names(dwellings)[names(dwellings) == "type of building"] <- "type"
names(dwellings)[names(dwellings) == "Number of dwellings"] <- "dwellings"

# Rename property types to simplified names
dwellings$type  <- mydict[dwellings$type]

# Change NAs to 0s
dwellings <- dwellings[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Aggregate house-type subsections into house and flats
dwellings <- dwellings[, .(dwellings = sum(dwellings)), by =  c("type", "year")]

################################################################################
##################### TOTALM2 ##################################################
################################################################################


# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//json_queries//totalm2_SE.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/MI/MI0810/MI0810B/BostLokiTatortReg", 
                  query = pxq)
totalm2 <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Rename column names to be more simplified, and convert totalm2 from 1000sqm to 1sqm
names(totalm2)[names(totalm2) == "type of property"] <- "type"
names(totalm2)[names(totalm2) == "every fifth year"] <- "year"
names(totalm2)[names(totalm2) == "Residential floor space in 1000 sq.m"] <- "totalm2"
totalm2$totalm2  <- totalm2$totalm2 * 1000

# Rename property types to simplified names
totalm2$type  <- mydict[totalm2$type]

# Change NAs to 0s
totalm2 <- totalm2[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Aggregate house-type subsections into house and flats
totalm2 <- totalm2[, .(totalm2 = sum(totalm2)), by =  c("type", "year")]

################################################################################
##################### INTERPOLATION OF TOTALM2 #################################
################################################################################


# Merge totalm2 and dwellings into one dataset (left join on dwellings)
stock <- merge(dwellings, totalm2, by = c("year", "type"), all.x = TRUE)

# Impute NAs for totalm2
stock_md3 <-as.md3(stock)
for(type in c("house", "flat"))
{
  stock_md3[type, "totalm2", ] <- imputena(stock_md3[type, "totalm2", ], 
                                           proxy = stock_md3[type, "dwellings", ][[2]], 
                                           method = "dlog",
                                           direction = "both")
  
}
stock_md3["all", , ] <-  stock_md3["flat", , ] + stock_md3["house", , ]
#as.data.table(stock_md3)

################################################################################
##################### DEMOLITIONS ##############################################
################################################################################


# Load the raw data 1989 - 2019
pxq <- pxweb_query("C://Users//hecttob//json_queries//dem_1989_2019.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/BO/BO0101/BO0101F/LagenhetRivAkBpLtRAr", query = pxq)
dem_1989_2019 <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Load the raw data 2020 - latest
pxq <- pxweb_query("C://Users//hecttob//json_queries//dem_2020_latest.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/BO/BO0101/BO0101F/LagenhRivAkBpLtRArN", query = pxq)
dem_2020_latest <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Bind them together
demolitions <- rbind(dem_1989_2019, dem_2020_latest)
names(demolitions)[names(demolitions) == "Demolition of dwellings in multi-dwelling buildings"] <- "demolitions"

################################################################################
##################### CONVERSIONS ##############################################
################################################################################


# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//json_queries//conversions.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/BO/BO0101/BO0101D/LghTillOmbUpplatArN", query = pxq)
conversions <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))
names(conversions)[names(conversions) == "Converted dwellings in multi-dwelling buildings with government subsidies (addition)"] <- "conversions"

################################################################################
##################### COMPLETIONS ##############################################
################################################################################


# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//json_queries//completions.json")
pxd <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/START/BO/BO0101/BO0101A/LghReHustypAr", query = pxq)
completions_by_type <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))
names(completions_by_type)[names(completions_by_type) == "Completed dwellings in newly constructed buildings"] <- "completions"
names(completions_by_type)[names(completions_by_type) == "type of building"] <- "type"

# Aggregate the two types into total completions
completions <- completions_by_type[, .(completions = sum(completions)), by = c("year")]

################################################################################
##################### MERGING INTO ONE DATASET #################################
################################################################################

# Add demolitions (all)
stock_md3["all", "demolitions", demolitions$year] <- demolitions$demolitions

# Add conversions (all)
stock_md3["all", "conversions", conversions$year] <- conversions$conversions

# Add completions (flats, houses and all)
stock_md3["flat", 
          "completions", 
           unique(completions_by_type$year)] <- completions_by_type[type == "multi-dwelling buildings"]$completions
stock_md3["house", 
          "completions", 
           unique(completions_by_type$year)] <- completions_by_type[type == "one- or two-dwelling buildings"]$completions
stock_md3["all", 
          "completions", 
           completions$year] <- completions$completions


################################################################################
##################### MAPADOMO LOADING #########################################
################################################################################

# Load MAPADOMO data in case it can be used to extend
mapadomo_full <- fread("C:/Users/hecttob/housestock/EU_national.csv", header = TRUE)
mapadomo_dwellings <- mapadomo_full[nuts == "SE" & indicator == "dwellings"]
mapadomo_m2 <- mapadomo_full[nuts == "SE" & indicator == "totalm2"]

################################################################################
##################### ECB LOADING ##############################################
################################################################################

# Load ECB data in case it can be used for extending
#helpmds("ECB/SHI", dim = "FREQ")
#helpmds("ECB/SHI", dim = "REF_AREA")
#helpmds("ECB/SHI", dim = "SHI_INDICATOR")
#helpmds("ECB/SHI", dim = "SHI_SUFFIX")

ecb_completions <- mds("ECB/RESH/A.SE._T.N.NTR.HCOM.SE2._Z.N._Z")

################################################################################
##################### SAVE DATA ################################################
################################################################################


stock <- dcast(as.data.table(stock_md3), type + TIME ~ A, value.var = "obs_value")
fwrite(stock, "C://Users//hecttob//housestock//SE_stock.csv")

################################################################################
##################### LINKS ETC ################################################
################################################################################

# O-T dwellings, multidwellings, other dwellings, special housing
# https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T04/table/tableViewLayout1/
