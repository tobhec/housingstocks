################################################################################
##################### FINLAND HOUSESTOCK #######################################
################################################################################


# Household-dwelling units (as I understand it, this is basically occupied dwellings) 
# downloaded from
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__asas/statfin_asas_pxt_116a.px/
# via API. This is used as a proxy (in terms of growth rates) for dwellings, to backwards-extend total dwellings 
# from 2023 (only available year).
#
# Dwellings (only year 2023) is downloaded from 
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__asas/statfin_asas_pxt_116f.px/table/tableViewLayout1/
# via API, and backwards-extended by Household-dwelling units growth rates
#
# Monthly totalm2 (only for completions and starts of new buildings. Not possible to find)
# for dwellings specifically) downloaded from 
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__ras/statfin_ras_pxt_12fy.px/table/tableViewLayout1/
# via API, and then recalculated based on number of dwellings and averagem2/dwelling (as estimated below)
#
# Averagem2/dwelling is calculated by averaging monthly totalm2, and dividing yearly totalm2 
# of completed buildings with number of dwellings. Non-residential buildings are excluded, since we
# cannot obtain any good proxy of the size of their dwellings. The averagem2 for the dwellings within these types
# of buildings would be very big, since it takes into account the whole size of the property. Therefore, the average size of flats
# as calculated in "blocks of flats" is applied to dwellings in these types of buildings, which is as close as we can get to an estimate.
#
# Completions (monthly) downloaded from
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__ras/statfin_ras_pxt_12fy.px/table/tableViewLayout1/
# via API, and average to get yearly completions


#install.packages("MDecfin")
#install.packages("pxweb")

library(MDecfin)
library(pxweb)

mydict = c("one- or two-dwelling buildings" = "house",
           "one- or two dwelling buildings" = "house",
           "One-dwelling and two-dwelling houses" = "house",
           "Terraced houses" = "house",
           "Blocks of flats" = "flat",
           "multi-dwelling buildings" = "flat",
           "multi-dwelling and commercial buildings" = "flat",
           "industrial buildings" = "flat",
           "other buildings" = "flat",
           "Other buildings" = "flat",
           "special housing" = "flat"
)

################################################################################
##################### DWELLINGS ################################################
################################################################################


# Load the raw data (occupied dwellings)
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//dwellings_occ_FI.json")
pxd <- pxweb_get("https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/asas/statfin_asas_pxt_116a.px", 
                 query = pxq)
dwellings_occ <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Load the raw data (total dwellings 2023)
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//dwellings_FI.json")
pxd <- pxweb_get("https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/asas/statfin_asas_pxt_116f.px", 
                 query = pxq)
dwellings <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))

# Rename column names to be more simplified
names(dwellings)[names(dwellings) == "Type of building"] <- "type"
names(dwellings)[names(dwellings) == "Dwellings"] <- "dwellings" 
names(dwellings)[names(dwellings) == "Year"] <- "year"
names(dwellings_occ)[names(dwellings_occ) == "Type of building"] <- "type"
names(dwellings_occ)[names(dwellings_occ) == "Household-dwelling units"] <- "dwellings_occ"
names(dwellings_occ)[names(dwellings_occ) == "Year"] <- "year"

# Reshape to md3 objects (in order to extend dwellings with the proxy occupied dwellings)
dwellings_md3 <- as.md3(dwellings[, c("type", "year", "dwellings")])
dwellings_occ_md3 <- as.md3(dwellings_occ[, c("type", "year", "dwellings_occ")])

# Extract the years we will extend from the proxy, and fill the dwellings_md3 with NAs for all years we will extend
years_available <- dimcodes(dwellings_md3)$TIME
years_to_extend <- dimcodes(dwellings_occ_md3)$TIME
years_to_extend <- setdiff(years_to_extend, years_available)
dwellings_temp <- dwellings_occ_md3
dwellings_temp[, years_available] <- dwellings_md3[, years_available]
dwellings_temp[, years_to_extend] <- NA
dwellings_occ_md3 <- as.md3(dwellings_occ[, c("type", "year", "dwellings_occ")]) # DELETE LATER

#dwellings_md3[, years_to_extend] <- NA
#dwellings_md3[, years_to_extend] <- NA
#dwellings_md3[, 2022] <- NA
#dwellings_md3

# Use imputena from MD3 package to backwards extend dwellings from year 2023 based on the growth rates of occupied dwellings
for(indicator in dimcodes(dwellings_occ_md3)$type[, 1])
{
  dwellings_temp[indicator,  ] <- imputena(dwellings_temp[indicator, ], 
                                           proxy = dwellings_occ_md3[indicator, ][[2]], 
                                           method = "dlog", 
                                           direction = "backward")
}

# Save dwellings as a data table
dwellings <- as.data.table(dwellings_temp)
names(dwellings)[names(dwellings) == "obs_value"] <- "dwellings"
names(dwellings)[names(dwellings) == "TIME"] <- "year"
dwellings$year <- as.character(dwellings$year)

################################################################################
##################### TOTALM2 & COMPLETIONS ####################################
################################################################################

# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//totalm2_compl_starts_FI.json")
pxd <- pxweb_get("https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/ras/statfin_ras_pxt_12fy.px", 
                 query = pxq)
compl_starts <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))
                              
# Rename column names to be more simplified
names(compl_starts)[names(compl_starts) == "Building type"] <- "type"
names(compl_starts)[names(compl_starts) == "Dwellings (No.)"] <- "dwellings" 
names(compl_starts)[names(compl_starts) == "Floor area (m2)"] <- "m2"
names(compl_starts)[names(compl_starts) == "Month"] <- "year"
names(compl_starts)[names(compl_starts) == "Construction stage"] <- "stage"

# Aggregate the monthly data to yearly
compl_starts$year <- substring(compl_starts$year, 1, 4)
compl_starts <- compl_starts[, .(m2 = sum(m2), dwellings = sum(dwellings)), by = c("year", "type", "stage")]

# Map together the house-typesubsections to match the ones from the dwellings dataset
# Separate at first blocks of flats with other flat buildings (other), they will be aggregated later on
mydict_dwellings <- c("One-dwelling and two-dwelling houses" = "house",
                      "One dwelling and two dwelling houses" = "house",
                      "One-dwelling and two dwelling houses" = "house",
                      "One dwelling and two dwelling houses" = "house",
                      "Terraced houses" = "house",
                      "Blocks of flats" = "flat",
                      "Other buildings" = "other"

)

mydict_compl_starts <- c("011 Detached and semi-detached houses" = "house",
                          "Detached and semi-detached houses" = "house",
                          "012 Blocks of flats" = "flat",
                          "Blocks of flats" = "flat",
                          "013 Residential buildings for communities" = "other",
                          "Residential buildings for communities" = "other",
                          "014 Dwellings for special groups" = "other",
                          "Dwellings for special groups" = "other",
                          "02-19 Non-residential buildings" = "non-residential",
                          "Non-residential buildings" = "non-residential"
)

# Rename property types to simplified names
dwellings$type  <- mydict_dwellings[dwellings$type]
compl_starts$type <- mydict_compl_starts[compl_starts$type]

# Aggregate m2 and dwellings by property types
dwellings <- dwellings[, .(dwellings = sum(dwellings)), by = c("year", "type")]
compl_starts <- compl_starts[, .(m2 = sum(m2), dwellings = sum(dwellings)), by = c("year", "type", "stage")]

# Split completions and building starts
completions <- compl_starts[stage == "Building completions"]
starts <- compl_starts[stage == "Building starts"]

# Multiply total number of dwellings with the averagem2 of new completions by type 
# (used as a proxy for average m2 of all dwellings), to create totalm2
averagem2_proxy <- completions[type != "non-residential"]
averagem2_proxy[, averagem2 := ifelse(dwellings != 0, m2 / dwellings, NA)]
stock <- merge(dwellings, 
               averagem2_proxy[, c("year", "type", "averagem2")], 
               by = c("year", "type"), 
               all = TRUE)
stock[, totalm2 := ifelse(!is.na(dwellings) & !is.na(averagem2), dwellings * averagem2, NA)]

# Aggregate together buildings of type "other" into type "flat"
mydict <- c("house" = "house",
            "flat" = "flat",
            "other" = "flat")
stock$type <- mydict[stock$type]
stock <- stock[, .(dwellings = sum(dwellings), totalm2 = sum(totalm2)), by = c("year", "type")]

# Aggregate together flats and houses to get total dwellings and total m2 for all property types
flat  <- stock[type == "flat"][order(year)]
house <- stock[type == "house"][order(year)]
all   <- data.table(year = unique(flat$year),
                  type = "all",
                  dwellings = flat$dwellings + house$dwellings,
                  totalm2 = flat$totalm2 + house$totalm2)
stock <- rbind(stock, all)[order(type)]

# Aggregate together completions and starts for different property types (CURRENTLY INCLUDING NON-RESIDENTIAL BUILDINGS)
mydict <- c("house" = "house",
            "flat" = "flat",
            "other" = "flat",
            "non-residential" = "flat")
completions$type <- mydict[completions$type]
starts$type <- mydict[starts$type]
completions <- completions[, .(m2 = sum(m2), dwellings = sum(dwellings)), by = c("year", "type")]
starts <- starts[, .(m2 = sum(m2), dwellings = sum(dwellings)), by = c("year", "type")]

# Aggregate together flats and houses of completions to get total dwellings and total m2 for all property types
flat_compl  <- completions[type == "flat"][order(year)]
house_compl <- completions[type == "house"][order(year)]
all_compl   <- data.table(year = unique(completions$year),
                        type = "all",
                        m2 = flat_compl$m2 + house_compl$m2,
                        dwellings = flat_compl$dwellings + house_compl$dwellings)
completions <- rbind(completions, all_compl)[order(type)]
names(completions)[names(completions) == "m2"] <- "m2_completions"
names(completions)[names(completions) == "dwellings"] <- "dwellings_completions"

# Aggregate together flats and houses of building starts to get total dwellings and total m2 for all property types
flat_start  <- starts[type == "flat"][order(year)]
house_start <- starts[type == "house"][order(year)]
all_start   <- data.table(year = unique(starts$year),
                        type = "all",
                        m2 = flat_start$m2 + house_start$m2,
                        dwellings = flat_start$dwellings + house_start$dwellings)
starts <- rbind(starts, all_start)[order(type)]
names(starts)[names(starts) == "m2"] <- "m2_starts"
names(starts)[names(starts) == "dwellings"] <- "dwellings_starts"

################################################################################
##################### MERGING INTO ONE DATASET #################################
################################################################################


stock <- merge(stock, starts, by = c("year", "type"))
stock <- merge(stock, completions, by = c("year", "type"))

################################################################################
##################### MAPADOMO LOADING #########################################
################################################################################


# There is no mapadomo data for Finland

################################################################################
##################### SAVE DATA ################################################
################################################################################


fwrite(stock, "C://Users//hecttob//git//housingstocks//FI_stock.csv")
