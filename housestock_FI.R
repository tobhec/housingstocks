################################################################################
##################### FINLAND HOUSESTOCK #######################################
################################################################################


# Household-dwelling units (as I understand it, this is basically occupied dwellings) 
# downloaded from
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__asas/statfin_asas_pxt_116a.px/
# via API. This is used as a proxy for dwellings, to extend total dwellings from 2023 (only available year).
#
# Dwellings (only year 2023) is downloaded from 
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__asas/statfin_asas_pxt_116f.px/table/tableViewLayout1/
# via API, and extended by Household-dwelling units
#
# Monthly totalm2 (only for completions of new buildings. Not possible to find)
# for dwellings specifically) downloaded from 
# https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__ras/statfin_ras_pxt_12fy.px/table/tableViewLayout1/
# via API, and then recalculated based on number of dwellings and averagem2/dwelling (as estimated below)
#
# Averagem2/dwelling is calculated by averaging monthly totalm2, and dividing yearly totalm2 
# of completed buildings with number of dwellings.
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



years <- dwellings_occ_md3[1, ][[1]]
dwellings_md3["dwellings_occ", years] <- dwellings_occ_md3[1, ][[2]]



imputena()



################################################################################
##################### TOTALM2 & COMPLETIONS ####################################
################################################################################

### ADD BUILDING STARTS
# Load the raw data
pxq <- pxweb_query("C://Users//hecttob//git//housingstocks//json_queries//totalm2_compl_FI.json")
pxd <- pxweb_get("https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/ras/statfin_ras_pxt_12fy.px", 
                 query = pxq)
completions_totalm2 <- as.data.table(as.data.frame(pxd, column.name.type = "text", variable.value.type = "text"))


