
#
##
### LOAD DATA
##
#

# Load UCDP conflict data
library(readr)
UCDParmeddata <- read_csv("UcdpPrioConflict_v23_1.csv")

# Load IPCC risk data
#install.packages("readxl")
library(readxl)
library(haven)
IPCCindicatordata <- read_excel("ipcc-inform-gri-2019-v0-3-7-xlsx.xlsx", sheet = "Indicator Data", skip = 1, col_names = T)
  IPCCindicatordata <- IPCCindicatordata[-c(1:3),] # Remove first three rows, which were extra info from Excel

IPCCinformdata <- read_excel("ipcc-inform-gri-2019-v0-3-7-xlsx.xlsx", sheet = "INFORM Mid2019 (a-z)", cell_rows(2:194), col_names = T)
  IPCCinformdata <- IPCCinformdata[-1,] # Remove first row, which was extra info from Excel

# Load Hensel colonial data
Henselcolonialdata <- read_dta("coldata110.dta")

# Load World Bank income classification data
WBclassdata <- read_excel("CLASS.xlsx")

# Load World Bank historical income classification data
WBhistclassdata <- read_excel("Historical Income Data.xlsx")

# Load World Bank population data
WBpopulationdata <- read_csv("World Bank Population Data.csv")

# Load Abel migration data
Abelmigrationdata <- read_csv("gf_imr.csv")

#
##
### SELECT CASES
##
#

# Select UCDP data cases
library(dplyr)
UCDPdata1 <- UCDParmeddata %>% # Only keep relevant variables
  select(conflict_id, location, side_a, side_b, incompatibility, territory_name, year, intensity_level, cumulative_intensity, type_of_conflict, start_date, start_prec, start_date2, start_prec2, ep_end, ep_end_date, ep_end_prec, gwno_a, gwno_b, gwno_loc, region) 
UCDPdata2 <-subset(UCDPdata1, UCDPdata1$start_prec == 1 | UCDPdata1$start_prec == 2 | UCDPdata1$start_prec == 3) # Only keep conflicts with fairly precise date data

# Separate multiple-country rows
library(tidyr)
UCDPdata2 <- separate_rows(UCDPdata2, side_a, sep = ",")
UCDPdata2 <- separate_rows(UCDPdata2, location, sep = ",")

library(stringr)
UCDPdata2 <- UCDPdata2 %>% # Trim white space from row separation
                mutate_if(is.character, str_trim)
#
##
### CREATE MATCHING ISO3 COLUMN
##
#

# Fill out missing gwno_a data in UCDPdata2
#install.packages("countrycode")
library(countrycode)
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Egypt'] <- '651'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Iraq'] <- '645'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Jordan'] <- '663'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Lebanon'] <- '660'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Syria'] <- '652'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of Australia'] <- '900'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of United Kingdom'] <- '200'
UCDPdata2$gwno_a[UCDPdata2$side_a == 'Government of United States of America'] <- '2'

# Create ISO3 column for each dataset
UCDPdata2$gwno_a <- as.numeric(UCDPdata2$gwno_a) 
UCDPdata2$Iso3_a <- NA
UCDPdata2$Iso3_a <- countrycode(UCDPdata2$gwno_a, "gwn", "iso3c", 
                                    custom_match = c("55" = "GRD",
                                                     "345" = "YUG",
                                                     "678" = "YEM",
                                                     "680" = "SYM",
                                                     "751" = "HIN"))
UCDPdata2$Iso3_a <- as.character(UCDPdata2$Iso3_a)

Henselcolonialdata$Iso3 <- NA
Henselcolonialdata$state <- as.numeric(Henselcolonialdata$state)
Henselcolonialdata$Iso3 <- countrycode(Henselcolonialdata$state, "cown", "iso3c",
                                       custom_match = c("240" = "HAN",
                                                        "245" = "BAV",
                                                        "260" = "GFR", 
                                                        "265" = "GDR", 
                                                        "267" = "BAD", 
                                                        "269" = "SAX", 
                                                        "271" = "WRT", 
                                                        "273" = "HSE", 
                                                        "275" = "HSG", 
                                                        "280" = "MEC", 
                                                        "300" = "AUH", 
                                                        "315" = "CZE", 
                                                        "329" = "SIC", 
                                                        "332" = "MOD", 
                                                        "335" = "PMA", 
                                                        "337" = "TUS", 
                                                        "345" = "YUG", 
                                                        "347" = "KOS", 
                                                        "511" = "ZAN", 
                                                        "678" = "YAR", 
                                                        "680" = "YPR", 
                                                        "730" = "KOR", 
                                                        "817" = "RVN"))

Henselcolonialdata$colrulerIso3 <- NA
Henselcolonialdata$colrulerIso3 <- countrycode(Henselcolonialdata$colruler, "cown", "iso3c", custom_match = c("300" = "AUH"))
Henselcolonialdata$indfromIso3 <- NA
Henselcolonialdata$indfromIso3 <- countrycode(Henselcolonialdata$indfrom, "cown", "iso3c", custom_match = c("89" = "PCA",
                                                                                                            "300" = "AUH", 
                                                                                                            "315" = "CZE",
                                                                                                            "345" = "YUG",
                                                                                                            "678" = "YAR",
                                                                                                            "730" = "KOR"))
# Create World Bank Development Indicators Region variable
Henselcolonialdata$WBRegion <- NA
Henselcolonialdata$WBRegion <- countrycode(Henselcolonialdata$Iso3, "iso3c", "region", custom_match = c("AUH" = "Europe & Central Asia", 
                                                                                                       "BAD" = "Europe & Central Asia", 
                                                                                                       "BAV" = "Europe & Central Asia", 
                                                                                                       "GDR" = "Europe & Central Asia", 
                                                                                                       "GFR" = "Europe & Central Asia", 
                                                                                                       "HAN" = "Middle East & North Africa",
                                                                                                       "HSE" = "Europe & Central Asia", 
                                                                                                       "HSG" = "Europe & Central Asia", 
                                                                                                       "KOS" = "Europe & Central Asia", 
                                                                                                       "MEC" = "Europe & Central Asia", 
                                                                                                       "MOD" = "Europe & Central Asia", 
                                                                                                       "PMA" = "Europe & Central Asia", 
                                                                                                       "RVN" = "East Asia & Pacific", 
                                                                                                       "SAX" = "Europe & Central Asia", 
                                                                                                       "SIC" = "Europe & Central Asia", 
                                                                                                       "TUS" = "Europe & Central Asia", 
                                                                                                       "WRT" = "Europe & Central Asia", 
                                                                                                       "YAR" = "Middle East & North Africa", 
                                                                                                       "YPR" = "Middle East & North Africa", 
                                                                                                       "YUG" = "Europe & Central Asia", 
                                                                                                       "ZAN" = "Sub-Saharan Africa"))
Henselcolonialdata$WBRegion[Henselcolonialdata$WBRegion == "North America" | Henselcolonialdata$WBRegion == "Latin America & Caribbean"] <- "North America + Latin America & Caribbean"
Henselcolonialdata$WBRegion <- as.factor(Henselcolonialdata$WBRegion)
Henselcolonialdata$WBRegion <- relevel(Henselcolonialdata$WBRegion, ref = "Sub-Saharan Africa")

# Create consolidated migration data for each country and year
library(dplyr)
Abelmigrationdata2 <- Abelmigrationdata %>%
  group_by(year0, orig) %>%
  summarise(total_flow = sum(flow))

#
##
### JOIN DATA SETS 
##
#

# Join data sets
JOINTdata <- left_join(UCDPdata2, Henselcolonialdata, by = c("Iso3_a" = "Iso3"), relationship = "many-to-one")
JOINTdata <- left_join(JOINTdata, WBclassdata, by = c("Iso3_a" = "Code"), relationship = "many-to-one")
JOINTdata <- left_join(JOINTdata, IPCCinformdata, by = c("Iso3_a" = "ISO3"), relationship = "many-to-one")
JOINTdata <- left_join(JOINTdata, IPCCindicatordata, by = c("Iso3_a" = "ISO3"), relationship = "many-to-one")

#
##
### CLEAN CONFLICT DATA: CREATE UNIQUE
##
#

# Create event month and event year columns
JOINTdata <- JOINTdata %>%
                dplyr::mutate(eventmonth = lubridate::month(start_date))
JOINTdata <- JOINTdata %>%
  dplyr::mutate(eventyear = lubridate::year(start_date))

# Remove conflict duplicates of same conflict_id
JOINTdata <- JOINTdata[order(JOINTdata$conflict_id, JOINTdata$ep_end == "0"),] # Order data to keep observation with recorded ep_end, if present
JOINTdataunique <- distinct(JOINTdata, conflict_id, .keep_all = TRUE) # Remove duplicates 

#
##
### CREATE SPELL VARIABLE
##
#

JOINTdataunique$start_date <- as.Date(JOINTdataunique$start_date)
JOINTdataunique$ep_end_date <- as.Date(JOINTdataunique$ep_end_date)

library(lubridate)
JOINTdataunique$ConflictMonthSpell <- NA
JOINTdataunique$ConflictMonthSpell[JOINTdataunique$ep_end == 1] <- interval(start = ymd(JOINTdataunique$start_date[JOINTdataunique$ep_end == 1]), end = ymd(JOINTdataunique$ep_end_date[JOINTdataunique$ep_end == 1]))/months(1)
JOINTdataunique$ConflictMonthSpell[JOINTdataunique$ep_end == 0] <- interval(start = ymd(JOINTdataunique$start_date[JOINTdataunique$ep_end == 0]), end = ymd("2022-12-31"))/months(1)
JOINTdataunique$ConflictMonthSpell <- as.numeric(JOINTdataunique$ConflictMonthSpell)

#
##
### Cleaning data further and creating variables
##
#

## IPCC RISK DATA
# Var: IPCC Risk class
JOINTdataunique$INFORMRiskClass <- as.factor(JOINTdataunique$'RISK CLASS')
JOINTdataunique$INFORMRiskClass <- factor(JOINTdataunique$INFORMRiskClass, levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Var: IPCC Natural disaster risk level
JOINTdataunique$Natural <- as.numeric(JOINTdataunique$Natural)
JOINTdataunique$INFORMNaturalHazard <- NA
JOINTdataunique$INFORMNaturalHazard[JOINTdataunique$Natural <= 10] <- "Very high"
JOINTdataunique$INFORMNaturalHazard[JOINTdataunique$Natural <= 6.8] <- "High"
JOINTdataunique$INFORMNaturalHazard[JOINTdataunique$Natural <= 4.6] <- "Medium"
JOINTdataunique$INFORMNaturalHazard[JOINTdataunique$Natural <= 2.7] <- "Low" # Lumping low and very low together since there's only one very low
JOINTdataunique$INFORMNaturalHazard <- as.factor(JOINTdataunique$INFORMNaturalHazard)
JOINTdataunique$INFORMNaturalHazard <- factor(JOINTdataunique$INFORMNaturalHazard, levels = c("Low", "Medium", "High", "Very high"))

# Var: IPCC GDP per capita
JOINTdataunique$GDP <- as.numeric(JOINTdataunique$'GDP per capita PPP int USD (Estimated)')

# Var: IPCC Population
JOINTdataunique$Population <- as.numeric(JOINTdataunique$'Total Population')

# Var: IPCC Risk Class Grouped
JOINTdataunique$INFORMRiskClassGrouped <- NA
JOINTdataunique$INFORMRiskClassGrouped[JOINTdataunique$INFORMRiskClass == "Very Low" | JOINTdataunique$INFORMRiskClass == "Low" | JOINTdataunique$INFORMRiskClass == "Medium"] <- "Low to medium"
JOINTdataunique$INFORMRiskClassGrouped[JOINTdataunique$INFORMRiskClass == "High" | JOINTdataunique$INFORMRiskClass == "Very High"] <- "High"
JOINTdataunique$INFORMRiskClassGrouped <- as.factor(JOINTdataunique$INFORMRiskClassGrouped)
JOINTdataunique$INFORMRiskClassGrouped <- factor(JOINTdataunique$INFORMRiskClassGrouped, levels = c("Low to medium", "High"))

# Var: IPCC Risk Class Grouped 2
JOINTdataunique$INFORMRiskGrouped <- NA
JOINTdataunique$INFORMRiskGrouped[JOINTdataunique$INFORMRiskClass == "Very Low" | JOINTdataunique$INFORMRiskClass == "Low"] <- "Low"
JOINTdataunique$INFORMRiskGrouped[JOINTdataunique$INFORMRiskClass == "Medium"] <- "Medium"
JOINTdataunique$INFORMRiskGrouped[JOINTdataunique$INFORMRiskClass == "High" | JOINTdataunique$INFORMRiskClass == "Very High"] <- "High"
JOINTdataunique$INFORMRiskGrouped <- as.factor(JOINTdataunique$INFORMRiskGrouped)
JOINTdataunique$INFORMRiskGrouped <- factor(JOINTdataunique$INFORMRiskGrouped, levels = c("Low", "Medium", "High"))

## HENSEL COLONIAL DATA
# Reformat Hensel colonial independence date
library(zoo)
JOINTdataunique$inddate <- as.integer(JOINTdataunique$inddate)
JOINTdataunique$inddate[JOINTdataunique$gwno_a == "698"] <- 165012
JOINTdataunique$inddate[JOINTdataunique$gwno_a == "630"] <- 197904
JOINTdataunique$inddate[JOINTdataunique$gwno_a == "710"] <- 191201
JOINTdataunique$inddate[JOINTdataunique$gwno_a == "800"] <- 123812
JOINTdataunique$inddate[JOINTdataunique$gwno_a == "616"] <- 195603
JOINTdataunique$inddate <- formatC(JOINTdataunique$inddate, width = 6, format = "d", flag = "0")
JOINTdataunique$inddate <- as.yearmon(as.character(JOINTdataunique$inddate), "%Y%m")

# Var: Hensel years since colonial independence - numeric
JOINTdataunique$HenselIndYearsn <- NA
JOINTdataunique$HenselIndYearsn <- interval(start = my(JOINTdataunique$inddate), end = my("Dec 2022"))/years(1)
JOINTdataunique$HenselIndYearsn <- as.numeric(JOINTdataunique$HenselIndYearsn)

# Var: Hensel state formation wave - categorical
JOINTdataunique$HenselFormationWave <- NA
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate >= 1985] <- "Post-Soviet (+1985)"
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate < 1985] <- "Pt. 2 Anticolonial revolution (1955-1984)"
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate < 1955] <- "Pt. 1 Anticolonial revolution (1945-1954)"
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate < 1946] <- "World Wars (1914-1944)"
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate < 1914] <- "Neo-Europes (1839-1913)"
JOINTdataunique$HenselFormationWave[JOINTdataunique$inddate < 1839] <- "Atlantic revolutions and prior (<1839)"
JOINTdataunique$HenselFormationWave <- as.factor(JOINTdataunique$HenselFormationWave)
JOINTdataunique$HenselFormationWave <- factor(JOINTdataunique$HenselFormationWave, levels = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"))

# Var: Hensel Independence type
JOINTdataunique$indtype <- as.numeric(JOINTdataunique$indtype)
JOINTdataunique$HenselIndType <- NA
JOINTdataunique$HenselIndType[JOINTdataunique$indtype == 1] <- "Formation"
JOINTdataunique$HenselIndType[JOINTdataunique$indtype == 2] <- "Decolonization"
JOINTdataunique$HenselIndType[JOINTdataunique$indtype == 3] <- "Secession"
JOINTdataunique$HenselIndType[JOINTdataunique$indtype == 4] <- "Partition"
JOINTdataunique$HenselIndType <- as.factor(JOINTdataunique$HenselIndType)

# Var: Hensel Decolonized binary
JOINTdataunique$HenselDecolonized <- 0
JOINTdataunique$HenselDecolonized[JOINTdataunique$HenselIndType == "Decolonization"] <- 1

# Var: Hensel Independence violence
JOINTdataunique$indviol <- as.numeric(JOINTdataunique$indviol)
JOINTdataunique$HenselIndViolence <- NA
JOINTdataunique$HenselIndViolence[JOINTdataunique$indviol == 0] <- "Nonviolent independence"
JOINTdataunique$HenselIndViolence[JOINTdataunique$indviol == 1] <- "Violent independence"
JOINTdataunique$HenselIndViolence <- as.factor(JOINTdataunique$HenselIndViolence)
JOINTdataunique$HenselIndViolence <- relevel(JOINTdataunique$HenselIndViolence, ref = "Nonviolent independence")

## UCDP CONFLICT DATA
# Var: UCDP Conflict intensity level
JOINTdataunique$UCDPConflictIntensity <- NA
JOINTdataunique$UCDPConflictIntensity[JOINTdataunique$intensity_level == 1] <- "Minor conflict"
JOINTdataunique$UCDPConflictIntensity[JOINTdataunique$intensity_level == 2] <- "War"
JOINTdataunique$UCDPConflictIntensity <- as.factor(JOINTdataunique$UCDPConflictIntensity)
JOINTdataunique$UCDPConflictIntensity <- factor(JOINTdataunique$UCDPConflictIntensity, levels = c("Minor conflict", "War"))

# Var: UCDP Government conflict
JOINTdataunique$UCDPGovernmentConflict <- "Other actor"
JOINTdataunique$UCDPGovernmentConflict[!is.na(JOINTdataunique$gwno_b)] <- "Government conflict"
JOINTdataunique$UCDPGovernmentConflict <- as.factor(JOINTdataunique$UCDPGovernmentConflict)
JOINTdataunique$UCDPGovernmentConflict <- relevel(JOINTdataunique$UCDPGovernmentConflict, ref = "Other actor")

# Var: UCDP Side B IPCC Risk Class
JOINTdataunique$UCDPSideBClass <- NA
JOINTdataunique$UCDPSideBClass <- JOINTdataunique$INFORMRiskClass[match(JOINTdataunique$gwno_b, JOINTdataunique$gwno_a)]
JOINTdataunique$UCDPSideBClass <- coalesce(JOINTdataunique$UCDPSideBClass, JOINTdataunique$INFORMRiskClass)
JOINTdataunique$UCDPSideBClass <- as.factor(JOINTdataunique$UCDPSideBClass)

## WB INCOME DATA 
# Var: WB Income group - categorical
JOINTdataunique$`Income group`[JOINTdataunique$gwno_a == 101] <- "Lower middle income"
JOINTdataunique$WBIncomeGroup <- as.factor(JOINTdataunique$`Income group`)

## CREATE INTERACTIONS
# Var: Hensel state formation wave and violent independence interaction
JOINTdataunique$HenselINTIndYears <- interaction(JOINTdataunique$HenselFormationWave, JOINTdataunique$HenselIndViolence)

## Create long dataset where each row is one conflict-month
JOINTdataunique$ep_end_last <- NA
JOINTdataunique$ep_end_last <- as.Date(JOINTdataunique$ep_end_last)

JOINTdataunique$ep_end_last[JOINTdataunique$ep_end == 0] <- "2022-12-31"
JOINTdataunique$ep_end_last[JOINTdataunique$ep_end == 1] <- JOINTdataunique$ep_end_date[JOINTdataunique$ep_end == 1]
JOINTdataunique$ep_end_last <- as.Date(JOINTdataunique$ep_end_last)

library(tidyverse)
JOINTdatalong <- JOINTdataunique %>%
  group_by(conflict_id) %>%
  mutate(dates = list(seq(ymd(start_date), ymd(ep_end_last), by = as.difftime(months(1))))) %>%
  unnest(dates) %>%
  mutate(to = lead(dates)) %>%
  mutate(to = coalesce(to, ep_end_date)) %>%
  mutate(year = lubridate::year(to))

# Create join variable for JOINTdatalong to Abelmigration2
JOINTdatalong$migyear <- NA
JOINTdatalong$migyear[JOINTdatalong$year == 1960 | JOINTdatalong$year == 1961 | JOINTdatalong$year == 1962 | JOINTdatalong$year == 1963 | JOINTdatalong$year == 1964] <- "1960"
JOINTdatalong$migyear[JOINTdatalong$year == 1965 | JOINTdatalong$year == 1966 | JOINTdatalong$year == 1967 | JOINTdatalong$year == 1968 | JOINTdatalong$year == 1969] <- "1965"
JOINTdatalong$migyear[JOINTdatalong$year == 1970 | JOINTdatalong$year == 1971 | JOINTdatalong$year == 1972 | JOINTdatalong$year == 1973 | JOINTdatalong$year == 1974] <- "1970"
JOINTdatalong$migyear[JOINTdatalong$year == 1975 | JOINTdatalong$year == 1976 | JOINTdatalong$year == 1977 | JOINTdatalong$year == 1978 | JOINTdatalong$year == 1979] <- "1975"
JOINTdatalong$migyear[JOINTdatalong$year == 1980 | JOINTdatalong$year == 1981 | JOINTdatalong$year == 1982 | JOINTdatalong$year == 1983 | JOINTdatalong$year == 1984] <- "1980"
JOINTdatalong$migyear[JOINTdatalong$year == 1985 | JOINTdatalong$year == 1986 | JOINTdatalong$year == 1987 | JOINTdatalong$year == 1988 | JOINTdatalong$year == 1989] <- "1985"
JOINTdatalong$migyear[JOINTdatalong$year == 1990 | JOINTdatalong$year == 1991 | JOINTdatalong$year == 1992 | JOINTdatalong$year == 1993 | JOINTdatalong$year == 1994] <- "1990"
JOINTdatalong$migyear[JOINTdatalong$year == 1995 | JOINTdatalong$year == 1996 | JOINTdatalong$year == 1997 | JOINTdatalong$year == 1998 | JOINTdatalong$year == 1999] <- "1995"
JOINTdatalong$migyear[JOINTdatalong$year == 2000 | JOINTdatalong$year == 2001 | JOINTdatalong$year == 2002 | JOINTdatalong$year == 2003 | JOINTdatalong$year == 2004] <- "2000"
JOINTdatalong$migyear[JOINTdatalong$year == 2005 | JOINTdatalong$year == 2006 | JOINTdatalong$year == 2007 | JOINTdatalong$year == 2008 | JOINTdatalong$year == 2009] <- "2005"
JOINTdatalong$migyear[JOINTdatalong$year == 2010 | JOINTdatalong$year == 2011 | JOINTdatalong$year == 2012 | JOINTdatalong$year == 2013 | JOINTdatalong$year == 2014 | JOINTdatalong$year == 2015] <- "2010"
JOINTdatalong$migyear <- as.numeric(JOINTdatalong$migyear)

# Join JOINTdatalong to Abelmigrationdata2
JOINTdatalong <- left_join(JOINTdatalong, Abelmigrationdata2, by = c("Iso3_a" = "orig", "migyear" = "year0"), relationship = "many-to-many")

# Long WBpopulationdata and merge
# Fromm wide to long
WBpopulationdatalong <- pivot_longer(data = WBpopulationdata,                     
                          !c('Country Name', 'Country Code', 'Indicator Name', 'Indicator Code'),                    
                          names_to = "PopYear",                     
                          values_to = "WBTotalPopulation")
WBpopulationdatalong$PopYear <- as.numeric(WBpopulationdatalong$PopYear)

JOINTdatalong <- left_join(JOINTdatalong, WBpopulationdatalong, by = c("Iso3_a" = "Country Code", "year" = "PopYear"), relationship = "many-to-one")

# Turn WBhistclassdata into long format
WBhistclassdatalong <- pivot_longer(data = WBhistclassdata,                     
                                    !c('ISO3 Code', 'Country'),                    
                                    names_to = "ClassYear",                     
                                    values_to = "WBIncomeClass")
WBhistclassdatalong$ClassYear <- as.numeric(WBhistclassdatalong$ClassYear)

JOINTdatalong <- left_join(JOINTdatalong, WBhistclassdatalong, by = c("Iso3_a" = "ISO3 Code", "year" = "ClassYear"), relationship = "many-to-one")

# Clean class variable
JOINTdatalong$WBIncomeClass[JOINTdatalong$WBIncomeClass == ".."] <- NA
JOINTdatalong$WBIncomeClass[JOINTdatalong$WBIncomeClass == "UM"] <- "Upper middle income"
JOINTdatalong$WBIncomeClass[JOINTdatalong$WBIncomeClass == "LM"] <- "Lower middle income"
JOINTdatalong$WBIncomeClass[JOINTdatalong$WBIncomeClass == "H"] <- "High income"
JOINTdatalong$WBIncomeClass[JOINTdatalong$WBIncomeClass == "L"] <- "Low income"
JOINTdatalong$WBIncomeClass <- as.factor(JOINTdatalong$WBIncomeClass)
JOINTdatalong$WBIncomeClass <- factor(JOINTdatalong$WBIncomeClass, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

JOINTdatalong <- group_by(JOINTdatalong, conflict_id)
JOINTdatalong <- JOINTdatalong[order(JOINTdatalong$conflict_id, JOINTdatalong$year),] # Order data to keep observation with recorded ep_end, if present

#
##
### CLEAN AND NEW CENSOR
##
#

## Create clean dataset where time > 0 and year == 1960-2015
DATA1 <- subset(JOINTdatalong, JOINTdatalong$ConflictMonthSpell > 0) # Remove 14 single-day events
DATA1 <- subset(DATA1, DATA1$year >= 1960 & DATA1$year <= 2015) # Remove conflicts without migration data
DATA1 <- subset(DATA1,!is.na(INFORMRiskClass)) # Remove conflicts without IPCC Risk data

## Create new censored variables
DATA1 <- DATA1 %>%
  dplyr::mutate(endyear = lubridate::year(ep_end_date))
DATA1$endyear <- as.numeric(DATA1$endyear)

DATA1$ep_end2 <- NA
DATA1$ep_end2 <- "1"
DATA1$ep_end2[DATA1$ep_end == 0] <- "0"
DATA1$ep_end2[DATA1$ep_end == 1 & DATA1$endyear > 2015] <- "0"
DATA1$ep_end2 <- as.numeric(DATA1$ep_end2)

DATA1$ep_end_date <- as.Date(DATA1$ep_end_date)

DATA1$ep_end_last2 <- NA
DATA1$ep_end_last2[DATA1$ep_end2 == 0] <- ymd("2015-12-31")
DATA1$ep_end_last2[DATA1$ep_end2 == 1] <- ymd(DATA1$ep_end_date[DATA1$ep_end2 == 1])
DATA1$ep_end_last2 <- as.Date(DATA1$ep_end_last2)

DATA1$start_date1 <- NA
DATA1$start_date1[DATA1$start_date >= ymd("1960-01-01")] <- DATA1$start_date[DATA1$start_date >= ymd("1960-01-01")]
DATA1$start_date1[DATA1$start_date < ymd("1960-01-01")] <- ymd("1960-01-01")
DATA1$start_date1 <- as.Date(DATA1$start_date1)

DATA1$ConflictMonthSpell3 <- NA
DATA1$ConflictMonthSpell3 <- interval(start = ymd(DATA1$start_date), end = ymd(DATA1$ep_end_last2))/months(1)
DATA1$ConflictMonthSpell3 <- as.numeric(DATA1$ConflictMonthSpell3)

DATA1$ConflictMonthSpell2 <- NA
DATA1$ConflictMonthSpell2 <- interval(start = ymd(DATA1$start_date1), end = ymd(DATA1$ep_end_last2))/months(1)
DATA1$ConflictMonthSpell2 <- as.numeric(DATA1$ConflictMonthSpell2)

## Create migration variables
# Var: Abelmigpc - migration flow per capita
DATA1$total_flow <- as.numeric(DATA1$total_flow)
DATA1$WBTotalPopulation <- as.numeric(DATA1$WBTotalPopulation)
DATA1$Abelmigpc <- NA
DATA1$Abelmigpc <- DATA1$total_flow/DATA1$WBTotalPopulation
DATA1$Abelmigpc <- as.numeric(DATA1$Abelmigpc)

# Var: Abelmigflow - migration flow categorical
quantile(DATA1$Abelmigpc)
DATA1$Abelmigflow <- NA
DATA1$Abelmigflow[DATA1$Abelmigpc <= 0.01929708] <- "Migration first quantile"
DATA1$Abelmigflow[DATA1$Abelmigpc >= 0.01929708] <- "Migration second quantile"
DATA1$Abelmigflow[DATA1$Abelmigpc > 0.07242524] <- "Migration third quantile"
DATA1$Abelmigflow[DATA1$Abelmigpc >= 0.26653914] <- "Migration fourth quantile"
DATA1$Abelmigflow <- as.factor(DATA1$Abelmigflow)
DATA1$Abelmigflow <- factor(DATA1$Abelmigflow, levels = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"))

## Backfill WBIncomeClass
library(zoo)
library(data.table)
DATA1 <- setDT(DATA1)
DATA1 <- DATA1[, WBIncomeClass := na.locf(WBIncomeClass, fromLast = T, na.rm = FALSE), by = gwno_a]
DATA1 <- DATA1 %>%
  mutate(WBIncomeClass = coalesce(WBIncomeClass, WBIncomeGroup))

## Trimmed data
DATA <- DATA1 %>% select(conflict_id, WBRegion, location, gwno_a, Iso3_a, side_a, side_b, start_date, start_date1, ep_end_last2, dates, to, ConflictMonthSpell2, ConflictMonthSpell3, ep_end2, UCDPConflictIntensity, UCDPGovernmentConflict, WBTotalPopulation, WBIncomeClass, HenselFormationWave, HenselIndViolence, migyear, total_flow, Abelmigpc, Abelmigflow, INFORMRiskClass, INFORMRiskClassGrouped, INFORMRiskGrouped, INFORMNaturalHazard, HenselINTIndYears)

na.omit(DATA) # No NAs to omit means we cleaned well!

## Clean environment
rm(Henselcolonialdata, IPCCindicatordata, IPCCinformdata, JOINTdata, JOINTdataunique, UCDParmeddata, UCDPdata1, UCDPdata2, WBclassdata, Abelmigrationdata, JOINTdatalong, WBpopulationdatalong, WBhistclassdata, WBhistclassdatalong)

# 
##
### Examine variables and make adjustments 
## 
#

## Count censored cases
n_distinct(DATA$conflict_id[DATA$ep_end2 == 0]) # Right-censored conflicts 
n_distinct(DATA$conflict_id[DATA$start_date1 == "1960-01-01"]) # Left-censored conflicts
n_distinct(DATA$conflict_id[DATA$ep_end2 == 1 & DATA$start_date1 != "1960-01-01"]) # Non-censored conflicts

## Count states, conflicts and state-months of conflict
n_distinct(DATA$conflict_id) # Total conflicts
n_distinct(DATA$side_a) # Total states
n_distinct(DATA$location) # Total locations

## Sample statistics
library("table1")

label(DATA$INFORMRiskClass) <- "Composite risk class of humanitarian crises and disasters (INFORMRiskClass)"
label(DATA$INFORMNaturalHazard) <- "Risk of natural disasters, incl. earthquakes, floods, tsunamis, tropical cyclones, droughts & epidemics (INFORMNaturalHazard)"
label(DATA$HenselFormationWave) <- "Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)"
label(DATA$HenselIndViolence) <- "Violence involved in a state's formation (HenselIndViolence)"
label(DATA$UCDPConflictIntensity) <- "Intensity of each episode of conflict (UCDPConflictIntensity)"
label(DATA$UCDPGovernmentConflict) <- "Actor opposing the state in each episode of conflict (UCDPGovernmentConflict)"
label(DATA$WBIncomeClass) <- "Gross national income (GNI) per capita (WBIncomeClass)"
label(DATA$WBRegion) <- "Region (WBRegion)"
label(DATA$Abelmigflow) <- "Migration flow per capita (Abelmigflow)"
label(DATA$gwno_a) <- "State character code (gwno_a)"
label(DATA$conflict_id) <- "Unique conflict identifier (conflict_id)"

TABLE01 <- table1(~ INFORMRiskClass + INFORMNaturalHazard + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + Abelmigflow + gwno_a + conflict_id | INFORMRiskClass, 
                  data = DATA,
                  caption = "Table 1: Sample statistics by composite risk of humanitarian crises and disasters (INFORMRiskClass)"
                  )
TABLE01

library(flextable)
library(officer)
pr <- prop_section(page_size = page_size(orient = "landscape"))
t1flex(TABLE01) %>% 
  save_as_docx(path="Table 1.docx", pr_section = pr)

# 
##
### Kaplan-Meier Survival Functions
## 
#

## Naive SF basic function
library(survival)
SURVIVAL01 <- survfit(Surv(DATA$ConflictMonthSpell2, DATA$ep_end2) ~ 1)

##Register fonts
#install.packages("extrafont")
library(extrafont)
#font_import()
#loadfonts(device = "win") # Try device = "quartz" if you have a Mac
#fonts() # Check available fonts
windowsFonts(
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Corbel")
)

library(survminer)
PLOT01med <- surv_median(SURVIVAL01)

PLOT01 <- plot(SURVIVAL01, col = "#A2291F", 
               main = "Figure 2: Naive survival function of peace",
               family = "C",
               ps = 12,
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               lwd = 2)
library(plotrix)
PLOT01 <- ablineclip(x2 = PLOT01med$median, h = .5, lty = 3, lwd = 1)
PLOT01 <- ablineclip(y2 = 0.5, v = PLOT01med$median, col = "#A2291F", lty = 2, lwd = 1)
PLOT01 <- text(190, 0.0, "175", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT01 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)

jpeg(filename = 'Figure 2_Naive survival function of peace.jpeg', width = 550, height = 350, pointsize = 12, family = "C")

PLOT01 <- plot(SURVIVAL01, col = "#A2291F", 
               main = "Figure 2: Naive survival function of peace",
               family = "C",
               ps = 12,
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               lwd = 2)
PLOT01 <- ablineclip(x2 = PLOT01med$median, h = .5, lty = 3, lwd = 1)
PLOT01 <- ablineclip(y2 = 0.5, v = PLOT01med$median, col = "#A2291F", lty = 2, lwd = 1)
PLOT01 <- text(190, 0.0, "175", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT01 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)

dev.off()
PLOT01

# Return median SURVIVAL01
surv_median(SURVIVAL01) # Median month of peace is 180

## Naive constructed SF by IPCC risk of natural disaster
SURVIVAL02 <- survfit(Surv(DATA$ConflictMonthSpell2, DATA$ep_end2) ~ DATA$INFORMNaturalHazard)

# Return median SURVIVIAL02
PLOT02med <- surv_median(SURVIVAL02) # Naive hump effect

# Comparing survival curves
library(adjustedCurves)
#install.packages("adjustedCurves")
#install.packages("WeighIt")
#install.packages("remotes")
library(remotes)
#remotes::install_github("ngreifer/WeightIt")
#library(WeighIt)

adjsurv02 <- adjustedsurv(data = DATA,
                        variable = "INFORMNaturalHazard",
                        ev_time = "ConflictMonthSpell2",
                        event = "ep_end2",
                        method = "iptw_km",
                        treatment_model = INFORMNaturalHazard ~ ep_end2,
                        conf_int = TRUE,
                        bootstrap = TRUE,
                        n_boot = 100,
                        stabilize = TRUE)

adjusted_surv_quantile(adjsurv02, p=0.5, conf_int=TRUE, contrast="diff")

plot(SURVIVAL02)

PLOT02 <- plot(SURVIVAL02, col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 6: Naive survival function of peace by risk of natural disasters (INFORMNaturalHazard)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT02 <- abline(h = .5, lty = 2, lwd = 1)
PLOT02 <- ablineclip(y2 = 0.5, v = PLOT02med$median, col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT02 <- text(190, 0.0, "175", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT02 <- text(140, 0.0, "157", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT02 <- text(190, 0.05, "171", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT02 <- text(225, 0.0, "241", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT02 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT02 <- legend("topright",
                 legend = c("Low", "Medium", "High", "Very high"), 
                 title = "Risk of natural disasters",
                 col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))

jpeg(filename = 'Figure 6_Naive survival function of peace by risk of natural disasters.jpeg', width = 600, height = 350, family = "C", pointsize = 12, quality = 100)

PLOT02 <- plot(SURVIVAL02, col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 6: Naive survival function of peace by risk of natural disasters (INFORMNaturalHazard)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT02 <- abline(h = .5, lty = 2, lwd = 1)
PLOT02 <- ablineclip(y2 = 0.5, v = PLOT02med$median, col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT02 <- text(190, 0.0, "175", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT02 <- text(140, 0.0, "157", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT02 <- text(190, 0.05, "171", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT02 <- text(230, 0.0, "241", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT02 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT02 <- legend("topright",
                 legend = c("Low", "Medium", "High", "Very high"), 
                 title = "Risk of natural disasters",
                 col = c("#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))

dev.off()
PLOT02

# 
##
### Naive, constructed and fully specified model comparison
## 
#

## Estimate the constructed model
library(survival)
library(eha)
phreg(Surv(DATA$ConflictMonthSpell2, DATA$ep_end2) ~ DATA$INFORMNaturalHazard, dist="weibull", shape=1)

# Naive constructed cox model
MODEL01 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMNaturalHazard, data = DATA)

# Naive constructed hazard ratio
library(gtsummary)
OUTPUT01 <- tbl_regression(MODEL01, exponentiate = TRUE)
OUTPUT01

# Hazard ratio 
DATA$INFORMRiskClass <- factor(DATA$INFORMRiskClass, levels = c("Low", "Very Low", "Medium", "High", "Very High"))
label(DATA$INFORMRiskClass) <- "Composite risk of humanitarian crises and disasters (INFORMRiskClass)"

MODEL02 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskClass, data = DATA)
OUTPUT02 <- tbl_regression(MODEL02, exponentiate = TRUE)
OUTPUT02

SURVIVAL03 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskClass, data = DATA)

SURVIVAL03DATA <- filter(DATA[INFORMRiskClass == "Very Low" | INFORMRiskClass == "Very High"])
adjsurv03 <- adjustedsurv(data = DATA,
                          variable = "INFORMRiskClass",
                          ev_time = "ConflictMonthSpell2",
                          event = "ep_end2",
                          method = "iptw_km",
                          treatment_model = INFORMRiskClass ~ ep_end2,
                          conf_int = TRUE,
                          bootstrap = TRUE,
                          n_boot = 100,
                          stabilize = TRUE)

adjusted_surv_quantile(adjsurv03, p=0.5, conf_int=TRUE, contrast="diff")

PLOT03med <- surv_median(SURVIVAL03)

PLOT03 <- plot(SURVIVAL03, col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 7: Naive survival function of peace by composite risk of humanitarian crises and disasters (INFORMRiskClass)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT03 <- abline(h = .5, lty = 2, lwd = 1)
PLOT03 <- ablineclip(y2 = 0.5, v = PLOT03med$median, col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT03 <- text(179, 0.0, "171", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT03 <- text(133, 0.0, "122", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT03 <- text(196, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT03 <- text(155, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT03 <- text(425, 0.0, "441", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT03 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT03 <- legend("topright",
                 legend = c("Very low", "Low", "Medium", "High", "Very high"), 
                 title = "Composite risk of humanitarian crises and disasters",
                 col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))

jpeg(filename = 'Figure 7_Naive survival function of peace by composite risk of humanitarian crises and disasters.jpeg', width = 750, height = 350, pointsize = 12, family = "C", quality = 100)

PLOT03 <- plot(SURVIVAL03, col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 7: Naive survival function of peace by composite risk of humanitarian crises and disasters (INFORMRiskClass)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT03 <- abline(h = .5, lty = 2, lwd = 1)
PLOT03 <- ablineclip(y2 = 0.5, v = PLOT03med$median, col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT03 <- text(179, 0.0, "171", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT03 <- text(133, 0.0, "122", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT03 <- text(196, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT03 <- text(155, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT03 <- text(425, 0.0, "441", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT03 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT03 <- legend("topright",
                 legend = c("Very low", "Low", "Medium", "High", "Very high"), 
                 title = "Composite risk of humanitarian crises and disasters",
                 col = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))
dev.off()
PLOT03


## Estimate the fully-specified model
# Hazard ratio - fully specified
MODEL03 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskClass + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + Abelmigflow + gwno_a + conflict_id, data = DATA)
OUTPUT03 <- tbl_regression(MODEL03, exponentiate = TRUE)
OUTPUT03

## Compare Cox models
library(stargazer)
SUMMARY01 <- stargazer(MODEL01, MODEL02, MODEL03, type = "text", out = "table1.htm", apply.coef = exp, t.auto = F,
          p.auto = F)

## Merge outputs
SUMMARY02 <- tbl_merge(  
  tbls = list(OUTPUT01, OUTPUT02, OUTPUT03), 
  tab_spanner = c("Naive model", "Controlled model", "Fully specified model"))

footnote1 <- "Naive model (MODEL01): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMNaturalHazard, data = DATA)"
footnote2 <- "Controlled model (MODEL02): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskClass, data = DATA)"
footnote3 <- "Fully specified model (MODEL03): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskClass + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + Abelmigflow + gwno_a + conflict_id, data = DATA)"

library(gt)
SUMMARY02 <- as_gt(SUMMARY02) %>%
  gt::tab_header(
    title = md("Table 2: Rate of peace by model")
  ) %>%
  tab_footnote(footnote1,
               locations = cells_column_spanners("Naive model")
  ) %>%
  tab_footnote(footnote2,
               locations = cells_column_spanners("Controlled model"),
  ) %>%
  tab_footnote(footnote3,
               locations = cells_column_spanners("Fully specified model"),
  )

gt::tab_options(SUMMARY02, table.font.names = "Corbel", table.font.size = 12, heading.title.font.weight = "bold")


gt::gtsave(SUMMARY02, file = file.path("Table 2.docx"))

# 
##
### Models by World Bank Development Indicators Region
## 
#

## Estimate the regional model
DATA$WBRegion <- factor(DATA$WBRegion, levels = c("Sub-Saharan Africa", "Middle East & North Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "North America + Latin America & Caribbean"))

SURVIVAL18 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ WBRegion, data = DATA)
PLOT18med <- surv_median(SURVIVAL18)


DATA$WBRegion <- factor(DATA$WBRegion, levels = c("Middle East & North Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "North America + Latin America & Caribbean", "Sub-Saharan Africa"))
label(DATA$WBRegion) <- "Region (WBRegion)"

adjsurv18 <- adjustedsurv(data = DATA,
                          variable = "WBRegion",
                          ev_time = "ConflictMonthSpell2",
                          event = "ep_end2",
                          method = "iptw_km",
                          treatment_model = WBRegion ~ ep_end2,
                          conf_int = TRUE,
                          bootstrap = TRUE,
                          n_boot = 100,
                          stabilize = TRUE)

adjusted_surv_quantile(adjsurv18, p=0.5, conf_int=TRUE, contrast="diff")


PLOT05 <- plot(SURVIVAL18, col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"),
               main = "Figure 9: Survival function of peace by region (WBRegion)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT05 <- abline(h = .5, lty = 2, lwd = 1)
PLOT05 <- ablineclip(y2 = 0.5, v = PLOT18med$median, col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"), lty = 2, lwd = 1)
PLOT05 <- text(46, 0.0, "55", col = "#7030A0", cex = 1, family = "C", ps = 12)
PLOT05 <- text(117, 0.0, "129", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT05 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT05 <- text(147, 0.0, "157", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT05 <- text(195, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT05 <- text(215, 0.0, "205", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT05 <- text(15, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT05 <- legend("topright",
                 legend = c("Sub-Saharan Africa", "Middle East & North Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "North America + Latin America & Caribbean"), 
                 title = "Region",
                 col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"), 
                 lwd = 2,
                 par(family = "C"))

jpeg(filename = 'Figure 9_Survival function of peace by region.jpeg', width = 650, height = 400, quality = 100, pointsize = 12, family = "C")

PLOT05 <- plot(SURVIVAL18, col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"),
               main = "Figure 9: Survival function of peace by region (WBRegion)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT05 <- abline(h = .5, lty = 2, lwd = 1)
PLOT05 <- ablineclip(y2 = 0.5, v = PLOT18med$median, col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"), lty = 2, lwd = 1)
PLOT05 <- text(46, 0.0, "55", col = "#7030A0", cex = 1, family = "C", ps = 12)
PLOT05 <- text(117, 0.0, "129", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT05 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT05 <- text(147, 0.0, "157", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT05 <- text(195, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT05 <- text(215, 0.0, "205", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT05 <- text(15, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT05 <- legend("topright",
                 legend = c("Sub-Saharan Africa", "Middle East & North Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "North America + Latin America & Caribbean"), 
                 title = "Region",
                 col = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"), 
                 lwd = 2,
                 par(family = "C"))
dev.off()
PLOT05

TABLE15 <- table1(~ INFORMRiskClass + INFORMRiskClassGrouped + INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + Abelmigflow + gwno_a + conflict_id | WBRegion, data = DATA)
TABLE15

DATA$INFORMRiskClass <- relevel(DATA$INFORMRiskClass, ref = "Medium")
DATA$HenselFormationWave <- relevel(DATA$HenselFormationWave, ref = "Pt. 2 Anticolonial revolution (1955-1984)") # Releveled to match regions
DATA$HenselIndViolence <- relevel(DATA$HenselIndViolence, ref = "Violent independence")
DATA$WBIncomeClass <- relevel(DATA$WBIncomeClass, ref = "Lower middle income")
label(DATA$INFORMRiskClass) <- "Composite risk of humanitarian crises and disasters (INFORMRiskClass)"
label(DATA$HenselFormationWave) <- "Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)"
label(DATA$HenselIndViolence) <- "Violence involved in a state's formation (HenselIndViolence)"
label(DATA$WBIncomeClass) <- "Gross national income (GNI) per capita (WBIncomeClass)"

DATA$INFORMRiskGrouped <- relevel(DATA$INFORMRiskGrouped, ref = "High")
label(DATA$INFORMRiskGrouped) <- "Grouped composite risk of humanitarian crises and disasters (INFORMRiskGrouped)"

# East Asia & Pacific
DataEAsia <- subset(DATA, DATA$WBRegion == "East Asia & Pacific")
TABLE08 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id | INFORMRiskGrouped, data = DataEAsia)
TABLE08

DataEAsia$HenselFormationWave <- relevel(DataEAsia$HenselFormationWave, ref = "Pt. 1 Anticolonial revolution (1945-1954)") # Releveled to match regions
label(DataEAsia$HenselFormationWave) <- "Period of state formation (Formation, decolonization, secession or partition)"

MODEL10 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataEAsia)
OUTPUT10 <- tbl_regression(MODEL10, exponentiate = TRUE) 
OUTPUT10

# Europe & Central Asia
DataEurope <- subset(DATA, DATA$WBRegion == "Europe & Central Asia")
TABLE09 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id | INFORMRiskGrouped, data = DataEurope)
TABLE09

DataEurope$INFORMRiskGrouped <- relevel(DataEurope$INFORMRiskGrouped, ref = "Low")
label(DATA$INFORMRiskGrouped) <- "Grouped composite risk of humanitarian crises and disasters (INFORMRiskGrouped)"

DataEurope$HenselFormationWave <- relevel(DataEurope$HenselFormationWave, ref = "Neo-Europes (1839-1913)") # Releveled to match regions
label(DataEurope$HenselFormationWave) <- "Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)"

MODEL11 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataEurope)
OUTPUT11 <- tbl_regression(MODEL11, exponentiate = TRUE) 
OUTPUT11

# Middle East & North Africa
DataME <- subset(DATA, DATA$WBRegion == "Middle East & North Africa")
TABLE11 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id | INFORMRiskGrouped, data = DataME)
TABLE11

DataME$INFORMRiskGrouped <- relevel(DataME$INFORMRiskGrouped, ref = "Low")
label(DATA$INFORMRiskGrouped) <- "Grouped composite risk of humanitarian crises and disasters (INFORMRiskGrouped)"

DataME$HenselFormationWave <- relevel(DataME$HenselFormationWave, ref = "Pt. 1 Anticolonial revolution (1945-1954)") # Releveled to match regions
label(DataME$HenselFormationWave) <- "Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)"

DataME$UCDPGovernmentConflict <- relevel(DataME$UCDPGovernmentConflict, ref = "Government conflict") # Releveled to match regions
label(DataME$UCDPGovernmentConflict) <- "Actor opposing the state in each episode of conflict (UCDPGovernmentConflict))"

MODEL13 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataME)
OUTPUT13 <- tbl_regression(MODEL13, exponentiate = TRUE) 
OUTPUT13

# South Asia
DataSAsia <- subset(DATA, DATA$WBRegion == "South Asia")
TABLE13 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id | INFORMRiskGrouped, data = DataSAsia)
TABLE13

DataSAsia$INFORMRiskGrouped <- relevel(DataSAsia$INFORMRiskGrouped, ref = "Low")
label(DATA$INFORMRiskGrouped) <- "Grouped composite risk of humanitarian crises and disasters (INFORMRiskGrouped)"

DataSAsia$HenselFormationWave <- relevel(DataSAsia$HenselFormationWave, ref = "World Wars (1914-1944)")
label(DataSAsia$HenselFormationWave) <- "Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)"

DataSAsia$UCDPGovernmentConflict <- relevel(DataSAsia$UCDPGovernmentConflict, ref = "Government conflict") # Releveled to match regions
label(DataSAsia$UCDPGovernmentConflict) <- "Actor opposing the state in each episode of conflict (UCDPGovernmentConflict)"

MODEL15 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataSAsia)
OUTPUT15 <- tbl_regression(MODEL15, exponentiate = TRUE) 
OUTPUT15

# Sub-Saharan Africa
DataSSA <- subset(DATA, DATA$WBRegion == "Sub-Saharan Africa")
TABLE14 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id | INFORMRiskGrouped, data = DataSSA)
TABLE14

MODEL16 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataSSA)
OUTPUT16 <- tbl_regression(MODEL16, exponentiate = TRUE) 
OUTPUT16

## Compare Cox models
library(stargazer)
SUMMARY07 <- stargazer(MODEL10, MODEL11, MODEL13, MODEL15, MODEL16, type = "text", out="table1.htm", apply.coef=exp, t.auto = F, #Removed Model14
                       p.auto=F)
## Merge outputs
SUMMARY08 <- tbl_merge(  
  tbls = list(OUTPUT10, OUTPUT11, OUTPUT13, OUTPUT15, OUTPUT16),
  tab_spanner = c("East Asia & Pacific", "Europe & Central Asia", "Middle East & North Africa", "South Asia", "Sub-Saharan Africa")) #Removed "North America + Latin America & Caribbean"

footnote11 <- "East Asia & Pacific (MODEL10): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataEAsia)"
footnote12 <- "Europe & Central Asia (MODEL11): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataEurope)"
footnote13 <- "Middle East & North Africa (MODEL13): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataME)"
footnote15 <- "South Asia (MODEL15): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataSAsia)"
footnote16 <- "Sub-Saharan Africa (MODEL16): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + Abelmigflow + gwno_a + conflict_id, data = DataSSA)"

SUMMARY08 <- as_gt(SUMMARY08) %>%
  gt::tab_header(
    title = md("Table 3: Rate of peace by region")
  ) %>%
  tab_footnote(footnote11,
               locations = cells_column_spanners("East Asia & Pacific")
  ) %>%
  tab_footnote(footnote12,
               locations = cells_column_spanners("Europe & Central Asia"),
  ) %>%
  tab_footnote(footnote13,
               locations = cells_column_spanners("Middle East & North Africa")
  ) %>%
  tab_footnote(footnote15,
               locations = cells_column_spanners("South Asia")
  ) %>%
  tab_footnote(footnote16,
               locations = cells_column_spanners("Sub-Saharan Africa")
  )

gt::tab_options(SUMMARY08, table.font.names = "Corbel", table.font.size = 12, heading.title.font.weight = "bold") %>%
  gtsave(file = file.path("Table 3.docx"))

SUMMARY08

# 
##
### Models by Migration Level
## 
#

SURVIVAL04 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ Abelmigflow, data = DATA)

PLOT04med <- surv_median(SURVIVAL04)

DATA$Abelmigflow <- factor(DATA$Abelmigflow, levels = c("Migration second quantile", "Migration first quantile", "Migration third quantile", "Migration fourth quantile"))
label(DATA$Abelmigflow) <- "Migration flow per capita (Abelmigflow)"

adjsurv04 <- adjustedsurv(data = DATA,
                          variable = "Abelmigflow",
                          ev_time = "ConflictMonthSpell2",
                          event = "ep_end2",
                          method = "iptw_km",
                          treatment_model = Abelmigflow ~ ep_end2,
                          conf_int = TRUE,
                          bootstrap = TRUE,
                          n_boot = 100,
                          stabilize = TRUE)

adjusted_surv_quantile(adjsurv04, p=0.5, conf_int=TRUE, contrast="diff")

DATA$Abelmigflow <- factor(DATA$Abelmigflow, levels = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"))
label(DATA$Abelmigflow) <- "Migration flow per capita (Abelmigflow)"

PLOT04 <- plot(SURVIVAL04, col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), 
               main = "Figure 11: Survival function of peace by migration flow (Abelmigflow)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT04 <- abline(h = .5, lty = 2, lwd = 1)
PLOT04 <- ablineclip(y2 = 0.5, v = PLOT04med$median, col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT04 <- text(143, 0.0, "152", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT04 <- text(195, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT04 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT04 <- text(213, 0.0, "205", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT04 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT04 <- legend("topright",
                 legend = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"),
                 title = "Migration flow",
                 col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))

jpeg(filename = 'Figure 11_Survival function of peace by migration flow.jpeg', width = 680, height = 350, family = "C", pointsize = 12, quality = 100) 

PLOT04 <- plot(SURVIVAL04, col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), 
               main = "Figure 11: Survival function of peace by migration flow (Abelmigflow)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT04 <- abline(h = .5, lty = 2, lwd = 1)
PLOT04 <- ablineclip(y2 = 0.5, v = PLOT04med$median, col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT04 <- text(143, 0.0, "152", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT04 <- text(195, 0.0, "187", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT04 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT04 <- text(213, 0.0, "205", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT04 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT04 <- legend("topright",
                 legend = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"),
                 title = "Migration flow",
                 col = c("#234756", "#88BACE", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))
dev.off()

PLOT04

# Migration first quantile
MIGDATA25 <- subset(DATA, DATA$Abelmigflow == "Migration first quantile")

TABLE02 <- table1(~ INFORMRiskClass + INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + Abelmigflow + gwno_a + conflict_id | WBRegion, data = MIGDATA25)
TABLE02

MODEL04 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA25)
OUTPUT04 <- tbl_regression(MODEL04, exponentiate = TRUE) 
OUTPUT04 

# Migration second quantile
MIGDATA50 <- subset(DATA, DATA$Abelmigflow == "Migration second quantile")

TABLE03 <- table1(~ INFORMRiskClass + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion | WBRegion, data = MIGDATA50)
TABLE03

MODEL05 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA50)
OUTPUT05 <- tbl_regression(MODEL05, exponentiate = TRUE) 
OUTPUT05

# Migration third quantile
MIGDATA51 <- subset(DATA, DATA$Abelmigflow == "Migration third quantile")

TABLE031 <- table1(~ INFORMRiskClass + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion | WBRegion, data = MIGDATA51)
TABLE031

MODEL051 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA51)
OUTPUT051 <- tbl_regression(MODEL051, exponentiate = TRUE) 
OUTPUT051

# Migration fourth quantile
MIGDATA75 <- subset(DATA, DATA$Abelmigflow == "Migration fourth quantile")

TABLE04 <- table1(~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion | WBRegion, data = MIGDATA75)
TABLE04

MODEL06 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA75)
OUTPUT06 <- tbl_regression(MODEL06, exponentiate = TRUE) 
OUTPUT06

## Compare Cox models
library(stargazer)
SUMMARY03 <- stargazer(MODEL04, MODEL05, MODEL051, MODEL06, type = "text", out="table1.htm", apply.coef=exp, t.auto = F,
          p.auto=F)

## Merge outputs
SUMMARY04 <- tbl_merge(  
  tbls = list(OUTPUT04, OUTPUT05, OUTPUT051, OUTPUT06),
  tab_spanner = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"))

footnote5 <- "Migration flow first quantile (MODEL04): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped  + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA25)"
footnote6 <- "Migration flow second quantile (MODEL05): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped  + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA50)"
footnote7 <- "Migration flow third quantile (MODEL051): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped  + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA75)"
footnote71 <- "Migration flow fourth quantile (MODEL06): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ INFORMRiskGrouped  + HenselFormationWave + HenselIndViolence + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id, data = MIGDATA50)"

SUMMARY04 <- as_gt(SUMMARY04) %>%
  gt::tab_header(
    title = md("Table 4: Rate of peace by migration flow")
  ) %>%
  tab_footnote(footnote5,
               locations = cells_column_spanners("Migration first quantile")
  ) %>%
  tab_footnote(footnote6,
               locations = cells_column_spanners("Migration second quantile"),
  ) %>%
  tab_footnote(footnote7,
               locations = cells_column_spanners("Migration third quantile")
  )%>%
  tab_footnote(footnote71,
               locations = cells_column_spanners("Migration fourth quantile")
  )

SUMMARY04

gt::tab_options(SUMMARY04, table.font.names = "Corbel", table.font.size = 12, heading.title.font.weight = "bold")

gt::gtsave(SUMMARY04, file = file.path("Table 4.docx"))

# 
##
### Models by state formation wave and independence
## 
#

unique(DATA$HenselFormationWave)

DATA$HenselFormationWave <- factor(DATA$HenselFormationWave, levels = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"))

SURVIVAL05 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ HenselFormationWave, data = DATA)
PLOT05med <- surv_median(SURVIVAL05)

DATA$HenselFormationWave <- factor(DATA$HenselFormationWave, levels = c("Neo-Europes (1839-1913)", "Atlantic revolutions and prior (<1839)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"))

adjsurv05 <- adjustedsurv(data = DATA,
                          variable = "HenselFormationWave",
                          ev_time = "ConflictMonthSpell2",
                          event = "ep_end2",
                          method = "iptw_km",
                          treatment_model = HenselFormationWave ~ ep_end2,
                          conf_int = TRUE,
                          bootstrap = TRUE,
                          n_boot = 100,
                          stabilize = TRUE)

adjusted_surv_quantile(adjsurv05, p=0.5, conf_int=TRUE, contrast="diff")

DATA$HenselFormationWave <- factor(DATA$HenselFormationWave, levels = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"))
label(DATA$Abelmigflow) <- "Wave of state formation (HenselFormationWave)"

PLOT05 <- plot(SURVIVAL05, col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 13: Survival function of peace by state formation wave (HenselFormationWave)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT05 <- abline(h = .5, lty = 2, lwd = 1)
PLOT05 <- ablineclip(y2 = 0.5, v = PLOT05med$median, col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT05 <- text(50, 0.0, "41", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT05 <- text(135, 0.0, "125", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT05 <- text(164, 0.0, "175", col = "#7030A0", cex = 1, family = "C", ps = 12)
PLOT05 <- text(228, 0.0, "239", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT05 <- text(262, 0.0, "272", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT05 <- text(111, 0.0, "122", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT05 <- text(15, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT05 <- legend("topright",
                 legend = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"),
                 title = "State formation wave",
                 col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))

jpeg(filename = 'Figure 13_Survival function of peace by state formation wave.jpeg', width = 680, height = 350, family = "C", pointsize = 12, quality = 100) 

PLOT05 <- plot(SURVIVAL05, col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
               main = "Figure 13: Survival function of peace by state formation wave (HenselFormationWave)",
               xlab = "Length of conflict in months",
               ylab = "Survival probability",
               family = "C",
               ps = 12,
               lwd = 2)
PLOT05 <- abline(h = .5, lty = 2, lwd = 1)
PLOT05 <- ablineclip(y2 = 0.5, v = PLOT05med$median, col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), lty = 2, lwd = 1)
PLOT05 <- text(50, 0.0, "41", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT05 <- text(135, 0.0, "125", col = "#DD933B", cex = 1, family = "C", ps = 12)
PLOT05 <- text(164, 0.0, "175", col = "#7030A0", cex = 1, family = "C", ps = 12)
PLOT05 <- text(228, 0.0, "239", col = "#388600", cex = 1, family = "C", ps = 12)
PLOT05 <- text(262, 0.0, "272", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT05 <- text(111, 0.0, "122", col = "#88BACE", cex = 1, family = "C", ps = 12)
PLOT05 <- text(15, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT05 <- legend("topright",
                 legend = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"),
                 title = "State formation wave",
                 col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"), 
                 lwd = 2,
                 par(family = "C"))
dev.off()

PLOT05

# Violence vs. nonviolence
SURVIVAL051 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ HenselIndViolence, data = DATA)

PLOT051med <- surv_median(SURVIVAL051)

DATA$HenselIndViolence <- factor(DATA$HenseldIndViolence, levels = c("Violent independence", "Nonviolent independence"))

adjsurv051 <- adjustedsurv(data = DATA,
                          variable = "HenselIndViolence",
                          ev_time = "ConflictMonthSpell2",
                          event = "ep_end2",
                          method = "iptw_km",
                          treatment_model = HenselIndViolence ~ ep_end2,
                          conf_int = TRUE,
                          bootstrap = TRUE,
                          n_boot = 100,
                          stabilize = TRUE)

adjusted_surv_quantile(adjsurv051, p=0.5, conf_int=TRUE, contrast="diff")

DATA$HenselIndViolence <- factor(DATA$HenselIndViolence, levels = c("Nonviolent independence", "Violent independence"))


PLOT051 <- plot(SURVIVAL051, col = c("#234756", "#A2291F"), 
                main = "Figure 15: Survival function of peace by state formation violence (HenselIndViolence)",
                xlab = "Length of conflict in months",
                ylab = "Survival probability",
                family = "C",
                ps = 12,
                lwd = 2)
PLOT051 <- abline(h = .5, lty = 2, lwd = 1)
PLOT051 <- ablineclip(y2 = 0.5, v = PLOT051med$median, col = c("#234756", "#A2291F"), lty = 2, lwd = 1)
PLOT051 <- text(231, 0.0, "221", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT051 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT051 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT051 <- legend("topright",
                  legend = c("Nonviolent", "Violent"),
                  title = "State formation violence",
                  col = c("#234756", "#A2291F"), 
                  lwd = 2,
                  par(family = "C"))

# Plot
jpeg(filename = 'Figure 15_Survival function of peace by state formation violence.jpeg', width = 680, height = 350, family = "C", pointsize = 12, quality = 100) 

PLOT051 <- plot(SURVIVAL051, col = c("#234756", "#A2291F"), 
                main = "Figure 15: Survival function of peace by state formation violence (HenselIndViolence)",
                xlab = "Length of conflict in months",
                ylab = "Survival probability",
                family = "C",
                ps = 12,
                lwd = 2)
PLOT051 <- abline(h = .5, lty = 2, lwd = 1)
PLOT051 <- ablineclip(y2 = 0.5, v = PLOT051med$median, col = c("#234756", "#A2291F"), lty = 2, lwd = 1)
PLOT051 <- text(231, 0.0, "221", col = "#A2291F", cex = 1, family = "C", ps = 12)
PLOT051 <- text(175, 0.0, "165", col = "#234756", cex = 1, family = "C", ps = 12)
PLOT051 <- text(100, 0.0, "Median:", col = "black", cex = 1, family = "C", ps = 12)
PLOT051 <- legend("topright",
                  legend = c("Nonviolent", "Violent"),
                  title = "State formation violence",
                  col = c("#234756", "#A2291F"), 
                  lwd = 2,
                  par(family = "C"))

dev.off()

# By state formation wave and violence
SURVIVAL052 <- survfit(Surv(ConflictMonthSpell2, ep_end2) ~ HenselFormationWave + HenselIndViolence, data = DATA)

PLOT052med <- surv_median(SURVIVAL052)

PLOT052 <- plot(SURVIVAL052[strata = c("HenselFormationWave=Atlantic revolutions and prior (<1839), HenselIndViolence=Nonviolent independence", 
                                       "HenselFormationWave=Neo-Europes (1839-1913), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=World Wars (1914-1944), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Pt. 1 Anticolonial revolution (1945-1954), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Pt. 2 Anticolonial revolution (1955-1984), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Post-Soviet (+1985), HenselIndViolence=Nonviolent independence")], 
                col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
                main = "Figure 16: Survival function of peace by state formation wave (HenselFormationWave) and violence (HenselIndViolence)",
                xlab = "Length of conflict in months",
                ylab = "Survival probability",
                xlim = c(0, 875),
                family = "C",
                ps = 12,
                lwd = 2,
                lty = 1)
PLOT052 <- lines(SURVIVAL052[strata = c("HenselFormationWave=Atlantic revolutions and prior (<1839), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Neo-Europes (1839-1913), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=World Wars (1914-1944), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Pt. 1 Anticolonial revolution (1945-1954), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Pt. 2 Anticolonial revolution (1955-1984), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Post-Soviet (+1985), HenselIndViolence=Violent independence   ")], 
                 col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
                 family = "C",
                 ps = 12,
                 lwd = 2,
                 lty = 5)
PLOT052 <- abline(h = .5, lty = 2, lwd = 1)
PLOT052 <- legend("topright",
                  legend = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)", "", "           State formation violence", "Nonviolent formation", "Violent formation"),
                  title = "State formation wave",
                  col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F", "", "#000000", "#000000"), 
                  lwd = 2,
                  lty = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 2),
                  par(family = "C"))

# Save
jpeg(filename = 'Figure 16_Survival function of peace by state formation wave and violence.jpeg', width = 800, height = 350, family = "C", pointsize = 12, quality = 100) 

PLOT052 <- plot(SURVIVAL052[strata = c("HenselFormationWave=Atlantic revolutions and prior (<1839), HenselIndViolence=Nonviolent independence", 
                                       "HenselFormationWave=Neo-Europes (1839-1913), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=World Wars (1914-1944), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Pt. 1 Anticolonial revolution (1945-1954), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Pt. 2 Anticolonial revolution (1955-1984), HenselIndViolence=Nonviolent independence",
                                       "HenselFormationWave=Post-Soviet (+1985), HenselIndViolence=Nonviolent independence")], 
                col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
                main = "Figure 16: Survival function of peace by state formation wave (HenselFormationWave) and violence (HenselIndViolence)",
                xlab = "Length of conflict in months",
                ylab = "Survival probability",
                xlim = c(0, 875),
                family = "C",
                ps = 12,
                lwd = 2,
                lty = 1)
PLOT052 <- lines(SURVIVAL052[strata = c("HenselFormationWave=Atlantic revolutions and prior (<1839), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Neo-Europes (1839-1913), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=World Wars (1914-1944), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Pt. 1 Anticolonial revolution (1945-1954), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Pt. 2 Anticolonial revolution (1955-1984), HenselIndViolence=Violent independence   ",
                                        "HenselFormationWave=Post-Soviet (+1985), HenselIndViolence=Violent independence   ")], 
                 col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
                 family = "C",
                 ps = 12,
                 lwd = 2,
                 lty = 5)
PLOT052 <- abline(h = .5, lty = 2, lwd = 1)
PLOT052 <- legend("topright",
                  legend = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)", "", "           State formation violence", "Nonviolent formation", "Violent formation"),
                  title = "State formation wave",
                  col = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F", "", "#000000", "#000000"), 
                  lwd = 2,
                  lty = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 2),
                  par(family = "C"))
dev.off()

DATA$HenselFormationWave <- factor(DATA$HenselFormationWave, levels = c("Post-Soviet (+1985)", "Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)"))

MODEL052 <- coxph(Surv(ConflictMonthSpell2, ep_end2) ~ HenselFormationWave + HenselIndViolence + INFORMRiskGrouped + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id + HenselINTIndYears, data = DATA)
OUTPUT052 <- tbl_regression(MODEL052, exponentiate = TRUE) 
OUTPUT052

footnote052 <- "State formation wave and violence model (MODEL052): coxph(Surv(ConflictMonthSpell2, ep_end2) ~ HenselFormationWave + HenselIndViolence + INFORMRiskGrouped + UCDPConflictIntensity + UCDPGovernmentConflict + WBIncomeClass + WBRegion + gwno_a + conflict_id + HenselINTIndYears, data = DATA)"

SUMMARY052 <- as_gt(OUTPUT052) %>%
  gt::tab_header(
    title = md("Table 5: Rate of peace by state formation wave and violence")
  )

gt::tab_options(SUMMARY052, table.font.names = "Corbel", table.font.size = 12, heading.title.font.weight = "bold")

gt::gtsave(SUMMARY052, file = file.path("Table 5.docx"))

SUMMARY052

#
##
### Map data
##
#

library(sf)
library(tmap)
library(mapview)
#install.packages("webshot")
library(webshot)
#webshot::install_phantomjs()
library(spData) 

data(world)

DATA$Iso3_a <- as.character(DATA$Iso3_a) 
DATA$Iso2_a <- NA
DATA$Iso2_a <- countrycode(DATA$Iso3_a, "iso3c", "iso2c")

GEODATA <- left_join(world, DATA, by = c("iso_a2" = "Iso2_a"), relationship = "one-to-many")

## Number of conflicts
GEODATA2 <- GEODATA %>% 
  group_by(Iso3_a) %>% 
  mutate(GlobalConflictMonths = sum(unique(ConflictMonthSpell2)))

ConflictMap <- tm_shape(GEODATA2) +
  tm_polygons("GlobalConflictMonths", 
              title = "Months in conflict",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette.swatch = "#A2291F",
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 1: Total months of conflict across episodes of conflict by state, 1960-2015 (ConflictMonthSpell2)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(ConflictMap, filename = "1ConflictMap.png")

## Risk map
DisasterMap <- tm_shape(GEODATA) +
  tm_polygons("INFORMNaturalHazard", 
              title = "Risk of natural disasters",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#88BACE", "#388600", "#DD933B", "#A2291F"),
              textNA = "No recorded conflicts",
              alpha = 1)+  
  tm_layout(panel.labels = "Figure 3: Map of states by risk of natural disasters (INFORMNaturalHazard)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(DisasterMap, filename = "3DisasterMap.png")

## Risk map

GEODATA$INFORMRiskClass <- relevel(GEODATA$INFORMRiskClass, ref = "Low") # Releveled 
GEODATA$INFORMRiskClass <- relevel(GEODATA$INFORMRiskClass, ref = "Very Low") # Releveled 
label(GEODATA$INFORMRiskClass) <- "Composite risk of humanitarian crises and disasters"

RiskMap <- tm_shape(GEODATA) +
  tm_polygons("INFORMRiskClass", 
              title = "INFORM Risk Class",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 4: Map of states by composite risk of humanitarian crises and disasters (INFORMRiskClass)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(RiskMap, filename = "4RiskMap.png")

## Grouped risk map 

GEODATA$INFORMRiskGrouped <- relevel(GEODATA$INFORMRiskGrouped, ref = "Medium") # Releveled 
GEODATA$INFORMRiskGrouped <- relevel(GEODATA$INFORMRiskGrouped, ref = "Low") # Releveled 
label(GEODATA$INFORMRiskGrouped) <- "Composite risk of humanitarian crises and disasters (INFORMRiskGrouped)"

GroupedRiskMap <- tm_shape(GEODATA) +
  tm_polygons("INFORMRiskGrouped", 
              title = "Grouped INFORM Risk Class",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#88BACE", "#388600", "#DD933B"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 5: Map of states by grouped composite risk of humanitarian crises and disasters (INFORMRiskGrouped)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(GroupedRiskMap, filename = "5GroupedRiskMap.png")

## Region map
GEODATA$WBRegion <- factor(GEODATA$WBRegion, levels = c("Sub-Saharan Africa", "Middle East & North Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "North America + Latin America & Caribbean"))

RegionMap <- tm_shape(GEODATA) +
  tm_polygons("WBRegion", 
              title = "Region",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#A2291F", "#88BACE", "#388600", "#DD933B", "#234756", "#7030A0"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 8: Map of states by region (WBRegion)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(RegionMap, filename = "8RegionMap.png")


## Migration map
Abelmigrationdata2$orig <- as.character(Abelmigrationdata2$orig) 
Abelmigrationdata2$Iso2 <- NA
Abelmigrationdata2$Iso2 <- countrycode(Abelmigrationdata2$orig, "iso3c", "iso2c",
                                       custom_match = c("ANT" = "AN",
                                                        "CHI" = "CL",
                                                        "SUD" = "SD",
                                                        "SCG" = "CS"))

MIGGEODATA <- left_join(world, Abelmigrationdata2, by = c("iso_a2" = "Iso2"), relationship = "one-to-many")

# Create migration variables
MIGGEODATA <- left_join(MIGGEODATA, WBpopulationdatalong, by = c("orig" = "Country Code", "year0" = "PopYear"), relationship = "many-to-one")

# Var: Abelmigpc - migration flow per capita
MIGGEODATA$total_flow <- as.numeric(MIGGEODATA$total_flow)
MIGGEODATA$WBTotalPopulation <- as.numeric(MIGGEODATA$WBTotalPopulation)
MIGGEODATA$Abelmigpc <- NA
MIGGEODATA$Abelmigpc <- MIGGEODATA$total_flow/MIGGEODATA$WBTotalPopulation
MIGGEODATA$Abelmigpc <- as.numeric(MIGGEODATA$Abelmigpc)

# Var: Abelmigflow - migration flow categorical
MIGGEODATA$Abelmigflow2 <- NA
MIGGEODATA$Abelmigflow2[MIGGEODATA$Abelmigpc <= 0.01929708] <- "Migration first quantile"
MIGGEODATA$Abelmigflow2[MIGGEODATA$Abelmigpc >= 0.01929708] <- "Migration second quantile"
MIGGEODATA$Abelmigflow2[MIGGEODATA$Abelmigpc > 0.07242524] <- "Migration third quantile"
MIGGEODATA$Abelmigflow2[MIGGEODATA$Abelmigpc >= 0.26653914] <- "Migration fourth quantile"

# Migration map 2
MIGGEODATA$year0 <- as.character(MIGGEODATA$year0)
MIGGEODATA <- filter(MIGGEODATA, year0 == "1960" |  year0 == "1970" |  year0 == "1980" |  year0 == "1990" |  year0 == "2000" |  year0 == "2010")
MIGGEODATA <-  subset(MIGGEODATA, orig %in% DATA$Iso3_a)

MIGGEODATA$Abelmigflow2 <- factor(MIGGEODATA$Abelmigflow2, levels = c("Migration first quantile", "Migration second quantile", "Migration third quantile", "Migration fourth quantile"))

MigrationMap <- tm_shape(GEODATA) +
  tm_polygons() +
  tm_shape(MIGGEODATA) +
  tm_polygons(col = "Abelmigflow2", 
              title = "Migration flow per capita",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#234756", "#88BACE", "#DD933B", "#A2291F"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(.1, .75)) +
  tm_facets(by = "year0", 
            # sync = T,
            # nrow = 3,
            # drop.empty.facets = T,
            # drop.NA.facets = T,
            # drop.units = F,
            # free.scales = F,
            # free.coords = F,
            textNA = "No recorded conflicts")

tmap_save(MigrationMap, filename = "10MigrationMap.png")

## State formation wave map
GEODATA$HenselFormationWave <- factor(GEODATA$HenselFormationWave, levels = c("Atlantic revolutions and prior (<1839)", "Neo-Europes (1839-1913)", "World Wars (1914-1944)", "Pt. 1 Anticolonial revolution (1945-1954)", "Pt. 2 Anticolonial revolution (1955-1984)", "Post-Soviet (+1985)"))

FormationMap <- tm_shape(GEODATA) +
  tm_polygons("HenselFormationWave", 
              title = "State formation wave",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#7030A0", "#234756", "#88BACE", "#388600", "#DD933B", "#A2291F"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 12: Period of state formation, incl. formation, decolonization, secession or partition (HenselFormationWave)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.05, .25))

tmap_save(FormationMap, filename = "12FormationMap.png")

## Formation violence map
ViolenceMap <- tm_shape(GEODATA) +
  tm_polygons("HenselIndViolence", 
              title = "Violence involved in a state's formation",
              id = "name_long",
              fontfamily = "Corbel",
              title.size = 1,
              palette = c("#234756", "#A2291F"),
              textNA = "No recorded conflicts",
              alpha = 1)+
  tm_layout(panel.labels = "Figure 14: Violence involved in a state's formation (HenselIndViolence)",
            panel.label.bg.color = "#FFF9F2",
            panel.label.color = "#000000",
            fontfamily = "Corbel",
            panel.label.size = 1,
            legend.position = c(0.025, .25))

tmap_save(ViolenceMap, filename = "14ViolenceMap.png")
