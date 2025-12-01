### Project: Global Crop Conservation Strategies Metrics ###
### Forages Re-processing
### Read in new forages data, pre-process for joining with previous forages data, re-process datasets
### Note: BGCI data not updated with new forages data
### (Filter for updated forages croplist later in script: 3_Post_taxa_standardization_processing.R)
### Data sources, clean individually and Join when it is possible (Genesys and WIEWS)
### Set working directory: working directory is assumed to be Code/R_code , following shared folder structure ####

#### Install packages ####
# tidyverse already includes tidyr , dplyr, readr, magrittr, stringr, readxl
library(tidyverse)
library(writexl)
library(rlang)

####################################################################################################
########### Read in all database data for all crops ################################################
BGCI_allcrops <- read_excel("../../GCCSmetricsI/Data/BGCIPlantSearch_data/FullReport_counts_and_origin/BGCI_allcrops_unformatted.xlsx")
WIEWS_allcrops <- read_csv("../../GCCSmetricsI/Data/FAO_WIEWS/Passport_data/SDGBrequestExp.csv")
Genesys_allcrops <- read_csv("../../GCCSmetricsI/Data/Genesys/Data_aggregated_all_selected_GCCS/Genesys_allcrops_unformatted.csv")

############ Read in new forages (Digitaria, Setaria) data #########################################
WIEWS_digitaria_setaria <- read_csv("../../GCCSmetricsI/Data/FAO_WIEWS/Passport_data/Digitaria_Setaria_Wiews_Exsitu_download_14_nov_2025.csv")
Genesys_digitaria_setaria <- read_excel("../../GCCSmetricsI/Data/Genesys/Data_aggregated_all_selected_GCCS/Digitaria_Setaria_Genesys_download_14_nov_2025.xlsx")
glis_digitaria_setaria <- read_excel("../../GCCSmetricsI/Data/Plant_Treaty/GLIS/Digitaria_Setaria_GLIS_APIextract_18_nov_2025.xlsx")

##### read file with country codes, I added na.strings to resolve the problem with NA for Namibia becoming a NaN value
geo_names <- read_csv("../../Data_processing/Support_files/Geographical/geo_names.csv" , na = c("", "-"))
## subset only the relevant column to join- 2 letter country code and the 3 letter abbreviation
geo_names <- subset(geo_names, select = c(country2, country3))
#####  file with institute names and FAO INSTCODE, some synonyms were added to the list
institute_names <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG_with_synonyms.xlsx")
names(institute_names)[names(institute_names) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names)[names(institute_names) == 'Name of organization'] <- 'Name_of_organization'
institute_names_full <- subset(institute_names, select = c(`INSTCODE`, `Name_of_organization`))  %>% drop_na()
institute_names_no_syn <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")
names(institute_names_no_syn)[names(institute_names_no_syn) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names_no_syn)[names(institute_names_no_syn) == 'Organization authority status'] <- 'ORGANIZATIONTYPE'
institute_names_no_syn <- subset(institute_names_no_syn, select = c(`INSTCODE`, `ORGANIZATIONTYPE`))  %>% drop_na()
WIEWS_institute_IDs <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/FAO_WIEWS/WIEWS_instIDs.xlsx")
WIEWS_institute_IDs = subset(WIEWS_institute_IDs, select = c('ID' , 'WIEWS_INSTCODE'))
#read file to select institution and data source
selection_data_sources <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/Source_selection/selection_data_sources.xlsx")

######################################################################################################
########## Change field names to follow MCPD standard see https://www.fao.org/plant-treaty/tools/toolbox-for-sustainable-use/details/en/c/1367915/ ############################################


############### BGCI Plant Search: Data Read in and Cleaning ####################
# PG notes BGCI country code is country of Botanical Garden not origin country of the plant (it may be the same ? but is it a. valid assumption)
# only useful column seems to be institute name, taxa name in plant search (standardized), and type of germplasm
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Name (in PlantSearch)'] <- 'fullTaxa'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Submitted Name'] <- 'SubmittedName'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Ex Situ Site GardenSearch ID'] <- 'ex_situ_site_gardenSearch_ID' #added
BGCI_allcrops <- cbind(BGCI_allcrops, data_source = "BGCI") # Add field: data source

##### correcting manually to remove quotes
BGCI_allcrops$SubmittedName <- gsub('[\\(\\)\"]', '', BGCI_allcrops$SubmittedName)

# when  fullTaxa is empty fill it with SubmittedName
BGCI_allcrops <- BGCI_allcrops %>%
  mutate(fullTaxa = ifelse(is.na(fullTaxa) | fullTaxa == "", SubmittedName, fullTaxa))

# Separate fields: fullSciName, still have fullTaxa (which is the fullSciName standardized by BGCI )
# Split the "fullSciName" column into "genus" and "species" without removing "fullSciName"
BGCI_allcrops <- BGCI_allcrops %>%
  mutate(GENUS   = word(fullTaxa, 1),  # Extract the first word (genus)
         SPECIES = word(fullTaxa, 2))  # Extract the second word (species)

### Encode all fields relevant to storage fields
BGCI_allcrops['Germplasm, seed'][BGCI_allcrops['Germplasm, seed'] == 1] <- 10
BGCI_allcrops['Germplasm, seed'][BGCI_allcrops['Germplasm, seed'] == 0] <- NA
BGCI_allcrops['Germplasm, plant'][BGCI_allcrops['Germplasm, plant'] == 1] <- 20
BGCI_allcrops['Germplasm, plant'][BGCI_allcrops['Germplasm, plant'] == 0] <- NA
BGCI_allcrops['Germplasm, pollen'][BGCI_allcrops['Germplasm, pollen'] == 1] <- 99
BGCI_allcrops['Germplasm, pollen'][BGCI_allcrops['Germplasm, pollen'] == 0] <- NA
BGCI_allcrops['Germplasm, explant'][BGCI_allcrops['Germplasm, explant'] == 1] <- 30
BGCI_allcrops['Germplasm, explant'][BGCI_allcrops['Germplasm, explant'] == 0] <- NA

# Combine all 4 fields into one storage field
BGCI_allcrops$STORAGE <- apply(BGCI_allcrops[, c("Germplasm, seed", "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant")], 1, function(x) paste(na.omit(x), collapse = "; "))

# Drop the specified columns from the BGCI_allcrops data frame
BGCI_allcrops <- select(BGCI_allcrops, -c('Germplasm, seed', "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant"))

# Fields we want to keep
BGCI_allcrops <- subset(BGCI_allcrops, select = c(data_source, fullTaxa, ex_situ_site_gardenSearch_ID, GENUS, SPECIES, STORAGE ))
# Note: you need to create folder DATE_OF_RUN before running the following line of code
write.csv(BGCI_allcrops, "../../GCCSmetricsI/Data_processing/1_merge_data/2025_10_20/BGCI_allcrops_processed.csv", row.names = FALSE)

############### WIEWS: Data Cleaning ####################
# ADD NEW FORAGES GENERA TO WIEWS before re-processing
# add column names to WIEWS_allcrops (no column names present in read in)
new_names <- c( "Country name","Holding institute code","Accession number",
                "Taxon","Genus","Species","Accepted Genus","Accepted Species",
                "Crop name","Acquisition date (YYYY/MM)","Country of origin (ISO3)",
                "Biological status","Genebank(s) holding safety duplications - code",
                "Genebank(s) holding safety duplications","Latitude of collecting site (decimal degrees format)",
                "Longitude of collecting site (decimal degrees format)","Collecting/acquisition source",
                "Type of germplasm storage","Status under the Multilateral System","DOI")
colnames(WIEWS_allcrops) <- new_names
# drop non-common columns to combine (keep only the columns present in WIEWS_allcrops)
common_cols <- intersect(colnames(WIEWS_digitaria_setaria), colnames(WIEWS_allcrops))
if (length(common_cols) != length(colnames(WIEWS_allcrops))) {
  message("Note: WIEWS_digitaria_setaria missing some columns present in WIEWS_allcrops; they will be added as NA later.")
}
WIEWS_digitaria_setaria <- WIEWS_digitaria_setaria[, common_cols, drop = FALSE]
# If any WIEWS_allcrops columns are missing in Digitaria, add them as NA
missing_in_dig <- setdiff(colnames(WIEWS_allcrops), colnames(WIEWS_digitaria_setaria))
for (mc in missing_in_dig) WIEWS_digitaria_setaria[[mc]] <- NA_character_
# reorder new forages columns to match WIEWS_allcrops
WIEWS_digitaria_setaria <- WIEWS_digitaria_setaria[, colnames(WIEWS_allcrops), drop = FALSE]
# align class type
WIEWS_allcrops[] <- lapply(WIEWS_allcrops, as.character)
WIEWS_digitaria_setaria[] <- lapply(WIEWS_digitaria_setaria, as.character)
# combine new forages with previous data (filter for updated forages croplist later)
WIEWS_allcrops <- bind_rows(WIEWS_allcrops, WIEWS_digitaria_setaria)

# Rename all columns according to MCPD naming style and select columns needed
WIEWS_allcrops <- WIEWS_allcrops %>%
  rename(
    holdingCty     = `Country name`,
    INSTCODE       = `Holding institute code`,
    ACCENUMB       = `Accession number`,
    fullTaxa       = `Taxon`,
    GENUS          = `Genus`,
    SPECIES        = `Species`,
    acceptedGenus  = `Accepted Genus`,
    acceptedSpecies= `Accepted Species`,
    CROPNAME       = `Crop name`,
    ACQDATE        = `Acquisition date (YYYY/MM)`,
    ORIGCTY        = `Country of origin (ISO3)`,
    SAMPSTAT       = `Biological status`,
    DUPLSITE       = `Genebank(s) holding safety duplications - code`,
    DUPLINSTNAME   = `Genebank(s) holding safety duplications`,
    DECLATITUDE    = `Latitude of collecting site (decimal degrees format)`,
    DECLONGITUDE   = `Longitude of collecting site (decimal degrees format)`,
    COLLSRC        = `Collecting/acquisition source`,
    STORAGE        = `Type of germplasm storage`,
    MLSSTAT        = `Status under the Multilateral System`,
    DOI            = `DOI`
  ) %>%
  select(
    holdingCty, INSTCODE, ACCENUMB, fullTaxa, GENUS, SPECIES,
    CROPNAME, ORIGCTY, SAMPSTAT, DUPLSITE, DUPLINSTNAME,
    DECLATITUDE, DECLONGITUDE, STORAGE, MLSSTAT, ACQDATE,
    COLLSRC, DOI)

# Add field: data source
WIEWS_allcrops <- cbind(WIEWS_allcrops, data_source = "WIEWS")

# format to MCPD standard for merging with Genesys
WIEWS_allcrops$SAMPSTAT[WIEWS_allcrops$SAMPSTAT == "Unknown"] <-  NA # replace unknown with NAs
WIEWS_allcrops$SAMPSTAT <- as.numeric(substr(WIEWS_allcrops$SAMPSTAT, 1, 3)) # convert SAMPSTAT in MCPD standard
WIEWS_allcrops$COLLSRC <- as.numeric(substr(WIEWS_allcrops$COLLSRC, 1, 2))  # convert COLLSRC following MCPD standard
# convert STORAGE in MCPD format
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "12) Seed medium-term"] <-  "10;12"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "20) Field collection"] <-  "20"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "12) Seed medium-term;13) Seed long-term"] <-  "10;12;13"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "13) Seed long-term"] <-  "10;13"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "13) Seed long-term;20) Field collection"] <-  "10;13;20"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "12) Seed medium-term;40) Cryopreserved collection"] <-  "10;12;40"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "12) Seed medium-term;40) Cryopreserved collection;50) DNA collection"] <-  "10;12;40;50"
WIEWS_allcrops$STORAGE[WIEWS_allcrops$STORAGE == "12) Seed medium-term;50) DNA collection"] <-  "10;12;50"

## Standardize ACCENUMB field: remove blank/space between institute abbreviation and number
WIEWS_allcrops  <- WIEWS_allcrops  %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))

# Split the DUPLSITE column into separate rows, trim spaces, join with WIEWS_institute_IDs conversion table
WIEWS_allcrops <- WIEWS_allcrops %>%
  separate_rows(DUPLSITE, sep = ";") %>%
  mutate(DUPLSITE = str_trim(DUPLSITE)) %>%
  mutate(DUPLSITE = as.integer(DUPLSITE)) %>%
  left_join(WIEWS_institute_IDs, by = c("DUPLSITE" = "ID"), relationship = "many-to-one")

# Combine rows back into a single row per original entry, with DUPLSITE and WIEWS_INSTCODE values separated by ";"
WIEWS_allcrops <- WIEWS_allcrops %>%
  group_by(across(-c(DUPLSITE, WIEWS_INSTCODE))) %>%
  summarize(
    DUPLSITE = paste(unique(DUPLSITE), collapse = ";"),
    WIEWS_INSTCODE = paste(unique(WIEWS_INSTCODE), collapse = ";"),
    .groups = 'drop')

WIEWS_allcrops <- WIEWS_allcrops %>%
  select(-DUPLSITE) %>%                 #drop DUPLSITE column with wiews IDs
  rename(DUPLSITE = WIEWS_INSTCODE)  # Rename WIEWS_INSTCODE to DUPLSITE

# recode MLSSTAT as TRUE and FALSE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "Included"] <-  TRUE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "I"] <-  TRUE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "Not included"] <-  FALSE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "N"] <-  FALSE
WIEWS_allcrops <- WIEWS_allcrops %>% mutate(MLSSTAT = as.logical(MLSSTAT))

############### Genesys PGR: Data Read in and Cleaning ####################
# ADD NEW FORAGE GENERA TO GENESYS before re-processing
# drop non-common columns to combine
Genesys_digitaria_setaria <- Genesys_digitaria_setaria %>% select(-AEGIS, -AVAILABLE, -COLLECTION)
Genesys_allcrops <- Genesys_allcrops %>% select(-`...1`, -Source)
# combine new forages with previous data (filter for updated forages croplist later)
Genesys_allcrops <- rbind(Genesys_digitaria_setaria, Genesys_allcrops)

# select columns to keep
Genesys_allcrops <- subset(Genesys_allcrops, select = c(INSTCODE, ACCENUMB,
                                                        GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR,
                                                        GRIN_NAME, GRIN_AUTHOR, CROPNAME, ACQDATE, ACCENAME, SAMPSTAT, #added GRIN author
                                                        DONORCODE, DONORNAME, OTHERNUMB, DONORNUMB, # added for PDCI calc
                                                        ORIGCTY, DECLATITUDE,DECLONGITUDE, ELEVATION,
                                                        BREDCODE, ANCEST, DUPLSITE, STORAGE,
                                                        COLLDATE, COLLSITE, COLLSRC, COLLNUMB, COLLCODE,
                                                        MLSSTAT, ACCEURL, DOI))

# Add field: data source
Genesys_allcrops <- cbind(Genesys_allcrops, data_source = "Genesys")

# Replace NA values with empty strings for 'subTaxa' and 'spAuthor'
Genesys_allcrops$SUBTAXA <- ifelse(is.na(Genesys_allcrops$SUBTAXA), "", Genesys_allcrops$SUBTAXA)
Genesys_allcrops$SPAUTHOR <- ifelse(is.na(Genesys_allcrops$SPAUTHOR), "", Genesys_allcrops$SPAUTHOR)

# Concatenate Genus, species, 'subTaxa', and 'spAuthor' with spaces in between
Genesys_allcrops$fullTaxa <- trimws(paste(Genesys_allcrops$GENUS, Genesys_allcrops$SPECIES, Genesys_allcrops$SUBTAXA, Genesys_allcrops$SPAUTHOR))

## Standardize ACCENUMB field. Remove blank/space between institute abbreviation and number
Genesys_allcrops <- Genesys_allcrops %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))

####################################################################################################
##### SELECTION DATA SOURCES TABLE #####          # SG Note: Use the previous selection_data_sources file:
# Use selection_data_sources to filter main data
genesys_keep_inst <- selection_data_sources %>% filter(keep == "Genesys") %>% pull(INSTCODE)
wiews_keep_inst   <- selection_data_sources %>% filter(keep == "WIEWS") %>% pull(INSTCODE)

Genesys_allcrops <- Genesys_allcrops %>% filter(INSTCODE %in% genesys_keep_inst)
WIEWS_allcrops   <- WIEWS_allcrops %>% filter(INSTCODE %in% wiews_keep_inst)

#####################################################################################################
## Combine Genesys and WIEWS data and Remove duplicates between Genesys and WIEWS, keep Genesys ##################################################
gen_wiews_df <- bind_rows(Genesys_allcrops, WIEWS_allcrops)
gen_wiews_df$ACCENUMB <- trimws(gen_wiews_df$ACCENUMB)
gen_wiews_df$INSTCODE <- trimws(gen_wiews_df$INSTCODE)
gen_wiews_df$ID <- paste0(gen_wiews_df$ACCENUMB, gen_wiews_df$INSTCODE)
gen_wiews_df$ID <- paste0(gen_wiews_df$ACCENUMB, gen_wiews_df$INSTCODE)

# create table with by GENUS and INSTCODE and data_source before dropping duplicates
gen_wiews_counts <- gen_wiews_df %>%
  group_by(INSTCODE, GENUS, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
# save
write.csv(gen_wiews_counts, "../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_18/gen_wiews_counts_before_dropping_duplicates.csv")

# Remove WIEWS rows where a Genesys row exists with the same (non-missing) DOI
gen_wiews_df <- gen_wiews_df %>%
  filter(
    !(data_source == "WIEWS" &
        !is.na(DOI) &
        DOI %in% gen_wiews_df$DOI[gen_wiews_df$data_source == "Genesys" & !is.na(gen_wiews_df$DOI)]))

# Remove duplicates based on ID number
gen_wiews_df <- gen_wiews_df[!duplicated(gen_wiews_df$ID), ]  # drop duplicates but keep the first occurrence, in this case Genesys

####### correct country codes iso-codes
source("../../GCCSmetricsI/Code/R_code/Functions/Correct_country_codes.R")
gen_wiews_df = correct_country_codes(gen_wiews_df, col = 'ORIGCTY')

####### assign organization type ############
source("../../GCCSmetricsI/Code/R_code/Functions/Assign_organization_status.R")
gen_wiews_df = assign_org_type(gen_wiews_df, institute_names_no_syn)

# save results
gen_wiews_df$STORAGE <- as.character(gen_wiews_df$STORAGE)
write.csv(gen_wiews_df, '../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_18/gen_wiews_df.csv', row.names = FALSE)

################## GLIS data #########################################################################
# GLIS data received from Plant Treaty
all_glis_data <- read_tsv("../../GCCSmetricsI/Data/Plant_Treaty/GLIS/glis_data_16_oct_2025.csv")

# ADD NEW FORAGE GENERA TO GLIS before re-processing
glis_digitaria_setaria$date <- as.numeric(glis_digitaria_setaria$date)
# combine new forages with previous data (filter for updated forages croplist later)
all_glis_data <- bind_rows(all_glis_data, glis_digitaria_setaria)

#rename all columns according to MCPD naming style, and select columns that are needed
all_glis_data <- all_glis_data %>%
  transmute(
    DOI = doi,
    ACCENUMB = holdsid,
    INSTCODE = holdwiews,
    GENUS = genus,
    SPECIES = species,
    SPAUTH = spauth,
    SUBTAXA = subtaxa,
    STAUTH = stauth,
    SAMPSTAT = biostatus,
    ORIGCTY = holdcountry,
    DECLATITUDE = colllat,
    DECLONGITUDE = colllon,
    MLS = as.numeric(ifelse(mlsstatus %in% c("", "(null)", NA), NA, mlsstatus)),
    date = date,
    HISTORICAL = historical
  )

# remove duplicate records in all_glis_data based on DOI, keeping only the first occurrence of each DOI
all_glis_data <- all_glis_data[!duplicated(all_glis_data$DOI), ]

# drop all historical records from GLIS data
all_glis_data <- all_glis_data[!(all_glis_data$HISTORICAL == "y"), ]

# replace all (null) with blank in every column
all_glis_data <- all_glis_data %>%
  mutate(across(everything(), ~replace(., . == "(null)", "")))
# drop rows where INSTCODE is NA or blank
all_glis_data <- all_glis_data %>% filter(!is.na(INSTCODE) & INSTCODE != "")

#generate MLSSTAT variable, used later to compute metrics on # of accessions included in the MLS
all_glis_data$MLSSTAT = NA
all_glis_data$MLSSTAT <- ifelse(all_glis_data$MLS %in% c(1, 11, 12, 13, 14, 15), TRUE, all_glis_data$MLSSTAT)
all_glis_data$MLSSTAT <- ifelse(all_glis_data$MLS %in% c(0), FALSE, all_glis_data$MLSSTAT)

# save results
write.csv(all_glis_data, '../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_18/GLIS_processed.csv', row.names = FALSE)

################# SGSV data ##########################################################################
source("../../GCCSmetricsI/Code/R_code/Functions/Load_SGSV_data.R")
sgsv = load_SGSV_data('../../GCCSmetricsI/Data/SGSV/Deposits_all_genera_aggregated/SGSV_allcrops_unformatted.xlsx')
sgsv_digitaria_setaria = load_SGSV_data('../../GCCSmetricsI/Data/SGSV/Deposits_all_genera_aggregated/SGSV_forages_unformatted.xlsx')
# combine new forages with previous data (filter for updated forages croplist later)
sgsv <- bind_rows(sgsv, sgsv_digitaria_setaria)

# create uniqueID and drop duplicates
sgsv$ACCENUMB <- trimws(sgsv$ACCENUMB)
sgsv$INSTCODE <- trimws(sgsv$INSTCODE)
sgsv$ID <- paste0(sgsv$ACCENUMB, sgsv$INSTCODE)
sgsv <- sgsv[!duplicated(sgsv$ID), ]
# save results
write.csv(sgsv, '../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_17/SGSV_allcrops_processed.csv', row.names = FALSE)

################# FAO WIEWS Indicator data ##############################################################
# read in FAO WIEWS indicator file and croplist_PG_forages within function
source("../../GCCSmetricsI/Code/R_code/Functions/Load_WIEWS_indicator_data.R")
WIEWS_indicator_proccessed <- process_wiews_indicator_data(
  wiews_path = "../../GCCSmetricsI/Data/FAO_WIEWS/Indicator_22_data/FAO_WIEWS_Indicator22.xlsx",
  croplist_path = "../../GCCSmetricsI/Data_processing/Support_files/GCCS_Selected_crops/croplist_PG_forages.xlsx")
# save results
write.csv(WIEWS_indicator_proccessed, '../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_17/WIEWS_indicator_processed.csv', row.names = FALSE)

######## END SCRIPT #######
