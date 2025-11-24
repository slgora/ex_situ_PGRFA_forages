######## use conversion table with validated taxa for taxa standardisation,
## do the processing that comes after standardization,as these depend on the correct genus and species #########
library(dplyr)
library(purrr)

###### 1. Load datasets with taxa to be standardized using conversion table  #######################
# crops = updated forages croplist; datasets are re-processed with new forages added
crops <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/GCCS_Selected_crops/croplist_PG_forages.xlsx")
combined_df <- read_csv("../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_18/gen_wiews_df.csv")
BGCI_processed <- read_csv("../../GCCSmetricsI/Data_processing/1_merge_data/2025_07_07/BGCI_processed.csv") #not re-processed
GLIS_processed <- read_csv("../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_18/GLIS_processed.csv")
SGSV_allcrops <- read_csv("../../GCCSmetricsI/Data_processing/1_merge_data/2025_11_17/SGSV_allcrops_processed.csv")

# create a copy of fullTaxa without some characters that gave problems when using the conversion table
combined_df$fullTaxa2 <- trimws(
  gsub("\\s+", " ",
       gsub("\\?", "",
            gsub("\\+", " ",
                 na.omit(combined_df$fullTaxa)
            )
       )
  )
)
# create fullTaxa field in GLIS to remove food crops in later step
GLIS_processed <- GLIS_processed %>%
  mutate(fullTaxa = pmap_chr(
    list(GENUS, SPECIES, SUBTAXA),
    ~ {
      parts <- c(... )
      parts <- parts[!is.na(parts) & parts != ""]    # drop NA/empty
      if (length(parts) == 0) NA_character_ else paste(parts, collapse = " ")
    }
  ))

# Standardize new forages first using GRIN accepted names:
# If data_source = Genesys, genus = Digitaria or Setaria, then fill in Standardized_taxa with GRIN_NAME + GRIN_AUTHOR
# If GRIN_NAME is blank, then fill in with GENUS
combined_df$Standardized_taxa = NA
combined_df <- combined_df %>%
  mutate(
    GRIN_NAME_trim   = trimws(coalesce(GRIN_NAME, "")),
    GRIN_AUTHOR_trim = trimws(coalesce(GRIN_AUTHOR, "")),
    Standardized_taxa = case_when(
      data_source == "Genesys" & GENUS %in% c("Digitaria", "Setaria") & nchar(GRIN_NAME_trim) > 0 ~
        paste(GRIN_NAME_trim, GRIN_AUTHOR_trim),
      data_source == "Genesys" & GENUS %in% c("Digitaria", "Setaria") & nchar(GRIN_NAME_trim) == 0 ~
        GENUS,
      TRUE ~ Standardized_taxa
    )
  ) %>%
  select(-GRIN_NAME_trim, -GRIN_AUTHOR_trim)
# Standardized format of selected species
combined_df$Standardized_taxa[combined_df$Standardized_taxa == "Digitaria spp."] <- "Digitaria sp."
combined_df$Standardized_taxa[combined_df$Standardized_taxa == "Digitaria"] <- "Digitaria sp."
combined_df$Standardized_taxa[combined_df$Standardized_taxa == "Setaria spp."] <- "Setaria sp."
combined_df$Standardized_taxa[combined_df$Standardized_taxa == "Setaria"] <- "Setaria sp."

#Load standardization table of taxa in genebanks and botanic gardens
#table_standardization <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/Taxa_standardization/standardization_table_WFO_GRIN_2025_08_22.xlsx")
table_standardization <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/Taxa_standardization/standardization_table_WFO_GRIN_2025_08_22.xlsx")
#Load standardization table of taxa in botanic gardens only
#table_standardization_bgci <- read_excel("../../GCCS metrics project shared folder/GCCSmetricsI/Data_processing/Support_files/Taxa_standardization/BGCI_bg_taxa_excludeforages_standardization_table_WFO_GRIN_2025_11_07.xlsx")
table_standardization_bgci <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/Taxa_standardization/BGCI_bg_taxa_excludeforages_standardization_table_WFO_GRIN_2025_11_07.xlsx")
#Replace tabs with space in input_name
table_standardization$input_name <- gsub("\t", " ", table_standardization$input_name)
table_standardization_bgci$input_name <- gsub("\t", " ", table_standardization_bgci$input_name)

#Replace multiple spaces with one space
table_standardization$input_name <- gsub("\\s+", " ", table_standardization$input_name)
table_standardization_bgci$input_name <- gsub("\\s+", " ", table_standardization_bgci$input_name)

#Add a space after period, "." between words in PG_recommendation
table_standardization$PG_recommendation <- gsub("(?<=[a-zA-Z])\\.(?=[a-zA-Z])", ". ", table_standardization$PG_recommendation, perl = TRUE)
table_standardization_bgci$PG_recommendation <- gsub("(?<=[a-zA-Z])\\.(?=[a-zA-Z])", ". ", table_standardization_bgci$PG_recommendation, perl = TRUE)

#Add a space after period, "." before "&" symbol in PG_recommendation
table_standardization$PG_recommendation <- gsub("\\.\\s*&", ". &", as.character(table_standardization$PG_recommendation), perl = TRUE)
table_standardization_bgci$PG_recommendation <- gsub("\\.\\s*&", ". &", as.character(table_standardization_bgci$PG_recommendation), perl = TRUE)

# Create named vector: input_name -> PG_recommendation
standardization_table <- setNames(table_standardization$PG_recommendation, table_standardization$input_name)
standardization_table_bgci <- setNames(table_standardization_bgci$PG_recommendation, table_standardization_bgci$input_name)

#standardize taxa in genebanks
combined_df <- combined_df %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa2], Standardized_taxa))

#standardize taxa only in botanic gardens
BGCI_processed$Standardized_taxa = NA
BGCI_processed <- BGCI_processed %>%
  mutate( mapped = standardization_table_bgci[fullTaxa],
          mapped = if_else(mapped == fullTaxa, NA_character_, mapped),
          Standardized_taxa = coalesce(Standardized_taxa, mapped) ) %>%
  select(-mapped)
#standardize taxa in both genebanks and botanic gardens
BGCI_processed <- BGCI_processed %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa], Standardized_taxa))

# NOTE in SGSV dataset and GLIS dataset taxa were not yet standardized

###### 2. Assign crop strategy categorical variable #####################
# use Assign_crop_strategy.R function in Functions folder
source("Functions/Assign_crop_strategy.R")

combined_df <- combined_df %>%
  mutate(GENUS_standardized = word(!!sym("Standardized_taxa"), 1)) %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS_standardized") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

BGCI_processed <- BGCI_processed %>%
  mutate(GENUS_standardized = word(!!sym("Standardized_taxa"), 1)) %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS_standardized") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

GLIS_processed <- GLIS_processed %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

SGSV_allcrops <- SGSV_allcrops %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA


###### 3. Remove food crops from Forages          ######################
# Define the list of species to remove from forages
species_to_remove <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/GCCS_Selected_crops/species_to_remove_updated.xlsx")[[1]] %>%
  as.character() %>% trimws()
# remove forage rows where either taxa column exactly matches any species in the list
combined_df <- combined_df %>%
  filter(!(Crop_strategy == "Tropical and subtropical forages" &
             (trimws(as.character(Standardized_taxa)) %in% species_to_remove |
                trimws(as.character(fullTaxa2))         %in% species_to_remove)))

BGCI_processed <- BGCI_processed %>%
  filter(!(Crop_strategy == "Tropical and subtropical forages" &
             (trimws(as.character(Standardized_taxa))         %in% species_to_remove)))

GLIS_processed <- GLIS_processed %>%
  filter(!(Crop_strategy == "Tropical and subtropical forages" &
             (trimws(as.character(fullTaxa))         %in% species_to_remove)))

SGSV_allcrops <- SGSV_allcrops %>%
  filter(!(Crop_strategy == "Tropical and subtropical forages" &
             (trimws(as.character(fullTaxa))         %in% species_to_remove)))

###### 4. Assign diversity region               ######################
# It requires crops dataframe (croplist_PG_forages.xlsx) and countries in regions (countries_in_regions.xlsx)
source("Functions/Assign_diversity_regions.R")
countries_in_regions <- read_excel("../../GCCSmetricsI/Data_processing/Support_files/Geographical/countries_in_regions.xlsx")
combined_df = assign_diversity_regions(combined_df, crops = crops, countries_in_regions = countries_in_regions)

###### 5. Assign Annex 1 status                 ######################
# in the function assign_annex1status one needs to change the path for the file containing the list of Petota and Melongena species
# function taking a dataframe including a column taxa names and returning TRUE/FALSE ,
source("Functions/Assign_annex1_status.R")
combined_df = assign_annex1status(combined_df, standardize_taxa = 'Standardized_taxa')  # assumed the column with standardized taxa is named Standardized_taxa


##### save resulting datasets in folder: ../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/
write.csv(combined_df, '../../GCCSmetricsI/Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_11_18/combined_df.csv', row.names = FALSE)
write.csv(BGCI_processed, '../../GCCSmetricsI/Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_11_18/BGCI_processed.csv', row.names = FALSE)
write.csv(GLIS_processed, '../../GCCSmetricsI/Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_11_18/GLIS_processed.csv', row.names = FALSE)
write.csv(SGSV_allcrops, '../../GCCSmetricsI/Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_11_18/SGSV_processed.csv', row.names = FALSE)

### END SCRIPT ###
