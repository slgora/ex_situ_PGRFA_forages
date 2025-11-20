#' load_SGSV_data
#'
#' Loads and formats SGSV data for the 15 crops from the Svalbard Global Seed Vault portal (by genus).
#' Expects columns: "Institute name", "Institute code", "Institute acronym", "Accession number",
#' "Full scientific name", "Species", "Country of collection", "source_file".
#' Output: INSTCODE, instAcronym, ACCENUMB, fullTaxa, GENUS, SPECIES, origctyFullName, ID.
#'
#' For SPECIES, only the second word in 'fullTaxa' is used.
#'
#' @param filepath Path to the Excel file.
#' @return Data frame with standardized and parsed columns.
#' @examples
#' sgsv <- load_SGSV_data("SGSV_new15crops_unformatted.xlsx")
load_SGSV_data <- function(filepath){
  library(readxl)
  library(dplyr)
  library(stringr)

  SGSV_allcrops <- read_excel(filepath)
  # Assign column names matching the input file structure
  colnames(SGSV_allcrops) <- c(
    "Source",           # Source file
    "instName",         # Institute name
    "INSTCODE",         # Institute code
    "instAcronym",      # Institute acronym
    "ACCENUMB",         # Accession number
    "fullTaxa",         # Full scientific name
    "Species",          # Species (not used, will be parsed from fullTaxa)
    "origctyFullName"  # Country of collection
  )
  # Select and parse columns
  SGSV_allcrops <- SGSV_allcrops %>%
    select(INSTCODE, instAcronym, ACCENUMB, fullTaxa, origctyFullName) %>%
    mutate(
      GENUS = word(fullTaxa, 1),
      SPECIES = word(fullTaxa, 2),
      ID = paste0(ACCENUMB, INSTCODE)
    ) %>%
    select(INSTCODE, instAcronym, ACCENUMB, fullTaxa, GENUS, SPECIES, origctyFullName, ID)

  return(SGSV_allcrops)
}

