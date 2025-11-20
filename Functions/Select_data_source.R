#' Summarizes and compares record counts per institute from two data sources for a set of crops,
#' assigns organization type, checks EURISCO presence, and determines which data source to keep.
#'
#' @param Genesys_allcrops Data frame of Genesys accessions (must include "INSTCODE").
#' @param WIEWS_allcrops Data frame of WIEWS accessions (must include "INSTCODE").
#' @param institute_names_no_syn Data frame for organization types (must include "INSTCODE", "ORGANIZATIONTYPE").
#' @param eurisco_path File path to EURISCO Excel sheet (must have "INSTCODE").
#' @return Data frame: INSTCODE, Genesys_records, WIEWS_records, ORGANIZATIONTYPE, EURISCO, keep.
#' @import dplyr
#' @import readxl
#' @export
select_data_source <- function(
    Genesys_allcrops,
    WIEWS_allcrops,
    institute_names_no_syn,
    eurisco_path
) {
  # --- Input checks ---
  stopifnot("INSTCODE" %in% colnames(Genesys_allcrops))
  stopifnot("INSTCODE" %in% colnames(WIEWS_allcrops))
  stopifnot(all(c("INSTCODE", "ORGANIZATIONTYPE") %in% colnames(institute_names_no_syn)))

  # --- Read EURISCO safely ---
  eurisco_list <- tryCatch(
    readxl::read_excel(eurisco_path),
    error = function(e) stop("Could not read EURISCO file: ", e$message)
  )
  stopifnot("INSTCODE" %in% colnames(eurisco_list))
  eurisco_codes <- unique(eurisco_list$INSTCODE)

  # --- Count records per INSTCODE ---
  genesys_counts <- Genesys_allcrops %>%
    dplyr::count(INSTCODE, name = "Genesys_records")
  wiews_counts <- WIEWS_allcrops %>%
    dplyr::count(INSTCODE, name = "WIEWS_records")

  # --- Full set of institutes from all sources ---
  all_instcodes <- union(
    union(genesys_counts$INSTCODE, wiews_counts$INSTCODE),
    union(institute_names_no_syn$INSTCODE, eurisco_codes)
  ) %>% unique() %>% as.data.frame()
  colnames(all_instcodes) <- "INSTCODE"

  # --- Merge all info ---
  summary_table <- all_instcodes %>%
    left_join(genesys_counts, by = "INSTCODE") %>%
    left_join(wiews_counts, by = "INSTCODE") %>%
    left_join(institute_names_no_syn[, c("INSTCODE", "ORGANIZATIONTYPE")], by = "INSTCODE") %>%
    mutate(
      Genesys_records = tidyr::replace_na(Genesys_records, 0L),
      WIEWS_records   = tidyr::replace_na(WIEWS_records, 0L),
      ORGANIZATIONTYPE = tidyr::replace_na(ORGANIZATIONTYPE, "Unknown"),
      EURISCO = INSTCODE %in% eurisco_codes
    )

  # --- Selection logic for 'keep' column ---
  summary_table <- summary_table %>%
    mutate(
      keep = dplyr::case_when(
        ORGANIZATIONTYPE == "CGIAR" & Genesys_records > 0 ~ "Genesys",
        Genesys_records > WIEWS_records ~ "Genesys",
        WIEWS_records > Genesys_records ~ "WIEWS",
        Genesys_records == WIEWS_records ~ "Genesys",
        TRUE ~ NA_character_
      )
    )

  # --- Select only relevant columns ---
  summary_table %>%
    select(INSTCODE, Genesys_records, WIEWS_records, ORGANIZATIONTYPE, EURISCO, keep)
}
