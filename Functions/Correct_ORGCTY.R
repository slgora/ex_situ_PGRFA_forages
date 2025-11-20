#' Convert country names to ISO 3-letter codes using a lookup table, with manual corrections for special cases.
#'
#' This function converts a vector of country names (e.g., from ORIGCTY) to ISO 3-letter country codes.
#' It first checks a set of common special or ambiguous country names and assigns their ISO codes directly.
#' If an entry is not in the manual correction list, it looks up the code using a lookup table such as geo_names (with columns ctyFullName and country3).
#'
#' @param origcty_vec A character vector of country names to be converted (e.g., your ORIGCTY column).
#' @param lookup_df   A data frame for country name to code conversion (default: geo_names).
#' @param name_col    The column in lookup_df that contains country names (default: "ctyFullName").
#' @param code_col    The column in lookup_df that contains the 3-letter ISO codes (default: "country3").
#'
#' @return A character vector of ISO 3-letter country codes (with NAs for unmatched names).
#' @examples
#' # Example: geo_names loaded and a data frame df with ORIGCTY to convert:
#' df$ORIGCTY_ISO <- correct_origcty(df$ORIGCTY)
#'
#' # With manual correction:
#' correct_origcty(c("Bolivia (Plurinational State of)", "Netherlands"), geo_names)
#'
#' # With a custom lookup table:
#' correct_origcty(origcty_vec, lookup_df = my_table, name_col = "country_name", code_col = "iso3")
correct_origcty <- function(origcty_vec,
                            lookup_df = geo_names,
                            name_col = "ctyFullName",
                            code_col = "country3") {
  # Manual corrections: map special names to their ISO codes
  manual_iso <- c(
    "Bolivia (Plurinational State of)" = "BOL",
    "Democratic Republic of the Congo" = "COD",
    "United Kingdom" = "GBR",
    "French Guyana" = "GUF",
    "Iran (Islamic Republic of)" = "IRN",
    "Republic of Korea" = "KOR",
    "Republic of Moldova" = "MDA",
    "Netherlands (Kingdom of the)" = "NLD",
    "Democratic People's Republic of Korea" = "PRK",
    "Palestine" = "PSE",
    "Türkiye" = "TUR",
    "United Republic of Tanzania" = "TZA",
    "Venezuela (Bolivarian Republic of)" = "VEN",
    "British Virgin Islands" = "VGB",
    "United States Virgin Islands" = "VIR",
    "Soviet Union" = "SUN",
    "Serbia and Montenegro" = "SCG",
    "China, Hong Kong SAR" = "HKG",
    "Yugoslavia" = "YUG",
    "Czechoslovakia" = "CSK",
    "Netherlands Antilles" = "ANT"
  )

  # Assign codes for manual cases
  iso_codes <- manual_iso[origcty_vec]

  # For entries not in manual_iso, look them up in the table
  not_manual <- is.na(iso_codes)
  if (!(name_col %in% names(lookup_df)) || !(code_col %in% names(lookup_df))) {
    stop("Lookup data frame must have columns: ", name_col, " and ", code_col)
  }
  country_to_iso <- setNames(lookup_df[[code_col]], lookup_df[[name_col]])
  iso_codes[not_manual] <- country_to_iso[origcty_vec[not_manual]]

  return(iso_codes)
}

