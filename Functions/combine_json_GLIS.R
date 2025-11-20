#!/usr/bin/env Rscript
# combine_glis_jsons_to_excel_fix.R
# Reads GLIS JSON files, normalizes list-columns (converts them to JSON/strings) so bind_rows won't fail,
# combines them, and writes an Excel file.
#
# Usage:
#  - Default (no args): uses the two paths you provided and writes GLIS_combined_Setaria_Digitaria.xlsx
#  - Provide 2 files: Rscript combine_glis_jsons_to_excel_fix.R fileA.json fileB.json
#  - Provide N>=3 args: last arg is output filename, preceding args are input JSONs:
#      Rscript combine_glis_jsons_to_excel_fix.R a.json b.json c.json out.xlsx

ensure_pkgs <- function(pkgs) {
  missing <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_pkgs(c("jsonlite", "dplyr", "purrr", "tibble", "openxlsx"))

read_json_to_df <- function(path) {
  p <- path.expand(path)
  if (!file.exists(p)) stop("File not found: ", path)
  txt <- paste0(readLines(p, warn = FALSE), collapse = "\n")
  obj <- jsonlite::fromJSON(txt, simplifyVector = TRUE, flatten = TRUE)
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))
  if (is.list(obj)) {
    df <- tryCatch({
      purrr::map_dfr(obj, function(x) {
        if (is.data.frame(x)) tibble::as_tibble(x)
        else if (is.list(x)) tibble::as_tibble(x)
        else tibble::tibble(value = x)
      })
    }, error = function(e) NULL)
    if (!is.null(df)) return(df)
    if (!is.null(names(obj)) && all(!vapply(obj, is.recursive, logical(1)))) {
      return(tibble::as_tibble(obj))
    }
  }
  stop("Unsupported or unexpected JSON structure in file: ", path)
}

# Convert any list-columns to character (JSON string or simple collapsed string).
# This ensures columns with same name across files have the same atomic type before bind_rows.
normalize_list_columns <- function(df) {
  if (nrow(df) == 0) return(df)
  list_cols <- names(df)[vapply(df, is.list, logical(1))]
  for (col in list_cols) {
    vec <- df[[col]]
    # Convert each element to a single character value
    df[[col]] <- vapply(seq_along(vec), function(i) {
      x <- vec[[i]]
      if (is.null(x)) return(NA_character_)
      # If atomic (e.g., character/numeric vector) and length 1, keep as character
      if (is.atomic(x) && length(x) == 1) return(as.character(x))
      # If atomic vector length>1, collapse with semicolon
      if (is.atomic(x) && length(x) > 1) return(paste(as.character(x), collapse = "; "))
      # Otherwise encode as JSON string (preserves structure)
      jsonlite::toJSON(x, auto_unbox = TRUE)
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  }
  df
}

combine_jsons <- function(files, out_xlsx = "GLIS_combined.xlsx", add_source = TRUE, sheet_name = "combined") {
  files <- unlist(files)
  dfs <- purrr::map(files, function(f) {
    df <- read_json_to_df(f)
    df <- normalize_list_columns(df)
    if (add_source) df <- dplyr::mutate(df, .source = basename(f))
    df
  })
  combined <- dplyr::bind_rows(dfs)
  openxlsx::write.xlsx(combined, file = out_xlsx, sheetName = sheet_name, rowNames = FALSE, asTable = TRUE)
  message("Wrote combined data to: ", normalizePath(out_xlsx))
  invisible(combined)
}

# Helper: inspect column classes to debug mismatches (run interactively)
inspect_cols <- function(df) {
  stats <- vapply(df, function(col) {
    paste(unique(vapply(col, function(x) class(x)[1], character(1))), collapse = "|")
  }, character(1))
  tibble::tibble(column = names(stats), types = stats)
}

# --- Main behavior ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  default_files <- c(
    "~/ex_situ_PGRFA_metrics_CLONE_forages/ex_situ_PGRFA_metrics_CLONE_forages/GLIS_result_Setaria.json",
    "~/ex_situ_PGRFA_metrics_CLONE_forages/ex_situ_PGRFA_metrics_CLONE_forages/GLIS_result_Digitaria.json"
  )
  combine_jsons(default_files, out_xlsx = "GLIS_combined_Setaria_Digitaria.xlsx")
} else if (length(args) == 1) {
  stop("Provide at least two JSON files to combine or pass two files and optional output filename.")
} else if (length(args) == 2) {
  combine_jsons(args, out_xlsx = "GLIS_combined.xlsx")
} else {
  in_files <- args[1:(length(args) - 1)]
  out_file <- args[length(args)]
  combine_jsons(in_files, out_xlsx = out_file)
}
