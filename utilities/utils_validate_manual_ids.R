#########################################################################################################
# Utility: Validate and auto-repair record_ids in manual xlsx files
# Author: Thais Takeuchi (3/2026)
#
# Sourced by all *_data_clean.R scripts before applying manual splits/merges.
# For each manual xlsx file:
#   1. Checks that every record_id exists in the current data
#   2. If broken IDs found in corrections (pairs), attempts to resolve via the valid pair's name
#   3. Generates a before/after corrections xlsx report
#   4. Updates the manual xlsx with corrected IDs
#   5. Stops only if any IDs could not be resolved automatically
#########################################################################################################

#' Validate and auto-repair record_ids in a manual xlsx file
#'
#' @param xlsx_path   Path to the xlsx file (splits or corrections)
#' @param valid_ids   Character vector of all valid record_ids in the current data
#' @param id_columns  Character vector of column names containing record_ids
#' @param label       Label for log messages
#' @param df_current  Data frame with current data (must have record_id + name columns for resolution)
#' @param output_dir  Directory to save the corrections report xlsx (NULL = same dir as xlsx_path)
#' @return Invisible TRUE if all valid or all repaired; stops if unresolvable IDs remain
validate_manual_ids <- function(xlsx_path, valid_ids, id_columns, label = basename(xlsx_path),
                                df_current = NULL, output_dir = NULL) {
  if (!file.exists(xlsx_path)) return(invisible(TRUE))

  df_manual <- readxl::read_xlsx(xlsx_path)
  if (nrow(df_manual) == 0) return(invisible(TRUE))

  # Collect all broken record_ids with their column and row
  broken_info <- data.frame(row = integer(), col = character(), old_id = character(),
                            stringsAsFactors = FALSE)
  total_ids <- 0L

  for (col in id_columns) {
    if (!col %in% names(df_manual)) next
    for (i in seq_len(nrow(df_manual))) {
      val <- as.character(df_manual[[col]][i])
      if (is.na(val) || !nzchar(trimws(val))) next
      total_ids <- total_ids + 1L
      if (!val %in% valid_ids) {
        broken_info <- rbind(broken_info, data.frame(row = i, col = col, old_id = val,
                                                     stringsAsFactors = FALSE))
      }
    }
  }

  if (nrow(broken_info) == 0) {
    cat("  ", label, ": OK (", total_ids, " IDs validated)\n", sep = "")
    return(invisible(TRUE))
  }

  cat("\n  ", label, ": ", nrow(broken_info), " broken ID(s) found\n", sep = "")

  # --- Attempt auto-resolution for corrections (paired record_ids) ---
  if (!is.null(df_current) && length(id_columns) == 2 &&
      all(c("record_id_a", "record_id_b") %in% id_columns)) {

    # Build name lookups from current data
    # Try common name column patterns
    name_cols <- intersect(names(df_current),
                           c("last", "first", "last_name", "first_name",
                             "surname", "name", "family_name", "given_name", "full_name"))

    if (length(name_cols) > 0) {
      cat("  Attempting auto-resolution via name matching...\n")
      resolved <- character(nrow(broken_info))

      # Detect country and canonical_name columns for fuzzy fallback
      country_col <- intersect(names(df_current), c("iso3code", "code"))[1]
      has_canonical <- "canonical_name" %in% names(df_current)

      for (b in seq_len(nrow(broken_info))) {
        broken_row <- broken_info$row[b]
        broken_col <- broken_info$col[b]
        broken_id  <- broken_info$old_id[b]

        # Find the valid pair
        other_col <- setdiff(c("record_id_a", "record_id_b"), broken_col)
        valid_pair_id <- as.character(df_manual[[other_col]][broken_row])

        if (is.na(valid_pair_id) || !valid_pair_id %in% valid_ids) {
          cat("    ", broken_id, ": BOTH sides broken, cannot resolve\n", sep = "")
          next
        }

        # Get the person's name from the valid pair
        valid_row <- df_current[df_current$record_id == valid_pair_id, ]
        if (nrow(valid_row) == 0) next

        # Extract year from broken ID
        broken_year <- as.integer(sub("_.*", "", broken_id))
        year_rows <- df_current[df_current$year == broken_year, ]
        if (nrow(year_rows) == 0) {
          cat("    ", broken_id, ": year ", broken_year, " not in current data\n", sep = "")
          next
        }

        # Narrow by country when possible
        if (!is.na(country_col) && country_col %in% names(year_rows)) {
          valid_country <- as.character(valid_row[[country_col]][1])
          country_rows <- year_rows[year_rows[[country_col]] == valid_country, ]
          if (nrow(country_rows) > 0) year_rows <- country_rows
        }

        # --- Strategy 1: exact last name matching ---
        new_id <- NA_character_
        for (ln_col in intersect(name_cols, c("last", "last_name", "surname", "family_name"))) {
          if (is.na(valid_row[[ln_col]][1]) || valid_row[[ln_col]][1] == "") next
          last_val <- tolower(trimws(valid_row[[ln_col]][1]))
          candidates <- year_rows[tolower(trimws(year_rows[[ln_col]])) == last_val, ]
          if (nrow(candidates) == 1) {
            new_id <- candidates$record_id[1]
            break
          } else if (nrow(candidates) > 1) {
            for (fn_col in intersect(name_cols, c("first", "first_name", "name", "given_name"))) {
              if (is.na(valid_row[[fn_col]][1])) next
              first_val <- tolower(trimws(valid_row[[fn_col]][1]))
              exact <- candidates[tolower(trimws(candidates[[fn_col]])) == first_val, ]
              if (nrow(exact) == 1) { new_id <- exact$record_id[1]; break }
            }
            if (!is.na(new_id)) break
          }
        }

        # --- Strategy 2: exact full_name ---
        if (is.na(new_id) && "full_name" %in% name_cols) {
          fn_val <- tolower(trimws(valid_row$full_name[1]))
          if (!is.na(fn_val) && fn_val != "") {
            candidates <- year_rows[tolower(trimws(year_rows$full_name)) == fn_val, ]
            if (nrow(candidates) == 1) new_id <- candidates$record_id[1]
          }
        }

        # --- Strategy 3: fuzzy canonical_name matching (handles transliteration) ---
        if (is.na(new_id) && has_canonical && requireNamespace("stringdist", quietly = TRUE)) {
          ref_name <- tolower(trimws(valid_row$canonical_name[1]))
          if (!is.na(ref_name) && ref_name != "") {
            dists <- stringdist::stringdist(ref_name,
                                            tolower(trimws(year_rows$canonical_name)),
                                            method = "jw", p = 0.1)
            best_idx <- which.min(dists)
            if (length(best_idx) == 1 && dists[best_idx] < 0.15) {
              # Ensure the best match is clearly better than the second best
              sorted_dists <- sort(dists)
              if (length(sorted_dists) < 2 || sorted_dists[2] - sorted_dists[1] > 0.05) {
                new_id <- year_rows$record_id[best_idx]
              }
            }
          }
        }

        resolved[b] <- ifelse(is.na(new_id), "", new_id)

        if (!is.na(new_id) && new_id != "") {
          cat("    ", broken_id, " -> ", new_id, "\n", sep = "")
        } else {
          cat("    ", broken_id, ": could not resolve automatically\n", sep = "")
        }
      }

      broken_info$new_id <- resolved

      # --- Apply fixes and save report ---
      fixable <- broken_info[broken_info$new_id != "", ]
      unfixable <- broken_info[broken_info$new_id == "", ]

      if (nrow(fixable) > 0) {
        # Save corrections report (fixed filename, overwrites previous)
        report_dir <- if (!is.null(output_dir)) output_dir else dirname(xlsx_path)
        report_name <- paste0(sub("\\.xlsx$", "", basename(xlsx_path)),
                              "_id_corrections.xlsx")
        report_path <- file.path(report_dir, report_name)

        report_df <- fixable[, c("old_id", "new_id"), drop = FALSE]
        names(report_df) <- c("old_record_id", "new_record_id")
        writexl::write_xlsx(report_df, report_path)
        cat("  Saved corrections report: ", basename(report_path), "\n", sep = "")

        # Update the manual xlsx
        wb <- openxlsx::loadWorkbook(xlsx_path)
        ws <- openxlsx::readWorkbook(wb, sheet = 1)

        for (f in seq_len(nrow(fixable))) {
          r <- fixable$row[f]
          col_name <- fixable$col[f]
          ws[[col_name]][r] <- fixable$new_id[f]
        }

        openxlsx::writeData(wb, sheet = 1, ws, colNames = TRUE)
        openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
        cat("  Updated ", basename(xlsx_path), " with ", nrow(fixable), " corrected ID(s)\n", sep = "")
      }

      if (nrow(unfixable) > 0) {
        cat("\n!!! UNRESOLVABLE record_ids in ", label, " !!!\n", sep = "")
        for (u in seq_len(nrow(unfixable))) {
          cat("  ", unfixable$col[u], ": ", unfixable$old_id[u], "\n", sep = "")
        }
        cat("\nUpdate these manually in the xlsx before re-running.\n\n")
        stop("Found ", nrow(unfixable), " unresolvable record_id(s) in ", label,
             ". Fix manually and re-run.", call. = FALSE)
      }

      cat("  ", label, ": REPAIRED (", nrow(fixable), " ID(s) corrected)\n", sep = "")
      return(invisible(TRUE))

    } else {
      cat("  No name columns found in data for auto-resolution\n")
    }
  }

  # --- No auto-resolution possible (splits or no df_current) ---
  cat("\n!!! BROKEN record_ids in ", label, " !!!\n", sep = "")
  for (b in seq_len(nrow(broken_info))) {
    cat("  ", broken_info$col[b], ": ", broken_info$old_id[b], "\n", sep = "")
  }
  cat("\nThese IDs no longer exist in the current id file.\n")
  cat("Update the xlsx before running the cleaning script.\n\n")
  stop("Found ", nrow(broken_info), " broken record_id(s) in ", label,
       ". Fix the xlsx file and re-run.", call. = FALSE)
}
