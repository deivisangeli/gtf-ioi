#########################################################################################################
# Utility: Reconcile record_ids when raw data changes for existing years
# Author: Thais Takeuchi (3/2026)
#
# Sourced by all *_record_id.R scripts. Provides reconcile_with_existing() which:
#   - Preserves record_ids for people already in the data
#   - Assigns new record_ids (max_seq + 1, +2, ...) for people added to existing years
#   - Retires record_ids for people removed (numbers are never reused)
#   - Generates fresh IDs for brand-new years
#   - Reports a clear summary of all changes
#########################################################################################################

#' Reconcile record_ids when raw data changes for existing years
#'
#' @param new_data    Raw data (no record_id column) with all years
#' @param existing_data Previous output (with record_id column)
#' @param match_keys  Character vector of columns that identify a person-year
#'                    (e.g., c("year", "code", "surname", "name")).
#'                    Must include "year". Should NOT include rank/score/award.
#' @param comp_code   Competition code string (e.g., "imo", "pamo")
#' @param generate_ids_fn Function(data) that generates record_ids for brand-new years
#' @return Data frame with reconciled record_ids (record_id as first column)
reconcile_with_existing <- function(new_data, existing_data, match_keys, comp_code, generate_ids_fn) {

  stopifnot("year" %in% match_keys)

  # Safety: remove record_id from new_data if it exists
  if ("record_id" %in% names(new_data)) {
    new_data <- new_data[, setdiff(names(new_data), "record_id")]
  }

  # Normalize match key columns (trim whitespace) in both datasets to prevent
  # spurious mismatches that cause record_ids to shift between runs
  char_keys <- match_keys[sapply(match_keys, function(k) is.character(new_data[[k]]))]
  for (k in char_keys) {
    new_data[[k]]      <- trimws(new_data[[k]])
    existing_data[[k]] <- trimws(existing_data[[k]])
  }

  shared_years    <- sort(intersect(unique(new_data$year), unique(existing_data$year)))
  new_only_years  <- sort(setdiff(unique(new_data$year), unique(existing_data$year)))

  # ---- Pre-check: column consistency between existing and new data ----
  cat("--- Consistency check: existing vs. new raw data ---\n")
  existing_cols  <- sort(setdiff(names(existing_data), "record_id"))
  new_cols       <- sort(names(new_data))
  cols_only_old  <- setdiff(existing_cols, new_cols)
  cols_only_new  <- setdiff(new_cols, existing_cols)

  if (length(cols_only_old) > 0) {
    cat("  WARN - Columns in existing but NOT in new data: ",
        paste(cols_only_old, collapse = ", "), "\n", sep = "")
  }
  if (length(cols_only_new) > 0) {
    cat("  WARN - Columns in new data but NOT in existing: ",
        paste(cols_only_new, collapse = ", "), "\n", sep = "")
  }
  if (length(cols_only_old) == 0 && length(cols_only_new) == 0) {
    cat("  Columns: OK (all match)\n")
  }

  # Columns available for value comparison (present in both, excluding keys and record_id)
  comparable_cols <- setdiff(intersect(existing_cols, new_cols), match_keys)

  result_parts    <- list()
  total_preserved <- 0L
  total_added     <- 0L
  total_removed   <- 0L
  total_modified  <- 0L

  # ---- Shared years: reconcile within-year changes ----
  for (yr in shared_years) {
    old_yr <- existing_data[existing_data$year == yr, ]
    new_yr <- new_data[new_data$year == yr, ]

    # Validate uniqueness of match keys within each year
    key_old <- old_yr[, match_keys, drop = FALSE]
    key_new <- new_yr[, match_keys, drop = FALSE]
    has_dup_old <- anyDuplicated(key_old) > 0
    has_dup_new <- anyDuplicated(key_new) > 0

    # Max sequence number already used for this year
    seq_nums <- as.integer(gsub(paste0("^\\d+_", comp_code, "_"), "", old_yr$record_id))
    max_seq  <- max(seq_nums)

    # --- Fallback: positional matching when match keys have duplicates ---
    if (has_dup_old || has_dup_new) {
      n_dup <- max(sum(duplicated(key_old)), sum(duplicated(key_new)))
      cat("  Year ", yr, ": WARN - ", n_dup, " duplicate match key(s), using positional matching\n", sep = "")

      # Sort both identically so rows align
      sort_cols <- setdiff(match_keys, "year")
      old_sorted <- old_yr[do.call(order, old_yr[, sort_cols, drop = FALSE]), ]
      new_sorted <- new_yr[do.call(order, new_yr[, sort_cols, drop = FALSE]), ]

      if (nrow(old_sorted) == nrow(new_sorted)) {
        # Same size: carry over record_ids positionally
        new_sorted$record_id <- old_sorted$record_id
        yr_result <- new_sorted[, c("record_id", setdiff(names(new_sorted), "record_id")), drop = FALSE]
        cat("  Year ", yr, ": OK (", nrow(yr_result), " rows, positional match)\n", sep = "")
        total_preserved <- total_preserved + nrow(yr_result)
      } else {
        # Different size: preserve matching rows positionally up to min, handle rest
        n_min <- min(nrow(old_sorted), nrow(new_sorted))
        preserved <- new_sorted[seq_len(n_min), ]
        preserved$record_id <- old_sorted$record_id[seq_len(n_min)]
        preserved <- preserved[, c("record_id", setdiff(names(preserved), "record_id")), drop = FALSE]
        total_preserved <- total_preserved + n_min

        if (nrow(new_sorted) > n_min) {
          extra <- new_sorted[(n_min + 1):nrow(new_sorted), ]
          extra$record_id <- paste0(
            extra$year, "_", comp_code, "_",
            sprintf("%04d", max_seq + seq_len(nrow(extra)))
          )
          extra <- extra[, c("record_id", setdiff(names(extra), "record_id")), drop = FALSE]
          yr_result <- rbind(preserved, extra)
          total_added <- total_added + nrow(extra)
          cat("  Year ", yr, ": ", nrow(extra), " new record(s) added\n", sep = "")
        } else {
          yr_result <- preserved
          n_removed_yr <- nrow(old_sorted) - n_min
          total_removed <- total_removed + n_removed_yr
          if (n_removed_yr > 0) {
            cat("  Year ", yr, ": ", n_removed_yr, " record(s) removed\n", sep = "")
          }
        }
      }

      result_parts[[as.character(yr)]] <- yr_result
      next
    }

    # --- Standard matching (unique match keys) ---
    # Matched: records in both old and new (keep old record_id, use new data values)
    old_keys_only <- old_yr[, c("record_id", match_keys), drop = FALSE]
    matched <- merge(new_yr, old_keys_only, by = match_keys, all = FALSE)
    matched <- matched[, c("record_id", setdiff(names(matched), "record_id")), drop = FALSE]

    # Value check: for matched records, detect if any non-key column values changed
    if (nrow(matched) > 0 && length(comparable_cols) > 0) {
      old_matched <- merge(old_yr, matched[, c("record_id", match_keys), drop = FALSE],
                           by = c("record_id", match_keys))
      old_matched <- old_matched[order(old_matched$record_id), , drop = FALSE]
      new_matched <- matched[order(matched$record_id), , drop = FALSE]

      changed_cols   <- character(0)
      any_row_diff   <- rep(FALSE, nrow(old_matched))
      cols_to_check  <- intersect(comparable_cols, intersect(names(old_matched), names(new_matched)))

      for (col in cols_to_check) {
        o <- as.character(old_matched[[col]])
        n <- as.character(new_matched[[col]])
        diff_mask <- (is.na(o) != is.na(n)) |
                     (!is.na(o) & !is.na(n) & o != n)
        if (any(diff_mask)) {
          changed_cols <- c(changed_cols, col)
          any_row_diff <- any_row_diff | diff_mask
        }
      }
      n_rows_changed <- sum(any_row_diff)

      if (n_rows_changed > 0) {
        cat("  Year ", yr, ": ", n_rows_changed, " matched record(s) with updated values in: ",
            paste(changed_cols, collapse = ", "), "\n", sep = "")
        total_modified <- total_modified + n_rows_changed
      }
    }

    # Added: in new but not in old -> assign new record_ids continuing from max_seq
    key_str_old <- do.call(paste, c(key_old, sep = "\x1F"))
    key_str_new <- do.call(paste, c(key_new, sep = "\x1F"))
    added_mask <- !key_str_new %in% key_str_old
    added <- new_yr[added_mask, ]

    if (nrow(added) > 0) {
      added <- added[do.call(order, added[, setdiff(match_keys, "year"), drop = FALSE]), ]
      added$record_id <- paste0(
        added$year, "_", comp_code, "_",
        sprintf("%04d", max_seq + seq_len(nrow(added)))
      )
      added <- added[, c("record_id", setdiff(names(added), "record_id")), drop = FALSE]
      cat("  Year ", yr, ": ", nrow(added), " new record(s) added -> ",
          paste(added$record_id, collapse = ", "), "\n", sep = "")
    }

    # Removed: in old but not in new -> retired (logged but not kept)
    removed_mask <- !key_str_old %in% key_str_new
    removed <- old_yr[removed_mask, ]

    if (nrow(removed) > 0) {
      cat("  Year ", yr, ": ", nrow(removed), " record(s) removed (retired IDs: ",
          paste(removed$record_id, collapse = ", "), ")\n", sep = "")
    }

    # Combine matched + added for this year
    yr_result <- rbind(matched, added)
    result_parts[[as.character(yr)]] <- yr_result

    n_matched <- nrow(matched)
    n_added   <- nrow(added)
    n_removed <- nrow(removed)

    total_preserved <- total_preserved + n_matched
    total_added     <- total_added + n_added
    total_removed   <- total_removed + n_removed

    # Log per-year summary only if there were changes
    if (n_added > 0 || n_removed > 0) {
      cat("  Year ", yr, " summary: ", n_matched, " preserved, ",
          n_added, " added, ", n_removed, " removed\n", sep = "")
    } else {
      cat("  Year ", yr, ": OK (", n_matched, " rows, no additions or removals)\n", sep = "")
    }
  }

  # ---- New-only years: generate fresh IDs ----
  if (length(new_only_years) > 0) {
    cat("New years: ", paste(new_only_years, collapse = ", "), "\n", sep = "")
    df_new_years <- generate_ids_fn(new_data[new_data$year %in% new_only_years, ])
    result_parts[["__new_years__"]] <- df_new_years
  }

  # ---- Combine all parts ----
  result <- do.call(rbind, result_parts)
  rownames(result) <- NULL
  result <- result[order(result$year, result$record_id), ]

  # ---- Verification: preserved record_ids are unchanged ----
  for (yr in shared_years) {
    old_ids <- existing_data$record_id[existing_data$year == yr]
    result_yr <- result[result$year == yr, ]
    preserved_ids <- result_yr$record_id[result_yr$record_id %in% old_ids]
    expected_ids  <- sort(old_ids[old_ids %in% result_yr$record_id])
    if (!identical(sort(preserved_ids), expected_ids)) {
      stop("Record ID integrity check FAILED for year ", yr,
           ". Existing record_ids were not preserved correctly.")
    }
  }

  # ---- Summary ----
  cat("\n--- Reconciliation summary ---\n")
  cat("Preserved: ", total_preserved, " | Added: ", total_added,
      " | Removed: ", total_removed, " | Modified values: ", total_modified, "\n", sep = "")
  if (length(new_only_years) > 0) {
    n_new <- sum(result$year %in% new_only_years)
    cat("New years: ", paste(new_only_years, collapse = ", "),
        " (", n_new, " records)\n", sep = "")
  }
  if (total_added == 0 && total_removed == 0 && total_modified == 0 && length(new_only_years) == 0) {
    cat("No changes detected. Existing data is identical to new raw data.\n")
  }
  cat("Total: ", nrow(result), " records\n", sep = "")

  return(result)
}
