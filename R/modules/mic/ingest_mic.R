# ==============================================================================
# MIC qPCR - STEP 1: INGESTION
# ==============================================================================
# Reads BioMérieux Excel format and extracts Cq values for all targets
# ==============================================================================

#' Ingest MIC qPCR Data
#'
#' @param file_path Path to BioMérieux Excel file
#' @param verbose Logical, print diagnostic messages
#' @return List with:
#'   - cq_data: tibble with columns Name, target, Cq, Replicate, Type, is_control
#'   - samples: tibble of sample metadata from Samples sheet
#'   - run_settings: list of run metadata
#'   - thresholds: list of extracted thresholds per target
#' @export
ingest_mic <- function(file_path, verbose = FALSE) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  if (verbose) cat("📂 Ingesting MIC data from:", basename(file_path), "\n")

  # ===========================================================================
  # STEP 1.1: Read Samples sheet (Well -> Name mapping)
  # ===========================================================================
  samples_raw <- suppressMessages(readxl::read_excel(file_path, sheet = "Samples"))

  well_col <- if ("Well" %in% names(samples_raw)) "Well" else find_column(samples_raw, c("^Well$", "Well.*Pos"))
  name_col <- if ("Name" %in% names(samples_raw)) "Name" else find_column(samples_raw, c("^Name$", "Sample.*Name"))
  type_col <- find_column(samples_raw, c("^Type$", "Sample.*Type", "Control.*Type"))

  samples <- tibble::tibble(
    Well = if (!is.null(well_col)) as.character(samples_raw[[well_col]]) else NA_character_,
    Name = if (!is.null(name_col)) as.character(samples_raw[[name_col]]) else NA_character_,
    Type = if (!is.null(type_col)) as.character(samples_raw[[type_col]]) else "Unknown"
  ) %>%
    dplyr::mutate(
      Name = dplyr::coalesce(Name, Well),
      is_control = Type %in% c("NTC", "Standard", "Positive Control", "Negative Control", "Positive", "Negative") |
        grepl("Control|NTC|CP|CN", Name, ignore.case = TRUE),
      Type = dplyr::case_when(
        Type == "NTC" ~ "Negative",
        Type == "Negative Control" ~ "Negative",
        Type == "Positive Control" ~ "Positive",
        grepl("^CP$|Pos.*Control", Name, ignore.case = TRUE) ~ "Positive",
        grepl("^CN$|Neg.*Control|NTC", Name, ignore.case = TRUE) ~ "Negative",
        TRUE ~ Type
      )
    )

  if (verbose) cat("✓ Loaded", nrow(samples), "samples from Samples sheet\n")

  # ===========================================================================
  # STEP 1.2: Read per-target Result sheets
  # ===========================================================================
  sheets <- readxl::excel_sheets(file_path)

  target_defs <- list(
    "177T"       = "Cycling 177T Result",
    "18S2"       = "Cycling 18S2 Result",
    "RNAseP_DNA" = "Cycling RNAseP-DNA Result",
    "RNAseP_RNA" = "Cycling RNAseP-RNA Result"
  )

  cq_data <- tibble::tibble()
  thresholds <- list()

  for (tgt in names(target_defs)) {
    result_sheet <- target_defs[[tgt]]

    # Check if sheet exists
    if (!(result_sheet %in% sheets)) {
      if (verbose) cat("⚠ Skipping", tgt, "- sheet not found\n")
      next
    }

    # Read result table
    df <- read_any_result_table(file_path, result_sheet)

    if (!nrow(df)) {
      if (verbose) cat("⚠ Skipping", tgt, "- no data rows\n")
      next
    }

    # Find required columns
    cq_col <- pick_cq_col(df)
    nm_col <- pick_name_col(df)

    if (is.null(cq_col) || is.null(nm_col)) {
      if (verbose) cat("⚠ Skipping", tgt, "- missing columns\n")
      next
    }

    # Extract Cq values
    tmp <- df %>%
      dplyr::transmute(
        Name_raw = .data[[nm_col]],
        Cq = suppressWarnings(as.numeric(.data[[cq_col]]))
      ) %>%
      dplyr::mutate(target = tgt)

    # Map wells to names if needed
    if (identical(nm_col, "Well") || all(grepl("^[A-H]\\d{1,2}$", tmp$Name_raw, perl = TRUE), na.rm = TRUE)) {
      tmp <- tmp %>%
        dplyr::rename(Well = Name_raw) %>%
        dplyr::left_join(samples %>% dplyr::select(Well, Name), by = "Well") %>%
        dplyr::mutate(Name = dplyr::coalesce(Name, Well)) %>%
        dplyr::select(Name, Cq, target)
    } else {
      tmp <- tmp %>%
        dplyr::rename(Name = Name_raw) %>%
        dplyr::select(Name, Cq, target)
    }

    cq_data <- dplyr::bind_rows(cq_data, tmp)

    # Extract threshold if available
    res_raw <- suppressMessages(readxl::read_excel(file_path, sheet = result_sheet, col_names = FALSE))
    res_mat <- apply(res_raw, 2, as.character)
    has_thr <- which(apply(res_mat, 1, function(r) any(grepl("Threshold|Cutoff", r, TRUE))))
    if (length(has_thr)) {
      row_i <- has_thr[1]
      nums <- suppressWarnings(as.numeric(res_mat[row_i, ]))
      thr <- nums[which(!is.na(nums))[1]]
      if (!is.na(thr)) {
        thresholds[[tgt]] <- thr
        if (verbose) cat("✓", tgt, "- extracted", nrow(tmp), "rows, threshold:", thr, "\n")
      }
    } else {
      if (verbose) cat("✓", tgt, "- extracted", nrow(tmp), "rows\n")
    }
  }

  if (!nrow(cq_data)) {
    stop("No Cq data extracted from any target sheet. Check file format.")
  }

  # ===========================================================================
  # STEP 1.3: Join with sample metadata and add replicate numbers
  # ===========================================================================
  samples_simple <- samples %>%
    dplyr::select(Name, Type, is_control) %>%
    dplyr::distinct()

  out <- cq_data %>%
    dplyr::left_join(samples_simple, by = "Name") %>%
    dplyr::group_by(Name, target) %>%
    dplyr::mutate(Replicate = paste0("Rep", dplyr::row_number())) %>%
    dplyr::ungroup()

  if (verbose) {
    cat("\n✅ Ingestion complete:\n")
    cat("   Total Cq rows:", nrow(out), "\n")
    cat("   Targets:", paste(unique(out$target), collapse = ", "), "\n")
    cat("   Samples:", length(unique(out$Name)), "\n\n")
  }

  list(
    cq_data = out,
    samples = samples,
    run_settings = list(file = basename(file_path)),
    thresholds = thresholds
  )
}

# ==============================================================================
# HELPER FUNCTIONS (from mic_qpcr_pipeline.R)
# ==============================================================================

find_column <- function(df, patterns) {
  for (pattern in patterns) {
    m <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) return(m[1])
  }
  NULL
}

guess_header_row <- function(mat) {
  candidates <- which(apply(mat, 1, function(r) {
    any(grepl("\\b(Well|Name|Sample|ID|Identifier|Position)\\b", r, ignore.case = TRUE)) &&
      any(grepl("\\b(Cq|Ct|Cp|Quant.*Cq|Cross.*Point)\\b", r, ignore.case = TRUE))
  }))
  if (length(candidates)) candidates[1] else NA_integer_
}

read_any_result_table <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
  if (nrow(raw) == 0) return(tibble::tibble())

  mat <- apply(raw, 2, as.character)
  hdr <- guess_header_row(mat)

  if (is.na(hdr)) return(tibble::tibble())

  headers <- as.character(unlist(raw[hdr, ]))
  headers <- make.names(trimws(headers), unique = TRUE)

  dat <- raw[(hdr + 1):nrow(raw), , drop = FALSE]
  names(dat) <- headers

  dat <- tibble::as_tibble(dat)
  dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]

  if (nrow(dat) == 0) return(tibble::tibble())

  dat
}

pick_cq_col <- function(df) {
  find_column(df, c("^Cq$", "^Ct$", "^Cp$",
                    "Cq\\s*Mean", "Ct\\s*Mean", "Cp\\s*Mean",
                    "Cq\\s*Avg", "Ct\\s*Avg", "Cp\\s*Avg",
                    "Cq.*Value", "Ct.*Value", "Cp.*Value",
                    "Quant.*Cq", "Cross.*Point"))
}

pick_name_col <- function(df) {
  if ("Name" %in% names(df)) return("Name")
  find_column(df, c("^Name$", "Sample.*Name", "^Sample$", "Identifier", "ID",
                    "^Well$", "Well.*Position", "Position"))
}
