# =============================================================================
# AMM ADAPTATION -- ADDITIONAL POOL: wCELO -- STEP 1 INPUT CHECK
# =============================================================================
#
# PURPOSE
# -------
# This read-only checkpoint verifies the two wCELO comparison datasets before
# they enter either the replicated AMM-paper pipeline or the WTEYE-style AMM
# adaptation. It follows the role of the original
# amm_adaptation_step_1_check_inputs.R script.
#
# This script does NOT detect wash trading, calculate candidate groups, change
# either CSV, or create cleaned data. A PASS means only that the supplied inputs
# have the expected structure and identity.
#
# EXPECTED INPUT FILES IN Data/raw
# --------------------------------
# 12C_wcelo_suspicious_pool_raw_swaps.csv
# 12E_wcelo_replacement_candidate_rank2_raw_swaps.csv
# 13_tokens_and_decimals_for_additional_pools.csv
# 13B_tokens_and_decimals_for_wCelo_replacement_candidate.csv
#
# HOW TO RUN
# ----------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_additional_pool_wcelo_step_1_check_inputs.R")

# CONSISTENCY WITH THE COMPLETED ECO-USDT CHECKPOINT
# -------------------------------------------------
# This is a pool-specific copy of the ECO-USDT Step 1 script. Its validation
# raw-swap checks are unchanged. Metadata loading now reads Query 13 for the
# suspicious pool and Query 13B for the replacement control, without editing
# either source CSV. Both metadata sources are checked and fingerprinted.
# Only filenames, pool/token identities, expected row
# counts, observation dates, and descriptive labels have been replaced.
# The observation dates below use UTC, the time standard in the raw exports.
# Both pools are checked against the SAME wCELO observation window. The control
# may have a shorter period of actual trading inside that window, as before.
#
# OUTPUT INVENTORY
# ----------------
# No output CSV is created by Step 1. The checks and input fingerprints are
# printed in the R console. Copy the complete console output for review.
# NEXT STEP: after Step 1 passes, prepare the two token-flow datasets in Step 2.
#
local({

  # ---------------------------------------------------------------------------
  # 1. Fixed project paths and expected sample information
  # ---------------------------------------------------------------------------
  base_dir <- "D:/Downloads/Bachelor Thesis"
  raw_dir <- file.path(base_dir, "Data", "raw")
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")

  metadata_filename <- "13_tokens_and_decimals_for_additional_pools.csv"
  metadata_path <- file.path(raw_dir, metadata_filename)
  replacement_metadata_filename <- "13B_tokens_and_decimals_for_wCelo_replacement_candidate.csv"
  replacement_metadata_path <- file.path(raw_dir, replacement_metadata_filename)

  samples <- data.frame(
    sample = c("suspicious_pool", "control_pool"),
    filename = c(
      "12C_wcelo_suspicious_pool_raw_swaps.csv",
      "12E_wcelo_replacement_candidate_rank2_raw_swaps.csv"
    ),
    expected_pool = c(
      "0x8d1ca95559abed542ef4402a252974221b5e1036",
      "0xfe7444f63e0010a7208529608eb173760629483b"
    ),
    expected_rows = c(11417L, 6459L),
    observation_start = c(
      "2020-12-17 21:19:44",
      "2020-12-17 21:19:44"
    ),
    observation_end = c(
      "2022-07-25 22:08:54",
      "2022-07-25 22:08:54"
    ),
    stringsAsFactors = FALSE
  )

  samples$path <- file.path(raw_dir, samples$filename)

  paper_start_block <- 10000835
  paper_end_block <- 15400000

  expected_columns <- c(
    "contract_address",
    "evt_tx_hash",
    "evt_tx_from",
    "evt_tx_to",
    "evt_tx_index",
    "evt_index",
    "evt_block_time",
    "evt_block_number",
    "evt_block_date",
    "amount0In",
    "amount0Out",
    "amount1In",
    "amount1Out",
    "sender",
    "to"
  )

  # evt_tx_index is included in the expected Dune schema, but historical Dune
  # exports may contain <nil> in that field. It is not needed for event ordering;
  # block number and event log index provide the ordering used in the prior work.
  essential_columns <- setdiff(expected_columns, "evt_tx_index")

  cat("AMM ADAPTATION -- wCELO ADDITIONAL POOL -- STEP 1\n\n")
  cat("R version:", R.version.string, "\n")
  cat("Raw-data directory:", raw_dir, "\n")
  cat("Cleaned-data directory:", cleaned_dir, "\n\n")

  # ---------------------------------------------------------------------------
  # 2. Helpers used only for validation and reporting
  # ---------------------------------------------------------------------------
  checks <- list()

  add_check <- function(sample, check, pass, detail) {
    pass <- isTRUE(pass)
    checks[[length(checks) + 1L]] <<- data.frame(
      sample = sample,
      check = check,
      result = if (pass) "PASS" else "FAIL",
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }

  normalize_text <- function(x) {
    tolower(trimws(as.character(x)))
  }

  is_hex_address <- function(x) {
    grepl("^0x[0-9a-f]{40}$", normalize_text(x))
  }

  # A top-level Ethereum transaction can have no recipient when it creates a
  # contract. Dune represents that legitimate case as <nil>. The Swap event
  # itself must still contain valid pool, sender, and recipient addresses.
  is_optional_tx_recipient <- function(x) {
    normalized <- normalize_text(x)
    normalized == "<nil>" | is_hex_address(normalized)
  }

  is_tx_hash <- function(x) {
    grepl("^0x[0-9a-f]{64}$", normalize_text(x))
  }

  parse_whole_numbers <- function(x) {
    value <- suppressWarnings(as.numeric(x))
    valid <- is.finite(value) & value >= 0 & value == floor(value)
    list(value = value, valid = valid)
  }

  # Raw token amounts can exceed the integer precision of ordinary R numbers.
  # We therefore validate their digit strings without converting them to numeric.
  is_unsigned_integer_text <- function(x) {
    !is.na(x) & grepl("^[0-9]+$", x)
  }

  is_positive_integer_text <- function(x) {
    is_unsigned_integer_text(x) & grepl("[1-9]", x)
  }

  parse_block_time <- function(x) {
    as.POSIXct(
      sub(" UTC$", "", x),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Confirm that the expected files and directories exist
  # ---------------------------------------------------------------------------
  add_check(
    "both_pools",
    "raw directory exists",
    dir.exists(raw_dir),
    raw_dir
  )

  add_check(
    "both_pools",
    "cleaned directory exists",
    dir.exists(cleaned_dir),
    cleaned_dir
  )

  for (i in seq_len(nrow(samples))) {
    add_check(
      samples$sample[i],
      "raw CSV exists",
      file.exists(samples$path[i]),
      samples$filename[i]
    )
  }

  add_check(
    "both_pools",
    "Query 13 metadata CSV exists",
    file.exists(metadata_path),
    metadata_filename
  )

  # The new control has its own metadata export; preserve the original file.
  add_check("both_pools", "Query 13B metadata CSV exists",
            file.exists(replacement_metadata_path), replacement_metadata_filename)

  required_paths <- c(samples$path, metadata_path, replacement_metadata_path)
  if (!dir.exists(raw_dir) || !all(file.exists(required_paths))) {
    check_table <- do.call(rbind, checks)
    print(check_table, row.names = FALSE)
    stop("Required wCELO input files are missing. No detection was run.",
         call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 4. Read and validate each raw Swap-event CSV
  # ---------------------------------------------------------------------------
  input_md5 <- unname(tools::md5sum(required_paths))
  names(input_md5) <- basename(required_paths)

  for (i in seq_len(nrow(samples))) {
    sample_name <- samples$sample[i]

    x <- utils::read.csv(
      samples$path[i],
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    )

    missing_columns <- setdiff(expected_columns, names(x))
    unexpected_columns <- setdiff(names(x), expected_columns)
    repeated_columns <- unique(names(x)[duplicated(names(x))])

    add_check(
      sample_name,
      "expected 15-column schema",
      !length(missing_columns) &&
        !length(unexpected_columns) &&
        !length(repeated_columns) &&
        identical(names(x), expected_columns),
      paste0(
        "found=", ncol(x),
        "; missing=", if (length(missing_columns)) paste(missing_columns, collapse = ",") else "none",
        "; unexpected=", if (length(unexpected_columns)) paste(unexpected_columns, collapse = ",") else "none",
        "; repeated=", if (length(repeated_columns)) paste(repeated_columns, collapse = ",") else "none"
      )
    )

    if (length(missing_columns)) {
      next
    }

    add_check(
      sample_name,
      "expected number of Swap-event rows",
      nrow(x) == samples$expected_rows[i],
      paste0("found=", nrow(x), "; expected=", samples$expected_rows[i])
    )

    missing_essential <- rowSums(is.na(x[, essential_columns, drop = FALSE])) > 0L
    add_check(
      sample_name,
      "no missing essential values",
      !any(missing_essential),
      paste0("rows_with_missing_essential_values=", sum(missing_essential))
    )

    pool_values <- unique(normalize_text(x$contract_address))
    add_check(
      sample_name,
      "one expected pool address",
      length(pool_values) == 1L && pool_values == samples$expected_pool[i],
      paste(pool_values, collapse = ",")
    )

    add_check(
      sample_name,
      "valid Ethereum addresses",
      all(is_hex_address(x$contract_address)) &&
        all(is_hex_address(x$evt_tx_from)) &&
        all(is_optional_tx_recipient(x$evt_tx_to)) &&
        all(is_hex_address(x$sender)) &&
        all(is_hex_address(x$to)),
      paste0(
        "pool, transaction initiator, event sender and event recipient required; ",
        "transaction recipient may be <nil> for contract creation"
      )
    )

    add_check(
      sample_name,
      "valid transaction hashes",
      all(is_tx_hash(x$evt_tx_hash)),
      paste0("rows=", nrow(x))
    )

    block_check <- parse_whole_numbers(x$evt_block_number)
    log_check <- parse_whole_numbers(x$evt_index)

    valid_order_fields <- all(block_check$valid) && all(log_check$valid)
    add_check(
      sample_name,
      "valid whole-number block and log indices",
      valid_order_fields,
      paste0(
        "invalid_blocks=", sum(!block_check$valid),
        "; invalid_log_indices=", sum(!log_check$valid)
      )
    )

    if (valid_order_fields) {
      block_number <- block_check$value
      log_index <- log_check$value

      add_check(
        sample_name,
        "inside AMM-paper block range",
        all(block_number >= paper_start_block & block_number <= paper_end_block),
        paste0(
          "observed=", format(min(block_number), scientific = FALSE),
          "..", format(max(block_number), scientific = FALSE),
          "; permitted=", paper_start_block, "..", paper_end_block
        )
      )

      chronological_order <- order(block_number, log_index)
      add_check(
        sample_name,
        "chronological row ordering",
        identical(chronological_order, seq_len(nrow(x))),
        "ordered by block number and event log index"
      )
    }

    block_time <- parse_block_time(x$evt_block_time)
    observation_start <- as.POSIXct(
      samples$observation_start[i], tz = "UTC"
    )
    observation_end <- as.POSIXct(
      samples$observation_end[i], tz = "UTC"
    )

    valid_times <- !is.na(block_time)
    inside_window <- valid_times &
      block_time >= observation_start &
      block_time <= observation_end

    add_check(
      sample_name,
      "inside wCELO observation window",
      all(valid_times) && all(inside_window),
      paste0(
        "invalid_times=", sum(!valid_times),
        "; outside_window=", sum(valid_times & !inside_window)
      )
    )

    event_key <- paste(
      normalize_text(x$contract_address),
      normalize_text(x$evt_tx_hash),
      x$evt_index,
      sep = "|"
    )
    duplicate_events <- duplicated(event_key)

    add_check(
      sample_name,
      "unique Swap-event identifiers",
      !any(duplicate_events),
      paste0("duplicate_rows=", sum(duplicate_events))
    )

    amount_columns <- c("amount0In", "amount0Out", "amount1In", "amount1Out")
    valid_amount_text <- vapply(
      x[, amount_columns, drop = FALSE],
      function(values) all(is_unsigned_integer_text(values)),
      logical(1)
    )

    add_check(
      sample_name,
      "raw amounts are nonnegative integer strings",
      all(valid_amount_text),
      paste(names(valid_amount_text)[!valid_amount_text], collapse = ",")
    )

    if (all(valid_amount_text)) {
      has_input <- is_positive_integer_text(x$amount0In) |
        is_positive_integer_text(x$amount1In)
      has_output <- is_positive_integer_text(x$amount0Out) |
        is_positive_integer_text(x$amount1Out)

      add_check(
        sample_name,
        "each Swap event has input and output",
        all(has_input & has_output),
        paste0("invalid_rows=", sum(!(has_input & has_output)))
      )
    }

    cat("Checked:", sample_name, "--", samples$filename[i], "\n")
    cat("MD5:", input_md5[samples$filename[i]], "\n\n")
  }

  # ---------------------------------------------------------------------------
  # 5. Validate Query 13 token identities and decimals for this comparison
  # ---------------------------------------------------------------------------
  metadata <- utils::read.csv(
    metadata_path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  # Read the replacement metadata as text, just like the original metadata.
  replacement_metadata <- utils::read.csv(
    replacement_metadata_path, colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = c("", "NA")
  )

  metadata_required <- c(
    "comparison_name",
    "pool_role",
    "pool_address",
    "token0_address",
    "token0_symbol",
    "token0_decimals",
    "token1_address",
    "token1_symbol",
    "token1_decimals"
  )

  add_check(
    "both_pools",
    "Query 13 metadata schema",
    all(metadata_required %in% names(metadata)) && !anyDuplicated(names(metadata)),
    paste0("rows=", nrow(metadata), "; columns=", ncol(metadata))
  )

  add_check(
    "both_pools", "Query 13B metadata schema",
    all(metadata_required %in% names(replacement_metadata)) &&
      !anyDuplicated(names(replacement_metadata)),
    paste0("rows=", nrow(replacement_metadata), "; columns=", ncol(replacement_metadata))
  )

  if (all(metadata_required %in% names(metadata)) &&
      all(metadata_required %in% names(replacement_metadata))) {
    # Keep the suspicious pool's original metadata. Use every row in 13B,
    # so extra, duplicate or incorrect replacement rows fail the exact check.
    # The former PDOG control row is left intact in the original source CSV.
    wcelo_metadata <- rbind(
      metadata[
        metadata$comparison_name == "wCELO comparison" &
          metadata$pool_role == "suspicious",
        metadata_required, drop = FALSE
      ],
      replacement_metadata[, metadata_required, drop = FALSE]
    )

    expected_metadata <- data.frame(
      comparison_name = c("wCELO comparison", "wCELO comparison"),
      pool_role = c("suspicious", "control"),
      pool_address = samples$expected_pool,
      token0_address = c(
        "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2",
        "0x1c9922314ed1415c95b9fd453c3818fd41867d0b"
      ),
      token0_symbol = c("WETH", "TOWER"),
      token0_decimals = c("18", "18"),
      token1_address = c(
        "0xe452e6ea2ddeb012e20db73bf5d3863a3ac8d77a",
        "0x557b933a7c2c45672b610f8954a3deb39a51a8ca"
      ),
      token1_symbol = c("wCELO", "REVV"),
      token1_decimals = c("18", "18"),
      stringsAsFactors = FALSE
    )

    normalize_metadata <- function(x) {
      x$comparison_name <- trimws(x$comparison_name)
      x$pool_role <- normalize_text(x$pool_role)
      x$pool_address <- normalize_text(x$pool_address)
      x$token0_address <- normalize_text(x$token0_address)
      x$token1_address <- normalize_text(x$token1_address)
      x$token0_symbol <- trimws(x$token0_symbol)
      x$token1_symbol <- trimws(x$token1_symbol)
      x$token0_decimals <- trimws(x$token0_decimals)
      x$token1_decimals <- trimws(x$token1_decimals)
      x[order(x$pool_role, decreasing = TRUE), , drop = FALSE]
    }

    wcelo_metadata <- normalize_metadata(wcelo_metadata)
    expected_metadata <- normalize_metadata(expected_metadata)
    rownames(wcelo_metadata) <- NULL
    rownames(expected_metadata) <- NULL

    add_check(
      "both_pools",
      "wCELO token identities and decimals",
      identical(wcelo_metadata, expected_metadata),
      "expected WETH(18)/wCELO(18) and TOWER(18)/REVV(18)"
    )
  }

  cat("Query 13 metadata MD5:", input_md5[metadata_filename], "\n")
  cat("Query 13B metadata MD5:", input_md5[replacement_metadata_filename], "\n\n")

  # ---------------------------------------------------------------------------
  # 6. Report later-stage R package availability
  # ---------------------------------------------------------------------------
  later_packages <- c("igraph", "data.table")
  package_available <- vapply(
    later_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )

  for (package_name in later_packages) {
    add_check(
      "environment",
      paste("R package available:", package_name),
      package_available[[package_name]],
      if (package_available[[package_name]]) "available" else "not installed"
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Final checkpoint decision
  # ---------------------------------------------------------------------------
  check_table <- do.call(rbind, checks)
  rownames(check_table) <- NULL

  cat("============================================================\n")
  cat("wCELO STEP-1 CHECK RESULTS\n")
  cat("============================================================\n\n")
  print(check_table, row.names = FALSE)

  failed <- check_table$result == "FAIL"

  cat("\nChecks passed:", sum(!failed), "of", nrow(check_table), "\n")
  cat("Checks failed:", sum(failed), "\n")

  if (any(failed)) {
    cat("\nFINAL STEP-1 STATUS: NEEDS REVIEW\n")
    stop(
      "wCELO input validation failed. No detection was run and no input was modified.",
      call. = FALSE
    )
  }

  cat("\nFINAL STEP-1 STATUS: PASS\n")
  cat("The inputs are ready for Step 2 token-flow preparation.\n")
  cat("This PASS is an input-integrity result, not a wash-trading classification.\n")
})
