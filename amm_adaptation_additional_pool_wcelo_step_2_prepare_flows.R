# ============================================================================
# AMM ADAPTATION -- ADDITIONAL POOL: wCELO -- STEP 2 PREPARE TOKEN FLOWS
# ============================================================================
#
# OUR PURPOSE
# -----------
# We convert our Uniswap V2 swap records into directed token movements. For
# each movement we record a source, a destination, and a quantity. This is the
# representation we will use when adapting our internship graph and balance
# calculations. We validate the row values before building this representation.
#
# WHAT WE DO IN THIS SCRIPT
# ----------------------------
# 1. We read the original CSV files as text and validate their essential fields.
# 2. We put swap events in chronological order using block and log indices.
# 3. We describe each ordinary swap as TWO movements, one for each token.
# 4. We retain the original event identifiers and exact raw amount strings.
# 5. We save the prepared tables and a preparation summary in Data/cleaned.
#
# WHAT WE HAVE NOT DONE AT THIS SCRIPT
# --------------------------------------
# We do not select trading windows, calculate SCCs, run volume matching, or
# label any event as suspicious. We do not compile volume_matching.cpp here.
# We do not infer common ownership, identify arbitrage, or inspect flash loans.
# Those tasks require later stages. No SQL query is needed for this checkpoint.
#
# OUR MODELING ASSUMPTION
# ----------------------
# We use evt_tx_from, the address that started the transaction, as a proxy for
# the trader. Swap-event sender/to fields can refer to routers. Our resulting
# edges represent swap flows ATTRIBUTED to the initiator; they are not a verified
# reconstruction of the actual ERC20 transfer recipients or ownership.
#
# EXAMPLE OF OUR CONVERSION -- ONE TOKEN AT A TIME
# ----------------------------------------------
# If address A receives 100 raw units of token X from the pool, our token-X row is:
# seller = pool, buyer = A, amount_raw = "100".
# If A sends 99 raw units of token X back, our token-X row is:
# seller = A, buyer = pool, amount_raw = "99".
# The other token exchanged in those swaps has its OWN rows and quantities.
# We must never combine different tokens in a single balance calculation.
#
# We retain the names seller/buyer for compatibility with the previously tested
# internship-style pipeline. Here they mean SOURCE/DESTINATION of the selected
# token. The pool is a distinct role, not a person or evidence that traders
# share ownership.
#
# OUR FILE LOCATIONS
# ------------------
# Script:
# D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_additional_pool_wcelo_step_2_prepare_flows.R
# Original inputs, with their existing names unchanged:
# D:/Downloads/Bachelor Thesis/Data/raw/12C_wcelo_suspicious_pool_raw_swaps.csv
# D:/Downloads/Bachelor Thesis/Data/raw/12E_wcelo_replacement_candidate_rank2_raw_swaps.csv
#
# Our three new CSVs are saved directly inside Data/cleaned:
# amm_adaptation_additional_pool_wcelo_input_suspicious_pool_token_flows.csv
# amm_adaptation_additional_pool_wcelo_input_control_pool_token_flows.csv
# amm_adaptation_additional_pool_wcelo_output_both_pools_preparation_summary.csv
#
# The two flow tables are OUTPUTS of this preparation step and INPUTS for the
# later detection step. Their names include "input" because their purpose is to
# supply the adapted detector. We keep those exact names in subsequent steps.
# The summary is a report about preparation, so its prefix is
# amm_adaptation_additional_pool_wcelo_output_. Original CSV names stay unchanged.
# Rerunning this script refreshes only these three named generated files.
#
# OUR QUANTITY UNITS
# ------------------
# Raw token quantities are integer counts in each token's smallest unit.
# We preserve these exact digit strings in amount_raw. They are not USD values.
# Token decimals are not supplied in these CSVs; we do not guess them.
# For later numerical calculations, we also compute amount, a positive number
# equal to amount_raw divided by the largest raw movement of that SAME token
# in that SAME pool sample. scale_raw records that divisor as an exact string.
# For example, raw movements 100 and 99 become amounts 1 and 0.99.
# This rescaling is not token-decimal normalization. In a balance/mean ratio,
# the same positive divisor cancels from numerator and denominator. It therefore
# preserves the mathematical criterion for one token. It does not justify
# summing amounts across tokens. R uses approximate arithmetic for these ratios;
# we retain exact raw strings to support later numerical verification.
#
# HOW WE RUN THE WHOLE SCRIPT IN OUR R CONSOLE
# ------------------------------------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_additional_pool_wcelo_step_2_prepare_flows.R")
# We review the complete console output before proceeding to the next script.
#
# HOW WE READ THIS CODE
# --------------------
# Comments start with #. R does not execute them. <- stores a value under a name.
# A function is a reusable operation. Parentheses contain its inputs, separated
# by commas. Curly braces group instructions. TRUE/FALSE mean yes/no. NA means
# missing. None of the checks below is a wash-trading classification.

# CONTROL REPLACEMENT CHECKPOINT -- 5 October 2026
# The original PDOG-WETH sample stopped at the ordinary-swap check (33 events).
# The second-ranked Query 11 candidate is now used: TOWER-REVV, 6,459 swaps.
# Its raw export passed the same ordinary-swap pattern check in file review.
# This script repeats that check and still stops if any event is incompatible.
# The original 12D export remains unchanged. The detailed explanation of the
# replacement and its 43.43% swap-count difference is deferred for the report.
# No rows are dropped or netted, and no token-flow rule is changed.
#
# CONSISTENCY NOTE
# This copies the completed ECO-USDT Step 2. Only the pool identifiers,
# expected input sizes, filenames, and descriptive comments/messages differ.
# Every token-flow transformation and validation operation is preserved.
# The obsolete comment about a C++ matcher is clarified: this branch uses
# the WTEYE balance calculation, not volume_matching.cpp.
# NEXT STEP: review these three outputs, then run Step 3 balance detection.

local({
  # ========================================================================
  # 1. WE SET OUR EXISTING FOLDER PATHS AND EXACT FILENAMES
  # ========================================================================
  # local() keeps our temporary variables out of the main R workspace.
  # file.path() joins path components without changing any file or folder name.
  base_dir <- "D:/Downloads/Bachelor Thesis"
  raw_dir <- file.path(base_dir, "Data", "raw")
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")

  # c() combines values. The names on the left identify our two samples.
  input_files <- c(
    suspicious_pool = "12C_wcelo_suspicious_pool_raw_swaps.csv",
    control_pool = "12E_wcelo_replacement_candidate_rank2_raw_swaps.csv"
  )
  expected_rows <- c(suspicious_pool = 11417L, control_pool = 6459L)
  expected_pools <- c(
    suspicious_pool = "0x8d1ca95559abed542ef4402a252974221b5e1036",
    control_pool = "0xfe7444f63e0010a7208529608eb173760629483b"
  )
  flow_files <- c(
    suspicious_pool = "amm_adaptation_additional_pool_wcelo_input_suspicious_pool_token_flows.csv",
    control_pool = "amm_adaptation_additional_pool_wcelo_input_control_pool_token_flows.csv"
  )
  summary_file <- "amm_adaptation_additional_pool_wcelo_output_both_pools_preparation_summary.csv"

  # cat() prints messages; \n starts a new line in a printed message.
  cat("AMM ADAPTATION -- wCELO ADDITIONAL POOL -- STEP 2: PREPARE TOKEN FLOWS\n\n")
  cat("Original inputs:", raw_dir, "\nPrepared files:", cleaned_dir, "\n")

  # We require our original data folder. We create only the agreed cleaned
  # folder if it is missing. Neither step changes our original CSV files.
  if (!dir.exists(raw_dir)) stop("Data/raw is missing. Send the console output.", call. = FALSE)
  if (!dir.exists(cleaned_dir)) {
    dir.create(cleaned_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(cleaned_dir)) stop("Data/cleaned is unavailable.", call. = FALSE)

  input_paths <- file.path(raw_dir, unname(input_files))
  names(input_paths) <- names(input_files)
  if (!all(file.exists(input_paths))) {
    stop("An original input CSV is missing. Keep the agreed filenames in Data/raw.", call. = FALSE)
  }

  # MD5 fingerprints identify the bytes used for this run. Comparing before
  # and after reading checks whether the source files changed during preparation.
  # This is not a claim that a fingerprint proves the authenticity of the data.
  input_md5_before <- unname(tools::md5sum(input_paths))

  # ========================================================================
  # 2. WE DEFINE SMALL, REUSABLE VALIDATION OPERATIONS
  # ========================================================================
  # function(...) defines an operation. We call each helper for both samples.
  # stop() ends the script with a specific explanation rather than guessing a fix.
  fail <- function(sample_name, message) {
    stop(paste0(sample_name, ": ", message), call. = FALSE)
  }

  # We require Ethereum addresses/hashes to have the expected hexadecimal form.
  # grepl() asks whether text matches a pattern. ^ and $ mean start/end; {40}
  # means exactly 40 characters. We lowercase/trim only our in-memory copy.
  check_hex <- function(values, digits, field, sample_name) {
    normalized <- tolower(trimws(values))
    pattern <- paste0("^0x[0-9a-f]{", digits, "}$")
    bad <- is.na(normalized) | !grepl(pattern, normalized)
    if (any(bad)) fail(sample_name, paste("Invalid or missing", field))
    normalized
  }

  # Our exports sometimes express block numbers as scientific notation:
  # 1.1374805e+07 means 11374805. We accept this for block/log indices.
  # as.numeric() converts the text; floor() removes a fractional part. We reject
  # missing, infinite, negative, fractional, or out-of-range values.
  # The upper bound below is R's ordinary signed-integer limit. Our samples are
  # far below it. The explicit limit prevents unsafe conversion to an integer.
  check_index <- function(values, field, sample_name) {
    numbers <- suppressWarnings(as.numeric(values))
    bad <- !is.finite(numbers) | numbers < 0 | numbers != floor(numbers) |
      numbers > .Machine$integer.max
    if (any(bad)) fail(sample_name, paste("Invalid or unsupported", field))
    as.integer(numbers)
  }

  # Raw amounts must be digit strings, such as "1000000000000000000".
  # We do not accept scientific notation here: its digits may already be rounded.
  # This differs deliberately from how we read the much smaller block indices.
  check_amount_text <- function(values, field, sample_name) {
    if (any(is.na(values) | !grepl("^[0-9]+$", values))) {
      fail(sample_name, paste("Missing or non-integer raw amount in", field))
    }
    # Conversion is only for calculations. The exact strings remain in the table.
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers))) fail(sample_name, paste("Non-finite amount in", field))
    numbers
  }

  # ========================================================================
  # 3. WE READ, VALIDATE, AND ORDER ONE POOL'S SWAP RECORDS
  # ========================================================================
  prepare_sample <- function(path, sample_name) {
    raw <- utils::read.csv(
      path, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("", "NA", "<nil>")
    )
    # All source values initially stay as text. We never resave this raw table.
    # We retain the source row number to trace a sorted event to its original row.
    raw$source_row <- seq_len(nrow(raw))
    required <- c(
      "contract_address", "evt_tx_hash", "evt_tx_from", "evt_index",
      "evt_block_number", "amount0In", "amount0Out", "amount1In", "amount1Out"
    )
    if (nrow(raw) == 0L) fail(sample_name, "The input is empty.")
    if (nrow(raw) != expected_rows[[sample_name]]) {
      fail(
        sample_name,
        paste0(
          "Expected ", expected_rows[[sample_name]],
          " Swap-event rows from the passed Step-1 input, but found ", nrow(raw), "."
        )
      )
    }
    if (anyDuplicated(names(raw))) fail(sample_name, "Duplicate column headings.")
    absent <- setdiff(required, names(raw))
    if (length(absent)) fail(sample_name, paste("Missing columns:", paste(absent, collapse = ", ")))

    raw$pool_address <- check_hex(raw$contract_address, 40, "contract_address", sample_name)
    raw$initiator_address <- check_hex(raw$evt_tx_from, 40, "evt_tx_from", sample_name)
    raw$transaction_hash <- check_hex(raw$evt_tx_hash, 64, "evt_tx_hash", sample_name)
    raw$block_number <- check_index(raw$evt_block_number, "evt_block_number", sample_name)
    raw$log_index <- check_index(raw$evt_index, "evt_index", sample_name)

    # unique() removes repetitions from a list used for checking. It does not
    # remove swap rows. We require exactly one pool in each original input file.
    if (length(unique(raw$pool_address)) != 1L) fail(sample_name, "Expected one pool per input file.")
    if (unique(raw$pool_address) != expected_pools[[sample_name]]) {
      fail(sample_name, "The pool address differs from the passed Step-1 input.")
    }
    if (any(raw$pool_address == raw$initiator_address)) {
      fail(sample_name, "Initiator equals pool; this requires separate inspection.")
    }

    # A transaction can contain several swap events. We therefore identify an
    # event by pool, transaction hash, and log index, joined using a colon.
    raw$event_id <- paste(raw$pool_address, raw$transaction_hash, raw$log_index, sep = ":")
    if (anyDuplicated(raw$event_id)) {
      fail(sample_name, "Repeated event IDs. We must review duplicates before counting flows.")
    }

    # Our Dune export uses a block-global log index. The (block, log) pair must
    # be unique. A missing evt_tx_index is not replaced by an invented value.
    if (anyDuplicated(paste(raw$block_number, raw$log_index, sep = ":"))) {
      fail(sample_name, "Repeated block/log pairs; our ordering assumption needs review.")
    }
    order_rows <- order(raw$block_number, raw$log_index)
    raw <- raw[order_rows, , drop = FALSE]
    rownames(raw) <- NULL
    raw$swap_index <- seq_len(nrow(raw))

    # We keep the original transaction index as context. NA means unavailable.
    # We use block and log indices for ordering, so this field may be missing.
    if (!("evt_tx_index" %in% names(raw))) raw$evt_tx_index <- NA_character_

    # We validate the four amount fields. The original text columns are retained.
    token0_in <- check_amount_text(raw$amount0In, "amount0In", sample_name)
    token0_out <- check_amount_text(raw$amount0Out, "amount0Out", sample_name)
    token1_in <- check_amount_text(raw$amount1In, "amount1In", sample_name)
    token1_out <- check_amount_text(raw$amount1Out, "amount1Out", sample_name)

    # An ordinary two-token swap follows one of two patterns:
    # (a) token0 enters while token1 leaves; or (b) token1 enters while token0 leaves.
    # & means AND for each row; | means OR. == asks whether values are equal.
    ordinary <-
      (token0_in > 0 & token0_out == 0 & token1_in == 0 & token1_out > 0) |
      (token0_in == 0 & token0_out > 0 & token1_in > 0 & token1_out == 0)
    if (any(!ordinary)) {
      fail(sample_name, paste0(
        "Nonstandard swap pattern at original row(s): ",
        paste(raw$source_row[!ordinary], collapse = ", "),
        ". We must inspect these before assigning ordinary swap flows."
      ))
    }

    # ======================================================================
    # 4. WE CONVERT EACH TOKEN'S MOVEMENTS INTO SOURCE/DESTINATION ROWS
    # ======================================================================
    # list() holds the two token tables separately while we construct them.
    token_tables <- list()
    scale_strings <- character(2)
    for (slot in 0:1) {
      # paste0() joins text without a separator: for example, "amount0In".
      # [[column_name]] retrieves that column from our data table.
      in_text <- raw[[paste0("amount", slot, "In")]]
      out_text <- raw[[paste0("amount", slot, "Out")]]
      enters_pool <- as.numeric(in_text) > 0

      # ifelse() chooses a value for each row. Only one of In/Out is positive,
      # as validated above. We preserve the chosen raw amount as an exact string.
      amount_raw <- ifelse(enters_pool, in_text, out_text)
      amount_approx <- as.numeric(amount_raw)

      # We rescale within THIS token and THIS sample only. which.max() identifies
      # a largest movement under R's approximate numeric representation.
      # scale_raw preserves the exact source string selected as our divisor.
      scale_raw <- amount_raw[which.max(amount_approx)]
      scale_strings[slot + 1L] <- scale_raw
      amount_scaled <- amount_approx / as.numeric(scale_raw)
      if (any(!is.finite(amount_scaled) | amount_scaled <= 0)) {
        fail(sample_name, "Invalid rescaled amount; numerical review is needed.")
      }

      # We assign the direction from the pool's perspective, separately by token.
      source <- ifelse(enters_pool, raw$initiator_address, raw$pool_address)
      destination <- ifelse(enters_pool, raw$pool_address, raw$initiator_address)

      # data.frame() creates a table in memory. Each row is ONE token movement.
      # event_id and swap_index repeat once for each token, which is intentional.
      # They preserve the link between these two movements and one original swap.
      # row_id carries our chronological swap index into the later matching step.
      token_tables[[slot + 1L]] <- data.frame(
        sample = sample_name,
        source_file = basename(path),
        source_row = raw$source_row,
        event_id = raw$event_id,
        swap_index = raw$swap_index,
        row_id = raw$swap_index,
        pool_address = raw$pool_address,
        initiator_address = raw$initiator_address,
        transaction_hash = raw$transaction_hash,
        block_number = raw$block_number,
        log_index = raw$log_index,
        transaction_index_original = raw$evt_tx_index,
        token_slot = paste0("token", slot),
        direction = ifelse(enters_pool, "INTO_POOL", "OUT_OF_POOL"),
        seller = source,
        buyer = destination,
        amount_raw = amount_raw,
        scale_raw = scale_raw,
        amount = amount_scaled,
        stringsAsFactors = FALSE
      )
    }

    # rbind joins the two tables. Sorting keeps both token movements together
    # for inspection. In detection we MUST separate each pool and token_slot
    # before applying the WTEYE balance calculation. Different tokens must
    # never be combined in a single balance calculation.
    flows <- do.call(rbind, token_tables)
    flows <- flows[order(flows$swap_index, flows$token_slot), , drop = FALSE]
    rownames(flows) <- NULL

    # These are transformation checks, not wash-trading tests. Each swap must
    # produce exactly one token0 row and one token1 row, with no repeated token leg.
    if (nrow(flows) != 2L * nrow(raw) ||
        sum(flows$token_slot == "token0") != nrow(raw) ||
        sum(flows$token_slot == "token1") != nrow(raw) ||
        anyDuplicated(paste(flows$event_id, flows$token_slot, sep = ":"))) {
      fail(sample_name, "The event-to-token-row mapping failed its consistency check.")
    }

    # We record preparation counts. More token rows do not mean more transactions:
    # one ordinary swap has two token rows, and one transaction may have many swaps.
    summary <- data.frame(
      sample = sample_name,
      source_file = basename(path),
      input_md5 = unname(tools::md5sum(path)),
      pool_address = raw$pool_address[1L],
      input_swap_events = nrow(raw),
      unique_transactions = length(unique(raw$transaction_hash)),
      unique_initiators = length(unique(raw$initiator_address)),
      token_flow_rows = nrow(flows),
      token0_rows = sum(flows$token_slot == "token0"),
      token1_rows = sum(flows$token_slot == "token1"),
      first_block = min(raw$block_number),
      last_block = max(raw$block_number),
      missing_transaction_indices = sum(is.na(raw$evt_tx_index)),
      token0_scale_raw = scale_strings[1L],
      token1_scale_raw = scale_strings[2L],
      quantity_units = "amount_raw: integer base units; amount: per-token relative scale",
      address_attribution = "TRANSACTION_INITIATOR_PROXY",
      detection_status = "NOT_RUN",
      stringsAsFactors = FALSE
    )
    list(flows = flows, summary = summary)
  }

  # ========================================================================
  # 5. WE PREPARE BOTH SAMPLES BEFORE SAVING ANY GENERATED CSV
  # ========================================================================
  prepared <- list()
  for (sample_name in names(input_files)) {
    cat("\nPreparing:", input_files[[sample_name]], "\n")
    prepared[[sample_name]] <- prepare_sample(input_paths[[sample_name]], sample_name)
    # We print a short progress table. The complete details go into our summary.
    print(prepared[[sample_name]]$summary[, c(
      "sample", "input_swap_events", "unique_transactions", "token_flow_rows",
      "token0_rows", "token1_rows"
    )], row.names = FALSE)
  }

  input_md5_after <- unname(tools::md5sum(input_paths))
  if (!identical(input_md5_before, input_md5_after)) {
    stop("A raw input changed during this run. Review the input files before continuing.", call. = FALSE)
  }

  # ========================================================================
  # 6. WE SAVE PREPARED DETECTOR INPUTS AND OUR PREPARATION SUMMARY
  # ========================================================================
  # CSV does not store R column types. We write approximate numeric amounts with
  # 17 significant digits so ordinary binary double values can be read back
  # without losing digits through a shorter default display format.
  # This does NOT make approximate arithmetic exact. amount_raw remains the exact
  # integer text. Our later reader must read identifiers/raw amounts as text and
  # explicitly convert only the computational amount to a number.
  write_prepared_csv <- function(table, filename) {
    table_for_disk <- table
    for (column_name in names(table_for_disk)) {
      if (is.double(table_for_disk[[column_name]])) {
        table_for_disk[[column_name]] <- sprintf("%.17g", table_for_disk[[column_name]])
      }
    }
    path <- file.path(cleaned_dir, filename)
    if (file.exists(path)) cat("Refreshing our generated file:", filename, "\n")
    utils::write.csv(table_for_disk, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    cat("Saved:", path, "\n")
  }

  for (sample_name in names(flow_files)) {
    write_prepared_csv(prepared[[sample_name]]$flows, flow_files[[sample_name]])
  }
  summary_rows <- lapply(prepared, function(result) result$summary)
  both_summaries <- do.call(rbind, summary_rows)
  rownames(both_summaries) <- NULL
  write_prepared_csv(both_summaries, summary_file)

  # We stop at this preparation checkpoint. The next script will read the flow
  # tables, split the two tokens, form short windows and SCCs, and apply the
  # internship-report/WTEYE-style balance equation. No candidate label is
  # created here because no detection has yet been performed.
  cat("\nSTEP 2 COMPLETE: both samples were validated and converted to token flows.\n")
  cat("Raw input fingerprints unchanged:", identical(input_md5_before, input_md5_after), "\n")
  cat("Detection status: NOT RUN. Token-flow rows are prepared data, not detections.\n")
  cat("Send the complete console output and all three generated CSVs for review.\n")
  cat("Next step: after review, Step 3 applies the WTEYE balance calculation.\n")
})
