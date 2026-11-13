# ============================================================================
# AMM ADAPTATION -- STEP 4B -- INTERNSHIP REPORT / WTEYE: REVIEW CANDIDATES
# ============================================================================
#
# OUR PURPOSE
# -----------
# This is the WTEYE/internship-report counterpart of the existing C++-branch
# script:
#
#   amm_adaptation_step_4_review_candidates.R
#
# Step 3 has already detected candidate groups. This Step 4 does NOT search for
# new candidates and does NOT change the Step-3 settings. Instead, it turns the
# validated Step-3 results into tables that we can inspect and explain.
#
# For each candidate group, we:
# - reconstruct the original swaps from Step 2;
# - calculate the internship-report / WTEYE balance ratio for BOTH tokens;
# - record whether token0, token1, or both pass the 10% condition;
# - record whether the included swap indices are consecutive;
# - list every original swap belonging to the candidate group;
# - list address-level received/sent/net balances for both tokens;
# - list swaps inside the recorded Step-3 source window that are not part of the
#   final SCC event set;
# - reproduce the Step-3 candidate counts before saving our reports.
#
# IMPORTANT
# ---------
# We already ran a separate technical-validation checkpoint and it PASSED.
# This script reads that PASS result as a gate. We do not need to rerun Step 3
# or the earlier validation script before running this review.
#
# BALANCE CONDITION USED IN THIS BRANCH
# -------------------------------------
# For one candidate event set and one token:
#
#   balance_i = amount received by address i - amount sent by address i
#
#   L1_balance = sum_i |balance_i|
#
#   V_total = sum of all token-movement amounts
#
#   balance_ratio = L1_balance / (2 * V_total)
#
# A token passes when:
#
#   balance_ratio < 0.10
#
# We calculate token0 and token1 separately. We never add quantities from
# different tokens.
#
# INTERPRETATION
# --------------
# Passing this balance condition means the event set is balance-neutral under our
# adapted structural model. It does NOT prove wash trading, common ownership,
# collusion, manipulative intent, or the absence of arbitrage/MEV.
#
# SCRIPT LOCATION
# ---------------
# D:/Downloads/Bachelor Thesis/Scripts/
# amm_adaptation_step_4b_internship_report_review_candidates.R
#
# REQUIRED INPUTS IN Data/cleaned
# -------------------------------
# Step-2 prepared inputs:
#   amm_adaptation_input_suspicious_pool_token_flows.csv
#   amm_adaptation_input_control_pool_token_flows.csv
#
# Step-3 WTEYE-style reports:
#   amm_adaptation_output_suspicious_pool_internship_report_wteye_token_matches.csv
#   amm_adaptation_output_suspicious_pool_internship_report_wteye_swaps_labeled.csv
#   amm_adaptation_output_control_pool_internship_report_wteye_token_matches.csv
#   amm_adaptation_output_control_pool_internship_report_wteye_swaps_labeled.csv
#   amm_adaptation_output_both_pools_internship_report_wteye_detection_summary.csv
#
# Technical-validation gate already produced:
#   amm_adaptation_output_both_pools_internship_report_wteye_validation_summary.csv
#
# OUR FIVE STEP-4B OUTPUTS
# -----------------------
# amm_adaptation_output_both_pools_internship_report_wteye_candidate_group_review.csv
# amm_adaptation_output_both_pools_internship_report_wteye_candidate_group_members.csv
# amm_adaptation_output_both_pools_internship_report_wteye_candidate_address_balances.csv
# amm_adaptation_output_both_pools_internship_report_wteye_candidate_window_exclusions.csv
# amm_adaptation_output_both_pools_internship_report_wteye_candidate_review_summary.csv
#
# HOW WE RUN IT
# -------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_4b_internship_report_review_candidates.R")

local({
  # ========================================================================
  # 1. PATHS, FILENAMES, AND THE DOCUMENTED BALANCE MARGIN
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")
  margin <- 0.10

  flow_files <- setNames(
    paste0("amm_adaptation_input_", samples, "_token_flows.csv"),
    samples
  )

  match_files <- c(
    suspicious_pool =
      "amm_adaptation_output_suspicious_pool_internship_report_wteye_token_matches.csv",
    control_pool =
      "amm_adaptation_output_control_pool_internship_report_wteye_token_matches.csv"
  )

  labeled_files <- c(
    suspicious_pool =
      "amm_adaptation_output_suspicious_pool_internship_report_wteye_swaps_labeled.csv",
    control_pool =
      "amm_adaptation_output_control_pool_internship_report_wteye_swaps_labeled.csv"
  )

  detection_summary_file <-
    "amm_adaptation_output_both_pools_internship_report_wteye_detection_summary.csv"

  validation_summary_file <-
    "amm_adaptation_output_both_pools_internship_report_wteye_validation_summary.csv"

  output_files <- c(
    group_review =
      "amm_adaptation_output_both_pools_internship_report_wteye_candidate_group_review.csv",
    group_members =
      "amm_adaptation_output_both_pools_internship_report_wteye_candidate_group_members.csv",
    address_balances =
      "amm_adaptation_output_both_pools_internship_report_wteye_candidate_address_balances.csv",
    window_exclusions =
      "amm_adaptation_output_both_pools_internship_report_wteye_candidate_window_exclusions.csv",
    review_summary =
      "amm_adaptation_output_both_pools_internship_report_wteye_candidate_review_summary.csv"
  )

  flow_paths <- file.path(cleaned_dir, unname(flow_files))
  match_paths <- file.path(cleaned_dir, unname(match_files))
  labeled_paths <- file.path(cleaned_dir, unname(labeled_files))
  names(flow_paths) <- samples
  names(match_paths) <- samples
  names(labeled_paths) <- samples

  detection_summary_path <- file.path(cleaned_dir, detection_summary_file)
  validation_summary_path <- file.path(cleaned_dir, validation_summary_file)

  all_input_paths <- c(
    flow_paths,
    match_paths,
    labeled_paths,
    detection_summary_path,
    validation_summary_path
  )

  cat("AMM ADAPTATION -- STEP 4B -- INTERNSHIP REPORT / WTEYE: REVIEW CANDIDATES\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")

  if (!dir.exists(cleaned_dir) || !all(file.exists(all_input_paths))) {
    missing <- all_input_paths[!file.exists(all_input_paths)]
    stop(
      paste0(
        "A required Step-2, Step-3, or validation file is missing:\n",
        paste(missing, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  input_md5_before <- unname(tools::md5sum(all_input_paths))

  # ========================================================================
  # 2. SMALL SAFE-READING HELPERS
  # ========================================================================
  read_text_csv <- function(path) {
    utils::read.csv(
      path,
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  }

  require_columns <- function(table, columns, label) {
    missing <- setdiff(columns, names(table))
    if (length(missing)) {
      stop(
        paste(label, "is missing:", paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
  }

  as_index <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    bad <- !is.finite(numbers) |
      numbers < 0 |
      numbers != floor(numbers) |
      numbers > .Machine$integer.max

    if (any(bad)) {
      stop(paste("Invalid index in", field), call. = FALSE)
    }
    as.integer(numbers)
  }

  as_number <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers))) {
      stop(paste("Invalid number in", field), call. = FALSE)
    }
    numbers
  }

  as_yes_no <- function(values, field) {
    text <- toupper(trimws(as.character(values)))
    if (any(!(text %in% c("TRUE", "FALSE")))) {
      stop(paste("Invalid TRUE/FALSE in", field), call. = FALSE)
    }
    text == "TRUE"
  }

  join_values <- function(values) {
    values <- unique(as.character(values))
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) "" else paste(values, collapse = ";")
  }

  parse_event_key <- function(key) {
    ids <- suppressWarnings(
      as.integer(strsplit(key, ";", fixed = TRUE)[[1L]])
    )

    if (!length(ids) || anyNA(ids) ||
        any(ids <= 0L) || anyDuplicated(ids) ||
        !identical(ids, sort(ids))) {
      stop(paste("Invalid event_key:", key), call. = FALSE)
    }

    ids
  }

  bind_or_empty <- function(rows, empty_table) {
    if (!length(rows)) return(empty_table)
    answer <- do.call(rbind, rows)
    rownames(answer) <- NULL
    answer
  }

  # ========================================================================
  # 3. REQUIRE THE EARLIER TECHNICAL VALIDATION TO HAVE PASSED
  # ========================================================================
  validation <- read_text_csv(validation_summary_path)

  require_columns(
    validation,
    c(
      "sample", "overall_validation",
      "all_candidate_recalculations_pass",
      "summary_counts_match"
    ),
    validation_summary_file
  )

  if (!setequal(validation$sample, samples) ||
      any(validation$overall_validation != "PASS") ||
      any(toupper(validation$all_candidate_recalculations_pass) != "TRUE") ||
      any(toupper(validation$summary_counts_match) != "TRUE")) {
    stop(
      "The earlier WTEYE-style technical validation is not PASS for both samples.",
      call. = FALSE
    )
  }

  cat("PASS: earlier WTEYE-style technical validation is PASS for both samples.\n")

  # ========================================================================
  # 4. READ AND CHECK THE STEP-3 SETTINGS
  # ========================================================================
  detection_summary <- read_text_csv(detection_summary_path)

  require_columns(
    detection_summary,
    c(
      "sample", "margin", "max_swaps", "max_block_span",
      "candidate_groups", "candidate_swap_events",
      "candidate_rule", "balance_method",
      "scc_evaluation", "prepared_input_md5"
    ),
    detection_summary_file
  )

  if (!setequal(detection_summary$sample, samples)) {
    stop("Step-3 summary does not contain our two expected samples.",
         call. = FALSE)
  }

  margins <- as_number(
    detection_summary$margin,
    "detection-summary margin"
  )

  if (any(abs(margins - margin) > 1e-12) ||
      any(detection_summary$candidate_rule !=
            "AT_LEAST_ONE_TOKEN_MATCH") ||
      any(detection_summary$balance_method !=
            "L1_NET_BALANCE_DIVIDED_BY_2_TOTAL_VOLUME") ||
      any(detection_summary$scc_evaluation !=
            "WHOLE_SCC_INTERNAL_MOVEMENTS_NO_PREFIX_TRUNCATION")) {
    stop(
      "Step-3 settings differ from this Step-4 review's assumptions.",
      call. = FALSE
    )
  }

  cat(
    "Reviewing Step-3 settings: margin =", margin,
    "; max swaps =", join_values(detection_summary$max_swaps),
    "; max block span =", join_values(detection_summary$max_block_span),
    "\n"
  )

  # Confirm the current Step-2 inputs are exactly those recorded by Step 3.
  for (sample_name in samples) {
    row <- detection_summary[
      detection_summary$sample == sample_name,
      ,
      drop = FALSE
    ]

    actual_md5 <- unname(
      tools::md5sum(flow_paths[[sample_name]])
    )

    if (nrow(row) != 1L ||
        actual_md5 != row$prepared_input_md5) {
      stop(
        paste(
          sample_name,
          "Step-2 fingerprint differs from the file used in Step 3."
        ),
        call. = FALSE
      )
    }
  }

  cat("PASS: Step-2 fingerprints match the Step-3 records.\n")

  # ========================================================================
  # 5. EMPTY TABLE SHAPES FOR VALID ZERO-RESULT CASES
  # ========================================================================
  empty_group_review <- data.frame(
    sample = character(),
    candidate_group_id = character(),
    event_key = character(),
    included_swap_indices = character(),
    n_swap_events = integer(),
    n_transactions = integer(),
    n_initiators = integer(),
    initiator_addresses = character(),
    first_swap_index = integer(),
    last_swap_index = integer(),
    skipped_between_count = integer(),
    skipped_between_indices = character(),
    consecutive_group = logical(),
    first_block = integer(),
    last_block = integer(),
    block_span = integer(),
    matched_token0 = logical(),
    matched_token1 = logical(),
    token0_wteye_balance_ratio = numeric(),
    token1_wteye_balance_ratio = numeric(),
    token0_passes_margin = logical(),
    token1_passes_margin = logical(),
    both_tokens_pass = logical(),
    evidence_pattern = character(),
    adjacency_comparison = character(),
    interpretation = character(),
    confirmation_status = character(),
    stringsAsFactors = FALSE
  )

  empty_members <- data.frame(
    sample = character(),
    candidate_group_id = character(),
    position_in_group = integer(),
    swap_index = integer(),
    source_row = integer(),
    event_id = character(),
    transaction_hash = character(),
    initiator_address = character(),
    block_number = integer(),
    log_index = integer(),
    token0_direction = character(),
    token0_amount_raw = character(),
    token1_direction = character(),
    token1_amount_raw = character(),
    stringsAsFactors = FALSE
  )

  empty_balances <- data.frame(
    sample = character(),
    candidate_group_id = character(),
    token_slot = character(),
    address = character(),
    address_role = character(),
    total_received_scaled = numeric(),
    total_sent_scaled = numeric(),
    net_balance_scaled = numeric(),
    absolute_net_balance_scaled = numeric(),
    total_group_volume_scaled = numeric(),
    l1_balance_scaled = numeric(),
    wteye_balance_ratio = numeric(),
    margin = numeric(),
    whole_token_group_passes = logical(),
    stringsAsFactors = FALSE
  )

  empty_exclusions <- data.frame(
    sample = character(),
    candidate_group_id = character(),
    matched_token_slot = character(),
    source_window_start = integer(),
    source_window_end = integer(),
    excluded_swap_index = integer(),
    position_relative_to_matched_group = character(),
    transaction_hash = character(),
    initiator_address = character(),
    block_number = integer(),
    log_index = integer(),
    exclusion_stage = character(),
    explanation = character(),
    stringsAsFactors = FALSE
  )

  group_review_rows <- list()
  member_rows <- list()
  balance_rows <- list()
  exclusion_rows <- list()
  sample_summary_rows <- list()

  # ========================================================================
  # 6. REVIEW EVERY DISTINCT STEP-3 GROUP
  # ========================================================================
  for (sample_name in samples) {
    flows <- read_text_csv(flow_paths[[sample_name]])
    matches <- read_text_csv(match_paths[[sample_name]])
    labeled <- read_text_csv(labeled_paths[[sample_name]])

    require_columns(
      flows,
      c(
        "sample", "source_row", "event_id", "swap_index",
        "pool_address", "initiator_address", "transaction_hash",
        "block_number", "log_index", "token_slot", "direction",
        "seller", "buyer", "amount_raw", "scale_raw", "amount"
      ),
      flow_files[[sample_name]]
    )

    require_columns(
      matches,
      c(
        "candidate_group_id", "token_slot", "event_key",
        "source_window_start", "source_window_end",
        "wteye_balance_ratio", "margin"
      ),
      match_files[[sample_name]]
    )

    require_columns(
      labeled,
      c(
        "sample", "swap_index", "structural_candidate",
        "candidate_group_ids"
      ),
      labeled_files[[sample_name]]
    )

    # Convert only the fields used numerically.
    flows$source_row <- as_index(
      flows$source_row, "flow source_row"
    )
    flows$swap_index <- as_index(
      flows$swap_index, "flow swap_index"
    )
    flows$block_number <- as_index(
      flows$block_number, "flow block_number"
    )
    flows$log_index <- as_index(
      flows$log_index, "flow log_index"
    )
    flows$amount <- as_number(
      flows$amount, "flow amount"
    )

    if (nrow(matches)) {
      matches$source_window_start <- as_index(
        matches$source_window_start, "source_window_start"
      )
      matches$source_window_end <- as_index(
        matches$source_window_end, "source_window_end"
      )
      matches$wteye_balance_ratio <- as_number(
        matches$wteye_balance_ratio, "wteye_balance_ratio"
      )
      matches$margin <- as_number(
        matches$margin, "match margin"
      )
    }

    labeled$swap_index <- as_index(
      labeled$swap_index, "labeled swap_index"
    )
    labeled$structural_candidate <- as_yes_no(
      labeled$structural_candidate, "structural_candidate"
    )

    if (any(flows$sample != sample_name) ||
        any(labeled$sample != sample_name) ||
        any(!(flows$token_slot %in% c("token0", "token1"))) ||
        (nrow(matches) &&
         any(abs(matches$margin - margin) > 1e-12))) {
      stop(
        paste("Sample/token/margin inconsistency in", sample_name),
        call. = FALSE
      )
    }

    if (anyDuplicated(
      paste(flows$event_id, flows$token_slot, sep = "|")
    )) {
      stop("A prepared event/token row is repeated.", call. = FALSE)
    }

    # token0 gives one metadata row per original supplied swap.
    swap_metadata <- flows[
      flows$token_slot == "token0",
      c(
        "source_row", "event_id", "swap_index",
        "pool_address", "initiator_address", "transaction_hash",
        "block_number", "log_index"
      ),
      drop = FALSE
    ]

    swap_metadata <- swap_metadata[
      order(swap_metadata$swap_index),
      ,
      drop = FALSE
    ]
    rownames(swap_metadata) <- NULL

    if (!nrow(matches)) {
      cat("No candidate groups to review for:", sample_name, "\n")

      sample_summary_rows[[length(sample_summary_rows) + 1L]] <-
        data.frame(
          sample = sample_name,
          candidate_groups = 0L,
          consecutive_groups = 0L,
          nonconsecutive_groups = 0L,
          both_token_groups = 0L,
          one_token_only_groups = 0L,
          unique_candidate_swap_events = 0L,
          unique_candidate_transactions = 0L,
          unique_candidate_initiators = 0L,
          current_interpretation =
            "NO_GROUPS_UNDER_CURRENT_SETTINGS",
          stringsAsFactors = FALSE
        )
      next
    }

    group_ids <- unique(matches$candidate_group_id)

    for (group_id in group_ids) {
      group_matches <- matches[
        matches$candidate_group_id == group_id,
        ,
        drop = FALSE
      ]

      event_keys <- unique(group_matches$event_key)

      if (length(event_keys) != 1L ||
          anyDuplicated(group_matches$token_slot)) {
        stop(
          paste("Inconsistent Step-3 match rows for", group_id),
          call. = FALSE
        )
      }

      included_ids <- parse_event_key(event_keys[[1L]])

      if (any(!(included_ids %in% swap_metadata$swap_index))) {
        stop(
          paste("Unknown swap index in event_key for", group_id),
          call. = FALSE
        )
      }

      included_meta <- swap_metadata[
        match(included_ids, swap_metadata$swap_index),
        ,
        drop = FALSE
      ]

      skipped_between <- setdiff(
        seq.int(min(included_ids), max(included_ids)),
        included_ids
      )

      consecutive <- length(skipped_between) == 0L

      # --------------------------------------------------------------------
      # 6A. INDEPENDENTLY CALCULATE THE WTEYE-STYLE RATIO FOR BOTH TOKENS
      # --------------------------------------------------------------------
      token_results <- list()

      for (slot in c("token0", "token1")) {
        movements <- flows[
          flows$token_slot == slot &
          flows$swap_index %in% included_ids,
          ,
          drop = FALSE
        ]

        movements <- movements[
          match(included_ids, movements$swap_index),
          ,
          drop = FALSE
        ]

        if (nrow(movements) != length(included_ids) ||
            anyNA(movements$swap_index)) {
          stop(
            paste("Missing", slot, "movement for", group_id),
            call. = FALSE
          )
        }

        addresses <- sort(unique(c(
          movements$seller,
          movements$buyer
        )))

        received <- vapply(addresses, function(address) {
          sum(movements$amount[movements$buyer == address])
        }, numeric(1))

        sent <- vapply(addresses, function(address) {
          sum(movements$amount[movements$seller == address])
        }, numeric(1))

        net <- received - sent
        total_volume <- sum(movements$amount)
        l1_balance <- sum(abs(net))
        ratio <- l1_balance / (2 * total_volume)
        passes <- ratio < margin

        # Token conservation should hold apart from floating-point noise.
        conservation_error <- abs(sum(net))
        if (conservation_error >
            1e-11 * max(1, total_volume)) {
          stop(
            paste("Token conservation failed for", group_id, slot),
            call. = FALSE
          )
        }

        one_token_balances <- data.frame(
          sample = sample_name,
          candidate_group_id = group_id,
          token_slot = slot,
          address = addresses,
          address_role = ifelse(
            addresses == included_meta$pool_address[1L],
            "POOL",
            "INITIATOR_PROXY"
          ),
          total_received_scaled = received,
          total_sent_scaled = sent,
          net_balance_scaled = net,
          absolute_net_balance_scaled = abs(net),
          total_group_volume_scaled = total_volume,
          l1_balance_scaled = l1_balance,
          wteye_balance_ratio = ratio,
          margin = margin,
          whole_token_group_passes = passes,
          stringsAsFactors = FALSE
        )

        balance_rows[[length(balance_rows) + 1L]] <-
          one_token_balances

        token_results[[slot]] <- list(
          ratio = ratio,
          passes = passes
        )
      }

      matched_token0 <- "token0" %in% group_matches$token_slot
      matched_token1 <- "token1" %in% group_matches$token_slot

      # Because token0/token1 graphs are edge reversals for the same swaps,
      # their SCC structure is preserved. Therefore the independently calculated
      # whole-event-set balance pass should reproduce the token evidence Step 3
      # recorded for this group.
      if (!identical(
            matched_token0,
            token_results$token0$passes
          ) ||
          !identical(
            matched_token1,
            token_results$token1$passes
          )) {
        stop(
          paste(
            "Independent token review disagrees with Step 3 for",
            group_id
          ),
          call. = FALSE
        )
      }

      # For token rows actually saved by Step 3, check the saved ratio too.
      if (matched_token0) {
        saved <- group_matches$wteye_balance_ratio[
          group_matches$token_slot == "token0"
        ][1L]

        if (abs(saved - token_results$token0$ratio) >
            1e-11 * max(1, abs(saved))) {
          stop(
            paste("Saved token0 ratio disagrees for", group_id),
            call. = FALSE
          )
        }
      }

      if (matched_token1) {
        saved <- group_matches$wteye_balance_ratio[
          group_matches$token_slot == "token1"
        ][1L]

        if (abs(saved - token_results$token1$ratio) >
            1e-11 * max(1, abs(saved))) {
          stop(
            paste("Saved token1 ratio disagrees for", group_id),
            call. = FALSE
          )
        }
      }

      both_tokens <- matched_token0 && matched_token1

      evidence_pattern <- if (both_tokens) {
        "BOTH_TOKENS_BALANCE"
      } else if (matched_token0) {
        "TOKEN0_ONLY_BALANCES"
      } else {
        "TOKEN1_ONLY_BALANCES"
      }

      adjacency_comparison <- if (consecutive) {
        "CONSECUTIVE_SWAP_GROUP"
      } else {
        "NONCONSECUTIVE_AFTER_SCC_SELECTION"
      }

      interpretation <- if (consecutive && both_tokens) {
        "STRONGER_STRUCTURAL_ROUND_TRIP; STILL_REQUIRES_ENTITY_AND_MEV_REVIEW"
      } else if (!consecutive && both_tokens) {
        "BOTH_TOKENS_BALANCE_BUT_GROUP_HAS_GAPS; REVIEW_EXCLUDED_SWAPS"
      } else if (consecutive) {
        "ONLY_ONE_TOKEN_BALANCES; WEAKER_STRUCTURAL_EVIDENCE"
      } else {
        "ONLY_ONE_TOKEN_BALANCES_AND_GROUP_HAS_GAPS; HIGH_FALSE_POSITIVE_CONCERN"
      }

      group_review_rows[[length(group_review_rows) + 1L]] <-
        data.frame(
          sample = sample_name,
          candidate_group_id = group_id,
          event_key = event_keys[[1L]],
          included_swap_indices = join_values(included_ids),
          n_swap_events = length(included_ids),
          n_transactions =
            length(unique(included_meta$transaction_hash)),
          n_initiators =
            length(unique(included_meta$initiator_address)),
          initiator_addresses =
            join_values(included_meta$initiator_address),
          first_swap_index = min(included_ids),
          last_swap_index = max(included_ids),
          skipped_between_count = length(skipped_between),
          skipped_between_indices =
            join_values(skipped_between),
          consecutive_group = consecutive,
          first_block = min(included_meta$block_number),
          last_block = max(included_meta$block_number),
          block_span =
            max(included_meta$block_number) -
            min(included_meta$block_number),
          matched_token0 = matched_token0,
          matched_token1 = matched_token1,
          token0_wteye_balance_ratio =
            token_results$token0$ratio,
          token1_wteye_balance_ratio =
            token_results$token1$ratio,
          token0_passes_margin =
            token_results$token0$passes,
          token1_passes_margin =
            token_results$token1$passes,
          both_tokens_pass = both_tokens,
          evidence_pattern = evidence_pattern,
          adjacency_comparison = adjacency_comparison,
          interpretation = interpretation,
          confirmation_status =
            "NOT_CONFIRMED_WASH_TRADING",
          stringsAsFactors = FALSE
        )

      # --------------------------------------------------------------------
      # 6B. ORIGINAL SWAP MEMBERSHIP TABLE
      # --------------------------------------------------------------------
      token0 <- flows[
        flows$token_slot == "token0" &
        flows$swap_index %in% included_ids,
        ,
        drop = FALSE
      ]

      token1 <- flows[
        flows$token_slot == "token1" &
        flows$swap_index %in% included_ids,
        ,
        drop = FALSE
      ]

      token0 <- token0[
        match(included_ids, token0$swap_index),
        ,
        drop = FALSE
      ]

      token1 <- token1[
        match(included_ids, token1$swap_index),
        ,
        drop = FALSE
      ]

      member_rows[[length(member_rows) + 1L]] <-
        data.frame(
          sample = sample_name,
          candidate_group_id = group_id,
          position_in_group = seq_along(included_ids),
          swap_index = included_ids,
          source_row = included_meta$source_row,
          event_id = included_meta$event_id,
          transaction_hash = included_meta$transaction_hash,
          initiator_address = included_meta$initiator_address,
          block_number = included_meta$block_number,
          log_index = included_meta$log_index,
          token0_direction = token0$direction,
          token0_amount_raw = token0$amount_raw,
          token1_direction = token1$direction,
          token1_amount_raw = token1$amount_raw,
          stringsAsFactors = FALSE
        )

      # --------------------------------------------------------------------
      # 6C. SOURCE-WINDOW SWAPS THAT WERE NOT PART OF THE FINAL SCC EVENT SET
      # --------------------------------------------------------------------
      # WTEYE Step 3 evaluates whole SCC internal movements and performs no C++
      # prefix truncation. Therefore an excluded row here is excluded by the SCC
      # selection from that source window, not by a later prefix procedure.
      for (match_row in seq_len(nrow(group_matches))) {
        first <- group_matches$source_window_start[match_row]
        last <- group_matches$source_window_end[match_row]

        if (first > last ||
            any(!(included_ids %in% seq.int(first, last)))) {
          stop(
            paste(
              "Source window does not contain its group for",
              group_id
            ),
            call. = FALSE
          )
        }

        excluded_ids <- setdiff(
          seq.int(first, last),
          included_ids
        )

        if (!length(excluded_ids)) next

        excluded_meta <- swap_metadata[
          match(excluded_ids, swap_metadata$swap_index),
          ,
          drop = FALSE
        ]

        if (anyNA(excluded_meta$swap_index)) {
          stop(
            "Source-window swap metadata is missing.",
            call. = FALSE
          )
        }

        relative_position <- ifelse(
          excluded_ids < min(included_ids),
          "BEFORE_MATCHED_EVENTS",
          ifelse(
            excluded_ids > max(included_ids),
            "AFTER_MATCHED_EVENTS",
            "BETWEEN_MATCHED_EVENTS"
          )
        )

        exclusion_rows[[length(exclusion_rows) + 1L]] <-
          data.frame(
            sample = sample_name,
            candidate_group_id = group_id,
            matched_token_slot =
              group_matches$token_slot[match_row],
            source_window_start = first,
            source_window_end = last,
            excluded_swap_index = excluded_ids,
            position_relative_to_matched_group =
              relative_position,
            transaction_hash =
              excluded_meta$transaction_hash,
            initiator_address =
              excluded_meta$initiator_address,
            block_number = excluded_meta$block_number,
            log_index = excluded_meta$log_index,
            exclusion_stage =
              "SCC_SELECTION_FROM_SOURCE_WINDOW",
            explanation =
              "WHOLE_SCC_EVALUATION_USED; NO_CPP_PREFIX_TRUNCATION_IN_THIS_BRANCH",
            stringsAsFactors = FALSE
          )
      }
    }

    # ======================================================================
    # 7. SAMPLE-LEVEL CHECKS AGAINST STEP-3 LABELS AND COUNTS
    # ======================================================================
    reviewed_for_sample <- group_review_rows[
      vapply(
        group_review_rows,
        function(row) row$sample[1L] == sample_name,
        logical(1)
      )
    ]

    reviewed_ids <- sort(unique(unlist(lapply(
      reviewed_for_sample,
      function(row) parse_event_key(row$event_key)
    ))))

    labeled_ids <- sort(
      labeled$swap_index[labeled$structural_candidate]
    )

    if (!identical(reviewed_ids, labeled_ids)) {
      stop(
        paste(
          "Group membership does not reproduce Step-3 labels for",
          sample_name
        ),
        call. = FALSE
      )
    }

    review_table <- do.call(rbind, reviewed_for_sample)

    detector_row <- detection_summary[
      detection_summary$sample == sample_name,
      ,
      drop = FALSE
    ]

    if (nrow(review_table) !=
          as.integer(detector_row$candidate_groups) ||
        length(reviewed_ids) !=
          as.integer(detector_row$candidate_swap_events)) {
      stop(
        paste(
          "Step-4 review counts disagree with Step 3 for",
          sample_name
        ),
        call. = FALSE
      )
    }

    member_subset <- bind_or_empty(
      member_rows[
        vapply(
          member_rows,
          function(row) row$sample[1L] == sample_name,
          logical(1)
        )
      ],
      empty_members
    )

    sample_summary_rows[[length(sample_summary_rows) + 1L]] <-
      data.frame(
        sample = sample_name,
        candidate_groups = nrow(review_table),
        consecutive_groups =
          sum(review_table$consecutive_group),
        nonconsecutive_groups =
          sum(!review_table$consecutive_group),
        both_token_groups =
          sum(review_table$both_tokens_pass),
        one_token_only_groups =
          sum(!review_table$both_tokens_pass),
        unique_candidate_swap_events =
          length(reviewed_ids),
        unique_candidate_transactions =
          length(unique(member_subset$transaction_hash)),
        unique_candidate_initiators =
          length(unique(member_subset$initiator_address)),
        current_interpretation =
          if (all(
            review_table$consecutive_group &
            review_table$both_tokens_pass
          )) {
            "ALL_GROUPS_HAVE_STRONGER_STRUCTURAL_EVIDENCE"
          } else {
            "SOME_GROUPS_HAVE_GAPS_OR_ONLY_ONE_MATCHED_TOKEN"
          },
        stringsAsFactors = FALSE
      )

    cat(
      "Reviewed:", sample_name,
      "groups =", nrow(review_table),
      "; consecutive =", sum(review_table$consecutive_group),
      "; both-token =", sum(review_table$both_tokens_pass),
      "\n"
    )
  }

  # ========================================================================
  # 8. COMBINE BOTH SAMPLES AND SAVE FIVE STEP-4 REVIEW REPORTS
  # ========================================================================
  group_review <- bind_or_empty(
    group_review_rows,
    empty_group_review
  )

  group_members <- bind_or_empty(
    member_rows,
    empty_members
  )

  address_balances <- bind_or_empty(
    balance_rows,
    empty_balances
  )

  window_exclusions <- bind_or_empty(
    exclusion_rows,
    empty_exclusions
  )

  review_summary <- do.call(
    rbind,
    sample_summary_rows
  )
  rownames(review_summary) <- NULL

  expected_counts <- setNames(
    as_index(
      detection_summary$candidate_groups,
      "candidate_groups"
    ),
    detection_summary$sample
  )

  actual_counts <- table(
    factor(group_review$sample, levels = samples)
  )

  if (!identical(
    as.integer(actual_counts),
    as.integer(expected_counts[samples])
  )) {
    stop(
      "Step-4B group counts disagree with the Step-3 summary.",
      call. = FALSE
    )
  }

  if (!identical(
    input_md5_before,
    unname(tools::md5sum(all_input_paths))
  )) {
    stop(
      "An input file changed during Step 4B. No review reports were written.",
      call. = FALSE
    )
  }

  write_report <- function(table, filename) {
    table_for_disk <- table

    for (field in names(table_for_disk)) {
      if (is.double(table_for_disk[[field]])) {
        table_for_disk[[field]] <-
          sprintf("%.17g", table_for_disk[[field]])
      }
    }

    path <- file.path(cleaned_dir, filename)

    if (file.exists(path)) {
      cat("Refreshing our generated report:", filename, "\n")
    }

    utils::write.csv(
      table_for_disk,
      path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    cat("Saved:", path, "\n")
  }

  write_report(
    group_review,
    output_files[["group_review"]]
  )

  write_report(
    group_members,
    output_files[["group_members"]]
  )

  write_report(
    address_balances,
    output_files[["address_balances"]]
  )

  write_report(
    window_exclusions,
    output_files[["window_exclusions"]]
  )

  write_report(
    review_summary,
    output_files[["review_summary"]]
  )

  cat("\nSTEP 4B COMPLETE: all WTEYE-style candidate groups were reviewed.\n")
  cat("Step-2, Step-3, and validation input fingerprints unchanged: TRUE\n")
  cat("No new detection was performed and no group was confirmed as wash trading.\n")
  cat("Send the complete console output and all five new Step-4 CSVs for review.\n")
})
