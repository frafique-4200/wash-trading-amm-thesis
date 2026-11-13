# ============================================================================
# AMM ADAPTATION -- STEP 4: REVIEW WHY EACH CANDIDATE WAS FLAGGED
# ============================================================================
#
# OUR PURPOSE
# -----------
# We turn the Step 3 detector output into tables that we can inspect and explain.
# For each candidate group, we show its swaps, both token balance calculations,
# whether its swap indices are consecutive, and which swaps from the detector's
# source window were excluded from the final matched group.
#
# This step does not detect new groups and does not change the Step 3 settings.
# It does not call volume_matching.cpp. It independently recalculates the balance
# ratios from the Step 2 token-flow tables and checks that they agree with the
# token matches saved by Step 3.
#
# WHY WE NEED THIS REVIEW
# -----------------------
# Step 3 found one group in the suspicious-pool sample and six in the control
# sample. Therefore, the counts alone cannot show that our adapted detector is
# effective. We must examine the reason each group passed and identify possible
# false-positive mechanisms before making an academic conclusion.
#
# A key issue is adjacency. The AMM paper's coarse filter examines consecutive
# slices of the pool's swap sequence. Our adapted SCC method can remove unrelated
# rows from a source window, producing a group such as swaps 1 and 3 while swap 2
# is absent. We explicitly report whether every integer swap index between the
# first and last matched swaps is present.
#
# OUR BALANCE CALCULATION
# -----------------------
# For one group and one token:
#   net balance = total received - total sent
#   balance ratio = absolute net balance / mean token-movement amount
# The group passes our current adapted rule for that token only when EVERY
# participating address, including the pool, has a ratio no larger than 0.10.
# We calculate token0 and token1 separately. We never add different tokens.
#
# A group can pass for one token but fail for the other because Step 3 uses an
# AT_LEAST_ONE_TOKEN_MATCH rule. Passing both tokens is stronger balance evidence,
# but it still does not prove wash trading. Address ownership, MEV/arbitrage, and
# ground-truth checks have not been performed.
#
# OUR INPUT FILES IN DATA/CLEANED
# -------------------------------
# Step 2 prepared inputs:
# amm_adaptation_input_suspicious_pool_token_flows.csv
# amm_adaptation_input_control_pool_token_flows.csv
#
# Step 3 reports:
# amm_adaptation_output_suspicious_pool_token_matches.csv
# amm_adaptation_output_suspicious_pool_swaps_labeled.csv
# amm_adaptation_output_control_pool_token_matches.csv
# amm_adaptation_output_control_pool_swaps_labeled.csv
# amm_adaptation_output_both_pools_detection_summary.csv
#
# OUR FIVE NEW OUTPUTS IN DATA/CLEANED
# ------------------------------------
# amm_adaptation_output_both_pools_candidate_group_review.csv
#   One row per distinct candidate group, with both-token results and warnings.
# amm_adaptation_output_both_pools_candidate_group_members.csv
#   One row per included original swap per group.
# amm_adaptation_output_both_pools_candidate_address_balances.csv
#   One row per group, token, and address showing the balance calculation.
# amm_adaptation_output_both_pools_candidate_window_exclusions.csv
#   Swaps inside a match's source window that were not in its final group.
# amm_adaptation_output_both_pools_candidate_review_summary.csv
#   Short sample-level counts for interpreting the review.
#
# The exclusion report does not claim that every excluded row was removed by the
# SCC operation specifically. A row can be excluded during SCC selection or the
# C++ prefix procedure. Step 3 did not record that intermediate distinction.
#
# HOW WE RUN THE WHOLE SCRIPT IN OUR R CONSOLE
# ------------------------------------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_4_review_candidates.R")
#
# HOW WE READ THIS CODE
# --------------------
# Comments begin with #. <- stores a value. A function is a reusable operation.
# A data.frame is a table. TRUE/FALSE mean yes/no. NA means missing. stop() ends
# the script with an explanation if an input or calculation is inconsistent.

local({
  # ========================================================================
  # 1. WE SET OUR EXISTING PATHS AND EXACT FILENAMES
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")
  margin <- 0.10

  flow_files <- setNames(
    paste0("amm_adaptation_input_", samples, "_token_flows.csv"), samples)
  match_files <- setNames(
    paste0("amm_adaptation_output_", samples, "_token_matches.csv"), samples)
  labeled_files <- setNames(
    paste0("amm_adaptation_output_", samples, "_swaps_labeled.csv"), samples)
  detection_summary_file <- "amm_adaptation_output_both_pools_detection_summary.csv"

  output_files <- c(
    group_review = "amm_adaptation_output_both_pools_candidate_group_review.csv",
    group_members = "amm_adaptation_output_both_pools_candidate_group_members.csv",
    address_balances = "amm_adaptation_output_both_pools_candidate_address_balances.csv",
    window_exclusions = "amm_adaptation_output_both_pools_candidate_window_exclusions.csv",
    review_summary = "amm_adaptation_output_both_pools_candidate_review_summary.csv"
  )

  cat("AMM ADAPTATION -- STEP 4: REVIEW STRUCTURAL CANDIDATES\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")
  all_input_files <- c(flow_files, match_files, labeled_files, detection_summary_file)
  all_input_paths <- file.path(cleaned_dir, unname(all_input_files))
  if (!dir.exists(cleaned_dir) || !all(file.exists(all_input_paths))) {
    stop("A required Step 2 or Step 3 file is missing from Data/cleaned.", call. = FALSE)
  }
  input_md5_before <- unname(tools::md5sum(all_input_paths))

  # ========================================================================
  # 2. WE DEFINE SMALL HELPERS FOR SAFE READING AND OUTPUT
  # ========================================================================
  read_text_csv <- function(filename) {
    utils::read.csv(file.path(cleaned_dir, filename), colClasses = "character",
      check.names = FALSE, stringsAsFactors = FALSE, na.strings = "")
  }

  require_columns <- function(table, columns, filename) {
    missing <- setdiff(columns, names(table))
    if (length(missing)) {
      stop(paste(filename, "is missing:", paste(missing, collapse = ", ")), call. = FALSE)
    }
  }

  # We accept only nonnegative whole-number indices that fit safely in an R integer.
  as_index <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    bad <- !is.finite(numbers) | numbers < 0 | numbers != floor(numbers) |
      numbers > .Machine$integer.max
    if (any(bad)) stop(paste("Invalid index in", field), call. = FALSE)
    as.integer(numbers)
  }

  as_number <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers))) stop(paste("Invalid number in", field), call. = FALSE)
    numbers
  }

  # We use this helper to join zero, one, or several values for a readable CSV cell.
  join_values <- function(values) {
    values <- unique(as.character(values))
    values <- values[nzchar(values)]
    if (!length(values)) "" else paste(values, collapse = ";")
  }

  bind_or_empty <- function(rows, empty_table) {
    if (!length(rows)) return(empty_table)
    answer <- do.call(rbind, rows)
    rownames(answer) <- NULL
    answer
  }

  # ========================================================================
  # 3. WE READ AND CHECK THE STEP 3 SETTINGS
  # ========================================================================
  detection_summary <- read_text_csv(detection_summary_file)
  require_columns(detection_summary,
    c("sample", "margin", "max_swaps", "max_block_span", "max_passes",
      "candidate_groups", "candidate_swap_events", "candidate_rule"),
    detection_summary_file)
  if (!setequal(detection_summary$sample, samples)) {
    stop("The detection summary does not contain our two expected samples.", call. = FALSE)
  }
  margins <- as_number(detection_summary$margin, "detection-summary margin")
  if (any(abs(margins - margin) > 1e-12) ||
      any(detection_summary$candidate_rule != "AT_LEAST_ONE_TOKEN_MATCH")) {
    stop("Step 3 settings differ from the Step 4 review assumptions.", call. = FALSE)
  }
  cat("Reviewing Step 3 settings: margin =", margin,
      "; max swaps =", join_values(detection_summary$max_swaps),
      "; max block span =", join_values(detection_summary$max_block_span),
      "; max passes =", join_values(detection_summary$max_passes), "\n")

  # ========================================================================
  # 4. WE PREPARE EMPTY TABLE SHAPES FOR VALID ZERO-ROW RESULTS
  # ========================================================================
  empty_group_review <- data.frame(
    sample = character(), candidate_group_id = character(), event_key = character(),
    included_swap_indices = character(), n_swap_events = integer(), n_initiators = integer(),
    initiator_addresses = character(), first_swap_index = integer(), last_swap_index = integer(),
    skipped_between_count = integer(), skipped_between_indices = character(),
    consecutive_group = logical(), first_block = integer(), last_block = integer(),
    block_span = integer(), matched_token0 = logical(), matched_token1 = logical(),
    token0_max_balance_ratio = numeric(), token1_max_balance_ratio = numeric(),
    token0_passes_margin = logical(), token1_passes_margin = logical(),
    both_tokens_pass = logical(), evidence_pattern = character(),
    adjacency_comparison = character(), interpretation = character(),
    confirmation_status = character(), stringsAsFactors = FALSE)

  empty_members <- data.frame(
    sample = character(), candidate_group_id = character(), position_in_group = integer(),
    swap_index = integer(), source_row = integer(), event_id = character(),
    transaction_hash = character(), initiator_address = character(),
    block_number = integer(), log_index = integer(), token0_direction = character(),
    token0_amount_raw = character(), token1_direction = character(),
    token1_amount_raw = character(), stringsAsFactors = FALSE)

  empty_balances <- data.frame(
    sample = character(), candidate_group_id = character(), token_slot = character(),
    address = character(), address_role = character(), total_received_scaled = numeric(),
    total_sent_scaled = numeric(), net_balance_scaled = numeric(),
    mean_group_amount_scaled = numeric(), absolute_balance_over_mean = numeric(),
    margin = numeric(), address_passes_margin = logical(),
    whole_token_group_passes = logical(), stringsAsFactors = FALSE)

  empty_exclusions <- data.frame(
    sample = character(), candidate_group_id = character(), matched_token_slot = character(),
    source_window_start = integer(), source_window_end = integer(), excluded_swap_index = integer(),
    position_relative_to_matched_group = character(), transaction_hash = character(),
    initiator_address = character(), block_number = integer(), log_index = integer(),
    exclusion_stage = character(), explanation = character(), stringsAsFactors = FALSE)

  # These lists collect rows from both samples before we write any report.
  group_review_rows <- list()
  member_rows <- list()
  balance_rows <- list()
  exclusion_rows <- list()
  sample_summary_rows <- list()

  # ========================================================================
  # 5. WE REVIEW EVERY DISTINCT GROUP IN EACH SAMPLE
  # ========================================================================
  for (sample_name in samples) {
    flows <- read_text_csv(flow_files[[sample_name]])
    matches <- read_text_csv(match_files[[sample_name]])
    labeled <- read_text_csv(labeled_files[[sample_name]])

    require_columns(flows, c("sample", "source_row", "event_id", "swap_index",
      "pool_address", "initiator_address", "transaction_hash", "block_number",
      "log_index", "token_slot", "direction", "seller", "buyer", "amount_raw",
      "scale_raw", "amount"), flow_files[[sample_name]])
    require_columns(matches, c("candidate_group_id", "token_slot", "event_key",
      "source_window_start", "source_window_end", "margin"), match_files[[sample_name]])
    require_columns(labeled, c("sample", "swap_index", "structural_candidate",
      "candidate_group_ids"), labeled_files[[sample_name]])

    if (!nrow(matches)) {
      cat("No candidate groups to review for:", sample_name, "\n")
      sample_summary_rows[[length(sample_summary_rows) + 1L]] <- data.frame(
        sample = sample_name, candidate_groups = 0L, consecutive_groups = 0L,
        nonconsecutive_groups = 0L, both_token_groups = 0L, one_token_only_groups = 0L,
        unique_candidate_swap_events = 0L,
        current_interpretation = "NO_GROUPS_UNDER_CURRENT_SETTINGS",
        stringsAsFactors = FALSE)
      next
    }

    # We convert only fields needed for arithmetic. Exact raw amounts remain text.
    flows$source_row <- as_index(flows$source_row, "flow source_row")
    flows$swap_index <- as_index(flows$swap_index, "flow swap_index")
    flows$block_number <- as_index(flows$block_number, "flow block_number")
    flows$log_index <- as_index(flows$log_index, "flow log_index")
    flows$amount <- as_number(flows$amount, "flow amount")
    matches$source_window_start <- as_index(matches$source_window_start, "source_window_start")
    matches$source_window_end <- as_index(matches$source_window_end, "source_window_end")
    matches$margin <- as_number(matches$margin, "match margin")
    labeled$swap_index <- as_index(labeled$swap_index, "labeled swap_index")

    if (any(flows$sample != sample_name) || any(labeled$sample != sample_name) ||
        any(!(flows$token_slot %in% c("token0", "token1"))) ||
        any(abs(matches$margin - margin) > 1e-12)) {
      stop(paste("Sample/token/margin inconsistency in", sample_name), call. = FALSE)
    }
    if (anyDuplicated(paste(flows$event_id, flows$token_slot, sep = "|"))) {
      stop("A prepared event/token row is repeated.", call. = FALSE)
    }

    # token0 provides one metadata record for each original swap. Step 2 already
    # checked that token1 maps to the same ordered set.
    swap_metadata <- flows[flows$token_slot == "token0",
      c("source_row", "event_id", "swap_index", "pool_address", "initiator_address",
        "transaction_hash", "block_number", "log_index"), drop = FALSE]
    swap_metadata <- swap_metadata[order(swap_metadata$swap_index), , drop = FALSE]
    rownames(swap_metadata) <- NULL

    # One group can have two match rows, one per token. Its event_key must remain
    # identical across those rows. candidate_group_id gives our distinct groups.
    group_ids <- unique(matches$candidate_group_id)
    for (group_id in group_ids) {
      group_matches <- matches[matches$candidate_group_id == group_id, , drop = FALSE]
      event_keys <- unique(group_matches$event_key)
      if (length(event_keys) != 1L || anyDuplicated(group_matches$token_slot)) {
        stop(paste("Inconsistent match rows for", group_id), call. = FALSE)
      }
      included_ids <- suppressWarnings(as.integer(strsplit(event_keys, ";", fixed = TRUE)[[1L]]))
      if (anyNA(included_ids) || anyDuplicated(included_ids) ||
          any(!(included_ids %in% swap_metadata$swap_index))) {
        stop(paste("Invalid event_key for", group_id), call. = FALSE)
      }
      included_ids <- sort(included_ids)
      included_meta <- swap_metadata[match(included_ids, swap_metadata$swap_index), , drop = FALSE]
      skipped_between <- setdiff(seq.int(min(included_ids), max(included_ids)), included_ids)
      consecutive <- length(skipped_between) == 0L

      # We independently calculate the balance of every address for both tokens.
      token_results <- list()
      for (slot in c("token0", "token1")) {
        movements <- flows[flows$token_slot == slot & flows$swap_index %in% included_ids, , drop = FALSE]
        movements <- movements[match(included_ids, movements$swap_index), , drop = FALSE]
        if (nrow(movements) != length(included_ids) || anyNA(movements$swap_index)) {
          stop(paste("Missing", slot, "movement for", group_id), call. = FALSE)
        }
        addresses <- sort(unique(c(movements$seller, movements$buyer)))
        mean_amount <- mean(movements$amount)
        one_token_balances <- list()
        for (address in addresses) {
          received <- sum(movements$amount[movements$buyer == address])
          sent <- sum(movements$amount[movements$seller == address])
          net <- received - sent
          ratio <- abs(net) / mean_amount
          one_token_balances[[length(one_token_balances) + 1L]] <- data.frame(
            sample = sample_name, candidate_group_id = group_id, token_slot = slot,
            address = address,
            address_role = ifelse(address == included_meta$pool_address[1L], "POOL", "INITIATOR_PROXY"),
            total_received_scaled = received, total_sent_scaled = sent,
            net_balance_scaled = net, mean_group_amount_scaled = mean_amount,
            absolute_balance_over_mean = ratio, margin = margin,
            address_passes_margin = ratio <= margin,
            whole_token_group_passes = FALSE, stringsAsFactors = FALSE)
        }
        one_token_balances <- do.call(rbind, one_token_balances)
        token_pass <- all(one_token_balances$address_passes_margin)
        one_token_balances$whole_token_group_passes <- token_pass
        balance_rows[[length(balance_rows) + 1L]] <- one_token_balances
        token_results[[slot]] <- list(
          max_ratio = max(one_token_balances$absolute_balance_over_mean), passes = token_pass)
      }

      matched_token0 <- "token0" %in% group_matches$token_slot
      matched_token1 <- "token1" %in% group_matches$token_slot
      # This assertion checks that our independent whole-group calculation agrees
      # with the token evidence recorded by Step 3.
      if (!identical(matched_token0, token_results$token0$passes) ||
          !identical(matched_token1, token_results$token1$passes)) {
        stop(paste("Independent token review disagrees with Step 3 for", group_id), call. = FALSE)
      }
      both_tokens <- matched_token0 && matched_token1
      evidence_pattern <- if (both_tokens) "BOTH_TOKENS_BALANCE" else
        if (matched_token0) "TOKEN0_ONLY_BALANCES" else "TOKEN1_ONLY_BALANCES"
      adjacency_comparison <- if (consecutive) "CONSECUTIVE_SWAP_GROUP" else
        "NONCONSECUTIVE_AFTER_SCC_OR_PREFIX_SELECTION"
      interpretation <- if (consecutive && both_tokens) {
        "STRONGER_STRUCTURAL_ROUND_TRIP; STILL_REQUIRES_ENTITY_AND_MEV_REVIEW"
      } else if (!consecutive && both_tokens) {
        "BOTH_TOKENS_BALANCE_BUT_GROUP_HAS_GAPS; REVIEW_EXCLUDED_SWAPS"
      } else if (consecutive) {
        "ONLY_ONE_TOKEN_BALANCES; WEAKER_STRUCTURAL_EVIDENCE"
      } else {
        "ONLY_ONE_TOKEN_BALANCES_AND_GROUP_HAS_GAPS; HIGH_FALSE_POSITIVE_CONCERN"
      }

      group_review_rows[[length(group_review_rows) + 1L]] <- data.frame(
        sample = sample_name, candidate_group_id = group_id, event_key = event_keys,
        included_swap_indices = join_values(included_ids), n_swap_events = length(included_ids),
        n_initiators = length(unique(included_meta$initiator_address)),
        initiator_addresses = join_values(included_meta$initiator_address),
        first_swap_index = min(included_ids), last_swap_index = max(included_ids),
        skipped_between_count = length(skipped_between),
        skipped_between_indices = join_values(skipped_between), consecutive_group = consecutive,
        first_block = min(included_meta$block_number), last_block = max(included_meta$block_number),
        block_span = max(included_meta$block_number) - min(included_meta$block_number),
        matched_token0 = matched_token0, matched_token1 = matched_token1,
        token0_max_balance_ratio = token_results$token0$max_ratio,
        token1_max_balance_ratio = token_results$token1$max_ratio,
        token0_passes_margin = token_results$token0$passes,
        token1_passes_margin = token_results$token1$passes,
        both_tokens_pass = both_tokens, evidence_pattern = evidence_pattern,
        adjacency_comparison = adjacency_comparison, interpretation = interpretation,
        confirmation_status = "NOT_CONFIRMED_WASH_TRADING", stringsAsFactors = FALSE)

      # We place token0/token1 quantities beside the original event metadata to
      # make each included swap readable without combining token quantities.
      token0 <- flows[flows$token_slot == "token0" & flows$swap_index %in% included_ids, , drop = FALSE]
      token1 <- flows[flows$token_slot == "token1" & flows$swap_index %in% included_ids, , drop = FALSE]
      token0 <- token0[match(included_ids, token0$swap_index), , drop = FALSE]
      token1 <- token1[match(included_ids, token1$swap_index), , drop = FALSE]
      member_rows[[length(member_rows) + 1L]] <- data.frame(
        sample = sample_name, candidate_group_id = group_id,
        position_in_group = seq_along(included_ids), swap_index = included_ids,
        source_row = included_meta$source_row, event_id = included_meta$event_id,
        transaction_hash = included_meta$transaction_hash,
        initiator_address = included_meta$initiator_address,
        block_number = included_meta$block_number, log_index = included_meta$log_index,
        token0_direction = token0$direction, token0_amount_raw = token0$amount_raw,
        token1_direction = token1$direction, token1_amount_raw = token1$amount_raw,
        stringsAsFactors = FALSE)

      # A match row records the source window used for that token. We list every
      # swap inside it that is absent from the final group. We do this per matched
      # token because two token matches can, in general, come from different windows.
      for (match_row in seq_len(nrow(group_matches))) {
        first <- group_matches$source_window_start[match_row]
        last <- group_matches$source_window_end[match_row]
        if (first > last || any(!(included_ids %in% seq.int(first, last)))) {
          stop(paste("Source window does not contain its group for", group_id), call. = FALSE)
        }
        excluded_ids <- setdiff(seq.int(first, last), included_ids)
        if (!length(excluded_ids)) next
        excluded_meta <- swap_metadata[match(excluded_ids, swap_metadata$swap_index), , drop = FALSE]
        if (anyNA(excluded_meta$swap_index)) stop("Source-window swap metadata is missing.", call. = FALSE)
        relative_position <- ifelse(excluded_ids < min(included_ids), "BEFORE_MATCHED_EVENTS",
          ifelse(excluded_ids > max(included_ids), "AFTER_MATCHED_EVENTS", "BETWEEN_MATCHED_EVENTS"))
        exclusion_rows[[length(exclusion_rows) + 1L]] <- data.frame(
          sample = sample_name, candidate_group_id = group_id,
          matched_token_slot = group_matches$token_slot[match_row],
          source_window_start = first, source_window_end = last,
          excluded_swap_index = excluded_ids, position_relative_to_matched_group = relative_position,
          transaction_hash = excluded_meta$transaction_hash,
          initiator_address = excluded_meta$initiator_address,
          block_number = excluded_meta$block_number, log_index = excluded_meta$log_index,
          exclusion_stage = "SCC_SELECTION_AND_OR_CPP_PREFIX",
          explanation = "STEP_3_DID_NOT_RECORD_THE_EXACT_INTERMEDIATE_STAGE",
          stringsAsFactors = FALSE)
      }
    }

    # We verify that the union of our group members equals the Step 3 candidate flags.
    reviewed_for_sample <- group_review_rows[vapply(group_review_rows,
      function(row) row$sample[1L] == sample_name, logical(1))]
    reviewed_ids <- sort(unique(unlist(lapply(reviewed_for_sample, function(row) {
      as.integer(strsplit(row$event_key, ";", fixed = TRUE)[[1L]])
    }))))
    labeled_ids <- sort(labeled$swap_index[labeled$structural_candidate == "TRUE"])
    if (!identical(reviewed_ids, labeled_ids)) {
      stop(paste("Group membership does not reproduce Step 3 labels for", sample_name), call. = FALSE)
    }
    review_table <- do.call(rbind, reviewed_for_sample)
    sample_summary_rows[[length(sample_summary_rows) + 1L]] <- data.frame(
      sample = sample_name, candidate_groups = nrow(review_table),
      consecutive_groups = sum(review_table$consecutive_group),
      nonconsecutive_groups = sum(!review_table$consecutive_group),
      both_token_groups = sum(review_table$both_tokens_pass),
      one_token_only_groups = sum(!review_table$both_tokens_pass),
      unique_candidate_swap_events = length(reviewed_ids),
      current_interpretation = if (all(review_table$consecutive_group & review_table$both_tokens_pass))
        "ALL_GROUPS_HAVE_STRONGER_STRUCTURAL_EVIDENCE" else
        "SOME_GROUPS_HAVE_GAPS_OR_ONLY_ONE_MATCHED_TOKEN",
      stringsAsFactors = FALSE)
    cat("Reviewed:", sample_name, "groups =", nrow(review_table),
        "; consecutive =", sum(review_table$consecutive_group),
        "; both-token =", sum(review_table$both_tokens_pass), "\n")
  }

  # ========================================================================
  # 6. WE COMBINE BOTH SAMPLES AND SAVE FIVE REVIEW REPORTS
  # ========================================================================
  group_review <- bind_or_empty(group_review_rows, empty_group_review)
  group_members <- bind_or_empty(member_rows, empty_members)
  address_balances <- bind_or_empty(balance_rows, empty_balances)
  window_exclusions <- bind_or_empty(exclusion_rows, empty_exclusions)
  review_summary <- do.call(rbind, sample_summary_rows)
  rownames(review_summary) <- NULL

  # We compare Step 4 group counts with the Step 3 summary before writing files.
  expected_counts <- setNames(as_index(detection_summary$candidate_groups,
    "candidate_groups"), detection_summary$sample)
  actual_counts <- table(factor(group_review$sample, levels = samples))
  if (!identical(as.integer(actual_counts), as.integer(expected_counts[samples]))) {
    stop("Step 4 group counts disagree with the Step 3 summary.", call. = FALSE)
  }
  if (!identical(input_md5_before, unname(tools::md5sum(all_input_paths)))) {
    stop("An input file changed during the review. No review reports were written.", call. = FALSE)
  }

  write_report <- function(table, filename) {
    table_for_disk <- table
    for (field in names(table_for_disk)) {
      if (is.double(table_for_disk[[field]])) {
        table_for_disk[[field]] <- sprintf("%.17g", table_for_disk[[field]])
      }
    }
    path <- file.path(cleaned_dir, filename)
    if (file.exists(path)) cat("Refreshing our generated report:", filename, "\n")
    utils::write.csv(table_for_disk, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    cat("Saved:", path, "\n")
  }

  write_report(group_review, output_files[["group_review"]])
  write_report(group_members, output_files[["group_members"]])
  write_report(address_balances, output_files[["address_balances"]])
  write_report(window_exclusions, output_files[["window_exclusions"]])
  write_report(review_summary, output_files[["review_summary"]])

  cat("\nSTEP 4 COMPLETE: all candidate groups were independently reviewed.\n")
  cat("Step 2 and Step 3 input fingerprints unchanged: TRUE\n")
  cat("No new detection was performed and no group was confirmed as wash trading.\n")
  cat("Send the complete console output and all five new Step 4 CSVs for review.\n")
})
