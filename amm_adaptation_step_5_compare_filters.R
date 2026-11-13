# ============================================================================
# AMM ADAPTATION -- STEP 5: COMPARE CANDIDATE-FILTERING CHOICES
# ============================================================================
#
# OUR PURPOSE
# -----------
# Step 3 produced structural candidates, and Step 4 showed that some candidates
# have gaps or balance for only one token. We now compare four transparent rules
# using those SAME groups. This is a sensitivity/ablation analysis: it shows which
# added condition causes a result to stay or disappear.
#
# We do not rerun SCCs or C++. We do not tune a rule until it gives a preferred
# answer. We apply every rule equally to the suspicious-pool and control-pool
# samples and report all results.
#
# OUR FOUR RULES
# --------------
# 1. CURRENT_SCC_AT_LEAST_ONE_TOKEN
#    Keep every Step 3 group. This reproduces the current adapted detector.
# 2. REQUIRE_CONSECUTIVE
#    Keep a group only if no original pool swap is missing between its first and
#    last included swap. This adds the AMM paper's adjacency idea.
# 3. REQUIRE_BOTH_TOKENS
#    Keep a group only if token0 and token1 both satisfy our adapted balance rule.
#    This is an exploratory stricter rule. The AMM paper's coarse filter accepts
#    either asset, so we must not describe this rule as a direct paper replication.
# 4. REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS
#    Apply both additional conditions. This is also exploratory.
#
# WHY THIS STEP IS NEEDED
# -----------------------
# The current detector found more groups in the control sample than in the
# suspicious sample. Step 4 showed a clear mechanism: all six control groups
# contain gaps, and five pass for only one token. Comparing the four variants
# tells us how much each condition explains that result.
#
# This analysis cannot establish accuracy, recall, precision, or an optimal rule.
# We have only two small selected samples and no event-level ground-truth labels.
# It provides evidence about transferability and possible false-positive behavior.
#
# OUR OUTPUTS IN DATA/CLEANED
# ---------------------------
# amm_adaptation_output_both_pools_filter_variant_comparison.csv
#   One row per sample and rule, with group/event/address/volume counts.
# amm_adaptation_output_both_pools_filter_variant_group_decisions.csv
#   One row per candidate group and rule showing whether it is retained and why.
# amm_adaptation_output_both_pools_filter_variant_swap_labels.csv
#   One row per original swap and rule, allowing unique event counts to be checked.
#
# HOW WE RUN THE WHOLE SCRIPT IN OUR R CONSOLE
# ------------------------------------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_5_compare_filters.R")
#
# HOW WE READ THIS CODE
# --------------------
# Comments begin with #. <- stores a value. A function is a reusable operation.
# TRUE/FALSE mean yes/no. stop() ends the script if an input is inconsistent.

local({
  # ========================================================================
  # 1. WE SET OUR EXISTING PATHS AND FILENAMES
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")

  flow_files <- setNames(
    paste0("amm_adaptation_input_", samples, "_token_flows.csv"), samples)
  group_review_file <- "amm_adaptation_output_both_pools_candidate_group_review.csv"
  group_members_file <- "amm_adaptation_output_both_pools_candidate_group_members.csv"
  review_summary_file <- "amm_adaptation_output_both_pools_candidate_review_summary.csv"

  output_files <- c(
    comparison = "amm_adaptation_output_both_pools_filter_variant_comparison.csv",
    decisions = "amm_adaptation_output_both_pools_filter_variant_group_decisions.csv",
    swap_labels = "amm_adaptation_output_both_pools_filter_variant_swap_labels.csv"
  )

  input_files <- c(flow_files, group_review_file, group_members_file, review_summary_file)
  input_paths <- file.path(cleaned_dir, unname(input_files))
  cat("AMM ADAPTATION -- STEP 5: COMPARE FILTERING CHOICES\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")
  if (!dir.exists(cleaned_dir) || !all(file.exists(input_paths))) {
    stop("A required Step 2 or Step 4 file is missing from Data/cleaned.", call. = FALSE)
  }
  input_md5_before <- unname(tools::md5sum(input_paths))

  # ========================================================================
  # 2. WE DEFINE SAFE READING AND CONVERSION HELPERS
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
  as_index <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers) | numbers < 0 | numbers != floor(numbers) |
            numbers > .Machine$integer.max)) stop(paste("Invalid index in", field), call. = FALSE)
    as.integer(numbers)
  }
  as_number <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers))) stop(paste("Invalid number in", field), call. = FALSE)
    numbers
  }
  as_yes_no <- function(values, field) {
    if (any(!(values %in% c("TRUE", "FALSE")))) stop(paste("Invalid TRUE/FALSE in", field), call. = FALSE)
    values == "TRUE"
  }
  join_values <- function(values) {
    values <- unique(as.character(values))
    values <- values[nzchar(values)]
    if (!length(values)) "" else paste(values, collapse = ";")
  }

  # ========================================================================
  # 3. WE READ AND VALIDATE THE STEP 4 GROUP TABLES
  # ========================================================================
  groups <- read_text_csv(group_review_file)
  members <- read_text_csv(group_members_file)
  previous_summary <- read_text_csv(review_summary_file)
  require_columns(groups, c("sample", "candidate_group_id", "event_key",
    "consecutive_group", "both_tokens_pass", "matched_token0", "matched_token1",
    "confirmation_status"), group_review_file)
  require_columns(members, c("sample", "candidate_group_id", "swap_index",
    "transaction_hash", "initiator_address"), group_members_file)
  require_columns(previous_summary, c("sample", "candidate_groups",
    "unique_candidate_swap_events"), review_summary_file)

  if (anyDuplicated(groups$candidate_group_id) ||
      any(groups$confirmation_status != "NOT_CONFIRMED_WASH_TRADING") ||
      !all(groups$sample %in% samples) || !setequal(previous_summary$sample, samples)) {
    stop("The Step 4 group tables have inconsistent identifiers or status.", call. = FALSE)
  }
  groups$consecutive_group <- as_yes_no(groups$consecutive_group, "consecutive_group")
  groups$both_tokens_pass <- as_yes_no(groups$both_tokens_pass, "both_tokens_pass")
  groups$matched_token0 <- as_yes_no(groups$matched_token0, "matched_token0")
  groups$matched_token1 <- as_yes_no(groups$matched_token1, "matched_token1")
  members$swap_index <- as_index(members$swap_index, "member swap_index")

  # Every group must have member rows that reproduce its event_key exactly.
  for (row in seq_len(nrow(groups))) {
    group_id <- groups$candidate_group_id[row]
    recorded_ids <- sort(as_index(strsplit(groups$event_key[row], ";", fixed = TRUE)[[1L]],
                                  paste(group_id, "event_key")))
    member_ids <- sort(members$swap_index[members$candidate_group_id == group_id])
    if (!identical(recorded_ids, member_ids)) {
      stop(paste("Member rows disagree with event_key for", group_id), call. = FALSE)
    }
  }

  # ========================================================================
  # 4. WE DEFINE THE FOUR RULES AND THEIR ACADEMIC INTERPRETATIONS
  # ========================================================================
  variants <- data.frame(
    variant = c("CURRENT_SCC_AT_LEAST_ONE_TOKEN", "REQUIRE_CONSECUTIVE",
                "REQUIRE_BOTH_TOKENS", "REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS"),
    theoretical_role = c(
      "CURRENT_INTERNSHIP_TO_AMM_ADAPTATION",
      "CURRENT_ADAPTATION_PLUS_AMM_PAPER_ADJACENCY",
      "EXPLORATORY_STRICTER_TWO_TOKEN_RULE",
      "EXPLORATORY_ADJACENCY_AND_TWO_TOKEN_RULE"),
    limitation = c(
      "SCC_OR_PREFIX_SELECTION_CAN_CREATE_GAPS",
      "ONE_TOKEN_BALANCE_CAN_STILL_RETAIN_A_GROUP",
      "TWO_TOKEN_REQUIREMENT_IS_STRICTER_THAN_AMM_PAPER_COARSE_FILTER",
      "COMBINED_RULE_NEEDS_VALIDATION_ON_LARGER_GROUND_TRUTH_DATA"),
    stringsAsFactors = FALSE)

  retain_group <- function(variant, group_row) {
    if (variant == "CURRENT_SCC_AT_LEAST_ONE_TOKEN") return(TRUE)
    if (variant == "REQUIRE_CONSECUTIVE") return(group_row$consecutive_group)
    if (variant == "REQUIRE_BOTH_TOKENS") return(group_row$both_tokens_pass)
    if (variant == "REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS") {
      return(group_row$consecutive_group && group_row$both_tokens_pass)
    }
    stop("Unknown filtering variant.", call. = FALSE)
  }

  # ========================================================================
  # 5. WE RECORD EVERY GROUP DECISION UNDER EVERY RULE
  # ========================================================================
  decision_rows <- list()
  for (variant_row in seq_len(nrow(variants))) {
    variant <- variants$variant[variant_row]
    for (group_row in seq_len(nrow(groups))) {
      retained <- retain_group(variant, groups[group_row, , drop = FALSE])
      reason <- if (retained) {
        "RETAINED_ALL_REQUIRED_CONDITIONS_PASS"
      } else if (!groups$consecutive_group[group_row] && !groups$both_tokens_pass[group_row]) {
        "REMOVED_NONCONSECUTIVE_AND_ONLY_ONE_TOKEN_BALANCES"
      } else if (!groups$consecutive_group[group_row]) {
        "REMOVED_NONCONSECUTIVE_GROUP"
      } else {
        "REMOVED_ONLY_ONE_TOKEN_BALANCES"
      }
      decision_rows[[length(decision_rows) + 1L]] <- data.frame(
        variant = variant, theoretical_role = variants$theoretical_role[variant_row],
        sample = groups$sample[group_row],
        candidate_group_id = groups$candidate_group_id[group_row],
        event_key = groups$event_key[group_row],
        consecutive_group = groups$consecutive_group[group_row],
        both_tokens_pass = groups$both_tokens_pass[group_row],
        matched_token0 = groups$matched_token0[group_row],
        matched_token1 = groups$matched_token1[group_row],
        retained = retained, decision_reason = reason,
        limitation = variants$limitation[variant_row],
        status = "EXPLORATORY_FILTER_COMPARISON",
        stringsAsFactors = FALSE)
    }
  }
  decisions <- do.call(rbind, decision_rows)
  rownames(decisions) <- NULL

  # ========================================================================
  # 6. WE CREATE UNIQUE SWAP LABELS AND SAMPLE-LEVEL COMPARISONS
  # ========================================================================
  comparison_rows <- list()
  swap_label_rows <- list()
  for (sample_name in samples) {
    flows <- read_text_csv(flow_files[[sample_name]])
    require_columns(flows, c("sample", "swap_index", "event_id", "transaction_hash",
      "initiator_address", "token_slot", "amount"), flow_files[[sample_name]])
    if (any(flows$sample != sample_name) || any(!(flows$token_slot %in% c("token0", "token1")))) {
      stop(paste("Invalid sample/token identifiers for", sample_name), call. = FALSE)
    }
    flows$swap_index <- as_index(flows$swap_index, "flow swap_index")
    flows$amount <- as_number(flows$amount, "flow amount")
    token0 <- flows[flows$token_slot == "token0", , drop = FALSE]
    token1 <- flows[flows$token_slot == "token1", , drop = FALSE]
    token0 <- token0[order(token0$swap_index), , drop = FALSE]
    token1 <- token1[order(token1$swap_index), , drop = FALSE]
    if (!identical(token0$swap_index, token1$swap_index) || anyDuplicated(token0$swap_index)) {
      stop(paste("Token rows do not map one-to-one for", sample_name), call. = FALSE)
    }

    for (variant_row in seq_len(nrow(variants))) {
      variant <- variants$variant[variant_row]
      retained_groups <- decisions$candidate_group_id[
        decisions$sample == sample_name & decisions$variant == variant & decisions$retained]
      retained_members <- members[members$sample == sample_name &
        members$candidate_group_id %in% retained_groups, , drop = FALSE]
      selected_ids <- sort(unique(retained_members$swap_index))

      # We label every original supplied swap so unique-event counts can be audited.
      one_variant_labels <- data.frame(
        variant = variant, sample = sample_name, swap_index = token0$swap_index,
        event_id = token0$event_id, transaction_hash = token0$transaction_hash,
        initiator_address = token0$initiator_address,
        retained_candidate = token0$swap_index %in% selected_ids,
        retained_group_ids = "", stringsAsFactors = FALSE)
      for (swap_row in seq_len(nrow(one_variant_labels))) {
        memberships <- retained_members$candidate_group_id[
          retained_members$swap_index == one_variant_labels$swap_index[swap_row]]
        one_variant_labels$retained_group_ids[swap_row] <- join_values(memberships)
      }
      swap_label_rows[[length(swap_label_rows) + 1L]] <- one_variant_labels

      selected <- token0$swap_index %in% selected_ids
      selected_hashes <- unique(token0$transaction_hash[selected])
      selected_initiators <- unique(token0$initiator_address[selected])
      comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
        variant = variant, theoretical_role = variants$theoretical_role[variant_row],
        sample = sample_name, retained_groups = length(retained_groups),
        unique_candidate_swap_events = sum(selected),
        unique_candidate_transactions = length(selected_hashes),
        unique_candidate_initiators = length(selected_initiators),
        candidate_event_share_pct = 100 * sum(selected) / nrow(token0),
        token0_candidate_volume_share_pct = if (any(selected))
          100 * sum(token0$amount[selected]) / sum(token0$amount) else 0,
        token1_candidate_volume_share_pct = if (any(selected))
          100 * sum(token1$amount[selected]) / sum(token1$amount) else 0,
        limitation = variants$limitation[variant_row],
        interpretation_status = "EXPLORATORY_NOT_ACCURACY_VALIDATION",
        stringsAsFactors = FALSE)
    }
  }
  comparison <- do.call(rbind, comparison_rows)
  swap_labels <- do.call(rbind, swap_label_rows)
  rownames(comparison) <- NULL
  rownames(swap_labels) <- NULL

  # ========================================================================
  # 7. WE CHECK THE BASELINE AND SAVE THREE REPORTS
  # ========================================================================
  baseline <- comparison[comparison$variant == "CURRENT_SCC_AT_LEAST_ONE_TOKEN", , drop = FALSE]
  previous_counts <- setNames(as_index(previous_summary$unique_candidate_swap_events,
    "previous candidate event count"), previous_summary$sample)
  if (!identical(as.integer(baseline$unique_candidate_swap_events),
                 as.integer(previous_counts[baseline$sample]))) {
    stop("The current-rule event counts do not reproduce Step 4.", call. = FALSE)
  }
  if (!identical(input_md5_before, unname(tools::md5sum(input_paths)))) {
    stop("An input changed during this comparison. No reports were written.", call. = FALSE)
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
  write_report(comparison, output_files[["comparison"]])
  write_report(decisions, output_files[["decisions"]])
  write_report(swap_labels, output_files[["swap_labels"]])

  cat("\nFILTER COMPARISON\n")
  print(comparison[, c("variant", "sample", "retained_groups",
    "unique_candidate_swap_events")], row.names = FALSE)
  cat("\nSTEP 5 COMPLETE: four filtering choices were compared equally.\n")
  cat("Input fingerprints unchanged: TRUE\n")
  cat("These are sensitivity results, not proof of accuracy or confirmed wash trading.\n")
  cat("Send the complete console output and all three Step 5 CSVs for review.\n")
})
