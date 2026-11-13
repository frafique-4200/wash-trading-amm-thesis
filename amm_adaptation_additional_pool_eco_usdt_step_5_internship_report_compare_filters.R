# ============================================================================
# AMM ADAPTATION -- ECO-USDT ADDITIONAL POOL -- STEP 5: INTERNSHIP REPORT / WTEYE -- COMPARE FILTERS
# ============================================================================
#
# OUR PURPOSE
# -----------
# This script is the WTEYE/internship-report counterpart of the existing
# volume-matching/C++ branch script:
#
#   amm_adaptation_step_5_compare_filters.R
#
# Step 3 produced WTEYE-style structural candidates.
# Step 4A technically validated those results.
# Step 4B reviewed the candidate groups and recorded whether each group:
#   - is consecutive in the original supplied swap sequence, and
#   - passes the WTEYE-style balance condition for token0, token1, or both.
#
# We now compare the SAME FOUR post-detection filtering choices used by the
# volume-matching/C++ branch.
#
# IMPORTANT
# ---------
# We do NOT rerun SCC detection.
# We do NOT rerun the WTEYE balance detector.
# We do NOT change the 10% balance threshold.
# We do NOT tune a filter to make the suspicious sample "look better".
#
# We apply every rule equally to the suspicious-pool and control-pool samples.
#
# OUR FOUR RULES
# --------------
# 1. CURRENT_SCC_AT_LEAST_ONE_TOKEN
#    Keep every Step-4B candidate group. This reproduces the current WTEYE-style
#    AMM adaptation after candidate review.
#
# 2. REQUIRE_CONSECUTIVE
#    Keep a group only if no original supplied pool swap is missing between its
#    first and last included swap. This tests the adjacency/consecutiveness idea
#    used in the AMM-paper adaptation.
#
# 3. REQUIRE_BOTH_TOKENS
#    Keep a group only if token0 AND token1 both satisfy the WTEYE-style balance
#    condition for the same event set.
#
#    This is an exploratory stricter rule. It is not the baseline WTEYE rule and
#    should not be described as a direct reproduction of the original WTEYE
#    framework or of the AMM paper's coarse filter.
#
# 4. REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS
#    Require both added conditions.
#
# WHY THIS STEP IS NEEDED
# -----------------------
# Step 4B showed a mixture of consecutive and nonconsecutive groups in both
# samples. It also showed that most suspicious-pool groups and every control-pool
# group pass for both tokens. This step quantifies how much each filtering
# condition changes the retained candidate groups and unique candidate swaps.
#
# This is a sensitivity/ablation analysis. It does NOT establish accuracy,
# precision, recall, false-positive rate, or confirmed wash trading because the
# two samples do not provide event-level ground-truth wash-trade labels.
#
# INPUTS
# ------
# Step-2 prepared token flows:
#   amm_adaptation_additional_pool_eco_usdt_input_suspicious_pool_token_flows.csv
#   amm_adaptation_additional_pool_eco_usdt_input_control_pool_token_flows.csv
#
# Official Step-4B review outputs:
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_group_review.csv
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_group_members.csv
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_review_summary.csv
#
# We intentionally use the Step-4B review tables here, exactly as the C++ branch's
# Step 5 uses its Step-4 review tables.
#
# OUTPUTS
# -------
# 1. One row per sample and filter variant:
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_comparison.csv
#
# 2. One row per candidate group and filter variant:
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_group_decisions.csv
#
# 3. One row per original supplied swap and filter variant:
#   amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_swap_labels.csv
#
# SCRIPT LOCATION
# ---------------
# D:/Downloads/Bachelor Thesis/Scripts/
# amm_adaptation_additional_pool_eco_usdt_step_5_internship_report_compare_filters.R
#
# HOW WE RUN IT
# -------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_additional_pool_eco_usdt_step_5_internship_report_compare_filters.R")

local({
  # ========================================================================
  # 1. WE SET OUR EXISTING PATHS AND FILENAMES
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")

  flow_files <- setNames(
    paste0("amm_adaptation_additional_pool_eco_usdt_input_", samples, "_token_flows.csv"),
    samples
  )

  group_review_file <-
    "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_group_review.csv"

  group_members_file <-
    "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_group_members.csv"

  review_summary_file <-
    "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_candidate_review_summary.csv"

  output_files <- c(
    comparison =
      "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_comparison.csv",
    decisions =
      "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_group_decisions.csv",
    swap_labels =
      "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_swap_labels.csv"
  )

  input_files <- c(
    flow_files,
    group_review_file,
    group_members_file,
    review_summary_file
  )

  input_paths <- file.path(
    cleaned_dir,
    unname(input_files)
  )

  cat("AMM ADAPTATION -- ECO-USDT ADDITIONAL POOL -- STEP 5: INTERNSHIP REPORT / WTEYE -- COMPARE FILTERS\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")

  if (!dir.exists(cleaned_dir) ||
      !all(file.exists(input_paths))) {
    missing <- input_paths[!file.exists(input_paths)]

    stop(
      paste0(
        "A required Step-2 or Step-4B file is missing:\n",
        paste(missing, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # We fingerprint every input at the beginning and compare again before writing.
  input_md5_before <- unname(
    tools::md5sum(input_paths)
  )

  # ========================================================================
  # 2. WE DEFINE SAFE READING AND CONVERSION HELPERS
  # ========================================================================
  read_text_csv <- function(filename) {
    utils::read.csv(
      file.path(cleaned_dir, filename),
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  }

  require_columns <- function(table, columns, filename) {
    missing <- setdiff(columns, names(table))

    if (length(missing)) {
      stop(
        paste(
          filename,
          "is missing:",
          paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  as_index <- function(values, field) {
    numbers <- suppressWarnings(
      as.numeric(values)
    )

    if (any(
      !is.finite(numbers) |
        numbers < 0 |
        numbers != floor(numbers) |
        numbers > .Machine$integer.max
    )) {
      stop(
        paste("Invalid index in", field),
        call. = FALSE
      )
    }

    as.integer(numbers)
  }

  as_number <- function(values, field) {
    numbers <- suppressWarnings(
      as.numeric(values)
    )

    if (any(!is.finite(numbers))) {
      stop(
        paste("Invalid number in", field),
        call. = FALSE
      )
    }

    numbers
  }

  as_yes_no <- function(values, field) {
    text <- toupper(
      trimws(as.character(values))
    )

    if (any(!(text %in% c("TRUE", "FALSE")))) {
      stop(
        paste("Invalid TRUE/FALSE in", field),
        call. = FALSE
      )
    }

    text == "TRUE"
  }

  join_values <- function(values) {
    values <- unique(
      as.character(values)
    )

    values <- values[
      !is.na(values) &
      nzchar(values)
    ]

    if (!length(values)) {
      ""
    } else {
      paste(values, collapse = ";")
    }
  }

  parse_event_key <- function(key, field = "event_key") {
    ids <- suppressWarnings(
      as.integer(
        strsplit(
          key,
          ";",
          fixed = TRUE
        )[[1L]]
      )
    )

    if (!length(ids) ||
        anyNA(ids) ||
        any(ids <= 0L) ||
        anyDuplicated(ids) ||
        !identical(ids, sort(ids))) {
      stop(
        paste("Invalid", field, ":", key),
        call. = FALSE
      )
    }

    ids
  }

  # ========================================================================
  # 3. WE READ AND VALIDATE THE OFFICIAL STEP-4B GROUP TABLES
  # ========================================================================
  groups <- read_text_csv(
    group_review_file
  )

  members <- read_text_csv(
    group_members_file
  )

  previous_summary <- read_text_csv(
    review_summary_file
  )

  require_columns(
    groups,
    c(
      "sample",
      "candidate_group_id",
      "event_key",
      "consecutive_group",
      "both_tokens_pass",
      "matched_token0",
      "matched_token1",
      "confirmation_status"
    ),
    group_review_file
  )

  require_columns(
    members,
    c(
      "sample",
      "candidate_group_id",
      "swap_index",
      "transaction_hash",
      "initiator_address"
    ),
    group_members_file
  )

  require_columns(
    previous_summary,
    c(
      "sample",
      "candidate_groups",
      "unique_candidate_swap_events"
    ),
    review_summary_file
  )

  if (anyDuplicated(groups$candidate_group_id) ||
      any(groups$confirmation_status !=
            "NOT_CONFIRMED_WASH_TRADING") ||
      !all(groups$sample %in% samples) ||
      !all(members$sample %in% samples) ||
      !setequal(previous_summary$sample, samples)) {
    stop(
      "The Step-4B group tables have inconsistent identifiers or status.",
      call. = FALSE
    )
  }

  groups$consecutive_group <- as_yes_no(
    groups$consecutive_group,
    "consecutive_group"
  )

  groups$both_tokens_pass <- as_yes_no(
    groups$both_tokens_pass,
    "both_tokens_pass"
  )

  groups$matched_token0 <- as_yes_no(
    groups$matched_token0,
    "matched_token0"
  )

  groups$matched_token1 <- as_yes_no(
    groups$matched_token1,
    "matched_token1"
  )

  members$swap_index <- as_index(
    members$swap_index,
    "member swap_index"
  )

  previous_summary$candidate_groups <- as_index(
    previous_summary$candidate_groups,
    "previous candidate_groups"
  )

  previous_summary$unique_candidate_swap_events <- as_index(
    previous_summary$unique_candidate_swap_events,
    "previous unique_candidate_swap_events"
  )

  # Every group must have at least one member row, and its member swap IDs must
  # reproduce its event_key exactly. Splitting once avoids repeatedly scanning
  # the full membership table; this is a speed optimization only and leaves the
  # comparison logic unchanged.
  member_ids_by_group <- split(
    members$swap_index,
    members$candidate_group_id
  )

  for (row in seq_len(nrow(groups))) {
    group_id <- groups$candidate_group_id[row]

    recorded_ids <- parse_event_key(
      groups$event_key[row],
      paste(group_id, "event_key")
    )

    member_ids <- sort(
      member_ids_by_group[[group_id]]
    )

    if (!length(member_ids) ||
        anyDuplicated(member_ids) ||
        !identical(
          recorded_ids,
          member_ids
        )) {
      stop(
        paste(
          "Step-4B member rows disagree with event_key for",
          group_id
        ),
        call. = FALSE
      )
    }
  }

  # No member row may refer to a group that is absent from group_review.
  if (any(
    !(members$candidate_group_id %in%
        groups$candidate_group_id)
  )) {
    stop(
      "A Step-4B member row refers to an unknown candidate group.",
      call. = FALSE
    )
  }

  # The Step-4B group table must itself reproduce the Step-4B summary before
  # we use it for any filtering experiment.
  for (sample_name in samples) {
    sample_groups <- groups[
      groups$sample == sample_name,
      ,
      drop = FALSE
    ]

    sample_summary <- previous_summary[
      previous_summary$sample == sample_name,
      ,
      drop = FALSE
    ]

    sample_member_ids <- sort(unique(
      members$swap_index[
        members$sample == sample_name
      ]
    ))

    if (nrow(sample_summary) != 1L ||
        nrow(sample_groups) !=
          sample_summary$candidate_groups ||
        length(sample_member_ids) !=
          sample_summary$unique_candidate_swap_events) {
      stop(
        paste(
          "Step-4B group/member tables do not reproduce the Step-4B summary for",
          sample_name
        ),
        call. = FALSE
      )
    }
  }

  cat("PASS: official Step-4B group tables reproduce their review summary.\n")

  # ========================================================================
  # 4. WE DEFINE THE SAME FOUR RULES AS THE C++ STEP-5 SCRIPT
  # ========================================================================
  variants <- data.frame(
    variant = c(
      "CURRENT_SCC_AT_LEAST_ONE_TOKEN",
      "REQUIRE_CONSECUTIVE",
      "REQUIRE_BOTH_TOKENS",
      "REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS"
    ),

    theoretical_role = c(
      "CURRENT_INTERNSHIP_REPORT_WTEYE_TO_AMM_ADAPTATION",
      "CURRENT_WTEYE_ADAPTATION_PLUS_AMM_PAPER_ADJACENCY",
      "EXPLORATORY_STRICTER_TWO_TOKEN_RULE",
      "EXPLORATORY_ADJACENCY_AND_TWO_TOKEN_RULE"
    ),

    limitation = c(
      "SCC_SELECTION_CAN_CREATE_GAPS_AND_ONE_TOKEN_BALANCE_CAN_RETAIN_A_GROUP",
      "ONE_TOKEN_BALANCE_CAN_STILL_RETAIN_A_GROUP",
      "TWO_TOKEN_REQUIREMENT_IS_STRICTER_THAN_BASELINE_WTEYE_ADAPTATION",
      "COMBINED_RULE_NEEDS_VALIDATION_ON_LARGER_GROUND_TRUTH_DATA"
    ),

    stringsAsFactors = FALSE
  )

  retain_group <- function(
    variant,
    group_row
  ) {
    if (variant ==
        "CURRENT_SCC_AT_LEAST_ONE_TOKEN") {
      return(TRUE)
    }

    if (variant ==
        "REQUIRE_CONSECUTIVE") {
      return(
        isTRUE(
          group_row$consecutive_group
        )
      )
    }

    if (variant ==
        "REQUIRE_BOTH_TOKENS") {
      return(
        isTRUE(
          group_row$both_tokens_pass
        )
      )
    }

    if (variant ==
        "REQUIRE_CONSECUTIVE_AND_BOTH_TOKENS") {
      return(
        isTRUE(
          group_row$consecutive_group
        ) &&
        isTRUE(
          group_row$both_tokens_pass
        )
      )
    }

    stop(
      "Unknown filtering variant.",
      call. = FALSE
    )
  }

  # ========================================================================
  # 5. WE RECORD EVERY GROUP DECISION UNDER EVERY RULE
  # ========================================================================
  decision_rows <- list()

  for (variant_row in seq_len(nrow(variants))) {
    variant <- variants$variant[variant_row]

    for (group_row in seq_len(nrow(groups))) {
      retained <- retain_group(
        variant,
        groups[group_row, , drop = FALSE]
      )

      reason <- if (retained) {
        "RETAINED_ALL_REQUIRED_CONDITIONS_PASS"
      } else if (
        !groups$consecutive_group[group_row] &&
        !groups$both_tokens_pass[group_row]
      ) {
        "REMOVED_NONCONSECUTIVE_AND_ONLY_ONE_TOKEN_BALANCES"
      } else if (
        !groups$consecutive_group[group_row]
      ) {
        "REMOVED_NONCONSECUTIVE_GROUP"
      } else {
        "REMOVED_ONLY_ONE_TOKEN_BALANCES"
      }

      decision_rows[[length(decision_rows) + 1L]] <-
        data.frame(
          variant = variant,
          theoretical_role =
            variants$theoretical_role[variant_row],
          sample =
            groups$sample[group_row],
          candidate_group_id =
            groups$candidate_group_id[group_row],
          event_key =
            groups$event_key[group_row],
          consecutive_group =
            groups$consecutive_group[group_row],
          both_tokens_pass =
            groups$both_tokens_pass[group_row],
          matched_token0 =
            groups$matched_token0[group_row],
          matched_token1 =
            groups$matched_token1[group_row],
          retained = retained,
          decision_reason = reason,
          limitation =
            variants$limitation[variant_row],
          status =
            "EXPLORATORY_FILTER_COMPARISON",
          stringsAsFactors = FALSE
        )
    }
  }

  decisions <- do.call(
    rbind,
    decision_rows
  )

  rownames(decisions) <- NULL

  # ========================================================================
  # 6. WE CREATE UNIQUE SWAP LABELS AND SAMPLE-LEVEL COMPARISONS
  # ========================================================================
  comparison_rows <- list()
  swap_label_rows <- list()

  for (sample_name in samples) {
    flows <- read_text_csv(
      flow_files[[sample_name]]
    )

    require_columns(
      flows,
      c(
        "sample",
        "swap_index",
        "event_id",
        "transaction_hash",
        "initiator_address",
        "token_slot",
        "amount"
      ),
      flow_files[[sample_name]]
    )

    if (any(flows$sample != sample_name) ||
        any(!(flows$token_slot %in%
              c("token0", "token1")))) {
      stop(
        paste(
          "Invalid sample/token identifiers for",
          sample_name
        ),
        call. = FALSE
      )
    }

    flows$swap_index <- as_index(
      flows$swap_index,
      "flow swap_index"
    )

    flows$amount <- as_number(
      flows$amount,
      "flow amount"
    )

    if (any(flows$amount <= 0)) {
      stop(
        paste(
          "Non-positive computational amount in",
          sample_name
        ),
        call. = FALSE
      )
    }

    token0 <- flows[
      flows$token_slot == "token0",
      ,
      drop = FALSE
    ]

    token1 <- flows[
      flows$token_slot == "token1",
      ,
      drop = FALSE
    ]

    token0 <- token0[
      order(token0$swap_index),
      ,
      drop = FALSE
    ]

    token1 <- token1[
      order(token1$swap_index),
      ,
      drop = FALSE
    ]

    # The two token rows must represent the same original swap list.
    if (!nrow(token0) ||
        !identical(
          token0$swap_index,
          token1$swap_index
        ) ||
        !identical(
          token0$event_id,
          token1$event_id
        ) ||
        !identical(
          token0$transaction_hash,
          token1$transaction_hash
        ) ||
        !identical(
          token0$initiator_address,
          token1$initiator_address
        ) ||
        anyDuplicated(token0$swap_index)) {
      stop(
        paste(
          "Token rows do not map one-to-one for",
          sample_name
        ),
        call. = FALSE
      )
    }

    # Step-4B members must refer only to original supplied swaps.
    member_ids_for_sample <- unique(
      members$swap_index[
        members$sample == sample_name
      ]
    )

    if (any(
      !(member_ids_for_sample %in%
        token0$swap_index)
    )) {
      stop(
        paste(
          "Step-4B member rows contain an unknown original swap for",
          sample_name
        ),
        call. = FALSE
      )
    }

    for (variant_row in seq_len(nrow(variants))) {
      variant <- variants$variant[variant_row]

      retained_groups <-
        decisions$candidate_group_id[
          decisions$sample == sample_name &
          decisions$variant == variant &
          decisions$retained
        ]

      retained_members <- members[
        members$sample == sample_name &
        members$candidate_group_id %in%
          retained_groups,
        ,
        drop = FALSE
      ]

      selected_ids <- sort(unique(
        retained_members$swap_index
      ))

      # We label EVERY original supplied swap. This prevents overlapping groups
      # from double-counting the same swap in the sample-level results.
      one_variant_labels <- data.frame(
        variant = variant,
        sample = sample_name,
        swap_index = token0$swap_index,
        event_id = token0$event_id,
        transaction_hash =
          token0$transaction_hash,
        initiator_address =
          token0$initiator_address,
        retained_candidate =
          token0$swap_index %in%
            selected_ids,
        retained_group_ids = "",
        stringsAsFactors = FALSE
      )

      # Build the swap-to-group mapping once. This gives the same labels as the
      # earlier row-by-row scan but scales to the larger ECO-USDT sample.
      memberships_by_swap <- split(
        retained_members$candidate_group_id,
        retained_members$swap_index
      )

      one_variant_labels$retained_group_ids <- vapply(
        one_variant_labels$swap_index,
        function(swap_id) {
          memberships <- memberships_by_swap[[
            as.character(swap_id)
          ]]

          if (is.null(memberships)) {
            ""
          } else {
            join_values(memberships)
          }
        },
        character(1)
      )

      swap_label_rows[[
        length(swap_label_rows) + 1L
      ]] <- one_variant_labels

      selected <-
        token0$swap_index %in%
        selected_ids

      selected_hashes <- unique(
        token0$transaction_hash[selected]
      )

      selected_initiators <- unique(
        token0$initiator_address[selected]
      )

      comparison_rows[[
        length(comparison_rows) + 1L
      ]] <- data.frame(
        variant = variant,
        theoretical_role =
          variants$theoretical_role[variant_row],
        sample = sample_name,
        retained_groups =
          length(retained_groups),
        unique_candidate_swap_events =
          sum(selected),
        unique_candidate_transactions =
          length(selected_hashes),
        unique_candidate_initiators =
          length(selected_initiators),
        candidate_event_share_pct =
          100 * sum(selected) /
          nrow(token0),
        token0_candidate_volume_share_pct =
          if (any(selected)) {
            100 *
            sum(token0$amount[selected]) /
            sum(token0$amount)
          } else {
            0
          },
        token1_candidate_volume_share_pct =
          if (any(selected)) {
            100 *
            sum(token1$amount[selected]) /
            sum(token1$amount)
          } else {
            0
          },
        limitation =
          variants$limitation[variant_row],
        interpretation_status =
          "EXPLORATORY_NOT_ACCURACY_VALIDATION",
        stringsAsFactors = FALSE
      )
    }
  }

  comparison <- do.call(
    rbind,
    comparison_rows
  )

  swap_labels <- do.call(
    rbind,
    swap_label_rows
  )

  rownames(comparison) <- NULL
  rownames(swap_labels) <- NULL

  # ========================================================================
  # 7. THE BASELINE MUST REPRODUCE STEP-4B EXACTLY
  # ========================================================================
  baseline <- comparison[
    comparison$variant ==
      "CURRENT_SCC_AT_LEAST_ONE_TOKEN",
    ,
    drop = FALSE
  ]

  previous_event_counts <- setNames(
    previous_summary$unique_candidate_swap_events,
    previous_summary$sample
  )

  previous_group_counts <- setNames(
    previous_summary$candidate_groups,
    previous_summary$sample
  )

  if (!setequal(
        baseline$sample,
        samples
      ) ||
      !identical(
        as.integer(
          baseline$unique_candidate_swap_events
        ),
        as.integer(
          previous_event_counts[
            baseline$sample
          ]
        )
      ) ||
      !identical(
        as.integer(
          baseline$retained_groups
        ),
        as.integer(
          previous_group_counts[
            baseline$sample
          ]
        )
      )) {
    stop(
      "The current-rule baseline does not reproduce Step-4B group/event counts.",
      call. = FALSE
    )
  }

  cat("PASS: current-rule baseline reproduces Step-4B group and event counts.\n")

  # Ensure none of our inputs changed while this comparison was running.
  if (!identical(
    input_md5_before,
    unname(
      tools::md5sum(input_paths)
    )
  )) {
    stop(
      "An input changed during this comparison. No reports were written.",
      call. = FALSE
    )
  }

  # ========================================================================
  # 8. WE SAVE THREE REPORTS
  # ========================================================================
  write_report <- function(
    table,
    filename
  ) {
    table_for_disk <- table

    for (field in names(table_for_disk)) {
      if (is.double(
        table_for_disk[[field]]
      )) {
        table_for_disk[[field]] <-
          sprintf(
            "%.17g",
            table_for_disk[[field]]
          )
      }
    }

    path <- file.path(
      cleaned_dir,
      filename
    )

    if (file.exists(path)) {
      cat(
        "Refreshing our generated report:",
        filename,
        "\n"
      )
    }

    utils::write.csv(
      table_for_disk,
      path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    cat(
      "Saved:",
      path,
      "\n"
    )
  }

  write_report(
    comparison,
    output_files[["comparison"]]
  )

  write_report(
    decisions,
    output_files[["decisions"]]
  )

  write_report(
    swap_labels,
    output_files[["swap_labels"]]
  )

  # ========================================================================
  # 9. WE PRINT THE MAIN FILTER COMPARISON TO THE R CONSOLE
  # ========================================================================
  cat("\nFILTER COMPARISON\n")

  print(
    comparison[, c(
      "variant",
      "sample",
      "retained_groups",
      "unique_candidate_swap_events"
    )],
    row.names = FALSE
  )

  cat("\nSTEP 5 COMPLETE: four WTEYE-style filtering choices were compared equally.\n")
  cat("Input fingerprints unchanged: TRUE\n")
  cat("No SCC detection or WTEYE balance detection was rerun.\n")
  cat("These are sensitivity results, not proof of accuracy or confirmed wash trading.\n")
  cat("Send the complete console output and all three Step-5 CSVs for review.\n")
})
