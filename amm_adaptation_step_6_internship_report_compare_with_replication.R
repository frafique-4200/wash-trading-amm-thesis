# ============================================================================
# AMM ADAPTATION -- STEP 6: INTERNSHIP REPORT / WTEYE -- COMPARE WITH REPLICATION
# ============================================================================
#
# OUR PURPOSE
# -----------
# This script is the internship-report / WTEYE counterpart of:
#
#   amm_adaptation_step_6_compare_with_replication.R
#
# The existing Step 6 compares:
#
#   historical C++ / volume-matching AMM adaptation
#       VS
#   reduced-scale AMM-paper replication
#
# This parallel Step 6 compares:
#
#   internship-report / WTEYE AMM adaptation
#       VS
#   the SAME reduced-scale AMM-paper replication
#
# We therefore keep the comparison logic and chosen Step-5 rule parallel across
# the two adaptation branches.
#
# OUR CHOSEN ADAPTATION RULE
# --------------------------
# We use:
#
#   REQUIRE_CONSECUTIVE
#
# exactly as the existing C++-branch Step 6 does.
#
# This means that the formal comparison uses WTEYE-style Step-3 candidates that
# survived the Step-5 consecutiveness requirement.
#
# We intentionally do NOT switch to REQUIRE_BOTH_TOKENS here, because doing so
# would change the formal filter between the C++ and WTEYE comparison branches
# and make the two Step-6 analyses less directly comparable.
#
# WHAT WE COMPARE
# ---------------
# For each supplied swap, we record whether it was selected by:
#
#   1. the reduced-scale AMM-paper replication;
#   2. the internship-report / WTEYE AMM adaptation under REQUIRE_CONSECUTIVE.
#
# Each supplied swap is then assigned one of four mutually exclusive categories:
#
#   BOTH_METHODS
#   REPLICATION_ONLY
#   ADAPTATION_ONLY
#   NEITHER_METHOD
#
# We also compare the final candidate-group structures.
#
# IMPORTANT INTERPRETATION
# ------------------------
# The replicated AMM method is used as a REFERENCE METHOD, not as ground truth.
#
# Therefore:
# - BOTH_METHODS means the methods agree on that supplied swap;
# - REPLICATION_ONLY or ADAPTATION_ONLY means they disagree;
# - none of these categories proves or disproves wash trading.
#
# WHY THE CONTROL REPLICATION CAN END AT STAGE 2
# ----------------------------------------------
# In the existing replicated-AMM pipeline, the control pool had no swaps
# surviving Stage 2. Therefore there were no control candidates requiring the
# Stage-3 MEV review. We verify that directly from the Stage-2 summary and the
# Stage-2 surviving-swaps file before comparing methods.
#
# INPUTS IN Data/cleaned
# ----------------------
# WTEYE Step-5 outputs:
#   amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_comparison.csv
#   amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_swap_labels.csv
#   amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_group_decisions.csv
#
# AMM-paper replication outputs:
#   suspicious_pool_final_candidate_swaps.csv
#   suspicious_pool_stage3_summary.csv
#   control_pool_stage2_surviving_swaps.csv
#   control_pool_stage2_summary.csv
#
# OUR THREE OUTPUTS IN Data/cleaned
# ---------------------------------
# amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_summary.csv
#
# amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_swap_labels.csv
#
# amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_groups.csv
#
# SCRIPT LOCATION
# ---------------
# D:/Downloads/Bachelor Thesis/Scripts/
# amm_adaptation_step_6_internship_report_compare_with_replication.R
#
# HOW WE RUN IT
# -------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_6_internship_report_compare_with_replication.R")

local({
  # ========================================================================
  # 1. WE SET OUR FOLDERS, FILENAMES, AND CHOSEN FILTER
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")

  chosen_variant <- "REQUIRE_CONSECUTIVE"

  # WTEYE Step-5 outputs.
  adaptation_comparison_file <-
    "amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_comparison.csv"

  adaptation_swap_file <-
    "amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_swap_labels.csv"

  adaptation_group_file <-
    "amm_adaptation_output_both_pools_internship_report_wteye_filter_variant_group_decisions.csv"

  # Replicated-AMM outputs. These are intentionally the SAME replication files
  # used by the C++-branch Step 6.
  replication_suspicious_swaps_file <-
    "suspicious_pool_final_candidate_swaps.csv"

  replication_suspicious_summary_file <-
    "suspicious_pool_stage3_summary.csv"

  replication_control_swaps_file <-
    "control_pool_stage2_surviving_swaps.csv"

  replication_control_summary_file <-
    "control_pool_stage2_summary.csv"

  output_files <- c(
    summary =
      "amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_summary.csv",

    swap_labels =
      "amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_swap_labels.csv",

    groups =
      "amm_adaptation_output_both_pools_internship_report_wteye_replication_comparison_groups.csv"
  )

  input_files <- c(
    adaptation_comparison_file,
    adaptation_swap_file,
    adaptation_group_file,
    replication_suspicious_swaps_file,
    replication_suspicious_summary_file,
    replication_control_swaps_file,
    replication_control_summary_file
  )

  input_paths <- file.path(
    cleaned_dir,
    input_files
  )

  cat("AMM ADAPTATION -- STEP 6: INTERNSHIP REPORT / WTEYE -- COMPARE WITH AMM REPLICATION\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")
  cat("Chosen WTEYE adaptation rule:", chosen_variant, "\n")

  if (!dir.exists(cleaned_dir) ||
      !all(file.exists(input_paths))) {

    missing_files <- input_files[
      !file.exists(input_paths)
    ]

    stop(
      paste(
        "A required WTEYE Step-5 or replication file is missing:",
        paste(missing_files, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Fingerprint all comparison inputs before doing any work.
  input_md5_before <- unname(
    tools::md5sum(input_paths)
  )

  # ========================================================================
  # 2. HELPER FUNCTIONS
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

  require_columns <- function(
    table,
    required_columns,
    filename
  ) {
    missing_columns <- setdiff(
      required_columns,
      names(table)
    )

    if (length(missing_columns)) {
      stop(
        paste(
          filename,
          "is missing:",
          paste(missing_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  as_index <- function(values, field_name) {
    if (!length(values)) {
      return(integer(0))
    }

    numbers <- suppressWarnings(
      as.numeric(values)
    )

    invalid <-
      !is.finite(numbers) |
      numbers < 0 |
      numbers != floor(numbers) |
      numbers > .Machine$integer.max

    if (any(invalid)) {
      stop(
        paste(
          "We found an invalid whole number in",
          field_name
        ),
        call. = FALSE
      )
    }

    as.integer(numbers)
  }

  as_number <- function(values, field_name) {
    if (!length(values)) {
      return(numeric(0))
    }

    numbers <- suppressWarnings(
      as.numeric(values)
    )

    if (any(!is.finite(numbers))) {
      stop(
        paste(
          "We found an invalid number in",
          field_name
        ),
        call. = FALSE
      )
    }

    numbers
  }

  as_yes_no <- function(values, field_name) {
    if (!length(values)) {
      return(logical(0))
    }

    text <- toupper(
      trimws(
        as.character(values)
      )
    )

    if (any(
      !(text %in% c("TRUE", "FALSE"))
    )) {
      stop(
        paste(
          "We found an invalid TRUE/FALSE value in",
          field_name
        ),
        call. = FALSE
      )
    }

    text == "TRUE"
  }

  normalize_hash <- function(values, field_name) {
    if (!length(values)) {
      return(character(0))
    }

    hashes <- tolower(
      trimws(
        as.character(values)
      )
    )

    valid <- grepl(
      "^0x[0-9a-f]{64}$",
      hashes
    )

    if (any(!valid)) {
      stop(
        paste(
          "We found an invalid transaction hash in",
          field_name
        ),
        call. = FALSE
      )
    }

    hashes
  }

  join_values <- function(values) {
    values <- sort(
      unique(
        as.character(values)
      )
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

  safe_percentage <- function(
    numerator,
    denominator
  ) {
    if (denominator == 0) {
      NA_real_
    } else {
      100 * numerator / denominator
    }
  }

  describe_set_relation <- function(
    replication_hashes,
    adaptation_hashes
  ) {
    replication_hashes <- unique(
      replication_hashes
    )

    adaptation_hashes <- unique(
      adaptation_hashes
    )

    if (setequal(
      replication_hashes,
      adaptation_hashes
    )) {
      return("EXACT_SET_MATCH")
    }

    if (all(
      adaptation_hashes %in%
      replication_hashes
    )) {
      return(
        "ADAPTATION_SUBSET_OF_REPLICATION"
      )
    }

    if (all(
      replication_hashes %in%
      adaptation_hashes
    )) {
      return(
        "REPLICATION_SUBSET_OF_ADAPTATION"
      )
    }

    if (!length(
      intersect(
        replication_hashes,
        adaptation_hashes
      )
    )) {
      return("DISJOINT_SETS")
    }

    "PARTIAL_OVERLAP"
  }

  split_event_key <- function(
    event_key,
    field_name
  ) {
    if (!nzchar(event_key)) {
      return(integer(0))
    }

    as_index(
      strsplit(
        event_key,
        ";",
        fixed = TRUE
      )[[1L]],
      field_name
    )
  }

  # ========================================================================
  # 3. READ AND VALIDATE WTEYE STEP-5 RESULTS
  # ========================================================================
  adaptation_comparison_all <-
    read_text_csv(
      adaptation_comparison_file
    )

  adaptation_swaps_all <-
    read_text_csv(
      adaptation_swap_file
    )

  adaptation_groups_all <-
    read_text_csv(
      adaptation_group_file
    )

  require_columns(
    adaptation_comparison_all,
    c(
      "variant",
      "sample",
      "retained_groups",
      "unique_candidate_swap_events"
    ),
    adaptation_comparison_file
  )

  require_columns(
    adaptation_swaps_all,
    c(
      "variant",
      "sample",
      "swap_index",
      "event_id",
      "transaction_hash",
      "initiator_address",
      "retained_candidate",
      "retained_group_ids"
    ),
    adaptation_swap_file
  )

  require_columns(
    adaptation_groups_all,
    c(
      "variant",
      "sample",
      "candidate_group_id",
      "event_key",
      "retained"
    ),
    adaptation_group_file
  )

  adaptation_comparison <-
    adaptation_comparison_all[
      adaptation_comparison_all$variant ==
        chosen_variant,
      ,
      drop = FALSE
    ]

  adaptation_swaps <-
    adaptation_swaps_all[
      adaptation_swaps_all$variant ==
        chosen_variant,
      ,
      drop = FALSE
    ]

  adaptation_groups <-
    adaptation_groups_all[
      adaptation_groups_all$variant ==
        chosen_variant,
      ,
      drop = FALSE
    ]

  if (!nrow(adaptation_comparison) ||
      !nrow(adaptation_swaps) ||
      !setequal(
        adaptation_comparison$sample,
        samples
      ) ||
      !setequal(
        adaptation_swaps$sample,
        samples
      )) {

    stop(
      "Our chosen WTEYE Step-5 variant does not contain both samples.",
      call. = FALSE
    )
  }

  adaptation_comparison$retained_groups <-
    as_index(
      adaptation_comparison$retained_groups,
      "WTEYE Step-5 retained_groups"
    )

  adaptation_comparison$unique_candidate_swap_events <-
    as_index(
      adaptation_comparison$unique_candidate_swap_events,
      "WTEYE Step-5 unique_candidate_swap_events"
    )

  adaptation_swaps$swap_index <-
    as_index(
      adaptation_swaps$swap_index,
      "WTEYE adaptation swap_index"
    )

  adaptation_swaps$transaction_hash <-
    normalize_hash(
      adaptation_swaps$transaction_hash,
      "WTEYE adaptation transaction_hash"
    )

  adaptation_swaps$retained_candidate <-
    as_yes_no(
      adaptation_swaps$retained_candidate,
      "WTEYE adaptation retained_candidate"
    )

  adaptation_groups$retained <-
    as_yes_no(
      adaptation_groups$retained,
      "WTEYE adaptation group retained"
    )

  # We expect one supplied swap per transaction inside each sample.
  swap_keys <- paste(
    adaptation_swaps$sample,
    adaptation_swaps$swap_index,
    sep = "|"
  )

  transaction_keys <- paste(
    adaptation_swaps$sample,
    adaptation_swaps$transaction_hash,
    sep = "|"
  )

  if (anyDuplicated(swap_keys) ||
      anyDuplicated(transaction_keys) ||
      any(
        !(adaptation_swaps$sample %in%
          samples)
      )) {

    stop(
      "Our WTEYE Step-5 swap universe contains duplicate or invalid identifiers.",
      call. = FALSE
    )
  }

  # Keep only groups retained under REQUIRE_CONSECUTIVE.
  adaptation_groups_final <-
    adaptation_groups[
      adaptation_groups$retained,
      ,
      drop = FALSE
    ]

  # Independently check that Step-5 labels and retained-group decisions reproduce
  # the Step-5 comparison table for the chosen rule.
  for (sample_name in samples) {
    one_comparison <-
      adaptation_comparison[
        adaptation_comparison$sample ==
          sample_name,
        ,
        drop = FALSE
      ]

    one_swaps <-
      adaptation_swaps[
        adaptation_swaps$sample ==
          sample_name,
        ,
        drop = FALSE
      ]

    one_groups <-
      adaptation_groups_final[
        adaptation_groups_final$sample ==
          sample_name,
        ,
        drop = FALSE
      ]

    if (nrow(one_comparison) != 1L ||
        sum(
          one_swaps$retained_candidate
        ) !=
          one_comparison$unique_candidate_swap_events ||
        nrow(one_groups) !=
          one_comparison$retained_groups) {

      stop(
        paste(
          "WTEYE Step-5 labels/groups do not reproduce the Step-5 comparison row for",
          sample_name
        ),
        call. = FALSE
      )
    }
  }

  cat(
    "PASS: chosen WTEYE Step-5 rule reproduces its comparison counts.\n"
  )

  # ========================================================================
  # 4. READ AND VALIDATE THE REPLICATION RESULTS
  # ========================================================================
  replication_suspicious_swaps <-
    read_text_csv(
      replication_suspicious_swaps_file
    )

  replication_suspicious_summary <-
    read_text_csv(
      replication_suspicious_summary_file
    )

  replication_control_swaps <-
    read_text_csv(
      replication_control_swaps_file
    )

  replication_control_summary <-
    read_text_csv(
      replication_control_summary_file
    )

  require_columns(
    replication_suspicious_swaps,
    c(
      "candidate_id",
      "swap_sequence",
      "evt_tx_hash"
    ),
    replication_suspicious_swaps_file
  )

  require_columns(
    replication_suspicious_summary,
    c(
      "candidate_id",
      "stage3_resolved",
      "stage3_pass"
    ),
    replication_suspicious_summary_file
  )

  require_columns(
    replication_control_swaps,
    c(
      "candidate_id",
      "swap_sequence",
      "evt_tx_hash"
    ),
    replication_control_swaps_file
  )

  require_columns(
    replication_control_summary,
    c(
      "candidate_id",
      "stage2_pass",
      "decision_route"
    ),
    replication_control_summary_file
  )

  replication_suspicious_swaps$candidate_id <-
    as_index(
      replication_suspicious_swaps$candidate_id,
      "suspicious replication candidate_id"
    )

  replication_suspicious_swaps$swap_sequence <-
    as_index(
      replication_suspicious_swaps$swap_sequence,
      "suspicious replication swap_sequence"
    )

  replication_suspicious_swaps$evt_tx_hash <-
    normalize_hash(
      replication_suspicious_swaps$evt_tx_hash,
      "suspicious replication evt_tx_hash"
    )

  replication_control_swaps$candidate_id <-
    as_index(
      replication_control_swaps$candidate_id,
      "control replication candidate_id"
    )

  replication_control_swaps$swap_sequence <-
    as_index(
      replication_control_swaps$swap_sequence,
      "control replication swap_sequence"
    )

  if (nrow(replication_control_swaps)) {
    replication_control_swaps$evt_tx_hash <-
      normalize_hash(
        replication_control_swaps$evt_tx_hash,
        "control replication evt_tx_hash"
      )
  }

  replication_suspicious_summary$candidate_id <-
    as_index(
      replication_suspicious_summary$candidate_id,
      "suspicious Stage-3 candidate_id"
    )

  replication_suspicious_summary$stage3_resolved <-
    as_yes_no(
      replication_suspicious_summary$stage3_resolved,
      "suspicious stage3_resolved"
    )

  replication_suspicious_summary$stage3_pass <-
    as_yes_no(
      replication_suspicious_summary$stage3_pass,
      "suspicious stage3_pass"
    )

  replication_control_summary$candidate_id <-
    as_index(
      replication_control_summary$candidate_id,
      "control Stage-2 candidate_id"
    )

  replication_control_summary$stage2_pass <-
    as_yes_no(
      replication_control_summary$stage2_pass,
      "control stage2_pass"
    )

  suspicious_passing_ids <-
    replication_suspicious_summary$candidate_id[
      replication_suspicious_summary$stage3_resolved &
      replication_suspicious_summary$stage3_pass
    ]

  suspicious_final_ids <- unique(
    replication_suspicious_swaps$candidate_id
  )

  if (!setequal(
    suspicious_passing_ids,
    suspicious_final_ids
  )) {
    stop(
      "Our suspicious replication Stage-3 summary and final swaps disagree.",
      call. = FALSE
    )
  }

  control_passing_ids <-
    replication_control_summary$candidate_id[
      replication_control_summary$stage2_pass
    ]

  control_final_ids <- unique(
    replication_control_swaps$candidate_id
  )

  if (!setequal(
    control_passing_ids,
    control_final_ids
  )) {
    stop(
      "Our control replication Stage-2 summary and surviving swaps disagree.",
      call. = FALSE
    )
  }

  control_stage3_required <-
    length(control_passing_ids) > 0L

  if (control_stage3_required) {
    stop(
      paste(
        "A control replication candidate survived Stage 2.",
        "Stage 3 is therefore required before a final comparison."
      ),
      call. = FALSE
    )
  }

  cat(
    "Replication checkpoint: suspicious Stage-3 final swaps =",
    nrow(replication_suspicious_swaps),
    "\n"
  )

  cat(
    "Replication checkpoint: control Stage-2 surviving swaps =",
    nrow(replication_control_swaps),
    "\n"
  )

  cat(
    "Control Stage 3 required:",
    control_stage3_required,
    "\n"
  )

  # ========================================================================
  # 5. CREATE ONE COMMON REPLICATION TABLE
  # ========================================================================
  make_replication_rows <- function(
    table,
    sample_name
  ) {
    if (!nrow(table)) {
      return(
        data.frame(
          sample = character(0),
          replication_group_id = character(0),
          replication_swap_index = integer(0),
          transaction_hash = character(0),
          stringsAsFactors = FALSE
        )
      )
    }

    data.frame(
      sample = sample_name,
      replication_group_id = paste0(
        "replication_",
        sample_name,
        "_C",
        sprintf(
          "%03d",
          table$candidate_id
        )
      ),
      replication_swap_index =
        table$swap_sequence,
      transaction_hash =
        table$evt_tx_hash,
      stringsAsFactors = FALSE
    )
  }

  replication_swaps <- rbind(
    make_replication_rows(
      replication_suspicious_swaps,
      "suspicious_pool"
    ),
    make_replication_rows(
      replication_control_swaps,
      "control_pool"
    )
  )

  rownames(replication_swaps) <- NULL

  # Every replication result must refer to exactly one swap in the same supplied
  # swap universe used by our WTEYE adaptation.
  for (row_number in seq_len(
    nrow(replication_swaps)
  )) {
    same_sample <-
      adaptation_swaps$sample ==
      replication_swaps$sample[
        row_number
      ]

    matching_row <- which(
      same_sample &
      adaptation_swaps$transaction_hash ==
        replication_swaps$transaction_hash[
          row_number
        ]
    )

    if (length(matching_row) != 1L) {
      stop(
        "A replication transaction is missing or duplicated in our WTEYE Step-5 swap universe.",
        call. = FALSE
      )
    }

    if (
      adaptation_swaps$swap_index[
        matching_row
      ] !=
      replication_swaps$replication_swap_index[
        row_number
      ]
    ) {
      stop(
        "The replication and WTEYE-adaptation swap indices disagree.",
        call. = FALSE
      )
    }
  }

  cat(
    "PASS: replication final swaps map exactly into the WTEYE Step-5 swap universe.\n"
  )

  # ========================================================================
  # 6. LABEL EVERY SUPPLIED SWAP UNDER BOTH METHODS
  # ========================================================================
  comparison_labels <-
    adaptation_swaps[, c(
      "sample",
      "swap_index",
      "event_id",
      "transaction_hash",
      "initiator_address",
      "retained_candidate",
      "retained_group_ids"
    )]

  names(comparison_labels)[
    names(comparison_labels) ==
      "retained_candidate"
  ] <- "adaptation_candidate"

  names(comparison_labels)[
    names(comparison_labels) ==
      "retained_group_ids"
  ] <- "adaptation_group_ids"

  comparison_labels$replication_candidate <-
    FALSE

  comparison_labels$replication_group_ids <-
    ""

  for (row_number in seq_len(
    nrow(comparison_labels)
  )) {
    matching_replication_rows <- which(
      replication_swaps$sample ==
        comparison_labels$sample[
          row_number
        ] &
      replication_swaps$transaction_hash ==
        comparison_labels$transaction_hash[
          row_number
        ]
    )

    comparison_labels$replication_candidate[
      row_number
    ] <- length(
      matching_replication_rows
    ) > 0L

    comparison_labels$replication_group_ids[
      row_number
    ] <- join_values(
      replication_swaps$replication_group_id[
        matching_replication_rows
      ]
    )
  }

  comparison_labels$agreement_category <-
    ifelse(
      comparison_labels$replication_candidate &
      comparison_labels$adaptation_candidate,

      "BOTH_METHODS",

      ifelse(
        comparison_labels$replication_candidate &
        !comparison_labels$adaptation_candidate,

        "REPLICATION_ONLY",

        ifelse(
          !comparison_labels$replication_candidate &
          comparison_labels$adaptation_candidate,

          "ADAPTATION_ONLY",

          "NEITHER_METHOD"
        )
      )
    )

  comparison_labels$adaptation_method <-
    "INTERNSHIP_REPORT_WTEYE_REQUIRE_CONSECUTIVE"

  comparison_labels$replication_method <-
    "REDUCED_SCALE_AMM_PAPER_REPLICATION"

  comparison_labels$comparison_status <-
    "METHOD_COMPARISON_NOT_GROUND_TRUTH"

  comparison_labels <- comparison_labels[, c(
    "sample",
    "swap_index",
    "event_id",
    "transaction_hash",
    "initiator_address",
    "replication_candidate",
    "adaptation_candidate",
    "agreement_category",
    "replication_group_ids",
    "adaptation_group_ids",
    "replication_method",
    "adaptation_method",
    "comparison_status"
  )]

  comparison_labels <- comparison_labels[
    order(
      match(
        comparison_labels$sample,
        samples
      ),
      comparison_labels$swap_index
    ),
    ,
    drop = FALSE
  ]

  rownames(comparison_labels) <- NULL

  # ========================================================================
  # 7. SAMPLE-LEVEL COMPARISON SUMMARY
  # ========================================================================
  summary_rows <- list()

  for (sample_name in samples) {
    sample_labels <- comparison_labels[
      comparison_labels$sample ==
        sample_name,
      ,
      drop = FALSE
    ]

    replication_hashes <-
      sample_labels$transaction_hash[
        sample_labels$replication_candidate
      ]

    adaptation_hashes <-
      sample_labels$transaction_hash[
        sample_labels$adaptation_candidate
      ]

    both_count <- sum(
      sample_labels$agreement_category ==
        "BOTH_METHODS"
    )

    replication_only_count <- sum(
      sample_labels$agreement_category ==
        "REPLICATION_ONLY"
    )

    adaptation_only_count <- sum(
      sample_labels$agreement_category ==
        "ADAPTATION_ONLY"
    )

    neither_count <- sum(
      sample_labels$agreement_category ==
        "NEITHER_METHOD"
    )

    union_count <-
      both_count +
      replication_only_count +
      adaptation_only_count

    replication_group_count <- length(
      unique(
        replication_swaps$replication_group_id[
          replication_swaps$sample ==
            sample_name
        ]
      )
    )

    adaptation_group_count <- length(
      unique(
        adaptation_groups_final$candidate_group_id[
          adaptation_groups_final$sample ==
            sample_name
        ]
      )
    )

    summary_rows[[
      length(summary_rows) + 1L
    ]] <- data.frame(
      sample = sample_name,

      input_swap_events =
        nrow(sample_labels),

      replication_final_groups =
        replication_group_count,

      adaptation_final_groups =
        adaptation_group_count,

      replication_candidate_swaps =
        length(
          unique(replication_hashes)
        ),

      adaptation_candidate_swaps =
        length(
          unique(adaptation_hashes)
        ),

      swaps_selected_by_both =
        both_count,

      replication_only_swaps =
        replication_only_count,

      adaptation_only_swaps =
        adaptation_only_count,

      swaps_selected_by_neither =
        neither_count,

      adaptation_coverage_of_replication_pct =
        safe_percentage(
          both_count,
          length(
            unique(replication_hashes)
          )
        ),

      adaptation_candidates_also_in_replication_pct =
        safe_percentage(
          both_count,
          length(
            unique(adaptation_hashes)
          )
        ),

      jaccard_overlap_pct =
        safe_percentage(
          both_count,
          union_count
        ),

      exact_final_set_agreement =
        setequal(
          replication_hashes,
          adaptation_hashes
        ),

      final_set_relation =
        describe_set_relation(
          replication_hashes,
          adaptation_hashes
        ),

      chosen_adaptation_variant =
        chosen_variant,

      replication_final_stage =
        if (
          sample_name ==
          "suspicious_pool"
        ) {
          "STAGE_3_MEV_REVIEW_COMPLETED"
        } else {
          "STAGE_2_COMPLETE_NO_SURVIVORS_STAGE_3_NOT_NEEDED"
        },

      interpretation_status =
        "REFERENCE_METHOD_COMPARISON_NOT_ACCURACY_PROOF",

      stringsAsFactors = FALSE
    )
  }

  comparison_summary <- do.call(
    rbind,
    summary_rows
  )

  rownames(comparison_summary) <- NULL

  # ========================================================================
  # 8. COMPARE THE FINAL GROUP STRUCTURES
  # ========================================================================
  adaptation_group_sets <- data.frame(
    sample =
      adaptation_groups_final$sample,

    method =
      "INTERNSHIP_REPORT_WTEYE_REQUIRE_CONSECUTIVE",

    group_id =
      adaptation_groups_final$candidate_group_id,

    event_key =
      adaptation_groups_final$event_key,

    stringsAsFactors = FALSE
  )

  replication_group_sets <- data.frame(
    sample = character(0),
    method = character(0),
    group_id = character(0),
    event_key = character(0),
    stringsAsFactors = FALSE
  )

  if (nrow(replication_swaps)) {
    for (
      replication_group_id in
      unique(
        replication_swaps$replication_group_id
      )
    ) {
      group_rows <- replication_swaps[
        replication_swaps$replication_group_id ==
          replication_group_id,
        ,
        drop = FALSE
      ]

      group_sample <- unique(
        group_rows$sample
      )

      if (length(group_sample) != 1L) {
        stop(
          "A replication group unexpectedly spans more than one sample.",
          call. = FALSE
        )
      }

      group_indices <-
        adaptation_swaps$swap_index[
          adaptation_swaps$sample ==
            group_sample &
          adaptation_swaps$transaction_hash %in%
            group_rows$transaction_hash
        ]

      replication_group_sets <- rbind(
        replication_group_sets,

        data.frame(
          sample = group_sample,

          method =
            "REDUCED_SCALE_AMM_PAPER_REPLICATION",

          group_id =
            replication_group_id,

          event_key =
            paste(
              sort(
                unique(group_indices)
              ),
              collapse = ";"
            ),

          stringsAsFactors = FALSE
        )
      )
    }
  }

  method_groups <- rbind(
    replication_group_sets,
    adaptation_group_sets
  )

  rownames(method_groups) <- NULL

  method_groups$number_of_swaps <- vapply(
    seq_len(
      nrow(method_groups)
    ),

    function(row_number) {
      length(
        split_event_key(
          method_groups$event_key[
            row_number
          ],
          paste(
            "group event_key",
            method_groups$group_id[
              row_number
            ]
          )
        )
      )
    },

    integer(1)
  )

  method_groups$exact_matching_other_group_ids <-
    ""

  method_groups$overlapping_other_group_ids <-
    ""

  method_groups$maximum_shared_swaps <-
    0L

  method_groups$group_comparison_outcome <-
    ""

  method_groups$comparison_status <-
    "METHOD_COMPARISON_NOT_GROUND_TRUTH"

  for (row_number in seq_len(
    nrow(method_groups)
  )) {
    this_indices <- split_event_key(
      method_groups$event_key[
        row_number
      ],
      paste(
        "group event_key",
        method_groups$group_id[
          row_number
        ]
      )
    )

    other_rows <- which(
      method_groups$sample ==
        method_groups$sample[
          row_number
        ] &
      method_groups$method !=
        method_groups$method[
          row_number
        ]
    )

    if (!length(other_rows)) {
      method_groups$group_comparison_outcome[
        row_number
      ] <- "NO_FINAL_GROUPS_IN_OTHER_METHOD"

      next
    }

    overlap_sizes <- vapply(
      other_rows,

      function(other_row) {
        other_indices <-
          split_event_key(
            method_groups$event_key[
              other_row
            ],
            paste(
              "group event_key",
              method_groups$group_id[
                other_row
              ]
            )
          )

        length(
          intersect(
            this_indices,
            other_indices
          )
        )
      },

      integer(1)
    )

    exact_rows <- other_rows[
      vapply(
        other_rows,

        function(other_row) {
          setequal(
            this_indices,

            split_event_key(
              method_groups$event_key[
                other_row
              ],
              paste(
                "group event_key",
                method_groups$group_id[
                  other_row
                ]
              )
            )
          )
        },

        logical(1)
      )
    ]

    overlapping_rows <-
      other_rows[
        overlap_sizes > 0L
      ]

    method_groups$exact_matching_other_group_ids[
      row_number
    ] <- join_values(
      method_groups$group_id[
        exact_rows
      ]
    )

    method_groups$overlapping_other_group_ids[
      row_number
    ] <- join_values(
      method_groups$group_id[
        overlapping_rows
      ]
    )

    method_groups$maximum_shared_swaps[
      row_number
    ] <- max(overlap_sizes)

    method_groups$group_comparison_outcome[
      row_number
    ] <- if (length(exact_rows)) {
      "EXACT_GROUP_MATCH"
    } else if (length(overlapping_rows)) {
      "PARTIAL_GROUP_OVERLAP"
    } else {
      "NO_GROUP_OVERLAP"
    }
  }

  method_groups <- method_groups[
    order(
      match(
        method_groups$sample,
        samples
      ),
      method_groups$method,
      method_groups$group_id
    ),
    ,
    drop = FALSE
  ]

  rownames(method_groups) <- NULL

  # ========================================================================
  # 9. FINAL CONSISTENCY CHECKS
  # ========================================================================
  category_totals <- with(
    comparison_summary,

    swaps_selected_by_both +
      replication_only_swaps +
      adaptation_only_swaps +
      swaps_selected_by_neither
  )

  if (!identical(
    as.integer(category_totals),
    as.integer(
      comparison_summary$input_swap_events
    )
  )) {
    stop(
      "Our four comparison categories do not reproduce the supplied swap totals.",
      call. = FALSE
    )
  }

  for (sample_name in samples) {
    one_summary <- comparison_summary[
      comparison_summary$sample ==
        sample_name,
      ,
      drop = FALSE
    ]

    one_labels <- comparison_labels[
      comparison_labels$sample ==
        sample_name,
      ,
      drop = FALSE
    ]

    if (
      one_summary$replication_candidate_swaps !=
        sum(
          one_labels$replication_candidate
        ) ||
      one_summary$adaptation_candidate_swaps !=
        sum(
          one_labels$adaptation_candidate
        )
    ) {
      stop(
        "Our detailed labels and sample-level comparison summary disagree.",
        call. = FALSE
      )
    }
  }

  # Final check that all seven inputs remained unchanged.
  if (!identical(
    input_md5_before,
    unname(
      tools::md5sum(input_paths)
    )
  )) {
    stop(
      "An input changed during Step 6. No reports were written.",
      call. = FALSE
    )
  }

  # ========================================================================
  # 10. SAVE THREE COMPARISON REPORTS
  # ========================================================================
  write_report <- function(
    table,
    filename
  ) {
    table_for_disk <- table

    for (
      field_name in
      names(table_for_disk)
    ) {
      if (
        is.double(
          table_for_disk[[field_name]]
        )
      ) {
        table_for_disk[[field_name]] <- ifelse(
          is.na(table_for_disk[[field_name]]),
          "",
          sprintf("%.17g", table_for_disk[[field_name]])
        )
      }
    }

    output_path <- file.path(
      cleaned_dir,
      filename
    )

    if (file.exists(output_path)) {
      cat(
        "Refreshing our generated report:",
        filename,
        "\n"
      )
    }

    utils::write.csv(
      table_for_disk,
      output_path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    cat(
      "Saved:",
      output_path,
      "\n"
    )
  }

  write_report(
    comparison_summary,
    output_files[["summary"]]
  )

  write_report(
    comparison_labels,
    output_files[["swap_labels"]]
  )

  write_report(
    method_groups,
    output_files[["groups"]]
  )

  # ========================================================================
  # 11. PRINT A SHORT CONSOLE SUMMARY
  # ========================================================================
  cat("\nFINAL METHOD COMPARISON\n")

  print(
    comparison_summary[, c(
      "sample",
      "replication_final_groups",
      "adaptation_final_groups",
      "replication_candidate_swaps",
      "adaptation_candidate_swaps",
      "swaps_selected_by_both",
      "replication_only_swaps",
      "adaptation_only_swaps",
      "final_set_relation"
    )],
    row.names = FALSE
  )

  cat("\nSTEP 6 COMPLETE: WTEYE adaptation and AMM replication were compared.\n")
  cat("Chosen adaptation rule:", chosen_variant, "\n")
  cat("Input fingerprints unchanged: TRUE\n")
  cat("Control Stage-3 status: NOT NEEDED because zero swaps survived Stage 2.\n")
  cat("These are method-agreement results, not ground-truth accuracy or confirmed wash trading.\n")
  cat("Send the complete console output and all three Step-6 CSVs for review.\n")
})
