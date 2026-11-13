# ============================================================================
# AMM ADAPTATION -- STEP 6: COMPARE OUR ADAPTED DETECTOR WITH OUR REPLICATION
# ============================================================================
#
# OUR PURPOSE
# -----------
# In our earlier steps, we produced two different detection results:
#
# 1. Our reduced-scale replication of the AMM paper produced its final
#    candidate swaps.
# 2. Our adapted internship detector produced structural candidates and then
#    applied several filtering choices.
#
# In this step, we compare those two methods on exactly the same supplied swaps.
# We compare transaction hashes because a transaction hash is the clearest
# common identifier shared by both pipelines.
#
# OUR CHOSEN ADAPTATION RULE
# --------------------------
# We use REQUIRE_CONSECUTIVE from Step 5. Under this rule, we keep an adapted
# candidate only when its swaps are consecutive in the original pool sequence.
# This follows the AMM paper's adjacent-group idea while retaining our adapted
# SCC and volume-matching logic.
#
# IMPORTANT INTERPRETATION
# ------------------------
# We use the replication as a REFERENCE METHOD, not as perfect ground truth.
# Therefore, an agreement means only that both algorithms selected the same
# swap. A disagreement does not automatically prove that either algorithm is
# wrong, and none of these outputs confirms wash trading.
#
# WHY THE CONTROL POOL ENDS AT STAGE 2
# ------------------------------------
# Our control-pool replication evaluated its candidates in Stage 2. Because no
# control candidate passed Stage 2, there were no surviving swaps requiring the
# Stage 3 MEV-elimination checks. We verify this directly from the Stage 2
# summary and the empty Stage 2 surviving-swaps file.
#
# OUR THREE OUTPUTS IN DATA/CLEANED
# ---------------------------------
# amm_adaptation_output_both_pools_replication_comparison_summary.csv
#   One row per sample with group counts, swap counts, overlap, and set relation.
#
# amm_adaptation_output_both_pools_replication_comparison_swap_labels.csv
#   One row per supplied swap showing whether each method selected it.
#
# amm_adaptation_output_both_pools_replication_comparison_groups.csv
#   One row per final candidate group showing whether the other method has an
#   exact or partial group overlap.
#
# HOW WE RUN THE WHOLE SCRIPT IN OUR R CONSOLE
# --------------------------------------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_6_compare_with_replication.R")
#
# HOW WE READ THIS CODE
# ---------------------
# Comments begin with #. The symbol <- stores a value. A function is a reusable
# operation. TRUE and FALSE mean yes and no. stop() safely ends the script when
# an input is missing or inconsistent, rather than producing misleading output.

local({
  # ========================================================================
  # 1. WE SET OUR FOLDERS, FILENAMES, AND CHOSEN RULE
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  samples <- c("suspicious_pool", "control_pool")

  # This is the Step 5 rule that we selected for the formal comparison.
  chosen_variant <- "REQUIRE_CONSECUTIVE"

  # These two files contain the final labels and group decisions from Step 5.
  adaptation_swap_file <-
    "amm_adaptation_output_both_pools_filter_variant_swap_labels.csv"
  adaptation_group_file <-
    "amm_adaptation_output_both_pools_filter_variant_group_decisions.csv"

  # These four files contain the final checkpoints from our AMM-paper
  # replication. The suspicious pool reached Stage 3. The control pool ended
  # at Stage 2 because no candidate survived that stage.
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
      "amm_adaptation_output_both_pools_replication_comparison_summary.csv",
    swap_labels =
      "amm_adaptation_output_both_pools_replication_comparison_swap_labels.csv",
    groups =
      "amm_adaptation_output_both_pools_replication_comparison_groups.csv"
  )

  input_files <- c(
    adaptation_swap_file,
    adaptation_group_file,
    replication_suspicious_swaps_file,
    replication_suspicious_summary_file,
    replication_control_swaps_file,
    replication_control_summary_file
  )
  input_paths <- file.path(cleaned_dir, input_files)

  cat("AMM ADAPTATION -- STEP 6: COMPARE WITH AMM REPLICATION\n\n")
  cat("Input/output folder:", cleaned_dir, "\n")
  cat("Chosen adaptation rule:", chosen_variant, "\n")

  # Before reading anything, we verify that the folder and every required file
  # exist. This gives us a clear error if a file was not copied into Data/cleaned.
  if (!dir.exists(cleaned_dir) || !all(file.exists(input_paths))) {
    missing_files <- input_files[!file.exists(input_paths)]
    stop(
      paste("A required Step 5 or replication file is missing:",
            paste(missing_files, collapse = ", ")),
      call. = FALSE
    )
  }

  # An MD5 fingerprint is a compact signature of a file. We record each input's
  # fingerprint now and check it again before saving our outputs. This confirms
  # that our source files did not change while this script was running.
  input_md5_before <- unname(tools::md5sum(input_paths))

  # ========================================================================
  # 2. WE DEFINE SMALL HELPER FUNCTIONS
  # ========================================================================

  # We read every value as text first. This prevents R from changing blockchain
  # identifiers or very large numbers automatically.
  read_text_csv <- function(filename) {
    utils::read.csv(
      file.path(cleaned_dir, filename),
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  }

  # We use this helper to verify that a table has every column we require.
  require_columns <- function(table, required_columns, filename) {
    missing_columns <- setdiff(required_columns, names(table))
    if (length(missing_columns)) {
      stop(
        paste(filename, "is missing:", paste(missing_columns, collapse = ", ")),
        call. = FALSE
      )
    }
  }

  # We convert a whole-number text field, such as a swap index, into an integer.
  as_index <- function(values, field_name) {
    if (!length(values)) return(integer(0))
    numbers <- suppressWarnings(as.numeric(values))
    invalid <- !is.finite(numbers) | numbers < 0 | numbers != floor(numbers) |
      numbers > .Machine$integer.max
    if (any(invalid)) {
      stop(paste("We found an invalid whole number in", field_name), call. = FALSE)
    }
    as.integer(numbers)
  }

  # We convert text containing TRUE or FALSE into R logical values.
  as_yes_no <- function(values, field_name) {
    if (!length(values)) return(logical(0))
    if (any(!(values %in% c("TRUE", "FALSE")))) {
      stop(paste("We found an invalid TRUE/FALSE value in", field_name),
           call. = FALSE)
    }
    values == "TRUE"
  }

  # Ethereum transaction hashes are not case-sensitive. We convert them to
  # lowercase so the same transaction cannot appear different only because of
  # letter capitalization.
  normalize_hash <- function(values, field_name) {
    hashes <- tolower(trimws(as.character(values)))
    valid <- grepl("^0x[0-9a-f]{64}$", hashes)
    if (any(!valid)) {
      stop(paste("We found an invalid transaction hash in", field_name),
           call. = FALSE)
    }
    hashes
  }

  # We join several identifiers into one readable semicolon-separated value.
  # If there are no identifiers, we return an empty text value.
  join_values <- function(values) {
    values <- sort(unique(as.character(values)))
    values <- values[nzchar(values)]
    if (!length(values)) "" else paste(values, collapse = ";")
  }

  # We calculate a percentage only when the denominator is greater than zero.
  # NA means that the percentage is not mathematically defined.
  safe_percentage <- function(numerator, denominator) {
    if (denominator == 0) NA_real_ else 100 * numerator / denominator
  }

  # We describe how the two final sets are related without claiming that either
  # set is ground truth.
  describe_set_relation <- function(replication_hashes, adaptation_hashes) {
    replication_hashes <- unique(replication_hashes)
    adaptation_hashes <- unique(adaptation_hashes)
    if (setequal(replication_hashes, adaptation_hashes)) {
      return("EXACT_SET_MATCH")
    }
    if (all(adaptation_hashes %in% replication_hashes)) {
      return("ADAPTATION_SUBSET_OF_REPLICATION")
    }
    if (all(replication_hashes %in% adaptation_hashes)) {
      return("REPLICATION_SUBSET_OF_ADAPTATION")
    }
    if (!length(intersect(replication_hashes, adaptation_hashes))) {
      return("DISJOINT_SETS")
    }
    "PARTIAL_OVERLAP"
  }

  # This helper turns a semicolon-separated event key such as "17;18" into
  # the two integer swap indices 17 and 18.
  split_event_key <- function(event_key, field_name) {
    if (!nzchar(event_key)) return(integer(0))
    as_index(strsplit(event_key, ";", fixed = TRUE)[[1L]], field_name)
  }

  # ========================================================================
  # 3. WE READ AND VALIDATE OUR ADAPTATION RESULTS
  # ========================================================================
  adaptation_swaps_all <- read_text_csv(adaptation_swap_file)
  adaptation_groups_all <- read_text_csv(adaptation_group_file)

  require_columns(
    adaptation_swaps_all,
    c("variant", "sample", "swap_index", "event_id", "transaction_hash",
      "initiator_address", "retained_candidate", "retained_group_ids"),
    adaptation_swap_file
  )
  require_columns(
    adaptation_groups_all,
    c("variant", "sample", "candidate_group_id", "event_key", "retained"),
    adaptation_group_file
  )

  # Step 5 contains four variants. We select only the consecutive-swap variant
  # that we chose for this comparison.
  adaptation_swaps <- adaptation_swaps_all[
    adaptation_swaps_all$variant == chosen_variant, , drop = FALSE
  ]
  adaptation_groups <- adaptation_groups_all[
    adaptation_groups_all$variant == chosen_variant, , drop = FALSE
  ]

  if (!nrow(adaptation_swaps) || !setequal(adaptation_swaps$sample, samples)) {
    stop("Our chosen Step 5 variant does not contain both samples.", call. = FALSE)
  }

  adaptation_swaps$swap_index <-
    as_index(adaptation_swaps$swap_index, "adaptation swap_index")
  adaptation_swaps$transaction_hash <-
    normalize_hash(adaptation_swaps$transaction_hash, "adaptation transaction_hash")
  adaptation_swaps$retained_candidate <-
    as_yes_no(adaptation_swaps$retained_candidate, "adaptation retained_candidate")
  adaptation_groups$retained <-
    as_yes_no(adaptation_groups$retained, "adaptation group retained")

  # Our supplied samples contain one swap event per transaction. We verify that
  # both the swap index and transaction hash are unique within each sample.
  swap_keys <- paste(adaptation_swaps$sample, adaptation_swaps$swap_index, sep = "|")
  transaction_keys <-
    paste(adaptation_swaps$sample, adaptation_swaps$transaction_hash, sep = "|")
  if (anyDuplicated(swap_keys) || anyDuplicated(transaction_keys) ||
      any(!(adaptation_swaps$sample %in% samples))) {
    stop("Our Step 5 swap universe contains duplicate or invalid identifiers.",
         call. = FALSE)
  }

  # We keep only the adapted groups that passed the chosen rule.
  adaptation_groups_final <-
    adaptation_groups[adaptation_groups$retained, , drop = FALSE]

  # ========================================================================
  # 4. WE READ AND VALIDATE OUR REPLICATION RESULTS
  # ========================================================================
  replication_suspicious_swaps <-
    read_text_csv(replication_suspicious_swaps_file)
  replication_suspicious_summary <-
    read_text_csv(replication_suspicious_summary_file)
  replication_control_swaps <-
    read_text_csv(replication_control_swaps_file)
  replication_control_summary <-
    read_text_csv(replication_control_summary_file)

  require_columns(
    replication_suspicious_swaps,
    c("candidate_id", "swap_sequence", "evt_tx_hash"),
    replication_suspicious_swaps_file
  )
  require_columns(
    replication_suspicious_summary,
    c("candidate_id", "stage3_resolved", "stage3_pass"),
    replication_suspicious_summary_file
  )
  require_columns(
    replication_control_swaps,
    c("candidate_id", "swap_sequence", "evt_tx_hash"),
    replication_control_swaps_file
  )
  require_columns(
    replication_control_summary,
    c("candidate_id", "stage2_pass", "decision_route"),
    replication_control_summary_file
  )

  replication_suspicious_swaps$candidate_id <-
    as_index(replication_suspicious_swaps$candidate_id,
             "suspicious replication candidate_id")
  replication_suspicious_swaps$swap_sequence <-
    as_index(replication_suspicious_swaps$swap_sequence,
             "suspicious replication swap_sequence")
  replication_suspicious_swaps$evt_tx_hash <-
    normalize_hash(replication_suspicious_swaps$evt_tx_hash,
                   "suspicious replication evt_tx_hash")

  # The control surviving-swaps table is allowed to have zero rows. Its header
  # remains useful because it documents the structure of a possible survivor.
  replication_control_swaps$candidate_id <-
    as_index(replication_control_swaps$candidate_id,
             "control replication candidate_id")
  replication_control_swaps$swap_sequence <-
    as_index(replication_control_swaps$swap_sequence,
             "control replication swap_sequence")
  if (nrow(replication_control_swaps)) {
    replication_control_swaps$evt_tx_hash <-
      normalize_hash(replication_control_swaps$evt_tx_hash,
                     "control replication evt_tx_hash")
  }

  replication_suspicious_summary$candidate_id <-
    as_index(replication_suspicious_summary$candidate_id,
             "suspicious Stage 3 candidate_id")
  replication_suspicious_summary$stage3_resolved <-
    as_yes_no(replication_suspicious_summary$stage3_resolved,
              "suspicious stage3_resolved")
  replication_suspicious_summary$stage3_pass <-
    as_yes_no(replication_suspicious_summary$stage3_pass,
              "suspicious stage3_pass")

  replication_control_summary$candidate_id <-
    as_index(replication_control_summary$candidate_id,
             "control Stage 2 candidate_id")
  replication_control_summary$stage2_pass <-
    as_yes_no(replication_control_summary$stage2_pass, "control stage2_pass")

  # Every suspicious final candidate must be resolved and marked as passing in
  # Stage 3. The candidate IDs in the summary and final swap table must agree.
  suspicious_passing_ids <- replication_suspicious_summary$candidate_id[
    replication_suspicious_summary$stage3_resolved &
      replication_suspicious_summary$stage3_pass
  ]
  suspicious_final_ids <- unique(replication_suspicious_swaps$candidate_id)
  if (!setequal(suspicious_passing_ids, suspicious_final_ids)) {
    stop("Our suspicious Stage 3 summary and final swaps disagree.", call. = FALSE)
  }

  # The control surviving table must contain exactly the candidates that passed
  # Stage 2. In our current control result, both sets are empty.
  control_passing_ids <- replication_control_summary$candidate_id[
    replication_control_summary$stage2_pass
  ]
  control_final_ids <- unique(replication_control_swaps$candidate_id)
  if (!setequal(control_passing_ids, control_final_ids)) {
    stop("Our control Stage 2 summary and surviving swaps disagree.", call. = FALSE)
  }

  control_stage3_required <- length(control_passing_ids) > 0L
  if (control_stage3_required) {
    stop(
      "A control candidate survived Stage 2, so Stage 3 would be required before comparison.",
      call. = FALSE
    )
  }

  cat("Replication checkpoint: suspicious Stage 3 final swaps =",
      nrow(replication_suspicious_swaps), "\n")
  cat("Replication checkpoint: control Stage 2 surviving swaps =",
      nrow(replication_control_swaps), "\n")
  cat("Control Stage 3 required:", control_stage3_required, "\n")

  # ========================================================================
  # 5. WE CREATE ONE COMMON REPLICATION TABLE
  # ========================================================================
  make_replication_rows <- function(table, sample_name) {
    if (!nrow(table)) {
      return(data.frame(
        sample = character(0),
        replication_group_id = character(0),
        replication_swap_index = integer(0),
        transaction_hash = character(0),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      sample = sample_name,
      replication_group_id = paste0(
        "replication_", sample_name, "_C",
        sprintf("%03d", table$candidate_id)
      ),
      replication_swap_index = table$swap_sequence,
      transaction_hash = table$evt_tx_hash,
      stringsAsFactors = FALSE
    )
  }

  replication_swaps <- rbind(
    make_replication_rows(replication_suspicious_swaps, "suspicious_pool"),
    make_replication_rows(replication_control_swaps, "control_pool")
  )
  rownames(replication_swaps) <- NULL

  # We verify that every replication result belongs to the same supplied swap
  # universe used by our adaptation. We also verify its recorded swap sequence.
  for (row_number in seq_len(nrow(replication_swaps))) {
    same_sample <- adaptation_swaps$sample == replication_swaps$sample[row_number]
    matching_row <- which(
      same_sample &
        adaptation_swaps$transaction_hash ==
          replication_swaps$transaction_hash[row_number]
    )
    if (length(matching_row) != 1L) {
      stop("A replication transaction is missing or duplicated in our Step 5 universe.",
           call. = FALSE)
    }
    if (adaptation_swaps$swap_index[matching_row] !=
        replication_swaps$replication_swap_index[row_number]) {
      stop("The replication and adaptation swap indices disagree.", call. = FALSE)
    }
  }

  # ========================================================================
  # 6. WE LABEL EVERY SWAP UNDER BOTH METHODS
  # ========================================================================
  comparison_labels <- adaptation_swaps[, c(
    "sample", "swap_index", "event_id", "transaction_hash",
    "initiator_address", "retained_candidate", "retained_group_ids"
  )]
  names(comparison_labels)[names(comparison_labels) == "retained_candidate"] <-
    "adaptation_candidate"
  names(comparison_labels)[names(comparison_labels) == "retained_group_ids"] <-
    "adaptation_group_ids"

  comparison_labels$replication_candidate <- FALSE
  comparison_labels$replication_group_ids <- ""

  for (row_number in seq_len(nrow(comparison_labels))) {
    matching_replication_rows <- which(
      replication_swaps$sample == comparison_labels$sample[row_number] &
        replication_swaps$transaction_hash ==
          comparison_labels$transaction_hash[row_number]
    )
    comparison_labels$replication_candidate[row_number] <-
      length(matching_replication_rows) > 0L
    comparison_labels$replication_group_ids[row_number] <- join_values(
      replication_swaps$replication_group_id[matching_replication_rows]
    )
  }

  # Each swap receives one of four mutually exclusive agreement categories.
  comparison_labels$agreement_category <- ifelse(
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
  comparison_labels$comparison_status <-
    "METHOD_COMPARISON_NOT_GROUND_TRUTH"

  # We order the columns so that the two method labels are easy to compare.
  comparison_labels <- comparison_labels[, c(
    "sample", "swap_index", "event_id", "transaction_hash",
    "initiator_address", "replication_candidate", "adaptation_candidate",
    "agreement_category", "replication_group_ids", "adaptation_group_ids",
    "comparison_status"
  )]
  comparison_labels <- comparison_labels[
    order(match(comparison_labels$sample, samples), comparison_labels$swap_index),
    , drop = FALSE
  ]
  rownames(comparison_labels) <- NULL

  # ========================================================================
  # 7. WE CREATE OUR SAMPLE-LEVEL COMPARISON SUMMARY
  # ========================================================================
  summary_rows <- list()

  for (sample_name in samples) {
    sample_labels <- comparison_labels[
      comparison_labels$sample == sample_name, , drop = FALSE
    ]
    replication_hashes <- sample_labels$transaction_hash[
      sample_labels$replication_candidate
    ]
    adaptation_hashes <- sample_labels$transaction_hash[
      sample_labels$adaptation_candidate
    ]

    both_count <- sum(sample_labels$agreement_category == "BOTH_METHODS")
    replication_only_count <-
      sum(sample_labels$agreement_category == "REPLICATION_ONLY")
    adaptation_only_count <-
      sum(sample_labels$agreement_category == "ADAPTATION_ONLY")
    neither_count <- sum(sample_labels$agreement_category == "NEITHER_METHOD")
    union_count <- both_count + replication_only_count + adaptation_only_count

    replication_group_count <- length(unique(
      replication_swaps$replication_group_id[
        replication_swaps$sample == sample_name
      ]
    ))
    adaptation_group_count <- length(unique(
      adaptation_groups_final$candidate_group_id[
        adaptation_groups_final$sample == sample_name
      ]
    ))

    # Coverage asks: what percentage of replication swaps did our adaptation
    # also find? Support asks: what percentage of adapted swaps also appear in
    # the replication? These are overlap descriptions, not validated accuracy.
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      sample = sample_name,
      input_swap_events = nrow(sample_labels),
      replication_final_groups = replication_group_count,
      adaptation_final_groups = adaptation_group_count,
      replication_candidate_swaps = length(unique(replication_hashes)),
      adaptation_candidate_swaps = length(unique(adaptation_hashes)),
      swaps_selected_by_both = both_count,
      replication_only_swaps = replication_only_count,
      adaptation_only_swaps = adaptation_only_count,
      swaps_selected_by_neither = neither_count,
      adaptation_coverage_of_replication_pct = safe_percentage(
        both_count, length(unique(replication_hashes))
      ),
      adaptation_candidates_also_in_replication_pct = safe_percentage(
        both_count, length(unique(adaptation_hashes))
      ),
      jaccard_overlap_pct = safe_percentage(both_count, union_count),
      exact_final_set_agreement = setequal(
        replication_hashes, adaptation_hashes
      ),
      final_set_relation = describe_set_relation(
        replication_hashes, adaptation_hashes
      ),
      replication_final_stage = if (sample_name == "suspicious_pool") {
        "STAGE_3_MEV_REVIEW_COMPLETED"
      } else {
        "STAGE_2_COMPLETE_NO_SURVIVORS_STAGE_3_NOT_NEEDED"
      },
      interpretation_status = "REFERENCE_METHOD_COMPARISON_NOT_ACCURACY_PROOF",
      stringsAsFactors = FALSE
    )
  }

  comparison_summary <- do.call(rbind, summary_rows)
  rownames(comparison_summary) <- NULL

  # ========================================================================
  # 8. WE COMPARE THE FINAL GROUP STRUCTURES
  # ========================================================================

  # First, we convert the adapted final groups into one standard group table.
  adaptation_group_sets <- data.frame(
    sample = adaptation_groups_final$sample,
    method = "ADAPTED_INTERNSHIP_REQUIRE_CONSECUTIVE",
    group_id = adaptation_groups_final$candidate_group_id,
    event_key = adaptation_groups_final$event_key,
    stringsAsFactors = FALSE
  )

  # Next, we create the same structure for the replication groups. Their event
  # keys are rebuilt from transaction hashes after matching them to our common
  # swap universe.
  replication_group_sets <- data.frame(
    sample = character(0), method = character(0), group_id = character(0),
    event_key = character(0), stringsAsFactors = FALSE
  )

  if (nrow(replication_swaps)) {
    for (replication_group_id in unique(replication_swaps$replication_group_id)) {
      group_rows <- replication_swaps[
        replication_swaps$replication_group_id == replication_group_id,
        , drop = FALSE
      ]
      group_sample <- unique(group_rows$sample)
      group_indices <- adaptation_swaps$swap_index[
        adaptation_swaps$sample == group_sample &
          adaptation_swaps$transaction_hash %in% group_rows$transaction_hash
      ]
      replication_group_sets <- rbind(
        replication_group_sets,
        data.frame(
          sample = group_sample,
          method = "AMM_PAPER_REPLICATION",
          group_id = replication_group_id,
          event_key = paste(sort(unique(group_indices)), collapse = ";"),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  method_groups <- rbind(replication_group_sets, adaptation_group_sets)
  rownames(method_groups) <- NULL

  method_groups$number_of_swaps <- vapply(
    seq_len(nrow(method_groups)),
    function(row_number) length(split_event_key(
      method_groups$event_key[row_number],
      paste("group event_key", method_groups$group_id[row_number])
    )),
    integer(1)
  )
  method_groups$exact_matching_other_group_ids <- ""
  method_groups$overlapping_other_group_ids <- ""
  method_groups$maximum_shared_swaps <- 0L
  method_groups$group_comparison_outcome <- ""
  method_groups$comparison_status <- "METHOD_COMPARISON_NOT_GROUND_TRUTH"

  for (row_number in seq_len(nrow(method_groups))) {
    this_indices <- split_event_key(
      method_groups$event_key[row_number],
      paste("group event_key", method_groups$group_id[row_number])
    )
    other_rows <- which(
      method_groups$sample == method_groups$sample[row_number] &
        method_groups$method != method_groups$method[row_number]
    )

    if (!length(other_rows)) {
      method_groups$group_comparison_outcome[row_number] <-
        "NO_FINAL_GROUPS_IN_OTHER_METHOD"
      next
    }

    overlap_sizes <- vapply(
      other_rows,
      function(other_row) {
        other_indices <- split_event_key(
          method_groups$event_key[other_row],
          paste("group event_key", method_groups$group_id[other_row])
        )
        length(intersect(this_indices, other_indices))
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
              method_groups$event_key[other_row],
              paste("group event_key", method_groups$group_id[other_row])
            )
          )
        },
        logical(1)
      )
    ]
    overlapping_rows <- other_rows[overlap_sizes > 0L]

    method_groups$exact_matching_other_group_ids[row_number] <-
      join_values(method_groups$group_id[exact_rows])
    method_groups$overlapping_other_group_ids[row_number] <-
      join_values(method_groups$group_id[overlapping_rows])
    method_groups$maximum_shared_swaps[row_number] <- max(overlap_sizes)
    method_groups$group_comparison_outcome[row_number] <- if (length(exact_rows)) {
      "EXACT_GROUP_MATCH"
    } else if (length(overlapping_rows)) {
      "PARTIAL_GROUP_OVERLAP"
    } else {
      "NO_GROUP_OVERLAP"
    }
  }

  method_groups <- method_groups[
    order(match(method_groups$sample, samples), method_groups$method,
          method_groups$group_id),
    , drop = FALSE
  ]
  rownames(method_groups) <- NULL

  # ========================================================================
  # 9. WE RUN FINAL CONSISTENCY CHECKS
  # ========================================================================

  # For every sample, the four agreement categories must add back to the total
  # number of supplied swaps.
  category_totals <- with(
    comparison_summary,
    swaps_selected_by_both + replication_only_swaps +
      adaptation_only_swaps + swaps_selected_by_neither
  )
  if (!identical(as.integer(category_totals),
                 as.integer(comparison_summary$input_swap_events))) {
    stop("Our four comparison categories do not reproduce the input totals.",
         call. = FALSE)
  }

  # We independently reconstruct the final candidate counts from the detailed
  # label table and compare them with our summary.
  for (sample_name in samples) {
    one_summary <- comparison_summary[
      comparison_summary$sample == sample_name, , drop = FALSE
    ]
    one_labels <- comparison_labels[
      comparison_labels$sample == sample_name, , drop = FALSE
    ]
    if (one_summary$replication_candidate_swaps !=
          sum(one_labels$replication_candidate) ||
        one_summary$adaptation_candidate_swaps !=
          sum(one_labels$adaptation_candidate)) {
      stop("Our detailed labels and sample summary disagree.", call. = FALSE)
    }
  }

  # Finally, we verify that none of our six input files changed during Step 6.
  if (!identical(input_md5_before, unname(tools::md5sum(input_paths)))) {
    stop("An input changed during Step 6. No reports were written.", call. = FALSE)
  }

  # ========================================================================
  # 10. WE SAVE OUR THREE COMPARISON REPORTS
  # ========================================================================
  write_report <- function(table, filename) {
    table_for_disk <- table

    # We save double-precision numbers with enough digits to avoid silently
    # changing a calculated percentage.
    for (field_name in names(table_for_disk)) {
      if (is.double(table_for_disk[[field_name]])) {
        table_for_disk[[field_name]] <- ifelse(
          is.na(table_for_disk[[field_name]]),
          "",
          sprintf("%.17g", table_for_disk[[field_name]])
        )
      }
    }

    output_path <- file.path(cleaned_dir, filename)
    if (file.exists(output_path)) {
      cat("Refreshing our generated report:", filename, "\n")
    }
    utils::write.csv(
      table_for_disk,
      output_path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
    cat("Saved:", output_path, "\n")
  }

  write_report(comparison_summary, output_files[["summary"]])
  write_report(comparison_labels, output_files[["swap_labels"]])
  write_report(method_groups, output_files[["groups"]])

  # ========================================================================
  # 11. WE PRINT A SHORT, READABLE CONSOLE SUMMARY
  # ========================================================================
  cat("\nFINAL METHOD COMPARISON\n")
  print(
    comparison_summary[, c(
      "sample", "replication_final_groups", "adaptation_final_groups",
      "replication_candidate_swaps", "adaptation_candidate_swaps",
      "swaps_selected_by_both", "replication_only_swaps",
      "adaptation_only_swaps", "final_set_relation"
    )],
    row.names = FALSE
  )

  cat("\nSTEP 6 COMPLETE: our final replication and adaptation labels were compared.\n")
  cat("Input fingerprints unchanged: TRUE\n")
  cat("Control Stage 3 status: NOT NEEDED because zero swaps survived Stage 2.\n")
  cat("These are method-agreement results, not ground-truth accuracy or confirmed wash trading.\n")
  cat("Send the complete console output and all three Step 6 CSVs for review.\n")
})
