# amm_adaptation_step_7_internship_report_eco-usdt_wcelo_pool_comparisons.R
# Read saved Step-6 results for the original pair, ECO-USDT pair and wCELO pair.
# No detector, parameter, candidate label, SQL query or API call is changed.
# The formal WTEYE filter remains REQUIRE_CONSECUTIVE in all three cases.
#
# Plain-language sequence:
# 1. Read each pair's Step-6 summary/event labels and Step-5 filter summary.
# 2. Translate the older original-pair column names into the newer terminology.
# 3. Recount the four categories from unique event labels and verify each summary.
# 4. Save six pool rows, three pair totals, and totals by pool role and all pools.
# Percentages are recomputed from summed counts, never averaged across pools.
#
# Interpretations: reference-only means selected by reduced AMM but not WTEYE;
# WTEYE-only means selected by WTEYE but not reduced AMM. Neither means unselected
# by both. None is an independently verified positive or negative wash-trade label.
# These are descriptive results for the supplied samples. The original pair has
# only 36/39 swaps; pooled percentages are influenced more by the larger samples.
# Step 7 checks comparison consistency; it does not independently establish that
# every historical detector implementation was identical or that samples represent
# the full population. The wCELO control is the replacement TOWER/REVV pool.
#
# INPUTS: nine existing CSVs in Data/cleaned (three per pair, named below).
# OUTPUTS: amm_adaptation_output_all_pools_internship_report_wteye_replication_comparison_
#   pool_summary.csv: six separate pool results, with pool addresses.
#   pair_summary.csv: one combined result for each suspicious/control pair.
#   overall_summary.csv: suspicious pools, control pools and all six pools.
#   manifest.csv: input/output file fingerprints for this consolidation run.
# No input is overwritten. No extra R package is needed.

local({
  cleaned_dir <- "D:/Downloads/Bachelor Thesis/Data/cleaned"
  output_prefix <- "amm_adaptation_output_all_pools_internship_report_wteye_replication_comparison_"
  variant <- "REQUIRE_CONSECUTIVE"
  samples <- c("suspicious_pool", "control_pool")
  cases <- c("original", "eco_usdt", "wcelo")
  prefixes <- c(
    original = "amm_adaptation_output_both_pools_internship_report_wteye_",
    eco_usdt = "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_",
    wcelo = "amm_adaptation_additional_pool_wcelo_output_both_pools_internship_report_wteye_")
  need <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  required <- function(tab, fields, label) {
    need(all(fields %in% names(tab)), paste("Missing columns in", label, ":",
      paste(setdiff(fields, names(tab)), collapse = ", ")))
  }
  count <- function(x) {
    n <- suppressWarnings(as.numeric(x))
    need(all(is.finite(n) & n >= 0 & n == floor(n)), "Invalid count")
    n
  }
  flag <- function(x) {
    need(all(x %in% c("TRUE", "FALSE")), "Missing or invalid method decision")
    x == "TRUE"
  }
  fields <- c("total_swap_events", "reference_selected", "wteye_selected", "shared",
              "reference_only", "wteye_only", "neither")
  rate_names <- c("reference_coverage_pct", "missed_reference_pct", "extra_wteye_pct",
                  "wteye_selected_overlap_pct", "jaccard_pct")
  # Zero denominators are undefined (NA), not zero or 100 percent.
  pct <- function(n, d) if (d == 0) NA_real_ else 100 * n / d
  rates <- function(n) c(
    reference_coverage_pct = pct(n[["shared"]], n[["reference_selected"]]),
    missed_reference_pct = pct(n[["reference_only"]], n[["reference_selected"]]),
    extra_wteye_pct = pct(n[["wteye_only"]], n[["wteye_selected"]]),
    wteye_selected_overlap_pct = pct(n[["shared"]], n[["wteye_selected"]]),
    jaccard_pct = pct(n[["shared"]], n[["reference_selected"]] + n[["wteye_selected"]] - n[["shared"]]))
  # Check a saved percentage when its denominator is defined; blanks/NA must
  # remain undefined where there were no selections.
  check_rate <- function(saved, expected, label) {
    missing <- is.na(saved) || trimws(saved) %in% c("", "NA")
    if (is.na(expected)) need(missing, paste("Expected an undefined rate:", label)) else {
      value <- suppressWarnings(as.numeric(saved))
      need(!missing && is.finite(value) && abs(value - expected) < 1e-7,
           paste("Saved percentage does not reconcile:", label))
    }
  }
  endings <- c("replication_comparison_summary.csv", "replication_comparison_swap_labels.csv",
               "filter_variant_comparison.csv")
  input_paths <- unlist(lapply(prefixes, function(pre)
    file.path(cleaned_dir, paste0(pre, endings))), use.names = FALSE)
  need(all(file.exists(input_paths)), paste("Missing input(s):",
    paste(input_paths[!file.exists(input_paths)], collapse = "\n")))
  before <- unname(tools::md5sum(input_paths))
  read <- function(pre, ending) read.csv(file.path(cleaned_dir, paste0(pre, ending)),
    colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE, na.strings = NULL)
  cat("AMM ADAPTATION -- STEP 7: CONSOLIDATE SIX POOLS\n")
  cat("Saved results only; no detector rerun or Dune request.\n")

  # These reviewed counts are validation checks, not targets used to change labels.
  expected <- rbind(
    c(36,4,2,2,2,0,32), c(39,0,0,0,0,0,39),
    c(9449,4584,4653,3946,638,707,4158), c(7815,0,9,0,0,9,7806),
    c(11417,3019,2278,1883,1136,395,8003), c(6459,2,22,2,0,20,6437))
  colnames(expected) <- fields
  # Map original-pair vocabulary onto the same event-level definitions.
  old_names <- c("input_swap_events", "replication_candidate_swaps", "adaptation_candidate_swaps",
    "swaps_selected_by_both", "replication_only_swaps", "adaptation_only_swaps", "swaps_selected_by_neither")
  old_rates <- c("adaptation_coverage_of_replication_pct",
    "adaptation_candidates_also_in_replication_pct", "jaccard_overlap_pct")
  new_old_rates <- c("reference_coverage_pct", "wteye_selected_overlap_pct", "jaccard_pct")
  pool_rows <- list(); all_events <- character(); all_pool_addresses <- character()
  for (case in cases) {
    pre <- prefixes[[case]]
    summary <- read(pre, endings[1]); labels <- read(pre, endings[2]); filters <- read(pre, endings[3])
    required(summary, "sample", case); required(labels, c("sample", "event_id", "transaction_hash", "swap_index"), case)
    required(filters, c("sample", "variant", "unique_candidate_swap_events"), case)
    need(!anyDuplicated(summary$sample), paste("Duplicate summary rows:", case))
    expected_samples <- if (case == "original") samples else c(samples, "both_pools")
    need(setequal(summary$sample, expected_samples), paste("Unexpected summary samples:", case))
    need(setequal(unique(labels$sample), samples), paste("Unexpected label samples:", case))
    if (case == "original") {
      required(summary, c(old_names, old_rates, "chosen_adaptation_variant"), case)
      need(all(summary$chosen_adaptation_variant == variant), "Original comparison uses a different filter")
      required(labels, c("replication_candidate", "adaptation_candidate", "agreement_category", "adaptation_method"), case)
      need(all(labels$adaptation_method == "INTERNSHIP_REPORT_WTEYE_REQUIRE_CONSECUTIVE"), "Original labels use a different method")
      labels$reference_selected <- labels$replication_candidate
      labels$wteye_selected <- labels$adaptation_candidate
      translate <- c(BOTH_METHODS="SHARED", REPLICATION_ONLY="REFERENCE_ONLY",
                     ADAPTATION_ONLY="WTEYE_ONLY", NEITHER_METHOD="NEITHER")
      need(all(labels$agreement_category %in% names(translate)), "Unknown original category")
      labels$comparison_category <- unname(translate[labels$agreement_category])
      names(summary)[match(old_names, names(summary))] <- fields
      names(summary)[match(old_rates, names(summary))] <- new_old_rates
      # The original script joined by transaction hash. This is equivalent here
      # only because each sample contains one supplied swap per transaction.
      need(!anyDuplicated(paste(labels$sample, tolower(labels$transaction_hash))),
           "Original transaction-based comparison needs event-level review")
    } else required(labels, c("reference_selected", "wteye_selected", "comparison_category"), case)
    required(summary, c(fields, if (case == "original") new_old_rates else rate_names), case)
    event <- tolower(labels$event_id)
    need(all(grepl("^0x[0-9a-f]{40}:0x[0-9a-f]{64}:[0-9]+$", event)), "Invalid event identifier")
    pieces <- strsplit(event, ":", fixed = TRUE)
    pool <- vapply(pieces, `[`, character(1), 1L)
    tx <- vapply(pieces, `[`, character(1), 2L)
    need(all(tx == tolower(labels$transaction_hash)), "Event and transaction identifiers disagree")
    need(!anyDuplicated(event) && !any(event %in% all_events), "Overlapping or duplicate input events")
    all_events <- c(all_events, event)
    ref <- flag(labels$reference_selected); wt <- flag(labels$wteye_selected)
    category <- ifelse(ref & wt, "SHARED", ifelse(ref, "REFERENCE_ONLY", ifelse(wt, "WTEYE_ONLY", "NEITHER")))
    need(all(labels$comparison_category == category), "Category differs from saved method flags")
    case_rows <- list()
    for (sample in samples) {
      ix <- labels$sample == sample; rr <- ref[ix]; ww <- wt[ix]
      n <- c(total_swap_events=sum(ix), reference_selected=sum(rr), wteye_selected=sum(ww),
        shared=sum(rr & ww), reference_only=sum(rr & !ww), wteye_only=sum(!rr & ww), neither=sum(!rr & !ww))
      need(setequal(count(labels$swap_index[ix]), seq_len(sum(ix))) &&
        !anyDuplicated(count(labels$swap_index[ix])), "Incomplete swap indices")
      addresses <- unique(pool[ix]); need(length(addresses) == 1L, "Sample must contain exactly one pool")
      need(!(addresses %in% all_pool_addresses), "A pool is repeated in another sample")
      all_pool_addresses <- c(all_pool_addresses, addresses)
      row <- summary[summary$sample == sample, , drop=FALSE]
      need(all(count(unlist(row[fields], use.names=FALSE)) == n), "Summary differs from event labels")
      filter <- filters[filters$sample == sample & filters$variant == variant, , drop=FALSE]
      need(nrow(filter) == 1L && count(filter$unique_candidate_swap_events) == n[["wteye_selected"]],
           "WTEYE selection count differs from REQUIRE_CONSECUTIVE")
      need(all(n == expected[length(pool_rows)+1L, ]), "Counts differ from the reviewed comparison")
      rate <- rates(n)
      for (k in intersect(rate_names, names(summary))) check_rate(row[[k]], rate[[k]], paste(case, sample, k))
      out <- data.frame(case=case, sample=sample, pool_address=addresses,
        as.list(n), as.list(rate), chosen_adaptation_variant=variant,
        interpretation_status="METHOD_AGREEMENT_NOT_ACCURACY", stringsAsFactors=FALSE)
      pool_rows[[length(pool_rows)+1L]] <- out; case_rows[[length(case_rows)+1L]] <- out
    }
    # Ignore existing both_pools rows when summing, then verify them separately.
    if (case != "original") {
      total <- colSums(do.call(rbind, case_rows)[fields]); row <- summary[summary$sample=="both_pools",,drop=FALSE]
      need(all(count(unlist(row[fields], use.names=FALSE)) == total), "Pair total double-counted or inconsistent")
      rate <- rates(total)
      for (k in rate_names) check_rate(row[[k]], rate[[k]], paste(case, "both_pools", k))
    }
    cat("PASS:", case, "comparison definitions, event labels, counts and percentages.\n")
  }
  pools <- do.call(rbind, pool_rows)
  need(nrow(pools) == 6L && length(unique(all_pool_addresses)) == 6L, "Expected six distinct pools")
  aggregate_rows <- function(rows, name) {
    n <- colSums(rows[fields]); rate <- rates(n)
    data.frame(scope=name, number_of_pools=nrow(rows), as.list(n), as.list(rate),
      chosen_adaptation_variant=variant, interpretation_status="METHOD_AGREEMENT_NOT_ACCURACY",
      stringsAsFactors=FALSE)
  }
  pairs <- do.call(rbind, lapply(cases, function(case) aggregate_rows(pools[pools$case==case,,drop=FALSE], case)))
  overall <- do.call(rbind, lapply(c(samples, "all_pools"), function(role)
    aggregate_rows(pools[pools$sample==role | role=="all_pools",,drop=FALSE], role)))
  need(identical(before, unname(tools::md5sum(input_paths))), "An input changed during consolidation")
  # Save the tables only after all checks pass. No original output is overwritten.
  written <- character()
  save <- function(tab, suffix) {
    path <- file.path(cleaned_dir, paste0(output_prefix, suffix, ".csv"))
    write.csv(tab, path, row.names=FALSE, na="", fileEncoding="UTF-8")
    written <<- c(written, path); cat("Saved:", path, "\n")
  }
  save(pools, "pool_summary"); save(pairs, "pair_summary"); save(overall, "overall_summary")
  manifest <- data.frame(artifact_type=c(rep("input",length(input_paths)),rep("output",length(written))),
    file_name=basename(c(input_paths,written)), md5=unname(tools::md5sum(c(input_paths,written))),
    stage="STEP7_CONSOLIDATION", stringsAsFactors=FALSE)
  save(manifest, "manifest")
  print(pools[c("case","sample",fields)], row.names=FALSE)
  print(overall, row.names=FALSE)
  cat("STEP 7 COMPLETE: six pools consolidated. Original input files unchanged.\n")
  cat("Combined percentages use summed counts, not averages of pool percentages.\n")
  cat("These are sample-level method-agreement results, not accuracy estimates.\n")
  cat("Next: send the console output and the three summary CSVs for review.\n")
})
