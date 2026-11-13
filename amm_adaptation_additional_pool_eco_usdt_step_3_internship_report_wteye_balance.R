# ============================================================================
# AMM ADAPTATION -- ECO-USDT ADDITIONAL POOL -- STEP 3 -- INTERNSHIP REPORT / WTEYE-STYLE BALANCE
# ============================================================================
#
# OUR PURPOSE
# -----------
# We create a SECOND AMM-detection branch that follows the balance condition
# documented in Section 3.6 of our internship report:
#
#       || S(m) - S(0) ||_1
#       -------------------  <  Delta
#          2 * V_total
#
# with Delta = 0.10.
#
# This branch is intentionally separate from our earlier AMM Step 3 that reused
# volume_matching.cpp. We preserve that earlier branch unchanged so that the two
# balance definitions can later be compared side by side.
#
# IMPORTANT TERMINOLOGY
# ---------------------
# We call this the "internship-report / WTEYE-style balance branch" because it
# implements the balance equation documented in our internship report. It is NOT
# a claim that this script reproduces the complete original WTEYE framework.
# We retain our AMM adaptation's short-window and SCC candidate-generation design
# so that, for the first comparison, the balance rule is the main change.
#
# WHAT WE KEEP THE SAME AS THE EXISTING AMM C++ BRANCH
# ----------------------------------------------------
# - The two Step-2 prepared token-flow files.
# - One pool is analyzed at a time.
# - token0 and token1 are analyzed separately.
# - Swaps remain in chronological block/log order.
# - We inspect overlapping sequences of 2 through 10 swaps.
# - A sequence may span at most 200 blocks.
# - We construct directed token-flow graphs and retain nontrivial SCCs.
# - We map candidate groups back to original swap/event identifiers.
# - A swap-level candidate flag uses the union rule:
#       token0 candidate OR token1 candidate.
#   We retain separate token0/token1 flags so later validation can distinguish
#   one-token matches from two-token matches.
#
# WHAT WE CHANGE
# --------------
# We DO NOT call volume_matching.cpp and we DO NOT perform longest-prefix
# truncation. For every SCC selected from a window, we evaluate the WHOLE set of
# internal token movements belonging to that SCC.
#
# For one token, one SCC, and one selected window:
#
#   balance_i = total amount received by address i
#               - total amount sent by address i
#
#   L1_balance = sum_i |balance_i|
#
#   V_total = sum of all movement amounts in that SCC
#
#   WTEYE-style balance ratio = L1_balance / (2 * V_total)
#
# The SCC is a structural balance candidate when:
#
#   WTEYE-style balance ratio < 0.10
#
# We use the strict "<" operator because that is how Equation (3.1) is written
# in our internship report. Results remain structural candidates, not confirmed
# wash trades.
#
# WHY OUR STEP-2 RESCALING IS SUFFICIENT
# --------------------------------------
# Step 2 stores amount_raw as exact integer text and amount as a per-token scaled
# positive number. If every amount is multiplied by the same positive constant c,
# then both L1_balance and V_total are multiplied by c:
#
#   c * L1_balance / (2 * c * V_total)
#       = L1_balance / (2 * V_total)
#
# Therefore the WTEYE-style ratio is invariant to our common per-token scaling.
# We still audit that amount agrees with amount_raw / scale_raw before detection.
# We never add token0 quantities to token1 quantities.
#
# AMM-SPECIFIC CAUTION
# --------------------
# In our adapted graph, the liquidity pool is a mechanical counterparty and can
# connect otherwise unrelated traders. A low balance ratio therefore identifies
# a balance-neutral structural pattern under this model; it does not prove common
# ownership, collusion, manipulative intent, or the absence of arbitrage/MEV.
#
# SCRIPT LOCATION
# ---------------
# D:/Downloads/Bachelor Thesis/Scripts/
# amm_adaptation_additional_pool_eco_usdt_step_3_internship_report_wteye_balance.R
#
# INPUTS -- UNCHANGED STEP-2 PREPARED FILES
# -----------------------------------------
# D:/Downloads/Bachelor Thesis/Data/cleaned/
# amm_adaptation_additional_pool_eco_usdt_input_suspicious_pool_token_flows.csv
#
# D:/Downloads/Bachelor Thesis/Data/cleaned/
# amm_adaptation_additional_pool_eco_usdt_input_control_pool_token_flows.csv
#
# OUTPUTS -- KEPT SEPARATE FROM THE C++ BRANCH
# --------------------------------------------
# amm_adaptation_additional_pool_eco_usdt_output_suspicious_pool_internship_report_wteye_token_matches.csv
# amm_adaptation_additional_pool_eco_usdt_output_suspicious_pool_internship_report_wteye_swaps_labeled.csv
# amm_adaptation_additional_pool_eco_usdt_output_control_pool_internship_report_wteye_token_matches.csv
# amm_adaptation_additional_pool_eco_usdt_output_control_pool_internship_report_wteye_swaps_labeled.csv
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_detection_summary.csv
#
# HOW WE RUN IT
# -------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_additional_pool_eco_usdt_step_3_internship_report_wteye_balance.R")
#
# We review the full console output and all five generated CSV files before
# creating the next validation/comparison step.

local({
  # ========================================================================
  # 1. WE SET PATHS, FILENAMES, AND CONTROLLED-COMPARISON SETTINGS
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  scripts_dir <- file.path(base_dir, "Scripts")
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  script_path <- file.path(
    scripts_dir,
    "amm_adaptation_additional_pool_eco_usdt_step_3_internship_report_wteye_balance.R"
  )

  input_files <- c(
    suspicious_pool = "amm_adaptation_additional_pool_eco_usdt_input_suspicious_pool_token_flows.csv",
    control_pool = "amm_adaptation_additional_pool_eco_usdt_input_control_pool_token_flows.csv"
  )
  expected_swap_events <- c(
    suspicious_pool = 9449L,
    control_pool = 7815L
  )
  expected_flow_rows <- 2L * expected_swap_events
  expected_pools <- c(
    suspicious_pool = "0x4dfcb1482878ededccd6cc19f9d043cdd56ffd06",
    control_pool = "0xa14dfbaa23ee8e8b69878381f8fd1d0bc502c043"
  )
  input_paths <- file.path(cleaned_dir, unname(input_files))
  names(input_paths) <- names(input_files)

  # These settings deliberately match our existing AMM C++ branch so that the
  # first comparison changes the balance rule rather than changing everything
  # at once. They are experimental AMM-adaptation settings, not parameters that
  # we claim were specified by the original WTEYE paper.
  margin <- 0.10
  max_swaps <- 10L
  max_block_span <- 200L

  cat("AMM ADAPTATION -- ECO-USDT ADDITIONAL POOL -- STEP 3 -- INTERNSHIP REPORT / WTEYE-STYLE BALANCE\n\n")
  cat("R version:", R.version.string, "\n")
  cat("Prepared inputs:", cleaned_dir, "\n")
  cat("Settings: margin =", margin,
      "; max swaps =", max_swaps,
      "; max block span =", max_block_span, "\n")
  cat("Balance rule: L1(net address balances) / (2 * total token movement) < margin\n")
  cat("Evaluation unit: whole SCC internal movements; no C++ prefix truncation\n")

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Missing R package: igraph -- send the complete console output.", call. = FALSE)
  }
  if (!all(file.exists(input_paths))) {
    stop("A Step-2 prepared input is missing. Keep the agreed filenames in Data/cleaned.",
         call. = FALSE)
  }

  input_hashes_before <- unname(tools::md5sum(input_paths))
  script_md5 <- if (file.exists(script_path)) {
    unname(tools::md5sum(script_path))
  } else {
    NA_character_
  }
  if (!is.na(script_md5)) cat("Step-3 script MD5:", script_md5, "\n")

  # ========================================================================
  # 2. WE READ AND REVALIDATE OUR STEP-2 PREPARED TOKEN FLOWS
  # ========================================================================
  read_flows <- function(path, sample_name) {
    x <- utils::read.csv(
      path,
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )

    required <- c(
      "sample", "source_file", "source_row", "event_id", "swap_index",
      "row_id", "pool_address", "initiator_address", "transaction_hash",
      "block_number", "log_index", "token_slot", "direction", "seller", "buyer",
      "amount_raw", "scale_raw", "amount"
    )

    if (!nrow(x) || anyDuplicated(names(x)) || !all(required %in% names(x))) {
      stop(paste(sample_name, "has an empty or unexpected prepared table."),
           call. = FALSE)
    }
    if (anyNA(x[, required, drop = FALSE])) {
      stop(paste(sample_name, "has missing essential prepared values."),
           call. = FALSE)
    }
    if (any(x$sample != sample_name) ||
        length(unique(x$pool_address)) != 1L ||
        any(!(x$token_slot %in% c("token0", "token1")))) {
      stop("Prepared sample/pool/token identifiers are inconsistent.", call. = FALSE)
    }
    if (nrow(x) != expected_flow_rows[[sample_name]] ||
        unique(x$pool_address) != expected_pools[[sample_name]]) {
      stop(
        paste(sample_name, "does not match the accepted ECO-USDT Step-2 output."),
        call. = FALSE
      )
    }

    # We convert only the fields needed numerically. Exact raw quantities and all
    # blockchain identifiers remain text.
    for (field in c(
      "source_row", "swap_index", "row_id",
      "block_number", "log_index"
    )) {
      numbers <- suppressWarnings(as.numeric(x[[field]]))
      if (any(
        !is.finite(numbers) |
          numbers < 0 |
          numbers != floor(numbers) |
          numbers > .Machine$integer.max
      )) {
        stop(paste("Invalid index:", field), call. = FALSE)
      }
      x[[field]] <- as.integer(numbers)
    }

    x$amount <- suppressWarnings(as.numeric(x$amount))
    if (any(!is.finite(x$amount) | x$amount <= 0)) {
      stop("Invalid computational amount.", call. = FALSE)
    }

    for (field in c("amount_raw", "scale_raw")) {
      if (any(!grepl("^[0-9]+$", x[[field]]))) {
        stop("Raw quantity digits were changed.", call. = FALSE)
      }
    }

    if (any(x$row_id != x$swap_index)) {
      stop("row_id no longer matches swap_index.", call. = FALSE)
    }
    if (anyDuplicated(paste(x$event_id, x$token_slot, sep = "|"))) {
      stop("Repeated event/token rows in a prepared input.", call. = FALSE)
    }

    x <- x[order(x$swap_index, x$token_slot), , drop = FALSE]
    rownames(x) <- NULL

    token0 <- x[x$token_slot == "token0", , drop = FALSE]
    token1 <- x[x$token_slot == "token1", , drop = FALSE]

    common <- c(
      "swap_index", "row_id", "source_row", "event_id", "pool_address",
      "initiator_address", "transaction_hash", "block_number", "log_index"
    )

    a <- token0[, common, drop = FALSE]
    b <- token1[, common, drop = FALSE]
    rownames(a) <- NULL
    rownames(b) <- NULL

    if (!nrow(token0) ||
        nrow(token0) != expected_swap_events[[sample_name]] ||
        !identical(a, b) ||
        !identical(token0$swap_index, seq_len(nrow(token0)))) {
      stop("The two token lists do not map to the same consecutive swap IDs.",
           call. = FALSE)
    }

    if (!identical(
      order(token0$block_number, token0$log_index),
      seq_len(nrow(token0))
    ) ||
      anyDuplicated(paste(
        token0$block_number,
        token0$log_index,
        sep = ":"
      ))) {
      stop("Chronological block/log order is inconsistent.", call. = FALSE)
    }

    if (any(token0$direction == token1$direction)) {
      stop("Both token legs have the same direction.", call. = FALSE)
    }

    into_pool <- x$direction == "INTO_POOL"
    if (any(!(x$direction %in% c("INTO_POOL", "OUT_OF_POOL"))) ||
        any(x$seller != ifelse(
          into_pool, x$initiator_address, x$pool_address
        )) ||
        any(x$buyer != ifelse(
          into_pool, x$pool_address, x$initiator_address
        )) ||
        any(x$seller == x$buyer)) {
      stop("Prepared source/destination mapping is inconsistent.", call. = FALSE)
    }

    # We re-check Step 2's scaling relationship. This is a numerical audit, not
    # a wash-trading test.
    for (slot in c("token0", "token1")) {
      one_token <- x[x$token_slot == slot, , drop = FALSE]
      if (length(unique(one_token$scale_raw)) != 1L) {
        stop("A token has inconsistent scales.", call. = FALSE)
      }
      reconstructed <-
        as.numeric(one_token$amount_raw) /
        as.numeric(one_token$scale_raw)

      if (any(!is.finite(reconstructed) | reconstructed <= 0) ||
          any(abs(one_token$amount / reconstructed - 1) > 1e-12)) {
        stop(
          "Prepared computational amounts do not agree with raw amounts and scales.",
          call. = FALSE
        )
      }
    }

    x
  }

  prepared <- list()
  for (sample_name in names(input_files)) {
    prepared[[sample_name]] <- read_flows(
      input_paths[[sample_name]],
      sample_name
    )
    cat("Prepared rows checked:", sample_name,
        nrow(prepared[[sample_name]]), "\n")
  }

  # ========================================================================
  # 3. WE DEFINE THE INTERNSHIP-REPORT / WTEYE-STYLE BALANCE CALCULATION
  # ========================================================================
  # This implementation calculates signed net balances using one grouped tapply()
  # calculation. Receiving contributes +amount; sending contributes -amount.
  balance_metrics <- function(movements, pool_address) {
    if (nrow(movements) < 1L) {
      stop("Balance metrics received an empty movement table.", call. = FALSE)
    }

    balances <- tapply(
      c(movements$amount, -movements$amount),
      c(movements$buyer, movements$seller),
      sum
    )

    balances <- balances[order(names(balances))]
    total_volume <- sum(movements$amount)
    l1_balance <- sum(abs(balances))

    if (!is.finite(total_volume) || total_volume <= 0 ||
        !is.finite(l1_balance)) {
      stop("Non-finite WTEYE-style balance calculation.", call. = FALSE)
    }

    # A transfer moves the same token from one source to one destination, so the
    # sum of all signed address balances should be zero apart from floating error.
    conservation_error <- abs(sum(balances))
    conservation_tolerance <- 1e-12 * max(1, total_volume)
    if (conservation_error > conservation_tolerance) {
      stop(
        "Token-balance conservation check failed during WTEYE-style calculation.",
        call. = FALSE
      )
    }

    ratio <- l1_balance / (2 * total_volume)

    pool_abs_balance <- if (pool_address %in% names(balances)) {
      abs(as.numeric(balances[[pool_address]]))
    } else {
      NA_real_
    }

    trader_balances <- balances[names(balances) != pool_address]
    trader_l1_balance <- if (length(trader_balances)) {
      sum(abs(trader_balances))
    } else {
      0
    }

    list(
      balances = balances,
      total_volume = total_volume,
      l1_balance = l1_balance,
      ratio = ratio,
      passes = is.finite(ratio) && ratio < margin,
      pool_abs_balance = pool_abs_balance,
      trader_l1_balance = trader_l1_balance,
      conservation_error = conservation_error
    )
  }

  # We independently recompute the same ratio using separate received/sent
  # aggregates. This gives us a second implementation to audit the first one.
  reference_balance_ratio <- function(movements) {
    addresses <- sort(unique(c(movements$seller, movements$buyer)))

    received <- vapply(addresses, function(address) {
      sum(movements$amount[movements$buyer == address])
    }, numeric(1))

    sent <- vapply(addresses, function(address) {
      sum(movements$amount[movements$seller == address])
    }, numeric(1))

    net <- received - sent
    sum(abs(net)) / (2 * sum(movements$amount))
  }

  checked_balance_metrics <- function(movements, pool_address) {
    primary <- balance_metrics(movements, pool_address)
    reference <- reference_balance_ratio(movements)

    tolerance <- 1e-12 * max(1, abs(reference), abs(primary$ratio))
    if (!is.finite(reference) ||
        abs(primary$ratio - reference) > tolerance) {
      stop(
        "The two independent WTEYE-style balance calculations disagree.",
        call. = FALSE
      )
    }

    primary
  }

  # ========================================================================
  # 4. WE IDENTIFY NONTRIVIAL SCC MOVEMENT SETS
  # ========================================================================
  scc_sequences <- function(movements) {
    if (nrow(movements) < 2L) return(list())

    graph <- igraph::graph_from_data_frame(
      movements[, c("seller", "buyer"), drop = FALSE],
      directed = TRUE
    )

    membership <- igraph::components(graph, mode = "strong")$membership
    vertices <- igraph::vertex_attr(graph, "name")
    vertex_groups <- split(vertices, membership)

    selected <- list()

    for (group in vertex_groups) {
      if (length(group) < 2L) next

      keep <-
        movements$seller %in% group &
        movements$buyer %in% group

      if (sum(keep) < 2L) next

      component <- movements[keep, , drop = FALSE]
      component <- component[
        order(component$swap_index, component$log_index),
        ,
        drop = FALSE
      ]
      rownames(component) <- NULL

      selected[[length(selected) + 1L]] <- component
    }

    selected
  }

  # ========================================================================
  # 5. WE RUN KNOWN-ANSWER SELF-CHECKS BEFORE TOUCHING THE REAL SAMPLES
  # ========================================================================
  toy <- function(sellers, buyers, amounts) {
    data.frame(
      row_id = seq_along(amounts),
      swap_index = seq_along(amounts),
      log_index = seq_along(amounts),
      seller = sellers,
      buyer = buyers,
      amount = amounts,
      stringsAsFactors = FALSE
    )
  }

  check <- function(condition, description) {
    if (!isTRUE(condition)) {
      stop(paste("SELF-CHECK FAILED:", description), call. = FALSE)
    }
    cat("PASS:", description, "\n")
  }

  exact_round_trip <- toy(
    c("POOL", "A"),
    c("A", "POOL"),
    c(100, 100)
  )

  # Ratio = 20 / (2 * 190) = 0.052631..., so this passes the report criterion.
  near_round_trip <- toy(
    c("POOL", "A"),
    c("A", "POOL"),
    c(100, 90)
  )

  # Ratio = 40 / (2 * 180) = 0.111111..., so this fails at 10%.
  imbalanced_round_trip <- toy(
    c("POOL", "A"),
    c("A", "POOL"),
    c(100, 80)
  )

  unrelated_one_way <- toy(
    c("A", "POOL"),
    c("POOL", "B"),
    c(100, 100)
  )

  exact_metrics <- checked_balance_metrics(exact_round_trip, "POOL")
  near_metrics <- checked_balance_metrics(near_round_trip, "POOL")
  imbalanced_metrics <- checked_balance_metrics(
    imbalanced_round_trip,
    "POOL"
  )

  check(
    abs(exact_metrics$ratio) < 1e-15 && exact_metrics$passes,
    "exact 100/100 round trip has ratio 0 and passes"
  )
  check(
    abs(near_metrics$ratio - (20 / 380)) < 1e-12 &&
      near_metrics$passes,
    "100/90 round trip has WTEYE-style ratio 20/380 and passes"
  )
  check(
    abs(imbalanced_metrics$ratio - (40 / 360)) < 1e-12 &&
      !imbalanced_metrics$passes,
    "100/80 round trip has WTEYE-style ratio 40/360 and fails"
  )

  scaled <- near_round_trip
  scaled$amount <- scaled$amount / 100
  scaled_metrics <- checked_balance_metrics(scaled, "POOL")
  check(
    abs(scaled_metrics$ratio - near_metrics$ratio) < 1e-12,
    "common token rescaling preserves the WTEYE-style ratio"
  )

  check(
    length(scc_sequences(unrelated_one_way)) == 0L,
    "a one-way pool path alone does not form a nontrivial SCC"
  )
  check(
    length(scc_sequences(exact_round_trip)) == 1L,
    "opposite pool/trader flows form one two-node SCC"
  )

  cat("Known-example checks complete.\n")

  # ========================================================================
  # 6. WE SEARCH ONE REAL SAMPLE
  # ========================================================================
  empty_matches <- data.frame(
    candidate_group_id = character(),
    token_slot = character(),
    event_key = character(),
    n_swap_events = integer(),
    n_addresses = integer(),
    n_initiators = integer(),
    first_block = integer(),
    last_block = integer(),
    block_span = integer(),
    source_window_start = integer(),
    source_window_end = integer(),
    contiguous_in_sample = logical(),
    total_volume_scaled = numeric(),
    l1_balance_scaled = numeric(),
    wteye_balance_ratio = numeric(),
    pool_abs_balance_scaled = numeric(),
    trader_l1_balance_scaled = numeric(),
    pool_fraction_of_l1 = numeric(),
    conservation_error_scaled = numeric(),
    margin = numeric(),
    margin_operator = character(),
    evaluation_mode = character(),
    classification = character(),
    stringsAsFactors = FALSE
  )

  detect_sample <- function(flows, sample_name) {
    # token0 supplies one metadata row per original swap. Step-2/reader checks
    # already established that token0 and token1 map to the same ordered swaps.
    swaps <- flows[
      flows$token_slot == "token0",
      c(
        "source_row", "swap_index", "event_id", "pool_address",
        "initiator_address", "transaction_hash",
        "block_number", "log_index"
      ),
      drop = FALSE
    ]
    rownames(swaps) <- NULL

    n <- nrow(swaps)
    pool_address <- swaps$pool_address[1L]

    token_tables <- list(
      token0 = flows[flows$token_slot == "token0", , drop = FALSE],
      token1 = flows[flows$token_slot == "token1", , drop = FALSE]
    )

    found <- list()
    seen <- character()

    windows <- 0L
    scc_views <- 0L
    balance_tests <- 0L

    start_time <- proc.time()[["elapsed"]]

    if (n >= 2L) {
      for (first in seq_len(n - 1L)) {
        last_allowed <- min(n, first + max_swaps - 1L)

        for (last in seq.int(first + 1L, last_allowed)) {
          if (
            swaps$block_number[last] -
            swaps$block_number[first] >
            max_block_span
          ) {
            break
          }

          windows <- windows + 1L

          for (slot in c("token0", "token1")) {
            window <- token_tables[[slot]][first:last, , drop = FALSE]
            components <- scc_sequences(window)
            scc_views <- scc_views + length(components)

            for (component in components) {
              balance_tests <- balance_tests + 1L

              metrics <- checked_balance_metrics(
                component,
                pool_address
              )

              # We evaluate the whole SCC movement set once. We do not truncate
              # it into a passing prefix.
              if (!metrics$passes) next

              event_key <- paste(component$swap_index, collapse = ";")
              token_key <- paste(slot, event_key, sep = "|")

              # The same SCC event set may reappear inside overlapping outer
              # windows. We keep one record per token + exact event set.
              if (token_key %in% seen) next

              balances <- metrics$balances
              l1 <- metrics$l1_balance
              pool_fraction <- if (l1 > 0 &&
                                   is.finite(metrics$pool_abs_balance)) {
                metrics$pool_abs_balance / l1
              } else if (l1 == 0) {
                0
              } else {
                NA_real_
              }

              found[[length(found) + 1L]] <- data.frame(
                candidate_group_id = "",
                token_slot = slot,
                event_key = event_key,
                n_swap_events = nrow(component),
                n_addresses = length(balances),
                n_initiators = length(
                  unique(component$initiator_address)
                ),
                first_block = min(component$block_number),
                last_block = max(component$block_number),
                block_span =
                  max(component$block_number) -
                  min(component$block_number),
                source_window_start = first,
                source_window_end = last,
                contiguous_in_sample =
                  nrow(component) ==
                  max(component$swap_index) -
                  min(component$swap_index) + 1L,
                total_volume_scaled = metrics$total_volume,
                l1_balance_scaled = metrics$l1_balance,
                wteye_balance_ratio = metrics$ratio,
                pool_abs_balance_scaled = metrics$pool_abs_balance,
                trader_l1_balance_scaled = metrics$trader_l1_balance,
                pool_fraction_of_l1 = pool_fraction,
                conservation_error_scaled =
                  metrics$conservation_error,
                margin = margin,
                margin_operator = "<",
                evaluation_mode =
                  "WHOLE_SCC_INTERNAL_MOVEMENTS_NO_PREFIX_TRUNCATION",
                classification =
                  "INTERNSHIP_REPORT_WTEYE_BALANCE_CANDIDATE",
                stringsAsFactors = FALSE
              )

              seen <- c(seen, token_key)
            }
          }
        }

        # These samples contain thousands of swaps. Reporting every 500 window
        # starts keeps the console readable without changing the search.
        if (first %% 500L == 0L || first == n - 1L) {
          cat(
            "  Completed window starts:",
            first,
            "of",
            n - 1L,
            "\n"
          )
        }
      }
    }

    search_seconds <-
      proc.time()[["elapsed"]] - start_time

    matches <- if (length(found)) {
      do.call(rbind, found)
    } else {
      empty_matches
    }
    rownames(matches) <- NULL

    # The same event set can pass for token0, token1, or both. We assign one
    # group ID per exact event set, while retaining one match row per token.
    keys <- unique(matches$event_key)
    group_ids <- sprintf(
      "%s_IRW_G%03d",
      sample_name,
      seq_along(keys)
    )

    if (nrow(matches)) {
      matches$candidate_group_id <-
        group_ids[match(matches$event_key, keys)]
    }

    swaps$candidate_token0 <- FALSE
    swaps$candidate_token1 <- FALSE
    swaps$candidate_group_ids <- ""

    for (k in seq_along(keys)) {
      ids <- as.integer(
        strsplit(keys[k], ";", fixed = TRUE)[[1L]]
      )

      selected <- swaps$swap_index %in% ids
      slots <- matches$token_slot[
        matches$event_key == keys[k]
      ]

      if ("token0" %in% slots) {
        swaps$candidate_token0[selected] <- TRUE
      }
      if ("token1" %in% slots) {
        swaps$candidate_token1[selected] <- TRUE
      }

      old <- swaps$candidate_group_ids[selected]
      swaps$candidate_group_ids[selected] <- ifelse(
        old == "",
        group_ids[k],
        paste(old, group_ids[k], sep = ";")
      )
    }

    # We intentionally preserve the existing branch's union rule for this first
    # controlled comparison. Later validation will report one-token versus
    # both-token confirmation separately.
    swaps$structural_candidate <-
      swaps$candidate_token0 |
      swaps$candidate_token1

    swaps$classification <- ifelse(
      swaps$structural_candidate,
      "INTERNSHIP_REPORT_WTEYE_BALANCE_CANDIDATE",
      "NO_MATCH_UNDER_CURRENT_SETTINGS"
    )

    swaps$sample <- sample_name
    selected <- swaps$structural_candidate

    summary <- data.frame(
      sample = sample_name,
      pool_address = pool_address,
      input_swap_events = n,
      input_token_movements = nrow(flows),
      candidate_groups = length(keys),
      token_matches = nrow(matches),
      candidate_swap_events = sum(selected),
      candidate_transactions = length(
        unique(swaps$transaction_hash[selected])
      ),
      candidate_initiators = length(
        unique(swaps$initiator_address[selected])
      ),
      both_token_candidate_swaps = sum(
        swaps$candidate_token0 &
        swaps$candidate_token1
      ),
      token0_candidate_swaps = sum(swaps$candidate_token0),
      token1_candidate_swaps = sum(swaps$candidate_token1),
      eligible_windows = windows,
      scc_views = scc_views,
      balance_tests = balance_tests,
      margin = margin,
      margin_operator = "<",
      max_swaps = max_swaps,
      max_block_span = max_block_span,
      search_with_audit_seconds = search_seconds,
      prepared_input_file = input_files[[sample_name]],
      prepared_input_md5 =
        unname(tools::md5sum(input_paths[[sample_name]])),
      step3_script_md5 = script_md5,
      r_version = R.version.string,
      igraph_version =
        as.character(utils::packageVersion("igraph")),
      run_time_utc =
        format(Sys.time(), tz = "UTC", usetz = TRUE),
      balance_method =
        "L1_NET_BALANCE_DIVIDED_BY_2_TOTAL_VOLUME",
      structural_method =
        "OVERLAPPING_SHORT_WINDOWS_THEN_STRONG_COMPONENTS",
      scc_evaluation =
        "WHOLE_SCC_INTERNAL_MOVEMENTS_NO_PREFIX_TRUNCATION",
      candidate_rule =
        "AT_LEAST_ONE_TOKEN_MATCH",
      window_design =
        "PRESERVED_FROM_PREVIOUS_AMM_ADAPTATION",
      address_attribution =
        "TRANSACTION_INITIATOR_PROXY",
      full_original_wteye_replication =
        "NO_BALANCE_CRITERION_ONLY",
      entity_and_mev_checks =
        "NOT_PERFORMED",
      ground_truth_validation =
        "NOT_PERFORMED",
      stringsAsFactors = FALSE
    )

    list(
      matches = matches,
      swaps = swaps,
      summary = summary
    )
  }

  # ========================================================================
  # 7. WE RUN THE SAME DETECTOR ON BOTH SAMPLES
  # ========================================================================
  results <- list()

  for (sample_name in names(prepared)) {
    cat("\nSearching:", sample_name, "\n")

    results[[sample_name]] <- detect_sample(
      prepared[[sample_name]],
      sample_name
    )

    print(
      results[[sample_name]]$summary[, c(
        "sample",
        "input_swap_events",
        "candidate_groups",
        "token_matches",
        "candidate_swap_events",
        "candidate_transactions",
        "both_token_candidate_swaps"
      )],
      row.names = FALSE
    )
  }

  # ========================================================================
  # 8. WE SAVE FIVE REPORTS WITHOUT TOUCHING THE C++ BRANCH OUTPUTS
  # ========================================================================
  if (!identical(
    input_hashes_before,
    unname(tools::md5sum(input_paths))
  )) {
    stop(
      "A prepared input changed during the run. No WTEYE-style reports were written.",
      call. = FALSE
    )
  }

  write_report <- function(table, filename) {
    table_for_disk <- table

    # We retain enough digits to reproduce ordinary R double values when the
    # CSV is read again. Exact raw token quantities remain in Step-2 inputs.
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

  for (sample_name in names(results)) {
    write_report(
      results[[sample_name]]$matches,
      paste0(
        "amm_adaptation_additional_pool_eco_usdt_output_",
        sample_name,
        "_internship_report_wteye_token_matches.csv"
      )
    )

    write_report(
      results[[sample_name]]$swaps,
      paste0(
        "amm_adaptation_additional_pool_eco_usdt_output_",
        sample_name,
        "_internship_report_wteye_swaps_labeled.csv"
      )
    )
  }

  summaries <- do.call(
    rbind,
    lapply(results, function(result) result$summary)
  )
  rownames(summaries) <- NULL

  write_report(
    summaries,
    "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_detection_summary.csv"
  )

  cat("\nSTEP 3 COMPLETE: internship-report / WTEYE-style balance search finished.\n")
  cat("Prepared input fingerprints unchanged: TRUE\n")
  cat("Two independent R balance calculations agreed on every tested SCC.\n")
  cat("No volume_matching.cpp classification was used in this branch.\n")
  cat("Results are structural balance candidates, not confirmed wash trades.\n")
  cat("Send the complete console output and all five generated CSVs for review.\n")
})
