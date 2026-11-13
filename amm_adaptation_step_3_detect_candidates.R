# ============================================================================
# AMM ADAPTATION -- STEP 3: FIND STRUCTURAL CANDIDATES
# ============================================================================
#
# OUR PURPOSE
# -----------
# We apply our internship graph-and-balance approach to the token-flow tables
# prepared in Step 2. We ask whether a short sequence contains a connected group
# whose incoming and outgoing amounts nearly cancel for EACH participating
# address, including the pool. We save the groups and their original swap IDs.
#
# We call the results STRUCTURAL CANDIDATES. A trader can buy and sell similar
# quantities for ordinary reasons. This program does not establish manipulative
# intent, common ownership, or the absence of arbitrage. A FALSE candidate flag
# means no match under this search; it does not prove a trade is legitimate.
# The words suspicious_pool/control_pool identify our samples, not true labels.
#
# WHAT WE HAVE ADAPTED
# -------------------
# 1. Our input describes AMM swaps through a pool rather than matched orders.
# 2. Our source/destination edges are attributed to the transaction initiator.
#    That is a proxy assumption, not verified ERC20 transfer ownership.
# 3. We examine each pool and each token separately. We never add different
#    tokens together, even though both appear in one prepared CSV.
# 4. We use block/log ordering and bounded overlapping sequences of swaps.
# 5. We retain SCC grouping and the EXISTING C++ balance kernel. That kernel
#    takes seller, buyer, amount, and wash_label; it does not read Zenodo files.
# 6. We map matches back to original events and count overlapping events once.
#
# This is an initial adaptation of our internship approach. It is separate from
# our replication of the AMM paper. It does not yet implement entity recognition,
# multi-pool routes, or flash-loan/MEV analysis. We have only the supplied samples;
# we cannot infer that every swap on the blockchain is present in these files.
#
# OUR STEPS IN EVERY RUN
# ---------------------
# Read/check prepared inputs -> load C++ -> check known examples -> select short
# sequences -> separate tokens -> find SCCs -> match balances -> save candidates.
#
# OUR THREE MAIN IDEAS, IN PLAIN LANGUAGE
# --------------------------------------
# A window is a short consecutive part of the supplied swap list.
# A directed graph is a map of addresses connected by token-movement arrows.
# An SCC (strongly connected component) is a group in which every address can
# reach every other address by following the arrows. We use igraph to find it.
# Official documentation: https://r.igraph.org/reference/components.html
#
# In a pool graph, addresses that both send and receive the token can fall in
# one SCC merely because they use the same pool. This does NOT show collusion.
# A pool sending to A and receiving from unrelated B is also not, by itself,
# evidence that A and B form a wash-trading group.
#
# After SCC filtering, an examined sequence can omit unrelated swaps. We retain
# event IDs and report contiguous_in_sample to make such gaps visible. We do
# not claim that graph connectivity alone establishes a time-respecting cycle.
# We calculate SCCs for the window before matching. A smaller matched prefix
# need not itself be an SCC; we report its balance evidence and original IDs.
#
# OUR ACTUAL C++ RULE
# ------------------
# For the ONE token being examined:
#   balance(address) = amount received - amount sent
#   mean_amount = total movement amount / number of movements
# We require EVERY address, including the pool, to satisfy:
#   abs(balance(address)) / mean_amount <= 0.10
# abs() means absolute value: the size of a number without its plus/minus sign.
#
# Example: pool -> A: 100, then A -> pool: 96.
# Mean = 98. Both absolute balances are 4. Ratio = 4/98, about 0.0408.
# This passes 0.10. Returning only 80 gives 20/90, about 0.2222, and fails.
# This is NOT a percentage of pool reserves, a probability, or a fraud score.
# It is also NOT the sum of all absolute balances divided by twice the volume;
# that would be a different rule. We describe the supplied code's actual rule.
#
# The C++ file first checks the whole sequence. If it fails, it removes the
# LAST row and checks again. It returns the longest passing beginning, called
# a prefix, with at least two rows. It does not search every possible subset.
# Our R wrapper removes a matched prefix and repeats on the remaining rows,
# up to max_passes. If no prefix matches, that pass loop stops. Overlapping
# outer windows allow additional starting positions, but do not make this an
# exhaustive search over all trade combinations.
#
# Our per-token rescaling from Step 2 cancels in the balance/mean ratio. It
# does not provide token decimals or USD prices. Calculations use approximate
# numbers. We independently recompute every C++ answer in R and stop if the two
# answers disagree. This is a numerical/implementation check, not fraud evidence.
#
# OUR FILES
# ---------
# We save this script in:
# D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_3_detect_candidates.R
# We use the existing volume_matching.cpp in that SAME Scripts folder.
# Our two prepared inputs stay in Data/cleaned with their Step 2 filenames.
# We save five new amm_adaptation_output_ CSVs directly in Data/cleaned.
# Rerunning refreshes only these five generated reports. It does not write to
# the original raw CSVs, the prepared input CSVs, or volume_matching.cpp.
# Compilation uses R's temporary cache; no new project folder is needed.
#
# HOW WE RUN THE WHOLE SCRIPT IN OUR R CONSOLE
# ------------------------------------------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_3_detect_candidates.R")
# We review the console and five output CSVs before the next checkpoint.
#
# HOW WE READ THIS CODE
# --------------------
# Lines beginning with # are explanations. <- stores a value under a name.
# A function is a reusable operation. A data.frame is a table. A list holds
# several objects. TRUE/FALSE mean yes/no. NA means missing. stop() ends a run
# with an explanation. We use functions below so both samples follow the same
# rules. We do not change thresholds according to a sample's name or results.

local({
  # ========================================================================
  # 1. WE SET OUR PATHS AND RECORD OUR STARTING SEARCH SETTINGS
  # ========================================================================
  base_dir <- "D:/Downloads/Bachelor Thesis"
  scripts_dir <- file.path(base_dir, "Scripts")
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")
  cpp_path <- file.path(scripts_dir, "volume_matching.cpp")
  input_files <- c(
    suspicious_pool = "amm_adaptation_input_suspicious_pool_token_flows.csv",
    control_pool = "amm_adaptation_input_control_pool_token_flows.csv"
  )
  input_paths <- file.path(cleaned_dir, unname(input_files))
  names(input_paths) <- names(input_files)

  # These are starting choices for our small adaptation experiment, not claimed
  # optimal settings or parameters quoted from the AMM paper. We keep them the
  # same for both samples and record them in the summary. Later sensitivity
  # analysis can show how results change when these choices change.
  margin <- 0.10
  max_swaps <- 10L
  max_block_span <- 200L
  max_passes <- 10L
  # 10L is R's notation for an integer 10. A window has 2 through 10 supplied
  # swaps. Its last block minus its first block must be at most 200. This is a
  # block-distance restriction, not a fixed number of minutes or hours.

  cat("AMM ADAPTATION -- STEP 3: FIND STRUCTURAL CANDIDATES\n\n")
  cat("R version:", R.version.string, "\n")
  cat("Prepared inputs:", cleaned_dir, "\nC++ source:", cpp_path, "\n")
  cat("Settings: margin =", margin, "; max swaps =", max_swaps,
      "; max block span =", max_block_span, "; max passes =", max_passes, "\n")

  # requireNamespace() checks an installed package without changing our setup.
  # We use igraph for SCCs and Rcpp to call the C++ function from R.
  for (package in c("igraph", "Rcpp")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(paste("Missing R package:", package, "-- send the console output."), call. = FALSE)
    }
  }
  if (!all(file.exists(c(input_paths, cpp_path)))) {
    stop("A prepared input or Scripts/volume_matching.cpp is missing. Send the console output.", call. = FALSE)
  }
  input_hashes_before <- unname(tools::md5sum(input_paths))
  cpp_md5 <- unname(tools::md5sum(cpp_path))

  # We check that our C++ file is the source version inspected for this workflow.
  # These four fingerprints allow Unix/Windows line endings, with or without a
  # final newline. A different source requires inspection before we reuse it.
  # We never replace a different C++ file automatically.
  reviewed_cpp_hashes <- c(
    "3789308cade82718250120f53a64e077", "d7d01fdae0d599d8d8151bfef5124761",
    "47d7d0a7668737b44e66aac72dea6051", "fd892ff3da6eda28c0ef5475acc54c78"
  )
  cat("C++ MD5:", cpp_md5, "\n")
  if (!(cpp_md5 %in% reviewed_cpp_hashes)) {
    stop("The C++ file differs from the reviewed version. Send volume_matching.cpp and this output for review.", call. = FALSE)
  }

  # ========================================================================
  # 2. WE READ OUR PREPARED TABLES WITHOUT LOSING LARGE INTEGER DIGITS
  # ========================================================================
  read_flows <- function(path, sample_name) {
    x <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = "")
    required <- c("sample", "source_file", "source_row", "event_id", "swap_index",
      "row_id", "pool_address", "initiator_address", "transaction_hash",
      "block_number", "log_index", "token_slot", "direction", "seller", "buyer",
      "amount_raw", "scale_raw", "amount")
    if (!nrow(x) || anyDuplicated(names(x)) || !all(required %in% names(x))) {
      stop(paste(sample_name, "has an empty or unexpected prepared table."), call. = FALSE)
    }
    if (anyNA(x[, required, drop = FALSE])) {
      stop(paste(sample_name, "has missing essential prepared values."), call. = FALSE)
    }
    if (any(x$sample != sample_name) || length(unique(x$pool_address)) != 1L ||
        any(!(x$token_slot %in% c("token0", "token1")))) {
      stop("Prepared sample/pool/token identifiers are inconsistent.", call. = FALSE)
    }
    # We explicitly convert ONLY indices and the computational amount. Original
    # amount_raw and scale_raw stay as text, as do transaction/address IDs.
    for (field in c("source_row", "swap_index", "row_id", "block_number", "log_index")) {
      numbers <- suppressWarnings(as.numeric(x[[field]]))
      if (any(!is.finite(numbers) | numbers < 0 | numbers != floor(numbers) |
              numbers > .Machine$integer.max)) stop(paste("Invalid index:", field), call. = FALSE)
      x[[field]] <- as.integer(numbers)
    }
    x$amount <- suppressWarnings(as.numeric(x$amount))
    if (any(!is.finite(x$amount) | x$amount <= 0)) stop("Invalid computational amount.", call. = FALSE)
    for (field in c("amount_raw", "scale_raw")) {
      if (any(!grepl("^[0-9]+$", x[[field]]))) stop("Raw quantity digits were changed.", call. = FALSE)
    }
    if (any(x$row_id != x$swap_index)) stop("row_id no longer matches swap_index.", call. = FALSE)
    if (anyDuplicated(paste(x$event_id, x$token_slot, sep = "|"))) {
      stop("Repeated event/token rows in a prepared input.", call. = FALSE)
    }
    # Each slot must describe the SAME ordered set of original swaps. This
    # prevents a missing/repeated leg from being mistaken for another event.
    x <- x[order(x$swap_index, x$token_slot), , drop = FALSE]
    rownames(x) <- NULL
    a <- x[x$token_slot == "token0", , drop = FALSE]
    b <- x[x$token_slot == "token1", , drop = FALSE]
    common <- c("swap_index", "row_id", "source_row", "event_id", "pool_address",
                "initiator_address", "transaction_hash", "block_number", "log_index")
    aa <- a[, common, drop = FALSE]; rownames(aa) <- NULL
    bb <- b[, common, drop = FALSE]; rownames(bb) <- NULL
    if (!nrow(a) || !identical(aa, bb) || !identical(a$swap_index, seq_len(nrow(a)))) {
      stop("The two token lists do not map to the same consecutive swap IDs.", call. = FALSE)
    }
    if (!identical(order(a$block_number, a$log_index), seq_len(nrow(a))) ||
        anyDuplicated(paste(a$block_number, a$log_index, sep = ":"))) {
      stop("Chronological block/log order is inconsistent.", call. = FALSE)
    }
    if (any(a$direction == b$direction)) stop("Both token legs have the same direction.", call. = FALSE)
    into <- x$direction == "INTO_POOL"
    if (any(!(x$direction %in% c("INTO_POOL", "OUT_OF_POOL"))) ||
        any(x$seller != ifelse(into, x$initiator_address, x$pool_address)) ||
        any(x$buyer != ifelse(into, x$pool_address, x$initiator_address)) ||
        any(x$seller == x$buyer)) stop("Prepared source/destination mapping is inconsistent.", call. = FALSE)
    for (slot in c("token0", "token1")) {
      one_token <- x[x$token_slot == slot, , drop = FALSE]
      if (length(unique(one_token$scale_raw)) != 1L) stop("A token has inconsistent scales.", call. = FALSE)
      reconstructed <- as.numeric(one_token$amount_raw) / as.numeric(one_token$scale_raw)
      if (any(!is.finite(reconstructed) | reconstructed <= 0) ||
          any(abs(one_token$amount / reconstructed - 1) > 1e-12)) {
        stop("Prepared computational amounts do not agree with raw amounts and scales.", call. = FALSE)
      }
    }
    x
  }
  prepared <- list()
  for (sample_name in names(input_files)) {
    prepared[[sample_name]] <- read_flows(input_paths[[sample_name]], sample_name)
    cat("Prepared rows checked:", sample_name, nrow(prepared[[sample_name]]), "\n")
  }

  # ========================================================================
  # 3. WE LOAD OUR EXISTING C++ BALANCE FUNCTION
  # ========================================================================
  # sourceCpp() compiles the source if necessary and exposes the exported
  # function to R. This is our first actual compiler check in this workflow.
  # A separate environment stores that function without replacing similarly
# named functions in our main R workspace. R's temporary cache stores builds.
  cat("\nLoading the C++ matcher; compilation may take a moment...\n")
  cpp_environment <- new.env(parent = globalenv())
  tryCatch(
    Rcpp::sourceCpp(cpp_path, env = cpp_environment, cacheDir = tempdir(), verbose = FALSE),
    error = function(e) stop(paste0(
      "C++ loading/compilation failed. Send the full console output. Original error: ",
      conditionMessage(e)), call. = FALSE)
  )
  if (!exists("detect_label_wash_trades", envir = cpp_environment, inherits = FALSE)) {
    stop("The expected C++ function was not loaded.", call. = FALSE)
  }
  cat("C++ function loaded.\n")

  # ========================================================================
  # 4. WE DEFINE A SEPARATE R CALCULATION TO AUDIT THE C++ ANSWERS
  # ========================================================================
  balances_for <- function(movements) {
    # tapply groups signed amounts by address and sums them. Receiving adds;
    # sending subtracts. The pool is included exactly like the other nodes.
    tapply(c(movements$amount, -movements$amount),
           c(movements$buyer, movements$seller), sum)
  }
  reference_prefix <- function(movements) {
    count <- nrow(movements)
    if (count < 2L) return(rep(FALSE, count))
    # We recompute each prefix from its own rows. This avoids the C++ approach
    # of subtracting the last row from accumulated balances.
    for (last in seq.int(count, 2L)) {
      prefix <- movements[seq_len(last), , drop = FALSE]
      ratios <- abs(balances_for(prefix)) / mean(prefix$amount)
      if (all(ratios <= margin)) return(seq_len(count) <= last)
    }
    rep(FALSE, count)
  }
  match_checked <- function(movements) {
    # The upstream C++ function assumes rows exist. We guard empty/single-row
    # inputs here; neither contains a two-movement round trip.
    if (nrow(movements) < 2L) return(rep(FALSE, nrow(movements)))
    input <- movements[, c("row_id", "seller", "buyer", "amount"), drop = FALSE]
    input$wash_label <- rep(FALSE, nrow(input))
    # wash_label is the historical C++ interface name. TRUE is interpreted only
    # as a balance match here. We use candidate names in our external reports.
    result <- cpp_environment$detect_label_wash_trades(input, margin = margin)
    if (!identical(result$row_id, input$row_id) || nrow(result) != nrow(input)) {
      stop("The C++ function changed the row identities/order.", call. = FALSE)
    }
    labels <- as.logical(result$wash_label)
    expected <- reference_prefix(movements)
    if (length(labels) != nrow(input) || anyNA(labels) || !identical(labels, expected)) {
      stop("C++ and independent R prefix calculations disagree. Send the console output for numerical review.", call. = FALSE)
    }
    labels
  }

  # ========================================================================
  # 5. WE SELECT SCCs FROM ONE POOL, ONE TOKEN, AND ONE WINDOW
  # ========================================================================
  scc_sequences <- function(movements) {
    if (nrow(movements) < 2L) return(list())
    graph <- igraph::graph_from_data_frame(
      movements[, c("seller", "buyer"), drop = FALSE], directed = TRUE)
    membership <- igraph::components(graph, mode = "strong")$membership
    vertices <- igraph::vertex_attr(graph, "name")
    vertex_groups <- split(vertices, membership)
    selected <- list()
    for (group in vertex_groups) {
      if (length(group) < 2L) next
      keep <- movements$seller %in% group & movements$buyer %in% group
      if (sum(keep) < 2L) next
      # We retain the input chronological order when selecting an SCC's rows.
      selected[[length(selected) + 1L]] <- movements[keep, , drop = FALSE]
    }
    # We compute strong components directly. Removing zero-in/out-degree nodes
    # first would not change which nontrivial SCCs survive this selection.
    selected
  }

  # ========================================================================
  # 6. WE CHECK EXAMPLES WITH KNOWN MATHEMATICAL ANSWERS
  # ========================================================================
  # These are invented inputs to check our software, not experimental evidence
  # about either real pool. Test labels mean pass/fail of our balance rule.
  toy <- function(sellers, buyers, amounts) {
    data.frame(row_id = seq_along(amounts), seller = sellers, buyer = buyers,
               amount = amounts, stringsAsFactors = FALSE)
  }
  check <- function(condition, description) {
    if (!isTRUE(condition)) stop(paste("SELF-CHECK FAILED:", description), call. = FALSE)
    cat("PASS:", description, "\n")
  }
  balanced <- toy(c("POOL", "A"), c("A", "POOL"), c(100, 100))
  near_balance <- toy(c("POOL", "A"), c("A", "POOL"), c(100, 96))
  imbalanced <- toy(c("POOL", "A"), c("A", "POOL"), c(100, 80))
  unrelated <- toy(c("POOL", "B"), c("A", "POOL"), c(100, 100))
  suffix <- toy(c("POOL", "A", "POOL"), c("A", "POOL", "B"), c(100, 100, 20))
  leading <- toy(c("POOL", "POOL", "A"), c("B", "A", "POOL"), c(20, 100, 100))
  check(all(match_checked(balanced)), "100 received / 100 returned passes")
  check(all(match_checked(near_balance)), "100 received / 96 returned passes")
  check(!any(match_checked(imbalanced)), "100 received / 80 returned fails")
  check(!any(match_checked(unrelated)), "unrelated one-way addresses fail balance matching")
  check(identical(match_checked(suffix), c(TRUE, TRUE, FALSE)), "longest passing prefix is retained")
  check(!any(match_checked(leading)), "C++ alone does not skip an unmatched first row")
  scaled <- near_balance; scaled$amount <- scaled$amount / 100
  check(identical(match_checked(near_balance), match_checked(scaled)), "common token rescaling preserves this answer")
  check(length(scc_sequences(unrelated)) == 0L, "a pool connection alone does not form an SCC")
  check(length(scc_sequences(balanced)) == 1L, "opposite flows form one two-node SCC")
  check(length(match_checked(balanced[FALSE, , drop = FALSE])) == 0L,
        "empty input is guarded before C++")
  cat("Known-example checks complete.\n")

  # ========================================================================
  # 7. WE SEARCH THE REAL SAMPLES USING THE SAME SETTINGS
  # ========================================================================
  # An empty report still has column headings. A sample with zero matches is
  # a valid possible result, not a reason to change our parameters.
  empty_matches <- data.frame(
    candidate_group_id = character(), token_slot = character(), event_key = character(),
    n_swap_events = integer(), n_initiators = integer(), first_block = integer(),
    last_block = integer(), block_span = integer(), source_window_start = integer(),
    source_window_end = integer(), pass = integer(), contiguous_in_sample = logical(),
    mean_amount_scaled = numeric(), max_abs_balance_over_mean = numeric(),
    pool_abs_balance_over_mean = numeric(), margin = numeric(),
    classification = character(), stringsAsFactors = FALSE)

  detect_sample <- function(flows, sample_name) {
    # token0 supplies one metadata row per swap. Both token tables have already
    # been checked to refer to this same ordered swap list.
    swaps <- flows[flows$token_slot == "token0", c("source_row", "swap_index",
      "event_id", "pool_address", "initiator_address", "transaction_hash",
      "block_number", "log_index"), drop = FALSE]
    rownames(swaps) <- NULL
    n <- nrow(swaps)
    token_tables <- list(
      token0 = flows[flows$token_slot == "token0", , drop = FALSE],
      token1 = flows[flows$token_slot == "token1", , drop = FALSE]
    )
    found <- list()
    # Our seen keys remove repeated findings across overlapping windows. The
    # key includes the token so a token0 match cannot hide a token1 match.
    seen <- character()
    windows <- 0L
    scc_views <- 0L
    cpp_calls <- 0L
    start_time <- proc.time()[["elapsed"]]
    if (n >= 2L) {
      # The outer loop moves our starting position through the swap list. The
      # inner loop tries each permitted ending position. break leaves a loop;
      # next skips to its next iteration. %in% means "is included in this set".
      for (first in seq_len(n - 1L)) {
        last_allowed <- min(n, first + max_swaps - 1L)
        for (last in seq.int(first + 1L, last_allowed)) {
          if (swaps$block_number[last] - swaps$block_number[first] > max_block_span) break
          windows <- windows + 1L
          for (slot in c("token0", "token1")) {
            window <- token_tables[[slot]][first:last, , drop = FALSE]
            components <- scc_sequences(window)
            scc_views <- scc_views + length(components)
            for (component in components) {
              remaining <- component
              for (pass_number in seq_len(max_passes)) {
                if (nrow(remaining) < 2L) break
                cpp_calls <- cpp_calls + 1L
                chosen <- match_checked(remaining)
                if (!any(chosen)) break
                hit <- remaining[chosen, , drop = FALSE]
                # A semicolon-separated event_key lists the original ordered
                # swap indices. It is not a new transaction identifier.
                event_key <- paste(hit$swap_index, collapse = ";")
                token_key <- paste(slot, event_key, sep = "|")
                if (!(token_key %in% seen)) {
                  balances <- balances_for(hit)
                  mean_amount <- mean(hit$amount)
                  pool <- swaps$pool_address[1L]
                  found[[length(found) + 1L]] <- data.frame(
                    candidate_group_id = "", token_slot = slot, event_key = event_key,
                    n_swap_events = nrow(hit), n_initiators = length(unique(hit$initiator_address)),
                    first_block = min(hit$block_number), last_block = max(hit$block_number),
                    block_span = max(hit$block_number) - min(hit$block_number),
                    source_window_start = first, source_window_end = last, pass = pass_number,
                    contiguous_in_sample = nrow(hit) == max(hit$swap_index) - min(hit$swap_index) + 1L,
                    mean_amount_scaled = mean_amount,
                    max_abs_balance_over_mean = max(abs(balances)) / mean_amount,
                    pool_abs_balance_over_mean = as.numeric(abs(balances[pool]) / mean_amount),
                    margin = margin, classification = "STRUCTURAL_BALANCE_CANDIDATE",
                    stringsAsFactors = FALSE)
                  seen <- c(seen, token_key)
                }
                remaining <- remaining[!chosen, , drop = FALSE]
              }
            }
          }
        }
        # %% gives a division remainder, so we print progress every 10 starts.
        if (first %% 10L == 0L) cat("  Completed window starts:", first, "of", n - 1L, "\n")
      }
    }
    # This duration includes our R audit on each C++ call. It excludes input
    # reading, C++ loading, known-example checks, and CSV writing. It is a run
    # diagnostic, not a fair speed comparison with another implementation.
    search_seconds <- proc.time()[["elapsed"]] - start_time
    matches <- if (length(found)) do.call(rbind, found) else empty_matches
    rownames(matches) <- NULL
    keys <- unique(matches$event_key)
    group_ids <- sprintf("%s_G%03d", sample_name, seq_along(keys))
    if (nrow(matches)) matches$candidate_group_id <- group_ids[match(matches$event_key, keys)]

    # The same event set can pass for one token or both. We keep token evidence
    # separate in the match report, while giving the identical set one group ID.
    # Different overlapping sets remain different groups. They are NOT distinct
    # colluding entities, and their row counts must not be added as unique trades.
    swaps$candidate_token0 <- FALSE
    swaps$candidate_token1 <- FALSE
    swaps$candidate_group_ids <- ""
    for (k in seq_along(keys)) {
      ids <- as.integer(strsplit(keys[k], ";", fixed = TRUE)[[1L]])
      selected <- swaps$swap_index %in% ids
      slots <- matches$token_slot[matches$event_key == keys[k]]
      if ("token0" %in% slots) swaps$candidate_token0[selected] <- TRUE
      if ("token1" %in% slots) swaps$candidate_token1[selected] <- TRUE
      old <- swaps$candidate_group_ids[selected]
      swaps$candidate_group_ids[selected] <- ifelse(old == "", group_ids[k], paste(old, group_ids[k], sep = ";"))
    }
    # Our union rule marks a swap if it belongs to at least one match for either
    # token. It does not require the same group to pass for both tokens. The two
    # separate flags retain that distinction for later analysis.
    swaps$structural_candidate <- swaps$candidate_token0 | swaps$candidate_token1
    swaps$classification <- ifelse(swaps$structural_candidate,
      "STRUCTURAL_BALANCE_CANDIDATE", "NO_MATCH_UNDER_CURRENT_SETTINGS")
    swaps$sample <- sample_name
    selected <- swaps$structural_candidate
    summary <- data.frame(
      sample = sample_name, pool_address = swaps$pool_address[1L],
      input_swap_events = n, input_token_movements = nrow(flows),
      candidate_groups = length(keys), token_matches = nrow(matches),
      candidate_swap_events = sum(selected),
      candidate_transactions = length(unique(swaps$transaction_hash[selected])),
      candidate_initiators = length(unique(swaps$initiator_address[selected])),
      eligible_windows = windows, scc_views = scc_views, cpp_calls = cpp_calls,
      margin = margin, max_swaps = max_swaps, max_block_span = max_block_span,
      max_passes = max_passes, search_with_audit_seconds = search_seconds,
      prepared_input_file = input_files[[sample_name]],
      prepared_input_md5 = unname(tools::md5sum(input_paths[[sample_name]])),
      cpp_md5 = cpp_md5, r_version = R.version.string,
      igraph_version = as.character(utils::packageVersion("igraph")),
      rcpp_version = as.character(utils::packageVersion("Rcpp")),
      run_time_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      candidate_rule = "AT_LEAST_ONE_TOKEN_MATCH",
      address_attribution = "TRANSACTION_INITIATOR_PROXY",
      entity_and_mev_checks = "NOT_PERFORMED", ground_truth_validation = "NOT_PERFORMED",
      stringsAsFactors = FALSE)
    list(matches = matches, swaps = swaps, summary = summary)
  }

  results <- list()
  for (sample_name in names(prepared)) {
    cat("\nSearching:", sample_name, "\n")
    results[[sample_name]] <- detect_sample(prepared[[sample_name]], sample_name)
    print(results[[sample_name]]$summary[, c("sample", "input_swap_events",
      "candidate_groups", "token_matches", "candidate_swap_events", "candidate_transactions")], row.names = FALSE)
  }

  # ========================================================================
  # 8. WE SAVE FIVE REPORTS AFTER BOTH SAMPLES FINISH SUCCESSFULLY
  # ========================================================================
  if (!identical(input_hashes_before, unname(tools::md5sum(input_paths))) ||
      !identical(cpp_md5, unname(tools::md5sum(cpp_path)))) {
    stop("A prepared input or C++ source changed during the run. No new reports were written.", call. = FALSE)
  }
  write_report <- function(table, filename) {
    # We preserve the precision of approximate results when writing them. The
    # original exact raw quantities remain in our unchanged prepared inputs.
    for (field in names(table)) {
      if (is.double(table[[field]])) table[[field]] <- sprintf("%.17g", table[[field]])
    }
    path <- file.path(cleaned_dir, filename)
    if (file.exists(path)) cat("Refreshing our generated report:", filename, "\n")
    utils::write.csv(table, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    cat("Saved:", path, "\n")
  }
  for (sample_name in names(results)) {
    write_report(results[[sample_name]]$matches,
      paste0("amm_adaptation_output_", sample_name, "_token_matches.csv"))
    write_report(results[[sample_name]]$swaps,
      paste0("amm_adaptation_output_", sample_name, "_swaps_labeled.csv"))
  }
  summaries <- do.call(rbind, lapply(results, function(result) result$summary))
  rownames(summaries) <- NULL
  write_report(summaries, "amm_adaptation_output_both_pools_detection_summary.csv")

  cat("\nSTEP 3 COMPLETE: structural candidate search finished.\n")
  cat("Prepared input and C++ fingerprints unchanged: TRUE\n")
  cat("C++ answers agreed with the independent R prefix calculation.\n")
  cat("Results are candidates under our settings, not confirmed wash trades.\n")
  cat("Send the complete console output and all five generated CSVs for review.\n")
})
