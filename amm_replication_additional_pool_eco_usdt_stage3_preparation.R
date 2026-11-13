# AMM REPLICATION -- ECO-USDT -- STAGE 3 EVIDENCE PREPARATION
# Run AFTER the completed Version 3.1 Stage-2 run. No Dune/API calls are made.
# This companion reads saved results so running it cannot replay Stage 2.
# It adds no wash-trading decision and changes no Stage-1 or Stage-2 rule.
# Plain-language map: read saved decisions -> keep passing groups -> list each
# transaction once -> write requests for evidence needed by the paper's 7.3.
#
# Output inventory: every name starts with
# amm_replication_additional_pool_eco_usdt_output_
# both_pools_stage3_candidate_transaction_map.csv: one row per group/member swap;
#   the same transaction can belong to several overlapping groups.
# both_pools_stage3_unique_transactions.csv: one row per transaction to retrieve.
# both_pools_stage3_preparation_summary.csv: group, event, transaction/block totals.
# both_pools_stage3_01_transaction_coverage.sql: check each transaction is present.
# both_pools_stage3_02_raw_logs_and_sandwich_context.sql: original event records
#   plus swaps in the same blocks/pools, including possible intervening victims.
# both_pools_stage3_03_decoded_dex_activity.sql: named exchange/pool trade records.
# both_pools_stage3_04_address_identity_evidence.sql: recipient/contract labels.
# both_pools_stage3_preparation_manifest.csv: input/output file fingerprints.
#
# Corrections carried forward: ordinary arbitrage is checked at GROUP level;
# multi-block groups cannot automatically bypass all sandwich checks; Stage-2
# public services are not automatically Stage-3 exchange/arbitrage evidence.
# Unidentified public services are retained. Not identified does not mean private.
# Stage 3 classification waits for downloaded evidence and coverage checks.

local({
  # Change this one folder only if your cleaned data are stored elsewhere.
  cleaned_dir <- "D:/Downloads/Bachelor Thesis/Data/cleaned"
  prefix <- "amm_replication_additional_pool_eco_usdt_output_"
  path_for <- function(ending) file.path(cleaned_dir, paste0(prefix, ending))
  # Reading everything as text preserves addresses and large integer amounts.
  read_table <- function(path) {
    if (!file.exists(path)) stop("Missing input: ", path)
    read.csv(path, colClasses = "character", check.names = FALSE,
             stringsAsFactors = FALSE, na.strings = NULL)
  }
  check <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
  }
  require_columns <- function(tab, cols) {
    check(all(cols %in% names(tab)), paste("Missing columns:",
      paste(setdiff(cols, names(tab)), collapse = ", ")))
  }
  cat("STAGE 3: PREPARATION ONLY -- no Dune requests\n")
  # A fingerprint detects accidental use of files from different runs.
  manifest_path <- path_for("pipeline_manifest.csv")
  manifest <- read_table(manifest_path)
  require_columns(manifest, c("file_name", "md5"))
  endings <- c("both_pools_stage2_final_candidate_decisions.csv",
               "suspicious_pool_stage1_candidate_swaps.csv",
               "control_pool_stage1_candidate_swaps.csv")
  input_paths <- vapply(endings, path_for, character(1))
  for (p in input_paths) {
    check(file.exists(p), paste("Missing input:", p))
    expected <- manifest$md5[manifest$file_name == basename(p)]
    check(length(expected) == 1L, paste("Missing/duplicate fingerprint:", p))
    check(tolower(unname(tools::md5sum(p))) == tolower(expected),
          paste("Input fingerprint differs from Stage-2 run:", p))
  }
  all_input_paths <- c(manifest_path, input_paths)
  initial_md5 <- unname(tools::md5sum(all_input_paths))
  decisions <- read_table(input_paths[1])
  members <- rbind(read_table(input_paths[2]), read_table(input_paths[3]))
  require_columns(decisions, c("sample", "candidate_group_id", "final_stage2_pass",
    "start_swap", "end_swap", "candidate_start_block", "candidate_end_block"))
  map_fields <- c("sample", "candidate_group_id", "position_in_group",
    "swap_sequence", "contract_address", "evt_tx_hash", "evt_tx_from",
    "evt_index", "evt_block_number")
  require_columns(members, map_fields)
  check(!anyDuplicated(decisions$candidate_group_id), "Duplicate candidate decisions")
  check(all(decisions$final_stage2_pass %in% c("TRUE", "FALSE")),
        "Stage-2 decisions are incomplete")
  passing <- decisions[decisions$final_stage2_pass == "TRUE", , drop = FALSE]
  # Preserve every membership before removing repetition from the request list.
  mapping <- members[members$candidate_group_id %in% passing$candidate_group_id,
                     map_fields, drop = FALSE]
  mapping <- mapping[order(mapping$sample, mapping$candidate_group_id,
                            as.integer(mapping$position_in_group)), , drop = FALSE]
  check(setequal(mapping$candidate_group_id, passing$candidate_group_id),
        "A passing candidate has no member swaps")
  # Ensure each candidate has its complete ordered sequence, not a partial join.
  groups <- split(mapping, mapping$candidate_group_id)
  for (i in seq_len(nrow(passing))) {
    d <- passing[i, ]; g <- groups[[d$candidate_group_id]]
    first <- as.integer(d$start_swap); last <- as.integer(d$end_swap)
    check(nrow(g) == last - first + 1L &&
          setequal(as.integer(g$position_in_group), seq_len(nrow(g))) &&
          setequal(as.integer(g$swap_sequence), seq.int(first, last)) &&
          all(g$sample == d$sample) &&
          min(as.integer(g$evt_block_number)) == as.integer(d$candidate_start_block) &&
          max(as.integer(g$evt_block_number)) == as.integer(d$candidate_end_block),
          paste("Incomplete/inconsistent candidate:", d$candidate_group_id))
  }
  check(all(grepl("^0x[0-9a-f]{64}$", mapping$evt_tx_hash)), "Invalid transaction hash")
  check(all(grepl("^[0-9]+$", mapping$evt_block_number)), "Invalid block number")
  block_numbers <- as.integer(mapping$evt_block_number)
  check(all(block_numbers >= 10000835L & block_numbers <= 15400000L),
        "A transaction is outside the frozen paper block interval")
  # A transaction must have one block, even if it belongs to several groups.
  transactions <- unique(data.frame(tx_hash = mapping$evt_tx_hash,
    expected_block = mapping$evt_block_number, stringsAsFactors = FALSE))
  check(!anyDuplicated(transactions$tx_hash), "A transaction maps to multiple blocks")
  transactions <- transactions[order(as.integer(transactions$expected_block),
                                     transactions$tx_hash), , drop = FALSE]
  summarize <- function(sample_name) {
    g <- if (sample_name == "both_pools") mapping else
      mapping[mapping$sample == sample_name, , drop = FALSE]
    data.frame(sample = sample_name,
      stage2_surviving_groups = length(unique(g$candidate_group_id)),
      candidate_member_rows = nrow(g),
      unique_candidate_events = nrow(unique(g[c("evt_tx_hash", "evt_index")])),
      unique_transactions = length(unique(g$evt_tx_hash)),
      unique_blocks = length(unique(g$evt_block_number)),
      stage3_status = "EVIDENCE_PREPARATION_ONLY", stringsAsFactors = FALSE)
  }
  summary <- do.call(rbind, lapply(c("suspicious_pool", "control_pool", "both_pools"), summarize))
  # These are checks against the completed run, never instructions to force counts.
  check(identical(as.integer(summary$stage2_surviving_groups), c(8280L,77L,8357L)),
        "Surviving group totals differ from the reviewed Stage-2 run")
  check(identical(as.integer(summary$unique_transactions), c(5538L,221L,5759L)),
        "Transaction totals differ from the reviewed Stage-2 run")
  cat("Local checks passed. Next: write the evidence requests.\n")
  # SQL is a written request for Dune data. Here we save it without sending it.
  values <- paste0("    (", transactions$tx_hash, ", ", transactions$expected_block, ")")
  values <- paste(values, collapse = ",\n")
  # 01 transaction coverage: saved request; no network execution.
  sql_01_transaction_coverage <- paste0(
    "-- AMM replication: ECO-USDT additional pools, Stage 3 evidence preparation.\n",
    "-- Both pools use the same query and study block bounds. No classifications here.\n",
    "-- Preserve the complete result; do not add LIMIT. Record the execution ID.\n",
    "WITH requested(tx_hash, expected_block) AS (\n",
    "  VALUES\n",
    "__VALUES__\n",
    "), requested_blocks AS (\n",
    "  SELECT DISTINCT expected_block AS block_number FROM requested\n",
    ")\n",
    "-- LEFT JOIN retains missing transactions so missing evidence is visible.\n",
    "SELECT concat('0x', to_hex(r.tx_hash)) AS requested_tx_hash,\n",
    "       r.expected_block, t.block_number, t.block_time,\n",
    "       t.\"index\" AS transaction_index, t.success,\n",
    "       concat('0x', to_hex(t.\"from\")) AS tx_from,\n",
    "       concat('0x', to_hex(t.\"to\")) AS tx_to,\n",
    "       t.hash IS NOT NULL AS transaction_found\n",
    "FROM requested r LEFT JOIN ethereum.transactions t\n",
    "  ON t.hash = r.tx_hash AND t.block_number = r.expected_block\n",
    " AND t.block_number BETWEEN 10000835 AND 15400000\n",
    " AND t.block_time >= TIMESTAMP '2020-05-04 00:00:00'\n",
    " AND t.block_time < TIMESTAMP '2022-08-25 00:00:00'\n",
    "ORDER BY r.expected_block, r.tx_hash\n"
  )
  # 02 raw logs and sandwich context: saved request; no network execution.
  sql_02_raw_logs_and_sandwich_context <- paste0(
    "-- AMM replication: ECO-USDT additional pools, Stage 3 evidence preparation.\n",
    "-- Both pools use the same query and study block bounds. No classifications here.\n",
    "-- Preserve the complete result; do not add LIMIT. Record the execution ID.\n",
    "WITH requested(tx_hash, expected_block) AS (\n",
    "  VALUES\n",
    "__VALUES__\n",
    "), requested_blocks AS (\n",
    "  SELECT DISTINCT expected_block AS block_number FROM requested\n",
    ")\n",
    ", block_logs AS (\n",
    "  SELECT l.* FROM ethereum.logs l\n",
    "  JOIN requested_blocks b ON b.block_number = l.block_number\n",
    "  WHERE l.block_number BETWEEN 10000835 AND 15400000\n",
    "    AND l.block_time >= TIMESTAMP '2020-05-04 00:00:00'\n",
    "    AND l.block_time < TIMESTAMP '2022-08-25 00:00:00'\n",
    "), candidate_logs AS (\n",
    "  SELECT l.* FROM block_logs l\n",
    "  JOIN requested r ON r.tx_hash = l.tx_hash AND r.expected_block = l.block_number\n",
    ")\n",
    ", touched_pools AS (\n",
    "  -- Also includes pools other than the original ECO/control pool.\n",
    "  SELECT DISTINCT l.block_number, l.contract_address FROM candidate_logs l\n",
    "  WHERE l.topic0 IN (keccak(to_utf8('Swap(address,uint256,uint256,uint256,uint256,address)')), keccak(to_utf8('Swap(address,address,int256,int256,uint160,uint128,int24)')))\n",
    "), retained_logs AS (\n",
    "  -- Candidate logs include Transfer, Sync and unknown events as well as Swap.\n",
    "  SELECT l.*, TRUE AS is_candidate_transaction FROM candidate_logs l\n",
    "  UNION ALL\n",
    "  -- Keep intervening trades even when their transactions were never candidates.\n",
    "  SELECT l.*, FALSE AS is_candidate_transaction FROM block_logs l\n",
    "  JOIN touched_pools p ON p.block_number = l.block_number\n",
    "                      AND p.contract_address = l.contract_address\n",
    "  WHERE l.topic0 IN (keccak(to_utf8('Swap(address,uint256,uint256,uint256,uint256,address)')), keccak(to_utf8('Swap(address,address,int256,int256,uint160,uint128,int24)')))\n",
    "    AND NOT EXISTS (SELECT 1 FROM requested r WHERE r.tx_hash = l.tx_hash)\n",
    ")\n",
    "SELECT block_number, block_time, tx_index AS transaction_index,\n",
    "       \"index\" AS log_index, concat('0x',to_hex(tx_hash)) AS tx_hash,\n",
    "       concat('0x',to_hex(contract_address)) AS contract_address,\n",
    "       concat('0x',to_hex(tx_from)) AS tx_from,\n",
    "       concat('0x',to_hex(tx_to)) AS tx_to,\n",
    "       concat('0x',to_hex(topic0)) AS topic0,\n",
    "       concat('0x',to_hex(topic1)) AS topic1,\n",
    "       concat('0x',to_hex(topic2)) AS topic2,\n",
    "       concat('0x',to_hex(topic3)) AS topic3,\n",
    "       concat('0x',to_hex(data)) AS data_hex, is_candidate_transaction\n",
    "FROM retained_logs ORDER BY block_number, transaction_index, log_index\n",
    "-- Hex preserves exact 256-bit values. Matching a Swap signature alone does not\n",
    "-- prove the emitting contract is an authentic Uniswap pool.\n"
  )
  # 03 decoded dex activity: saved request; no network execution.
  sql_03_decoded_dex_activity <- paste0(
    "-- AMM replication: ECO-USDT additional pools, Stage 3 evidence preparation.\n",
    "-- Both pools use the same query and study block bounds. No classifications here.\n",
    "-- Preserve the complete result; do not add LIMIT. Record the execution ID.\n",
    "WITH requested(tx_hash, expected_block) AS (\n",
    "  VALUES\n",
    "__VALUES__\n",
    "), requested_blocks AS (\n",
    "  SELECT DISTINCT expected_block AS block_number FROM requested\n",
    ")\n",
    ", block_trades AS (\n",
    " SELECT d.* FROM dex.trades d\n",
    " JOIN requested_blocks b ON b.block_number = d.block_number\n",
    " WHERE d.blockchain = 'ethereum'\n",
    "   AND d.block_number BETWEEN 10000835 AND 15400000\n",
    "   AND d.block_month BETWEEN DATE '2020-05-01' AND DATE '2022-08-01'\n",
    "   AND d.block_time >= TIMESTAMP '2020-05-04 00:00:00'\n",
    "   AND d.block_time < TIMESTAMP '2022-08-25 00:00:00'\n",
    "), touched_pools AS (\n",
    " SELECT DISTINCT d.block_number, d.project_contract_address\n",
    " FROM block_trades d JOIN requested r ON r.tx_hash = d.tx_hash\n",
    ")\n",
    "SELECT d.project, d.version, d.block_number, d.block_time, d.evt_index,\n",
    "       concat('0x',to_hex(d.tx_hash)) AS tx_hash,\n",
    "       concat('0x',to_hex(d.project_contract_address)) AS project_contract_address,\n",
    "       concat('0x',to_hex(d.tx_from)) AS tx_from,\n",
    "       concat('0x',to_hex(d.tx_to)) AS tx_to,\n",
    "       concat('0x',to_hex(d.token_bought_address)) AS token_bought_address,\n",
    "       concat('0x',to_hex(d.token_sold_address)) AS token_sold_address,\n",
    "       CAST(d.token_bought_amount_raw AS VARCHAR) AS token_bought_amount_raw,\n",
    "       CAST(d.token_sold_amount_raw AS VARCHAR) AS token_sold_amount_raw,\n",
    "       r.tx_hash IS NOT NULL AS is_candidate_transaction\n",
    "FROM block_trades d LEFT JOIN requested r ON r.tx_hash = d.tx_hash\n",
    "WHERE r.tx_hash IS NOT NULL OR EXISTS (\n",
    " SELECT 1 FROM touched_pools p WHERE p.block_number = d.block_number\n",
    " AND p.project_contract_address = d.project_contract_address\n",
    ")\n",
    "ORDER BY d.block_number, d.tx_hash, d.evt_index, d.project, d.version\n",
    "-- Decoded records supplement raw logs. Missing decoding does not prove no swap.\n",
    "-- Preserve raw amounts as text when importing the result into R.\n"
  )
  # 04 address identity evidence: saved request; no network execution.
  sql_04_address_identity_evidence <- paste0(
    "-- AMM replication: ECO-USDT additional pools, Stage 3 evidence preparation.\n",
    "-- Both pools use the same query and study block bounds. No classifications here.\n",
    "-- Preserve the complete result; do not add LIMIT. Record the execution ID.\n",
    "WITH requested(tx_hash, expected_block) AS (\n",
    "  VALUES\n",
    "__VALUES__\n",
    "), requested_blocks AS (\n",
    "  SELECT DISTINCT expected_block AS block_number FROM requested\n",
    ")\n",
    ", block_logs AS (\n",
    "  SELECT l.* FROM ethereum.logs l\n",
    "  JOIN requested_blocks b ON b.block_number = l.block_number\n",
    "  WHERE l.block_number BETWEEN 10000835 AND 15400000\n",
    "    AND l.block_time >= TIMESTAMP '2020-05-04 00:00:00'\n",
    "    AND l.block_time < TIMESTAMP '2022-08-25 00:00:00'\n",
    "), candidate_logs AS (\n",
    "  SELECT l.* FROM block_logs l\n",
    "  JOIN requested r ON r.tx_hash = l.tx_hash AND r.expected_block = l.block_number\n",
    ")\n",
    ", addresses_to_check AS (\n",
    " -- Receiving addresses of standard ERC20 transfers, as required by paper 7.3.\n",
    " SELECT DISTINCT varbinary_substring(topic2,13,20) AS address,\n",
    "                 'ERC20_TRANSFER_RECIPIENT' AS address_role\n",
    " FROM candidate_logs\n",
    " WHERE topic0 = keccak(to_utf8('Transfer(address,address,uint256)'))\n",
    "   AND varbinary_length(topic1) = 32 AND varbinary_length(topic2) = 32\n",
    "   AND topic3 IS NULL AND varbinary_length(data) = 32\n",
    " UNION\n",
    " -- Keep emitting-contract labels too, for protocol/pool identity checks.\n",
    " SELECT DISTINCT contract_address AS address, 'EVENT_EMITTER' AS address_role\n",
    " FROM candidate_logs\n",
    ")\n",
    "SELECT concat('0x',to_hex(a.address)) AS address, a.address_role,\n",
    "       l.name, l.category, l.label_type, l.source, l.model_name,\n",
    "       l.address IS NOT NULL AS label_found\n",
    "FROM addresses_to_check a LEFT JOIN labels.addresses l\n",
    " ON l.blockchain = 'ethereum' AND l.address = a.address\n",
    "ORDER BY a.address, a.address_role, l.category, l.name, l.source, l.model_name\n",
    "-- This is evidence retrieval, not an automatic exclusion list.\n",
    "-- Generic contract/public-service labels do not establish an exchange function.\n",
    "-- An unidentified address is retained, not assumed private. Labels are a\n",
    "-- retrieval-time snapshot; they do not by themselves prove historical function.\n"
  )

  # Write a separate Stage-3 manifest; keep the completed Stage-2 manifest intact.
  written <- character()
  save_table <- function(tab, ending) {
    p <- path_for(ending)
    write.csv(tab, p, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    written <<- c(written, p)
    cat("Saved:", p, "\n")
  }
  save_table(mapping, "both_pools_stage3_candidate_transaction_map.csv")
  save_table(transactions, "both_pools_stage3_unique_transactions.csv")
  save_table(summary, "both_pools_stage3_preparation_summary.csv")
  writeLines(sub("__VALUES__", values, sql_01_transaction_coverage, fixed = TRUE),
    path_for("both_pools_stage3_01_transaction_coverage.sql"), useBytes = TRUE)
  written <- c(written, path_for("both_pools_stage3_01_transaction_coverage.sql"))
  writeLines(sub("__VALUES__", values, sql_02_raw_logs_and_sandwich_context, fixed = TRUE),
    path_for("both_pools_stage3_02_raw_logs_and_sandwich_context.sql"), useBytes = TRUE)
  written <- c(written, path_for("both_pools_stage3_02_raw_logs_and_sandwich_context.sql"))
  writeLines(sub("__VALUES__", values, sql_03_decoded_dex_activity, fixed = TRUE),
    path_for("both_pools_stage3_03_decoded_dex_activity.sql"), useBytes = TRUE)
  written <- c(written, path_for("both_pools_stage3_03_decoded_dex_activity.sql"))
  writeLines(sub("__VALUES__", values, sql_04_address_identity_evidence, fixed = TRUE),
    path_for("both_pools_stage3_04_address_identity_evidence.sql"), useBytes = TRUE)
  written <- c(written, path_for("both_pools_stage3_04_address_identity_evidence.sql"))

  check(identical(initial_md5, unname(tools::md5sum(all_input_paths))),
        "An input file changed during preparation")
  file_list <- c(all_input_paths, written)
  preparation_manifest <- data.frame(
    artifact_type = c(rep("input", length(all_input_paths)), rep("output", length(written))),
    file_name = basename(file_list), md5 = unname(tools::md5sum(file_list)),
    pipeline_stage = "STAGE3_PREPARATION", stringsAsFactors = FALSE)
  write.csv(preparation_manifest, path_for("both_pools_stage3_preparation_manifest.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  print(summary, row.names = FALSE)
  cat("Preparation complete. No Dune query was submitted.\n")
  cat("Next step: run query 01 (transaction coverage) and review its full result.\n")
  cat("Then retrieve queries 02-04 before implementing Stage-3 decisions.\n")
})
