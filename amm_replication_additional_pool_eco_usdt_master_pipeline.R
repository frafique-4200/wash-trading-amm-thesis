# ============================================================================
# AMM REPLICATION -- ECO-USDT ADDITIONAL POOL -- MASTER PIPELINE
# VERSION 3.1 CORRECTED: STAGE 1 AND COMPLETE STAGE-2 ENTITY RECOGNITION
# ============================================================================
#
# PURPOSE
# -------
# This is the replication counterpart of the scripts named:
#
#   amm_adaptation_additional_pool_eco_usdt_*
#
# The adaptation scripts apply the internship-report/WTEYE method to AMM data.
# This script applies the reduced AMM-paper method to the additional pools.
# It preserves the earlier local calculations and corrects the inconsistent
# public-service filtering used in the original small-pool experiments.
#
# The final research comparison will use:
#
#   WTEYE adaptation result under REQUIRE_CONSECUTIVE
#                         versus
#   final reduced-AMM replication result after Stages 1, 2, and 3.
#
# IMPORTANT VERSION-3 BOUNDARY
# ----------------------------
# This version completes Stage 1 for both additional pools. It then performs
# the local parts of Stage 2:
#
# - one-initiator groups receive the same-sender automatic pass;
# - multi-initiator groups receive the sender-level cross-combination test;
# - unresolved groups are converted into candidate/address-pair evidence
#   requests for Dune ETH-relationship analysis.
#
# When explicitly enabled, Version 3 submits one auditable SQL statement to the
# Dune API. The SQL tests all unique initiator pairs for direct or two-hop ETH
# transfer relationships in the paper-period Ethereum transaction graph. It
# excludes graph edges using one documented binary service rule for both pools.
# This corrects inconsistent earlier rules. Unidentified addresses are retained.
# Service decisions and their sources are saved for audit; no third status is used.
# It then maps pair evidence back to each candidate and applies Algorithm 2's
# connected-graph rule. A candidate does not need every possible pair to be
# connected; it needs one connected graph spanning all of its initiators.
#
# The API is OFF by default to prevent accidental use of Dune credits. To run
# it, set RUN_DUNE_ENTITY_API=true and DUNE_API_KEY in the R environment. The
# key is read from the environment and is never written to an output file.
#
#In the R console inside VS Code, run this, replacing the placeholder 
#with your actual key:
#Sys.setenv(DUNE_API_KEY = "PASTE_YOUR_KEY_HERE")
#
#Check that it is set without displaying it:
#nzchar(Sys.getenv("DUNE_API_KEY"))
#
#If this returns TRUE, rerun:
#source("D:/Downloads/Bachelor Thesis/Scripts/amm_replication_additional_pool_eco_usdt_master_pipeline.R")
#
# Version 3 completes Stage 2 only. Stage 3 MEV elimination is still required
# before any candidate can be reported as a final reduced-replication wash trade.
#
# PARAMETERS PRESERVED FROM THE AMM-PAPER REPLICATION
# ---------------------------------------------------
# - adjacent sliding windows only;
# - group lengths from 2 through 10 swaps;
# - maximum block span tau = 200 blocks;
# - position-restoration threshold rho = 5%;
# - either token may satisfy the restoration condition;
# - the comparison operator remains <=, matching the earlier replication code.
#
# INPUTS
# ------
# Data/raw/12A_eco_usdt_suspicious_pool_raw_swaps.csv
# Data/raw/12B_eco_usdt_matched_control_pool_raw_swaps.csv
# Data/raw/13_tokens_and_decimals_for_additional_pools.csv
#
# OUTPUT NAMING RULE
# ------------------
# Every generated file begins with:
#
#   amm_replication_additional_pool_eco_usdt_output_
#
# This separates AMM-paper replication outputs from WTEYE-adaptation outputs.
#
# OUTPUT FILES CREATED BY THIS VERSION
# ------------------------------------
#
# STAGE 1 -- SUSPICIOUS POOL
#
# 1. amm_replication_additional_pool_eco_usdt_output_suspicious_pool_stage1_all_windows.csv
#    One row for every adjacent 2--10-swap window examined, whether it passed
#    or failed the AMM coarse filter.
#
# 2. amm_replication_additional_pool_eco_usdt_output_suspicious_pool_stage1_candidates.csv
#    One row for every suspicious-pool window that passed the coarse filter.
#
# 3. amm_replication_additional_pool_eco_usdt_output_suspicious_pool_stage1_candidate_swaps.csv
#    The individual swaps belonging to each retained suspicious-pool candidate.
#
# STAGE 1 -- CONTROL POOL
#
# 4. amm_replication_additional_pool_eco_usdt_output_control_pool_stage1_all_windows.csv
#    One row for every adjacent control-pool window examined.
#
# 5. amm_replication_additional_pool_eco_usdt_output_control_pool_stage1_candidates.csv
#    One row for every control-pool window that passed the coarse filter.
#
# 6. amm_replication_additional_pool_eco_usdt_output_control_pool_stage1_candidate_swaps.csv
#    The individual swaps belonging to each retained control-pool candidate.
#
# STAGE 1 -- COMBINED SUMMARY
#
# 7. amm_replication_additional_pool_eco_usdt_output_both_pools_stage1_summary.csv
#    Pool-level Stage-1 counts, parameters, and classification limitations.
#
# LOCAL STAGE 2 -- CALCULATIONS AND DECISIONS
#
# 8. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_sender_position_restoration.csv
#    One row per candidate/initiator, including both token-restoration ratios
#    and the resulting sender-restoration decision.
#
# 9. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_preliminary_candidate_decisions.csv
#    One row per candidate showing same-sender, cross-combination, and unresolved
#    entity-evidence status. These are not final Stage-2 classifications.
#
# 10. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_entity_candidate_pair_map.csv
#     One row per unresolved candidate/address-pair association. This table maps
#     reusable Dune evidence back to every candidate that needs it.
#
# DUNE/API PREPARATION
#
# 11. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_entity_recognition_api_input.csv
#     One row per unique unordered initiator pair that requires direct or
#     two-hop ETH relationship evidence from Dune.
#
# 12. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_pre_api_summary.csv
#     Per-pool and combined counts after local Stage-2 rules but before Dune.
#
# REPRODUCIBILITY
#
# 13. amm_replication_additional_pool_eco_usdt_output_pipeline_manifest.csv
#     Names and MD5 fingerprints of every source and generated evidence file.
#
# DUNE ENTITY RECOGNITION AND FINAL STAGE 2
#
# 14. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_dune_entity_query.sql
#     Exact SQL submitted to Dune. It is also written when API execution is off.
#
# Files 15--20 are created only when RUN_DUNE_ENTITY_API=true.
#
# 15. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_dune_execution_log.csv
#     Execution identifier, state, timing, cost metadata, and result-row count.
#
# 16. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_dune_raw_result.json
#     Unmodified paginated result responses returned by Dune, excluding the key.
#
# 17. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_pair_evidence.csv
#     One normalized row per requested address pair and its shortest ETH path.
#
# 18. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_candidate_entity_connectivity.csv
#     One row per externally tested candidate, including graph connectivity.
#
# 19. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_final_candidate_decisions.csv
#     One row per Stage-1 candidate with its final Stage-2 pass/fail decision.
#
# 20. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_post_api_summary.csv
#     Pool-level and combined counts after external entity recognition.
#
# HOW TO RUN
# ----------
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_replication_additional_pool_eco_usdt_master_pipeline.R")
#
# PLAIN-LANGUAGE MAP OF THIS VERSION
# ----------------------------------
# 1. Tell R where the project files are and lock the replication parameters.
# 2. Define small helper instructions for reading, checking, and saving data.
# 3. Confirm that all required files and metadata are present and usable.
# 4. Examine every adjacent group of 2--10 swaps in each pool.
# 5. Retain a group when it occurs within 200 blocks and approximately restores
#    at least one pool token to within the fixed 5% threshold.
# 6. Test each candidate initiator's token-position restoration independently.
# 7. Separate automatic, cross-combination, and unresolved entity cases.
# 8. Convert unresolved cases into unique address pairs for Dune checks.
# 9. Generate and save the exact entity-recognition SQL.
# 10. If explicitly enabled, submit the SQL, wait for completion, and download
#     every result page while preserving the raw response.
# 11. Reconstruct each candidate's initiator graph and test connectivity.
# 12. Save final Stage-2 decisions, summaries, and file fingerprints.
# 13. Stop before Stage-3 MEV analysis.
#
# ADDITIONAL OUTPUTS IN THE CORRECTED VERSION (same output prefix)
# 21. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_reviewed_service_register.csv
#     Address-specific reviewed exclusions, reasons and supporting sources.
# 22. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_service_policy.csv
#     Common rule, categories, graph interval and limitation statement.
# 23. amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_service_exclusion_evidence.csv
#     API mode only: the actual service evidence for addresses touching retrieved
#     transfers. Multiple evidence rows for one excluded address are preserved.
#
# BASIC R NOTATION USED BELOW
# ---------------------------
# - A line beginning with # is an explanation; R does not execute it.
# - <- assigns the value on the right to the name on the left.
# - c(...) combines values into a vector, similar to a short list.
# - list(...) groups related objects that may have different structures.
# - function(...) defines reusable instructions but does not run them yet.
# - if (...) checks a condition; stop(...) ends the run if the condition fails.
# - for (...) repeats the enclosed instructions for a sequence of values.
# - table$column selects one named column from a table.
# - table[rows, columns] selects specified rows and columns.
# - [[...]] extracts one exact element from a vector or list.
# - TRUE and FALSE are logical yes/no values.
# - data.frame(...) creates a rectangular table that can be saved as a CSV.
#
# The outer local({...}) keeps temporary names created here from overwriting
# similarly named objects that may already exist in the user's R session.

local({
  # ========================================================================
  # 1. FIXED PROJECT PATHS AND REPLICATION PARAMETERS
  # ========================================================================
  # The base directory is the common parent of the Data and Scripts folders.
  # file.path() safely builds the remaining Windows paths from folder names.
  base_dir <- "D:/Downloads/Bachelor Thesis"
  raw_dir <- file.path(base_dir, "Data", "raw")
  cleaned_dir <- file.path(base_dir, "Data", "cleaned")

  # These values reproduce the previously implemented AMM-paper coarse filter.
  # They are written once here so that both pools receive exactly the same
  # treatment and no value can be adjusted after seeing a pool's results.
  tau <- 200L
  rho <- 0.05
  minimum_group_size <- 2L
  maximum_group_size <- 10L

  # The paper-period limits already used for the additional-pool extraction.
  # Any swap outside this historical interval causes the script to stop.
  paper_start_block <- 10000835L
  paper_end_block <- 15400000L

  # These are labels for the two datasets, not conclusions produced by R.
  # "Suspicious" reflects the source selection; "control" reflects the
  # reconstructed non-abnormal comparison-pool selection.
  samples <- c("suspicious_pool", "control_pool")

  # Associate each sample label with its raw Dune export filename.
  input_files <- c(
    suspicious_pool = "12A_eco_usdt_suspicious_pool_raw_swaps.csv",
    control_pool = "12B_eco_usdt_matched_control_pool_raw_swaps.csv"
  )

  metadata_file <- "13_tokens_and_decimals_for_additional_pools.csv"

  # Every output uses one prefix so it cannot be confused with an adaptation
  # output or with results for the later wCELO comparison.
  output_prefix <- "amm_replication_additional_pool_eco_usdt_output_"

  # Predeclare every Stage-1 filename. Each pool receives the same three kinds
  # of evidence: all tested windows, retained groups, and their member swaps.
  stage1_output_files <- list(
    suspicious_pool = c(
      all_windows = paste0(output_prefix, "suspicious_pool_stage1_all_windows.csv"),
      candidates = paste0(output_prefix, "suspicious_pool_stage1_candidates.csv"),
      candidate_swaps = paste0(output_prefix, "suspicious_pool_stage1_candidate_swaps.csv")
    ),
    control_pool = c(
      all_windows = paste0(output_prefix, "control_pool_stage1_all_windows.csv"),
      candidates = paste0(output_prefix, "control_pool_stage1_candidates.csv"),
      candidate_swaps = paste0(output_prefix, "control_pool_stage1_candidate_swaps.csv")
    )
  )

  summary_file <- paste0(output_prefix, "both_pools_stage1_summary.csv")
  stage2_sender_file <- paste0(
    output_prefix,
    "both_pools_stage2_sender_position_restoration.csv"
  )
  stage2_decision_file <- paste0(
    output_prefix,
    "both_pools_stage2_preliminary_candidate_decisions.csv"
  )
  stage2_pair_map_file <- paste0(
    output_prefix,
    "both_pools_stage2_entity_candidate_pair_map.csv"
  )
  stage2_api_input_file <- paste0(
    output_prefix,
    "both_pools_stage2_entity_recognition_api_input.csv"
  )
  stage2_summary_file <- paste0(
    output_prefix,
    "both_pools_stage2_pre_api_summary.csv"
  )
  stage2_dune_sql_file <- paste0(
    output_prefix,
    "both_pools_stage2_dune_entity_query.sql"
  )
  stage2_dune_log_file <- paste0(
    output_prefix,
    "both_pools_stage2_dune_execution_log.csv"
  )
  stage2_dune_raw_file <- paste0(
    output_prefix,
    "both_pools_stage2_dune_raw_result.json"
  )
  stage2_pair_evidence_file <- paste0(
    output_prefix,
    "both_pools_stage2_pair_evidence.csv"
  )
  stage2_connectivity_file <- paste0(
    output_prefix,
    "both_pools_stage2_candidate_entity_connectivity.csv"
  )
  stage2_final_decision_file <- paste0(
    output_prefix,
    "both_pools_stage2_final_candidate_decisions.csv"
  )
  stage2_post_api_summary_file <- paste0(
    output_prefix,
    "both_pools_stage2_post_api_summary.csv"
  )
  manifest_file <- paste0(output_prefix, "pipeline_manifest.csv")

  # External execution is deliberately opt-in because Dune charges credits.
  # Run these two commands before source(...) when the external stage is wanted:
  #
  #   Sys.setenv(RUN_DUNE_ENTITY_API = "true")
  #   Sys.setenv(DUNE_API_KEY = "your-key")
  #
  # Prefer storing DUNE_API_KEY in the user's .Renviron file rather than typing
  # it into a shared script or console transcript.
  run_dune_entity_api <- identical(
    tolower(trimws(Sys.getenv("RUN_DUNE_ENTITY_API", unset = "false"))),
    "true"
  )

  # These settings govern API transport only; they do not change rho, tau, or
  # the candidate classification rules.
  dune_api_base <- "https://api.dune.com/api/v1"
  dune_performance <- "medium"
  dune_poll_seconds <- 5
  dune_timeout_minutes <- 90
  dune_result_page_size <- 10000L

  # CORRECTION V3.1 (11 September 2026)
  # The earlier original-pool scripts used different category and timing rules.
  # This revision uses ONE binary rule and ONE block interval for both pools.
  # Identified public service -> remove its graph edges. Not identified -> retain.
  # Retention is NOT proof of private ownership. Unrecognised services can create
  # misleading links between independent traders. This is a reduced replication,
  # not a reconstruction of the paper's full double-verified service list.
  #
  # Evidence route 1: an Ethereum Dune identifier with a named service, an exact
  # service category below, and a recorded source/model. Generic categories such
  # as contracts, social or institution are NOT sufficient for automatic removal.
  # Evidence route 2: the address-specific reviewed register below, supported by
  # named functional labels and sources. A contract name is reviewed in context;
  # the script does not guess from arbitrary keywords or remove every contract.
  # Dune evidence alone is disclosed as such; it is not Etherscan corroboration.
  #
  # Labels are a retrospective attribution snapshot, not historical role proofs.
  # Their creation dates are label-maintenance dates, not service start dates.
  # Both pools use paper_start_block..paper_end_block, never a per-swap cutoff.
  # Next step: save this rule and the register, then inspect the generated SQL.
  service_policy_version <- "V3.1_BINARY_IDENTIFIED_SERVICES"
  service_disclaimer <- paste(
    "Only addresses supported by sufficient evidence are identified as public",
    "services and excluded from entity recognition. Unconfirmed addresses are",
    "retained. Retention does not establish private ownership; unidentified",
    "services can create misleading connections between unrelated traders.",
    "The same rule and historical interval apply to both pools."
  )
  excluded_service_categories <- c(
    "cex", "dex", "lending", "aggregator", "bridge", "mixer"
  )
  # These 22 entries are evidence-backed decisions, not the old 26-entry list.
  # ENS, SKALE, the two OpenSea contracts and the three disputed token contracts
  # have no manual exclusion here. They remain unless qualifying Dune evidence
  # identifies them under the same rule as every other address.
  # WETH is excluded only from ETH relationship paths; its swaps stay in Stage 1.
  reviewed_services <- data.frame(
    address = c(
      "0x0fabaf48bbf864a3947bdd0ba9d764791a60467a",
      "0x11111112542d85b3ef69ae05771c2dccff4faa26",
      "0x111111125434b319222cdbf8c261674adb56f3ae",
      "0x1111111254fb6c44bac0bed2854e76f90643097d",
      "0x3980c9ed79d2c191a89e02fa3529c60ed6e9c04b",
      "0x3e66b66fd1d0b02fda6c811da9e0547970db2f21",
      "0x4dbd4fc535ac27206064b68ffcf827b0a60bab3f",
      "0x52ec2f3d7c5977a8e558c8d9c6000b615098e8fc",
      "0x564286362092d8e7936f0549571a803b203aaced",
      "0x68b3465833fb72a70ecdf485e0e4c7bd8665fc45",
      "0x75e89d5979e4f6fba9f97c104c2f0afb3f1dcb88",
      "0x7a250d5630b4cf539739df2c5dacb4c659f2488d",
      "0x986a2fca9eda0e06fbf7839b89bfc006ee2a23dd",
      "0xa0c68c638235ee32657e8f720a23cec1bfc77c77",
      "0xabea9132b05a70803a4e85094fd0e1800777fbef",
      "0xba12222222228d8ba445958a75a0704d566bf2c8",
      "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2",
      "0xc098b2a3aa256d2140208c3de6543aaef5cd3a94",
      "0xcd4ec7b66fbc029c116ba9ffb3e59351c20b5b06",
      "0xd9e1ce17f2641f24ae83637ab66a2cca9c378b9f",
      "0xdef1c0ded9bec7f1a1670819833240f027b25eff",
      "0xe592427a0aece92de3edee1f18e0157c05861564"
    ),
    service_name = c(
      "Ribbon v1 RibbonThetaVault — public DeFi infrastructure",
      "1inch AggregationRouterV3 — DEX aggregator",
      "1inch AggregationRouterV2 — DEX aggregator",
      "1inch AggregationRouterV4 — DEX aggregator",
      "Metis L1StandardBridge — bridge infrastructure",
      "Balancer V2 ExchangeProxy — DEX infrastructure",
      "Arbitrum Inbox / gateway infrastructure",
      "Teleportr / Optimism deposit infrastructure",
      "Binance — centralized exchange",
      "Uniswap V3 SwapRouter02 — DEX infrastructure",
      "MEXC — centralized exchange",
      "Uniswap V2 Router02 — DEX infrastructure",
      "AscendEX — centralized exchange",
      "Polygon RootChainManager — bridge/service infrastructure",
      "zkSync Lite bridge / service infrastructure",
      "Balancer V2 Vault — DEX infrastructure",
      "WETH9 — shared public token/wrapping infrastructure",
      "FTX — centralized exchange",
      "Rarible ExchangeV1 — public marketplace infrastructure",
      "SushiSwap Router02 — DEX infrastructure",
      "0x ExchangeProxy — DEX / aggregator infrastructure",
      "Uniswap V3 SwapRouter — DEX infrastructure"
    ),
    evidence_source = c(
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts; https://docs.ribbon.finance/developers/contract-addresses",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; bridges_ethereum; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; bridges_ethereum; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; bridges_ethereum; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; cex_ethereum",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; cex_ethereum",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; cex_ethereum",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; bridges_ethereum; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts; ens",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts; ens; https://developers.uniswap.org/docs/protocols/v3/deployments/v3-ethereum-deployments; https://raw.githubusercontent.com/gnosis/canonical-weth/master/contracts/WETH9.sol",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; cex_ethereum",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts; https://etherscan.io/address/0xcd4ec7b66fbc029c116ba9ffb3e59351c20b5b06",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts; ens",
      "Dune labels.addresses; control_pool_stage2_additional_labels(4).csv; contracts"
    ),
    evidence_detail = c(
      "Ribbon_v1: RibbonThetaVault",
      "Oneinch: AggregationRouterV3",
      "Oneinch: AggregationRouterV2",
      "Oneinch: AggregationRouterV4",
      "Metis Andromeda: Bridge | Metis: L1StandardBridge",
      "Balancer_v2: ExchangeProxy",
      "Arbitrum: Delayed Inbox | Arbitrum: Inbox | Arbitrum: L1ERC20Gateway",
      "Optimism: Teleportr v2 | Teleportr: TeleportrDeposit",
      "Binance 3",
      "Uniswap_v3: SwapRouter02",
      "MEXC 1",
      "Uniswap_v2: Router02 | Uniswap_v2: UniswapV2Factory",
      "AscendEX 3",
      "Polygon: RootChainManager",
      "Zksync: ZkSync | Zksync_trade_volume: zksync_trade_volume | zkSync Lite Bridge",
      "Balancer_v2: Vault | balancervault.eth",
      "Myname: WETH9 | Zeroex: WETH9 | wraps.eth",
      "FTX 2",
      "Rarible: ExchangeV1",
      "Sushiswap: UniswapV2Router02",
      "Zeroex: ExchangeProxy | zeroex.eth",
      "Uniswap_v3: SwapRouter"
    ),
    stringsAsFactors = FALSE
  )
  reviewed_services$decision <- "IDENTIFIED_PUBLIC_SERVICE"
  reviewed_services$policy_version <- service_policy_version
  service_registry_file <- paste0(output_prefix, "both_pools_stage2_reviewed_service_register.csv")
  service_audit_file <- paste0(output_prefix, "both_pools_stage2_service_exclusion_evidence.csv")
  service_policy_file <- paste0(output_prefix, "both_pools_stage2_service_policy.csv")

  # cat() prints a readable run header in the R console. It does not change the
  # data or influence candidate selection.
  cat("AMM REPLICATION -- ECO-USDT ADDITIONAL POOL -- MASTER PIPELINE\n")
  cat("Version 3.1 corrected target: Stage 1 plus complete Stage-2 entity recognition\n\n")
  cat("Raw-data directory:", raw_dir, "\n")
  cat("Cleaned-data directory:", cleaned_dir, "\n")
  cat(
    "Fixed replication settings: rho =", rho,
    "; tau =", tau,
    "; group sizes =", minimum_group_size, "to", maximum_group_size,
    "\n\n"
  )
  cat("Dune entity API enabled:", run_dune_entity_api, "\n\n")

  # ========================================================================
  # 2. SMALL REUSABLE VALIDATION AND WRITING FUNCTIONS
  # ========================================================================
  # This reader initially keeps every CSV column as text. Blockchain amounts
  # can be extremely large integers, so premature conversion could silently
  # change their digits. Individual fields are converted only when needed.
  read_text_csv <- function(path) {
    utils::read.csv(
      path,
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  }

  # Check that a table contains every column needed by the later calculations.
  # This prevents a renamed or incomplete Dune export from being misanalysed.
  require_columns <- function(table, required, label) {
    missing <- setdiff(required, names(table))
    if (length(missing)) {
      stop(
        paste(label, "is missing:", paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
  }

  # Convert fields such as block number, log index, and token decimals to
  # numbers, while rejecting missing, negative, fractional, or invalid values.
  as_whole_number <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    invalid <- !is.finite(numbers) | numbers < 0 | numbers != floor(numbers)
    if (any(invalid)) {
      stop(paste("Invalid whole number in", field), call. = FALSE)
    }
    numbers
  }

  # Convert token amounts for arithmetic and reject impossible negative values.
  as_nonnegative_number <- function(values, field) {
    numbers <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numbers) | numbers < 0)) {
      stop(paste("Invalid nonnegative number in", field), call. = FALSE)
    }
    numbers
  }

  # Remove repeated addresses and place the remaining addresses in one
  # semicolon-separated text field for an auditable candidate summary.
  join_unique <- function(values) {
    values <- unique(as.character(values))
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) "" else paste(values, collapse = ";")
  }

  # Save one R table as a CSV in the cleaned-data folder. Numeric values are
  # written with enough digits for a later R stage to reconstruct them.
  write_report <- function(table, filename) {
    path <- file.path(cleaned_dir, filename)
    table_for_disk <- table

    # Seventeen significant digits make numerical outputs reproducible when
    # they are read again by a later R stage.
    for (field in names(table_for_disk)) {
      if (is.double(table_for_disk[[field]])) {
        table_for_disk[[field]] <- sprintf("%.17g", table_for_disk[[field]])
      }
    }

    utils::write.csv(
      table_for_disk,
      path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
    cat("Saved:", path, "\n")
    path
  }

  # Calculate the AMM-replication position-restoration ratio for one token and
  # one candidate window. Smaller values mean the final pool-token position is
  # small relative to the total amount that moved through the pool.
  restoration_ratio <- function(delta_values) {
    # Numerator: absolute net position change of the pool over the window.
    net_change <- sum(delta_values)

    # Denominator: total absolute movement of that token over the window.
    gross_movement <- sum(abs(delta_values))

    # A zero-movement token cannot demonstrate restoration.
    if (!is.finite(gross_movement) || gross_movement == 0) {
      return(Inf)
    }

    abs(net_change) / gross_movement
  }

  # Create a correctly structured zero-row table when a pool has no candidates.
  # This lets later stages read the same columns regardless of the result count.
  empty_candidate_table <- function() {
    data.frame(
      sample = character(),
      candidate_id = integer(),
      candidate_group_id = character(),
      start_swap = integer(),
      end_swap = integer(),
      included_swap_indices = character(),
      group_size = integer(),
      start_block = numeric(),
      end_block = numeric(),
      block_span = numeric(),
      ratio_token0 = numeric(),
      ratio_token1 = numeric(),
      token0_restored = logical(),
      token1_restored = logical(),
      time_condition = logical(),
      restoration_condition = logical(),
      coarse_filter_pass = logical(),
      number_of_initiators = integer(),
      initiator_addresses = character(),
      start_tx_hash = character(),
      end_tx_hash = character(),
      stringsAsFactors = FALSE
    )
  }

  # Convert one validated Ethereum address to a DuneSQL varbinary literal.
  # For example, "0x12..." stays unquoted because Dune represents addresses as
  # binary values rather than ordinary character strings.
  dune_address_literal <- function(address) {
    address <- tolower(trimws(as.character(address)))
    if (length(address) != 1L ||
        !grepl("^0x[0-9a-f]{40}$", address)) {
      stop("Invalid Ethereum address while constructing Dune SQL.",
           call. = FALSE)
    }
    address
  }

  # Construct one self-contained DuneSQL statement for every unique pair.
  # A single query avoids repeatedly scanning the large Ethereum transaction
  # table. The VALUES section is generated only from validated pipeline output.
  build_entity_sql <- function(pair_table) {
    if (!nrow(pair_table)) {
      stop("Cannot build entity SQL because the pair table is empty.",
           call. = FALSE)
    }
    if (anyDuplicated(pair_table$pair_id)) {
      stop("Pair IDs must be unique before constructing Dune SQL.",
           call. = FALSE)
    }

    pair_values <- vapply(seq_len(nrow(pair_table)), function(row_number) {
      pair_id <- pair_table$pair_id[[row_number]]
      if (!grepl("^ECO_USDT_PAIR_[0-9]{5}$", pair_id)) {
        stop("Unexpected pair ID while constructing Dune SQL.",
             call. = FALSE)
      }
      paste0(
        "    ('", pair_id, "', ",
        dune_address_literal(pair_table$address_a[[row_number]]), ", ",
        dune_address_literal(pair_table$address_b[[row_number]]), ")"
      )
    }, character(1L))

    # Escape quotes in evidence text before embedding it as SQL data.
    sql_text <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
    reviewed_values <- vapply(seq_len(nrow(reviewed_services)), function(i) {
      paste0("    (", dune_address_literal(reviewed_services$address[i]), ", ",
             paste(sql_text(unlist(reviewed_services[i, c("service_name",
               "evidence_source", "evidence_detail")], use.names = FALSE)),
               collapse = ", "), ")")
    }, character(1L))

    service_types <- paste0(
      "'", excluded_service_categories, "'",
      collapse = ", "
    )

    paste0(
      "-- AMM replication: ECO-USDT Stage-2 entity recognition\n",
      "-- Generated by corrected Version 3.1: identified services excluded; others retained.\n",
      "-- A pair is related when its shortest path in the filtered undirected\n",
      "-- ETH-transfer graph contains one or two edges.\n\n",
      "WITH requested_pairs (pair_id, address_a, address_b) AS (\n",
      "  VALUES\n", paste(pair_values, collapse = ",\n"), "\n",
      "),\n",
      "requested_addresses AS (\n",
      "  SELECT address_a AS address FROM requested_pairs\n",
      "  UNION\n",
      "  SELECT address_b AS address FROM requested_pairs\n",
      "),\n",
      "reviewed_services(address, service_name, evidence_source, evidence_detail) AS (\n",
      "  VALUES\n", paste(reviewed_values, collapse = ",\n"), "\n),\n",
      "service_evidence AS (\n",
      "  SELECT DISTINCT address, name AS service_name, lower(trim(category)) AS service_category,\n",
      "    concat('labels.addresses; model=', coalesce(model_name, ''), '; source=', coalesce(source, '')) AS evidence_source,\n",
      "    concat('identifier=', name, '; label updated=', coalesce(cast(updated_at AS varchar), '')) AS evidence_detail\n",
      "  FROM labels.addresses\n",
      "  WHERE blockchain = 'ethereum'\n",
      "    AND lower(trim(label_type)) = 'identifier'\n",
      "    AND lower(trim(category)) IN (", service_types, ")\n",
      "    AND nullif(trim(name), '') IS NOT NULL\n",
      "    AND (nullif(trim(source), '') IS NOT NULL OR nullif(trim(model_name), '') IS NOT NULL)\n",
      "  UNION ALL\n",
      "  SELECT address, service_name, 'reviewed_function', evidence_source, evidence_detail FROM reviewed_services\n",
      "),\n",
      "service_addresses AS (SELECT DISTINCT address FROM service_evidence),\n",
      "raw_relevant_eth_transfers AS (\n",
      "  SELECT\n",
      "    t.block_number, t.hash AS tx_hash,\n",
      "    t.\"from\" AS address_from, t.\"to\" AS address_to\n",
      "  FROM ethereum.transactions AS t\n",
      "  WHERE t.block_number BETWEEN ", paper_start_block,
      " AND ", paper_end_block, "\n",
      "    AND t.success = true\n",
      "    AND t.value > 0\n",
      "    AND t.\"from\" <> t.\"to\"\n",
      "    AND t.\"to\" IS NOT NULL\n",
      "    AND (\n",
      "      t.\"from\" IN (SELECT address FROM requested_addresses)\n",
      "      OR t.\"to\" IN (SELECT address FROM requested_addresses)\n",
      "    )\n",
      "),\n",
      "relevant_eth_transfers AS (\n",
      "  SELECT t.* FROM raw_relevant_eth_transfers t\n",
      "  WHERE NOT EXISTS (\n",
      "    SELECT 1 FROM service_addresses s\n",
      "    WHERE s.address = t.address_from OR s.address = t.address_to\n",
      "  )\n",
      "),\n",
      "touched_service_evidence AS (\n",
      "  SELECT DISTINCT s.* FROM service_evidence s\n",
      "  WHERE s.address IN (SELECT address_from FROM raw_relevant_eth_transfers\n",
      "                      UNION SELECT address_to FROM raw_relevant_eth_transfers)\n",
      "),\n",
      "service_audit AS (\n",
      "  SELECT coalesce(json_format(cast(array_agg(map(\n",
      "    ARRAY['address','service_name','service_category','evidence_source','evidence_detail'],\n",
      "    ARRAY[lower(concat('0x',to_hex(address))),service_name,service_category,evidence_source,evidence_detail]\n",
      "  )) AS JSON)), '[]') AS evidence_json\n",
      "  FROM touched_service_evidence\n",
      "),\n",
      "oriented_neighbor_summaries AS (\n",
      "  SELECT\n",
      "    address_from AS endpoint, address_to AS neighbor,\n",
      "    count(*) AS transfer_count,\n",
      "    min(block_number) AS first_block,\n",
      "    max(block_number) AS last_block,\n",
      "    min_by(tx_hash, block_number) AS example_tx_hash\n",
      "  FROM relevant_eth_transfers\n",
      "  WHERE address_from IN (SELECT address FROM requested_addresses)\n",
      "  GROUP BY 1, 2\n",
      "  UNION ALL\n",
      "  SELECT\n",
      "    address_to AS endpoint, address_from AS neighbor,\n",
      "    count(*) AS transfer_count,\n",
      "    min(block_number) AS first_block,\n",
      "    max(block_number) AS last_block,\n",
      "    min_by(tx_hash, block_number) AS example_tx_hash\n",
      "  FROM relevant_eth_transfers\n",
      "  WHERE address_to IN (SELECT address FROM requested_addresses)\n",
      "  GROUP BY 1, 2\n",
      "),\n",
      "undirected_neighbors AS (\n",
      "  SELECT\n",
      "    endpoint, neighbor, sum(transfer_count) AS transfer_count,\n",
      "    min(first_block) AS first_block, max(last_block) AS last_block,\n",
      "    min_by(example_tx_hash, first_block) AS example_tx_hash\n",
      "  FROM oriented_neighbor_summaries\n",
      "  GROUP BY 1, 2\n",
      "),\n",
      "direct_evidence AS (\n",
      "  SELECT p.pair_id, n.transfer_count, n.first_block, n.last_block,\n",
      "         n.example_tx_hash\n",
      "  FROM requested_pairs AS p\n",
      "  JOIN undirected_neighbors AS n\n",
      "    ON n.endpoint = p.address_a AND n.neighbor = p.address_b\n",
      "),\n",
      "two_hop_paths AS (\n",
      "  SELECT\n",
      "    p.pair_id, a.neighbor AS intermediary,\n",
      "    least(a.first_block, b.first_block) AS first_block,\n",
      "    greatest(a.last_block, b.last_block) AS last_block,\n",
      "    a.example_tx_hash AS example_tx_hash_a,\n",
      "    b.example_tx_hash AS example_tx_hash_b\n",
      "  FROM requested_pairs AS p\n",
      "  JOIN undirected_neighbors AS a ON a.endpoint = p.address_a\n",
      "  JOIN undirected_neighbors AS b\n",
      "    ON b.endpoint = p.address_b AND b.neighbor = a.neighbor\n",
      "  WHERE a.neighbor <> p.address_a\n",
      "    AND a.neighbor <> p.address_b\n",
      "),\n",
      "two_hop_evidence AS (\n",
      "  SELECT\n",
      "    pair_id, count(*) AS intermediary_count,\n",
      "    min(first_block) AS first_block, max(last_block) AS last_block,\n",
      "    min_by(intermediary, intermediary) AS example_intermediary,\n",
      "    min_by(example_tx_hash_a, intermediary) AS example_tx_hash_a,\n",
      "    min_by(example_tx_hash_b, intermediary) AS example_tx_hash_b\n",
      "  FROM two_hop_paths\n",
      "  GROUP BY 1\n",
      ")\n",
      "SELECT\n",
      "  p.pair_id,\n",
      "  lower(concat('0x', to_hex(p.address_a))) AS address_a,\n",
      "  lower(concat('0x', to_hex(p.address_b))) AS address_b,\n",
      "  coalesce(d.transfer_count, 0) AS direct_transfer_count,\n",
      "  d.first_block AS direct_first_block,\n",
      "  d.last_block AS direct_last_block,\n",
      "  CASE WHEN d.example_tx_hash IS NULL THEN NULL\n",
      "       ELSE lower(concat('0x', to_hex(d.example_tx_hash))) END\n",
      "       AS direct_example_tx_hash,\n",
      "  coalesce(h.intermediary_count, 0) AS two_hop_intermediary_count,\n",
      "  h.first_block AS two_hop_first_block,\n",
      "  h.last_block AS two_hop_last_block,\n",
      "  CASE WHEN h.example_intermediary IS NULL THEN NULL\n",
      "       ELSE lower(concat('0x', to_hex(h.example_intermediary))) END\n",
      "       AS two_hop_example_intermediary,\n",
      "  CASE WHEN h.example_tx_hash_a IS NULL THEN NULL\n",
      "       ELSE lower(concat('0x', to_hex(h.example_tx_hash_a))) END\n",
      "       AS two_hop_example_tx_hash_a,\n",
      "  CASE WHEN h.example_tx_hash_b IS NULL THEN NULL\n",
      "       ELSE lower(concat('0x', to_hex(h.example_tx_hash_b))) END\n",
      "       AS two_hop_example_tx_hash_b,\n",
      "  CASE WHEN d.pair_id IS NOT NULL THEN 1\n",
      "       WHEN h.pair_id IS NOT NULL THEN 2 ELSE NULL END\n",
      "       AS shortest_path_hops,\n",
      "  (d.pair_id IS NOT NULL OR h.pair_id IS NOT NULL)\n",
      "       AS entity_relationship_found,\n",
      "  CASE WHEN p.pair_id = (SELECT min(pair_id) FROM requested_pairs)\n",
      "       THEN (SELECT evidence_json FROM service_audit) ELSE NULL END AS service_evidence_json\n",
      "FROM requested_pairs AS p\n",
      "LEFT JOIN direct_evidence AS d ON d.pair_id = p.pair_id\n",
      "LEFT JOIN two_hop_evidence AS h ON h.pair_id = p.pair_id\n",
      "ORDER BY p.pair_id\n"
    )
  }

  # Test whether an undirected graph is connected using a simple breadth-first
  # search. This implements Algorithm 2 without requiring another R package.
  connected_sender_graph <- function(nodes, address_a, address_b) {
    nodes <- sort(unique(as.character(nodes)))
    if (length(nodes) <= 1L) return(TRUE)
    reached <- nodes[[1L]]
    frontier <- reached
    while (length(frontier)) {
      current <- frontier[[1L]]
      frontier <- frontier[-1L]
      neighbours <- unique(c(
        address_b[address_a == current],
        address_a[address_b == current]
      ))
      new_nodes <- setdiff(neighbours, reached)
      reached <- unique(c(reached, new_nodes))
      frontier <- unique(c(frontier, new_nodes))
    }
    setequal(nodes, reached)
  }

  # Send one authenticated request and return both parsed JSON and the original
  # response text. The original text is retained for the reproducibility file.
  dune_json_request <- function(method, url, api_key, body = NULL) {
    request <- httr2::request(url)
    request <- httr2::req_method(request, method)
    request <- httr2::req_headers(
      request,
      `X-Dune-Api-Key` = api_key,
      `Content-Type` = "application/json"
    )
    request <- httr2::req_retry(request, max_tries = 3L)
    request <- httr2::req_timeout(request, seconds = 120)
    if (!is.null(body)) {
      request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
    }

    response <- httr2::req_perform(request)
    status_code <- httr2::resp_status(response)
    raw_text <- httr2::resp_body_string(response)
    if (status_code < 200L || status_code >= 300L) {
      stop(
        paste("Dune API returned HTTP", status_code, raw_text),
        call. = FALSE
      )
    }
    list(
      parsed = jsonlite::fromJSON(raw_text, simplifyVector = TRUE),
      raw = raw_text,
      status_code = status_code
    )
  }

  # Submit the generated SQL, poll until Dune reaches a terminal state, and
  # download every result page. Partial/truncated results are rejected.
  run_dune_sql <- function(sql_text, api_key) {
    submitted <- dune_json_request(
      "POST",
      paste0(dune_api_base, "/sql/execute"),
      api_key,
      list(sql = sql_text, performance = dune_performance)
    )
    execution_id <- submitted$parsed$execution_id
    if (is.null(execution_id) || !nzchar(execution_id)) {
      stop("Dune did not return an execution ID.", call. = FALSE)
    }

    started_at <- Sys.time()
    status_response <- NULL
    repeat {
      status_response <- dune_json_request(
        "GET",
        paste0(dune_api_base, "/execution/", execution_id, "/status"),
        api_key
      )
      state <- status_response$parsed$state
      cat("Dune execution", execution_id, "state:", state, "\n")

      if (state %in% c(
        "QUERY_STATE_COMPLETED", "QUERY_STATE_COMPLETED_PARTIAL",
        "QUERY_STATE_FAILED", "QUERY_STATE_CANCELLED", "QUERY_STATE_EXPIRED"
      )) break

      if (as.numeric(difftime(Sys.time(), started_at, units = "mins")) >
          dune_timeout_minutes) {
        stop("Dune execution exceeded the configured polling timeout.",
             call. = FALSE)
      }
      Sys.sleep(dune_poll_seconds)
    }

    if (!identical(state, "QUERY_STATE_COMPLETED")) {
      error_message <- if (!is.null(status_response$parsed$error$message)) {
        status_response$parsed$error$message
      } else {
        "No detailed Dune error message was returned."
      }
      stop(
        paste("Dune execution ended in", state, "--", error_message),
        call. = FALSE
      )
    }

    page_tables <- list()
    raw_pages <- character()
    offset <- 0L
    total_rows <- Inf

    while (offset < total_rows) {
      page <- dune_json_request(
        "GET",
        paste0(
          dune_api_base, "/execution/", execution_id,
          "/results?limit=", dune_result_page_size,
          "&offset=", offset
        ),
        api_key
      )
      raw_pages <- c(raw_pages, page$raw)
      if (!identical(page$parsed$state, "QUERY_STATE_COMPLETED")) {
        stop("A Dune result page was not complete.", call. = FALSE)
      }

      rows <- page$parsed$result$rows
      if (is.null(rows)) rows <- data.frame()
      if (!is.data.frame(rows)) rows <- as.data.frame(rows)
      page_tables[[length(page_tables) + 1L]] <- rows

      metadata <- page$parsed$result$metadata
      total_rows <- as.numeric(metadata$total_row_count)
      received <- nrow(rows)
      if (!is.finite(total_rows)) {
        stop("Dune result metadata omitted total_row_count.", call. = FALSE)
      }
      if (received == 0L && offset < total_rows) {
        stop("Dune pagination stopped before all rows were returned.",
             call. = FALSE)
      }
      offset <- offset + received
    }

    result_table <- if (length(page_tables)) {
      do.call(rbind, page_tables)
    } else {
      data.frame()
    }
    rownames(result_table) <- NULL

    list(
      execution_id = execution_id,
      submission = submitted,
      final_status = status_response,
      raw_pages = raw_pages,
      rows = result_table
    )
  }

  # ========================================================================
  # 3. PREFLIGHT CHECKS
  # ========================================================================
  # First check the folders. A missing folder usually indicates that the base
  # path is wrong, so continuing would only produce a misleading file error.
  if (!dir.exists(raw_dir)) {
    stop("The raw-data directory does not exist.", call. = FALSE)
  }
  if (!dir.exists(cleaned_dir)) {
    stop("The cleaned-data directory does not exist.", call. = FALSE)
  }

  # External execution requires two small packages. They are checked only when
  # the user explicitly enables Dune, so preparation mode needs no new package.
  if (run_dune_entity_api) {
    missing_api_packages <- c("httr2", "jsonlite")[!vapply(
      c("httr2", "jsonlite"),
      requireNamespace,
      logical(1L),
      quietly = TRUE
    )]
    if (length(missing_api_packages)) {
      stop(
        paste(
          "Install the required Dune API package(s):",
          paste(missing_api_packages, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    dune_api_key <- trimws(Sys.getenv("DUNE_API_KEY", unset = ""))
    if (!nzchar(dune_api_key)) {
      stop(
        "RUN_DUNE_ENTITY_API is true, but DUNE_API_KEY is not set.",
        call. = FALSE
      )
    }
  } else {
    dune_api_key <- ""
  }

  # Build complete paths for both raw swap files and the token metadata file.
  input_paths <- file.path(raw_dir, unname(input_files))
  names(input_paths) <- names(input_files)
  metadata_path <- file.path(raw_dir, metadata_file)

  required_paths <- c(input_paths, metadata_path)
  if (any(!file.exists(required_paths))) {
    stop(
      paste(
        "Required input file missing:",
        paste(required_paths[!file.exists(required_paths)], collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # An MD5 fingerprint is a compact identifier for file contents. Saving it
  # before calculation allows the script to detect a mid-run file replacement.
  input_md5_before <- unname(tools::md5sum(required_paths))

  # Read Query 13's token identities and decimal places, then verify its schema.
  metadata <- read_text_csv(metadata_path)
  require_columns(
    metadata,
    c(
      "comparison_name", "pool_role", "pool_address",
      "token0_address", "token0_symbol", "token0_decimals",
      "token1_address", "token1_symbol", "token1_decimals"
    ),
    metadata_file
  )

  # Ethereum addresses are case-insensitive for this comparison. Lowercase
  # avoids treating the same address as different only because of typography.
  metadata$pool_address <- tolower(metadata$pool_address)
  metadata$token0_decimals <- as_whole_number(
    metadata$token0_decimals,
    "token0_decimals"
  )
  metadata$token1_decimals <- as_whole_number(
    metadata$token1_decimals,
    "token1_decimals"
  )

  # Query 13 contains four rows because it describes both the ECO-USDT and
  # wCELO comparisons. This pipeline deliberately selects only ECO-USDT.
  eco_metadata <- metadata[
    metadata$comparison_name == "ECO-USDT comparison",
    ,
    drop = FALSE
  ]

  if (nrow(eco_metadata) != 2L ||
      !setequal(eco_metadata$pool_role, c("suspicious", "control"))) {
    stop(
      "Query 13 must contain exactly one suspicious and one control ECO-USDT row.",
      call. = FALSE
    )
  }

  # ========================================================================
  # 4. ONE GENERAL STAGE-1 FUNCTION APPLIED IDENTICALLY TO BOTH POOLS
  # ========================================================================
  # This function contains the entire coarse-filter calculation for one pool.
  # Calling the same function twice prevents the suspicious and control samples
  # from receiving subtly different formulas or thresholds.
  run_stage1 <- function(sample_name, input_path, pool_metadata) {
    cat("Running Stage 1 for:", sample_name, "\n")

    # STEP 4A: Read blockchain identifiers and integer amounts as text first.
    # Each CSV row represents one Uniswap V2 Swap event.
    swaps <- read_text_csv(input_path)

    required_swap_columns <- c(
      "contract_address", "evt_tx_hash", "evt_tx_from",
      "evt_index", "evt_block_number",
      "amount0In", "amount0Out", "amount1In", "amount1Out"
    )
    require_columns(swaps, required_swap_columns, basename(input_path))

    # At least two swaps are required because the smallest permitted group has
    # two members; a one-row file cannot form a candidate group.
    if (nrow(swaps) < minimum_group_size) {
      stop(paste(sample_name, "contains fewer than two swaps."), call. = FALSE)
    }

    # STEP 4B: Normalize identifiers so capitalization cannot create false
    # address or transaction mismatches.
    swaps$contract_address <- tolower(swaps$contract_address)
    swaps$evt_tx_hash <- tolower(swaps$evt_tx_hash)
    swaps$evt_tx_from <- tolower(swaps$evt_tx_from)

    # Confirm that all rows truly belong to the pool specified by Query 13.
    expected_pool <- pool_metadata$pool_address[[1L]]
    if (!all(swaps$contract_address == expected_pool)) {
      stop(paste(sample_name, "contains an unexpected pool address."), call. = FALSE)
    }

    # STEP 4C: Convert and validate chronological fields.
    swaps$block_number <- as_whole_number(
      swaps$evt_block_number,
      paste(sample_name, "evt_block_number")
    )
    swaps$event_index <- as_whole_number(
      swaps$evt_index,
      paste(sample_name, "evt_index")
    )

    # Restrict the analysis to the historical block range used for this study.
    if (any(swaps$block_number < paper_start_block |
            swaps$block_number > paper_end_block)) {
      stop(paste(sample_name, "contains a block outside the paper period."), call. = FALSE)
    }

    # STEP 4D: Sort by block and event-log position. These are the adjacent
    # swaps used by the AMM paper's sliding-window coarse filter.
    swaps <- swaps[
      order(swaps$block_number, swaps$event_index),
      ,
      drop = FALSE
    ]
    rownames(swaps) <- NULL
    swaps$swap_sequence <- seq_len(nrow(swaps))

    # A transaction can contain more than one event, so transaction hash plus
    # event-log index—not transaction hash alone—uniquely identifies a swap.
    event_identifier <- paste(swaps$evt_tx_hash, swaps$event_index, sep = ":")
    if (anyDuplicated(event_identifier)) {
      stop(paste(sample_name, "contains duplicate Swap events."), call. = FALSE)
    }

    # STEP 4E: Convert raw token integers into token units using Query 13.
    # The conversion formula is raw_amount / 10^token_decimals.
    # ERC-20 contracts store token quantities as integers. Dividing by 10 raised
    # to the token's decimal count converts them into human-readable token units.
    token0_scale <- 10 ^ pool_metadata$token0_decimals[[1L]]
    token1_scale <- 10 ^ pool_metadata$token1_decimals[[1L]]

    swaps$amount0_in <- as_nonnegative_number(
      swaps$amount0In,
      paste(sample_name, "amount0In")
    ) / token0_scale
    swaps$amount0_out <- as_nonnegative_number(
      swaps$amount0Out,
      paste(sample_name, "amount0Out")
    ) / token0_scale
    swaps$amount1_in <- as_nonnegative_number(
      swaps$amount1In,
      paste(sample_name, "amount1In")
    ) / token1_scale
    swaps$amount1_out <- as_nonnegative_number(
      swaps$amount1Out,
      paste(sample_name, "amount1Out")
    ) / token1_scale

    # STEP 4F: Define token changes from the pool's perspective.
    # Positive means tokens entered the pool; negative means tokens left it.
    swaps$delta_token0 <- swaps$amount0_in - swaps$amount0_out
    swaps$delta_token1 <- swaps$amount1_in - swaps$amount1_out

    # STEP 4G: Generate every adjacent window of length 2 through 10.
    # Count the number of windows in advance so R can allocate the results list
    # once rather than repeatedly enlarging it during thousands of iterations.
    number_of_windows <- sum(vapply(
      minimum_group_size:maximum_group_size,
      function(group_size) max(0L, nrow(swaps) - group_size + 1L),
      integer(1)
    ))

    window_rows <- vector("list", number_of_windows)
    candidate_rows <- list()
    candidate_swap_rows <- list()
    window_counter <- 0L
    candidate_counter <- 0L

    # The outer loop chooses a group length (2, 3, ..., 10). The inner loop then
    # slides that fixed-length window forward by one chronological swap at a time.
    for (group_size in minimum_group_size:maximum_group_size) {
      possible_windows <- nrow(swaps) - group_size + 1L
      if (possible_windows <= 0L) next

      for (start_position in seq_len(possible_windows)) {
        window_counter <- window_counter + 1L
        end_position <- start_position + group_size - 1L
        positions <- start_position:end_position

        # Measure elapsed blockchain blocks between the first and last members.
        block_span <- swaps$block_number[[end_position]] -
          swaps$block_number[[start_position]]
        time_condition <- block_span <= tau

        # Evaluate position restoration independently for token0 and token1.
        ratio_token0 <- restoration_ratio(swaps$delta_token0[positions])
        ratio_token1 <- restoration_ratio(swaps$delta_token1[positions])
        token0_restored <- ratio_token0 <= rho
        token1_restored <- ratio_token1 <= rho

        # The paper's coarse filter accepts approximate restoration of either
        # pool asset; it does not require both tokens at this stage.
        restoration_condition <- token0_restored || token1_restored
        coarse_filter_pass <- time_condition && restoration_condition

        # Save every tested window—including rejected windows—so the decision can
        # be audited and reproduced rather than retaining only positive results.
        window_rows[[window_counter]] <- data.frame(
          sample = sample_name,
          window_id = window_counter,
          start_swap = start_position,
          end_swap = end_position,
          group_size = group_size,
          start_block = swaps$block_number[[start_position]],
          end_block = swaps$block_number[[end_position]],
          block_span = block_span,
          ratio_token0 = ratio_token0,
          ratio_token1 = ratio_token1,
          token0_restored = token0_restored,
          token1_restored = token1_restored,
          time_condition = time_condition,
          restoration_condition = restoration_condition,
          coarse_filter_pass = coarse_filter_pass,
          start_tx_hash = swaps$evt_tx_hash[[start_position]],
          end_tx_hash = swaps$evt_tx_hash[[end_position]],
          stringsAsFactors = FALSE
        )

        # Only passing windows enter the candidate tables. "Candidate" means the
        # window passed this coarse structural test, not that wash trading is proven.
        if (coarse_filter_pass) {
          candidate_counter <- candidate_counter + 1L
          candidate_group_id <- sprintf(
            "%s_AMMR_G%06d",
            sample_name,
            candidate_counter
          )
          group <- swaps[positions, , drop = FALSE]
          # evt_tx_from is the transaction initiator. Stage 2 later asks whether
          # multiple initiators can reasonably be treated as one entity.
          initiators <- unique(group$evt_tx_from)

          candidate_rows[[candidate_counter]] <- data.frame(
            sample = sample_name,
            candidate_id = candidate_counter,
            candidate_group_id = candidate_group_id,
            start_swap = start_position,
            end_swap = end_position,
            included_swap_indices = paste(positions, collapse = ";"),
            group_size = group_size,
            start_block = group$block_number[[1L]],
            end_block = group$block_number[[nrow(group)]],
            block_span = block_span,
            ratio_token0 = ratio_token0,
            ratio_token1 = ratio_token1,
            token0_restored = token0_restored,
            token1_restored = token1_restored,
            time_condition = time_condition,
            restoration_condition = restoration_condition,
            coarse_filter_pass = coarse_filter_pass,
            number_of_initiators = length(initiators),
            initiator_addresses = join_unique(initiators),
            start_tx_hash = group$evt_tx_hash[[1L]],
            end_tx_hash = group$evt_tx_hash[[nrow(group)]],
            stringsAsFactors = FALSE
          )

          # Store one row for every candidate-group/member-swap association. A
          # swap may appear repeatedly because overlapping windows are permitted.
          candidate_swap_rows[[candidate_counter]] <- data.frame(
            sample = sample_name,
            candidate_id = candidate_counter,
            candidate_group_id = candidate_group_id,
            position_in_group = seq_len(nrow(group)),
            swap_sequence = group$swap_sequence,
            contract_address = group$contract_address,
            evt_tx_hash = group$evt_tx_hash,
            evt_tx_from = group$evt_tx_from,
            evt_index = group$event_index,
            evt_block_number = group$block_number,
            amount0In = group$amount0In,
            amount0Out = group$amount0Out,
            amount1In = group$amount1In,
            amount1Out = group$amount1Out,
            delta_token0 = group$delta_token0,
            delta_token1 = group$delta_token1,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # Combine the many small per-window tables into one ordinary rectangular
    # table. rbind means "bind rows underneath one another."
    all_windows <- do.call(rbind, window_rows)
    rownames(all_windows) <- NULL

    candidates <- if (length(candidate_rows)) {
      answer <- do.call(rbind, candidate_rows)
      rownames(answer) <- NULL
      answer
    } else {
      empty_candidate_table()
    }

    candidate_swaps <- if (length(candidate_swap_rows)) {
      answer <- do.call(rbind, candidate_swap_rows)
      rownames(answer) <- NULL
      answer
    } else {
      data.frame(
        sample = character(), candidate_id = integer(),
        candidate_group_id = character(), position_in_group = integer(),
        swap_sequence = integer(), contract_address = character(),
        evt_tx_hash = character(), evt_tx_from = character(),
        evt_index = numeric(), evt_block_number = numeric(),
        amount0In = character(), amount0Out = character(),
        amount1In = character(), amount1Out = character(),
        delta_token0 = numeric(), delta_token1 = numeric(),
        stringsAsFactors = FALSE
      )
    }

    # STEP 4H: Reconstruct candidate membership before saving anything.
    if (nrow(candidates)) {
      reconstructed_groups <- unique(candidate_swaps$candidate_group_id)
      if (!setequal(reconstructed_groups, candidates$candidate_group_id)) {
        stop(paste(sample_name, "candidate memberships did not reconstruct."), call. = FALSE)
      }
    }

    # Print a short progress summary for the person running the script.
    cat(
      "Stage 1 complete for", sample_name,
      ": swaps =", nrow(swaps),
      "; windows =", nrow(all_windows),
      "; candidates =", nrow(candidates),
      "\n"
    )

    list(
      swaps = swaps,
      all_windows = all_windows,
      candidates = candidates,
      candidate_swaps = candidate_swaps,
      token0_symbol = pool_metadata$token0_symbol[[1L]],
      token1_symbol = pool_metadata$token1_symbol[[1L]]
    )
  }

  # ========================================================================
  # 5. RUN THE IDENTICAL STAGE-1 FUNCTION FOR SUSPICIOUS AND CONTROL POOLS
  # ========================================================================
  # The loop selects the matching metadata row, then passes both pools through
  # the same function. No branch changes rho, tau, window size, or formula.
  results <- list()

  for (sample_name in samples) {
    role <- if (sample_name == "suspicious_pool") "suspicious" else "control"
    pool_metadata <- eco_metadata[
      eco_metadata$pool_role == role,
      ,
      drop = FALSE
    ]

    results[[sample_name]] <- run_stage1(
      sample_name,
      input_paths[[sample_name]],
      pool_metadata
    )
  }

  # ========================================================================
  # 6. APPLY THE LOCALLY CALCULABLE STAGE-2 RULES
  # ========================================================================
  # Stage 2 first asks whether a candidate has only one initiator. If so, the
  # same-sender condition passes without an external relationship query.
  #
  # When a candidate has several initiators, the cross-combination rule checks
  # each initiator separately. For each token and initiator, it uses the same
  # position-restoration formula as Stage 1:
  #
  #   absolute net token change / total absolute token movement.
  #
  # An initiator is restored when token0 OR token1 has ratio <= rho. The whole
  # cross-combination candidate passes only when EVERY initiator is restored.
  # Multi-initiator groups that fail this rule need external ETH-graph evidence.
  stage2_sender_rows <- list()
  stage2_candidate_rows <- list()
  entity_pair_rows <- list()

  for (sample_name in samples) {
    candidates <- results[[sample_name]]$candidates
    candidate_swaps <- results[[sample_name]]$candidate_swaps
    if (!nrow(candidates)) next

    for (candidate_row in seq_len(nrow(candidates))) {
      candidate_group_id <- candidates$candidate_group_id[[candidate_row]]

      # Select all member swaps belonging to this one candidate window.
      group <- candidate_swaps[
        candidate_swaps$candidate_group_id == candidate_group_id,
        ,
        drop = FALSE
      ]

      # Sort the unique initiator addresses. A fixed order makes pair generation
      # reproducible and ensures that A--B and B--A are treated as one pair.
      initiators <- sort(unique(group$evt_tx_from))
      number_of_initiators <- length(initiators)
      sender_restored_flags <- logical(number_of_initiators)

      for (sender_position in seq_along(initiators)) {
        initiator <- initiators[[sender_position]]
        sender_swaps <- group[
          group$evt_tx_from == initiator,
          ,
          drop = FALSE
        ]

        ratio_token0 <- restoration_ratio(sender_swaps$delta_token0)
        ratio_token1 <- restoration_ratio(sender_swaps$delta_token1)
        token0_restored <- ratio_token0 <= rho
        token1_restored <- ratio_token1 <= rho
        sender_restored <- token0_restored || token1_restored
        sender_restored_flags[[sender_position]] <- sender_restored

        # Preserve every sender-level calculation, including failed senders, so
        # the candidate-level decision can be independently reconstructed.
        stage2_sender_rows[[length(stage2_sender_rows) + 1L]] <- data.frame(
          sample = sample_name,
          candidate_id = candidates$candidate_id[[candidate_row]],
          candidate_group_id = candidate_group_id,
          initiator_address = initiator,
          initiator_swap_count = nrow(sender_swaps),
          ratio_token0 = ratio_token0,
          ratio_token1 = ratio_token1,
          token0_restored = token0_restored,
          token1_restored = token1_restored,
          sender_restored = sender_restored,
          rho = rho,
          stringsAsFactors = FALSE
        )
      }

      same_sender_pass <- number_of_initiators == 1L
      cross_combination_applicable <- number_of_initiators > 1L
      cross_combination_pass <-
        cross_combination_applicable && all(sender_restored_flags)
      local_stage2_pass <- same_sender_pass || cross_combination_pass
      requires_entity_evidence <- !local_stage2_pass

      local_stage2_status <- if (same_sender_pass) {
        "SAME_SENDER_AUTOMATIC_PASS"
      } else if (cross_combination_pass) {
        "CROSS_COMBINATION_PASS"
      } else {
        "EXTERNAL_ETH_ENTITY_EVIDENCE_REQUIRED"
      }

      stage2_candidate_rows[[length(stage2_candidate_rows) + 1L]] <- data.frame(
        sample = sample_name,
        candidate_id = candidates$candidate_id[[candidate_row]],
        candidate_group_id = candidate_group_id,
        start_swap = candidates$start_swap[[candidate_row]],
        end_swap = candidates$end_swap[[candidate_row]],
        candidate_start_block = candidates$start_block[[candidate_row]],
        candidate_end_block = candidates$end_block[[candidate_row]],
        number_of_initiators = number_of_initiators,
        same_sender_pass = same_sender_pass,
        cross_combination_applicable = cross_combination_applicable,
        cross_combination_pass = cross_combination_pass,
        local_stage2_pass = local_stage2_pass,
        requires_entity_evidence = requires_entity_evidence,
        local_stage2_status = local_stage2_status,
        final_stage2_status = if (requires_entity_evidence) {
          "PENDING_EXTERNAL_ENTITY_EVIDENCE"
        } else {
          "PASS_FROM_LOCAL_STAGE2_RULE"
        },
        stringsAsFactors = FALSE
      )

      if (requires_entity_evidence) {
        # combn(..., 2) creates every unordered pair of initiators in this
        # candidate. These pairs collect evidence; final entity connectivity is
        # evaluated only after the direct/two-hop Dune results are available.
        candidate_pairs <- utils::combn(initiators, 2L)

        for (pair_column in seq_len(ncol(candidate_pairs))) {
          entity_pair_rows[[length(entity_pair_rows) + 1L]] <- data.frame(
            sample = sample_name,
            candidate_id = candidates$candidate_id[[candidate_row]],
            candidate_group_id = candidate_group_id,
            candidate_start_block = candidates$start_block[[candidate_row]],
            candidate_end_block = candidates$end_block[[candidate_row]],
            number_of_initiators = number_of_initiators,
            address_a = candidate_pairs[1L, pair_column],
            address_b = candidate_pairs[2L, pair_column],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # Combine the small per-candidate records into ordinary rectangular tables.
  stage2_sender_results <- do.call(rbind, stage2_sender_rows)
  rownames(stage2_sender_results) <- NULL
  stage2_candidate_decisions <- do.call(rbind, stage2_candidate_rows)
  rownames(stage2_candidate_decisions) <- NULL

  entity_candidate_pair_map <- if (length(entity_pair_rows)) {
    answer <- do.call(rbind, entity_pair_rows)
    rownames(answer) <- NULL
    answer
  } else {
    data.frame(
      sample = character(), candidate_id = integer(),
      candidate_group_id = character(), candidate_start_block = numeric(),
      candidate_end_block = numeric(), number_of_initiators = integer(),
      address_a = character(), address_b = character(),
      stringsAsFactors = FALSE
    )
  }

  # ========================================================================
  # 7. REDUCE REPEATED PAIRS TO ONE DUNE/API EVIDENCE REQUEST EACH
  # ========================================================================
  # The same two addresses can occur together in many overlapping candidates.
  # Dune only needs to retrieve their historical ETH relationship once. The
  # candidate-pair map above retains the many-to-many link needed later.
  if (nrow(entity_candidate_pair_map)) {
    pair_key <- paste(
      entity_candidate_pair_map$address_a,
      entity_candidate_pair_map$address_b,
      sep = "|"
    )
    unique_keys <- sort(unique(pair_key))
    pair_id_lookup <- setNames(
      sprintf("ECO_USDT_PAIR_%05d", seq_along(unique_keys)),
      unique_keys
    )
    entity_candidate_pair_map$pair_id <- unname(pair_id_lookup[pair_key])

    unique_pair_rows <- lapply(unique_keys, function(key) {
      rows <- entity_candidate_pair_map[pair_key == key, , drop = FALSE]
      data.frame(
        pair_id = unname(pair_id_lookup[[key]]),
        address_a = rows$address_a[[1L]],
        address_b = rows$address_b[[1L]],
        number_of_candidate_groups = length(unique(rows$candidate_group_id)),
        samples = join_unique(rows$sample),
        earliest_candidate_start_block = min(rows$candidate_start_block),
        latest_candidate_end_block = max(rows$candidate_end_block),
        direct_eth_check_status = "NOT_RUN",
        two_hop_eth_check_status = "NOT_RUN",
        stringsAsFactors = FALSE
      )
    })
    stage2_api_input <- do.call(rbind, unique_pair_rows)
    rownames(stage2_api_input) <- NULL
  } else {
    entity_candidate_pair_map$pair_id <- character()
    stage2_api_input <- data.frame(
      pair_id = character(), address_a = character(), address_b = character(),
      number_of_candidate_groups = integer(), samples = character(),
      earliest_candidate_start_block = numeric(),
      latest_candidate_end_block = numeric(),
      direct_eth_check_status = character(),
      two_hop_eth_check_status = character(),
      stringsAsFactors = FALSE
    )
  }

  # Put pair_id near the identifying addresses in the detailed mapping table.
  entity_candidate_pair_map <- entity_candidate_pair_map[, c(
    "sample", "candidate_id", "candidate_group_id", "pair_id",
    "candidate_start_block", "candidate_end_block", "number_of_initiators",
    "address_a", "address_b"
  )]

  # ========================================================================
  # 8. RECONCILE STAGE-2 TABLES BEFORE WRITING ANY RESULT
  # ========================================================================
  # Every Stage-1 candidate must have exactly one preliminary Stage-2 decision.
  expected_candidate_groups <- unlist(lapply(
    results,
    function(result) result$candidates$candidate_group_id
  ), use.names = FALSE)

  if (!setequal(
    expected_candidate_groups,
    stage2_candidate_decisions$candidate_group_id
  ) || anyDuplicated(stage2_candidate_decisions$candidate_group_id)) {
    stop("Stage-2 candidate decisions do not match Stage-1 candidates.",
         call. = FALSE)
  }

  # Each candidate must have one sender-result row per unique initiator.
  sender_rows_per_candidate <- table(stage2_sender_results$candidate_group_id)
  expected_sender_rows <- setNames(
    stage2_candidate_decisions$number_of_initiators,
    stage2_candidate_decisions$candidate_group_id
  )
  if (!identical(
    as.integer(sender_rows_per_candidate[names(expected_sender_rows)]),
    as.integer(expected_sender_rows)
  )) {
    stop("Sender-level Stage-2 rows do not match initiator counts.",
         call. = FALSE)
  }

  # The three local outcomes must form a complete, non-overlapping partition.
  outcome_count <- as.integer(stage2_candidate_decisions$same_sender_pass) +
    as.integer(stage2_candidate_decisions$cross_combination_pass) +
    as.integer(stage2_candidate_decisions$requires_entity_evidence)
  if (any(outcome_count != 1L)) {
    stop("Stage-2 local outcomes are not mutually exclusive and exhaustive.",
         call. = FALSE)
  }

  # An unresolved candidate with m initiators must have choose(m, 2) pair rows.
  unresolved <- stage2_candidate_decisions[
    stage2_candidate_decisions$requires_entity_evidence,
    ,
    drop = FALSE
  ]
  pair_rows_per_candidate <- table(entity_candidate_pair_map$candidate_group_id)
  expected_pair_rows <- setNames(
    choose(unresolved$number_of_initiators, 2L),
    unresolved$candidate_group_id
  )
  if (!identical(
    as.integer(pair_rows_per_candidate[names(expected_pair_rows)]),
    as.integer(expected_pair_rows)
  )) {
    stop("Entity-pair rows do not reconstruct from initiator counts.",
         call. = FALSE)
  }

  # Every detailed pair must map to exactly one unique API-input pair ID.
  if (nrow(entity_candidate_pair_map) &&
      (any(is.na(entity_candidate_pair_map$pair_id)) ||
       !setequal(entity_candidate_pair_map$pair_id, stage2_api_input$pair_id))) {
    stop("Detailed entity pairs do not reconcile with unique API pairs.",
         call. = FALSE)
  }

  # ========================================================================
  # 9. BUILD SUMMARY TABLES AND SAVE THE LOCAL STAGE-2 CHECKPOINT
  # ========================================================================
  # Build one summary row per pool. These counts provide quick diagnostics while
  # the detailed files retain the evidence behind every count.
  summary_rows <- lapply(samples, function(sample_name) {
    result <- results[[sample_name]]
    data.frame(
      sample = sample_name,
      input_swap_events = nrow(result$swaps),
      evaluated_adjacent_windows = nrow(result$all_windows),
      stage1_candidate_groups = nrow(result$candidates),
      unique_stage1_candidate_swaps = length(unique(
        result$candidate_swaps$swap_sequence
      )),
      unique_stage1_candidate_transactions = length(unique(
        result$candidate_swaps$evt_tx_hash
      )),
      single_initiator_candidate_groups = sum(
        result$candidates$number_of_initiators == 1L
      ),
      multi_initiator_candidate_groups = sum(
        result$candidates$number_of_initiators > 1L
      ),
      rho = rho,
      tau_blocks = tau,
      minimum_group_size = minimum_group_size,
      maximum_group_size = maximum_group_size,
      classification_status = "STAGE1_COARSE_CANDIDATES_NOT_CONFIRMED_WASH_TRADES",
      stringsAsFactors = FALSE
    )
  })
  stage1_summary <- do.call(rbind, summary_rows)
  rownames(stage1_summary) <- NULL

  # Summarize the formal local Stage-2 results separately for each sample.
  stage2_summary_rows <- lapply(samples, function(sample_name) {
    decisions <- stage2_candidate_decisions[
      stage2_candidate_decisions$sample == sample_name,
      ,
      drop = FALSE
    ]
    pair_map <- entity_candidate_pair_map[
      entity_candidate_pair_map$sample == sample_name,
      ,
      drop = FALSE
    ]
    data.frame(
      sample = sample_name,
      stage1_candidate_groups = nrow(decisions),
      same_sender_automatic_pass_groups = sum(decisions$same_sender_pass),
      multi_initiator_candidate_groups = sum(
        decisions$cross_combination_applicable
      ),
      cross_combination_pass_groups = sum(decisions$cross_combination_pass),
      local_stage2_pass_groups = sum(decisions$local_stage2_pass),
      entity_evidence_required_groups = sum(
        decisions$requires_entity_evidence
      ),
      detailed_candidate_pair_rows = nrow(pair_map),
      unique_entity_evidence_addresses = length(unique(c(
        pair_map$address_a,
        pair_map$address_b
      ))),
      unique_entity_evidence_pairs = length(unique(pair_map$pair_id)),
      external_eth_evidence_status = "NOT_RUN",
      final_stage2_status = "NOT_AVAILABLE_UNTIL_EXTERNAL_EVIDENCE",
      stringsAsFactors = FALSE
    )
  })

  stage2_pre_api_summary <- do.call(rbind, stage2_summary_rows)
  rownames(stage2_pre_api_summary) <- NULL

  # Add a combined row. Counts of candidate groups are additive, while unique
  # addresses and pairs are recalculated globally to avoid double-counting an
  # address or pair that happens to occur in both samples.
  combined_stage2_summary <- data.frame(
    sample = "both_pools",
    stage1_candidate_groups = nrow(stage2_candidate_decisions),
    same_sender_automatic_pass_groups = sum(
      stage2_candidate_decisions$same_sender_pass
    ),
    multi_initiator_candidate_groups = sum(
      stage2_candidate_decisions$cross_combination_applicable
    ),
    cross_combination_pass_groups = sum(
      stage2_candidate_decisions$cross_combination_pass
    ),
    local_stage2_pass_groups = sum(
      stage2_candidate_decisions$local_stage2_pass
    ),
    entity_evidence_required_groups = sum(
      stage2_candidate_decisions$requires_entity_evidence
    ),
    detailed_candidate_pair_rows = nrow(entity_candidate_pair_map),
    unique_entity_evidence_addresses = length(unique(c(
      entity_candidate_pair_map$address_a,
      entity_candidate_pair_map$address_b
    ))),
    unique_entity_evidence_pairs = nrow(stage2_api_input),
    external_eth_evidence_status = "NOT_RUN",
    final_stage2_status = "NOT_AVAILABLE_UNTIL_EXTERNAL_EVIDENCE",
    stringsAsFactors = FALSE
  )
  stage2_pre_api_summary <- rbind(
    stage2_pre_api_summary,
    combined_stage2_summary
  )

  # Reconcile the per-sample summary with the complete decision table.
  if (any(
    stage2_pre_api_summary$stage1_candidate_groups !=
      stage2_pre_api_summary$same_sender_automatic_pass_groups +
      stage2_pre_api_summary$cross_combination_pass_groups +
      stage2_pre_api_summary$entity_evidence_required_groups
  )) {
    stop("The Stage-2 summary does not partition all candidate groups.",
         call. = FALSE)
  }

  # Recalculate the three source fingerprints before saving anything. If a
  # source changed during the calculation, stop rather than mixing versions.
  input_md5_after <- unname(tools::md5sum(required_paths))
  if (!identical(input_md5_before, input_md5_after)) {
    stop("An original input changed during Stages 1--2; no output was saved.",
         call. = FALSE)
  }

  # Save the suspicious and control tables using the filenames declared above.
  stage1_written_paths <- character()
  for (sample_name in samples) {
    files <- stage1_output_files[[sample_name]]
    stage1_written_paths <- c(
      stage1_written_paths,
      write_report(results[[sample_name]]$all_windows, files[["all_windows"]]),
      write_report(results[[sample_name]]$candidates, files[["candidates"]]),
      write_report(results[[sample_name]]$candidate_swaps, files[["candidate_swaps"]])
    )
  }

  stage1_written_paths <- c(
    stage1_written_paths,
    write_report(stage1_summary, summary_file)
  )

  # Save the complete local Stage-2 calculation and the reduced Dune workload.
  stage2_written_paths <- c(
    write_report(stage2_sender_results, stage2_sender_file),
    write_report(stage2_candidate_decisions, stage2_decision_file),
    write_report(entity_candidate_pair_map, stage2_pair_map_file),
    write_report(stage2_api_input, stage2_api_input_file),
    write_report(stage2_pre_api_summary, stage2_summary_file)
  )

  # These files document the decision before external execution; neither is an
  # API result. Next step: review the generated SQL in preparation mode.
  service_policy <- data.frame(
    policy_version = service_policy_version,
    excluded_categories = paste(excluded_service_categories, collapse = ";"),
    paper_start_block = paper_start_block, paper_end_block = paper_end_block,
    unidentified_address_action = "RETAIN_IN_GRAPH",
    evidence_rule = "NAMED_SERVICE_CATEGORY_IDENTIFIER_WITH_SOURCE_OR_REVIEWED_ADDRESS",
    disclaimer = service_disclaimer,
    correction = "Earlier scripts used inconsistent service categories and timing gates.",
    stringsAsFactors = FALSE
  )
  stage2_written_paths <- c(stage2_written_paths,
    write_report(reviewed_services, service_registry_file),
    write_report(service_policy, service_policy_file))

  # ========================================================================
  # 10. GENERATE THE DUNE QUERY AND, WHEN ENABLED, COMPLETE STAGE 2
  # ========================================================================
  # The exact query is saved even in preparation mode. This makes the external
  # rule inspectable before any credits are used and fixes the query text in the
  # reproducibility manifest.
  entity_sql <- build_entity_sql(stage2_api_input)
  stage2_dune_sql_path <- file.path(cleaned_dir, stage2_dune_sql_file)
  writeLines(entity_sql, stage2_dune_sql_path, useBytes = TRUE)
  cat("Saved:", stage2_dune_sql_path, "\n")

  api_written_paths <- stage2_dune_sql_path
  api_written_stages <- "STAGE2_DUNE_QUERY_PREPARATION"

  if (run_dune_entity_api) {
    cat("\nSubmitting the complete entity-recognition SQL to Dune.\n")
    dune_run <- run_dune_sql(entity_sql, dune_api_key)
    # Save the raw response before validation so failed checks remain auditable.
    # Next step: validate the service snapshot and all requested pair rows.
    stage2_dune_raw_path <- file.path(cleaned_dir, stage2_dune_raw_file)
    writeLines(c("[", paste(dune_run$raw_pages, collapse = ",\n"), "]"),
               stage2_dune_raw_path, useBytes = TRUE)
    pair_evidence <- dune_run$rows
    # Audit evidence is returned in the SAME execution as the filtered paths.
    # This prevents labels changing between a separate audit and graph query.
    require_columns(pair_evidence, "service_evidence_json", "Dune service audit")
    audit_text <- pair_evidence$service_evidence_json[
      !is.na(pair_evidence$service_evidence_json)]
    if (length(audit_text) != 1L) {
      stop("Expected exactly one complete service evidence snapshot.", call. = FALSE)
    }
    service_audit <- jsonlite::fromJSON(audit_text[[1L]])
    audit_columns <- c("address", "service_name", "service_category",
                       "evidence_source", "evidence_detail")
    if (!length(service_audit)) {
      service_audit <- as.data.frame(setNames(rep(list(character()), 5L), audit_columns))
    }
    require_columns(service_audit, audit_columns, "Service exclusion audit")
    if (nrow(service_audit) && (anyNA(service_audit[, audit_columns]) ||
        any(!grepl("^0x[0-9a-f]{40}$", service_audit$address)) ||
        any(!nzchar(service_audit$service_name)) ||
        any(!nzchar(service_audit$evidence_source)))) {
      stop("Service exclusion evidence is incomplete.", call. = FALSE)
    }
    service_audit$decision <- rep("IDENTIFIED_PUBLIC_SERVICE", nrow(service_audit))
    service_audit$policy_version <- rep(service_policy_version, nrow(service_audit))
    pair_evidence$service_evidence_json <- NULL


    # Dune must return exactly one evidence row for every requested pair. A
    # missing row is not interpreted as no relationship because that would turn
    # incomplete retrieval into a false negative.
    required_evidence_columns <- c(
      "pair_id", "address_a", "address_b",
      "direct_transfer_count", "direct_first_block", "direct_last_block",
      "direct_example_tx_hash", "two_hop_intermediary_count",
      "two_hop_first_block", "two_hop_last_block",
      "two_hop_example_intermediary", "two_hop_example_tx_hash_a",
      "two_hop_example_tx_hash_b", "shortest_path_hops",
      "entity_relationship_found"
    )
    require_columns(
      pair_evidence,
      required_evidence_columns,
      "Dune entity-recognition result"
    )
    if (nrow(pair_evidence) != nrow(stage2_api_input) ||
        anyDuplicated(pair_evidence$pair_id) ||
        !setequal(pair_evidence$pair_id, stage2_api_input$pair_id)) {
      stop("Dune did not return exactly one row for every requested pair.",
           call. = FALSE)
    }

    pair_evidence$address_a <- tolower(pair_evidence$address_a)
    pair_evidence$address_b <- tolower(pair_evidence$address_b)
    evidence_order <- match(stage2_api_input$pair_id, pair_evidence$pair_id)
    pair_evidence <- pair_evidence[evidence_order, , drop = FALSE]
    rownames(pair_evidence) <- NULL
    if (any(pair_evidence$address_a != stage2_api_input$address_a) ||
        any(pair_evidence$address_b != stage2_api_input$address_b)) {
      stop("Dune pair addresses do not match the submitted API input.",
           call. = FALSE)
    }

    # JSON parsers can represent booleans as logical values or text. Convert
    # only the two accepted spellings and reject anything ambiguous.
    relationship_text <- tolower(as.character(
      pair_evidence$entity_relationship_found
    ))
    if (any(!relationship_text %in% c("true", "false"))) {
      stop("Dune returned an invalid entity_relationship_found value.",
           call. = FALSE)
    }
    pair_evidence$entity_relationship_found <- relationship_text == "true"
    pair_evidence$evidence_status <- "COMPLETE"
    pair_evidence$dune_execution_id <- dune_run$execution_id

    # The shortest-path result must agree with the relationship flag: a found
    # relationship has distance 1 or 2, while an absent relationship has no
    # finite path within the chosen maximum distance.
    path_hops <- suppressWarnings(as.numeric(pair_evidence$shortest_path_hops))
    if (any(
      pair_evidence$entity_relationship_found &
        !(path_hops %in% c(1, 2))
    ) || any(
      !pair_evidence$entity_relationship_found & !is.na(path_hops)
    )) {
      stop("Dune relationship flags and shortest paths disagree.",
           call. = FALSE)
    }

    # Map the deduplicated evidence back to every unresolved candidate pair.
    pair_match <- match(entity_candidate_pair_map$pair_id, pair_evidence$pair_id)
    if (anyNA(pair_match)) {
      stop("Some candidate pairs have no retrieved Dune evidence.",
           call. = FALSE)
    }
    mapped_relationship <- pair_evidence$entity_relationship_found[pair_match]
    mapped_hops <- path_hops[pair_match]

    # Algorithm 2 accepts a multi-address entity group when the sender graph is
    # connected. It does not require all possible address pairs to have edges.
    connectivity_rows <- lapply(seq_len(nrow(unresolved)), function(row_number) {
      candidate_group_id <- unresolved$candidate_group_id[[row_number]]
      candidate_pairs <- entity_candidate_pair_map[
        entity_candidate_pair_map$candidate_group_id == candidate_group_id,
        ,
        drop = FALSE
      ]
      candidate_indices <- which(
        entity_candidate_pair_map$candidate_group_id == candidate_group_id
      )
      edge_flags <- mapped_relationship[candidate_indices]
      edge_hops <- mapped_hops[candidate_indices]
      sender_nodes <- stage2_sender_results$initiator_address[
        stage2_sender_results$candidate_group_id == candidate_group_id
      ]
      connected <- connected_sender_graph(
        sender_nodes,
        candidate_pairs$address_a[edge_flags],
        candidate_pairs$address_b[edge_flags]
      )

      data.frame(
        sample = unresolved$sample[[row_number]],
        candidate_id = unresolved$candidate_id[[row_number]],
        candidate_group_id = candidate_group_id,
        number_of_initiators = unresolved$number_of_initiators[[row_number]],
        expected_pair_count = choose(
          unresolved$number_of_initiators[[row_number]], 2L
        ),
        evidence_pair_count = nrow(candidate_pairs),
        relationship_edge_count = sum(edge_flags),
        direct_edge_count = sum(edge_flags & edge_hops == 1, na.rm = TRUE),
        two_hop_edge_count = sum(edge_flags & edge_hops == 2, na.rm = TRUE),
        sender_graph_connected = connected,
        entity_recognition_status = if (connected) {
          "CONNECTED_WITHIN_TWO_HOPS"
        } else {
          "NOT_CONNECTED_WITHIN_TWO_HOPS"
        },
        dune_execution_id = dune_run$execution_id,
        stringsAsFactors = FALSE
      )
    })
    candidate_connectivity <- do.call(rbind, connectivity_rows)
    rownames(candidate_connectivity) <- NULL

    if (any(
      candidate_connectivity$expected_pair_count !=
        candidate_connectivity$evidence_pair_count
    ) || anyDuplicated(candidate_connectivity$candidate_group_id) ||
        !setequal(
          candidate_connectivity$candidate_group_id,
          unresolved$candidate_group_id
        )) {
      stop("Candidate connectivity evidence is incomplete.", call. = FALSE)
    }

    # Add the external graph result to every preliminary decision. Local passes
    # remain passes. Previously unresolved candidates pass only when their
    # reconstructed sender graph is connected.
    final_stage2_decisions <- stage2_candidate_decisions
    connectivity_match <- match(
      final_stage2_decisions$candidate_group_id,
      candidate_connectivity$candidate_group_id
    )
    final_stage2_decisions$external_entity_graph_connected <- NA
    external_rows <- final_stage2_decisions$requires_entity_evidence
    final_stage2_decisions$external_entity_graph_connected[external_rows] <-
      candidate_connectivity$sender_graph_connected[
        connectivity_match[external_rows]
      ]
    final_stage2_decisions$final_stage2_pass <-
      final_stage2_decisions$local_stage2_pass |
      (!is.na(final_stage2_decisions$external_entity_graph_connected) &
         final_stage2_decisions$external_entity_graph_connected)
    final_stage2_decisions$final_stage2_status <- ifelse(
      final_stage2_decisions$local_stage2_pass,
      "PASS_FROM_LOCAL_STAGE2_RULE",
      ifelse(
        final_stage2_decisions$external_entity_graph_connected,
        "PASS_FROM_CONNECTED_ENTITY_GRAPH",
        "FAIL_ENTITY_GRAPH_NOT_CONNECTED"
      )
    )

    if (anyNA(final_stage2_decisions$final_stage2_pass) ||
        any(final_stage2_decisions$requires_entity_evidence &
            is.na(final_stage2_decisions$external_entity_graph_connected))) {
      stop("Final Stage-2 decisions contain unresolved API cases.",
           call. = FALSE)
    }

    # Summarize the completed Stage-2 partition for each pool and both together.
    post_api_samples <- c(samples, "both_pools")
    stage2_post_api_summary <- do.call(rbind, lapply(
      post_api_samples,
      function(sample_name) {
        rows <- if (sample_name == "both_pools") {
          final_stage2_decisions
        } else {
          final_stage2_decisions[
            final_stage2_decisions$sample == sample_name,
            ,
            drop = FALSE
          ]
        }
        data.frame(
          sample = sample_name,
          stage1_candidate_groups = nrow(rows),
          local_stage2_pass_groups = sum(rows$local_stage2_pass),
          entity_graph_tested_groups = sum(rows$requires_entity_evidence),
          connected_entity_graph_pass_groups = sum(
            rows$requires_entity_evidence & rows$final_stage2_pass
          ),
          entity_graph_fail_groups = sum(
            rows$requires_entity_evidence & !rows$final_stage2_pass
          ),
          final_stage2_pass_groups = sum(rows$final_stage2_pass),
          final_stage2_fail_groups = sum(!rows$final_stage2_pass),
          external_eth_evidence_status = "COMPLETE",
          stage3_mev_status = "NOT_RUN",
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(stage2_post_api_summary) <- NULL

    if (any(
      stage2_post_api_summary$stage1_candidate_groups !=
        stage2_post_api_summary$final_stage2_pass_groups +
        stage2_post_api_summary$final_stage2_fail_groups
    )) {
      stop("Post-API summary does not partition all Stage-1 candidates.",
           call. = FALSE)
    }

    # Preserve raw Dune result pages as a valid JSON array. The API key is sent
    # only in request headers and therefore cannot appear in this file.
    stage2_dune_raw_path <- file.path(cleaned_dir, stage2_dune_raw_file)
    writeLines(
      c("[", paste(dune_run$raw_pages, collapse = ",\n"), "]"),
      stage2_dune_raw_path,
      useBytes = TRUE
    )
    cat("Saved:", stage2_dune_raw_path, "\n")

    # Extract non-secret execution metadata. Missing optional timestamps or cost
    # fields are recorded as NA rather than invented.
    status <- dune_run$final_status$parsed
    scalar_or_na <- function(value) {
      if (is.null(value) || !length(value)) NA else value[[1L]]
    }
    dune_execution_log <- data.frame(
      execution_id = dune_run$execution_id,
      state = scalar_or_na(status$state),
      query_id = scalar_or_na(status$query_id),
      submitted_at = scalar_or_na(status$submitted_at),
      execution_started_at = scalar_or_na(status$execution_started_at),
      execution_ended_at = scalar_or_na(status$execution_ended_at),
      expires_at = scalar_or_na(status$expires_at),
      execution_cost_credits = scalar_or_na(status$execution_cost_credits),
      result_rows = nrow(pair_evidence),
      sql_md5 = unname(tools::md5sum(stage2_dune_sql_path)),
      service_policy_version = service_policy_version,
      unidentified_address_action = "RETAIN_IN_GRAPH",
      performance_tier = dune_performance,
      paper_start_block = paper_start_block,
      paper_end_block = paper_end_block,
      maximum_entity_distance_hops = 2L,
      excluded_service_categories = paste(
        excluded_service_categories,
        collapse = ";"
      ),
      stringsAsFactors = FALSE
    )

    api_written_paths <- c(
      api_written_paths,
      write_report(service_audit, service_audit_file),
      write_report(dune_execution_log, stage2_dune_log_file),
      stage2_dune_raw_path,
      write_report(pair_evidence, stage2_pair_evidence_file),
      write_report(candidate_connectivity, stage2_connectivity_file),
      write_report(final_stage2_decisions, stage2_final_decision_file),
      write_report(stage2_post_api_summary, stage2_post_api_summary_file)
    )
    api_written_stages <- c(
      api_written_stages,
      rep("STAGE2_EXTERNAL_ENTITY_RECOGNITION", 7L)
    )
  }

  written_paths <- c(
    stage1_written_paths,
    stage2_written_paths,
    api_written_paths
  )
  written_stages <- c(
    rep("STAGE1_COARSE_FILTER", length(stage1_written_paths)),
    rep("STAGE2_LOCAL_AND_API_PREPARATION", length(stage2_written_paths)),
    api_written_stages
  )

  # The manifest lists every input/output filename and fingerprint. It supports
  # reproducibility by identifying the exact files used in this run.
  manifest <- data.frame(
    artifact_type = c(
      rep("INPUT", length(required_paths)),
      rep("OUTPUT", length(written_paths))
    ),
    file_name = c(basename(required_paths), basename(written_paths)),
    md5 = c(
      input_md5_after,
      unname(tools::md5sum(written_paths))
    ),
    pipeline_stage = c(
      rep("SOURCE_INPUT", length(required_paths)),
      written_stages
    ),
    stringsAsFactors = FALSE
  )
  write_report(manifest, manifest_file)

  # ========================================================================
  # 11. EXPLICIT CHECKPOINT: DO NOT CLAIM STAGE 3 BEFORE IT IS RUN
  # ========================================================================
  # The final console messages state exactly what has and has not been completed.
  # This protects the thesis analysis from confusing coarse candidates with the
  # final output of the reduced AMM-paper replication.
  cat("\nSTAGE 1 CHECKPOINT COMPLETE\n")
  print(stage1_summary, row.names = FALSE)
  cat("\nLOCAL STAGE-2 RULES COMPLETE\n")
  print(stage2_pre_api_summary, row.names = FALSE)
  cat("\nOriginal input fingerprints unchanged: TRUE\n")
  cat("Same-sender condition: COMPLETE\n")
  cat("Cross-combination condition: COMPLETE\n")
  if (run_dune_entity_api) {
    cat("External ETH entity recognition: COMPLETE\n")
    cat("Final Stage-2 classification: COMPLETE\n")
    print(stage2_post_api_summary, row.names = FALSE)
  } else {
    cat("External ETH entity recognition: NOT RUN (preparation mode)\n")
    cat("Final Stage-2 classification: NOT YET AVAILABLE\n")
  }
  cat("Stage 3 MEV elimination: NOT YET RUN\n")
  cat("Final AMM-replication classification: NOT YET AVAILABLE\n")
  cat(
    "Generated Dune SQL:",
    stage2_dune_sql_path,
    "\n"
  )
  cat(
    if (run_dune_entity_api) {
      "Send the console output and the six post-query Stage-2 files for review.\n"
    } else {
      paste0(
        "Review the generated SQL. Then enable RUN_DUNE_ENTITY_API and rerun ",
        "the same script.\n"
      )
    },
    sep = ""
  )
})
