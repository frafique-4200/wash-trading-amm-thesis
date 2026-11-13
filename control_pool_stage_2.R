# ============================================================
# CONTROL POOL
# STAGE 2 — ENTITY RECOGNITION
#
# File name:
#   control_pool_stage_2.R
#
# PURPOSE
# ============================================================
#
# This script reproduces Stage 2 of the wash-trading detector
# for the CONTROL pool.
#
# It follows the same Stage-2 logic used for the suspicious
# pool:
#
#   1. Read the Stage-1 candidate groups.
#   2. Check the Cross Combination special case.
#   3. Read the ETH-transfer evidence collected from Dune.
#   4. Remove public-service intermediaries before using
#      2-hop ETH relationships.
#   5. Build a sender graph for every candidate group.
#   6. A sender pair receives an edge if:
#
#          distance(sender_a, sender_b) <= 2
#
#      in the cleaned ETH-transfer evidence:
#
#          direct ETH transfer        -> distance 1
#          common non-service node    -> distance 2
#
#   7. A candidate passes entity recognition when the sender
#      graph is connected.
#   8. A candidate survives Stage 2 when it passes either:
#
#          Cross Combination
#              OR
#          entity recognition.
#
#
# IMPORTANT METHODOLOGICAL NOTE
# ============================================================
#
# The paper removes public DeFi/CeFi service addresses and
# their incident edges before entity recognition because
# high-degree public services can connect unrelated traders.
#
# Our bachelor-thesis replication does NOT have the paper's
# complete public-service database. Therefore the 26 service
# addresses below are the high-confidence public-service /
# shared-infrastructure addresses identified from the Dune
# evidence collected in Stages 2C, 2D and 2F.
#
# Addresses not removed are NOT called "private".
# The correct wording is:
#
#   "not identified as a public service in the Dune datasets
#    used in this reduced replication."
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

library(data.table)
library(igraph)


# ============================================================
# 1. USER SETTINGS
# ============================================================

# Main thesis data folder.
data_root <- "D:/Downloads/Bachelor Thesis/Data"

# Folder where Stage-2 outputs will be written.
cleaned_dir <- file.path(data_root, "cleaned")

if (!dir.exists(cleaned_dir)) {
  dir.create(cleaned_dir, recursive = TRUE)
}

# Same restoration threshold used in Stage 1.
rho <- 0.05


# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# 2.1 Find an input file
# ------------------------------------------------------------
#
# Stage-1 output is normally in /cleaned.
# Dune downloads are normally in /raw.
#
# To make the script safer, this function checks:
#
#   preferred folder
#   /raw
#   /cleaned
#   data_root itself
#
# and stops with a clear error if the file cannot be found.

find_input <- function(filename, preferred_subdir = NULL) {

  candidates <- character(0)

  if (!is.null(preferred_subdir)) {
    candidates <- c(
      candidates,
      file.path(data_root, preferred_subdir, filename)
    )
  }

  candidates <- unique(c(
    candidates,
    file.path(data_root, "raw", filename),
    file.path(data_root, "cleaned", filename),
    file.path(data_root, filename)
  ))

  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) {

    stop(
      paste0(
        "\nCould not find input file:\n  ",
        filename,
        "\n\nChecked:\n  ",
        paste(candidates, collapse = "\n  ")
      )
    )
  }

  found[1]
}


# ------------------------------------------------------------
# 2.2 Check required columns
# ------------------------------------------------------------

require_columns <- function(DT, required, table_name) {

  missing_columns <- setdiff(required, names(DT))

  if (length(missing_columns) > 0) {

    stop(
      paste0(
        table_name,
        " is missing required column(s): ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}


# ------------------------------------------------------------
# 2.3 Replace Dune's <nil> strings with NA
# ------------------------------------------------------------

replace_nil <- function(DT) {

  character_columns <- names(DT)[
    vapply(DT, is.character, logical(1))
  ]

  for (column_name in character_columns) {

    bad_rows <- which(
      DT[[column_name]] %in% c("<nil>", "")
    )

    if (length(bad_rows) > 0) {

      set(
        DT,
        i = bad_rows,
        j = column_name,
        value = NA_character_
      )
    }
  }

  invisible(DT)
}


# ------------------------------------------------------------
# 2.4 Standardize Ethereum addresses
# ------------------------------------------------------------
#
# Ethereum addresses are case-insensitive for our graph work.
# We therefore use lowercase consistently.

lower_address <- function(x) {
  tolower(as.character(x))
}


# ------------------------------------------------------------
# 2.5 Put an unordered sender pair in a standard order
# ------------------------------------------------------------
#
# Example:
#
#   (B, A) -> (A, B)
#
# This prevents the same undirected graph edge from appearing
# twice in opposite directions.

standardize_pair <- function(a, b) {

  data.table(
    sender_a = ifelse(a <= b, a, b),
    sender_b = ifelse(a <= b, b, a)
  )
}


# ============================================================
# 3. INPUT FILES
# ============================================================

stage1_file <- find_input(
  "control_pool_stage1_candidate_swaps.csv",
  "cleaned"
)

direct_eth_file <- find_input(
  "control_pool_stage2_direct_eth_relationships.csv",
  "raw"
)

common_intermediary_file <- find_input(
  "control_pool_stage2_common_intermediaries.csv",
  "raw"
)

intermediary_labels_file <- find_input(
  "control_pool_stage2_intermediary_labels.csv",
  "raw"
)

additional_labels_file <- find_input(
  "control_pool_stage2_additional_labels.csv",
  "raw"
)

historical_service_file <- find_input(
  "control_pool_stage2_historical_service_check.csv",
  "raw"
)


cat("\n============================================================\n")
cat("CONTROL POOL — STAGE 2\n")
cat("============================================================\n\n")

cat("Input files:\n")
cat("  Stage 1 candidates:       ", stage1_file, "\n", sep = "")
cat("  Direct ETH relations:     ", direct_eth_file, "\n", sep = "")
cat("  Common intermediaries:    ", common_intermediary_file, "\n", sep = "")
cat("  First label check:        ", intermediary_labels_file, "\n", sep = "")
cat("  Additional label check:   ", additional_labels_file, "\n", sep = "")
cat("  Historical service check: ", historical_service_file, "\n\n", sep = "")


# ============================================================
# 4. READ INPUT DATA
# ============================================================

stage1 <- fread(stage1_file, na.strings = c("", "NA", "<nil>"), integer64 = "character")

direct_eth <- fread(
  direct_eth_file,
  na.strings = c("", "NA", "<nil>"),
  integer64 = "character"
)

common_intermediaries <- fread(
  common_intermediary_file,
  na.strings = c("", "NA", "<nil>"),
  integer64 = "character"
)

intermediary_labels <- fread(
  intermediary_labels_file,
  na.strings = c("", "NA", "<nil>"),
  integer64 = "character"
)

additional_labels <- fread(
  additional_labels_file,
  na.strings = c("", "NA", "<nil>"),
  integer64 = "character"
)

historical_service <- fread(
  historical_service_file,
  na.strings = c("", "NA", "<nil>"),
  integer64 = "character"
)


replace_nil(stage1)
replace_nil(direct_eth)
replace_nil(common_intermediaries)
replace_nil(intermediary_labels)
replace_nil(additional_labels)
replace_nil(historical_service)


# ============================================================
# 5. VALIDATE INPUT SCHEMAS
# ============================================================

require_columns(
  stage1,
  c(
    "candidate_id",
    "evt_tx_hash",
    "evt_tx_from",
    "delta_WETH",
    "delta_ANON"
  ),
  "control_pool_stage1_candidate_swaps.csv"
)

require_columns(
  direct_eth,
  c(
    "sender_from",
    "sender_to"
  ),
  "control_pool_stage2_direct_eth_relationships.csv"
)

require_columns(
  common_intermediaries,
  c(
    "sender_a",
    "sender_b",
    "common_intermediary"
  ),
  "control_pool_stage2_common_intermediaries.csv"
)

require_columns(
  intermediary_labels,
  c(
    "address",
    "owner_key",
    "custody_owner"
  ),
  "control_pool_stage2_intermediary_labels.csv"
)

require_columns(
  additional_labels,
  c(
    "address",
    "name",
    "category",
    "label_type"
  ),
  "control_pool_stage2_additional_labels.csv"
)

expected_historical_columns <- c(
  "address",
  "owner_key",
  "custody_owner",
  "account_owner",
  "contract_name",
  "eoa",
  "source",
  "source_website",
  "source_evidence",
  "entity_name",
  "primary_category",
  "category_tags",
  "cex_name",
  "deposit_cex_name",
  "deposit_first_block_time",
  "contract_deployed_at",
  "contract_deployed_block",
  "contract_existed_by_candidate_time"
)

require_columns(
  historical_service,
  expected_historical_columns,
  "control_pool_stage2_historical_service_check.csv"
)


# ============================================================
# 6. STANDARDIZE ADDRESS COLUMNS
# ============================================================

stage1[
  ,
  evt_tx_from := lower_address(evt_tx_from)
]

direct_eth[
  ,
  `:=`(
    sender_from = lower_address(sender_from),
    sender_to   = lower_address(sender_to)
  )
]

common_intermediaries[
  ,
  `:=`(
    sender_a            = lower_address(sender_a),
    sender_b            = lower_address(sender_b),
    common_intermediary = lower_address(common_intermediary)
  )
]

intermediary_labels[
  ,
  address := lower_address(address)
]

additional_labels[
  ,
  address := lower_address(address)
]

historical_service[
  ,
  address := lower_address(address)
]


# ============================================================
# 7. CONTROL-POOL SNAPSHOT SANITY CHECKS
# ============================================================
#
# These checks protect against accidentally using the wrong
# CSV or an incomplete download.
#
# They describe the control-pool dataset that we have already
# validated manually.

candidate_ids <- sort(unique(stage1$candidate_id))

if (length(candidate_ids) != 16) {
  stop(
    paste0(
      "Expected 16 Stage-1 candidates, but found ",
      length(candidate_ids),
      "."
    )
  )
}

if (nrow(common_intermediaries) != 130) {
  stop(
    paste0(
      "Expected 130 Stage-2B 2-hop path rows, but found ",
      nrow(common_intermediaries),
      "."
    )
  )
}

if (uniqueN(common_intermediaries$common_intermediary) != 40) {
  stop(
    paste0(
      "Expected 40 unique Stage-2B intermediaries, but found ",
      uniqueN(common_intermediaries$common_intermediary),
      "."
    )
  )
}

if (
  nrow(historical_service) != 40 ||
  uniqueN(historical_service$address) != 40
) {

  stop(
    paste0(
      "Historical service file should contain 40 rows and ",
      "40 unique intermediary addresses."
    )
  )
}


# ============================================================
# 8. CROSS COMBINATION CHECK
# ============================================================
#
# This is the same reduced operational rule used in the
# suspicious-pool implementation.
#
# For each sender inside each candidate group, calculate:
#
#               |net position change|
#   ratio = --------------------------------
#            gross absolute position change
#
#
# separately for token0 and token1.
#
# A sender approximately restores its position when at least
# one token has:
#
#               ratio <= rho
#
# where:
#
#               rho = 0.05
#
#
# A Cross Combination requires:
#
#   1. more than one sender; and
#   2. every sender passes the per-sender restoration test.
#
# A single-sender group is NOT labelled Cross Combination.


stage1[
  ,
  `:=`(
    delta_WETH = as.numeric(delta_WETH),
    delta_ANON = as.numeric(delta_ANON)
  )
]

if (
  any(is.na(stage1$delta_WETH)) ||
  any(is.na(stage1$delta_ANON))
) {

  stop(
    paste0(
      "Stage-1 candidate file contains missing/non-numeric ",
      "delta_WETH or delta_ANON values."
    )
  )
}


sender_restoration <- stage1[
  ,
  .(
    n_sender_swaps = .N,

    net_WETH = sum(delta_WETH),
    gross_WETH = sum(abs(delta_WETH)),

    net_ANON = sum(delta_ANON),
    gross_ANON = sum(abs(delta_ANON))
  ),
  by = .(
    candidate_id,
    sender = evt_tx_from
  )
]


sender_restoration[
  ,
  ratio_WETH := ifelse(
    gross_WETH > 0,
    abs(net_WETH) / gross_WETH,
    NA_real_
  )
]


sender_restoration[
  ,
  ratio_ANON := ifelse(
    gross_ANON > 0,
    abs(net_ANON) / gross_ANON,
    NA_real_
  )
]


sender_restoration[
  ,
  sender_restoration_pass :=

    (
      !is.na(ratio_WETH) &
      ratio_WETH <= rho
    ) |

    (
      !is.na(ratio_ANON) &
      ratio_ANON <= rho
    )
]


cross_combination_summary <- sender_restoration[
  ,
  .(
    n_senders = .N,

    cross_combination_pass =
      (.N > 1) &&
      all(sender_restoration_pass)
  ),
  by = candidate_id
]


# ============================================================
# 9. PUBLIC-SERVICE CLASSIFICATION
# ============================================================
#
# These 26 addresses are the high-confidence public service /
# shared-infrastructure addresses identified by reviewing the
# control-pool Stage 2C, 2D and 2F Dune outputs.
#
# IMPORTANT:
#
# This is a MANUALLY REVIEWED classification, not a guess
# produced from a broad keyword search.
#
# We encode the reviewed decisions explicitly so that the
# Stage-2 result is reproducible and auditable.


service_reasons <- c(

  "0x0fabaf48bbf864a3947bdd0ba9d764791a60467a" =
    "Ribbon v1 RibbonThetaVault — public DeFi infrastructure",

  "0x11111112542d85b3ef69ae05771c2dccff4faa26" =
    "1inch AggregationRouterV3 — DEX aggregator",

  "0x111111125434b319222cdbf8c261674adb56f3ae" =
    "1inch AggregationRouterV2 — DEX aggregator",

  "0x1111111254fb6c44bac0bed2854e76f90643097d" =
    "1inch AggregationRouterV4 — DEX aggregator",

  "0x283af0b28c62c092c9727f1ee09c02ca627eb7f5" =
    "Ethereum Name Service registrar infrastructure",

  "0x3980c9ed79d2c191a89e02fa3529c60ed6e9c04b" =
    "Metis L1StandardBridge — bridge infrastructure",

  "0x3e66b66fd1d0b02fda6c811da9e0547970db2f21" =
    "Balancer V2 ExchangeProxy — DEX infrastructure",

  "0x4dbd4fc535ac27206064b68ffcf827b0a60bab3f" =
    "Arbitrum Inbox / gateway infrastructure",

  "0x52ec2f3d7c5977a8e558c8d9c6000b615098e8fc" =
    "Teleportr / Optimism deposit infrastructure",

  "0x564286362092d8e7936f0549571a803b203aaced" =
    "Binance — centralized exchange",

  "0x68b3465833fb72a70ecdf485e0e4c7bd8665fc45" =
    "Uniswap V3 SwapRouter02 — DEX infrastructure",

  "0x73282a63f0e3d7e9604575420f777361eca3c86a" =
    "SKALE Escrow — blockchain/service infrastructure",

  "0x75e89d5979e4f6fba9f97c104c2f0afb3f1dcb88" =
    "MEXC — centralized exchange",

  "0x7a250d5630b4cf539739df2c5dacb4c659f2488d" =
    "Uniswap V2 Router02 — DEX infrastructure",

  "0x7be8076f4ea4a4ad08075c2508e481d6c946d12b" =
    "OpenSea WyvernExchange — public marketplace infrastructure",

  "0x7f268357a8c2552623316e2562d90e642bb538e5" =
    "OpenSea WyvernExchange — public marketplace infrastructure",

  "0x986a2fca9eda0e06fbf7839b89bfc006ee2a23dd" =
    "AscendEX — centralized exchange",

  "0xa0c68c638235ee32657e8f720a23cec1bfc77c77" =
    "Polygon RootChainManager — bridge/service infrastructure",

  "0xabea9132b05a70803a4e85094fd0e1800777fbef" =
    "zkSync Lite bridge / service infrastructure",

  "0xba12222222228d8ba445958a75a0704d566bf2c8" =
    "Balancer V2 Vault — DEX infrastructure",

  "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2" =
    "WETH9 — shared public token/wrapping infrastructure",

  "0xc098b2a3aa256d2140208c3de6543aaef5cd3a94" =
    "FTX — centralized exchange",

  "0xcd4ec7b66fbc029c116ba9ffb3e59351c20b5b06" =
    "Rarible ExchangeV1 — public marketplace infrastructure",

  "0xd9e1ce17f2641f24ae83637ab66a2cca9c378b9f" =
    "SushiSwap Router02 — DEX infrastructure",

  "0xdef1c0ded9bec7f1a1670819833240f027b25eff" =
    "0x ExchangeProxy — DEX / aggregator infrastructure",

  "0xe592427a0aece92de3edee1f18e0157c05861564" =
    "Uniswap V3 SwapRouter — DEX infrastructure"
)


service_addresses <- names(service_reasons)


if (length(service_addresses) != 26) {

  stop(
    paste0(
      "Internal service-classification error: expected ",
      "26 service addresses, but found ",
      length(service_addresses),
      "."
    )
  )
}


all_intermediaries <- sort(
  unique(common_intermediaries$common_intermediary)
)


missing_service_addresses <- setdiff(
  service_addresses,
  all_intermediaries
)


if (length(missing_service_addresses) > 0) {

  stop(
    paste0(
      "The reviewed service list contains address(es) that ",
      "are missing from the Stage-2B intermediary file:\n",
      paste(missing_service_addresses, collapse = "\n")
    )
  )
}


service_classification <- data.table(
  address = all_intermediaries
)


service_classification[
  ,
  remove_as_public_service :=
    address %in% service_addresses
]


service_classification[
  ,
  classification := ifelse(
    remove_as_public_service,
    "REMOVE_PUBLIC_SERVICE",
    "RETAIN_NOT_IDENTIFIED_AS_PUBLIC_SERVICE"
  )
]


service_classification[
  ,
  service_reason := unname(
    service_reasons[address]
  )
]


# ============================================================
# 10. ADD LABEL EVIDENCE TO THE CLASSIFICATION TABLE
# ============================================================
#
# This section does not change the classification.
#
# It adds the Dune evidence to the output CSV so that the
# manual public-service decisions are transparent and can be
# reviewed later.


identifier_labels <- additional_labels[
  label_type == "identifier" &
    !is.na(name),
  .(
    identifier_names =
      paste(
        sort(unique(name)),
        collapse = " | "
      ),

    identifier_categories =
      paste(
        sort(unique(category[!is.na(category)])),
        collapse = " | "
      )
  ),
  by = address
]


service_classification <- merge(
  service_classification,
  historical_service,
  by = "address",
  all.x = TRUE,
  sort = FALSE
)


service_classification <- merge(
  service_classification,
  identifier_labels,
  by = "address",
  all.x = TRUE,
  sort = FALSE
)


setorder(
  service_classification,
  address
)


# ============================================================
# 11. REMOVE PUBLIC-SERVICE 2-HOP PATHS
# ============================================================
#
# Stage 2B found:
#
#     Sender A -- common intermediary -- Sender B
#
# If the common intermediary is a public service, that path is
# removed before entity recognition.


clean_common_intermediaries <- common_intermediaries[
  !common_intermediary %in% service_addresses
]


# Snapshot sanity checks.
#
# These are the counts obtained from the reviewed control-pool
# data. They help catch accidental file/address changes.

if (nrow(clean_common_intermediaries) != 34) {

  stop(
    paste0(
      "After service removal, expected 34 remaining 2-hop ",
      "path rows, but found ",
      nrow(clean_common_intermediaries),
      "."
    )
  )
}


if (
  uniqueN(clean_common_intermediaries$common_intermediary) != 14
) {

  stop(
    paste0(
      "After service removal, expected 14 retained ",
      "intermediaries, but found ",
      uniqueN(clean_common_intermediaries$common_intermediary),
      "."
    )
  )
}


# ============================================================
# 12. CONVERT DIRECT ETH TRANSFERS INTO DISTANCE-1 EDGES
# ============================================================
#
# Self-transfers:
#
#     A -> A
#
# do NOT create a relationship between two different trader
# addresses and are therefore removed.


direct_nonself <- direct_eth[
  !is.na(sender_from) &
    !is.na(sender_to) &
    sender_from != sender_to
]


direct_pair_order <- standardize_pair(
  direct_nonself$sender_from,
  direct_nonself$sender_to
)


direct_edges <- unique(
  direct_pair_order
)


if (nrow(direct_edges) != 1) {

  stop(
    paste0(
      "Expected exactly 1 unique non-self direct sender edge, ",
      "but found ",
      nrow(direct_edges),
      "."
    )
  )
}


# ============================================================
# 13. CONVERT CLEAN 2-HOP PATHS INTO SENDER EDGES
# ============================================================

two_hop_pair_order <- standardize_pair(
  clean_common_intermediaries$sender_a,
  clean_common_intermediaries$sender_b
)


two_hop_evidence <- data.table(
  sender_a = two_hop_pair_order$sender_a,
  sender_b = two_hop_pair_order$sender_b,
  common_intermediary =
    clean_common_intermediaries$common_intermediary
)


two_hop_evidence <- unique(
  two_hop_evidence
)


two_hop_edges <- unique(
  two_hop_evidence[
    ,
    .(
      sender_a,
      sender_b
    )
  ]
)


if (nrow(two_hop_edges) != 26) {

  stop(
    paste0(
      "Expected 26 unique cleaned 2-hop sender pairs, but ",
      "found ",
      nrow(two_hop_edges),
      "."
    )
  )
}


# ============================================================
# 14. COMBINE DISTANCE-1 AND DISTANCE-2 EVIDENCE
# ============================================================
#
# We retain the type of evidence for auditability.
#
# The same sender pair can have both:
#
#   a direct relationship
#   and
#   one or more cleaned 2-hop relationships.


direct_evidence <- direct_edges[
  ,
  .(
    sender_a,
    sender_b,
    relation_type = "direct_1_hop",
    common_intermediary = NA_character_
  )
]


two_hop_relation_evidence <- two_hop_evidence[
  ,
  .(
    sender_a,
    sender_b,
    relation_type = "two_hop",
    common_intermediary
  )
]


relationship_evidence <- rbindlist(
  list(
    direct_evidence,
    two_hop_relation_evidence
  ),
  use.names = TRUE,
  fill = TRUE
)


pair_summary <- relationship_evidence[
  ,
  .(
    has_direct_1_hop =
      any(relation_type == "direct_1_hop"),

    has_two_hop =
      any(relation_type == "two_hop"),

    n_clean_two_hop_intermediaries =
      uniqueN(
        common_intermediary[
          relation_type == "two_hop" &
            !is.na(common_intermediary)
        ]
      )
  ),
  by = .(
    sender_a,
    sender_b
  )
]


setorder(
  pair_summary,
  sender_a,
  sender_b
)


# ============================================================
# 15. CHECK WHETHER ANY CANDIDATE SENDER IS A SERVICE ADDRESS
# ============================================================
#
# In this control sample the answer should be zero.
#
# We check explicitly because the paper removes service nodes
# before entity recognition.

candidate_senders <- sort(
  unique(stage1$evt_tx_from)
)


candidate_senders_marked_service <- intersect(
  candidate_senders,
  service_addresses
)


if (length(candidate_senders_marked_service) > 0) {

  stop(
    paste0(
      "At least one Stage-1 candidate initiator is also in ",
      "the reviewed public-service list. This requires manual ",
      "review before continuing:\n",
      paste(
        candidate_senders_marked_service,
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# 16. BUILD A SENDER GRAPH FOR EACH CANDIDATE
# ============================================================
#
# Paper rule:
#
#   - one sender:
#       automatically same entity
#
#   - multiple senders:
#       connect sender pairs when distance <= 2
#       in the cleaned ETH graph;
#
#       candidate passes if the sender graph is connected.
#
#
# Because our Dune queries already extracted the distance-1
# and distance-2 relationships, we can construct G_sender
# directly from pair_summary.


candidate_edge_details_list <- list()
stage2_result_list <- list()


for (cid in candidate_ids) {

  candidate_swaps <- stage1[
    candidate_id == cid
  ]


  senders <- sort(
    unique(candidate_swaps$evt_tx_from)
  )


  n_senders <- length(senders)


  candidate_edges <- pair_summary[
    sender_a %in% senders &
      sender_b %in% senders
  ]


  # ----------------------------------------------------------
  # Create the candidate sender graph.
  # ----------------------------------------------------------

  vertices <- data.frame(
    name = senders,
    stringsAsFactors = FALSE
  )


  if (nrow(candidate_edges) == 0) {

    sender_graph <- make_empty_graph(
      n = n_senders,
      directed = FALSE
    )

    V(sender_graph)$name <- senders

  } else {

    edge_dataframe <- data.frame(
      from = candidate_edges$sender_a,
      to   = candidate_edges$sender_b,
      stringsAsFactors = FALSE
    )

    sender_graph <- graph_from_data_frame(
      edge_dataframe,
      directed = FALSE,
      vertices = vertices
    )
  }


  # A one-vertex graph is connected, which reproduces the
  # paper's automatic single-sender rule.
  entity_recognition_pass <- is_connected(
    sender_graph
  )


  component_information <- components(
    sender_graph
  )


  n_components <- component_information$no


  largest_component_size <- max(
    component_information$csize
  )


  cross_pass <- cross_combination_summary[
    candidate_id == cid,
    cross_combination_pass
  ]


  if (length(cross_pass) != 1) {

    stop(
      paste0(
        "Could not obtain exactly one Cross Combination ",
        "result for candidate ",
        cid,
        "."
      )
    )
  }


  stage2_pass <-
    isTRUE(cross_pass) ||
    isTRUE(entity_recognition_pass)


  decision_route <- if (
    isTRUE(cross_pass)
  ) {

    "PASS_CROSS_COMBINATION"

  } else if (
    isTRUE(entity_recognition_pass)
  ) {

    "PASS_ENTITY_GRAPH"

  } else {

    "FAIL_STAGE_2"
  }


  stage2_result_list[[length(stage2_result_list) + 1]] <-

    data.table(

      candidate_id = cid,

      n_candidate_swaps =
        nrow(candidate_swaps),

      n_senders =
        n_senders,

      cross_combination_pass =
        isTRUE(cross_pass),

      n_clean_graph_edges =
        nrow(candidate_edges),

      n_connected_components =
        n_components,

      largest_component_size =
        largest_component_size,

      entity_recognition_pass =
        isTRUE(entity_recognition_pass),

      stage2_pass =
        stage2_pass,

      decision_route =
        decision_route
    )


  if (nrow(candidate_edges) > 0) {

    candidate_edge_output <- copy(
      candidate_edges
    )

    candidate_edge_output[
      ,
      candidate_id := cid
    ]

    setcolorder(
      candidate_edge_output,
      c(
        "candidate_id",
        "sender_a",
        "sender_b",
        "has_direct_1_hop",
        "has_two_hop",
        "n_clean_two_hop_intermediaries"
      )
    )

    candidate_edge_details_list[[length(candidate_edge_details_list) + 1]] <- candidate_edge_output
  }
}


stage2_summary <- rbindlist(
  stage2_result_list
)


setorder(
  stage2_summary,
  candidate_id
)


if (length(candidate_edge_details_list) > 0) {

  candidate_edge_details <- rbindlist(
    candidate_edge_details_list,
    use.names = TRUE,
    fill = TRUE
  )

} else {

  candidate_edge_details <- data.table(
    candidate_id = integer(),
    sender_a = character(),
    sender_b = character(),
    has_direct_1_hop = logical(),
    has_two_hop = logical(),
    n_clean_two_hop_intermediaries = integer()
  )
}


# ============================================================
# 17. STAGE-2 SURVIVING SWAPS
# ============================================================

surviving_candidate_ids <- stage2_summary[
  stage2_pass == TRUE,
  candidate_id
]


stage2_surviving_swaps <- stage1[
  candidate_id %in% surviving_candidate_ids
]


# ============================================================
# 18. WRITE OUTPUT FILES
# ============================================================
#
# The first two files are the principal Stage-2 outputs.
#
# The remaining files provide audit evidence so that every
# decision can be traced later when writing the thesis.


summary_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_summary.csv"
)


surviving_swaps_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_surviving_swaps.csv"
)


service_classification_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_service_classification.csv"
)


cleaned_intermediaries_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_cleaned_intermediaries.csv"
)


cleaned_paths_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_cleaned_two_hop_paths.csv"
)


sender_restoration_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_sender_restoration.csv"
)


clean_relationships_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_clean_relationships.csv"
)


candidate_edges_output_file <- file.path(
  cleaned_dir,
  "control_pool_stage2_candidate_edges.csv"
)


fwrite(
  stage2_summary,
  summary_output_file
)


fwrite(
  stage2_surviving_swaps,
  surviving_swaps_output_file
)


fwrite(
  service_classification,
  service_classification_output_file
)


fwrite(
  service_classification[
    remove_as_public_service == FALSE
  ],
  cleaned_intermediaries_output_file
)


fwrite(
  clean_common_intermediaries,
  cleaned_paths_output_file
)


fwrite(
  sender_restoration,
  sender_restoration_output_file
)


fwrite(
  pair_summary,
  clean_relationships_output_file
)


fwrite(
  candidate_edge_details,
  candidate_edges_output_file
)


# ============================================================
# 19. PRINT RESULTS
# ============================================================

cat("\n============================================================\n")
cat("STAGE 2 RESULTS\n")
cat("============================================================\n\n")

cat(
  "Stage-1 candidates entering Stage 2: ",
  nrow(stage2_summary),
  "\n",
  sep = ""
)

cat(
  "Cross Combination candidates:        ",
  sum(stage2_summary$cross_combination_pass),
  "\n",
  sep = ""
)

cat(
  "Public-service intermediaries removed: ",
  sum(service_classification$remove_as_public_service),
  "\n",
  sep = ""
)

cat(
  "Intermediaries retained after cleaning: ",
  sum(!service_classification$remove_as_public_service),
  "\n",
  sep = ""
)

cat(
  "Original 2-hop path rows:             ",
  nrow(common_intermediaries),
  "\n",
  sep = ""
)

cat(
  "Cleaned 2-hop path rows:              ",
  nrow(clean_common_intermediaries),
  "\n",
  sep = ""
)

cat(
  "Unique cleaned 2-hop sender pairs:    ",
  nrow(two_hop_edges),
  "\n",
  sep = ""
)

cat(
  "Unique non-self direct sender edges:  ",
  nrow(direct_edges),
  "\n",
  sep = ""
)

cat(
  "Candidates passing entity graph:      ",
  sum(stage2_summary$entity_recognition_pass),
  "\n",
  sep = ""
)

cat(
  "Final Stage-2 survivors:              ",
  sum(stage2_summary$stage2_pass),
  "\n\n",
  sep = ""
)


print(
  stage2_summary[
    ,
    .(
      candidate_id,
      n_senders,
      cross_combination_pass,
      n_clean_graph_edges,
      n_connected_components,
      largest_component_size,
      entity_recognition_pass,
      stage2_pass,
      decision_route
    )
  ]
)


# ============================================================
# 20. VERIFIED CONTROL-SAMPLE RESULT CHECK
# ============================================================
#
# Our independent manual reconstruction produced 0/16 Stage-2
# survivors.
#
# We do NOT force the algorithm to return zero.
#
# We only warn if the reproducible script gives a different
# answer, because that would indicate an input/version mismatch
# that should be investigated before proceeding.


if (sum(stage2_summary$stage2_pass) != 0) {

  warning(
    paste0(
      "The script result differs from the independently ",
      "verified control-pool result of 0 Stage-2 survivors. ",
      "Do not proceed to Stage 3 until the difference is ",
      "investigated."
    )
  )

} else {

  cat(
    "\nVerified result reproduced: 0 of 16 candidates survive Stage 2.\n"
  )

  cat(
    "Because there are no Stage-2 survivors, Stage 3 is not required for this control sample.\n"
  )
}


cat("\nOutput files written to:\n")
cat("  ", summary_output_file, "\n", sep = "")
cat("  ", surviving_swaps_output_file, "\n", sep = "")
cat("  ", service_classification_output_file, "\n", sep = "")
cat("  ", cleaned_intermediaries_output_file, "\n", sep = "")
cat("  ", cleaned_paths_output_file, "\n", sep = "")
cat("  ", sender_restoration_output_file, "\n", sep = "")
cat("  ", clean_relationships_output_file, "\n", sep = "")
cat("  ", candidate_edges_output_file, "\n", sep = "")

cat("\n============================================================\n")
cat("CONTROL POOL — STAGE 2 COMPLETE\n")
cat("============================================================\n")
