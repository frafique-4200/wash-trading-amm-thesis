# ============================================================
# VALIDATION TEST 1
# End-to-end validation of the AMM wash-trading methodology
#
# PURPOSE:
# The academic paper explicitly identifies this pair of
# Uniswap V2 transactions as a one-hop wash-trading example.
#
# We therefore use it as a KNOWN POSITIVE TEST CASE.
#
# The script validates:
#
#   Stage 1: Coarse filtering
#       - chronological ordering
#       - adjacency
#       - time threshold
#       - approximate pool restoration
#
#   Stage 2: Entity recognition
#
#   Stage 3: MEV elimination
#
# EXPECTED FINAL RESULT:
#
#       KEEP AS WASH-TRADING CANDIDATE
#
# ============================================================



# ============================================================
# PARAMETERS
# ============================================================

# Maximum block distance used in the paper.
TAU <- 200


# Maximum approximate pool-position change.
#
# 0.05 = 5%
RHO <- 0.05


# Maximum ETH-transfer graph distance for entity recognition.
K <- 2



# ============================================================
# KNOWN TRANSACTIONS FROM THE PAPER
# ============================================================

# First wash-trading transaction.

wash_tx1 <-
  "0x78d1713837aae1d0995383550900ef33efc321e8870bdba7fbf5e398f7484eec"


# Second wash-trading transaction.

wash_tx2 <-
  "0xa28546bd2203675f0dc27cad7a45549f4c71dc1c4cfaa82a485d3eafcc7125ac"



# ============================================================
# KNOWN ADDRESSES
# ============================================================

# Initiator of the first swap.

trader_A <-
  "0x5c2bc88f19674fbf71bcede54f88e6b05ede40d7"


# Initiator of the second swap.

trader_B <-
  "0x3a7f85b30678b27668b5b65a6519bfe818235467"


# XRT/WETH Uniswap V2 pool.

pool_address <-
  "0x3185626c14acb9531d19560decb9d3e5e80681b1"


# Both transactions call this same execution contract.

shared_execution_contract <-
  "0x284900d5ab66356fbf936a0469ab0e138f965114"



# ============================================================
# STAGE 1 — COARSE FILTERING
# ============================================================


# ============================================================
# STEP 1A — LOAD THE TWO KNOWN SWAPS
# ============================================================

# These are the two swaps explicitly presented by the authors
# as a wash-trading example.
#
# We load all columns as character first so that Ethereum
# addresses, hashes, and large integer token values are not
# modified by R.

swaps <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_data.csv",
  colClasses = "character"
)



# ============================================================
# STEP 1B — CHRONOLOGICAL ORDERING
# ============================================================

# Blockchain events must be processed chronologically.
#
# The block number gives the main ordering.
# Within the same block, evt_index determines event order.

swaps$block_number <-
  as.integer(
    as.numeric(swaps$evt_block_number)
  )


swaps$event_index <-
  as.integer(swaps$evt_index)


swaps <- swaps[
  order(
    swaps$block_number,
    swaps$event_index
  ),
]



# ============================================================
# STEP 1C — VERIFY ADJACENCY
# ============================================================

# IMPORTANT:
#
# The paper does not simply compare arbitrary transactions
# occurring within 200 blocks.
#
# It constructs candidate groups from ADJACENT swaps in each
# AMM pool.
#
# Therefore, we separately downloaded EVERY swap occurring
# in the XRT/WETH pool between the two known transactions.
#
# File:
#
# validation_test_1b_adjacency_check.csv
#
# The goal is to verify that no other pool swap occurs between
# wash_tx1 and wash_tx2.


adjacency_data <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1b_adjacency_check.csv",
  colClasses = "character"
)



# Convert blockchain ordering variables.

adjacency_data$block_number <-
  as.integer(
    as.numeric(adjacency_data$evt_block_number)
  )


adjacency_data$event_index <-
  as.integer(adjacency_data$evt_index)



# Normalize hashes to lowercase.

adjacency_data$evt_tx_hash <-
  tolower(adjacency_data$evt_tx_hash)


wash_tx1 <- tolower(wash_tx1)
wash_tx2 <- tolower(wash_tx2)



# Sort every pool swap in true chronological order.

adjacency_data <- adjacency_data[
  order(
    adjacency_data$block_number,
    adjacency_data$event_index
  ),
]



# Find the chronological positions of the two target swaps.

position_tx1 <-
  which(
    adjacency_data$evt_tx_hash == wash_tx1
  )


position_tx2 <-
  which(
    adjacency_data$evt_tx_hash == wash_tx2
  )



# The two swaps are adjacent if:
#
#     position of transaction 2
#
# equals
#
#     position of transaction 1 + 1
#
# Example:
#
# swap position 1 → wash_tx1
# swap position 2 → wash_tx2
#
# Then adjacency = TRUE.

adjacency_condition <-
  length(position_tx1) == 1 &&
  length(position_tx2) == 1 &&
  position_tx2 == position_tx1 + 1



# ============================================================
# STEP 1D — NORMALIZE TOKEN AMOUNTS
# ============================================================

# In this validation pool:
#
# token0 = XRT
# token1 = WETH
#
# XRT has 9 decimals.
# WETH has 18 decimals.
#
# These values are hard-coded ONLY for this controlled
# validation example.
#
# The later general detector must obtain token decimals
# automatically from metadata.

XRT_DECIMALS  <- 9
WETH_DECIMALS <- 18



swaps$amount0_in <-
  as.numeric(swaps$amount0In) /
  (10 ^ XRT_DECIMALS)


swaps$amount0_out <-
  as.numeric(swaps$amount0Out) /
  (10 ^ XRT_DECIMALS)


swaps$amount1_in <-
  as.numeric(swaps$amount1In) /
  (10 ^ WETH_DECIMALS)


swaps$amount1_out <-
  as.numeric(swaps$amount1Out) /
  (10 ^ WETH_DECIMALS)



# ============================================================
# STEP 1E — CALCULATE POOL POSITION CHANGES
# ============================================================

# Positive delta:
# token entered the liquidity pool.
#
# Negative delta:
# token left the liquidity pool.


swaps$delta_XRT <-
  swaps$amount0_in -
  swaps$amount0_out


swaps$delta_WETH <-
  swaps$amount1_in -
  swaps$amount1_out



# ============================================================
# STEP 1F — TIME CONDITION
# ============================================================

# Calculate the difference between the first and last block.
#
# Expected:
#
# 10,525,621 - 10,525,617 = 4 blocks.


block_span <-
  max(swaps$block_number) -
  min(swaps$block_number)



# Paper parameter:
#
# tau = 200 blocks.

time_condition <-
  block_span <= TAU



# ============================================================
# STEP 1G — APPROXIMATE POOL RESTORATION
# ============================================================

# We measure:
#
#           absolute net position change
# ratio = --------------------------------
#             total gross movement
#
#
# If the ratio is close to zero, substantial trading occurred
# but the final position of the pool changed very little.



# ------------------------------------------------------------
# XRT
# ------------------------------------------------------------

net_XRT <-
  sum(swaps$delta_XRT)


gross_XRT <-
  sum(
    abs(swaps$delta_XRT)
  )


if (gross_XRT == 0) {

  ratio_XRT <- Inf

} else {

  ratio_XRT <-
    abs(net_XRT) /
    gross_XRT
}



# ------------------------------------------------------------
# WETH
# ------------------------------------------------------------

net_WETH <-
  sum(swaps$delta_WETH)


gross_WETH <-
  sum(
    abs(swaps$delta_WETH)
  )


if (gross_WETH == 0) {

  ratio_WETH <- Inf

} else {

  ratio_WETH <-
    abs(net_WETH) /
    gross_WETH
}



# According to the paper's coarse filtering rule,
# approximate restoration of EITHER asset can satisfy the
# position-change requirement.

restoration_condition <-
  ratio_XRT <= RHO |
  ratio_WETH <= RHO



# ============================================================
# COMPLETE STAGE 1 RESULT
# ============================================================

# For Stage 1 to pass:
#
# 1. The target swaps must be adjacent.
# 2. They must occur within tau blocks.
# 3. Their pool position must approximately restore.


stage1_pass <-
  adjacency_condition &
  time_condition &
  restoration_condition



# ============================================================
# STAGE 2 — ENTITY RECOGNITION
# ============================================================


# ============================================================
# STEP 2A — LOAD ETH-TRANSFER DATA
# ============================================================

# The two swaps have different initiating addresses.
#
# The paper therefore checks whether these addresses are
# related through the ETH-transfer graph.

eth_transfers <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_stage_2_eth_transfers.csv",
  colClasses = "character"
)



# Normalize addresses.

eth_transfers[["from"]] <-
  tolower(
    eth_transfers[["from"]]
  )


eth_transfers[["to"]] <-
  tolower(
    eth_transfers[["to"]]
  )


trader_A <- tolower(trader_A)
trader_B <- tolower(trader_B)



# ============================================================
# STEP 2B — CHECK DIRECT ETH RELATIONSHIP
# ============================================================

# A direct ETH transfer may occur:
#
# A -> B
#
# or
#
# B -> A.


direct_AB <-
  (
    eth_transfers[["from"]] == trader_A &
    eth_transfers[["to"]] == trader_B
  ) |
  (
    eth_transfers[["from"]] == trader_B &
    eth_transfers[["to"]] == trader_A
  )



number_direct_transfers <-
  sum(direct_AB)



# ============================================================
# STEP 2C — GRAPH DISTANCE
# ============================================================

# If a direct transfer exists:
#
# graph distance = 1.
#
# The paper allows:
#
# k <= 2.


if (number_direct_transfers > 0) {

  graph_distance <- 1

} else {

  graph_distance <- Inf
}



stage2_pass <-
  graph_distance <= K



# ============================================================
# STAGE 3 — MEV ELIMINATION
# ============================================================


# ============================================================
# STEP 3A — LOAD COMPLETE DEX ACTIVITY
# ============================================================

# This file contains decoded DEX trades occurring inside the
# two candidate Ethereum transactions.

dex_trades <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_stage_3_data.csv",
  colClasses = "character"
)



dex_trades$tx_hash <-
  tolower(dex_trades$tx_hash)


dex_trades$pool <-
  tolower(dex_trades$pool)



# ============================================================
# STEP 3B — MULTI-POOL ARBITRAGE CHECK
# ============================================================

# Ordinary arbitrage often routes assets through multiple
# liquidity pools.
#
# Count how many different pools appear inside each Ethereum
# transaction.


pools_per_transaction <-
  tapply(
    dex_trades$pool,
    dex_trades$tx_hash,
    function(x) {
      length(unique(x))
    }
  )



multi_pool_arbitrage <-
  any(
    pools_per_transaction > 1
  )



# ============================================================
# STEP 3C — REBASE-ARBITRAGE POSSIBILITY
# ============================================================

# The paper's rebase-token pattern involves multiple opposite
# swaps inside a single transaction.
#
# Therefore, as an initial validation check, count the number
# of DEX swaps inside each Ethereum transaction.


trades_per_transaction <-
  table(
    dex_trades$tx_hash
  )



rebase_arbitrage_possible <-
  any(
    trades_per_transaction >= 2
  )



# ============================================================
# STEP 3D — SAME-BLOCK SANDWICH POSSIBILITY
# ============================================================

# Sandwich attacks generally require the relevant transactions
# to surround another transaction in the same block.
#
# The two known wash transactions occur four blocks apart.


candidate_blocks <-
  unique(
    as.integer(
      as.numeric(
        dex_trades$block_number
      )
    )
  )



same_block_pattern <-
  length(candidate_blocks) == 1



# ============================================================
# STEP 3E — LOAD ERC-20 TRANSFER EVENTS
# ============================================================

# The paper also examines token-transfer activity to identify
# cases where assets are routed through other DEX protocols.

token_transfers <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_stage_3b_data.csv",
  colClasses = "character"
)



token_transfers[["from"]] <-
  tolower(
    token_transfers[["from"]]
  )


token_transfers[["to"]] <-
  tolower(
    token_transfers[["to"]]
  )



pool_address <-
  tolower(pool_address)


shared_execution_contract <-
  tolower(
    shared_execution_contract
  )



# ============================================================
# STEP 3F — CHECK FOR UNEXPECTED PROTOCOL ADDRESSES
# ============================================================

# In this controlled validation example, token transfers occur
# only between:
#
# 1. the XRT/WETH pool
# 2. the shared execution contract
#
# If another address were involved, we would need to inspect
# whether it belongs to another exchange/protocol.


transfer_addresses <-
  unique(
    c(
      token_transfers[["from"]],
      token_transfers[["to"]]
    )
  )



expected_addresses <-
  c(
    pool_address,
    shared_execution_contract
  )



unexpected_transfer_addresses <-
  setdiff(
    transfer_addresses,
    expected_addresses
  )



cross_protocol_activity_observed <-
  length(
    unexpected_transfer_addresses
  ) > 0



# ============================================================
# COMPLETE STAGE 3 RESULT
# ============================================================

# If any MEV exclusion condition is triggered, the suspicious
# candidate should be removed.


mev_exclusion_trigger <-
  multi_pool_arbitrage |
  rebase_arbitrage_possible |
  same_block_pattern |
  cross_protocol_activity_observed



# Stage 3 passes when the candidate is NOT removed as MEV.

stage3_pass <-
  !mev_exclusion_trigger



# ============================================================
# FINAL VALIDATION CLASSIFICATION
# ============================================================

# Known wash-trading example should survive all three stages.


final_candidate <-
  stage1_pass &
  stage2_pass &
  stage3_pass



# ============================================================
# PRINT COMPLETE RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("VALIDATION TEST 1 - COMPLETE RESULT\n")
cat("============================================\n\n")



# ------------------------------------------------------------
# STAGE 1 OUTPUT
# ------------------------------------------------------------

cat("STAGE 1 - COARSE FILTERING\n")
cat("--------------------------------------------\n")

cat(
  "Adjacent swaps:",
  adjacency_condition,
  "\n"
)

cat(
  "Block span:",
  block_span,
  "\n"
)

cat(
  "Time condition:",
  time_condition,
  "\n"
)

cat(
  "XRT restoration ratio:",
  ratio_XRT,
  "\n"
)

cat(
  "WETH restoration ratio:",
  ratio_WETH,
  "\n"
)

cat(
  "Restoration condition:",
  restoration_condition,
  "\n"
)

cat(
  "Stage 1 PASS:",
  stage1_pass,
  "\n\n"
)



# ------------------------------------------------------------
# STAGE 2 OUTPUT
# ------------------------------------------------------------

cat("STAGE 2 - ENTITY RECOGNITION\n")
cat("--------------------------------------------\n")

cat(
  "Direct ETH transfers:",
  number_direct_transfers,
  "\n"
)

cat(
  "Graph distance:",
  graph_distance,
  "\n"
)

cat(
  "Maximum allowed distance k:",
  K,
  "\n"
)

cat(
  "Stage 2 PASS:",
  stage2_pass,
  "\n\n"
)



# ------------------------------------------------------------
# STAGE 3 OUTPUT
# ------------------------------------------------------------

cat("STAGE 3 - MEV ELIMINATION\n")
cat("--------------------------------------------\n")

cat(
  "Multi-pool arbitrage detected:",
  multi_pool_arbitrage,
  "\n"
)

cat(
  "Rebase-arbitrage pattern possible:",
  rebase_arbitrage_possible,
  "\n"
)

cat(
  "Same-block sandwich pattern:",
  same_block_pattern,
  "\n"
)

cat(
  "Unexpected cross-protocol address:",
  cross_protocol_activity_observed,
  "\n"
)

cat(
  "MEV exclusion triggered:",
  mev_exclusion_trigger,
  "\n"
)

cat(
  "Stage 3 PASS:",
  stage3_pass,
  "\n\n"
)



# ------------------------------------------------------------
# FINAL CLASSIFICATION
# ------------------------------------------------------------

cat("============================================\n")
cat("FINAL CLASSIFICATION\n")
cat("============================================\n")


if (final_candidate) {

  cat(
    "\nPASS: The implementation reproduces the",
    "paper's known wash-trading candidate.\n"
  )

} else {

  cat(
    "\nFAIL: The implementation does not reproduce",
    "the expected wash-trading classification.\n"
  )
}