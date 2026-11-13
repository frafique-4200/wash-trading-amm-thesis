# ============================================================
# VALIDATION TEST 3
# Ordinary Arbitrage with Asset Conversion
#
# PURPOSE:
#
# This validation test checks the ORDINARY ARBITRAGE
# elimination rule from Stage 3 of the paper.
#
# The paper explains that ordinary arbitrage can resemble
# wash trading when transactions are examined only inside an
# individual liquidity pool.
#
# To eliminate ordinary arbitrage occurring on Uniswap, the
# authors inspect the complete transaction and determine
# whether it contains multiple swap actions involving
# DIFFERENT liquidity pools.
#
# If it does, the suspicious transaction group is removed.
#
#
# EXPECTED RESULT:
#
# Both transactions used in this validation test are explicit
# ordinary-arbitrage examples from the paper.
#
# Therefore:
#
#       Multi-pool activity = TRUE
#
#       Ordinary arbitrage detected = TRUE
#
#       Candidate should be REMOVED from wash trading.
#
# ============================================================



# ============================================================
# KNOWN ARBITRAGE TRANSACTIONS FROM THE PAPER
# ============================================================

arbitrage_tx1 <-
  "0xc63c6b32597d18b5fb8bdba6dd1d6d21fa1ade61d6242a8c8471c4aee7f16363"


arbitrage_tx2 <-
  "0xdc5cbec5c4bc6d082fe88cd59aaa21b6ba555319693418d09a6b3c31400fb92f"



# ============================================================
# STEP 1 — LOAD COMPLETE DEX ACTIVITY
# ============================================================

# The CSV was obtained from Dune.
#
# It contains every decoded DEX trade belonging to the two
# ordinary-arbitrage transactions identified in the paper.
#
# We initially load everything as character so Ethereum
# addresses and transaction hashes cannot be modified by R.

trades <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_3_arbitrage_dex_trades.csv",
  colClasses = "character"
)



# ============================================================
# STEP 2 — NORMALIZE IMPORTANT VARIABLES
# ============================================================

# Normalize hexadecimal values to lowercase.

trades$tx_hash <-
  tolower(trades$tx_hash)


trades$tx_from <-
  tolower(trades$tx_from)


trades$pool <-
  tolower(trades$pool)



arbitrage_tx1 <-
  tolower(arbitrage_tx1)


arbitrage_tx2 <-
  tolower(arbitrage_tx2)



# Convert blockchain ordering variables to numeric form.

trades$block_number <-
  as.integer(
    as.numeric(trades$block_number)
  )


trades$evt_index <-
  as.integer(trades$evt_index)



# Convert decoded token quantities.

trades$token_sold_amount <-
  as.numeric(trades$token_sold_amount)


trades$token_bought_amount <-
  as.numeric(trades$token_bought_amount)



# ============================================================
# STEP 3 — SORT SWAPS IN BLOCKCHAIN ORDER
# ============================================================

# Inside a transaction, evt_index tells us the order in which
# the swap events occurred.

trades <- trades[
  order(
    trades$block_number,
    trades$tx_hash,
    trades$evt_index
  ),
]



# ============================================================
# STEP 4 — EXTRACT THE TWO PAPER TRANSACTIONS
# ============================================================

tx1 <-
  trades[
    trades$tx_hash == arbitrage_tx1,
  ]


tx2 <-
  trades[
    trades$tx_hash == arbitrage_tx2,
  ]



# Confirm that both expected transactions were actually found.

tx1_found <-
  nrow(tx1) > 0


tx2_found <-
  nrow(tx2) > 0



# ============================================================
# STEP 5 — COUNT SWAP ACTIONS
# ============================================================

# Each row in dex.trades represents a decoded swap action.
#
# The transactions in this example are expected to contain
# three swaps each.

tx1_number_swaps <-
  nrow(tx1)


tx2_number_swaps <-
  nrow(tx2)



# ============================================================
# STEP 6 — COUNT UNIQUE LIQUIDITY POOLS
# ============================================================

# This is the central test for the paper's ordinary-arbitrage
# elimination rule.
#
# Wash trading normally involves reversing trades in the SAME
# liquidity pool.
#
# Ordinary arbitrage instead routes assets through several
# pools.
#
# Therefore, if the complete Ethereum transaction contains
# swap actions from more than one liquidity pool, the paper
# treats that as evidence for ordinary arbitrage.

tx1_unique_pools <-
  length(
    unique(tx1$pool)
  )


tx2_unique_pools <-
  length(
    unique(tx2$pool)
  )



# More than one pool means multi-pool activity.

tx1_multi_pool <-
  tx1_unique_pools > 1


tx2_multi_pool <-
  tx2_unique_pools > 1



# ============================================================
# STEP 7 — RECONSTRUCT TOKEN-CONVERSION ROUTES
# ============================================================

# This step is mainly for interpretation.
#
# It is NOT an additional condition invented for the paper's
# ordinary-arbitrage filter.
#
# It simply lets us see the economic route taken by each
# transaction.
#
# For example:
#
# OCEAN -> ALBT -> WETH -> OCEAN
#
# demonstrates a triangular arbitrage cycle.


build_route <- function(transaction_data) {

  # Make sure the transaction is ordered by event index.

  transaction_data <-
    transaction_data[
      order(transaction_data$evt_index),
    ]


  # Start with the token sold in the first swap.

  route <-
    transaction_data$token_sold_symbol[1]


  # Then append the token bought by each consecutive swap.

  route <-
    c(
      route,
      transaction_data$token_bought_symbol
    )


  paste(
    route,
    collapse = " -> "
  )
}



tx1_route <-
  build_route(tx1)


tx2_route <-
  build_route(tx2)



# ============================================================
# STEP 8 — CHECK WHETHER THE ROUTE CLOSES
# ============================================================

# A triangular arbitrage transaction often starts and ends
# with the same asset.
#
# Example:
#
# OCEAN -> ALBT -> WETH -> OCEAN
#
# This is useful supporting evidence.
#
# IMPORTANT:
# This is NOT required by the paper's stated multi-pool
# exclusion rule. The central paper criterion remains the
# presence of multiple swap actions across different pools.


tx1_start_asset <-
  tx1$token_sold_symbol[
    which.min(tx1$evt_index)
  ]


tx1_end_asset <-
  tx1$token_bought_symbol[
    which.max(tx1$evt_index)
  ]


tx2_start_asset <-
  tx2$token_sold_symbol[
    which.min(tx2$evt_index)
  ]


tx2_end_asset <-
  tx2$token_bought_symbol[
    which.max(tx2$evt_index)
  ]



tx1_closed_cycle <-
  tx1_start_asset == tx1_end_asset


tx2_closed_cycle <-
  tx2_start_asset == tx2_end_asset



# ============================================================
# STEP 9 — CHECK THE TRANSACTION INITIATORS
# ============================================================

# This is included for interpretation only.
#
# Both arbitrage transactions in the Dune data are initiated
# by the same Ethereum address.

tx1_initiators <-
  unique(tx1$tx_from)


tx2_initiators <-
  unique(tx2$tx_from)



same_initiator <-
  length(tx1_initiators) == 1 &&
  length(tx2_initiators) == 1 &&
  tx1_initiators == tx2_initiators



# ============================================================
# STEP 10 — APPLY ORDINARY-ARBITRAGE EXCLUSION RULE
# ============================================================

# The paper's ordinary-arbitrage rule is triggered because
# the complete transaction contains swaps involving different
# liquidity pools.
#
# For this validation example we want BOTH paper transactions
# to demonstrate the expected multi-pool behavior.


ordinary_arbitrage_tx1 <-
  tx1_multi_pool


ordinary_arbitrage_tx2 <-
  tx2_multi_pool



# If either candidate transaction exhibits multi-pool activity,
# the ordinary-arbitrage exclusion mechanism would be triggered.

ordinary_arbitrage_detected <-
  ordinary_arbitrage_tx1 |
  ordinary_arbitrage_tx2



# For the VALIDATION TEST specifically, both examples supplied
# by the paper should satisfy the rule.

validation_test_pass <-
  tx1_found &&
  tx2_found &&
  ordinary_arbitrage_tx1 &&
  ordinary_arbitrage_tx2



# ============================================================
# FINAL WASH-TRADING DECISION
# ============================================================

# Stage 3 should REMOVE these candidates because they have
# been identified as ordinary arbitrage.

removed_by_mev_filter <-
  ordinary_arbitrage_detected



# Therefore they should NOT remain classified as wash trading.

final_wash_candidate <-
  !removed_by_mev_filter



# ============================================================
# PRINT COMPLETE RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("VALIDATION TEST 3 - COMPLETE RESULT\n")
cat("============================================\n\n")



# ------------------------------------------------------------
# DATA CHECK
# ------------------------------------------------------------

cat("DATA CHECK\n")
cat("--------------------------------------------\n")

cat(
  "Total DEX swaps loaded:",
  nrow(trades),
  "\n"
)

cat(
  "Transaction 1 found:",
  tx1_found,
  "\n"
)

cat(
  "Transaction 2 found:",
  tx2_found,
  "\n\n"
)



# ------------------------------------------------------------
# TRANSACTION 1
# ------------------------------------------------------------

cat("ARBITRAGE TRANSACTION 1\n")
cat("--------------------------------------------\n")

cat(
  "Number of swaps:",
  tx1_number_swaps,
  "\n"
)

cat(
  "Number of unique pools:",
  tx1_unique_pools,
  "\n"
)

cat(
  "Multi-pool activity:",
  tx1_multi_pool,
  "\n"
)

cat(
  "Token route:",
  tx1_route,
  "\n"
)

cat(
  "Closed asset cycle:",
  tx1_closed_cycle,
  "\n\n"
)



# ------------------------------------------------------------
# TRANSACTION 2
# ------------------------------------------------------------

cat("ARBITRAGE TRANSACTION 2\n")
cat("--------------------------------------------\n")

cat(
  "Number of swaps:",
  tx2_number_swaps,
  "\n"
)

cat(
  "Number of unique pools:",
  tx2_unique_pools,
  "\n"
)

cat(
  "Multi-pool activity:",
  tx2_multi_pool,
  "\n"
)

cat(
  "Token route:",
  tx2_route,
  "\n"
)

cat(
  "Closed asset cycle:",
  tx2_closed_cycle,
  "\n\n"
)



# ------------------------------------------------------------
# ENTITY INFORMATION
# ------------------------------------------------------------

cat("TRANSACTION INITIATOR\n")
cat("--------------------------------------------\n")

cat(
  "Transaction 1 initiator:",
  tx1_initiators,
  "\n"
)

cat(
  "Transaction 2 initiator:",
  tx2_initiators,
  "\n"
)

cat(
  "Same initiator:",
  same_initiator,
  "\n\n"
)



# ------------------------------------------------------------
# STAGE 3 ORDINARY-ARBITRAGE RESULT
# ------------------------------------------------------------

cat("STAGE 3 - ORDINARY ARBITRAGE ELIMINATION\n")
cat("--------------------------------------------\n")

cat(
  "Transaction 1 identified as multi-pool arbitrage:",
  ordinary_arbitrage_tx1,
  "\n"
)

cat(
  "Transaction 2 identified as multi-pool arbitrage:",
  ordinary_arbitrage_tx2,
  "\n"
)

cat(
  "Ordinary arbitrage detected:",
  ordinary_arbitrage_detected,
  "\n"
)

cat(
  "Removed by MEV filter:",
  removed_by_mev_filter,
  "\n"
)

cat(
  "Remains a wash-trading candidate:",
  final_wash_candidate,
  "\n\n"
)



# ============================================================
# FINAL VALIDATION CLASSIFICATION
# ============================================================

cat("============================================\n")
cat("FINAL CLASSIFICATION\n")
cat("============================================\n")


if (validation_test_pass &&
    removed_by_mev_filter &&
    !final_wash_candidate) {

  cat(
    "\nPASS: Both known ordinary-arbitrage transactions",
    "contain swaps across multiple liquidity pools and",
    "are correctly removed by the paper's MEV filter.\n"
  )

} else {

  cat(
    "\nFAIL: The implementation did not reproduce",
    "the expected ordinary-arbitrage exclusion.\n"
  )
}