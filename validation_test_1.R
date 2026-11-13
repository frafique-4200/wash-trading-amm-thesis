# ============================================================
# VALIDATION TEST 1
# Known positive wash-trading example from the AMM paper
#
# PURPOSE:
# The paper explicitly describes these two Uniswap V2 swaps
# as part of a wash-trading example.
#
# Therefore, before applying our implementation to unknown
# data, we test whether our coarse-filtering code identifies
# this known example as suspicious.
# ============================================================


# ------------------------------------------------------------
# STEP 1: Load the raw Dune swap-event data
#
# WHY:
# Ethereum addresses and transaction hashes are very large
# hexadecimal strings. We therefore load every column as text
# first so that R does not accidentally convert them.
# ------------------------------------------------------------

swaps <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_data.csv",
  colClasses = "character"
)


# ------------------------------------------------------------
# STEP 2: Check that exactly two swaps were loaded
#
# EXPECTED RESULT:
# The Dune query returned the two transactions explicitly
# discussed in the paper.
# ------------------------------------------------------------

cat("Number of swaps loaded:", nrow(swaps), "\n")


# ------------------------------------------------------------
# STEP 3: Convert blockchain-ordering variables to numbers
#
# WHY:
# The swaps must be analysed in blockchain order.
#
# Dune exported the block numbers using scientific notation,
# for example:
#       1.0525617e+07
# which equals:
#       10,525,617
# ------------------------------------------------------------

swaps$block_number <- as.integer(as.numeric(swaps$evt_block_number))
swaps$event_index  <- as.integer(swaps$evt_index)


# ------------------------------------------------------------
# STEP 4: Put the swaps in blockchain order
#
# WHY:
# The paper searches consecutive/adjacent swaps.
# Therefore the transaction sequence must be correctly ordered.
# ------------------------------------------------------------

swaps <- swaps[
  order(swaps$block_number, swaps$event_index),
]


# ------------------------------------------------------------
# STEP 5: Convert the raw token amounts into normal token units
#
# In this Uniswap V2 pool:
#
# token0 = XRT
# token1 = WETH
#
# XRT uses 9 decimal places.
# WETH uses 18 decimal places.
#
# Example:
# 67,496,972,896 raw XRT units
# becomes
# 67.496972896 XRT.
# ------------------------------------------------------------

XRT_DECIMALS  <- 9
WETH_DECIMALS <- 18

swaps$amount0_in  <- as.numeric(swaps$amount0In)  / (10^XRT_DECIMALS)
swaps$amount0_out <- as.numeric(swaps$amount0Out) / (10^XRT_DECIMALS)

swaps$amount1_in  <- as.numeric(swaps$amount1In)  / (10^WETH_DECIMALS)
swaps$amount1_out <- as.numeric(swaps$amount1Out) / (10^WETH_DECIMALS)


# ------------------------------------------------------------
# STEP 6: Calculate the change in the liquidity pool
#
# Formula:
#
#     pool change = amount entering pool - amount leaving pool
#
# A positive value means the pool gained the token.
# A negative value means the pool lost the token.
# ------------------------------------------------------------

swaps$delta_XRT <-
  swaps$amount0_in - swaps$amount0_out

swaps$delta_WETH <-
  swaps$amount1_in - swaps$amount1_out


# ------------------------------------------------------------
# STEP 7: Display what happened in the two swaps
#
# EXPECTED BEHAVIOUR FROM THE PAPER:
#
# Swap 1:
#   approximately 0.740 WETH enters the pool
#   approximately 67.497 XRT leaves the pool
#
# Swap 2:
#   approximately 67.497 XRT enters the pool
#   approximately 0.736 WETH leaves the pool
# ------------------------------------------------------------

print(
  swaps[, c(
    "block_number",
    "evt_tx_from",
    "delta_XRT",
    "delta_WETH"
  )]
)


# ------------------------------------------------------------
# STEP 8: Calculate the block interval
#
# The paper requires suspicious swaps to occur within a
# short period.
#
# Parameter used by the paper:
#
#     tau = 200 blocks
#
# These two transactions are expected to be only 4 blocks apart.
# ------------------------------------------------------------

block_span <-
  max(swaps$block_number) - min(swaps$block_number)

cat("\nBlock span:", block_span, "blocks\n")


# ------------------------------------------------------------
# STEP 9: Calculate the AMM pool-restoration ratio
#
# IDEA:
#
# A wash-trading group should move tokens through the pool
# but leave the pool approximately where it started.
#
# For each token:
#
#     restoration ratio =
#
#        absolute net position change
#        ----------------------------
#        total token movement
#
# A ratio close to zero means:
#
#     "A lot moved, but very little remained changed."
#
# The paper uses:
#
#     rho = 5% = 0.05
#
# The paper states that the condition is satisfied if EITHER
# asset satisfies the position-change criterion.
# ------------------------------------------------------------

net_XRT   <- sum(swaps$delta_XRT)
gross_XRT <- sum(abs(swaps$delta_XRT))

net_WETH   <- sum(swaps$delta_WETH)
gross_WETH <- sum(abs(swaps$delta_WETH))


ratio_XRT <-
  abs(net_XRT) / gross_XRT

ratio_WETH <-
  abs(net_WETH) / gross_WETH


# ------------------------------------------------------------
# STEP 10: Print the calculated restoration results
# ------------------------------------------------------------

cat("\n--- Pool Restoration Results ---\n")

cat(
  "XRT net change:",
  net_XRT,
  "\n"
)

cat(
  "XRT total movement:",
  gross_XRT,
  "\n"
)

cat(
  "XRT restoration ratio:",
  ratio_XRT,
  "\n\n"
)

cat(
  "WETH net change:",
  net_WETH,
  "\n"
)

cat(
  "WETH total movement:",
  gross_WETH,
  "\n"
)

cat(
  "WETH restoration ratio:",
  ratio_WETH,
  "\n"
)


# ------------------------------------------------------------
# STEP 11: Apply the paper's coarse-filtering thresholds
#
# Conditions:
#
# 1. Time condition:
#       block span <= 200
#
# 2. Pool restoration:
#       XRT ratio <= 5%
#                OR
#       WETH ratio <= 5%
# ------------------------------------------------------------

TAU <- 200
RHO <- 0.05

time_condition <-
  block_span <= TAU

restoration_condition <-
  ratio_XRT <= RHO |
  ratio_WETH <= RHO

coarse_filter_pass <-
  time_condition & restoration_condition


# ------------------------------------------------------------
# STEP 12: Report the final result
#
# EXPECTED RESULT FOR THIS VALIDATION CASE:
#
#     PASS
#
# because the paper explicitly presents these transactions as
# part of its wash-trading example.
#
# IMPORTANT:
# PASS here does NOT yet mean "confirmed wash trading."
#
# It means:
#     "This transaction group survives Stage 1:
#      Coarse Filtering."
#
# Entity recognition and MEV elimination still follow.
# ------------------------------------------------------------

cat("\n============================================\n")
cat("VALIDATION TEST 1 RESULT\n")
cat("============================================\n")

cat("Time condition:", time_condition, "\n")
cat("Restoration condition:", restoration_condition, "\n")
cat("Coarse-filter result:", coarse_filter_pass, "\n")

if (coarse_filter_pass) {

  cat(
    "\nPASS: The known paper example is detected as a",
    "coarse-filtering candidate.\n"
  )

} else {

  cat(
    "\nFAIL: The implementation did not reproduce the",
    "expected coarse-filtering behaviour.\n"
  )
}