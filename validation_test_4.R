# ============================================================
# VALIDATION TEST 4
# Rebase-Token Arbitrage Exclusion
#
# PURPOSE:
#
# This validation test checks ONLY the rebase-token arbitrage
# exclusion rule described in the reference paper.
#
# Flash-loan detection is NOT part of this study and is not
# implemented here.
#
# The paper states that a transaction should be identified as
# rebase-token arbitrage when:
#
#   1. Two opposite swaps occur in one transaction.
#   2. Both swaps occur in the same liquidity pool.
#   3. The amount of one asset is the same in both swaps.
#   4. The amount of the other asset returned in the second
#      swap is greater than the amount used in the first.
#
# EXPECTED RESULT:
#
#     Rebase arbitrage detected = TRUE
#
#     Candidate removed from wash-trading results.
#
# ============================================================



# ============================================================
# KNOWN TRANSACTION FROM THE PAPER
# ============================================================

rebase_tx <-
  "0x8a5029d6b29f749cd04963f27c7ddc3e4f990a2aa2fc4e70d89c4e890b14fe2b"



# ============================================================
# STEP 1 — LOAD RAW UNISWAP V2 SWAP EVENTS
# ============================================================

# We use the raw Uniswap V2 Swap-event table rather than
# Dune's aggregated dex.trades table because dex.trades did
# not return this historical transaction.
#
# All columns are initially loaded as character.
#
# This is especially important for token amounts because the
# raw integer quantities are too large to represent exactly
# with ordinary floating-point numbers.

swaps <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_4_rebase_arbitrage.csv",
  colClasses = "character"
)



# ============================================================
# STEP 2 — NORMALIZE IDENTIFIERS
# ============================================================

swaps$evt_tx_hash <-
  tolower(swaps$evt_tx_hash)


swaps$pool <-
  tolower(swaps$pool)


rebase_tx <-
  tolower(rebase_tx)



# Convert event index for chronological ordering.

swaps$evt_index <-
  as.integer(swaps$evt_index)



# ============================================================
# STEP 3 — RETAIN THE PAPER'S TRANSACTION
# ============================================================

tx_swaps <-
  swaps[
    swaps$evt_tx_hash == rebase_tx,
  ]


transaction_found <-
  nrow(tx_swaps) > 0



# ============================================================
# STEP 4 — SORT THE SWAPS CHRONOLOGICALLY
# ============================================================

tx_swaps <-
  tx_swaps[
    order(tx_swaps$evt_index),
  ]



# ============================================================
# STEP 5 — CHECK THAT EXACTLY TWO SWAPS OCCUR
# ============================================================

number_of_swaps <-
  nrow(tx_swaps)


two_swaps <-
  number_of_swaps == 2



# ============================================================
# STEP 6 — CHECK SAME TRANSACTION
# ============================================================

# Both Swap events must belong to one Ethereum transaction.

same_transaction <-
  length(
    unique(tx_swaps$evt_tx_hash)
  ) == 1



# ============================================================
# STEP 7 — CHECK SAME LIQUIDITY POOL
# ============================================================

# The paper's rebase-arbitrage condition concerns two opposite
# swaps occurring in the same AMM pool.

same_pool <-
  length(
    unique(tx_swaps$pool)
  ) == 1



# ============================================================
# STEP 8 — CHECK OPPOSITE SWAP DIRECTIONS
# ============================================================

# Swap 1 has:
#
#     token0 IN
#     token1 OUT
#
# Swap 2 has:
#
#     token1 IN
#     token0 OUT
#
# Therefore the two swaps have opposite directions.


swap1 <-
  tx_swaps[1, ]


swap2 <-
  tx_swaps[2, ]



# Because raw values are stored as character strings,
# compare against "0" without converting the large integers.

swap1_direction <-
  swap1$amount0In != "0" &&
  swap1$amount1Out != "0" &&
  swap1$amount0Out == "0" &&
  swap1$amount1In == "0"


swap2_direction <-
  swap2$amount1In != "0" &&
  swap2$amount0Out != "0" &&
  swap2$amount1Out == "0" &&
  swap2$amount0In == "0"



opposite_directions <-
  swap1_direction &&
  swap2_direction



# ============================================================
# STEP 9 — CHECK EXACT POSITION MATCH FOR TOKEN1
# ============================================================

# The first swap receives token1:
#
#     amount1Out
#
# The second swap sends token1 back:
#
#     amount1In
#
# The paper requires the amount of this asset to be the same.
#
# Because these are raw blockchain integers, we compare the
# CHARACTER STRINGS directly.
#
# This avoids loss of numerical precision.


token1_first_out <-
  swap1$amount1Out


token1_second_in <-
  swap2$amount1In


same_token1_amount <-
  token1_first_out ==
  token1_second_in



# ============================================================
# STEP 10 — COMPARE TOKEN0 RETURN AMOUNTS
# ============================================================

# We now need to determine whether:
#
# token0 received in Swap 2
#
#          >
#
# token0 used in Swap 1
#
#
# The raw values are extremely large integers.
#
# Ordinary R numeric values cannot preserve every digit
# exactly.
#
# Therefore we compare the integer strings directly.



# ------------------------------------------------------------
# Helper function for comparing two positive integer strings.
#
# Returns TRUE when integer a > integer b.
# ------------------------------------------------------------

integer_string_greater <- function(a, b) {

  # Remove leading zeros.

  a <- sub("^0+", "", a)
  b <- sub("^0+", "", b)


  # Protect the special case of zero.

  if (a == "") {
    a <- "0"
  }

  if (b == "") {
    b <- "0"
  }


  # A number containing more digits is larger.

  if (nchar(a) > nchar(b)) {

    return(TRUE)

  }


  if (nchar(a) < nchar(b)) {

    return(FALSE)

  }


  # If they have the same number of digits,
  # lexicographic comparison gives the correct result.

  return(a > b)
}



token0_first_in <-
  swap1$amount0In


token0_second_out <-
  swap2$amount0Out



token0_return_greater <-
  integer_string_greater(
    token0_second_out,
    token0_first_in
  )



# ============================================================
# STEP 11 — HUMAN-READABLE VALUES
# ============================================================

# This section is ONLY for interpretation and printing.
#
# The actual logical tests above used the original exact raw
# integers.
#
# Both tokens in this particular pool use 18 decimals.
#
# Conversion to ordinary R numeric values here is acceptable
# because these values are not used for exact equality tests.

TOKEN0_DECIMALS <- 18
TOKEN1_DECIMALS <- 18


token0_first_in_readable <-
  as.numeric(token0_first_in) /
  (10 ^ TOKEN0_DECIMALS)


token0_second_out_readable <-
  as.numeric(token0_second_out) /
  (10 ^ TOKEN0_DECIMALS)


token1_first_out_readable <-
  as.numeric(token1_first_out) /
  (10 ^ TOKEN1_DECIMALS)


token1_second_in_readable <-
  as.numeric(token1_second_in) /
  (10 ^ TOKEN1_DECIMALS)



# Approximate gain in token0.
#
# Again, this is diagnostic information only.

token0_difference <-
  token0_second_out_readable -
  token0_first_in_readable



# ============================================================
# STEP 12 — APPLY REBASE-ARBITRAGE RULE
# ============================================================

rebase_arbitrage_detected <-
  transaction_found &&
  two_swaps &&
  same_transaction &&
  same_pool &&
  opposite_directions &&
  same_token1_amount &&
  token0_return_greater



# ============================================================
# STEP 13 — MEV EXCLUSION
# ============================================================

# If the pattern is identified as rebase arbitrage, it should
# be removed from the wash-trading candidates.

removed_by_mev_filter <-
  rebase_arbitrage_detected



# Therefore it should NOT remain classified as wash trading.

final_wash_candidate <-
  !removed_by_mev_filter



# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("VALIDATION TEST 4 - COMPLETE RESULT\n")
cat("============================================\n\n")



cat("DATA CHECK\n")
cat("--------------------------------------------\n")

cat(
  "Transaction found:",
  transaction_found,
  "\n"
)

cat(
  "Number of swaps:",
  number_of_swaps,
  "\n"
)

cat(
  "Exactly two swaps:",
  two_swaps,
  "\n\n"
)



cat("STRUCTURAL CONDITIONS\n")
cat("--------------------------------------------\n")

cat(
  "Same transaction:",
  same_transaction,
  "\n"
)

cat(
  "Same liquidity pool:",
  same_pool,
  "\n"
)

cat(
  "Opposite swap directions:",
  opposite_directions,
  "\n\n"
)



cat("ASSET-MATCH CONDITIONS\n")
cat("--------------------------------------------\n")

cat(
  "Token1 amount from Swap 1:",
  token1_first_out_readable,
  "\n"
)

cat(
  "Token1 amount into Swap 2:",
  token1_second_in_readable,
  "\n"
)

cat(
  "Same Token1 amount:",
  same_token1_amount,
  "\n\n"
)


cat(
  "Token0 amount into Swap 1:",
  token0_first_in_readable,
  "\n"
)

cat(
  "Token0 amount out of Swap 2:",
  token0_second_out_readable,
  "\n"
)

cat(
  "Token0 returned is greater:",
  token0_return_greater,
  "\n"
)

cat(
  "Approximate Token0 difference:",
  token0_difference,
  "\n\n"
)



cat("STAGE 3 - REBASE ARBITRAGE ELIMINATION\n")
cat("--------------------------------------------\n")

cat(
  "Rebase arbitrage detected:",
  rebase_arbitrage_detected,
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


if (
  rebase_arbitrage_detected &&
  removed_by_mev_filter &&
  !final_wash_candidate
) {

  cat(
    "\nPASS: The known rebase-arbitrage transaction",
    "satisfies the paper's exclusion rule and is",
    "correctly removed from the wash-trading candidates.\n"
  )

} else {

  cat(
    "\nFAIL: The implementation did not reproduce",
    "the expected rebase-arbitrage exclusion.\n"
  )
}