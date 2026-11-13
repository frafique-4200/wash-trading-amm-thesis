# ============================================================
# VALIDATION TEST 1 - STAGE 2: ENTITY RECOGNITION
#
# PURPOSE:
# Stage 1 showed that the two XRT/WETH swaps satisfy the
# paper's coarse-filtering conditions.
#
# The two swaps were initiated by DIFFERENT Ethereum addresses.
# Therefore, Stage 2 asks whether those addresses are related.
#
# The paper reports direct ETH transfers between them.
# We test whether our data reproduces that relationship.
# ============================================================


# ------------------------------------------------------------
# STEP 1: Define the two swap initiators
#
# These are the evt_tx_from addresses from the two swaps used
# in Validation Test 1.
# ------------------------------------------------------------

trader_A <- "0x5c2bc88f19674fbf71bcede54f88e6b05ede40d7"

trader_B <- "0x3a7f85b30678b27668b5b65a6519bfe818235467"


# ------------------------------------------------------------
# STEP 2: Load the ETH-transfer data downloaded from Dune
#
# We again keep columns as text initially so that Ethereum
# addresses and transaction hashes cannot be altered by R.
# ------------------------------------------------------------

eth_transfers <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_1_stage_2_eth_transfers.csv",
  colClasses = "character"
)


# ------------------------------------------------------------
# STEP 3: Check how many ETH-transfer transactions were loaded
#
# EXPECTED RESULT:
# 3 transactions, because the paper gives three direct-transfer
# examples between these two addresses.
# ------------------------------------------------------------

cat("Number of ETH transfers loaded:",
    nrow(eth_transfers),
    "\n")


# ------------------------------------------------------------
# STEP 4: Normalize address capitalization
#
# Ethereum addresses are hexadecimal strings.
#
# Address comparisons should not fail merely because one source
# uses uppercase letters and another uses lowercase letters.
# ------------------------------------------------------------

eth_transfers[["from"]] <- tolower(eth_transfers[["from"]])
eth_transfers[["to"]]   <- tolower(eth_transfers[["to"]])

trader_A <- tolower(trader_A)
trader_B <- tolower(trader_B)


# ------------------------------------------------------------
# STEP 5: Identify direct transfers between Trader A and B
#
# We accept either direction:
#
#     A -> B
#
# or
#
#     B -> A
#
# A direct transfer means the graph distance between the
# addresses is 1 hop.
# ------------------------------------------------------------

direct_AB <-
  (
    eth_transfers[["from"]] == trader_A &
    eth_transfers[["to"]]   == trader_B
  ) |
  (
    eth_transfers[["from"]] == trader_B &
    eth_transfers[["to"]]   == trader_A
  )


# ------------------------------------------------------------
# STEP 6: Count the direct connections
# ------------------------------------------------------------

number_direct_transfers <- sum(direct_AB)

cat(
  "Direct transfers between the two addresses:",
  number_direct_transfers,
  "\n"
)


# ------------------------------------------------------------
# STEP 7: Display the direct-transfer evidence
#
# This allows us to inspect the actual transactions rather than
# relying only on a TRUE/FALSE result.
# ------------------------------------------------------------

print(
  eth_transfers[
    direct_AB,
    c(
      "hash",
      "block_number",
      "from",
      "to",
      "eth_amount"
    )
  ]
)


# ------------------------------------------------------------
# STEP 8: Apply the paper's entity-recognition rule
#
# The paper uses a maximum graph distance:
#
#     k = 2
#
# Because these two addresses have direct ETH transfers,
#
#     distance(A,B) = 1
#
# and:
#
#     1 <= 2
#
# so they satisfy the entity-linkage criterion.
# ------------------------------------------------------------

K <- 2

if (number_direct_transfers > 0) {

  graph_distance <- 1

} else {

  graph_distance <- Inf
}


entity_condition <-
  graph_distance <= K


# ------------------------------------------------------------
# STEP 9: Report the validation result
#
# EXPECTED RESULT:
#
#     PASS
#
# IMPORTANT:
# This does NOT mathematically prove the two addresses belong
# to the same human being.
#
# It means that, under the entity-recognition heuristic used
# in the paper, the two addresses are treated as related.
# ------------------------------------------------------------

cat("\n============================================\n")
cat("VALIDATION TEST 1 - STAGE 2 RESULT\n")
cat("============================================\n")

cat("Graph distance:", graph_distance, "\n")
cat("Maximum allowed distance k:", K, "\n")
cat("Entity condition:", entity_condition, "\n")

if (entity_condition) {

  cat(
    "\nPASS: The two swap initiators satisfy the",
    "paper's entity-recognition criterion.\n"
  )

} else {

  cat(
    "\nFAIL: The expected entity relationship was",
    "not reproduced.\n"
  )
}