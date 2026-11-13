# ============================================================
# VALIDATION TEST 2
# Known sandwich-attack false positive
#
# PURPOSE:
#
# Validation Test 1 checked whether our implementation can
# KEEP a known wash trade.
#
# Validation Test 2 checks the opposite:
#
# Can the implementation REMOVE an MEV transaction that
# superficially resembles wash trading?
#
# This example is explicitly presented in the paper as a
# sandwich attack with asset conversion.
# ============================================================


# ============================================================
# PARAMETERS
# ============================================================

TAU <- 200
RHO <- 0.05


# Transaction hashes from the paper.

attacker_tx1 <-
  "0xbd72e0fb6ad4472ece37aa0c07a244634e98156c03f049808f8e5f6fc1740f9e"

victim_tx <-
  "0x7e90e7db1c4f6e32e2c1e5a8bcc914da84420dac9e07679a2f15cdcde30ffa7f"

attacker_tx2 <-
  "0x6d39098090fcc340cee00a1b518ca7e5d3766711124a084a5f46d6595e0c948b"


# Relevant pools.

WETH_USDT_POOL <-
  "0x0d4a11d5eeaac28ec3f61d100daf4d40471f1852"

NGC_USDT_POOL <-
  "0x5e4a92625a337707ba362abff463bccc5b896d21"


# ============================================================
# LOAD DATA
# ============================================================

trades <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/validation_test_2_sandwich_dex_trades.csv",
  colClasses = "character"
)


# Normalize addresses/hashes.

trades$tx_hash <- tolower(trades$tx_hash)
trades$tx_from <- tolower(trades$tx_from)
trades$pool    <- tolower(trades$pool)

trades$evt_index <-
  as.integer(trades$evt_index)

trades$block_number <-
  as.integer(as.numeric(trades$block_number))

trades$token_sold_amount <-
  as.numeric(trades$token_sold_amount)

trades$token_bought_amount <-
  as.numeric(trades$token_bought_amount)


# ============================================================
# STAGE 1 — COARSE FILTERING
# ============================================================

# We intentionally examine ONLY the attacker's two swaps in
# the WETH/USDT pool.
#
# This demonstrates why a pool-level detector could mistake
# the sandwich attack for wash trading.


weth_usdt <- trades[
  trades$pool == WETH_USDT_POOL &
    trades$tx_hash %in% c(attacker_tx1, attacker_tx2),
]


# ------------------------------------------------------------
# Find transaction 1:
#
# WETH -> USDT
# ------------------------------------------------------------

trade1 <- weth_usdt[
  weth_usdt$tx_hash == attacker_tx1,
]


# ------------------------------------------------------------
# Find transaction 2:
#
# USDT -> WETH
# ------------------------------------------------------------

trade2 <- weth_usdt[
  weth_usdt$tx_hash == attacker_tx2,
]


# Extract amounts.

weth_in <-
  trade1$token_sold_amount[
    trade1$token_sold_symbol == "WETH"
  ]

usdt_out <-
  trade1$token_bought_amount[
    trade1$token_bought_symbol == "USDT"
  ]


usdt_in <-
  trade2$token_sold_amount[
    trade2$token_sold_symbol == "USDT"
  ]

weth_out <-
  trade2$token_bought_amount[
    trade2$token_bought_symbol == "WETH"
  ]


# ------------------------------------------------------------
# Pool balance changes.
#
# Transaction 1:
#
# pool receives WETH  -> positive
# pool sends USDT     -> negative
#
# Transaction 2:
#
# pool receives USDT  -> positive
# pool sends WETH     -> negative
# ------------------------------------------------------------

delta_WETH <-
  c(
    +weth_in,
    -weth_out
  )

delta_USDT <-
  c(
    -usdt_out,
    +usdt_in
  )


# ------------------------------------------------------------
# Calculate pool-restoration ratios.
# ------------------------------------------------------------

ratio_WETH <-
  abs(sum(delta_WETH)) /
  sum(abs(delta_WETH))

ratio_USDT <-
  abs(sum(delta_USDT)) /
  sum(abs(delta_USDT))


# Both transactions occur in the same block.

block_span <-
  max(weth_usdt$block_number) -
  min(weth_usdt$block_number)


time_condition <-
  block_span <= TAU


# As in Validation Test 1, the paper requires either asset
# to satisfy the restoration condition.

restoration_condition <-
  ratio_WETH <= RHO |
  ratio_USDT <= RHO


stage1_pass <-
  time_condition &
  restoration_condition



# ============================================================
# STAGE 2 — ENTITY RECOGNITION
# ============================================================

# Both attack transactions have the SAME initiating address.
#
# According to the paper, if a suspicious group has only one
# initiating address, it obviously belongs to one entity.
#
# Therefore no ETH-transfer graph search is necessary here.


initiator_tx1 <-
  unique(
    trades$tx_from[
      trades$tx_hash == attacker_tx1
    ]
  )

initiator_tx2 <-
  unique(
    trades$tx_from[
      trades$tx_hash == attacker_tx2
    ]
  )


same_initiator <-
  length(initiator_tx1) == 1 &&
  length(initiator_tx2) == 1 &&
  initiator_tx1 == initiator_tx2


stage2_pass <- same_initiator



# ============================================================
# STAGE 3 — SANDWICH ATTACK DETECTION
# ============================================================

# The paper gives THREE conditions for identifying the
# sandwich pattern.
#
# CONDITION A:
# Transactions occur sequentially in the same block.
#
# CONDITION B:
# Attacker transaction 1 has the same trading direction
# as the victim, while attacker transaction 2 has the
# opposite direction.
#
# CONDITION C:
# The amount sold by attacker transaction 2 is between
# 90% and 110% of the amount bought by attacker
# transaction 1.
# ============================================================



# ------------------------------------------------------------
# CONDITION A
# Same block and correct ordering.
# ------------------------------------------------------------

block_attacker1 <-
  unique(
    trades$block_number[
      trades$tx_hash == attacker_tx1
    ]
  )

block_victim <-
  unique(
    trades$block_number[
      trades$tx_hash == victim_tx
    ]
  )

block_attacker2 <-
  unique(
    trades$block_number[
      trades$tx_hash == attacker_tx2
    ]
  )


same_block <-
  block_attacker1 == block_victim &&
  block_victim == block_attacker2


# The smallest DEX event index belonging to each transaction
# gives their ordering inside our decoded DEX-event data.

index_attacker1 <-
  min(
    trades$evt_index[
      trades$tx_hash == attacker_tx1
    ]
  )

index_victim <-
  min(
    trades$evt_index[
      trades$tx_hash == victim_tx
    ]
  )

index_attacker2 <-
  min(
    trades$evt_index[
      trades$tx_hash == attacker_tx2
    ]
  )


correct_order <-
  index_attacker1 <
  index_victim &&
  index_victim <
  index_attacker2


condition_A <-
  same_block &&
  correct_order



# ------------------------------------------------------------
# CONDITION B
# Compare directions in the NGC/USDT pool.
# ------------------------------------------------------------

ngc_pool <- trades[
  trades$pool == NGC_USDT_POOL,
]


attack1_ngc <-
  ngc_pool[
    ngc_pool$tx_hash == attacker_tx1,
  ]

victim_ngc <-
  ngc_pool[
    ngc_pool$tx_hash == victim_tx,
  ]

attack2_ngc <-
  ngc_pool[
    ngc_pool$tx_hash == attacker_tx2,
  ]


# Transaction 1:
# USDT -> NGC

attack1_direction <-
  attack1_ngc$token_sold_symbol == "USDT" &&
  attack1_ngc$token_bought_symbol == "NGC"


# Victim:
# USDT -> NGC

victim_direction <-
  victim_ngc$token_sold_symbol == "USDT" &&
  victim_ngc$token_bought_symbol == "NGC"


# Transaction 3:
# NGC -> USDT

attack2_opposite_direction <-
  attack2_ngc$token_sold_symbol == "NGC" &&
  attack2_ngc$token_bought_symbol == "USDT"


condition_B <-
  attack1_direction &&
  victim_direction &&
  attack2_opposite_direction



# ------------------------------------------------------------
# CONDITION C
#
# Compare:
#
# NGC bought by attacker in transaction 1
#
# versus
#
# NGC sold by attacker in transaction 2.
# ------------------------------------------------------------

ngc_bought_attack1 <-
  attack1_ngc$token_bought_amount


ngc_sold_attack2 <-
  attack2_ngc$token_sold_amount


amount_ratio <-
  ngc_sold_attack2 /
  ngc_bought_attack1


condition_C <-
  amount_ratio >= 0.90 &&
  amount_ratio <= 1.10



# ============================================================
# FINAL MEV CLASSIFICATION
# ============================================================

sandwich_detected <-
  condition_A &&
  condition_B &&
  condition_C


# If it is a sandwich attack, Stage 3 should REMOVE it.

stage3_removed <-
  sandwich_detected


# Final wash-trading classification.
#
# Even though Stages 1 and 2 pass, Stage 3 removes the group.

final_wash_candidate <-
  stage1_pass &&
  stage2_pass &&
  !stage3_removed



# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("VALIDATION TEST 2 - COMPLETE RESULT\n")
cat("============================================\n\n")


cat("STAGE 1 - COARSE FILTERING\n")
cat("--------------------------------------------\n")
cat("Block span:", block_span, "\n")
cat("WETH restoration ratio:", ratio_WETH, "\n")
cat("USDT restoration ratio:", ratio_USDT, "\n")
cat("Stage 1 PASS:", stage1_pass, "\n\n")


cat("STAGE 2 - ENTITY RECOGNITION\n")
cat("--------------------------------------------\n")
cat("Attacker 1 initiator:", initiator_tx1, "\n")
cat("Attacker 2 initiator:", initiator_tx2, "\n")
cat("Same initiator:", same_initiator, "\n")
cat("Stage 2 PASS:", stage2_pass, "\n\n")


cat("STAGE 3 - SANDWICH DETECTION\n")
cat("--------------------------------------------\n")

cat(
  "Condition A - same block/order:",
  condition_A,
  "\n"
)

cat(
  "Condition B - sandwich directions:",
  condition_B,
  "\n"
)

cat(
  "NGC bought in attacker transaction 1:",
  ngc_bought_attack1,
  "\n"
)

cat(
  "NGC sold in attacker transaction 2:",
  ngc_sold_attack2,
  "\n"
)

cat(
  "Amount ratio:",
  amount_ratio,
  "\n"
)

cat(
  "Condition C - amount match:",
  condition_C,
  "\n"
)

cat(
  "Sandwich attack detected:",
  sandwich_detected,
  "\n\n"
)


cat("============================================\n")
cat("FINAL CLASSIFICATION\n")
cat("============================================\n")


if (
  stage1_pass &&
  stage2_pass &&
  sandwich_detected &&
  !final_wash_candidate
) {

  cat(
    "\nPASS: The apparent wash-trading candidate",
    "was correctly identified as a sandwich attack",
    "and removed by the MEV filter.\n"
  )

} else {

  cat(
    "\nFAIL: The expected sandwich-attack",
    "classification was not reproduced.\n"
  )
}