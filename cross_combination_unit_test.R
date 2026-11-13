# ============================================================
# CROSS-COMBINATION STRUCTURAL UNIT TEST
#
# PURPOSE:
#
# Validate the Cross Combination / Group (1) logic described
# in the AMM wash-trading paper.
#
# This is NOT a historical blockchain example.
#
# We construct artificial swaps for several different senders
# and test whether EACH sender approximately restores their
# own position.
#
# IMPORTANT:
#
# Cross Combination differs from the entity-recognition case.
#
# The senders MAY have no one-hop or two-hop ETH relationship.
# What matters here is that each sender individually restores
# their trading position.
#
# PAPER PARAMETER:
#
#       rho = 5%
#
# ============================================================



# ============================================================
# PARAMETER
# ============================================================

RHO <- 0.05



# ============================================================
# FUNCTION — TEST ONE SENDER'S POSITION
# ============================================================

# The paper describes the relative position change as:
#
#       absolute net position change
#       ----------------------------
#          sender trading volume
#
# <= rho
#
# For this reduced implementation, we use the same operational
# convention used in our coarse-filter implementation:
#
# sender volume = sum of absolute movements of the selected
# pool asset.
#
# Therefore:
#
#                |sum(delta_X)|
# ratio_sender = ----------------
#                sum(|delta_X|)
#
#
# A ratio near zero means that the sender traded substantially
# but approximately returned to their original position.


sender_position_ratio <- function(delta_values) {

  net_position <-
    sum(delta_values)


  gross_movement <-
    sum(
      abs(delta_values)
    )


  # No trading activity cannot establish position restoration.

  if (gross_movement == 0) {

    return(Inf)

  }


  ratio <-
    abs(net_position) /
    gross_movement


  return(ratio)
}



# ============================================================
# FUNCTION — TEST A COMPLETE CROSS COMBINATION
# ============================================================

cross_combination_test <- function(
    swap_data,
    rho = 0.05
) {

  # ----------------------------------------------------------
  # A Cross Combination requires more than one sender.
  # ----------------------------------------------------------

  senders <-
    unique(
      swap_data$sender
    )


  if (length(senders) < 2) {

    return(
      list(
        detected = FALSE,
        ratios = NULL
      )
    )
  }


  # ----------------------------------------------------------
  # Calculate the position-change ratio separately for each
  # sender.
  # ----------------------------------------------------------

  sender_ratios <-
    sapply(

      senders,

      function(current_sender) {

        sender_swaps <-
          swap_data[
            swap_data$sender == current_sender,
          ]


        sender_position_ratio(
          sender_swaps$delta_X
        )
      }
    )


  # ----------------------------------------------------------
  # PAPER'S CROSS-COMBINATION IDEA:
  #
  # Every sender must individually approximately restore
  # their position.
  # ----------------------------------------------------------

  every_sender_restored <-
    all(
      sender_ratios <= rho
    )


  return(
    list(
      detected = every_sender_restored,
      ratios = sender_ratios
    )
  )
}



# ============================================================
# TEST 1 — EXACT CROSS COMBINATION
# ============================================================

# Artificial chronological sequence:
#
# Swap 1: Sender A buys  10 X
# Swap 2: Sender B buys  20 X
# Swap 3: Sender A sells 10 X
# Swap 4: Sender B sells 20 X
#
#
# From the SENDER'S perspective:
#
# Buying X  -> +X
# Selling X -> -X
#
#
# Sender A:
#
#       +10 - 10 = 0
#
# Sender B:
#
#       +20 - 20 = 0
#
#
# Both senders perfectly restore their positions.
#
# They do NOT need to be connected through the ETH-transfer
# graph for this particular pattern.


test1_data <- data.frame(

  swap = c(
    1,
    2,
    3,
    4
  ),

  sender = c(
    "A",
    "B",
    "A",
    "B"
  ),

  delta_X = c(
    +10,
    +20,
    -10,
    -20
  ),

  stringsAsFactors = FALSE
)



test1_result <-
  cross_combination_test(
    test1_data,
    RHO
  )


test1_expected <- TRUE


test1_pass <-
  test1_result$detected ==
  test1_expected



# ============================================================
# TEST 2 — SMALL DEVIATION WITHIN rho
# ============================================================

# Now Sender A does not return exactly to zero.
#
# Sender A:
#
#       +10
#       -9.5
#
# Net change:
#
#       0.5
#
# Gross movement:
#
#       19.5
#
# Ratio:
#
#       0.5 / 19.5
#       ≈ 0.02564
#       ≈ 2.56%
#
# Since:
#
#       2.56% < 5%
#
# Sender A still satisfies rho.
#
#
# Sender B restores exactly.
#
# EXPECTED:
#
# TRUE


test2_data <- data.frame(

  swap = c(
    1,
    2,
    3,
    4
  ),

  sender = c(
    "A",
    "B",
    "A",
    "B"
  ),

  delta_X = c(
    +10,
    +20,
    -9.5,
    -20
  ),

  stringsAsFactors = FALSE
)



test2_result <-
  cross_combination_test(
    test2_data,
    RHO
  )


test2_expected <- TRUE


test2_pass <-
  test2_result$detected ==
  test2_expected



# ============================================================
# TEST 3 — ONE SENDER EXCEEDS rho
# ============================================================

# Sender A:
#
#       +10
#       -8
#
# Net position:
#
#       2
#
# Gross movement:
#
#       18
#
# Ratio:
#
#       2 / 18
#       ≈ 0.1111
#       ≈ 11.11%
#
# This exceeds rho = 5%.
#
# Sender B still restores exactly.
#
# Because EVERY sender must restore their position,
# the entire Cross Combination condition must fail.
#
# EXPECTED:
#
# FALSE


test3_data <- data.frame(

  swap = c(
    1,
    2,
    3,
    4
  ),

  sender = c(
    "A",
    "B",
    "A",
    "B"
  ),

  delta_X = c(
    +10,
    +20,
    -8,
    -20
  ),

  stringsAsFactors = FALSE
)



test3_result <-
  cross_combination_test(
    test3_data,
    RHO
  )


test3_expected <- FALSE


test3_pass <-
  test3_result$detected ==
  test3_expected



# ============================================================
# TEST 4 — SINGLE SENDER IS NOT A CROSS COMBINATION
# ============================================================

# A single sender may represent the paper's Self-trade
# Combination, but by definition it is not the multi-sender
# Cross Combination pattern.
#
# EXPECTED:
#
# FALSE


test4_data <- data.frame(

  swap = c(
    1,
    2
  ),

  sender = c(
    "A",
    "A"
  ),

  delta_X = c(
    +10,
    -10
  ),

  stringsAsFactors = FALSE
)



test4_result <-
  cross_combination_test(
    test4_data,
    RHO
  )


test4_expected <- FALSE


test4_pass <-
  test4_result$detected ==
  test4_expected



# ============================================================
# TEST 5 — DISCONNECTED SENDERS CAN STILL PASS
# ============================================================

# This test emphasizes the conceptual difference from entity
# recognition.
#
# We deliberately provide no ETH-transfer graph at all.
#
# Sender A restores:
#
#       +15 - 15 = 0
#
# Sender B restores:
#
#       +25 - 25 = 0
#
#
# The paper says Cross Combination senders may have no
# one-hop or two-hop entity relationship.
#
# Therefore the absence of an entity connection does NOT
# invalidate this pattern.
#
# EXPECTED:
#
# TRUE


test5_data <- data.frame(

  swap = c(
    1,
    2,
    3,
    4
  ),

  sender = c(
    "A",
    "B",
    "A",
    "B"
  ),

  delta_X = c(
    +15,
    +25,
    -15,
    -25
  ),

  stringsAsFactors = FALSE
)



test5_result <-
  cross_combination_test(
    test5_data,
    RHO
  )


test5_expected <- TRUE


test5_pass <-
  test5_result$detected ==
  test5_expected



# ============================================================
# OVERALL TEST RESULT
# ============================================================

all_tests_pass <-
  test1_pass &&
  test2_pass &&
  test3_pass &&
  test4_pass &&
  test5_pass



# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("CROSS-COMBINATION STRUCTURAL UNIT TEST\n")
cat("============================================\n\n")



# ------------------------------------------------------------
# TEST 1
# ------------------------------------------------------------

cat("TEST 1 — EXACT POSITION RESTORATION\n")
cat("--------------------------------------------\n")

cat(
  "Sender ratios:\n"
)

print(
  test1_result$ratios
)

cat(
  "Expected Cross Combination: TRUE\n"
)

cat(
  "Observed:",
  test1_result$detected,
  "\n"
)

cat(
  "PASS:",
  test1_pass,
  "\n\n"
)



# ------------------------------------------------------------
# TEST 2
# ------------------------------------------------------------

cat("TEST 2 — DEVIATION WITHIN 5%\n")
cat("--------------------------------------------\n")

cat(
  "Sender ratios:\n"
)

print(
  test2_result$ratios
)

cat(
  "Expected Cross Combination: TRUE\n"
)

cat(
  "Observed:",
  test2_result$detected,
  "\n"
)

cat(
  "PASS:",
  test2_pass,
  "\n\n"
)



# ------------------------------------------------------------
# TEST 3
# ------------------------------------------------------------

cat("TEST 3 — DEVIATION ABOVE 5%\n")
cat("--------------------------------------------\n")

cat(
  "Sender ratios:\n"
)

print(
  test3_result$ratios
)

cat(
  "Expected Cross Combination: FALSE\n"
)

cat(
  "Observed:",
  test3_result$detected,
  "\n"
)

cat(
  "PASS:",
  test3_pass,
  "\n\n"
)



# ------------------------------------------------------------
# TEST 4
# ------------------------------------------------------------

cat("TEST 4 — SINGLE SENDER\n")
cat("--------------------------------------------\n")

cat(
  "Expected Cross Combination: FALSE\n"
)

cat(
  "Observed:",
  test4_result$detected,
  "\n"
)

cat(
  "PASS:",
  test4_pass,
  "\n\n"
)



# ------------------------------------------------------------
# TEST 5
# ------------------------------------------------------------

cat("TEST 5 — NO ENTITY RELATIONSHIP REQUIRED\n")
cat("--------------------------------------------\n")

cat(
  "Sender ratios:\n"
)

print(
  test5_result$ratios
)

cat(
  "Expected Cross Combination: TRUE\n"
)

cat(
  "Observed:",
  test5_result$detected,
  "\n"
)

cat(
  "PASS:",
  test5_pass,
  "\n\n"
)



# ============================================================
# FINAL RESULT
# ============================================================

cat("============================================\n")
cat("FINAL UNIT-TEST RESULT\n")
cat("============================================\n")


if (all_tests_pass) {

  cat(
    "\nPASS: All Cross-Combination structural",
    "tests produced the expected results.\n"
  )

} else {

  cat(
    "\nFAIL: At least one Cross-Combination",
    "test did not produce the expected result.\n"
  )
}