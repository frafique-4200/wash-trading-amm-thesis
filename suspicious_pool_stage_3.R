# ============================================================
# EMPIRICAL SUSPICIOUS POOL
# STAGE 3 — MEV FALSE-POSITIVE ELIMINATION
#
# PURPOSE
# ============================================================
#
# Apply the MEV false-positive elimination stage to the
# suspicious candidate groups that survived Stage 2.
#
# We test the three MEV categories used in our replication:
#
#   1. Sandwich attack with asset conversion
#
#   2. Ordinary arbitrage with asset conversion
#      a. decoded DEX multi-pool activity
#      b. raw Uniswap V2 multi-pool activity
#      c. possible cross-protocol ERC-20 routing
#
#   3. Rebase-token arbitrage
#
#
# INPUT FILES
# ============================================================
#
# Stage 2:
#
#   suspicious_pool_stage2_surviving_swaps.csv
#
#
# Stage 3A:
#
#   suspicious_pool_stage3_candidate_dex_activity.csv
#
#
# Stage 3B:
#
#   suspicious_pool_stage3_raw_uniswap_swaps.csv
#
#
# Stage 3C:
#
#   suspicious_pool_stage3_erc20_transfers.csv
#
#
# IMPORTANT
# ============================================================
#
# Flash-loan detection is NOT implemented because flash-loan
# analysis is outside the objective of this thesis.
#
# ============================================================



# ============================================================
# PARAMETERS
# ============================================================

# No additional numerical MEV parameter is required for these
# empirical candidates because neither candidate satisfies the
# same-block prerequisite for the sandwich test.



# ============================================================
# FILE LOCATIONS
# ============================================================

stage2_swaps_file <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage2_surviving_swaps.csv"


dex_activity_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/suspicious_pool_stage3_candidate_dex_activity.csv"


raw_uniswap_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/suspicious_pool_stage3_raw_uniswap_swaps.csv"


erc20_transfers_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/suspicious_pool_stage3_erc20_transfers.csv"



# ============================================================
# OUTPUT FILE LOCATIONS
# ============================================================

stage3_summary_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage3_summary.csv"


final_candidate_swaps_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_final_candidate_swaps.csv"



# ============================================================
# HELPER FUNCTION 1
# CHECK REQUIRED COLUMNS
# ============================================================

require_columns <- function(data, required_columns, file_name) {

  missing_columns <-
    setdiff(
      required_columns,
      names(data)
    )


  if (length(missing_columns) > 0) {

    stop(
      paste0(
        "\nMissing required column(s) in ",
        file_name,
        ":\n",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
}



# ============================================================
# HELPER FUNCTION 2
# NORMALIZE RAW BLOCKCHAIN INTEGER STRINGS
# ============================================================

# Raw ERC-20 amounts can exceed the exact integer precision of
# ordinary R numeric values.
#
# For comparisons involving raw blockchain integers, we keep
# the values as character strings.


normalize_integer_string <- function(x) {

  x <- as.character(x)


  if (is.na(x)) {

    return("0")
  }


  x <- trimws(x)


  if (
    x == "" ||
    x == "NA" ||
    x == "<nil>"
  ) {

    return("0")
  }


  # Remove leading zeroes.

  x <- sub(
    "^0+",
    "",
    x
  )


  if (x == "") {

    return("0")
  }


  x
}



# ============================================================
# HELPER FUNCTION 3
# TEST WHETHER A RAW INTEGER IS NONZERO
# ============================================================

is_nonzero_raw <- function(x) {

  normalize_integer_string(x) != "0"
}



# ============================================================
# HELPER FUNCTION 4
# EXACT POSITIVE INTEGER GREATER-THAN COMPARISON
# ============================================================

# We compare giant blockchain integers without converting them
# to floating-point numbers.
#
# Returns TRUE when:
#
#       a > b
#
# where a and b are non-negative integer strings.


integer_string_greater <- function(a, b) {

  a <- normalize_integer_string(a)
  b <- normalize_integer_string(b)


  # Longer positive integer = larger number.

  if (nchar(a) > nchar(b)) {

    return(TRUE)
  }


  if (nchar(a) < nchar(b)) {

    return(FALSE)
  }


  # Equal strings are not greater.

  if (identical(a, b)) {

    return(FALSE)
  }


  # Same number of digits:
  # compare digit by digit.

  for (position in seq_len(nchar(a))) {

    digit_a <-
      as.integer(
        substr(
          a,
          position,
          position
        )
      )


    digit_b <-
      as.integer(
        substr(
          b,
          position,
          position
        )
      )


    if (digit_a > digit_b) {

      return(TRUE)
    }


    if (digit_a < digit_b) {

      return(FALSE)
    }
  }


  FALSE
}



# ============================================================
# HELPER FUNCTION 5
# REBASE-ARBITRAGE DETECTION
# ============================================================

# Rebase arbitrage requires two opposite Swap events inside
# the SAME Ethereum transaction and the SAME liquidity pool.
#
#
# Orientation A:
#
#   Swap 1:
#       token0 IN
#       token1 OUT
#
#   Swap 2:
#       token1 IN
#       token0 OUT
#
#
# We then test:
#
#   token1_out(first swap)
#          =
#   token1_in(second swap)
#
# AND
#
#   returned token0
#          >
#   starting token0
#
#
# The reverse orientation is also tested.


detect_rebase_pattern <- function(transaction_swaps) {

  # ----------------------------------------------------------
  # At least two raw swaps are required.
  # ----------------------------------------------------------

  if (nrow(transaction_swaps) < 2) {

    return(FALSE)
  }


  # ----------------------------------------------------------
  # Sort swaps by event index.
  # ----------------------------------------------------------

  transaction_swaps <-
    transaction_swaps[
      order(
        transaction_swaps$evt_index
      ),
    ]


  # ----------------------------------------------------------
  # Test every possible pair of swaps.
  # ----------------------------------------------------------

  for (i in seq_len(nrow(transaction_swaps) - 1)) {

    for (j in seq.int(i + 1, nrow(transaction_swaps))) {


      swap1 <- transaction_swaps[i, ]
      swap2 <- transaction_swaps[j, ]


      # ------------------------------------------------------
      # Both swaps must occur in the same pool.
      # ------------------------------------------------------

      if (swap1$pool != swap2$pool) {

        next
      }



      # ======================================================
      # ORIENTATION A
      #
      # Swap 1:
      # token0 -> token1
      #
      # Swap 2:
      # token1 -> token0
      # ======================================================

      first_is_token0_to_token1 <-

        is_nonzero_raw(
          swap1$amount0In
        ) &&

        !is_nonzero_raw(
          swap1$amount0Out
        ) &&

        !is_nonzero_raw(
          swap1$amount1In
        ) &&

        is_nonzero_raw(
          swap1$amount1Out
        )



      second_is_token1_to_token0 <-

        !is_nonzero_raw(
          swap2$amount0In
        ) &&

        is_nonzero_raw(
          swap2$amount0Out
        ) &&

        is_nonzero_raw(
          swap2$amount1In
        ) &&

        !is_nonzero_raw(
          swap2$amount1Out
        )



      if (
        first_is_token0_to_token1 &&
        second_is_token1_to_token0
      ) {


        # Exact intermediate-token equality.

        intermediate_amount_matches <-

          normalize_integer_string(
            swap1$amount1Out
          ) ==

          normalize_integer_string(
            swap2$amount1In
          )



        # More token0 returned than originally supplied.

        profitable_return <-

          integer_string_greater(
            swap2$amount0Out,
            swap1$amount0In
          )



        if (
          intermediate_amount_matches &&
          profitable_return
        ) {

          return(TRUE)
        }
      }



      # ======================================================
      # ORIENTATION B
      #
      # Swap 1:
      # token1 -> token0
      #
      # Swap 2:
      # token0 -> token1
      # ======================================================

      first_is_token1_to_token0 <-

        !is_nonzero_raw(
          swap1$amount0In
        ) &&

        is_nonzero_raw(
          swap1$amount0Out
        ) &&

        is_nonzero_raw(
          swap1$amount1In
        ) &&

        !is_nonzero_raw(
          swap1$amount1Out
        )



      second_is_token0_to_token1 <-

        is_nonzero_raw(
          swap2$amount0In
        ) &&

        !is_nonzero_raw(
          swap2$amount0Out
        ) &&

        !is_nonzero_raw(
          swap2$amount1In
        ) &&

        is_nonzero_raw(
          swap2$amount1Out
        )



      if (
        first_is_token1_to_token0 &&
        second_is_token0_to_token1
      ) {


        # Exact intermediate-token equality.

        intermediate_amount_matches <-

          normalize_integer_string(
            swap1$amount0Out
          ) ==

          normalize_integer_string(
            swap2$amount0In
          )



        # More token1 returned than originally supplied.

        profitable_return <-

          integer_string_greater(
            swap2$amount1Out,
            swap1$amount1In
          )



        if (
          intermediate_amount_matches &&
          profitable_return
        ) {

          return(TRUE)
        }
      }
    }
  }


  FALSE
}



# ============================================================
# STEP 1
# LOAD STAGE-2 SURVIVING CANDIDATES
# ============================================================

stage2_swaps <- read.csv(
  stage2_swaps_file,
  colClasses = "character"
)



require_columns(
  stage2_swaps,
  c(
    "candidate_id",
    "swap_sequence",
    "evt_tx_hash",
    "evt_tx_from"
  ),
  "suspicious_pool_stage2_surviving_swaps.csv"
)



stage2_swaps$candidate_id <-
  as.integer(
    stage2_swaps$candidate_id
  )


stage2_swaps$swap_sequence <-
  as.integer(
    stage2_swaps$swap_sequence
  )


stage2_swaps$evt_tx_hash <-
  tolower(
    stage2_swaps$evt_tx_hash
  )


stage2_swaps$evt_tx_from <-
  tolower(
    stage2_swaps$evt_tx_from
  )



candidate_ids <-
  sort(
    unique(
      stage2_swaps$candidate_id
    )
  )



cat(
  "Stage-2 candidates loaded:",
  length(candidate_ids),
  "\n"
)



# ============================================================
# STEP 2
# LOAD STAGE 3A — COMPLETE DECODED DEX ACTIVITY
# ============================================================

dex_activity <- read.csv(
  dex_activity_file,
  colClasses = "character"
)



require_columns(
  dex_activity,
  c(
    "block_number",
    "tx_hash",
    "tx_from",
    "pool",
    "token_sold_symbol",
    "token_bought_symbol"
  ),
  "suspicious_pool_stage3_candidate_dex_activity.csv"
)



dex_activity$tx_hash <-
  tolower(
    dex_activity$tx_hash
  )


dex_activity$tx_from <-
  tolower(
    dex_activity$tx_from
  )


dex_activity$pool <-
  tolower(
    dex_activity$pool
  )


dex_activity$block_number <-
  as.integer(
    as.numeric(
      dex_activity$block_number
    )
  )



# ============================================================
# STEP 3
# LOAD STAGE 3B — RAW UNISWAP V2 SWAPS
# ============================================================

raw_swaps <- read.csv(
  raw_uniswap_file,
  colClasses = "character"
)



require_columns(
  raw_swaps,
  c(
    "pool",
    "evt_tx_hash",
    "evt_tx_from",
    "evt_block_number",
    "evt_index",
    "amount0In",
    "amount0Out",
    "amount1In",
    "amount1Out",
    "sender",
    "to"
  ),
  "suspicious_pool_stage3_raw_uniswap_swaps.csv"
)



raw_swaps$evt_tx_hash <-
  tolower(
    raw_swaps$evt_tx_hash
  )


raw_swaps$evt_tx_from <-
  tolower(
    raw_swaps$evt_tx_from
  )


raw_swaps$pool <-
  tolower(
    raw_swaps$pool
  )


raw_swaps$sender <-
  tolower(
    raw_swaps$sender
  )


raw_swaps$to <-
  tolower(
    raw_swaps$to
  )


raw_swaps$evt_block_number <-
  as.integer(
    as.numeric(
      raw_swaps$evt_block_number
    )
  )


raw_swaps$evt_index <-
  as.integer(
    as.numeric(
      raw_swaps$evt_index
    )
  )



# ============================================================
# STEP 4
# LOAD STAGE 3C — ERC-20 TRANSFERS
# ============================================================

erc20_transfers <- read.csv(
  erc20_transfers_file,
  colClasses = "character"
)



require_columns(
  erc20_transfers,
  c(
    "evt_tx_hash",
    "token_address",
    "from",
    "to",
    "value"
  ),
  "suspicious_pool_stage3_erc20_transfers.csv"
)



erc20_transfers$evt_tx_hash <-
  tolower(
    erc20_transfers$evt_tx_hash
  )


erc20_transfers$token_address <-
  tolower(
    erc20_transfers$token_address
  )


erc20_transfers[["from"]] <-
  tolower(
    erc20_transfers[["from"]]
  )


erc20_transfers[["to"]] <-
  tolower(
    erc20_transfers[["to"]]
  )



# ============================================================
# STEP 5
# SANITY CHECK — DO WE HAVE THE CANDIDATE TRANSACTIONS?
# ============================================================

candidate_transaction_hashes <-
  unique(
    stage2_swaps$evt_tx_hash
  )



missing_from_dex_activity <-
  setdiff(
    candidate_transaction_hashes,
    unique(dex_activity$tx_hash)
  )


missing_from_raw_swaps <-
  setdiff(
    candidate_transaction_hashes,
    unique(raw_swaps$evt_tx_hash)
  )


missing_from_erc20 <-
  setdiff(
    candidate_transaction_hashes,
    unique(erc20_transfers$evt_tx_hash)
  )



if (length(missing_from_dex_activity) > 0) {

  warning(
    paste(
      "Candidate transaction(s) missing from dex.trades:",
      paste(
        missing_from_dex_activity,
        collapse = ", "
      )
    )
  )
}



if (length(missing_from_raw_swaps) > 0) {

  warning(
    paste(
      "Candidate transaction(s) missing from raw Uniswap V2 data:",
      paste(
        missing_from_raw_swaps,
        collapse = ", "
      )
    )
  )
}



if (length(missing_from_erc20) > 0) {

  warning(
    paste(
      "Candidate transaction(s) missing from ERC-20 transfer data:",
      paste(
        missing_from_erc20,
        collapse = ", "
      )
    )
  )
}



# ============================================================
# STEP 6
# CREATE EMPTY STAGE-3 SUMMARY TABLE
# ============================================================

# We deliberately use a data.frame and rbind() here instead of
# list [[...]] indexing.
#
# This avoids the bracket syntax problem encountered earlier.


stage3_summary <- data.frame(

  candidate_id = integer(),

  start_swap = integer(),

  end_swap = integer(),

  number_transactions = integer(),

  sandwich_same_block = logical(),

  sandwich_detected = logical(),

  decoded_multi_pool = logical(),

  raw_v2_multi_pool = logical(),

  unexpected_erc20_addresses = integer(),

  cross_protocol_review_required = logical(),

  ordinary_arbitrage_detected = logical(),

  rebase_arbitrage_detected = logical(),

  stage3_resolved = logical(),

  mev_exclusion_trigger = logical(),

  stage3_pass = logical(),

  stringsAsFactors = FALSE
)



# ============================================================
# STEP 7
# PROCESS EACH CANDIDATE
# ============================================================

for (candidate_id in candidate_ids) {


  # ----------------------------------------------------------
  # Candidate swaps surviving Stage 2
  # ----------------------------------------------------------

  candidate <-
    stage2_swaps[
      stage2_swaps$candidate_id == candidate_id,
    ]


  candidate_hashes <-
    unique(
      candidate$evt_tx_hash
    )



  # ----------------------------------------------------------
  # Candidate's decoded DEX activity
  # ----------------------------------------------------------

  candidate_dex <-
    dex_activity[
      dex_activity$tx_hash %in% candidate_hashes,
    ]



  # ----------------------------------------------------------
  # Candidate's raw Uniswap V2 swaps
  # ----------------------------------------------------------

  candidate_raw <-
    raw_swaps[
      raw_swaps$evt_tx_hash %in% candidate_hashes,
    ]



  # ----------------------------------------------------------
  # Candidate's ERC-20 transfers
  # ----------------------------------------------------------

  candidate_transfers <-
    erc20_transfers[
      erc20_transfers$evt_tx_hash %in% candidate_hashes,
    ]



  # ==========================================================
  # MEV TEST 1
  # SANDWICH ATTACK
  # ==========================================================
  #
  # The validated sandwich structure requires the relevant
  # attacker/victim/attacker transactions to occur in the
  # same Ethereum block.
  #
  # Our empirical candidate groups are separated by several
  # blocks.
  #
  # Therefore they fail the same-block prerequisite.
  #
  # If a future candidate occurs in one block, we mark it for
  # additional review rather than automatically declaring it
  # a sandwich attack.


  candidate_blocks <-
    sort(
      unique(
        candidate_raw$evt_block_number
      )
    )


  sandwich_same_block <-
    length(candidate_blocks) == 1



  if (sandwich_same_block) {

    sandwich_review_required <- TRUE

  } else {

    sandwich_review_required <- FALSE
  }



  # No empirical sandwich is detected for the present
  # candidates because the same-block prerequisite is false.

  sandwich_detected <- FALSE



  # ==========================================================
  # MEV TEST 2A
  # ORDINARY ARBITRAGE — DECODED MULTI-POOL ACTIVITY
  # ==========================================================
  #
  # For every Ethereum transaction, count how many distinct
  # liquidity pools appear in dex.trades.
  #
  # More than one pool in a transaction is evidence of the
  # multi-pool pattern used for ordinary arbitrage exclusion.


  decoded_multi_pool <- FALSE



  if (nrow(candidate_dex) > 0) {


    decoded_pools_per_transaction <-
      tapply(
        candidate_dex$pool,
        candidate_dex$tx_hash,
        function(x) {

          length(
            unique(
              x[
                !is.na(x) &
                x != ""
              ]
            )
          )
        }
      )


    decoded_multi_pool <-
      any(
        decoded_pools_per_transaction > 1
      )
  }



  # ==========================================================
  # MEV TEST 2B
  # ORDINARY ARBITRAGE — RAW UNISWAP V2 MULTI-POOL ACTIVITY
  # ==========================================================
  #
  # We repeat the multi-pool check using raw Uniswap V2 Swap
  # events.
  #
  # This is useful because Validation Test 4 demonstrated that
  # dex.trades does not necessarily contain every relevant
  # historical swap event.


  raw_v2_multi_pool <- FALSE



  if (nrow(candidate_raw) > 0) {


    raw_pools_per_transaction <-
      tapply(
        candidate_raw$pool,
        candidate_raw$evt_tx_hash,
        function(x) {

          length(
            unique(
              x[
                !is.na(x) &
                x != ""
              ]
            )
          )
        }
      )


    raw_v2_multi_pool <-
      any(
        raw_pools_per_transaction > 1
      )
  }



  # ==========================================================
  # MEV TEST 2C
  # POSSIBLE CROSS-PROTOCOL ERC-20 ROUTING
  # ==========================================================
  #
  # We inspect ERC-20 transfer counterparties.
  #
  # Addresses already explained by the known Uniswap
  # transaction structure are:
  #
  #   - transaction initiators
  #   - the liquidity pool
  #   - raw Swap-event sender
  #   - raw Swap-event recipient
  #
  # Any other address would require additional Dune/Etherscan
  # investigation to determine whether it is another DEX or
  # public-service protocol.


  expected_transfer_addresses <-
    unique(
      c(
        candidate$evt_tx_from,
        candidate_raw$pool,
        candidate_raw$sender,
        candidate_raw$to
      )
    )



  expected_transfer_addresses <-
    expected_transfer_addresses[
      !is.na(expected_transfer_addresses) &
      expected_transfer_addresses != ""
    ]



  transfer_counterparties <-
    unique(
      c(
        candidate_transfers[["from"]],
        candidate_transfers[["to"]]
      )
    )



  transfer_counterparties <-
    transfer_counterparties[
      !is.na(transfer_counterparties) &
      transfer_counterparties != ""
    ]



  unexpected_transfer_addresses <-
    setdiff(
      transfer_counterparties,
      expected_transfer_addresses
    )



  number_unexpected_transfer_addresses <-
    length(
      unexpected_transfer_addresses
    )



  # For the present empirical candidates, the manually
  # inspected Stage-3C data contains no unexplained
  # cross-protocol receiving address.


  cross_protocol_review_required <-
    number_unexpected_transfer_addresses > 0



  # We do NOT automatically classify an unexpected address as
  # an arbitrage protocol.
  #
  # If one existed, Stage 3 would remain unresolved until its
  # identity was checked.


  cross_protocol_arbitrage_detected <- FALSE



  # ==========================================================
  # COMPLETE ORDINARY-ARBITRAGE RESULT
  # ==========================================================

  ordinary_arbitrage_detected <-

    decoded_multi_pool ||

    raw_v2_multi_pool ||

    cross_protocol_arbitrage_detected



  # ==========================================================
  # MEV TEST 3
  # REBASE-TOKEN ARBITRAGE
  # ==========================================================

  rebase_arbitrage_detected <- FALSE



  for (tx_hash in candidate_hashes) {


    transaction_raw_swaps <-
      raw_swaps[
        raw_swaps$evt_tx_hash == tx_hash,
      ]


    if (
      detect_rebase_pattern(
        transaction_raw_swaps
      )
    ) {

      rebase_arbitrage_detected <- TRUE

      break
    }
  }



  # ==========================================================
  # STAGE-3 RESOLUTION STATUS
  # ==========================================================
  #
  # Stage 3 is considered fully resolved only if:
  #
  #   1. no additional sandwich investigation is required;
  #
  #   2. no unexplained ERC-20 counterparty requires
  #      cross-protocol service identification.
  #
  #
  # For our present two candidates, we expect both conditions
  # to be satisfied.


  stage3_resolved <-

    !sandwich_review_required &&

    !cross_protocol_review_required



  # ==========================================================
  # COMPLETE MEV EXCLUSION RESULT
  # ==========================================================

  mev_exclusion_trigger <-

    sandwich_detected ||

    ordinary_arbitrage_detected ||

    rebase_arbitrage_detected



  # ==========================================================
  # FINAL STAGE-3 DECISION
  # ==========================================================
  #
  # A candidate survives Stage 3 only when:
  #
  #   - the MEV investigation is resolved;
  #
  #   - none of the three MEV false-positive categories was
  #     detected.


  stage3_pass <-

    stage3_resolved &&

    !mev_exclusion_trigger



  # ==========================================================
  # CREATE ONE SUMMARY ROW
  # ==========================================================

  result_row <-
    data.frame(

      candidate_id =
        candidate_id,

      start_swap =
        min(
          candidate$swap_sequence
        ),

      end_swap =
        max(
          candidate$swap_sequence
        ),

      number_transactions =
        length(
          candidate_hashes
        ),

      sandwich_same_block =
        sandwich_same_block,

      sandwich_detected =
        sandwich_detected,

      decoded_multi_pool =
        decoded_multi_pool,

      raw_v2_multi_pool =
        raw_v2_multi_pool,

      unexpected_erc20_addresses =
        number_unexpected_transfer_addresses,

      cross_protocol_review_required =
        cross_protocol_review_required,

      ordinary_arbitrage_detected =
        ordinary_arbitrage_detected,

      rebase_arbitrage_detected =
        rebase_arbitrage_detected,

      stage3_resolved =
        stage3_resolved,

      mev_exclusion_trigger =
        mev_exclusion_trigger,

      stage3_pass =
        stage3_pass,

      stringsAsFactors = FALSE
    )



  # ==========================================================
  # ADD RESULT TO SUMMARY TABLE
  # ==========================================================

  stage3_summary <-
    rbind(
      stage3_summary,
      result_row
    )
}



# ============================================================
# STEP 8
# IDENTIFY FINAL SURVIVING CANDIDATES
# ============================================================

final_candidate_ids <-
  stage3_summary$candidate_id[
    stage3_summary$stage3_pass
  ]



final_candidate_swaps <-
  stage2_swaps[
    stage2_swaps$candidate_id %in% final_candidate_ids,
  ]



# ============================================================
# STEP 9
# COUNT UNIQUE FINAL TRANSACTIONS
# ============================================================

number_final_transactions <-
  length(
    unique(
      final_candidate_swaps$evt_tx_hash
    )
  )



# ============================================================
# STEP 10
# SAVE OUTPUT FILES
# ============================================================

write.csv(
  stage3_summary,
  stage3_summary_output,
  row.names = FALSE
)



write.csv(
  final_candidate_swaps,
  final_candidate_swaps_output,
  row.names = FALSE
)



# ============================================================
# STEP 11
# PRINT COMPLETE STAGE-3 RESULTS
# ============================================================

cat("\n")

cat(
  "============================================\n"
)

cat(
  "SUSPICIOUS POOL — STAGE 3 EMPIRICAL RESULT\n"
)

cat(
  "============================================\n\n"
)



cat(
  "Candidates entering Stage 3:",
  length(candidate_ids),
  "\n\n"
)



# ============================================================
# PRINT EACH CANDIDATE SEPARATELY
# ============================================================

for (i in seq_len(nrow(stage3_summary))) {


  result <-
    stage3_summary[i, ]


  cat(
    "CANDIDATE",
    result$candidate_id,
    "\n"
  )


  cat(
    "--------------------------------------------\n"
  )


  cat(
    "Swaps:",
    result$start_swap,
    "-",
    result$end_swap,
    "\n"
  )


  cat(
    "Number of transactions:",
    result$number_transactions,
    "\n"
  )


  cat(
    "Sandwich same-block condition:",
    result$sandwich_same_block,
    "\n"
  )


  cat(
    "Sandwich attack detected:",
    result$sandwich_detected,
    "\n"
  )


  cat(
    "Decoded multi-pool activity:",
    result$decoded_multi_pool,
    "\n"
  )


  cat(
    "Raw V2 multi-pool activity:",
    result$raw_v2_multi_pool,
    "\n"
  )


  cat(
    "Unexpected ERC-20 counterparties:",
    result$unexpected_erc20_addresses,
    "\n"
  )


  cat(
    "Cross-protocol review required:",
    result$cross_protocol_review_required,
    "\n"
  )


  cat(
    "Ordinary arbitrage detected:",
    result$ordinary_arbitrage_detected,
    "\n"
  )


  cat(
    "Rebase arbitrage detected:",
    result$rebase_arbitrage_detected,
    "\n"
  )


  cat(
    "MEV exclusion triggered:",
    result$mev_exclusion_trigger,
    "\n"
  )


  cat(
    "Stage 3 resolved:",
    result$stage3_resolved,
    "\n"
  )


  cat(
    "Stage 3 PASS:",
    result$stage3_pass,
    "\n\n"
  )
}



# ============================================================
# STEP 12
# PRINT FINAL EMPIRICAL SUMMARY
# ============================================================

cat(
  "============================================\n"
)

cat(
  "FINAL EMPIRICAL CLASSIFICATION\n"
)

cat(
  "============================================\n"
)



cat(
  "Candidates entering Stage 3:",
  nrow(stage3_summary),
  "\n"
)



cat(
  "Candidates removed as MEV:",
  sum(
    stage3_summary$mev_exclusion_trigger
  ),
  "\n"
)



cat(
  "Candidates surviving Stage 3:",
  sum(
    stage3_summary$stage3_pass
  ),
  "\n"
)



cat(
  "Unique transactions in final candidates:",
  number_final_transactions,
  "\n"
)



if (all(stage3_summary$stage3_resolved)) {

  cat(
    "\nAll Stage-3 candidate classifications were resolved.\n"
  )

} else {

  cat(
    "\nWARNING: At least one candidate requires additional ",
    "MEV investigation before final classification.\n"
  )
}



# ============================================================
# STEP 13
# PRINT OUTPUT FILE LOCATIONS
# ============================================================

cat("\n")

cat(
  "OUTPUT FILES\n"
)

cat(
  "--------------------------------------------\n"
)



cat(
  "Stage-3 summary:\n",
  stage3_summary_output,
  "\n\n"
)



cat(
  "Final suspicious candidate swaps:\n",
  final_candidate_swaps_output,
  "\n"
)