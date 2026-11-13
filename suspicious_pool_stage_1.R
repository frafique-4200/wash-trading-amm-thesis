# ============================================================
# EMPIRICAL APPLICATION — SUSPICIOUS UNISWAP V2 POOL
# STAGE 1: COARSE FILTERING
#
# INPUT:
# uniswap_v2_suspicious_pool_raw_swaps.csv
#
# PURPOSE:
#
# Apply the paper's Stage-1 coarse filtering procedure to
# the real pool data downloaded from Dune.
#
# The procedure:
#
#   1. Sort swaps chronologically.
#   2. Construct adjacent sliding windows.
#   3. Use group lengths from 2 through 10.
#   4. Require block span <= tau = 200.
#   5. Calculate approximate pool restoration.
#   6. Require either pool asset to satisfy rho = 5%.
#
# IMPORTANT:
#
# At this stage we are NOT yet:
#
#   - deciding whether addresses belong to the same entity,
#   - removing public-service addresses,
#   - checking Cross Combination,
#   - eliminating MEV.
#
# Those steps will be applied ONLY to groups that survive
# this coarse filter.
# ============================================================



# ============================================================
# PARAMETERS FROM THE PAPER
# ============================================================

TAU <- 200
RHO <- 0.05

MIN_GROUP_SIZE <- 2
MAX_GROUP_SIZE <- 10



# ============================================================
# TOKEN INFORMATION FOR THIS CONTROLLED POOL
# ============================================================

# This particular suspicious pool contains:
#
# token0 = RFI
# token1 = WETH
#
# Both use 18 decimals.
#
# These values are specific to this reduced empirical dataset.
# A general large-scale detector would resolve token metadata
# automatically.

TOKEN0_SYMBOL <- "RFI"
TOKEN1_SYMBOL <- "WETH"

TOKEN0_DECIMALS <- 18
TOKEN1_DECIMALS <- 18



# ============================================================
# FILE LOCATIONS
# ============================================================

input_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_suspicious_pool_raw_swaps.csv"


all_windows_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage1_all_windows.csv"


candidates_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage1_candidates.csv"


candidate_swaps_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage1_candidate_swaps.csv"



# ============================================================
# STEP 1 — LOAD RAW DUNE DATA
# ============================================================

# Read all fields as character initially.
#
# This protects:
#
#   - Ethereum addresses,
#   - transaction hashes,
#   - very large blockchain integer values.

swaps <- read.csv(
  input_file,
  colClasses = "character"
)


cat(
  "Raw swaps loaded:",
  nrow(swaps),
  "\n"
)



# ============================================================
# STEP 2 — BASIC DATA CHECKS
# ============================================================

required_columns <- c(

  "contract_address",
  "evt_tx_hash",
  "evt_tx_from",
  "evt_index",
  "evt_block_number",

  "amount0In",
  "amount0Out",
  "amount1In",
  "amount1Out"
)


missing_columns <-
  setdiff(
    required_columns,
    names(swaps)
  )


if (length(missing_columns) > 0) {

  stop(
    paste(
      "Missing required columns:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}



# ============================================================
# STEP 3 — NORMALIZE IDENTIFIERS
# ============================================================

swaps$contract_address <-
  tolower(swaps$contract_address)


swaps$evt_tx_hash <-
  tolower(swaps$evt_tx_hash)


swaps$evt_tx_from <-
  tolower(swaps$evt_tx_from)



# ============================================================
# STEP 4 — CONVERT BLOCKCHAIN ORDERING VARIABLES
# ============================================================

swaps$block_number <-
  as.integer(
    as.numeric(
      swaps$evt_block_number
    )
  )


swaps$event_index <-
  as.integer(
    swaps$evt_index
  )



# ============================================================
# STEP 5 — SORT CHRONOLOGICALLY
# ============================================================

# The paper works with adjacent swaps in chronological order.
#
# block_number gives the main order.
# evt_index determines event order within the same block.

swaps <- swaps[
  order(
    swaps$block_number,
    swaps$event_index
  ),
]


rownames(swaps) <- NULL



# Give every swap its chronological position in this pool.

swaps$swap_sequence <-
  seq_len(
    nrow(swaps)
  )



# ============================================================
# STEP 6 — CHECK FOR DUPLICATE SWAP EVENTS
# ============================================================

# A swap event should be uniquely represented by:
#
# transaction hash + event index.

duplicate_swap <-
  duplicated(
    paste(
      swaps$evt_tx_hash,
      swaps$event_index,
      sep = "_"
    )
  )


number_duplicates <-
  sum(duplicate_swap)


cat(
  "Duplicate swap events:",
  number_duplicates,
  "\n"
)


if (number_duplicates > 0) {

  warning(
    "Duplicate swap events were detected."
  )
}



# ============================================================
# STEP 7 — NORMALIZE TOKEN AMOUNTS
# ============================================================

# These are approximate numerical calculations used for the
# paper's 5% position-restoration threshold.
#
# We do not perform exact integer-equality comparisons here,
# unlike the rebase-arbitrage validation test.


swaps$amount0_in <-
  as.numeric(swaps$amount0In) /
  (10 ^ TOKEN0_DECIMALS)


swaps$amount0_out <-
  as.numeric(swaps$amount0Out) /
  (10 ^ TOKEN0_DECIMALS)


swaps$amount1_in <-
  as.numeric(swaps$amount1In) /
  (10 ^ TOKEN1_DECIMALS)


swaps$amount1_out <-
  as.numeric(swaps$amount1Out) /
  (10 ^ TOKEN1_DECIMALS)



# ============================================================
# STEP 8 — CALCULATE POOL POSITION CHANGES
# ============================================================

# From the POOL's perspective:
#
# positive delta = token entered pool
# negative delta = token left pool.


swaps$delta_token0 <-
  swaps$amount0_in -
  swaps$amount0_out


swaps$delta_token1 <-
  swaps$amount1_in -
  swaps$amount1_out



# ============================================================
# STEP 9 — FUNCTION FOR RESTORATION RATIO
# ============================================================

# Operational implementation:
#
#              | net position change |
# ratio = ---------------------------------
#            total absolute movement
#
#
# A value near zero means:
#
# a relatively large amount moved through the pool,
# but the final pool position changed only slightly.


restoration_ratio <- function(delta_values) {

  net_change <-
    sum(
      delta_values,
      na.rm = TRUE
    )


  gross_movement <-
    sum(
      abs(delta_values),
      na.rm = TRUE
    )


  if (
    gross_movement == 0
  ) {

    return(Inf)

  }


  abs(net_change) /
    gross_movement
}



# ============================================================
# STEP 10 — GENERATE ALL ADJACENT WINDOWS
# ============================================================

# For n swaps and group length g:
#
# number of windows = n - g + 1
#
# Every window contains CONSECUTIVE swaps only.


window_results <- list()

candidate_swap_rows <- list()


window_counter <- 1
candidate_counter <- 1



for (
  group_size in
  MIN_GROUP_SIZE:MAX_GROUP_SIZE
) {


  number_possible_windows <-
    nrow(swaps) -
    group_size +
    1


  if (
    number_possible_windows <= 0
  ) {

    next
  }


  for (
    start_position in
    seq_len(number_possible_windows)
  ) {


    end_position <-
      start_position +
      group_size -
      1


    group <-
      swaps[
        start_position:end_position,
      ]



    # ========================================================
    # STEP 10A — TIME CONDITION
    # ========================================================

    block_span <-
      max(group$block_number) -
      min(group$block_number)


    time_condition <-
      block_span <= TAU



    # ========================================================
    # STEP 10B — TOKEN0 RESTORATION
    # ========================================================

    ratio_token0 <-
      restoration_ratio(
        group$delta_token0
      )



    # ========================================================
    # STEP 10C — TOKEN1 RESTORATION
    # ========================================================

    ratio_token1 <-
      restoration_ratio(
        group$delta_token1
      )



    # ========================================================
    # STEP 10D — PAPER'S RESTORATION CONDITION
    # ========================================================

    # Approximate restoration of EITHER pool asset is
    # sufficient for the coarse-filter condition.

    restoration_condition <-
      ratio_token0 <= RHO |
      ratio_token1 <= RHO



    # ========================================================
    # STEP 10E — COMPLETE COARSE-FILTER DECISION
    # ========================================================

    coarse_filter_pass <-
      time_condition &&
      restoration_condition



    # ========================================================
    # STEP 10F — SAVE WINDOW INFORMATION
    # ========================================================

    window_results[[window_counter]] <-
      data.frame(

        window_id =
          window_counter,

        start_swap =
          group$swap_sequence[1],

        end_swap =
          group$swap_sequence[
            nrow(group)
          ],

        group_size =
          group_size,

        start_block =
          min(group$block_number),

        end_block =
          max(group$block_number),

        block_span =
          block_span,

        ratio_RFI =
          ratio_token0,

        ratio_WETH =
          ratio_token1,

        time_condition =
          time_condition,

        restoration_condition =
          restoration_condition,

        coarse_filter_pass =
          coarse_filter_pass,

        start_tx_hash =
          group$evt_tx_hash[1],

        end_tx_hash =
          group$evt_tx_hash[
            nrow(group)
          ],

        stringsAsFactors = FALSE
      )



    # ========================================================
    # STEP 10G — SAVE CONSTITUENT SWAPS FOR PASSING WINDOWS
    # ========================================================

    if (
      coarse_filter_pass
    ) {


      candidate_group <-
        group


      candidate_group$candidate_id <-
        candidate_counter


      candidate_group$candidate_group_size <-
        group_size


      candidate_swap_rows[[candidate_counter]] <-
        candidate_group


      candidate_counter <-
        candidate_counter + 1
    }


    window_counter <-
      window_counter + 1
  }
}



# ============================================================
# STEP 11 — COMBINE WINDOW RESULTS
# ============================================================

all_windows <-
  do.call(
    rbind,
    window_results
  )



# Keep only Stage-1 candidates.

candidates <-
  all_windows[
    all_windows$coarse_filter_pass,
  ]


rownames(candidates) <- NULL



# ============================================================
# STEP 12 — COMBINE CANDIDATE-SWAP DETAILS
# ============================================================

if (
  length(candidate_swap_rows) > 0
) {

  candidate_swaps <-
    do.call(
      rbind,
      candidate_swap_rows
    )

  rownames(candidate_swaps) <- NULL

} else {

  candidate_swaps <-
    data.frame()
}



# ============================================================
# STEP 13 — SANITY CHECK FOR NUMBER OF WINDOWS
# ============================================================

expected_windows <-
  sum(
    sapply(

      MIN_GROUP_SIZE:MAX_GROUP_SIZE,

      function(g) {

        max(
          nrow(swaps) - g + 1,
          0
        )
      }
    )
  )


actual_windows <-
  nrow(all_windows)


window_count_check <-
  expected_windows ==
  actual_windows



# ============================================================
# STEP 14 — SAVE OUTPUT FILES
# ============================================================

write.csv(
  all_windows,
  all_windows_output,
  row.names = FALSE
)


write.csv(
  candidates,
  candidates_output,
  row.names = FALSE
)


if (
  nrow(candidate_swaps) > 0
) {

  write.csv(
    candidate_swaps,
    candidate_swaps_output,
    row.names = FALSE
  )
}



# ============================================================
# STEP 15 — PRINT EMPIRICAL RESULT
# ============================================================

cat("\n")
cat("============================================\n")
cat("SUSPICIOUS POOL — STAGE 1 EMPIRICAL RESULT\n")
cat("============================================\n\n")


cat(
  "Pool address:",
  unique(swaps$contract_address),
  "\n"
)


cat(
  "Number of raw swaps:",
  nrow(swaps),
  "\n"
)


cat(
  "Group lengths tested:",
  MIN_GROUP_SIZE,
  "through",
  MAX_GROUP_SIZE,
  "\n"
)


cat(
  "Expected adjacent windows:",
  expected_windows,
  "\n"
)


cat(
  "Actual adjacent windows:",
  actual_windows,
  "\n"
)


cat(
  "Window-count sanity check:",
  window_count_check,
  "\n"
)


cat(
  "Tau:",
  TAU,
  "blocks\n"
)


cat(
  "Rho:",
  RHO,
  "\n"
)


cat(
  "Groups passing Stage 1:",
  nrow(candidates),
  "\n\n"
)



# ============================================================
# STEP 16 — PRINT PASSING CANDIDATES
# ============================================================

if (
  nrow(candidates) > 0
) {

  cat(
    "STAGE-1 CANDIDATES\n"
  )

  cat(
    "--------------------------------------------\n"
  )


  print(
    candidates[
      ,
      c(
        "window_id",
        "start_swap",
        "end_swap",
        "group_size",
        "block_span",
        "ratio_RFI",
        "ratio_WETH"
      )
    ]
  )


} else {

  cat(
    "No candidate groups passed Stage 1.\n"
  )
}



# ============================================================
# STEP 17 — OUTPUT LOCATIONS
# ============================================================

cat("\n")
cat("OUTPUT FILES\n")
cat("--------------------------------------------\n")


cat(
  "All windows:\n",
  all_windows_output,
  "\n\n"
)


cat(
  "Stage-1 candidates:\n",
  candidates_output,
  "\n\n"
)


if (
  nrow(candidate_swaps) > 0
) {

  cat(
    "Constituent candidate swaps:\n",
    candidate_swaps_output,
    "\n"
  )
}