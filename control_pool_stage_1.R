# ============================================================
# CONTROL POOL
# STAGE 1 — COARSE FILTERING
#
# SAME DETECTION LOGIC AS:
# suspicious_pool_stage_1.R
#
# Only dataset-specific information is changed.
# ============================================================


# ============================================================
# PARAMETERS
# ============================================================

tau <- 200

rho <- 0.05

min_group_size <- 2

max_group_size <- 10


# ============================================================
# CONTROL-POOL TOKEN INFORMATION
# ============================================================

token0_name <- "WETH"
token1_name <- "ANON"

token0_decimals <- 18
token1_decimals <- 18


# ============================================================
# FILE LOCATIONS
# ============================================================

input_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_control_pool_raw_swaps.csv"


all_windows_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/control_pool_stage1_all_windows.csv"


candidates_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/control_pool_stage1_candidates.csv"


candidate_swaps_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/control_pool_stage1_candidate_swaps.csv"


# ============================================================
# LOAD RAW SWAPS
# ============================================================

swaps <- read.csv(
  input_file,
  colClasses = "character",
  check.names = FALSE
)


cat(
  "Raw swaps loaded:",
  nrow(swaps),
  "\n"
)


# ============================================================
# CHECK REQUIRED COLUMNS
# ============================================================

required_columns <- c(
  "contract_address",
  "evt_tx_hash",
  "evt_tx_from",
  "evt_block_number",
  "evt_index",
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
      "Missing required column(s):",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# NORMALIZE ADDRESSES AND HASHES
# ============================================================

swaps$contract_address <-
  tolower(
    swaps$contract_address
  )


swaps$evt_tx_hash <-
  tolower(
    swaps$evt_tx_hash
  )


swaps$evt_tx_from <-
  tolower(
    swaps$evt_tx_from
  )


# ============================================================
# CONVERT BLOCK NUMBER AND EVENT INDEX
# ============================================================

swaps$evt_block_number_numeric <-
  as.numeric(
    swaps$evt_block_number
  )


swaps$evt_index_numeric <-
  as.numeric(
    swaps$evt_index
  )


# ============================================================
# CHECK FOR DUPLICATE SWAP EVENTS
# ============================================================

duplicate_swap_events <-
  duplicated(
    paste(
      swaps$evt_tx_hash,
      swaps$evt_index,
      sep = "_"
    )
  )


number_duplicate_swaps <-
  sum(
    duplicate_swap_events
  )


cat(
  "Duplicate swap events:",
  number_duplicate_swaps,
  "\n"
)


if (number_duplicate_swaps > 0) {

  swaps <-
    swaps[
      !duplicate_swap_events,
    ]
}


# ============================================================
# SORT CHRONOLOGICALLY
# ============================================================

swaps <-
  swaps[
    order(
      swaps$evt_block_number_numeric,
      swaps$evt_index_numeric
    ),
  ]


rownames(swaps) <- NULL


swaps$swap_sequence <-
  seq_len(
    nrow(swaps)
  )


# ============================================================
# CONVERT RAW TOKEN AMOUNTS
# ============================================================

swaps$amount0In_token <-
  as.numeric(
    swaps$amount0In
  ) /
  (10 ^ token0_decimals)


swaps$amount0Out_token <-
  as.numeric(
    swaps$amount0Out
  ) /
  (10 ^ token0_decimals)


swaps$amount1In_token <-
  as.numeric(
    swaps$amount1In
  ) /
  (10 ^ token1_decimals)


swaps$amount1Out_token <-
  as.numeric(
    swaps$amount1Out
  ) /
  (10 ^ token1_decimals)


# ============================================================
# POSITION CHANGES
# ============================================================

swaps$delta_WETH <-
  swaps$amount0Out_token -
  swaps$amount0In_token


swaps$delta_ANON <-
  swaps$amount1Out_token -
  swaps$amount1In_token


# ============================================================
# RESTORATION-RATIO FUNCTION
# ============================================================

restoration_ratio <- function(delta_values) {

  net_position_change <-
    abs(
      sum(
        delta_values,
        na.rm = TRUE
      )
    )


  gross_position_change <-
    sum(
      abs(
        delta_values
      ),
      na.rm = TRUE
    )


  if (gross_position_change == 0) {

    return(NA_real_)
  }


  net_position_change /
    gross_position_change
}


# ============================================================
# EXPECTED NUMBER OF ADJACENT WINDOWS
# ============================================================

number_swaps <-
  nrow(swaps)


group_sizes <-
  min_group_size:max_group_size


expected_adjacent_windows <-
  sum(
    sapply(
      group_sizes,
      function(group_size) {

        max(
          0,
          number_swaps -
            group_size +
            1
        )
      }
    )
  )


# ============================================================
# CREATE ALL ADJACENT WINDOWS
# ============================================================

window_results <- list()

window_id <- 0


for (group_size in group_sizes) {

  number_windows <-
    number_swaps -
    group_size +
    1


  if (number_windows <= 0) {

    next
  }


  for (start_swap in seq_len(number_windows)) {

    window_id <-
      window_id + 1


    end_swap <-
      start_swap +
      group_size -
      1


    current_window <-
      swaps[
        start_swap:end_swap,
      ]


    first_block <-
      min(
        current_window$evt_block_number_numeric
      )


    last_block <-
      max(
        current_window$evt_block_number_numeric
      )


    block_span <-
      last_block -
      first_block


    # --------------------------------------------------------
    # TIME CONDITION
    # --------------------------------------------------------

    time_condition <-
      block_span <= tau


    # --------------------------------------------------------
    # RESTORATION RATIOS
    # --------------------------------------------------------

    ratio_WETH <-
      restoration_ratio(
        current_window$delta_WETH
      )


    ratio_ANON <-
      restoration_ratio(
        current_window$delta_ANON
      )


    # --------------------------------------------------------
    # RESTORATION CONDITION
    #
    # Same rule as suspicious pool:
    # either asset may satisfy rho.
    # --------------------------------------------------------

    restoration_condition <-
      (
        !is.na(ratio_WETH) &&
          ratio_WETH <= rho
      ) ||
      (
        !is.na(ratio_ANON) &&
          ratio_ANON <= rho
      )


    # --------------------------------------------------------
    # STAGE-1 PASS
    # --------------------------------------------------------

    stage1_pass <-
      time_condition &&
      restoration_condition


    # --------------------------------------------------------
    # STORE RESULT
    #
    # IMPORTANT:
    # [[ ... ]] is deliberately kept on ONE line.
    # --------------------------------------------------------

    window_results[[length(window_results) + 1]] <-
      data.frame(

        window_id =
          window_id,

        start_swap =
          start_swap,

        end_swap =
          end_swap,

        group_size =
          group_size,

        first_block =
          first_block,

        last_block =
          last_block,

        block_span =
          block_span,

        ratio_WETH =
          ratio_WETH,

        ratio_ANON =
          ratio_ANON,

        time_condition =
          time_condition,

        restoration_condition =
          restoration_condition,

        stage1_pass =
          stage1_pass,

        stringsAsFactors = FALSE
      )
  }
}


# ============================================================
# COMBINE ALL WINDOW RESULTS
# ============================================================

all_windows <-
  do.call(
    rbind,
    window_results
  )


rownames(all_windows) <- NULL


actual_adjacent_windows <-
  nrow(
    all_windows
  )


window_count_sanity_check <-
  actual_adjacent_windows ==
  expected_adjacent_windows


# ============================================================
# EXTRACT STAGE-1 CANDIDATES
# ============================================================

stage1_candidates <-
  all_windows[
    all_windows$stage1_pass,
  ]


rownames(stage1_candidates) <- NULL


if (nrow(stage1_candidates) > 0) {

  stage1_candidates$candidate_id <-
    seq_len(
      nrow(stage1_candidates)
    )


  stage1_candidates <-
    stage1_candidates[
      ,
      c(
        "candidate_id",
        setdiff(
          names(stage1_candidates),
          "candidate_id"
        )
      )
    ]
}


# ============================================================
# EXTRACT CONSTITUENT SWAPS
# ============================================================

candidate_swap_list <- list()


if (nrow(stage1_candidates) > 0) {

  for (
    candidate_row in seq_len(
      nrow(stage1_candidates)
    )
  ) {

    candidate_id <-
      stage1_candidates$candidate_id[
        candidate_row
      ]


    candidate_window_id <-
      stage1_candidates$window_id[
        candidate_row
      ]


    candidate_group_size <-
      stage1_candidates$group_size[
        candidate_row
      ]


    candidate_start_swap <-
      stage1_candidates$start_swap[
        candidate_row
      ]


    candidate_end_swap <-
      stage1_candidates$end_swap[
        candidate_row
      ]


    candidate_swaps <-
      swaps[
        candidate_start_swap:
          candidate_end_swap,
      ]


    candidate_swaps$candidate_id <-
      candidate_id


    candidate_swaps$window_id <-
      candidate_window_id


    candidate_swaps$group_size <-
      candidate_group_size


    candidate_swaps <-
      candidate_swaps[
        ,
        c(
          "candidate_id",
          "window_id",
          "group_size",
          setdiff(
            names(candidate_swaps),
            c(
              "candidate_id",
              "window_id",
              "group_size"
            )
          )
        )
      ]


    # IMPORTANT:
    # Again, keep [[ ... ]] together on one line.

    candidate_swap_list[[length(candidate_swap_list) + 1]] <-
      candidate_swaps
  }
}


# ============================================================
# COMBINE CANDIDATE SWAPS
# ============================================================

if (length(candidate_swap_list) > 0) {

  stage1_candidate_swaps <-
    do.call(
      rbind,
      candidate_swap_list
    )


  rownames(stage1_candidate_swaps) <-
    NULL

} else {

  stage1_candidate_swaps <-
    data.frame()
}


# ============================================================
# SAVE OUTPUTS
# ============================================================

write.csv(
  all_windows,
  all_windows_output,
  row.names = FALSE
)


write.csv(
  stage1_candidates,
  candidates_output,
  row.names = FALSE
)


write.csv(
  stage1_candidate_swaps,
  candidate_swaps_output,
  row.names = FALSE
)


# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n")

cat(
  "============================================\n"
)

cat(
  "CONTROL POOL — STAGE 1 EMPIRICAL RESULT\n"
)

cat(
  "============================================\n\n"
)


pool_addresses <-
  unique(
    swaps$contract_address
  )


cat(
  "Pool address:",
  paste(
    pool_addresses,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Number of raw swaps:",
  number_swaps,
  "\n"
)


cat(
  "Group lengths tested:",
  min_group_size,
  "through",
  max_group_size,
  "\n"
)


cat(
  "Expected adjacent windows:",
  expected_adjacent_windows,
  "\n"
)


cat(
  "Actual adjacent windows:",
  actual_adjacent_windows,
  "\n"
)


cat(
  "Window-count sanity check:",
  window_count_sanity_check,
  "\n"
)


cat(
  "Tau:",
  tau,
  "blocks\n"
)


cat(
  "Rho:",
  rho,
  "\n"
)


cat(
  "Groups passing Stage 1:",
  nrow(stage1_candidates),
  "\n\n"
)


# ============================================================
# PRINT CANDIDATES
# ============================================================

if (nrow(stage1_candidates) > 0) {

  cat(
    "STAGE-1 CANDIDATES\n"
  )


  cat(
    "--------------------------------------------\n"
  )


  print(
    stage1_candidates[
      ,
      c(
        "candidate_id",
        "window_id",
        "start_swap",
        "end_swap",
        "group_size",
        "block_span",
        "ratio_WETH",
        "ratio_ANON"
      )
    ],
    row.names = FALSE
  )

} else {

  cat(
    "No groups passed Stage 1.\n"
  )
}


# ============================================================
# OUTPUT FILES
# ============================================================

cat("\n\n")

cat(
  "OUTPUT FILES\n"
)

cat(
  "--------------------------------------------\n"
)


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


cat(
  "Constituent candidate swaps:\n",
  candidate_swaps_output,
  "\n"
)