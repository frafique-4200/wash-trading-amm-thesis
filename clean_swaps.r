suspicious <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_suspicious_pool_raw_swaps.csv",
  colClasses = "character"
)

control <- read.csv(
  "D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_control_pool_raw_swaps.csv",
  colClasses = "character"
)

suspicious$block_number <- as.integer(as.numeric(suspicious$evt_block_number))
suspicious$event_index  <- as.integer(suspicious$evt_index)
suspicious$block_time <- as.POSIXct(
  suspicious$evt_block_time,
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "UTC"
)

suspicious <- suspicious[
  order(suspicious$block_number, suspicious$event_index),
]

suspicious$amount0_in  <- as.numeric(suspicious$amount0In)  / 1e18
suspicious$amount0_out <- as.numeric(suspicious$amount0Out) / 1e18
suspicious$amount1_in  <- as.numeric(suspicious$amount1In)  / 1e18
suspicious$amount1_out <- as.numeric(suspicious$amount1Out) / 1e18

suspicious$delta_token0 <- suspicious$amount0_in - suspicious$amount0_out
suspicious$delta_token1 <- suspicious$amount1_in - suspicious$amount1_out


control$block_number <- as.integer(as.numeric(control$evt_block_number))
control$event_index  <- as.integer(control$evt_index)
control$block_time <- as.POSIXct(
  control$evt_block_time,
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "UTC"
)
control <- control[
  order(control$block_number, control$event_index),
]
control$amount0_in  <- as.numeric(control$amount0In)  / 1e18
control$amount0_out <- as.numeric(control$amount0Out) / 1e18
control$amount1_in  <- as.numeric(control$amount1In)  / 1e18
control$amount1_out <- as.numeric(control$amount1Out) / 1e18

control$delta_token0 <- control$amount0_in - control$amount0_out
control$delta_token1 <- control$amount1_in - control$amount1_out

suspicious$swap_sequence <- seq_len(nrow(suspicious))

control$swap_sequence <- seq_len(nrow(control))

two_swap_groups <- data.frame(
  start_swap = 1:(nrow(suspicious) - 1),
  end_swap   = 2:nrow(suspicious)
)

candidate_groups <- data.frame()

for (window_size in 2:10) {
  for (start_swap in 1:(nrow(suspicious) - window_size + 1)) {
    
    candidate_groups <- rbind(
      candidate_groups,
      data.frame(
        window_size = window_size,
        start_swap = start_swap,
        end_swap = start_swap + window_size - 1
      )
    )
    
  }
}

control_candidate_groups <- data.frame()

for (window_size in 2:10) {
  for (start_swap in 1:(nrow(control) - window_size + 1)) {

    control_candidate_groups <- rbind(
      control_candidate_groups,
      data.frame(
        window_size = window_size,
        start_swap = start_swap,
        end_swap = start_swap + window_size - 1
      )
    )

  }
}

candidate_groups$ratio_token0 <- NA_real_
candidate_groups$ratio_token1 <- NA_real_

for (i in seq_len(nrow(candidate_groups))) {

  start <- candidate_groups$start_swap[i]
  end   <- candidate_groups$end_swap[i]

  group_token0 <- suspicious$delta_token0[start:end]
  group_token1 <- suspicious$delta_token1[start:end]

  candidate_groups$ratio_token0[i] <-
    abs(sum(group_token0)) / sum(abs(group_token0))

  candidate_groups$ratio_token1[i] <-
    abs(sum(group_token1)) / sum(abs(group_token1))
}

control_candidate_groups$ratio_token0 <- NA_real_
control_candidate_groups$ratio_token1 <- NA_real_

for (i in seq_len(nrow(control_candidate_groups))) {

  start <- control_candidate_groups$start_swap[i]
  end   <- control_candidate_groups$end_swap[i]

  group_token0 <- control$delta_token0[start:end]
  group_token1 <- control$delta_token1[start:end]

  control_candidate_groups$ratio_token0[i] <-
    abs(sum(group_token0)) / sum(abs(group_token0))

  control_candidate_groups$ratio_token1[i] <-
    abs(sum(group_token1)) / sum(abs(group_token1))
}

candidate_groups$block_span <- NA_integer_

for (i in seq_len(nrow(candidate_groups))) {
  start <- candidate_groups$start_swap[i]
  end   <- candidate_groups$end_swap[i]

  candidate_groups$block_span[i] <-
    suspicious$block_number[end] - suspicious$block_number[start]
}

control_candidate_groups$block_span <- NA_integer_

for (i in seq_len(nrow(control_candidate_groups))) {
  start <- control_candidate_groups$start_swap[i]
  end   <- control_candidate_groups$end_swap[i]

  control_candidate_groups$block_span[i] <-
    control$block_number[end] - control$block_number[start]
}


write.csv(
  suspicious,
  "D:/Downloads/Bachelor Thesis/Data/cleaned/uniswap_v2_suspicious_pool_cleaned.csv",
  row.names = FALSE
)

write.csv(
  control,
  "D:/Downloads/Bachelor Thesis/Data/cleaned/uniswap_v2_control_pool_cleaned.csv",
  row.names = FALSE
)

