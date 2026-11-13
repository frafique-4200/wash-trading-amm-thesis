# ============================================================
# EMPIRICAL SUSPICIOUS POOL
# STAGE 2 — SENDER / ENTITY RECOGNITION
#
# PURPOSE:
#
# Apply Stage 2 of the reduced AMM wash-trading methodology
# to the candidates that survived Stage 1.
#
# Stage 1 produced two candidate groups:
#
# Candidate 1 = swaps 17–18
# Candidate 2 = swaps 32–33
#
#
# STAGE-2 LOGIC
#
# 1. Identify transaction initiators using evt_tx_from.
#
# 2. If there is only one initiator:
#
#       entity condition automatically passes.
#
# 3. If there are multiple initiators:
#
#       first test whether every sender approximately restores
#       their own position (Cross Combination / Group 1).
#
# 4. Otherwise test entity recognition:
#
#       - direct ETH relationship;
#       - or ETH-transfer distance <= k = 2;
#       - known public-service intermediaries are removed.
#
#
# IMPORTANT:
#
# ERC-20 transfers are NOT used to establish entity
# relationships. The reference paper specifically uses the
# ETH-transfer graph for entity recognition.
#
# ============================================================



# ============================================================
# PARAMETERS
# ============================================================

# Position-restoration tolerance.

RHO <- 0.05


# Maximum ETH-transfer graph distance.

K <- 2



# ============================================================
# FILE LOCATIONS
# ============================================================

candidate_swaps_file <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage1_candidate_swaps.csv"


candidate2_intermediaries_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/candidate_2_common_intermediaries.csv"


candidate2_service_file <-
  "D:/Downloads/Bachelor Thesis/Data/raw/candidate_2_historical_service_check.csv"



# Output files.

stage2_summary_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage2_summary.csv"


stage2_surviving_swaps_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/suspicious_pool_stage2_surviving_swaps.csv"


candidate2_intermediary_output <-
  "D:/Downloads/Bachelor Thesis/Data/cleaned/candidate_2_cleaned_intermediaries.csv"



# ============================================================
# HELPER FUNCTION 1
# NORMALIZE DUNE NULL-LIKE VALUES
# ============================================================

# Dune exports missing values in several possible forms:
#
# <nil>
# NA
# ""
#
# This function makes later label checks easier.

has_real_value <- function(x) {

  !is.na(x) &
    x != "" &
    x != "<nil>" &
    tolower(x) != "null"
}



# ============================================================
# HELPER FUNCTION 2
# POSITION-RESTORATION RATIO
# ============================================================

# We use the same operational ratio used earlier:
#
#             | net position change |
# ratio = -------------------------------
#            total absolute movement
#
#
# A ratio close to zero means that the address traded but
# approximately returned to its initial position.

position_ratio <- function(delta_values) {

  delta_values <-
    as.numeric(delta_values)


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


  if (gross_movement == 0) {

    return(Inf)

  }


  abs(net_change) /
    gross_movement
}



# ============================================================
# STEP 1 — LOAD STAGE-1 CANDIDATE SWAPS
# ============================================================

candidate_swaps <- read.csv(
  candidate_swaps_file,
  colClasses = "character"
)



# Normalize transaction initiators.

candidate_swaps$evt_tx_from <-
  tolower(
    candidate_swaps$evt_tx_from
  )



# Convert candidate IDs and numerical token changes.

candidate_swaps$candidate_id <-
  as.integer(
    candidate_swaps$candidate_id
  )


candidate_swaps$swap_sequence <-
  as.integer(
    candidate_swaps$swap_sequence
  )


candidate_swaps$delta_token0 <-
  as.numeric(
    candidate_swaps$delta_token0
  )


candidate_swaps$delta_token1 <-
  as.numeric(
    candidate_swaps$delta_token1
  )



candidate_ids <-
  sort(
    unique(
      candidate_swaps$candidate_id
    )
  )



cat(
  "Stage-1 candidates loaded:",
  length(candidate_ids),
  "\n"
)



# ============================================================
# STEP 2 — CALCULATE SENDER-LEVEL POSITION RESTORATION
# ============================================================

# This implements the Group (1) / Cross Combination idea.
#
# IMPORTANT:
#
# The candidate must contain MORE THAN ONE sender.
#
# For every sender, we calculate position restoration
# separately.
#
# In this reduced implementation we calculate the ratio for
# both RFI and WETH.
#
# A sender is considered approximately restored when either
# asset satisfies rho.
#
# For Candidate 2 each sender only performs one swap, so both
# ratios will equal 1 and the conclusion does not depend on
# which token is selected.


cross_combination_results <- list()



for (candidate_id in candidate_ids) {


  group <-
    candidate_swaps[
      candidate_swaps$candidate_id == candidate_id,
    ]


  senders <-
    unique(
      group$evt_tx_from
    )


  # Cross Combination requires multiple senders.

  if (length(senders) < 2) {

    cross_combination_results[[as.character(candidate_id)]] <-
      list(

        detected = FALSE,

        sender_table = data.frame()
      )

    next
  }



  sender_results <- list()



  for (sender_address in senders) {


    sender_swaps <-
      group[
        group$evt_tx_from == sender_address,
      ]


    ratio_RFI <-
      position_ratio(
        sender_swaps$delta_token0
      )


    ratio_WETH <-
      position_ratio(
        sender_swaps$delta_token1
      )


    sender_restored <-
      ratio_RFI <= RHO |
      ratio_WETH <= RHO



    sender_results[[sender_address]] <-
      data.frame(

        candidate_id =
          candidate_id,

        sender =
          sender_address,

        number_swaps =
          nrow(sender_swaps),

        ratio_RFI =
          ratio_RFI,

        ratio_WETH =
          ratio_WETH,

        sender_restored =
          sender_restored,

        stringsAsFactors = FALSE
      )
  }



  sender_table <-
    do.call(
      rbind,
      sender_results
    )



  # Every sender must approximately restore their position.

  cross_detected <-
    all(
      sender_table$sender_restored
    )



  cross_combination_results[[as.character(candidate_id)]] <-
    list(

      detected =
        cross_detected,

      sender_table =
        sender_table
    )
}



# ============================================================
# STEP 3 — LOAD CANDIDATE-2 COMMON INTERMEDIARIES
# ============================================================

candidate2_intermediaries <- read.csv(
  candidate2_intermediaries_file,
  colClasses = "character"
)



candidate2_intermediaries$common_intermediary <-
  tolower(
    candidate2_intermediaries$common_intermediary
  )



candidate2_intermediaries <-
  unique(
    candidate2_intermediaries
  )



number_common_intermediaries <-
  nrow(
    candidate2_intermediaries
  )



# ============================================================
# STEP 4 — LOAD HISTORICAL PUBLIC-SERVICE CHECK
# ============================================================

# This CSV was generated by the Dune query saved as:
#
# "Empirical Suspicious Pool-- Stage 2F-- Candidate 2--
#  Historical Public Service Cross Check"
#
# It checks only the 32 intermediary addresses relevant to
# Candidate 2.
#
# It contains:
#
# - owner/entity labels;
# - category information;
# - CEX information;
# - CEX deposit information;
# - contract deployment information;
# - whether the contract existed by the candidate period.


service_check <- read.csv(
  candidate2_service_file,
  colClasses = "character"
)



service_check$address <-
  tolower(
    service_check$address
  )



# ============================================================
# STEP 5 — IDENTIFY SERVICE-LIKE ENTITY CATEGORIES
# ============================================================

# The reference paper removes known public-service addresses
# such as:
#
# - decentralized exchanges;
# - centralized exchanges;
# - lending protocols;
# - aggregators;
# - bridges;
# - mixers.
#
# We therefore identify those service categories in Dune's
# entity information.


service_category_pattern <-
  paste(

    c(
      "decentralized exchange",
      "centralized exchange",
      "exchange",
      "lending",
      "aggregator",
      "bridge",
      "mixer"
    ),

    collapse = "|"
  )



# Combine useful ownership/category information into one
# searchable text field.

service_identity_text <-
  paste(

    ifelse(
      has_real_value(service_check$owner_key),
      service_check$owner_key,
      ""
    ),

    ifelse(
      has_real_value(service_check$custody_owner),
      service_check$custody_owner,
      ""
    ),

    ifelse(
      has_real_value(service_check$account_owner),
      service_check$account_owner,
      ""
    ),

    ifelse(
      has_real_value(service_check$contract_name),
      service_check$contract_name,
      ""
    ),

    ifelse(
      has_real_value(service_check$entity_name),
      service_check$entity_name,
      ""
    ),

    ifelse(
      has_real_value(service_check$primary_category),
      service_check$primary_category,
      ""
    ),

    ifelse(
      has_real_value(service_check$cex_name),
      service_check$cex_name,
      ""
    ),

    ifelse(
      has_real_value(service_check$deposit_cex_name),
      service_check$deposit_cex_name,
      ""
    )
  )



# Identify addresses that have an explicit service-type
# category.

service_check$service_identity_found <-
  grepl(

    service_category_pattern,

    service_identity_text,

    ignore.case = TRUE
  ) |
  has_real_value(
    service_check$cex_name
  ) |
  has_real_value(
    service_check$deposit_cex_name
  )



# ============================================================
# STEP 6 — HISTORICAL CHECK
# ============================================================

# We do not want to remove an address merely because it became
# a service years later.
#
# For smart-contract services, the Dune query already checked
# whether the contract existed by the candidate period.


service_check$contract_existed_by_candidate_time <-
  tolower(
    service_check$contract_existed_by_candidate_time
  ) == "true"



# CEX deposit addresses may contain their own first-observed
# timestamp.
#
# We parse it only when available.

candidate_time <-
  as.POSIXct(
    "2020-12-02 19:27:15",
    tz = "UTC"
  )



service_check$deposit_historically_active <-
  FALSE



deposit_available <-
  has_real_value(
    service_check$deposit_first_block_time
  )



if (any(deposit_available)) {


  deposit_times <-
    as.POSIXct(

      service_check$deposit_first_block_time[
        deposit_available
      ],

      format =
        "%Y-%m-%d %H:%M:%OS UTC",

      tz = "UTC"
    )


  service_check$deposit_historically_active[
    deposit_available
  ] <-
    deposit_times <= candidate_time
}



# ============================================================
# STEP 7 — PUBLIC-SERVICE CLASSIFICATION
# ============================================================

# Three possible results:
#
# 1. PUBLIC_SERVICE_REMOVE
#
#    Explicit service identity AND historical evidence that the
#    service existed by the Candidate-2 period.
#
#
# 2. UNCERTAIN_SERVICE_TIMING
#
#    Service identity exists, but our reduced dataset does not
#    provide enough historical evidence to establish whether
#    it was already active at the candidate time.
#
#
# 3. NOT_IDENTIFIED_AS_SERVICE
#
#    No public-service identity was found in the Dune sources
#    used in this reduced replication.


service_check$service_status <-
  "NOT_IDENTIFIED_AS_SERVICE"



# Known smart-contract service that already existed.

known_contract_service <-
  service_check$service_identity_found &
  service_check$contract_existed_by_candidate_time



# Known CEX deposit address that was already observed.

known_deposit_service <-
  service_check$service_identity_found &
  service_check$deposit_historically_active



service_check$service_status[
  known_contract_service |
  known_deposit_service
] <-
  "PUBLIC_SERVICE_REMOVE"



# Service identity exists but historical timing is unresolved.

uncertain_service <-
  service_check$service_identity_found &
  service_check$service_status !=
    "PUBLIC_SERVICE_REMOVE"



service_check$service_status[
  uncertain_service
] <-
  "UNCERTAIN_SERVICE_TIMING"



# ============================================================
# STEP 8 — JOIN SERVICE RESULTS TO COMMON INTERMEDIARIES
# ============================================================

candidate2_cleaned <-
  merge(

    candidate2_intermediaries,

    service_check[
      ,
      c(
        "address",
        "owner_key",
        "custody_owner",
        "entity_name",
        "primary_category",
        "contract_deployed_at",
        "contract_existed_by_candidate_time",
        "service_identity_found",
        "service_status"
      )
    ],

    by.x = "common_intermediary",

    by.y = "address",

    all.x = TRUE
  )



# If an intermediary somehow did not appear in the historical
# service-check file, mark it as unresolved rather than
# silently treating it as a normal address.

missing_service_check <-
  is.na(
    candidate2_cleaned$service_status
  )


candidate2_cleaned$service_status[
  missing_service_check
] <-
  "SERVICE_CHECK_MISSING"



# ============================================================
# STEP 9 — REMOVE KNOWN PUBLIC-SERVICE INTERMEDIARIES
# ============================================================

public_service_removed <-
  candidate2_cleaned$service_status ==
  "PUBLIC_SERVICE_REMOVE"



number_public_services <-
  sum(
    public_service_removed
  )



# Only addresses not identified as services are accepted as
# usable 2-hop intermediaries.
#
# UNCERTAIN addresses are NOT counted as valid until checked.

valid_intermediary <-
  candidate2_cleaned$service_status ==
  "NOT_IDENTIFIED_AS_SERVICE"



candidate2_cleaned$valid_two_hop_intermediary <-
  valid_intermediary



number_valid_intermediaries <-
  sum(
    valid_intermediary
  )



number_uncertain_intermediaries <-
  sum(

    candidate2_cleaned$service_status ==
      "UNCERTAIN_SERVICE_TIMING"

  ) +
  sum(

    candidate2_cleaned$service_status ==
      "SERVICE_CHECK_MISSING"

  )



# ============================================================
# STEP 10 — DIRECT ETH-TRANSFER RESULT FOR CANDIDATE 2
# ============================================================

# We previously ran the Candidate-2 direct ETH-transfer Dune
# query.
#
# Dune returned:
#
#       "No results from query"
#
# Therefore no direct ETH-transfer edge A-B was observed.


candidate2_direct_eth_relationship <-
  FALSE



# ============================================================
# STEP 11 — CANDIDATE-2 ENTITY DISTANCE
# ============================================================

# Logic:
#
# Direct ETH edge:
#
#       distance = 1
#
# Otherwise, if at least one non-excluded common intermediary
# remains:
#
#       A -- C -- B
#
#       distance = 2
#
# Otherwise:
#
#       distance = Inf


if (candidate2_direct_eth_relationship) {


  candidate2_entity_distance <-
    1


} else if (
  number_valid_intermediaries > 0
) {


  candidate2_entity_distance <-
    2


} else {


  candidate2_entity_distance <-
    Inf
}



candidate2_entity_pass <-
  candidate2_entity_distance <= K



# ============================================================
# STEP 12 — BUILD CANDIDATE-BY-CANDIDATE STAGE-2 SUMMARY
# ============================================================

stage2_results <- list()



for (candidate_id in candidate_ids) {


  group <-
    candidate_swaps[
      candidate_swaps$candidate_id == candidate_id,
    ]


  senders <-
    unique(
      group$evt_tx_from
    )


  number_senders <-
    length(senders)



  # ----------------------------------------------------------
  # Same-sender condition
  # ----------------------------------------------------------

  same_sender <-
    number_senders == 1



  # ----------------------------------------------------------
  # Cross Combination condition
  # ----------------------------------------------------------

  cross_combination <-
    cross_combination_results[[as.character(candidate_id)]]$detected
    

  # ----------------------------------------------------------
  # Entity relationship
  # ----------------------------------------------------------

  if (same_sender) {


    entity_distance <- 0

    entity_relationship <- TRUE


  } else if (
    candidate_id == 2
  ) {


    entity_distance <-
      candidate2_entity_distance


    entity_relationship <-
      candidate2_entity_pass


  } else {


    # We currently only collected ETH evidence for Candidate 2.
    #
    # If additional multi-sender candidates appear in future
    # data, their entity evidence must be collected separately.

    entity_distance <- NA

    entity_relationship <- FALSE
  }



  # ----------------------------------------------------------
  # Complete Stage-2 decision
  #
  # Candidate survives Stage 2 when:
  #
  # - same sender
  # OR
  # - Cross Combination
  # OR
  # - sender addresses satisfy entity recognition.
  # ----------------------------------------------------------

  stage2_pass <-
    same_sender |
    cross_combination |
    entity_relationship



  stage2_results[[as.character(candidate_id)]] <-
    data.frame(

      candidate_id =
        candidate_id,

      start_swap =
        min(group$swap_sequence),

      end_swap =
        max(group$swap_sequence),

      number_swaps =
        nrow(group),

      number_senders =
        number_senders,

      same_sender =
        same_sender,

      cross_combination =
        cross_combination,

      entity_distance =
        entity_distance,

      entity_relationship =
        entity_relationship,

      stage2_pass =
        stage2_pass,

      stringsAsFactors = FALSE
    )
}



stage2_summary <-
  do.call(
    rbind,
    stage2_results
  )


rownames(stage2_summary) <- NULL



# ============================================================
# STEP 13 — RETAIN SWAPS FROM STAGE-2 CANDIDATES
# ============================================================

passing_candidate_ids <-
  stage2_summary$candidate_id[
    stage2_summary$stage2_pass
  ]



stage2_surviving_swaps <-
  candidate_swaps[
    candidate_swaps$candidate_id %in%
      passing_candidate_ids,
  ]



# ============================================================
# STEP 14 — SAVE OUTPUTS
# ============================================================

write.csv(
  stage2_summary,
  stage2_summary_output,
  row.names = FALSE
)


write.csv(
  stage2_surviving_swaps,
  stage2_surviving_swaps_output,
  row.names = FALSE
)


write.csv(
  candidate2_cleaned,
  candidate2_intermediary_output,
  row.names = FALSE
)



# ============================================================
# STEP 15 — PRINT RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("SUSPICIOUS POOL — STAGE 2 EMPIRICAL RESULT\n")
cat("============================================\n\n")



# ============================================================
# CANDIDATE 2 SERVICE-CLEANING INFORMATION
# ============================================================

cat("CANDIDATE 2 — ETH ENTITY EVIDENCE\n")
cat("--------------------------------------------\n")


cat(
  "Direct ETH relationship:",
  candidate2_direct_eth_relationship,
  "\n"
)


cat(
  "Common 2-hop intermediaries:",
  number_common_intermediaries,
  "\n"
)


cat(
  "Known historical public services removed:",
  number_public_services,
  "\n"
)


cat(
  "Uncertain service addresses:",
  number_uncertain_intermediaries,
  "\n"
)


cat(
  "Non-excluded 2-hop intermediaries:",
  number_valid_intermediaries,
  "\n"
)


cat(
  "Candidate 2 entity distance:",
  candidate2_entity_distance,
  "\n"
)


cat(
  "Candidate 2 entity condition:",
  candidate2_entity_pass,
  "\n\n"
)



# ============================================================
# COMPLETE CANDIDATE SUMMARY
# ============================================================

cat("STAGE-2 CANDIDATE SUMMARY\n")
cat("--------------------------------------------\n")


print(
  stage2_summary
)



# ============================================================
# OVERALL RESULT
# ============================================================

cat("\n")
cat(
  "Candidates entering Stage 2:",
  nrow(stage2_summary),
  "\n"
)


cat(
  "Candidates surviving Stage 2:",
  sum(stage2_summary$stage2_pass),
  "\n"
)



# ============================================================
# OUTPUT LOCATIONS
# ============================================================

cat("\n")
cat("OUTPUT FILES\n")
cat("--------------------------------------------\n")


cat(
  "Stage-2 summary:\n",
  stage2_summary_output,
  "\n\n"
)


cat(
  "Stage-2 surviving swaps:\n",
  stage2_surviving_swaps_output,
  "\n\n"
)


cat(
  "Candidate-2 cleaned intermediaries:\n",
  candidate2_intermediary_output,
  "\n"
)