# ============================================================
# ENTITY-RECOGNITION STRUCTURAL UNIT TEST
#
# PURPOSE:
#
# Validate the GENERAL entity-recognition logic described
# in Section 7.2 / Algorithm 2 of the AMM wash-trading paper.
#
# This is NOT a historical blockchain example.
#
# Instead, we construct a small artificial ETH-transfer graph
# where the correct relationships are known in advance.
#
# We test:
#
#   1. Same sender
#   2. Direct relationship (1 hop)
#   3. Indirect relationship (2 hops)
#   4. Disconnected addresses
#   5. False relationship through a public service address
#   6. Multi-sender connected-graph logic
#
# PAPER PARAMETER:
#
#       k = 2
#
# ============================================================


# ============================================================
# LOAD PACKAGE
# ============================================================

library(igraph)


# ============================================================
# PARAMETER
# ============================================================

K <- 2


# ============================================================
# STEP 1 — CREATE SYNTHETIC ETH-TRANSFER GRAPH
# ============================================================

# Each edge represents at least one ETH transfer.
#
# The graph is UNDIRECTED because the paper uses an undirected
# ETH-transfer network for entity recognition.
#
#
# TEST STRUCTURES:
#
# A ---- B
#
# Direct relationship.
#
#
# C ---- M ---- D
#
# C and D are two hops apart.
#
#
# E        F
#
# No relationship.
#
#
# G ---- SERVICE ---- H
#
# G and H appear two hops apart BEFORE removing the service
# address. After SERVICE is removed they must become
# disconnected.
#
#
# J ---- X ---- K ---- Y ---- L
#
# Distances:
#
# J to K = 2
# K to L = 2
# J to L = 4
#
# Nevertheless the sender graph {J,K,L} is connected:
#
# J ---- K ---- L
#
# This tests an important part of Algorithm 2:
#
# every sender does NOT need to be within 2 hops of every
# other sender.
#
# The resulting G_sender only needs to be connected.
# ============================================================


edges <- data.frame(

  from = c(
    "a",
    "c",
    "m",
    "g",
    "service",
    "j",
    "x",
    "k",
    "y"
  ),

  to = c(
    "b",
    "m",
    "d",
    "service",
    "h",
    "x",
    "k",
    "y",
    "l"
  ),

  stringsAsFactors = FALSE
)


# We explicitly list every vertex so isolated addresses
# E and F are also present in the graph.

vertices <- data.frame(

  name = c(
    "a", "b",
    "c", "m", "d",
    "e", "f",
    "g", "service", "h",
    "j", "x", "k", "y", "l"
  ),

  stringsAsFactors = FALSE
)


eth_graph_raw <- graph_from_data_frame(

  d = edges,

  directed = FALSE,

  vertices = vertices
)


# ============================================================
# STEP 2 — IDENTIFY PUBLIC SERVICE ADDRESSES
# ============================================================

# In the real implementation these would come from labeled
# DEX, CEX, lending, bridge, aggregator, mixer, etc. addresses.
#
# For this controlled test we create one artificial service
# address.

service_addresses <- c(
  "service"
)


# ============================================================
# STEP 3 — REMOVE SERVICE ADDRESSES
# ============================================================

# The paper removes service addresses and all associated edges
# BEFORE calculating entity relationships.
#
# This prevents high-degree exchanges and other services from
# falsely linking unrelated traders.

eth_graph_clean <- delete_vertices(

  eth_graph_raw,

  V(eth_graph_raw)[
    name %in% service_addresses
  ]
)


# ============================================================
# STEP 4 — SAFE SHORTEST-PATH FUNCTION
# ============================================================

# This function returns the shortest-path distance between
# two addresses.
#
# If no path exists:
#
#       distance = Inf
#
# which means the addresses are not considered related.

get_distance <- function(graph, address_a, address_b) {

  address_a <- tolower(address_a)
  address_b <- tolower(address_b)


  # If an address is not present, we cannot establish a
  # relationship.

  if (
    !(address_a %in% V(graph)$name) ||
    !(address_b %in% V(graph)$name)
  ) {

    return(Inf)

  }


  d <- distances(

    graph,

    v = address_a,

    to = address_b,

    mode = "all"
  )[1, 1]


  return(
    as.numeric(d)
  )
}


# ============================================================
# STEP 5 — GENERAL ENTITY-RECOGNITION FUNCTION
# ============================================================

# INPUT:
#
# senders
#     initiating addresses belonging to one suspicious
#     swap group.
#
# eth_graph
#     cleaned ETH-transfer graph.
#
# k
#     maximum allowed ETH-transfer distance.
#
#
# OUTPUT:
#
# TRUE
#     sender group satisfies the paper's entity-recognition
#     condition.
#
# FALSE
#     sender group does not satisfy it.

entity_group_connected <- function(
    senders,
    eth_graph,
    k = 2
) {

  # Normalize and remove duplicate sender addresses.

  senders <-
    unique(
      tolower(senders)
    )


  # ----------------------------------------------------------
  # PAPER CASE 1:
  # Only one sender.
  #
  # Algorithm 2 automatically keeps such a group.
  # ----------------------------------------------------------

  if (length(senders) == 1) {

    return(TRUE)

  }


  # ----------------------------------------------------------
  # Create G_sender.
  #
  # Initially it contains sender nodes but NO edges.
  # ----------------------------------------------------------

  sender_graph <-
    make_empty_graph(
      n = length(senders),
      directed = FALSE
    )


  V(sender_graph)$name <-
    senders


  # ----------------------------------------------------------
  # Generate every pair of senders.
  # ----------------------------------------------------------

  sender_pairs <-
    combn(
      senders,
      2,
      simplify = FALSE
    )


  # ----------------------------------------------------------
  # Add an edge to G_sender when:
  #
  # distance(sender_a, sender_b) <= k
  #
  # in the cleaned ETH-transfer graph.
  # ----------------------------------------------------------

  for (pair in sender_pairs) {

    sender_a <- pair[1]
    sender_b <- pair[2]


    d <-
      get_distance(
        eth_graph,
        sender_a,
        sender_b
      )


    if (
      is.finite(d) &&
      d <= k
    ) {

      sender_graph <-
        add_edges(
          sender_graph,
          c(sender_a, sender_b)
        )
    }
  }


  # ----------------------------------------------------------
  # PAPER FINAL CONDITION:
  #
  # G_sender must be connected.
  # ----------------------------------------------------------

  graph_is_connected <-
    components(sender_graph)$no == 1


  return(
    graph_is_connected
  )
}


# ============================================================
# UNIT TEST 1 — SAME SENDER
# ============================================================

# The same initiating address appears repeatedly.
#
# EXPECTED:
#
# TRUE

test_same_sender <-
  entity_group_connected(

    senders = c("a", "a"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# UNIT TEST 2 — DIRECT / ONE-HOP RELATIONSHIP
# ============================================================

# Graph:
#
# A ---- B
#
# distance(A,B) = 1
#
# EXPECTED:
#
# TRUE

distance_A_B <-
  get_distance(
    eth_graph_clean,
    "a",
    "b"
  )


test_one_hop <-
  entity_group_connected(

    senders = c("a", "b"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# UNIT TEST 3 — TWO-HOP RELATIONSHIP
# ============================================================

# Graph:
#
# C ---- M ---- D
#
# distance(C,D) = 2
#
# Since:
#
#       2 <= k
#
# EXPECTED:
#
# TRUE

distance_C_D <-
  get_distance(
    eth_graph_clean,
    "c",
    "d"
  )


test_two_hop <-
  entity_group_connected(

    senders = c("c", "d"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# UNIT TEST 4 — DISCONNECTED ADDRESSES
# ============================================================

# Graph:
#
# E         F
#
# There is no ETH-transfer path between them.
#
# EXPECTED:
#
# FALSE

distance_E_F <-
  get_distance(
    eth_graph_clean,
    "e",
    "f"
  )


test_disconnected <-
  entity_group_connected(

    senders = c("e", "f"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# UNIT TEST 5 — SERVICE-ADDRESS REMOVAL
# ============================================================

# BEFORE cleaning:
#
# G ---- SERVICE ---- H
#
# distance(G,H) = 2
#
# A naive graph would therefore classify G and H as related.
#
#
# AFTER cleaning:
#
# G               H
#
# SERVICE and its edges have been removed.
#
# Therefore:
#
# distance(G,H) = Inf
#
# EXPECTED FINAL ENTITY RESULT:
#
# FALSE


distance_G_H_before <-
  get_distance(
    eth_graph_raw,
    "g",
    "h"
  )


distance_G_H_after <-
  get_distance(
    eth_graph_clean,
    "g",
    "h"
  )


test_service_removal <-
  entity_group_connected(

    senders = c("g", "h"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# UNIT TEST 6 — MULTI-SENDER CONNECTIVITY
# ============================================================

# ETH-transfer graph:
#
# J ---- X ---- K ---- Y ---- L
#
#
# Distances:
#
# J to K = 2
#
# K to L = 2
#
# J to L = 4
#
#
# G_sender therefore becomes:
#
# J ---- K ---- L
#
#
# Although J and L are NOT directly linked under k = 2,
# the entire sender graph is still connected.
#
# According to Algorithm 2 this group should PASS.

distance_J_K <-
  get_distance(
    eth_graph_clean,
    "j",
    "k"
  )


distance_K_L <-
  get_distance(
    eth_graph_clean,
    "k",
    "l"
  )


distance_J_L <-
  get_distance(
    eth_graph_clean,
    "j",
    "l"
  )


test_multi_sender <-
  entity_group_connected(

    senders = c("j", "k", "l"),

    eth_graph = eth_graph_clean,

    k = K
  )


# ============================================================
# EXPECTED RESULTS
# ============================================================

expected_same_sender     <- TRUE
expected_one_hop         <- TRUE
expected_two_hop         <- TRUE
expected_disconnected    <- FALSE
expected_service_removal <- FALSE
expected_multi_sender    <- TRUE


# ============================================================
# CHECK EACH UNIT TEST
# ============================================================

pass_same_sender <-
  test_same_sender ==
  expected_same_sender


pass_one_hop <-
  test_one_hop ==
  expected_one_hop


pass_two_hop <-
  test_two_hop ==
  expected_two_hop


pass_disconnected <-
  test_disconnected ==
  expected_disconnected


pass_service_removal <-
  test_service_removal ==
  expected_service_removal


pass_multi_sender <-
  test_multi_sender ==
  expected_multi_sender


# ============================================================
# OVERALL RESULT
# ============================================================

all_tests_pass <-
  pass_same_sender &&
  pass_one_hop &&
  pass_two_hop &&
  pass_disconnected &&
  pass_service_removal &&
  pass_multi_sender


# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n")
cat("============================================\n")
cat("ENTITY-RECOGNITION STRUCTURAL UNIT TEST\n")
cat("============================================\n\n")


cat("TEST 1 — SAME SENDER\n")
cat("--------------------------------------------\n")
cat("Expected: TRUE\n")
cat("Observed:", test_same_sender, "\n")
cat("PASS:", pass_same_sender, "\n\n")


cat("TEST 2 — ONE-HOP RELATIONSHIP\n")
cat("--------------------------------------------\n")
cat("Distance A-B:", distance_A_B, "\n")
cat("Expected entity result: TRUE\n")
cat("Observed:", test_one_hop, "\n")
cat("PASS:", pass_one_hop, "\n\n")


cat("TEST 3 — TWO-HOP RELATIONSHIP\n")
cat("--------------------------------------------\n")
cat("Distance C-D:", distance_C_D, "\n")
cat("Maximum k:", K, "\n")
cat("Expected entity result: TRUE\n")
cat("Observed:", test_two_hop, "\n")
cat("PASS:", pass_two_hop, "\n\n")


cat("TEST 4 — DISCONNECTED ADDRESSES\n")
cat("--------------------------------------------\n")
cat("Distance E-F:", distance_E_F, "\n")
cat("Expected entity result: FALSE\n")
cat("Observed:", test_disconnected, "\n")
cat("PASS:", pass_disconnected, "\n\n")


cat("TEST 5 — PUBLIC SERVICE ADDRESS REMOVAL\n")
cat("--------------------------------------------\n")
cat(
  "Distance G-H BEFORE service removal:",
  distance_G_H_before,
  "\n"
)

cat(
  "Distance G-H AFTER service removal:",
  distance_G_H_after,
  "\n"
)

cat("Expected entity result after cleaning: FALSE\n")
cat("Observed:", test_service_removal, "\n")
cat("PASS:", pass_service_removal, "\n\n")


cat("TEST 6 — MULTI-SENDER CONNECTED GRAPH\n")
cat("--------------------------------------------\n")

cat(
  "Distance J-K:",
  distance_J_K,
  "\n"
)

cat(
  "Distance K-L:",
  distance_K_L,
  "\n"
)

cat(
  "Distance J-L:",
  distance_J_L,
  "\n"
)

cat("Expected entity result: TRUE\n")
cat("Observed:", test_multi_sender, "\n")
cat("PASS:", pass_multi_sender, "\n\n")


cat("============================================\n")
cat("FINAL UNIT-TEST RESULT\n")
cat("============================================\n")


if (all_tests_pass) {

  cat(
    "\nPASS: All entity-recognition structural",
    "tests produced the expected results.\n"
  )

} else {

  cat(
    "\nFAIL: At least one entity-recognition",
    "test did not produce the expected result.\n"
  )
}