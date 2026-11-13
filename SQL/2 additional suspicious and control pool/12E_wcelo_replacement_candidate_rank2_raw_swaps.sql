-- wCELO: retrieve the second-ranked control candidate for input screening.
-- CHECKPOINT: 5 October 2026, 17:26 Europe/Berlin.
-- Deferred: finish documenting PDOG-WETH's preparation incompatibility,
-- the diagnostic findings, and the justification/limitations of replacement.
-- Existing finding: PDOG-WETH has 33 events that fail the unchanged
-- ordinary-swap pattern required by Step 2. Its exact cause is not established.
-- This candidate is NOT yet accepted as the replacement control.
-- Original Query 11 ranks by absolute swap-count difference, then address.
-- Shared-token status is descriptive, not a filter or ranking criterion.
-- Stored rank-2 count: 6,459 versus wCELO's 11,417 (43.43% difference).
-- Retrieve all events, including unusual ones; do not filter by amounts.
-- No detector parameters or classifications are changed by this query.
--
-- HOW TO USE
-- Copy this entire file into a new Dune query and run it.
-- Export the FULL CSV result (not only the visible preview rows) as:
-- 12E_wcelo_replacement_candidate_rank2_raw_swaps.csv
-- Save it in D:/Downloads/Bachelor Thesis/Data/raw and upload it for review.
-- Keep the original 12D PDOG-WETH file unchanged.
-- NEXT: retrieve token metadata, check both inputs, then decide suitability.

-- Preserve the same 15 raw-data columns used by the existing preparation.
SELECT
    s.contract_address,
    s.evt_tx_hash,
    s.evt_tx_from,
    s.evt_tx_to,
    s.evt_tx_index,
    s.evt_index,
    s.evt_block_time,
    s.evt_block_number,
    s.evt_block_date,
    s.amount0In,
    s.amount0Out,
    s.amount1In,
    s.amount1Out,
    s.sender,
    s.to
FROM uniswap_v2_ethereum.pair_evt_swap AS s
-- Select the original shortlist's second-ranked candidate for wCELO.
WHERE s.contract_address = 0xfe7444f63e0010a7208529608eb173760629483b
-- Keep the same inclusive paper block bounds as Query 11.
  AND s.evt_block_number BETWEEN 10000835 AND 15400000
-- Keep the same inclusive wCELO observation window (UTC) as Query 11.
  AND s.evt_block_time >= TIMESTAMP '2020-12-17 21:19:44'
  AND s.evt_block_time <= TIMESTAMP '2022-07-25 22:08:54'
-- Return events in the chronological order expected by the R workflow.
ORDER BY s.evt_block_number, s.evt_index;
