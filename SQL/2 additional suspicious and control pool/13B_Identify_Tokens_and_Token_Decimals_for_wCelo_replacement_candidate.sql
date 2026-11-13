-- QUERY 13B: wCELO replacement candidate, rank 2, token metadata.
-- Supplement to the original Query 13, which remains unchanged.
-- Copy this entire file into a new Dune query and run it.
-- Export as 13B_wcelo_replacement_candidate_rank2_token_metadata.csv.
-- Expected: one row, with both token addresses, symbols and decimal counts.
--
-- CORRECTION (5 October 2026): the first 13B draft added a factory block
-- filter and started directly from factory records. This version restores
-- the original selected_pools LEFT JOIN and removes that extra block filter.
-- Only the selected-pool VALUES list differs from the original executable
-- SQL; the retrieval joins, output columns and ordering are unchanged.
-- Historical eligibility is checked separately through Query 11/input checks.
-- Metadata is for token identification and unit interpretation; this query
-- neither transforms raw swap amounts nor changes detector parameters.
-- NEXT: review this output with 12E before updating pool-specific R inputs.

-- Step 1: select only the proposed replacement; acceptance is still pending.
WITH selected_pools (
    display_order,
    comparison_name,
    pool_role,
    pool_address
) AS (
    VALUES
        (1, 'wCELO comparison', 'control',
         0xfe7444f63e0010a7208529608eb173760629483b)
),

-- Step 2: keep the original LEFT JOIN to find token0 and token1.
-- If no factory record exists, the selected pool remains with missing tokens.
pool_tokens AS (
    SELECT
        p.display_order,
        p.comparison_name,
        p.pool_role,
        p.pool_address,
        f.token0 AS token0_address,
        f.token1 AS token1_address
    FROM selected_pools AS p
    LEFT JOIN uniswap_v2_ethereum.Factory_evt_PairCreated AS f
        ON f.pair = p.pool_address
),

-- Step 3: retrieve Ethereum token symbols and decimals, exactly as Query 13.
-- Missing metadata remains NULL and must be checked; no decimal default is used.
pool_metadata AS (
    SELECT
        p.display_order,
        p.comparison_name,
        p.pool_role,
        p.pool_address,

        p.token0_address,
        t0.symbol AS token0_symbol,
        t0.decimals AS token0_decimals,

        p.token1_address,
        t1.symbol AS token1_symbol,
        t1.decimals AS token1_decimals

    FROM pool_tokens AS p

    LEFT JOIN tokens.erc20 AS t0
        ON t0.blockchain = 'ethereum'
       AND t0.contract_address = p.token0_address

    LEFT JOIN tokens.erc20 AS t1
        ON t1.blockchain = 'ethereum'
       AND t1.contract_address = p.token1_address
)

-- Step 4: return the original nine columns in the original order.
SELECT
    comparison_name,
    pool_role,
    pool_address,

    token0_address,
    token0_symbol,
    token0_decimals,

    token1_address,
    token1_symbol,
    token1_decimals

FROM pool_metadata
ORDER BY display_order;
