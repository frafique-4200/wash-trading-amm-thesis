# Detecting and Quantifying Suspicious Wash Trading Activity in Automated Market Maker Decentralized Exchanges

Research code, Dune SQL queries, selected Dune CSV exports, and 230 raw/processed research data files for a Bachelor's thesis in Applied Mathematics at **Hochschule Mittweida, Germany**.

## Project overview

This project studies potentially suspicious trading activity in Ethereum-based **Uniswap V2** automated market maker (AMM) liquidity pools. It evaluates whether a graph-based approach previously applied to order-book decentralized exchanges can be adapted to AMM swap-event data.

Two approaches are applied to the **same ordered swap events** in each sampled pool:

1. **Reduced replication of Gan et al. (2024).** A three-stage procedure combining (i) a coarse filter for nearly balanced consecutive swaps, (ii) transaction-initiator and entity-relationship checks, and (iii) checks for alternative explanations associated with maximal extractable value (MEV), such as arbitrage and sandwich-like patterns.
2. **WTEYE-based adaptation for AMMs.** An adaptation of the graph-based approach of Cui and Gao (2023), informed by earlier order-book exchange research. The implementation represents token movements between initiators and liquidity pools as directed edges, finds strongly connected components (SCCs), evaluates aggregate address balance, and filters candidate groups by swap-event consecutiveness.

The transaction initiator serves as a **proxy for the trader**. This does not establish ultimate token ownership or prove that different addresses are controlled by one entity.

## Study data and liquidity pools

| Study group | Uniswap V2 pool | Observed swap events |
| --- | --- | ---: |
| Initial pool of interest | RFI–WETH | 36 |
| Initial control | WETH–ANON | 39 |
| Main pool of interest | ECO–USDT | 9,449 |
| Main control | MKR–USDT | 7,815 |
| Main pool of interest | WETH–wCELO | 11,417 |
| Main control | TOWER–REVV | 6,459 |

The **four main pools comprise 35,140 observed swap events**. These counts refer to swap events, not necessarily unique transactions or traders.

The initial RFI–WETH pool and the two main pools of interest were selected with reference to the abnormal-pool data and pool listings published by Gan et al. Controls were selected from pools not appearing in the authors' abnormal-pool list, using trading periods and swap counts for comparison. **A control designation does not establish that its transactions are legitimate or wash-trade-free.**

## Data extraction from Dune Analytics

The underlying Ethereum swap records were queried using **Dune Analytics**. The repository's [`SQL/`](SQL/) directory includes saved query files and selected CSV exports returned by those queries.

### Initial RFI–WETH experiment

- The RFI–WETH contract was identified by comparing Gan et al.'s abnormal-pool addresses with Ethereum Uniswap V2 trading activity recorded by Dune.
- The original RFI–WETH swap extraction imposed **no explicit timestamp or block-number restriction**. It returned 36 events dated **2 December 2020, 18:47:28–19:50:25 UTC**. This is the observed extraction result, **not a claim about the pool's complete historical activity**.
- The WETH–ANON control was selected using that observed time interval. Candidate pools with 30–50 swaps were considered, and WETH–ANON had 39 events.

Examples of saved files:

| Path within `SQL/` | Role |
| --- | --- |
| `1_Find uniswap v2 pool.txt` | Initial pool identification query |
| `3_View the 36 Trades.txt` | Initial swap-inspection query |
| `4_Query the Raw Swap Events for Our Suspicious Pool.txt` | Initial pool swap extraction query |
| `4_uniswap_v2_suspicious_pool_raw_swaps.csv` | Exported initial-pool swap records |
| `9_Get Raw Swap Events for the Control Pool.txt` | Initial control-pool swap extraction query |
| `9_uniswap_v2_control_pool_raw_swaps.csv` | Exported initial control-pool swap records |

### Main four-pool comparison

For the main comparison, swaps were queried from Dune's **`uniswap_v2_ethereum.pair_evt_swap`** table, restricted by liquidity pool contract address and by the following inclusive boundaries:

| Pool pair | Observation interval (UTC) | Ethereum block range |
| --- | --- | --- |
| ECO–USDT / MKR–USDT | 2020-11-09 12:13:04 to 2022-08-23 10:54:06 | 10,000,835–15,400,000 |
| WETH–wCELO / TOWER–REVV | 2020-12-17 21:19:44 to 2022-07-25 22:08:54 | 10,000,835–15,400,000 |

Within each pool, events were ordered by **block number and event log index**. The combination of pool contract address, transaction hash, and event log index identifies an individual swap event for matching between methods.

The main-sample query files and exports are in:

[`SQL/2 additional suspicious and control pool/`](SQL/2%20additional%20suspicious%20and%20control%20pool/)

Key examples include:

| Saved file in that directory | Purpose |
| --- | --- |
| `10b_Find Trading Period and Swap Counts for Additional Suspicious Pools Within the AMM Paper Block Range.txt` | Counts and periods for candidate pools |
| `11_Revised_Reconstruct Paper-Period Non-Abnormal Uniswap V2 Pools and Find Matched Controls for ECO-USDT and wCELO.txt` | Search for activity-matched controls outside the abnormal-pool list |
| `12A_Get Raw Swap Events for the ECO-USDT Suspicious Pool.txt` | ECO–USDT extraction query |
| `12A_eco_usdt_suspicious_pool_raw_swaps.csv` | ECO–USDT swap export |
| `12B_eco_usdt_matched_control_pool_raw_swaps.csv` | MKR–USDT control swap export |
| `12C_wcelo_suspicious_pool_raw_swaps.csv` | WETH–wCELO swap export |
| `12D_wcelo_matched_control_pool_raw_swaps.csv` | TOWER–REVV control swap export |
| `13_Identify Tokens and Token Decimals for the Four Additional Pools.txt` | Token identities and decimal precision |

Control-pool selection involved querying Uniswap V2 factory records, excluding listed abnormal pools, and comparing observed swap counts over the corresponding pool's time interval. The initially selected WETH–wCELO control, PDOG–WETH, could not be processed consistently by the ordinary two-movement swap conversion, so **TOWER–REVV** was used instead.

### Relationship checks, MEV checks, and validation queries

Other saved files under `SQL/` document checks supporting the reduced replication:

- **Entity relationships (Stage 2):** queries for direct ETH transfers, two-hop intermediary relationships, address labels, and historical public-service labels. For the main four pools, ETH-transfer checks used the **`ethereum.transactions`** table within blocks **10,000,835–15,400,000**. Some queries from the initial experiment have different bounds, as described in the thesis.
- **Alternative trading explanations (Stage 3):** queries inspecting candidate transactions, raw swap logs, cross-protocol activity, and evidence relevant to arbitrage, sandwich-like patterns, and rebase-token behavior.
- **Validation:** files named `VALIDATION TEST ...` for checking particular stages and examples discussed in the reference paper.

The saved query files have a mixture of **`.txt`, `.sql`, and extensionless filenames**, while their exported result tables typically use **`.csv`**. These are research artifacts retained with their original filenames. The directory may also contain exploratory queries, diagnostic outputs, or incomplete/empty files; not every file is a standalone, production-ready query. Some R scripts submit queries to Dune through an API and may require authentication or changes to execute today.

The CSV files stored under `SQL/` are **selected Dune exports**. Additional input datasets, intermediate analysis files, and outputs are included under [`Data/`](Data/). The presence of these artifacts does not by itself establish that every step of the analysis can be reproduced without additional configuration or access.

## Repository organization

| Location / filename pattern | Contents |
| --- | --- |
| [`SQL/`](SQL/) | Saved Dune queries, CSV exports, entity/MEV validation queries, and an additional-pools subfolder |
| [`Data/raw/`](Data/raw/) | Source datasets and saved Dune query outputs used as inputs, including swap-event extracts and validation data |
| [`Data/cleaned/`](Data/cleaned/) | Prepared and transformed data, intermediate detection artifacts, classification results, and comparison summaries |
| `suspicious_pool_*.R`, `control_pool_*.R`, `suspicious_pool_full_pipeline.R` | Reduced replication for the initial pools |
| `amm_replication_additional_pool_eco_usdt_*.R` | Reduced replication for ECO–USDT and its control |
| `amm_replication_additional_pool_wcelo_*.R` | Reduced replication for WETH–wCELO and its control |
| `amm_adaptation_step_*.R` | Initial-pool graph-based adaptation and comparisons |
| `amm_adaptation_additional_pool_eco_usdt_step_*.R` | Main ECO–USDT WTEYE-based adaptation |
| `amm_adaptation_additional_pool_wcelo_step_*.R` | Main WETH–wCELO WTEYE-based adaptation |
| `validation_test_*.R`, `entity_recognition_unit_test.R`, `cross_combination_unit_test.R` | Checks of detection behavior and processing logic |
| `clean_swaps.r` | Swap-data preparation utility |
| `volume_matching.cpp` | Earlier C++ volume-matching implementation; **not the same as** the whole-component aggregate-balance criterion used in the main thesis adaptation |

The three stages of the **reduced AMM replication** and the numbered processing steps of the **WTEYE adaptation** are distinct workflows.

### Research datasets: `Data/raw/` and `Data/cleaned/`

The repository includes **230 files** under [`Data/`](Data/), preserving the project's research inputs and saved outputs. These files were added in commit `b8684b7` (`Add thesis research datasets`). The distinction between the directories describes their **role in this project** rather than a guarantee that every `raw` file is an untouched blockchain export or that every `cleaned` file is a finalized publication dataset.

| Directory | Contents | Examples |
| --- | --- | --- |
| [`Data/raw/`](Data/raw/) | Source records and saved query responses used during data preparation and validation: Uniswap V2 swap-event exports, token/decimal lookups, ETH-transfer and intermediary-label results, and validation cases. | `12A_eco_usdt_suspicious_pool_raw_swaps.csv`, `12B_eco_usdt_matched_control_pool_raw_swaps.csv`, `12C_wcelo_suspicious_pool_raw_swaps.csv`, `12D_wcelo_matched_control_pool_raw_swaps.csv` |
| [`Data/cleaned/`](Data/cleaned/) | Prepared swap data and token flows, Stage 1 candidate windows, Stage 2 entity evidence, Stage 3 exclusion/classification evidence, WTEYE-based candidates, event-level labels, and cross-method comparison tables. This directory also includes supporting SQL and JSON artifacts from certain pipeline runs. | `uniswap_v2_suspicious_pool_cleaned.csv`, `amm_adaptation_input_suspicious_pool_token_flows.csv`, `amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_final_reference_swaps.csv` |

Examples of the relationship between research stages and saved data:

- **Extraction:** Dune queries under `SQL/` produce blockchain-event or metadata exports, with analysis inputs stored under `Data/raw/`.
- **Preparation and candidate generation:** R scripts read relevant extracted data and create cleaned swap records, token-movement representations, candidate groups, and balance checks in `Data/cleaned/`.
- **Detection and validation:** Separate replication/adaptation scripts generate Stage 1–3 decisions or graph-based selections, with related evidence and validation files saved under `Data/cleaned/`.
- **Method comparison:** Saved per-swap labels and summary tables support the comparison of the reduced Gan et al. replication and the WTEYE-based AMM adaptation.

Not every dataset in `Data/cleaned/` is necessarily a final result. File names generally encode the pool, method, stage, and role (`input`, `output`, `summary`, `candidate`, or `comparison`), and the underlying R scripts remain the reference for how individual files were generated.

## Methodological parameters

| Component | Setting in the thesis implementation |
| --- | --- |
| Consecutive candidate-window length | 2–10 swaps in the same pool |
| Maximum block span for reduced-replication window | 200 Ethereum blocks |
| Coarse pool-balance threshold | At most 5% normalized net movement for at least one pool token |
| WTEYE-based aggregate address-balance criterion | Strictly below 10% for at least one token |
| WTEYE comparison variant | `REQUIRE_CONSECUTIVE` |
| Comparison unit | Unique `(pool address, transaction hash, event log index)` swap events |

When one swap belongs to several qualifying groups, it is counted **once per method** in the final event-level comparison.

## Software and reproduction guidance

The thesis reports **R 4.5.1** and **igraph 2.2.1** for the graph-based candidate search. Individual scripts may require additional R packages, Dune/API access, and supporting files. Check the script headers before execution.

A researcher attempting to reproduce the analysis should:

1. Review the extraction conditions and saved queries in `SQL/`. Where appropriate, start from the corresponding files already included in `Data/raw/` rather than re-querying Dune. Re-run SQL queries only when validation or a fresh extraction is needed.
2. Confirm the **pool contract address**, chronological event order, token identifiers, token decimal precision, and expected input-column structure before using those input files.
3. Review the existing prepared data and saved outputs in `Data/cleaned/`, distinguishing inputs, intermediate artifacts, and final comparison tables. These can be used to inspect individual stages without necessarily running the entire pipeline.
4. Configure file paths and dependencies referenced by the R scripts. Some scripts use local absolute paths beginning with `D:/Downloads/Bachelor Thesis/`; the published repository now keeps `Data/` inside its root alongside the R scripts, so local path settings may need adjustment.
5. Run the reduced-replication and WTEYE-adaptation workflows **separately** for the same pool observations, following the relevant numbered scripts and their individual instructions.
6. Compare event-level selections using the downstream analysis scripts, taking care to deduplicate overlapping candidate windows.

Some comparison scripts check MD5 fingerprints of saved intermediate results. Replacing their expected inputs can cause those checks to fail even if the replacement data are otherwise well-formed.

**Reproducibility status:** This repository now preserves research code, Dune queries/exports, and 230 files of raw, prepared, and analysis-output data. However, it **has not been verified as a self-contained, end-to-end, one-command replication package**. Re-running particular scripts may still require path changes, R dependencies, external token metadata, API configuration, or Dune access; some saved intermediates may also be tied to earlier execution environments.

## Interpretation and limitations

The methods select overlapping but non-identical sets of potentially suspicious swap events. For example:

- For **ECO–USDT**, the WTEYE adaptation selected **86.08%** of the events selected by the reduced replication. Their total selection counts differed by 69, while the methods disagreed on **1,345 individual event classifications**.
- For **WETH–wCELO**, the WTEYE adaptation selected **62.37%** of the events selected by the reduced replication.

**A flagged swap is not a verified wash trade.** The study compares detection-rule selections; it does not establish transaction-level ground-truth labels, classification accuracy, or the absence of manipulation in control pools. The available timing evidence also does not support an overall performance or scalability ranking between the approaches.

## Related research

- Gan, R., Wang, L., Xue, L., & Lin, X. (2024). *Exposing Stealthy Wash Trading on Automated Market Maker Exchanges*. *ACM Transactions on Internet Technology*. https://doi.org/10.1145/3689631
- Cui, W., & Gao, C. (2023). *WTEYE: On-chain Wash Trade Detection and Quantification for ERC20 Cryptocurrencies*. *Blockchain: Research and Applications, 4*(1), 100108. https://doi.org/10.1016/j.bcra.2022.100108
- Victor, F., & Weintraud, A. M. (2021). *Detecting and Quantifying Wash Trading on Decentralized Cryptocurrency Exchanges*. *Proceedings of The Web Conference 2021*. https://doi.org/10.1145/3442381.3449824

## Repository URLs

- HSMW GitLab: https://git.hs-mittweida.de/frafique/wash-trading-amm-thesis
- GitHub: https://github.com/frafique-4200/wash-trading-amm-thesis
