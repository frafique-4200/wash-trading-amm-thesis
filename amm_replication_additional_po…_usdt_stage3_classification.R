# AMM REPLICATION -- ECO-USDT -- STAGE 3 CLASSIFICATION
# Version 1: evidence-backed reduced reference; runs entirely on saved CSV files.
# No API call, API key or extra R package is needed. Stages 1 and 2 are not rerun.
#
# Read this script as: verify inputs -> decode raw swaps -> identify exchange
# functions -> apply the three paper rules -> save the reasons and retained swaps.
# Source: supplied AMM paper, section 7.3, pages 17:15-17:16.
# This reduced reference is not independently verified ground truth.
#
# INPUTS: Stage-2 final decisions, both Stage-1 candidate/member tables, Stage-2
# pair evidence, and the four Stage-3 Dune exports ending in _output.csv.
# Save all inputs in Data/cleaned. The script will identify missing files by name.
#
# OUTPUT INVENTORY: each output starts with
# amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_
# exchange_identity_register.csv: accepted exchange identities and their sources.
# decoded_raw_swaps.csv: raw V2-signature swaps, including nonstandard swaps.
# raw_decoded_exceptions.csv: missing decoding or nonstandard input structure.
# sandwich_evidence.csv: ordered opening/victim/closing events and amount checks.
# rebase_pattern_evidence.csv: exact opposite-swap/profit patterns, if present.
# ordinary_arbitrage_evidence.csv: group pool lists and exchange-recipient events.
# candidate_decisions.csv: one decision per Stage-2 surviving group.
# final_reference_swaps.csv: unique target swaps in at least one retained group.
# summary.csv: counts by pool and both pools; reason counts may overlap.
# classification_manifest.csv: fingerprints of inputs and generated outputs.
#
# METHOD DETAILS / CORRECTIONS
# * Different Uniswap pools are counted across the WHOLE GROUP, not just one tx.
# * Same-block sandwich triples may exist inside a group spanning several blocks.
# * Opening and closing swaps must belong to the candidate group's transactions.
#   Attribution uses the same sender, a positive saved Stage-2 pair, or the group's
#   connected entity graph. A victim must have no such observed link to attackers.
#   These are explicit reduced-replication operational choices, not proof of identity.
# * 90%-110% is inclusive. Exact raw integer amounts are compared without rounding.
# * An identified OTHER DEX receiving an ERC20 transfer triggers the paper's
#   cross-protocol rule. Governance tokens, DEX users, generic contracts, WETH,
#   and standalone aggregator/router labels do NOT establish another exchange.
# * Unidentified addresses stay included. Not identified does not mean private.
# * Nonstandard swaps stay visible. If no other rule settles their group, that
#   group requires review because the simple direction test cannot represent it.
# * Unknown protocol identity alone does not remove an address or force review.
# * No flash-loan detector is added. Rebase means the paper's pattern, not proof
#   of a token rebase. Retained means no implemented exclusion was found.

local({
  # Edit only this directory if your saved files are elsewhere.
  cleaned_dir <- "D:/Downloads/Bachelor Thesis/Data/cleaned"
  prefix <- "amm_replication_additional_pool_eco_usdt_output_"
  fail_if <- function(bad, message) if (isTRUE(bad)) stop(message, call. = FALSE)
  need <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
  # Null markers from Dune exports are converted to empty text, not addresses.
  clean <- function(x) {
    x <- tolower(trimws(x)); x[x %in% c("<nil>", "null", "na")] <- ""; x
  }
  number <- function(x) {
    # Block/log indices are small exact integers, sometimes exported as 1.12e+07.
    y <- suppressWarnings(as.numeric(x))
    need(all(is.finite(y) & y >= 0 & y == floor(y) & y < 2^53), "Invalid event index/block")
    y
  }
  cat("STAGE 3 CLASSIFICATION -- saved data only; no Dune requests\n")
  # Frozen checksums bind this analysis to the files already reviewed in this chat.
  # A later evidence run requires an explicit new validation, not silent reuse.
  inputs <- data.frame(
    key = c("decisions", "sus", "ctl", "pairs", "coverage", "logs", "dex", "labels"),
    file_name = c("amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_final_candidate_decisions.csv", "amm_replication_additional_pool_eco_usdt_output_suspicious_pool_stage1_candidate_swaps.csv", "amm_replication_additional_pool_eco_usdt_output_control_pool_stage1_candidate_swaps.csv", "amm_replication_additional_pool_eco_usdt_output_both_pools_stage2_pair_evidence.csv", "amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_01_transaction_coverage_output.csv", "amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_02_raw_logs_and_sandwich_context_output.csv", "amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_03_decoded_dex_activity_output.csv", "amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_04_address_identity_evidence_output.csv"),
    md5 = c("d7fc9435bf8896ca7e75b981d5651bd5", "5df5131ca2f45c801f66aafc018bc07d", "430a9f88a9c58b92881b4ab5efc46324", "e0b8f3ad7d14c8fbe12461bf0b216238", "3f712b1419c5240091e9f53ca5c38f50", "44cbeeb83e5b8c4bae728df28703ed75", "482add461268a1464c3a94f4ba9c8d59", "2a689d6196b4b99714d4560f61fcefd2"),
    stringsAsFactors = FALSE)
  input_paths <- file.path(cleaned_dir, inputs$file_name)
  tabs <- list()
  for (i in seq_len(nrow(inputs))) {
    path <- input_paths[i]
    need(file.exists(path), paste("Missing input:", path))
    need(unname(tools::md5sum(path)) == inputs$md5[i],
         paste("Input differs from the reviewed file:", path))
    tabs[[inputs$key[i]]] <- read.csv(path, colClasses = "character",
      na.strings = NULL, check.names = FALSE, stringsAsFactors = FALSE)
  }
  input_md5 <- unname(tools::md5sum(input_paths))
  d <- tabs$decisions
  need(!anyDuplicated(d$candidate_group_id), "Duplicate group decision")
  need(all(d$final_stage2_pass %in% c("TRUE", "FALSE")), "Incomplete Stage-2 decisions")
  d <- d[d$final_stage2_pass == "TRUE", , drop = FALSE]
  m <- rbind(tabs$sus, tabs$ctl)
  m <- m[m$candidate_group_id %in% d$candidate_group_id, , drop = FALSE]
  m$evt_tx_hash <- clean(m$evt_tx_hash); m$evt_tx_from <- clean(m$evt_tx_from)
  m$contract_address <- clean(m$contract_address)
  need(nrow(d) == 8357 && length(unique(m$evt_tx_hash)) == 5759,
       "Stage-2 survivor totals changed")
  group_members <- split(m, m$candidate_group_id)
  need(setequal(names(group_members), d$candidate_group_id), "Missing candidate members")
  # Verify every ordered member is present, since missing members bias pool counts.
  for (i in seq_len(nrow(d))) {
    g <- group_members[[d$candidate_group_id[i]]]
    first <- number(d$start_swap[i]); last <- number(d$end_swap[i])
    need(nrow(g) == last-first+1 &&
         setequal(number(g$swap_sequence), seq.int(first,last)) &&
         setequal(number(g$position_in_group), seq_len(nrow(g))) &&
         all(g$sample == d$sample[i]), "Incomplete group membership")
  }
  logs <- tabs$logs
  for (k in c("tx_hash","contract_address","tx_from","tx_to","topic0",
              "topic1","topic2","topic3","data_hex")) logs[[k]] <- clean(logs[[k]])
  for (k in c("block_number","transaction_index","log_index")) logs[[k]] <- number(logs[[k]])
  logs$candidate <- clean(logs$is_candidate_transaction) == "true"
  need(!anyDuplicated(paste(logs$block_number,logs$log_index)), "Duplicate raw events")
  need(setequal(logs$tx_hash[logs$candidate], m$evt_tx_hash), "Raw transaction coverage changed")
  coverage <- tabs$coverage; coverage$requested_tx_hash <- clean(coverage$requested_tx_hash)
  need(nrow(coverage)==5759 && !anyDuplicated(coverage$requested_tx_hash) &&
       setequal(coverage$requested_tx_hash,m$evt_tx_hash) &&
       all(clean(coverage$success)=="true" & clean(coverage$transaction_found)=="true"),
       "Transaction coverage is incomplete")
  ci <- match(logs$tx_hash[logs$candidate], coverage$requested_tx_hash)
  need(all(logs$block_number[logs$candidate] == number(coverage$block_number[ci])) &&
       all(logs$transaction_index[logs$candidate] == number(coverage$transaction_index[ci])),
       "Raw log transaction positions differ from coverage")
  need(all(logs$block_number>=10000835 & logs$block_number<=15400000), "Outside paper interval")
  cat("Input checks passed. Next: construct exchange and raw-swap evidence.\n")

  # Each accepted label names a pool or an exchange settlement function.
  # The word 'DEX' in a usage/persona label is deliberately insufficient.
  accepted <- data.frame(
    name = c("Uniswap_v2: Pair", "Uniswap_v2: UniswapV2Pair", "Sushi: Pair", "Curvefi: susd_swap", "Curvefi: threepool_swap", "Dodo: DVM", "Gnosis_protocol_v2: GPv2Settlement"),
    project = c("uniswap", "uniswap", "sushiswap", "curve", "curve", "dodo", "cow_protocol"),
    identity_class = c("UNISWAP_POOL", "UNISWAP_POOL", "OTHER_DEX_EXCHANGE", "OTHER_DEX_EXCHANGE", "OTHER_DEX_EXCHANGE", "OTHER_DEX_EXCHANGE", "OTHER_DEX_EXCHANGE"),
    stringsAsFactors = FALSE)
  labels <- tabs$labels
  good <- labels$name %in% accepted$name & labels$label_type == "identifier" &
    labels$category == "contracts" & nzchar(clean(labels$source)) & nzchar(clean(labels$model_name))
  lab <- labels[good, , drop = FALSE]; ai <- match(lab$name, accepted$name)
  registry <- data.frame(address=clean(lab$address), project=accepted$project[ai],
    identity_class=accepted$identity_class[ai], evidence_source="Dune labels.addresses",
    evidence_detail=paste(lab$name,lab$source,lab$model_name,sep="; "), stringsAsFactors=FALSE)
  dex <- tabs$dex
  for (k in c("tx_hash","project_contract_address","token_bought_address","token_sold_address"))
    dex[[k]] <- clean(dex[[k]])
  known <- dex[dex$project %in% c("uniswap","sushiswap","curve","dodo"),,drop=FALSE]
  registry <- unique(rbind(registry, data.frame(address=known$project_contract_address,
    project=known$project, identity_class=ifelse(known$project=="uniswap","UNISWAP_POOL","OTHER_DEX_EXCHANGE"),
    evidence_source="Dune dex.trades",
    evidence_detail=paste0("project=",known$project,"; version=",known$version),stringsAsFactors=FALSE)))
  classes <- split(registry$identity_class,registry$address)
  need(all(vapply(classes,function(x) length(unique(x))==1,logical(1))), "Conflicting exchange identities")
  uni <- unique(registry$address[registry$identity_class=="UNISWAP_POOL"])
  other <- unique(registry$address[registry$identity_class=="OTHER_DEX_EXCHANGE"])

  # Exact arithmetic: blockchain integers are larger than ordinary R numbers.
  # Keep each amount as hexadecimal text. Multiplication uses one digit at a time;
  # only tiny intermediate digit values enter R arithmetic, so no precision is lost.
  hn <- function(x) { x <- sub("^0+","",tolower(x)); if (!nzchar(x)) "0" else x }
  hcmp <- function(a,b) {
    a<-hn(a);b<-hn(b)
    if(nchar(a)!=nchar(b)) return(sign(nchar(a)-nchar(b)))
    av<-strtoi(strsplit(a,"")[[1]],16L); bv<-strtoi(strsplit(b,"")[[1]],16L)
    ix<-which(av!=bv); if(length(ix)) sign(av[ix[1]]-bv[ix[1]]) else 0L
  }
  hmul <- function(a,k) {
    digits<-rev(strtoi(strsplit(hn(a),"")[[1]],16L));carry<-0L;res<-character()
    alphabet<-strsplit("0123456789abcdef","")[[1]]
    for(v in digits) { t<-v*k+carry;res<-c(res,alphabet[(t %% 16)+1]);carry<-t %/% 16 }
    while(carry>0) {res<-c(res,alphabet[(carry %% 16)+1]);carry<-carry %/% 16}
    hn(paste(rev(res),collapse=""))
  }
  in_band <- function(first_bought,last_sold) {
    # 0.9*A <= B <= 1.1*A becomes 9*A <= 10*B <= 11*A exactly.
    hcmp(hmul(first_bought,9L),hmul(last_sold,10L))<=0 &&
      hcmp(hmul(last_sold,10L),hmul(first_bought,11L))<=0
  }
  # Small boundary checks and a >256-bit carry catch rounding/inequality mistakes.
  need(in_band("64","5a") && in_band("64","6e") &&
       !in_band("64","59") && !in_band("64","6f"), "90%-110% boundary check failed")
  need(hmul(strrep("f",64),2L)==paste0("1",strrep("f",63),"e"), "Large-integer check failed")
  swap_topic <- "0xd78ad95fa46c994b6551d0da85fc275fe613ce37657fb8d5e3d130840159d822"
  transfer_topic <- "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
  raw <- logs[logs$topic0==swap_topic,,drop=FALSE]
  need(all(nchar(raw$data_hex)==258), "Unexpected V2 Swap data length")
  # Decode the four unsigned amounts in their event-defined order.
  amount <- lapply(c(3,67,131,195),function(start)
    vapply(substr(raw$data_hex,start,start+63),hn,character(1)))
  names(amount) <- c("amount0In","amount1In","amount0Out","amount1Out")
  direction <- rep("NONSTANDARD",nrow(raw))
  direction[amount[[1]]!="0" & amount[[2]]=="0" & amount[[3]]=="0" & amount[[4]]!="0"] <- "0_TO_1"
  direction[amount[[2]]!="0" & amount[[1]]=="0" & amount[[4]]=="0" & amount[[3]]!="0"] <- "1_TO_0"
  sw <- data.frame(tx_hash=raw$tx_hash,pool=raw$contract_address,block=raw$block_number,
    tx_index=raw$transaction_index,log_index=raw$log_index,sender=raw$tx_from,
    candidate=raw$candidate,direction=direction,
    sold_hex=ifelse(direction=="0_TO_1",amount[[1]],amount[[2]]),
    bought_hex=ifelse(direction=="0_TO_1",amount[[4]],amount[[3]]),
    amount0In_hex=amount[[1]],amount1In_hex=amount[[2]],
    amount0Out_hex=amount[[3]],amount1Out_hex=amount[[4]],stringsAsFactors=FALSE)
  sw <- sw[order(sw$block,sw$tx_index,sw$log_index),,drop=FALSE]
  # Validate original target events separately from additional transaction swaps.
  target_keys <- unique(paste(m$evt_tx_hash,number(m$evt_index)))
  need(all(target_keys %in% paste(sw$tx_hash,sw$log_index)), "Missing target swap event")
  dex_keys <- paste(dex$tx_hash,number(dex$evt_index))
  need(!anyDuplicated(paste(dex_keys,dex$project,dex$version)), "Duplicate decoded trade")
  need(all(dex_keys %in% paste(logs$tx_hash,logs$log_index)), "Decoded trade lacks a raw event")
  exceptions <- sw[!(paste(sw$tx_hash,sw$log_index) %in% dex_keys) | sw$direction=="NONSTANDARD",,drop=FALSE]
  exceptions$exception <- ifelse(exceptions$direction=="NONSTANDARD",
    "BOTH_TOKEN_INPUTS_USE_RAW_RECORD", "RAW_SWAP_WITHOUT_DECODED_RECORD")
  # Index transactions/blocks once; avoid repeatedly searching all 30,499 logs.
  swtx <- split(which(sw$candidate),sw$tx_hash[sw$candidate])
  swblocks <- split(seq_len(nrow(sw)),paste(sw$block,sw$pool))
  tr <- logs[logs$candidate & logs$topic0==transfer_topic &
    nchar(logs$topic1)==66 & nchar(logs$topic2)==66 & logs$topic3=="" &
    nchar(logs$data_hex)==66,,drop=FALSE]
  tr$recipient <- paste0("0x",substr(tr$topic2,27,66))
  tr <- tr[tr$recipient %in% other,,drop=FALSE]
  transfer_by_tx <- split(seq_len(nrow(tr)),tr$tx_hash)
  pairs <- tabs$pairs[tabs$pairs$entity_relationship_found=="TRUE",,drop=FALSE]
  pair_key <- function(a,b) paste(sort(c(a,b)),collapse="|")
  links <- new.env(hash=TRUE,parent=emptyenv())
  if(nrow(pairs)) for(i in seq_len(nrow(pairs)))
    assign(pair_key(clean(pairs$address_a[i]),clean(pairs$address_b[i])),TRUE,envir=links)
  related <- function(a,b) a==b || exists(pair_key(a,b),envir=links,inherits=FALSE)
  cat("Evidence constructed. Next: apply exact rebase and ordered sandwich tests.\n")

  # Rebase proxy: two opposite swaps in the same transaction/pool; intermediate
  # token quantity restored exactly and final starting-token quantity increases.
  rebase <- list(); rebase_txs <- character()
  for(tx in names(swtx)) {
    idx <- swtx[[tx]]
    if(length(idx)<2) next
    for(a_pos in seq_len(length(idx)-1)) for(b_pos in seq.int(a_pos+1,length(idx))) {
      a<-sw[idx[a_pos],];b<-sw[idx[b_pos],]
      if(a$pool==b$pool && a$direction!="NONSTANDARD" && b$direction!="NONSTANDARD" &&
         a$direction!=b$direction && a$bought_hex==b$sold_hex && hcmp(b$bought_hex,a$sold_hex)>0) {
        rebase_txs<-c(rebase_txs,tx)
        rebase[[length(rebase)+1L]]<-data.frame(tx_hash=tx,pool=a$pool,
          first_log=a$log_index,second_log=b$log_index,intermediate_hex=a$bought_hex,
          initial_hex=a$sold_hex,returned_hex=b$bought_hex,stringsAsFactors=FALSE)
      }
    }
  }
  # First find all possible ordered triples in each block/pool. Candidate-group
  # membership and attacker identity are checked below, before excluding a group.
  patterns <- list()
  for(idx in swblocks) {
    ends <- idx[sw$candidate[idx] & sw$direction[idx]!="NONSTANDARD"]
    if(length(ends)<2) next
    for(aa in seq_len(length(ends)-1)) for(bb in seq.int(aa+1,length(ends))) {
      a<-ends[aa];b<-ends[bb]
      if(sw$tx_index[a]>=sw$tx_index[b] || sw$direction[a]==sw$direction[b] ||
         !in_band(sw$bought_hex[a],sw$sold_hex[b])) next
      victims<-idx[sw$tx_index[idx]>sw$tx_index[a] & sw$tx_index[idx]<sw$tx_index[b] &
                     sw$direction[idx]==sw$direction[a]]
      for(v in victims) if(!related(sw$sender[v],sw$sender[a]) && !related(sw$sender[v],sw$sender[b]))
        patterns[[length(patterns)+1L]]<-c(a,v,b)
    }
  }
  pattern_by_front <- list()
  if(length(patterns)) for(i in seq_along(patterns)) {
    tx<-sw$tx_hash[patterns[[i]][1]];pattern_by_front[[tx]]<-c(pattern_by_front[[tx]],i)
  }
  # Every rule is evaluated for every group; one reason does not hide another.
  decisions <- list();sandwich <- list();ordinary <- list()
  for(i in seq_len(nrow(d))) {
    cid<-d$candidate_group_id[i];g<-group_members[[cid]]
    txs<-unique(g$evt_tx_hash);senders<-unique(g$evt_tx_from)
    idx<-unlist(swtx[txs],use.names=FALSE);ss<-sw[idx,,drop=FALSE]
    pools<-sort(unique(ss$pool[ss$pool %in% uni]))
    hitidx<-unlist(transfer_by_tx[txs],use.names=FALSE)
    hits<-tr[hitidx,,drop=FALSE]
    multi<-length(pools)>1;cross<-nrow(hits)>0;reb<-any(txs %in% rebase_txs);sand<-FALSE
    graph<-d$external_entity_graph_connected[i]=="TRUE"
    potential<-unlist(pattern_by_front[txs],use.names=FALSE)
    for(j in potential) {
      pat<-patterns[[j]];a<-sw[pat[1],];v<-sw[pat[2],];b<-sw[pat[3],]
      if(!(b$tx_hash %in% txs)) next
      if((related(a$sender,b$sender) || graph) && !(graph && v$sender %in% senders)) {
        sand<-TRUE
        sandwich[[length(sandwich)+1L]]<-data.frame(candidate_group_id=cid,pool=a$pool,
          block=a$block,first_tx=a$tx_hash,victim_tx=v$tx_hash,last_tx=b$tx_hash,
          first_log=a$log_index,victim_log=v$log_index,last_log=b$log_index,
          first_bought_hex=a$bought_hex,last_sold_hex=b$sold_hex,
          attacker_link=if(a$sender==b$sender) "SAME_SENDER" else if(graph)
            "CANDIDATE_CONNECTED_ENTITY_GRAPH" else "POSITIVE_STAGE2_PAIR",stringsAsFactors=FALSE)
      }
    }
    nonstandard<-any(ss$direction=="NONSTANDARD")
    unidentified<-length(unique(ss$pool[!(ss$pool %in% c(uni,other))]))
    excluded<-multi || cross || reb || sand
    status<-if(excluded) "EXCLUDED_BY_STAGE3_RULE" else if(nonstandard)
      "REVIEW_NONSTANDARD_SWAP" else "RETAINED_REDUCED_REFERENCE"
    decisions[[i]]<-data.frame(sample=d$sample[i],candidate_group_id=cid,
      uniswap_pool_count=length(pools),other_exchange_transfer_count=nrow(hits),
      sandwich_detected=sand,multipool_arbitrage_detected=multi,
      cross_protocol_arbitrage_detected=cross,rebase_pattern_detected=reb,
      nonstandard_swap_present=nonstandard,unidentified_swap_pool_count=unidentified,
      stage3_status=status,final_reference_pass=status=="RETAINED_REDUCED_REFERENCE",
      stringsAsFactors=FALSE)
    if(multi || cross) ordinary[[length(ordinary)+1L]]<-data.frame(candidate_group_id=cid,
      uniswap_pools=paste(pools,collapse=";"),other_exchange_transfers=paste(sort(paste(
        hits$tx_hash,hits$log_index,hits$recipient,sep=":")),collapse=";"),stringsAsFactors=FALSE)
  }
  decision <- do.call(rbind,decisions)
  decision <- decision[order(decision$sample,decision$candidate_group_id),,drop=FALSE]
  # Compare every group's output with a separate Python implementation, not just
  # the final count. The temporary checksum file contains public analysis values.
  audit_columns <- c("sample","candidate_group_id","uniswap_pool_count",
    "other_exchange_transfer_count","sandwich_detected","multipool_arbitrage_detected",
    "cross_protocol_arbitrage_detected","rebase_pattern_detected","nonstandard_swap_present",
    "unidentified_swap_pool_count","stage3_status","final_reference_pass")
  canonical <- vapply(seq_len(nrow(decision)), function(i)
    paste(vapply(audit_columns,function(k) tolower(as.character(decision[[k]][i])),character(1)),
          collapse="|"), character(1))
  tmp <- tempfile();writeBin(charToRaw(paste0(paste(canonical,collapse="\n"),"\n")),tmp)
  observed <- unname(tools::md5sum(tmp));unlink(tmp)
  need(observed == "6786e826a48e3f352f4de83995e55b8f", "R/Python candidate decisions differ; stop for review")

  # A swap can occur in both excluded and retained overlapping groups. The final
  # reference set is the UNION of target swaps belonging to any retained group.
  kept <- decision$candidate_group_id[decision$final_reference_pass]
  final_members <- m[m$candidate_group_id %in% kept,,drop=FALSE]
  event_id <- paste(final_members$evt_tx_hash,number(final_members$evt_index))
  final_swaps <- final_members[!duplicated(event_id),c("sample","swap_sequence",
    "contract_address","evt_tx_hash","evt_tx_from","evt_index","evt_block_number"),drop=FALSE]
  final_swaps <- final_swaps[order(final_swaps$sample,number(final_swaps$swap_sequence)),,drop=FALSE]
  summarize <- function(sample_name) {
    rr<-decision[decision$sample==sample_name | sample_name=="both_pools",,drop=FALSE]
    ff<-final_swaps[final_swaps$sample==sample_name | sample_name=="both_pools",,drop=FALSE]
    data.frame(sample=sample_name,stage2_groups=nrow(rr),sandwich_groups=sum(rr$sandwich_detected),
      multipool_groups=sum(rr$multipool_arbitrage_detected),cross_protocol_groups=sum(rr$cross_protocol_arbitrage_detected),
      rebase_groups=sum(rr$rebase_pattern_detected),excluded_groups=sum(rr$stage3_status=="EXCLUDED_BY_STAGE3_RULE"),
      review_groups=sum(rr$stage3_status=="REVIEW_NONSTANDARD_SWAP"),retained_groups=sum(rr$final_reference_pass),
      retained_unique_swaps=nrow(ff),stringsAsFactors=FALSE)
  }
  summary<-do.call(rbind,lapply(c("suspicious_pool","control_pool","both_pools"),summarize))
  need(all(summary$stage2_groups==summary$excluded_groups+summary$review_groups+summary$retained_groups),
       "Decision totals do not reconcile")
  cat("All candidate decisions match the independent calculation. Next: save results.\n")
  # Empty evidence tables still receive headers, distinguishing zero findings
  # from a missing output file.
  table_or_empty <- function(rows,fields) {
    if(length(rows)) return(do.call(rbind,rows))
    as.data.frame(setNames(rep(list(character()),length(fields)),fields),stringsAsFactors=FALSE)
  }
  written<-character()
  save <- function(tab,suffix) {
    path<-file.path(cleaned_dir,paste0(prefix,"both_pools_stage3_",suffix,".csv"))
    write.csv(tab,path,row.names=FALSE,na="",fileEncoding="UTF-8")
    written<<-c(written,path);cat("Saved:",path,"\n")
  }
  save(registry,"exchange_identity_register")
  save(sw,"decoded_raw_swaps")
  save(exceptions,"raw_decoded_exceptions")
  save(table_or_empty(sandwich,c("candidate_group_id","pool","block","first_tx","victim_tx","last_tx",
    "first_log","victim_log","last_log","first_bought_hex","last_sold_hex","attacker_link")),"sandwich_evidence")
  save(table_or_empty(rebase,c("tx_hash","pool","first_log","second_log","intermediate_hex","initial_hex","returned_hex")),
    "rebase_pattern_evidence")
  save(table_or_empty(ordinary,c("candidate_group_id","uniswap_pools","other_exchange_transfers")),"ordinary_arbitrage_evidence")
  save(decision,"candidate_decisions")
  save(final_swaps,"final_reference_swaps")
  save(summary,"summary")
  need(identical(input_md5,unname(tools::md5sum(input_paths))),"An input changed during this run")
  manifest<-data.frame(artifact_type=c(rep("input",length(input_paths)),rep("output",length(written))),
    file_name=basename(c(input_paths,written)),md5=unname(tools::md5sum(c(input_paths,written))),
    stage="STAGE3_CLASSIFICATION",stringsAsFactors=FALSE)
  save(manifest,"classification_manifest")
  print(summary,row.names=FALSE)
  cat("Stage-3 reduced-reference classification complete. Original input files unchanged.\n")
  cat("Retained is a reference-method result, not independently proven wash trading.\n")
  cat("Next step: compare these unique swaps with WTEYE REQUIRE_CONSECUTIVE results.\n")
})
