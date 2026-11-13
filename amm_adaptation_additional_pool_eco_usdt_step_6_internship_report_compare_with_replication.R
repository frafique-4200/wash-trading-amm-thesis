# AMM ADAPTATION -- ECO-USDT -- STEP 6: COMPARE WITH REDUCED REPLICATION
# Revised swap-level comparison: explicit missed and extra rates.
# This replaces amm_comparison_additional_pool_eco_usdt.R for this experiment.
# The earlier original-pool Step 6 also had a separate group-overlap report.
# This script implements the agreed swap-level comparison; it does not recreate
# that separate group-overlap analysis or change detection parameters.
# No new detector is run. Read the saved classifications and compare exact events.
# Each event key is sample + pool address + transaction hash + log index.
# Swap sequence numbers are checked for consistency, not used as the join key.
#
# OUTPUT INVENTORY (CSV files saved in Data/cleaned):
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_summary.csv
#   counts, coverage, missed-reference and extra-WTEYE percentages.
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_swap_labels.csv
#   every original swap with both method decisions.
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_missed_reference_swaps.csv
#   reference-selected events not selected by WTEYE.
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_extra_wteye_swaps.csv
#   WTEYE-selected events not selected by the reference.
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_disagreements.csv
#   both types of disagreement in one event-level table.
# amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_manifest.csv
#   input/output file fingerprints.
#
# SHARED: selected by both methods. REFERENCE_ONLY: selected only by reference.
# WTEYE_ONLY: selected only by WTEYE. NEITHER: selected by neither method.
# None of these categories is a verified true/false-positive ground-truth label.
local({
  # This folder contains the three previously generated and reviewed input CSVs.
  cleaned_dir <- "D:/Downloads/Bachelor Thesis/Data/cleaned"
  prefix <- "amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_replication_comparison_"
  need <- function(ok,msg) if(!isTRUE(ok)) stop(msg,call.=FALSE)
  # Fingerprints ensure we compare the exact validated run. A failed check
  # means an input needs investigation; it is not a reason to alter the labels.
  inputs <- data.frame(
    file_name=c("amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_swap_labels.csv","amm_adaptation_additional_pool_eco_usdt_output_both_pools_internship_report_wteye_filter_variant_comparison.csv","amm_replication_additional_pool_eco_usdt_output_both_pools_stage3_final_reference_swaps.csv"),
    md5=c("299686cef48dd16d769e586ecb80dc61","213baf4eb8a701abf99c7cc281070851","81bd61bec6198b8cdb93e5057e1395f6"),
    stringsAsFactors=FALSE)
  paths <- file.path(cleaned_dir,inputs$file_name)
  tabs <- list()
  for(i in seq_along(paths)) {
    need(file.exists(paths[i]),paste("Missing input:",paths[i]))
    need(unname(tools::md5sum(paths[i]))==inputs$md5[i],paste("Changed input:",paths[i]))
    tabs[[i]]<-read.csv(paths[i],colClasses="character",na.strings=NULL,
                       stringsAsFactors=FALSE,check.names=FALSE)
  }
  cat("AMM ADAPTATION -- ECO-USDT -- STEP 6 COMPARISON\n")
  cat("Saved classifications only; no detector rerun or Dune request.\n")
  # Select the agreed formal WTEYE variant; do not mix alternative filters.
  a<-tabs[[1]][tabs[[1]]$variant=="REQUIRE_CONSECUTIVE",,drop=FALSE]
  b<-tabs[[3]]; declared<-tabs[[2]]
  a$event_id<-tolower(a$event_id);a$transaction_hash<-tolower(a$transaction_hash)
  a$initiator_address<-tolower(a$initiator_address)
  need(all(a$retained_candidate %in% c("TRUE","FALSE")),"Missing WTEYE decision")
  akey<-paste(a$sample,a$event_id,sep="|")
  bkey<-paste(b$sample,paste(tolower(b$contract_address),tolower(b$evt_tx_hash),
                            as.integer(b$evt_index),sep=":"),sep="|")
  need(!anyDuplicated(akey) && !anyDuplicated(bkey),"Duplicate event identifiers")
  need(all(bkey %in% akey),"A reference event is outside the WTEYE event universe")
  # Compare independent fields to detect an incorrect or differently ordered sample.
  j<-match(bkey,akey)
  need(all(as.integer(b$swap_sequence)==as.integer(a$swap_index[j])) &&
       all(tolower(b$evt_tx_from)==a$initiator_address[j]),"Event metadata mismatch")
  for(sample_name in c("suspicious_pool","control_pool")) {
    aa<-a[a$sample==sample_name,,drop=FALSE]
    expected_n<-if(sample_name=="suspicious_pool") 9449L else 7815L
    need(nrow(aa)==expected_n && setequal(as.integer(aa$swap_index),seq_len(expected_n)),
         "Incomplete event universe")
    z<-declared[declared$variant=="REQUIRE_CONSECUTIVE" & declared$sample==sample_name,,drop=FALSE]
    need(nrow(z)==1 && sum(aa$retained_candidate=="TRUE")==as.integer(z$unique_candidate_swap_events),
         "WTEYE labels disagree with the uploaded WTEYE summary")
  }
  # Union membership is evaluated once per unique swap, avoiding group overlap.
  ref<-akey %in% bkey;wt<-a$retained_candidate=="TRUE"
  result<-data.frame(sample=a$sample,event_id=a$event_id,swap_index=a$swap_index,
    transaction_hash=a$transaction_hash,initiator_address=a$initiator_address,
    reference_selected=ref,wteye_selected=wt,
    comparison_category=ifelse(ref & wt,"SHARED",ifelse(ref,"REFERENCE_ONLY",
      ifelse(wt,"WTEYE_ONLY","NEITHER"))),stringsAsFactors=FALSE)
  result<-result[order(result$sample,as.integer(result$swap_index)),,drop=FALSE]
  # A missing denominator produces NA (blank in CSV), not an invented zero.
  pct<-function(n,d) if(d==0) NA_real_ else 100*n/d
  summarize<-function(sample_name) {
    rr<-result[result$sample==sample_name | sample_name=="both_pools",,drop=FALSE]
    nr<-sum(rr$reference_selected);nw<-sum(rr$wteye_selected)
    ns<-sum(rr$comparison_category=="SHARED")
    # Coverage = shared/reference; selected overlap = shared/WTEYE;
    # Jaccard = shared/union. A zero denominator means undefined, saved blank.
    # Missed-reference percentage = 100 * reference-only / all reference-selected.
    # Extra-WTEYE percentage = 100 * WTEYE-only / all WTEYE-selected.
    # These use different denominators and therefore need not add to 100%.
    # A missed swap is missed RELATIVE TO THE REFERENCE, not a proven wash trade.
    # An extra selection is not automatically a false positive.
    data.frame(sample=sample_name,total_swap_events=nrow(rr),reference_selected=nr,
      wteye_selected=nw,shared=ns,reference_only=nr-ns,wteye_only=nw-ns,
      neither=sum(rr$comparison_category=="NEITHER"),reference_coverage_pct=pct(ns,nr),
      missed_reference_pct=pct(nr-ns,nr),
      extra_wteye_pct=pct(nw-ns,nw),
      wteye_selected_overlap_pct=pct(ns,nw),jaccard_pct=pct(ns,nr+nw-ns),stringsAsFactors=FALSE)
  }
  summary<-do.call(rbind,lapply(c("suspicious_pool","control_pool","both_pools"),summarize))
  need(all(summary$total_swap_events==summary$shared+summary$reference_only+
             summary$wteye_only+summary$neither),"Set totals do not reconcile")
  # Compare calculated counts with the independent audit; never modify labels.
  need(identical(as.integer(summary$shared),c(3946L,0L,3946L)) &&
       identical(as.integer(summary$reference_only),c(638L,0L,638L)) &&
       identical(as.integer(summary$wteye_only),c(707L,9L,716L)),"Independent comparison differs")
  # Each rate must agree with its complementary overlap rate when defined.
  has_reference <- summary$reference_selected > 0
  has_wteye <- summary$wteye_selected > 0
  need(all(abs(summary$reference_coverage_pct[has_reference] +
               summary$missed_reference_pct[has_reference] - 100) < 1e-10),
       "Coverage and missed-reference percentages do not reconcile")
  need(all(abs(summary$wteye_selected_overlap_pct[has_wteye] +
               summary$extra_wteye_pct[has_wteye] - 100) < 1e-10),
       "Overlap and extra-WTEYE percentages do not reconcile")
  cat("Comparison checks passed. Next: save summary and exact event lists.\n")
  # Write new comparison outputs while preserving every detector input file.
  written<-character()
  save<-function(tab,name) {
    path<-file.path(cleaned_dir,paste0(prefix,name,".csv"))
    write.csv(tab,path,row.names=FALSE,na="",fileEncoding="UTF-8")
    written<<-c(written,path);cat("Saved:",path,"\n")
  }
  save(result,"swap_labels")
  # These two separate files directly answer which swaps were missed or added.
  # Keep their columns even if a category is empty.
  save(result[result$comparison_category == "REFERENCE_ONLY",,drop=FALSE],"missed_reference_swaps")
  save(result[result$comparison_category == "WTEYE_ONLY",,drop=FALSE],"extra_wteye_swaps")
  save(result[result$comparison_category %in% c("REFERENCE_ONLY","WTEYE_ONLY"),,drop=FALSE],"disagreements")
  save(summary,"summary")
  need(identical(unname(tools::md5sum(paths)),inputs$md5),"An input changed during comparison")
  save(data.frame(artifact_type=c(rep("input",length(paths)),rep("output",length(written))),
    file_name=basename(c(paths,written)),md5=unname(tools::md5sum(c(paths,written)))),"manifest")
  print(summary,row.names=FALSE)
  cat("Comparison complete. These are method-agreement measures, not accuracy.\n")
  cat("Next step: send the console output and comparison summary; then inspect missed and extra events.\n")
})
