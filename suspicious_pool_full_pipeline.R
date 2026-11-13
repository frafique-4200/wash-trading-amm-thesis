# ============================================================
# EMPIRICAL SUSPICIOUS POOL
# COMPLETE PAPER-DERIVED DETECTION PIPELINE
#
# PURPOSE
# ============================================================
#
# Run the complete small-scale empirical wash-trading
# detection pipeline automatically:
#
#   Stage 1 — Coarse filtering
#   Stage 2 — Sender / entity recognition
#   Stage 3 — MEV false-positive elimination
#
#
# IMPORTANT
# ============================================================
#
# The individual Stage-1, Stage-2, and Stage-3 scripts are
# intentionally kept separate.
#
# This master script simply runs them in the correct order.
#
# This is preferable to copying all code into one giant file
# because:
#
#   - each methodological stage remains independently testable;
#   - debugging is easier;
#   - the thesis methodology remains clearly structured;
#   - the complete pipeline can still be executed with one
#     command.
#
# ============================================================



# ============================================================
# SCRIPT DIRECTORY
# ============================================================

script_directory <-
  "D:/Downloads/Bachelor Thesis/Scripts"



# ============================================================
# INDIVIDUAL PIPELINE SCRIPTS
# ============================================================

stage1_script <-
  file.path(
    script_directory,
    "suspicious_pool_stage_1.R"
  )


stage2_script <-
  file.path(
    script_directory,
    "suspicious_pool_stage_2.R"
  )


stage3_script <-
  file.path(
    script_directory,
    "suspicious_pool_stage_3.R"
  )



# ============================================================
# CHECK THAT ALL THREE SCRIPTS EXIST
# ============================================================

required_scripts <-
  c(
    stage1_script,
    stage2_script,
    stage3_script
  )


missing_scripts <-
  required_scripts[
    !file.exists(required_scripts)
  ]



if (length(missing_scripts) > 0) {

  stop(
    paste0(
      "\nThe following required script(s) could not be found:\n\n",
      paste(
        missing_scripts,
        collapse = "\n"
      ),
      "\n\nCheck the filenames before running the pipeline."
    )
  )
}



# ============================================================
# PIPELINE START
# ============================================================

cat("\n")
cat("====================================================\n")
cat("SUSPICIOUS POOL — COMPLETE DETECTION PIPELINE\n")
cat("====================================================\n\n")



pipeline_start_time <-
  Sys.time()



# ============================================================
# STAGE 1
# COARSE FILTERING
# ============================================================

cat("\n")
cat("====================================================\n")
cat("STAGE 1 — COARSE FILTERING\n")
cat("====================================================\n\n")



source(
  stage1_script,
  echo = FALSE
)



cat("\n")
cat(">>> STAGE 1 COMPLETE\n")



# ============================================================
# STAGE 2
# SENDER / ENTITY RECOGNITION
# ============================================================

cat("\n")
cat("====================================================\n")
cat("STAGE 2 — SENDER / ENTITY RECOGNITION\n")
cat("====================================================\n\n")



source(
  stage2_script,
  echo = FALSE
)



cat("\n")
cat(">>> STAGE 2 COMPLETE\n")



# ============================================================
# STAGE 3
# MEV FALSE-POSITIVE ELIMINATION
# ============================================================

cat("\n")
cat("====================================================\n")
cat("STAGE 3 — MEV FALSE-POSITIVE ELIMINATION\n")
cat("====================================================\n\n")



source(
  stage3_script,
  echo = FALSE
)



cat("\n")
cat(">>> STAGE 3 COMPLETE\n")



# ============================================================
# PIPELINE FINISHED
# ============================================================

pipeline_end_time <-
  Sys.time()


pipeline_runtime <-
  difftime(
    pipeline_end_time,
    pipeline_start_time,
    units = "secs"
  )



cat("\n")
cat("====================================================\n")
cat("COMPLETE PIPELINE FINISHED SUCCESSFULLY\n")
cat("====================================================\n\n")



cat(
  "Stage 1: COMPLETE\n"
)


cat(
  "Stage 2: COMPLETE\n"
)


cat(
  "Stage 3: COMPLETE\n"
)



cat(
  "\nTotal pipeline runtime:",
  round(
    as.numeric(pipeline_runtime),
    2
  ),
  "seconds\n"
)



cat("\n")
cat(
  "Final suspicious candidates are stored in:\n"
)


cat(
  "D:/Downloads/Bachelor Thesis/Data/cleaned/",
  "suspicious_pool_final_candidate_swaps.csv\n",
  sep = ""
)



cat("\n")
cat("====================================================\n")