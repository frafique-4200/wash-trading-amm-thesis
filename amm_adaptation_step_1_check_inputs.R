# ============================================================================
# AMM ADAPTATION -- STEP 1: CHECK OUR INPUT FILES AND R ENVIRONMENT
# ============================================================================
#
# OUR PURPOSE IN PLAIN LANGUAGE
# -----------------------------
# Our purpose is to check that R can find and read our two existing CSV files
# and that the column names needed for the adapted algorithm are present.
# We also report whether the software components needed later are available.
# This helps catch wrong file paths, missing files, and column-name mismatches.
#
# We use this script as a SETUP CHECKPOINT for the adaptation. It differs from the
# academic paper's Stage 1, which performs coarse filtering of swap sequences.
# We do not perform that coarse filtering in this script.
#
# HOW THIS SUPPORTS OUR THESIS
# ---------------------------
# Our proposal's Objective 4 is "Data Transformation and Pipeline Integration".
# We perform this check to prepare for that work. We do not yet transform the
# swaps into graph edges, run SCC analysis, or complete Objective 4.
#
# WHERE WE SAVE THIS SCRIPT, KEEPING ITS FILENAME UNCHANGED
# ---------------------------------------------------
# D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_1_check_inputs.R
#
# OUR EXISTING INPUT FILES -- WE KEEP THESE NAMES UNCHANGED
# ------------------------------------------------
# D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_suspicious_pool_raw_swaps.csv
# D:/Downloads/Bachelor Thesis/Data/raw/uniswap_v2_control_pool_raw_swaps.csv
#
# WHERE WE KEEP THE C++ HELPER FOR A LATER STEP
# -----------------------------------------------------
# D:/Downloads/Bachelor Thesis/Scripts/volume_matching.cpp
# We reuse the supplied internship volume-matching calculation. Here we only
# check whether the file exists. We do not compile it or run its calculation.
#
# WHAT OUR SCRIPT PRODUCES
# ---------------------
# We display the results as text and tables in the R console. We do not create
# output CSVs, rename or move our files, or overwrite their contents. We do not
# create folders or install R packages. We need no SQL query for this step.
# We do not produce candidate groups or make wash-trading decisions here.
#
# HOW WE RUN THIS SCRIPT
# -------------
# In the R console in VS Code, we run the following command at the > prompt:
# source("D:/Downloads/Bachelor Thesis/Scripts/amm_adaptation_step_1_check_inputs.R")
# We run the whole saved file rather than executing its sections separately.
#
# HOW WE READ THE CODE
# --------------------
# We use lines starting with # to explain our work; R does not execute them.
# Text inside quotation marks, such as "Data", is a text value.
# The symbol <- saves a value under a name, called a variable.
# A function is a named operation: for example, file.exists() checks a path.
# Parentheses contain a function's inputs. Commas separate those inputs.
# Curly braces { } keep several instructions together as one block.
# TRUE means yes; FALSE means no. NA means a missing value in R.
# None of these TRUE/FALSE values represents a wash-trading classification.
#
# WHAT WE DO AFTER RUNNING IT
# ---------------------------
# We review the complete console output, including any FALSE entries or error
# messages, and address those results before the next checkpoint.
# If a dependency is missing, we identify the cause before deciding on a fix.

# ============================================================================
# 1. WE SET OUR FOLDER PATHS AND ORIGINAL INPUT FILENAMES
# ============================================================================
# local({ ... }) keeps this script's temporary variables in a separate workspace
# inside R. They do not replace similarly named variables in our main workspace.
# The final }) at the bottom closes this block. We keep the whole block together.

local({
  # We use base_dir to store the path of our main thesis folder on Windows.
  # Forward slashes work in R on Windows and avoid special backslash escapes.
  base_dir <- "D:/Downloads/Bachelor Thesis"

  # file.path() joins folder names into a path. It does not create a folder.
  # raw_dir therefore becomes D:/Downloads/Bachelor Thesis/Data/raw.
  raw_dir <- file.path(base_dir, "Data", "raw")

  # We use c() to combine the TWO original filenames into one named list of values.
  # suspicious_pool and control_pool are labels used to identify the samples
  # in the console. They do not rename the files or establish their true status.
  input_files <- c(
    suspicious_pool = "uniswap_v2_suspicious_pool_raw_swaps.csv",
    control_pool = "uniswap_v2_control_pool_raw_swaps.csv"
  )

  # unname() removes those temporary labels from the in-memory list of names.
  # It does NOT change the filenames. file.path() then builds both full paths.
  input_paths <- file.path(raw_dir, unname(input_files))

  # ========================================================================
  # 2. WE REPORT THE R VERSION, FOLDERS, AND INPUT FILE LOCATIONS
  # ========================================================================
  # cat() prints a message to the console. In quoted text, \n starts a new line.
  # We record the R version to describe the environment used for our study.
  cat("AMM ADAPTATION -- STEP 1: INPUT CHECK\n\n")
  cat("R version:", R.version.string, "\n\n")

  # We use the existing folders agreed for our thesis project.
  folders <- c("Scripts", "Data/raw", "Data/cleaned")

  # data.frame() creates a small table in R's memory; print() displays it.
  # dir.exists() reports TRUE or FALSE for each folder.
  # row.names = FALSE hides automatic row numbers to make the table easier to read.
  # Data/cleaned is where later scripts will save results. This check does not
  # write there, so its absence is reported but does not prevent reading raw data.
  print(data.frame(
    folder = file.path(base_dir, folders),
    exists = dir.exists(file.path(base_dir, folders))
  ), row.names = FALSE)

  # names(input_files) retrieves our sample labels. The "exists" column checks
  # the full path of each input. This first check tests existence, not contents.
  cat("\nInput files:\n")
  print(data.frame(
    sample = names(input_files),
    file = unname(input_files),
    exists = file.exists(input_paths)
  ), row.names = FALSE)

  # all() is TRUE only when every supplied value is TRUE. The ! symbol means NOT.
  # Therefore this means: if NOT every input path exists, stop with a message.
  # Continuing without a required file would not give us a valid input check.
  # call. = FALSE keeps the error message focused on the problem.
  if (!all(file.exists(input_paths))) {
    stop("An input CSV is missing from Data/raw. Send this output for review.",
         call. = FALSE)
  }

  # ========================================================================
  # 3. WE DEFINE WHICH CSV COLUMNS OUR ADAPTATION WILL NEED
  # ========================================================================
  # A CSV is a table. Its column names are the headings in its first line.
  # We require these exact column names for our proposed Uniswap V2 adapter:
  #
  # contract_address : identifies the liquidity pool emitting the swap event.
  # evt_tx_hash       : identifies the blockchain transaction. A transaction can
  #                     contain several swap events, so it is not an event ID alone.
  # evt_tx_from       : identifies the address that started the transaction.
  #                     Later we use it as a proxy for the trader, an assumption
  #                     that does not prove who ultimately owns the tokens.
  # evt_index         : identifies the event's log index, used with the block
  #                     number to order swaps under the Dune export convention.
  # evt_block_number  : identifies the Ethereum block containing the event.
  # amount0In         : raw quantity of token0 entering the pool in this swap.
  # amount0Out        : raw quantity of token0 leaving the pool in this swap.
  # amount1In         : raw quantity of token1 entering the pool in this swap.
  # amount1Out        : raw quantity of token1 leaving the pool in this swap.
  #
  # token0 and token1 are the pool's two token positions. These names alone do not
  # tell us the token symbols or decimals. We do not assume either here.
  # The In/Out directions are from the POOL'S perspective.
  # We check that these headings exist; the detailed validity of their values
  # and the event-ordering assumptions will be checked in the transformation step.
  required_columns <- c(
    "contract_address", "evt_tx_hash", "evt_tx_from", "evt_index",
    "evt_block_number", "amount0In", "amount0Out", "amount1In", "amount1Out"
  )

  # We use this variable to track the basic file/header checks across both inputs.
  # It starts as TRUE and becomes FALSE if either table is empty, is missing a
  # required column, or repeats a column heading. It is not a detection result.
  inputs_ok <- TRUE

  # ========================================================================
  # 4. WE READ AND INSPECT EACH CSV, ONE AT A TIME
  # ========================================================================
  # A for loop repeats instructions. seq_along(input_files) supplies 1 then 2.
  # i is the current position. [[i]] retrieves the item at that position.
  # We therefore apply exactly the same checks to both samples.
  for (i in seq_along(input_files)) {
    cat("\nINPUT:", input_files[[i]], "\n")

    # A. We read the CSV into a temporary table named x.
    # utils::read.csv means use read.csv() from R's built-in utils package.
    # Reading copies the data into memory; it does not edit the original CSV.
    #
    # colClasses = "character": read ALL values as text. Ethereum raw token
    # amounts can be very large integers. Reading them immediately as ordinary
    # R numbers could round their last digits. We do no amount arithmetic here.
    #
    # check.names = FALSE: preserve the column headings so we can inspect the
    # actual schema instead of allowing R to adjust the headings automatically.
    # stringsAsFactors = FALSE: keep text as text rather than categorical codes.
    # na.strings: treat blank cells, "NA", and "<nil>" as missing in memory.
    # These settings do not rewrite the source file.
    x <- utils::read.csv(
      input_paths[[i]], colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c("", "NA", "<nil>")
    )

    # B. We find missing or repeated column headings.
    # names(x) returns the headings found in the file.
    # setdiff(required, found) returns items required but not found.
    missing_columns <- setdiff(required_columns, names(x))

    # duplicated() marks repeated names. Square brackets select those names.
    # unique() lists each repeated name only once for the report.
    # This checks duplicated HEADINGS, not duplicated swap records.
    duplicate_headers <- unique(names(x)[duplicated(names(x))])

    # C. We report the number of rows and columns using nrow() and ncol().
    # These counts help us compare the local inputs with the intended samples.
    # They do not tell us how many swaps are suspicious.
    cat("Rows:", nrow(x), "\nColumns:", ncol(x), "\n")

    # paste(..., collapse = ", ") joins a list of headings into readable text.
    cat("Column names:", paste(names(x), collapse = ", "), "\n")

    # length() counts items. If a missing/duplicate list has any items, print
    # them; otherwise print NONE. NONE is the desired result for these checks.
    cat("Missing required columns:",
        if (length(missing_columns)) paste(missing_columns, collapse = ", ") else "NONE",
        "\n")
    cat("Duplicate column names:",
        if (length(duplicate_headers)) paste(duplicate_headers, collapse = ", ") else "NONE",
        "\n")

    # D. We display the pool address or addresses appearing in this input.
    # %in% asks whether the column exists; $ retrieves that column from x.
    # We guard the access so a missing column is reported without failing here.
    # A later step will check the address format and one-pool-per-file rule.
    if ("contract_address" %in% names(x)) {
      cat("Pool address(es):", paste(unique(x$contract_address), collapse = ", "), "\n")
    }

    # E. We print an MD5 checksum: a short fingerprint calculated from file bytes.
    # It helps record which input version we used and notice later changes.
    # It does not establish that the data is accurate or prove its authenticity.
    # tools is an R package supplied with R. This command only reads the file.
    cat("Input MD5:", unname(tools::md5sum(input_paths[[i]])), "\n")

    # F. We update the combined file/header result.
    # && means AND: every condition must be satisfied.
    # 0L means the integer zero; > means greater than.
    # !length(...) is TRUE for an empty list: there are no missing or repeated
    # headings. A failure in the first input remains a failure after the second.
    inputs_ok <- inputs_ok && nrow(x) > 0L &&
      !length(missing_columns) && !length(duplicate_headers)
  }

  # ========================================================================
  # 5. WE REPORT THE SOFTWARE COMPONENTS NEEDED LATER
  # ========================================================================
  # An R package is a collection of functions for a particular kind of work.
  # igraph provides graph construction and SCC analysis.
  # Rcpp connects R to the supplied C++ volume-matching function.
  # We use built-in R functions for this check, so it can run even if these
  # packages are missing. Their status tells us what to address before detection.
  cat("\nDependencies needed in a later step:\n")
  packages <- c("igraph", "Rcpp")

  # vapply() applies the same check to each package name.
  # requireNamespace() checks whether that package can be loaded. It does not
  # install it. logical(1) tells vapply to expect one TRUE/FALSE answer per name.
  # quietly = TRUE suppresses routine loading messages.
  # TRUE here means loadable in this R session, not that every package function
  # or the C++ compiler has already been tested successfully.
  print(data.frame(
    package = packages,
    installed = vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ), row.names = FALSE)

  # We look for the C++ source file next to our R scripts in the Scripts folder.
  # A .cpp file contains program code, so it belongs with the scripts.
  # FALSE means we still need to place/check that file at the agreed location.
  cat("volume_matching.cpp in Scripts:",
      file.exists(file.path(base_dir, "Scripts", "volume_matching.cpp")), "\n")
  cat("The C++ compiler has not been tested by this step.\n")

  # ========================================================================
  # 6. WE INTERPRET THE RESULT AND STOP AT THIS CHECKPOINT
  # ========================================================================
  # File/header PASS means both CSVs were read, neither was empty, all required
  # headings were present, and no heading was duplicated.
  # It does NOT include the separate folder/package/C++ checks printed above.
  # It does NOT mean all rows are valid, that the graph model is verified, or
  # that wash trading has been detected. No detection algorithm ran in this step.
  #
  # NEEDS REVIEW means at least one of those basic checks failed. An unreadable
  # or missing file may instead stop earlier with an R error. We review either result.
  # Even with PASS, we review the full output, including counts, addresses, and
  # dependencies, before moving to our next script.
  cat("\nFile/header check:", if (inputs_ok) "PASS" else "NEEDS REVIEW", "\n")
  cat("STEP 1 COMPLETE. No CSVs were changed or created.\n")
  cat("This checks file access and headers; it does not validate all row values or detect candidates.\n")
  cat("Send the complete console output before moving to the next step.\n")
})
