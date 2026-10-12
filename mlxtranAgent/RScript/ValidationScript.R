################################################################################
#--------------mlxtran Agent Validation Script (batch re-scoring)--------------#
#------------------------------------------------------------------------------#
# Re-scores the generated models of one or more result folders against the    #
# reference models, using compare.R (one Rscript call per model pair).         #
#                                                                              #
# Usage (from this folder):                                                    #
#   Rscript ValidationScript.R                       # all runs in ../Results  #
#   Rscript ValidationScript.R gpt56sol_low opus5v2_low   # selected runs      #
#                                                                              #
# Requires MonolixSuite (Simulx) with lixoftConnectors installed.              #
# Writes ../Results/<run>/results_rescored.csv for each run.                   #
################################################################################

args <- commandArgs(trailingOnly = TRUE)

path.prog <- tryCatch(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))),
  error = function(e) getwd()
)
if (length(path.prog) == 0) path.prog <- getwd()

path.compare <- file.path(path.prog, "compare.R")
path.refs    <- file.path(path.prog, "..", "ValidationDataSets", "references")
path.results <- file.path(path.prog, "..", "Results")

runs <- if (length(args) > 0) args else list.dirs(path.results, full.names = FALSE, recursive = FALSE)

for (run in runs) {
  run.dir <- file.path(path.results, run)
  report.file <- file.path(run.dir, "validation_report.csv")
  if (!file.exists(report.file)) next

  report <- read.csv(report.file, stringsAsFactors = FALSE)
  # Each generated file is named <Reference>_generated_rep<Replicate>.txt
  report$generated_file <- sprintf("%s_generated_rep%d.txt", report$Reference, report$Replicate)
  report <- report[file.exists(file.path(run.dir, report$generated_file)), ]
  report <- report[!duplicated(report$generated_file, fromLast = TRUE), ]
  if (nrow(report) == 0) {
    cat(sprintf("[%s] no generated models found, skipping\n", run))
    next
  }

  cat(sprintf("[%s] re-scoring %d models\n", run, nrow(report)))

  report$Pass_rescored <- NA
  report$Details_rescored <- NA
  for (i in seq_len(nrow(report))) {
    gen <- file.path(run.dir, report$generated_file[i])
    ref <- file.path(path.refs, paste0(report$Reference[i], ".txt"))
    out <- suppressWarnings(system2("Rscript", c(shQuote(path.compare), shQuote(gen), shQuote(ref)),
                                    stdout = TRUE, stderr = FALSE))
    status <- attr(out, "status")
    if (!is.null(status) && status != 0) {
      report$Pass_rescored[i] <- "FAIL"
      report$Details_rescored[i] <- "R error"
    } else {
      report$Pass_rescored[i] <- ifelse(tail(trimws(out), 1) == "[1] TRUE", "PASS", "FAIL")
      report$Details_rescored[i] <- paste(trimws(grep("^  ", out, value = TRUE)), collapse = "; ")
    }
    cat(sprintf("  %-45s original=%s rescored=%s\n", report$generated_file[i],
                report$Pass[i], report$Pass_rescored[i]))
  }

  write.csv(report[, c("Row", "Replicate", "Model.type", "Reference", "generated_file",
                       "Pass", "Pass_rescored", "Details_rescored")],
            file.path(run.dir, "results_rescored.csv"), row.names = FALSE)
  cat(sprintf("[%s] %d/%d PASS after re-scoring\n\n", run,
              sum(report$Pass_rescored == "PASS"), nrow(report)))
}
