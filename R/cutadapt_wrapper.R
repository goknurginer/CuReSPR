# function adapted from
# https://rdrr.io/github/omicsCore/SEQprocess/src/R/02_trimming.R
# the SEQprocess package is dependent on the sequenza package,
# which was removed from CRAN to simplify, I will just use the
# wrapper function directly
# the issue with SEQprocess is that it is under a GPL license,
# so we will want to remove the dependency

cutadapt_path <- "cutadapt" # assume that cutadapt is in our path

cutadapt <- function(fq1,
                     output_dir,
                     sample_name,
                     front_adapter = "TATTTATTTTGCTACTTAATAATTGGGACTA",
                     mismatches = 1,
                     cores = 1) {

  output_dir <- file.path(output_dir, paste0(sample_name, "_trim"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  fq1_trimmed <- file.path(output_dir, paste0(sample_name, "_trimmed.fq"))
  cutadapt_txt <- file.path(output_dir, paste0(sample_name, ".cutadapt.out"))

  cmd <- paste0(cutadapt_path,
                " --discard-untrimmed",
                " --cores ", cores,
                " --errors ", mismatches,
                " --front ", front_adapter,
                " ", fq1, " > ", fq1_trimmed, " 2> ", cutadapt_txt)

  message("[[", Sys.time(), "]] Cut adapter sequence ---- ")
  system(cmd)
  cat(cmd, file = file.path(output_dir, "run_cutadapt.log"),
      sep = "\n", append = FALSE)

  fq1_trimmed
}
