count_path <- "python/count_guides.py"

count <- function(fq1_trimmed,
                  guides_fasta,
                  output_dir,
                  sample_name,
                  guide_len = 20,
                  primer = "",
                  mismatches = 1,
                  infer_primer = TRUE,
                  num_infer = 20,
                  primer_len = 30) {

  output_dir <- file.path(output_dir, paste0(sample_name, "_count"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  counts_out <- file.path(output_dir, paste0(sample_name, ".tsv"))
  counts_stderr <- file.path(output_dir, paste0(sample_name, "_count.out"))

  primer_args <- paste0(" -p ", primer)
  if (infer_primer) {
    primer_args <- paste0(" -I ",
                          " -n ", num_infer,
                          " -P ", primer_len)
  }

  primer_arg <- ifelse(length(primer) > 0, paste0(" -p ", primer), "")
  cmd <- paste0("python ",
                count_path,
                " -r1 ", fq1_trimmed,
                " -l ", guides_fasta,
                " -i ", guide_len,
                primer_args,
                " -m ", mismatches,
                " > ", counts_out, " 2> ", counts_stderr)

  message("[[", Sys.time(), "]] Counts reads ---- ")
  print(cmd)

  system(cmd)
  cat(cmd, file = file.path(output_dir, "run_count.log"),
      sep = "\n", append = FALSE)

  counts_out
}
