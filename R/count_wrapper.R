count_path <- "../python/count_guides.py" # assume that cutadapt is in our path

count <- function(fq1_trimmed,
                  guides_fasta,
                  output_dir,
                  sample_name,
                  guide_len,
                  primer = "",
                  mismatches = 1) {

  output_dir <- file.path(output_dir, paste0(sample_name, "_count"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  counts_out <- file.path(output_dir, paste0(sample_name, ".tsv"))
  counts_stderr <- file.path(output_dir, paste0(sample_name, "_count.out"))

  cmd <- paste0("python ",
                count_path,
                " -r1 ", fq1_trimmed,
                " -l ", guides_fasta,
                " -i ", guide_len,
                " -p ", primer,
                " -m ", mismatches,
                " > ", counts_out, " 2> ", counts_stderr)

  message("[[", Sys.time(), "]] Counts reads ---- ")
  print(cmd)

  system(cmd)
  cat(cmd, file = file.path(output_dir, "run_count.log"),
      sep = "\n", append = FALSE)

  counts_out
}
