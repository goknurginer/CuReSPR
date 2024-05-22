library(tools)

source("count_wrapper.R")

input1 <- "../data/Base1.fastq"
guides_fasta <- "../data/small_sgRNA_library.fasta"
outdir <-  "test"
sample_name <- tools::file_path_sans_ext(basename(input1))

count_output <- count(input1,
                      guides_fasta,
                      outdir,
                      sample_name)
