library(dplyr)

source("count_wrapper.R")

input1 <- "../data/Base1.fastq"
guides_fasta <- "../data/small_sgRNA_library.fasta"
outdir <-  "test"

sample_name <- basename(input1) %>%
  strsplit(., "(.fastq|.fq)") %>%
  first() %>%
  first()

count_output <- count(input1,
                      guides_fasta,
                      outdir,
                      sample_name)
