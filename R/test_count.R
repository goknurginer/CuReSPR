library(tools)

source("cutadapt_wrapper.R")
source("count_wrapper.R")

input1 <- "../data/Base1.fastq"
guides_fasta <- "../data/small_sgRNA_library.fasta"
outdir <-  "test"
sample_name <- tools::file_path_sans_ext(basename(input1))

trim_outfile <- cutadapt(input1,
                         outdir,
                         sample_name,
                         front_adapter = "TATTTATTTTGCTACTTAATAATTGGGACT",
                         mismatches = 1,
                         cores = 1)

count_output <- count(trim_outfile,
                      guides_fasta,
                      outdir,
                      sample_name,
                      guide_len = 20)
