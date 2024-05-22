library(dplyr)
library(reticulate)

source("R/count_wrapper.R")

# Define Conda environment and packages for counting script
conda_dependencies <- c("pandas", "pyfastx", "python-edlib")
conda_env_name <- "count-env"

conda_create(conda_env_name,
             packages = conda_dependencies,
             channel = c("bioconda", "conda-forge"))
use_condaenv(conda_env_name, required = TRUE)

# Set the environment variables to ensure the correct python is used
python_path <- conda_python(conda_env_name)
Sys.setenv(RETICULATE_PYTHON = python_path)
Sys.setenv(PATH = paste(dirname(python_path), Sys.getenv("PATH"), sep = ":"))

# input parameter setup
input1 <- "data/Base1.fastq"
guides_fasta <- "data/small_sgRNA_library.fasta"
outdir <-  "test"
sample_name <- basename(input1) %>%
  strsplit(., "(.fastq|.fq)") %>%
  first() %>%
  first()

# call the count wrapper function
count_output <- count(input1,
                      guides_fasta,
                      outdir,
                      sample_name)
