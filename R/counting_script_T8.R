# counting T8 example ----
# dir.create("T8")
 path = "/Users/giner.g/Documents/Github/CuReSPR/T8"
## mageck ----
# dir.create(paste0(path,"/mageck"))
setwd(paste0(path,"/mageck"))
library(reticulate) # to run python code in r
venv_path <- "/Users/giner.g/miniconda3/envs/mageckenv/bin/python"
use_python(venv_path, required = TRUE)
system("/Users/giner.g/miniconda3/envs/mageckenv/bin/mageck count -l ../guidelibrary/brunello.tsv -n ./T8_results --sample-label Control,Vehicle,T8 --fastq ../fastqs/T0-Control.fastq.gz ../fastqs/T8-Vehicle.fastq.gz ../fastqs/T8-APR-246.fastq.gz", intern = TRUE)

## RSubread ----
library(Biostrings)
library(Rsubread)
library(GenomicAlignments)
# dir.create(paste0(path,"/rsubread"))
setwd(paste0(path,"/rsubread"))
guides <- read.csv("../guidelibrary/brunello.csv",header=FALSE)
head(guides)
sgRNAs <- DNAStringSet(guides$V2)
names(sgRNAs) <- guides$V1
sgRNAs
# dir.create("./index")
writeXStringSet(sgRNAs, file = "./index/guides.fa")
buildindex("./index/guides", "./index/guides.fa", indexSplit = FALSE)
setwd(paste0(path,"/fastqs"))
# single-end
reads <- dir(full.names = TRUE, pattern = "*.fastq*")
counts <- list()
mapping_results <- list()
for (i in 1:length(reads)) {
  mapping_results[[i]] <- align("../rsubread/index/guides",
                                readfile1 = reads[i],
                                output_file = gsub(".fastq", ".bam",
                                                   tools::file_path_sans_ext(reads[i])),
                                nthreads = 4,
                                unique = TRUE,
                                nBestLocations = 1,
                                type = "DNA",
                                TH1 = 1,
                                maxMismatches = 0,
                                indels = 0)
  temp <- readGAlignments(gsub(".fastq", ".bam",
                               tools::file_path_sans_ext(reads[i])))
  counts[[i]] <- data.frame(table(seqnames(temp[width(temp) == "20"])), row.names = "Var1")
}
my_counts <- do.call(cbind, counts)
colnames(my_counts) <- reads
write.table(my_counts, paste0(path,"/rsubread/counts.txt"))

# paired-end
reads1 <- dir(full.names = TRUE, pattern = "*1.fastq*")
reads2 <- dir(full.names = TRUE, pattern = "*2.fastq*")
counts <- list()
mapping_results <- list()
for (i in 1:length(reads)) {
  mapping_results[[i]] <- align("../rsubread/index/guides",
                                readfile1=reads1[i],
                                readfile2=reads2[i],
                                output_file = gsub(".fastq.gz", ".bam",
                                                   tools::file_path_sans_ext(reads[i])),
                                nthreads = 4,
                                unique = TRUE,
                                nBestLocations = 1,
                                type = "DNA",
                                TH1 = 1,
                                maxMismatches = 0,
                                indels = 0)
  temp <- readGAlignments(gsub(".fastq.gz", ".bam",
                               tools::file_path_sans_ext(reads[i])))
  counts[[i]] <- data.frame(table(seqnames(temp[width(temp) == "20"])), row.names = "Var1")
}
my_counts <- do.call(cbind, counts)
colnames(my_counts) <- reads
write.table(my_counts, paste0(path,"/rsubread/counts.txt"))

# wehi-marek
