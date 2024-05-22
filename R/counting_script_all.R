# counting base-low-high example ----
## mageck ----
# install.packages("reticulate")
library(reticulate) # to run python code in r
# conda activate mageckenv
# which python
# Specify the path to the Python executable within the virtual environment
venv_path <- "/Users/giner.g/miniconda3/envs/mageckenv/bin/python"
# Use the specified Python executable
use_python(venv_path, required = TRUE)
# Run mageck count function
system("/Users/giner.g/miniconda3/envs/mageckenv/bin/mageck count -l ./guidelibrary/guide_library.txt -n mageckcounts --sample-label Base,High,Low --fastq ./fastqs/Base1.fastq ./fastqs/High1.fastq ./fastqs/Low1.fastq --fastq-2 ./fastqs/Base2.fastq ./fastqs/High2.fastq ./fastqs/Low2.fastq", intern = TRUE)

system("/Users/giner.g/miniconda3/envs/mageckenv/bin/mageck count -l ./guidelibrary/GeCKOv21_Human.tsv -n mageckcountsTox --sample-label Control,Control,ToxA,ToxA,ToxB,ToxB --fastq ./fastqs/PBS_Repl1_S14_L008_R1_001_x.fastq ./fastqs/PBS_R2_S15_L008_R1_001_x.fastq ./fastqs/Tox-A_R01_98_S2_L008_R1_001_x.fastq ./fastqs/Tox-A_R2_S2_L005_R1_001_x.fastq ./fastqs/Tox-B-Repl1_98_S4_L008_R1_001_x.fastq ./fastqs/Tox-B-Repl2_S4_L005_R1_001_x.fastq", intern = TRUE)

system("/Users/giner.g/miniconda3/envs/mageckenv/bin/mageck count -l ./guidelibrary/Brunello.tsv -n mageckcountsT8 --sample-label Control,Vehicle,T8 --fastq ./fastqs/T0-Control.fastq.gz ./fastqs/T8-Vehicle.fastq.gz ./fastqs/T8-APR-246.fastq.gz", intern = TRUE)

## read guide table
library(phylotools) # to call read.fasta()
r <- read.fasta(file = "./guidelibrary/small_sgRNA_library.fasta", clean_name = FALSE)
head(r)
r$geneID <- limma::strsplit2(r$seq.name,"_")[,1]
write.table(r,"./guidelibrary/guide_library.txt", row.names = FALSE)
guides <- read.table("./guidelibrary/guide_library.txt", header = TRUE)
## read count table WEHI
data <- read.csv("/Users/giner.g/Documents/Github/CuReSPR/counts/count_matrix_WEHI.csv")
data$geneID <- limma::strsplit2(data$gene,"_")[,1]
colnames(data)[1:2] <- c("guideSeq","guideID")
data <- data[,c(1,2,6,3,4,5)]
write.csv(data,"./counts/count_matrix_WEHI.csv", row.names = FALSE)
## create count table with Rsubread
colnames(guides) <- c("UID","seq","gene_id")
head(guides)
sgRNAs <- DNAStringSet(guides$seq)
names(sgRNAs) <- guides$UID
sgRNAs
writeXStringSet(sgRNAs, file = "./index/guides.fa")
buildindex("./index/guides", "./index/guides.fa", indexSplit = FALSE)
setwd("./fastqs/")
reads1 <- dir(full.names = TRUE, pattern = "*1.fastq$")
reads2 <- dir(full.names = TRUE, pattern = "*2.fastq$")
counts <- list()
mapping_results <- list()
for (i in 1:length(reads1)) {
  mapping_results[[i]] <- align("../index/guides",
                                readfile1=reads1[i],
                                readfile2=reads2[i],
                                output_file = gsub(".fastq", ".bam", reads1[i]),
                                nthreads = 4,
                                unique = TRUE,
                                nBestLocations = 1,
                                type = "DNA",
                                TH1 = 1,
                                maxMismatches = 0,
                                indels = 0)
  temp <- readGAlignments(gsub(".fastq", ".bam", fastqs[i]))
  counts[[i]] <- data.frame(table(seqnames(temp[width(temp) == "20"])), row.names = "Var1")
}

my_counts <- do.call(cbind, counts)

colnames(my_counts) <- c("Control_1", "Control_2",
                         "ToxA_1", "ToxA_2",
                         "ToxB_1", "ToxB_2")
write.table(my_counts, "my_counts.txt")

##. create count table with mageck



# toxa-toxb example
setwd("~/Documents/Github/CuReSPR")
GeCKO <- read.delim("guidelibrary/GeCKOv21_Human.tsv")
GeCKO[1:2, ]
sgRNAs <- DNAStringSet(GeCKO$seq)
names(sgRNAs) <- GeCKO$UID
sgRNAs
writeXStringSet(sgRNAs, file = "./index/GeCKO.fa")
buildindex("./index/GeCKO", "./index/GeCKO.fa", indexSplit = FALSE)
fastqs <- dir(path = "./fastq_files", pattern = "*.fastq.gz", full.names = TRUE)
counts <- list()
mapping_results <- list()
for (i in 1:length(fastqs)) {
  mapping_results[[i]] <- align("./index/GeCKO", fastqs[i], output_file = gsub(".fastq.gz",
                                                                               ".bam", fastqs[i]), nthreads = 4, unique = TRUE, nBestLocations = 1, type = "DNA",
                                TH1 = 1, maxMismatches = 0, indels = 0)
  temp <- readGAlignments(gsub(".fastq.gz", ".bam", fastqs[i]))
  counts[[i]] <- data.frame(table(seqnames(temp[width(temp) == "20"])), row.names = "Var1")
}
my_counts <- do.call(cbind, counts)
colnames(my_counts) <- c("Control_1", "Control_2", "ToxA_1", "ToxA_2", "ToxB_1",
                         "ToxB_2")
write.table(my_counts, "my_counts.txt")

counts <- read.table("my_counts.txt", header = TRUE)
group <- factor(c("Control", "Control", "ToxA", "ToxA", "ToxB", "ToxB"), levels = c("Control",
                                                                                    "ToxA", "ToxB"))
samples <- data.frame(group = group, sampleName = colnames(counts), biorep = rep(c(1,
                                                                                   2), 3))
genes <- GeCKO
names(genes)[names(genes) == "gene_id"] <- "Symbol"
d <- DGEList(counts = counts, samples = samples, genes = genes)
d
save(d, file = "./RData/DGEList.RData")


# mageck
mageck count -l ./guidelibrary/GeCKOv21_Human.tsv -n mageckcounts --sample-label Base,High,Low --fastq Base1.fastq High1.fastq Low1.fastq --fastq-2 Base2.fastq High2.fastq Low2.fastq
