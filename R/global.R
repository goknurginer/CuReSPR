# Load the necessary packages ####
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(refund.shiny)
library(tidyverse)
library(readr)
library(edgeR)
library(stringr)
library(RColorBrewer)
library(varhandle)
library(gplots)
library(here)
library(MAGeCKFlute)
library(clusterProfiler)
library(ggplot2)

utils::globalVariables(c("reactiveVal",
                         "renderPlot",
                         "renderDataTable",
                         "dataTableOutput",
                         "read.table",
                         "write.csv",
                         "barplot",
                         "pdf",
                         "dev.off"))

# Set maximum upload size to 1 GB ####
options(shiny.maxRequestSize = 1000 * 1024^2)

# Functions ####
# read_file_and_render function ####
# Define a function to read files and render DataTable
read_file_and_render <- function(file_input = NULL, example_file_path = NULL) {
    # Check which file path to use
    if (!is.null(example_file_path)) {
        file_path <- example_file_path
        file_ext <- tools::file_ext(file_path)
    } else if (!is.null(file_input)) {
        req(file_input)
        file_path <- file_input$datapath
        file_ext <- tools::file_ext(file_input$name)
    } else {
        stop("No file provided. Please upload a file or select an example.")
    }

    # Read file based on extension and detect separator if necessary
    df <- switch(file_ext,
                 "csv" = read.table(file_path, check.names = FALSE, sep = ",", header = TRUE),
                 "tsv" = read.table(file_path, check.names = FALSE, sep = "\t", header = TRUE),
                 "txt" = {
                     first_line <- readLines(file_path, n = 1)
                     sep_char <- if (grepl("\t", first_line)) "\t" else " "
                     read.table(file_path, check.names = FALSE, sep = sep_char, header = TRUE)
                 },
                 stop("Unsupported file type"))
    return(df)
}


sample_data <- function(){
  e <- new.env()
  name <- load("sample data/sample_df.RData")
  x <- e[[name]]
  return(x)
}

genes_data <- function(){
  e <- new.env()
  name <- load("sample data/genes_df.RData")
  x <- e[[name]]
  return(x)
}

read_file <- function(x){
  name <- toString(x$name)
  temp <-  strsplit(name,  "")
  name <- temp[[1]]
  if (prod(name[(length(name)-3):length(name)] == strsplit(".rds","")[[1]])){
    return(readRDS(x$datapath))
  }
  if (prod(name[(length(name)-5):length(name)] == strsplit(".RData","")[[1]])){
    e <- new.env()
    name <- load(x$datapath, envir = e)
    return(e[[name]])
  }
}

get_library <- function(x){
  if (class(x)[1]=="DGEList"){
    return(colnames(x$count))
  }
}

get_dge <- function(x){
  if (class(x)[1]=="DGEList"){
    return(x)
  }
}

# -------------------- Quality Check ----------------------------------------

get_count_matrix <- function(dge){
  return(dge$counts)
}

get_quantile <- function(dge, lib, fastq_set){
  q <- quantile(dge$counts[, match(lib, fastq_set)], 
                probs = c(0.05, 0.1, 0.9, 0.95))
  return(q)
}

quantile_table <- function(q){
  d <- data.frame(Percentile = c("5th", "10th", "90th", "95th", 
                            paste("90th/10th (80% of sgRNA within ", 
                                   round(q[3]/q[1], digits = 2), "-fold range)"), 
                            paste("95th/5th (90% of sgRNA within ",
                                   round(q[4]/q[1], digits = 2), "-fold range)")), 
             Data.treshold = round(c(q, q[3]/q[1], q[4]/q[1]), digits = 2))
  return(d)
}


plot_cdf <- function(dge, q, lib, fastq_set){
  plot(ecdf(dge$counts[, match(lib, fastq_set)]), col = "lightblue", 
       xlab = "sgRNA counts", 
       main = paste("Cumulative Distribution of sgRNA Counts for ",lib, sep = ""), 
       ylab = "Cumulative Frequency")
  for (i in c(0, 100, 200, 300)) {
    for (j in c(0, 0.2, 0.4, 0.6, 0.8, 1)) {
      abline(a = j, b = 0, col = "lightgray", lty = "dashed")
      abline(v = i, col = "lightgray", lty = "dashed")
    }
  }
  abline(v = q[2], col = "red", lty = "dashed")
  abline(v = q[3], col = "blue", lty = "dashed")
}  
  

get_gene_dist <- function(dge, gene, xlab){
  index <- match("Symbol", names(dge$genes))
  if ( is.na(index) ){
    index <- match("gene_symbol", names(dge$genes))
  }
  extract <- dge$counts[rownames(dge$genes)[dge$genes[,index] == gene],]
  if (is.null(dim(extract))){
    count <- extract
  } else{
    count <- colSums(dge$counts[rownames(dge$genes)[dge$genes[,index] == gene],])
  }
  barplot(count, xlab = paste("Number of ",gene, " in different samples", sep = ""),
          main = paste("Distribution of ", gene, sep=""),
          col = "#CD534CFF", ylab = "frequency", names = xlab,
          border = NA)
}

zero_heatmat <- function(d, group, xlab){
  dge <- d$counts
  Z <- dge
  M <- Z
  M[M > 0] <- 1
  o <- order(aveLogCPM(Z), decreasing=FALSE)
  M <- M[o,]
  par(cex.main=0.75)
  col1 <- RColorBrewer::brewer.pal(7, "Pastel1")
  col2 <- RColorBrewer::brewer.pal(7, "Accent")
  n.replicate <- ncol(dge)/length(unique(group))
  heatmap(M,
          labRow = NA,
          labCol = xlab,
          cexRow=1,
          main=paste0("Zeros across samples (ordered by aveLogCPM)"),
          scale="none", 
          Rowv=NA, 
          Colv=NA, 
          ColSideColors=col1[rep(1:(ncol(dge)/n.replicate),each=n.replicate)],
          col=col2[c(4,5)])
  legend(x="right", legend=c("zero", "none zero"),fill=col2[c(4,5)])
}

# -------------------- Pre - processing -------------------------------------


filter3_update <- function(dge, col, groups){
  #print(dim(dge))
  cpm_dge <- edgeR::cpm(dge, log=FALSE)
  M <- median(dge$samples$lib.size) * 1e-6
  
  if(class(dge) != "DGEList"){
    message("obeject not a DGEList")
  }
  
  # Errors, warnings, and messages can be generated within R code using the 
  # functions stop, stopifnot, warning, and message
  # stop("Something erroneous has occurred!")
  
  
  # first filter out guides with low counts and variability across all groups
  mean.sd <- apply(cpm(dge$counts, log = TRUE),1,sd)
  lq_sd <- quantile(mean.sd, 0.05)
  mean.count <- rowMeans(cpm(dge$counts, log = TRUE))
  lq_count <- quantile(mean.count, 0.05)
  
  dge <- dge[mean.sd > lq_sd & mean.count > lq_count,]
  cpm_dge <- edgeR::cpm(dge, log=FALSE)
  M <- median(dge$samples$lib.size) * 1e-6
  
  # if groups argument not supplied, take all the groups into consideration
  if(is.na(groups[1]) | is.null(groups[1])){
    groups <- unique(dge$samples[,col])
  }
  
  ineff_guide_num <- colSums(dge$counts == 0)
  ineff_guide_prop <- ineff_guide_num/dim(dge)[1]
  
  group_col <- vector(mode = "list", length = length(groups)) # column index of the groups
  num_lanes <- vector() # number of lanes each group has
  ineff_guide_mean <- vector() # mean proportion of inefficient guide for each group
  
  j <- 1
  
  for(i in groups){
    col <- as.character(col)
    temp <- which(dge$samples[,col] == i)
    group_col[[j]] <- temp
    ineff_guide_mean[j] <- mean(ineff_guide_prop[temp])
    num_lanes[j] <- length(temp)
    j <- j+1
  }
  
  # vector for controlled group
  controls <- setdiff(unique(dge$samples[,col]), groups) 
  ctr_col <- vector(mode = "list", length = length(controls)) # column index of the groups
  num_lanes_ctr <- vector() # number of lanes each group has
  j <- 1
  for(i in controls){
    col <- as.character(col)
    temp <- which(dge$samples[,col] == i)
    ctr_col[[j]] <- temp
    num_lanes_ctr[j] <- length(temp)
    j <- j+1
  }
  
  
  if(min(num_lanes) < 2){
    message("At least 2 biological replicates in each group")
  }
  
  # keep out the group that has the highest proportion of inefficient guide
  keep <- which(ineff_guide_mean == max(ineff_guide_mean))
  keep_group <- group_col[keep]
  group_col <- group_col[-keep]
  num_lanes <-  num_lanes[-keep]
  
  # adjusting the 3 parameters
  #C <- median(cpm_dge[,unlist(group_col)]) # threshold for count
  L <- 0.4 # threshold for biological replicates
  keep_cpm <- cpm_dge[,keep_group[[1]]]
  threshold <- quantile(keep_cpm[keep_cpm != 0], probs = 0.25)
  ctr_cpm <- cpm_dge[,unlist(ctr_col)]
  threshold_ctr <- quantile(ctr_cpm[ctr_cpm != 0], probs = 0.25)
  #threshold <- (quantile(dge$counts[,keep_group[[1]]],.99)/M)
  
  good_guides <- vector(mode = "list", length = length(group_col)) # number of lanes that passed the count threshold
  
  # keep if had large count in control, low count in treat; 
  # low count in control, high count in treat
  for(i in 1:length(group_col)){
    L_prop <- ceiling(num_lanes[i]*L)
    group_cpm <- cpm_dge[,(group_col[[i]])]
    C <- quantile(group_cpm [group_cpm  != 0],probs = 0.25)
    good_guides[[i]] <- (rowSums(group_cpm > C) >= L_prop) |
      ((rowSums(keep_cpm > threshold) >= 2)) #| 
     # ((rowSums(ctr_cpm < threshold_ctr) >= 2))
  }
  
  bool_index <- good_guides[[1]]
  
  if(length(good_guides) > 1){
    for(i in 2:length(good_guides)){
      bool_index <- bool_index+good_guides[[i]]
    }
    bool_index <- bool_index >= 1
  }
  
  filtered <- dge[bool_index,, keep.lib.sizes=FALSE]
  #print(dim(filtered))
  return(filtered)
}



edgeR_filter <- function(dge, group){
    keep.exprs <- filterByExpr(dge, group=group)
    dge <- dge[keep.exprs,, keep.lib.sizes=FALSE]
    return(dge)
}

zero_filter <- function(dge){
  return(dge[rowSums(dge$counts==0)<ncol(dge),]
)
}

plot_density <- function(dge, title){
  par(mar=rep((3),4))
  L <- mean(dge$samples$lib.size) * 1e-6
  M <- median(dge$samples$lib.size) * 1e-6
  lcpm.cutoff <- log2(10/M + 2/L)
  samplenames <- colnames(dge$counts)
  lcpm <- cpm(dge, log=TRUE)
  
  if(length(lcpm) == 0){
    plot(1, type="n", xlab="Data is empty after filtering", 
         ylab="Data is empty after filtering", xlim=c(0, 10), ylim=c(0, 10))
    title(main=title)
  } else{
    nsamples <- ncol(dge)
    col <- brewer.pal(nsamples, "Paired")
    plot(density(lcpm[,1]), col=col[1], lwd=2 , las=2, main="", xlab="", ylim=c(0, 3), bw=0.4)
    title(main=title, xlab="Log-cpm")
    abline(v=lcpm.cutoff, lty=3)
    for (i in 2:nsamples){
      den <- density(lcpm[,i])
      lines(den$x, den$y, col=col[i], lwd=2, bw=0.4)
    }
    legend("topright", samplenames, text.col=col, bty="n")
  }
}

plot_boxplot <- function(dge, title, x_lab){
  par(mar=rep((4),4))
  lcpm <- cpm(dge, log=TRUE)
  nsamples <- ncol(dge)
  col <- brewer.pal(nsamples, "Paired")
  boxplot(lcpm, las=2, col=col, main="", names = x_lab)
  title(main=title, ylab="Log-cpm")
}
  

# function to check whether Treatment or Group column should be use for group name
get_group_info <- function(dge){
  nsamples <- ncol(dge)
  len <- unique(dge$samples$group)
  info <- "group"
  if ( 1==len || nsamples==len){
    info <- "Treatment"
  }
  return(info)
}

# zero imputation
zero_imp <- function(dge, group){
  d <- dge$counts
  n <- ncol(d)
  n.replicate <- n/length(unique(group))
  L <- list()
  start <- 1
  i <- 1
  while (start < n){
    end <- start +n.replicate-1
    L[[i]] <- start:end
    i <- i+1
    start <- start +n.replicate
  }
  Z <- d
  Z.imp <- NULL
  for(j in 1:length(L)){
    g <- L[[j]]
    Z.star <- Z[,g]
    lib.sizes <- colSums(Z.star)
    for(i in 1:nrow(Z.star)){
      gene.counts <- Z.star[i,]
      is.zero <- gene.counts==0
      if(sum(is.zero) > 0 & !sum(is.zero)==ncol(Z.star)){
        imputed <- lib.sizes*sum(gene.counts[!is.zero])/sum(lib.sizes[!is.zero])
        gene.counts[is.zero] <- imputed[is.zero]
        Z.star[i,] <- gene.counts
      }
    }
    Z.imp <- cbind(Z.imp, Z.star)
    dim(Z.imp)
  }
  return(DGEList(Z.imp, genes=dge$genes, samples = dge$samples))
}

plot_mds <- function(dge, title){
  par(mar=rep((3),4))
  lcpm <- cpm(dge, log=TRUE)
  if (! is.factor(dge$samples$group)){
    group  <- as.factor(dge$samples$group)
  } else{
    group <- dge$samples$group
  }
  col.group <- group
  levels(col.group) <-  brewer.pal(nlevels(col.group), "Set1")
  col.group <- as.character(col.group)
  plotMDS(lcpm, labels=group, col=col.group)
  title(main=title)
}

# -------------------- Analysis -------------------------------------
get_design<- function(dge){
  biorep <- make.names(dge$samples$biorep) 
  group <- make.names(dge$samples$group)
  # include biorep in design if there is biorep
  if(length(unique(biorep)) >1){
    design <- model.matrix(~0+group+biorep, data = dge)
    colnames(design) <- gsub("group", "", colnames(design))
  } else {
    design <- model.matrix(~0+group, data = dge)
    colnames(design) <- gsub("group", "", colnames(design))
  }
  
  return(design)
}


get_contrast_matrix <- function(dge, design){
  groups <- make.names(unique(dge$samples$group))
  n.pairs <- factorial(length(groups))/(factorial(2)*factorial(length(groups)-2))
  unique.pairs <- data.frame((matrix(ncol=2, nrow=n.pairs)))

  k <- 1
  for(i in 1:(length(groups)-1)){
    for(j in (i+1):length(groups)){
       unique.pairs[k,1] <- groups[i]
       unique.pairs[k,2] <- groups[j]
       k <- k+1
    }
  }
  x <- paste(unique.pairs[,1], unique.pairs[,2], sep = "-")
  contrast <- makeContrasts(contrasts = x,levels = make.names(colnames(design)))
  return(contrast)
}


limma_pipeline <- function(dge, design, contrast){
  v <- voom(dge, design, plot=FALSE)
  vfit <- lmFit(v, design)
  vfit <- contrasts.fit(vfit, contrasts=contrast)
  efit <- eBayes(vfit)
  return(efit)
}

limma_table <- function(efit){
  temp <- summary(decideTests(efit))
  df <- data.frame(matrix(ncol=dim(temp)[2], nrow=dim(temp)[1]))
  colnames(df) <- colnames(temp)
  rownames(df) <- rownames(temp)
  for(i in 1:nrow(temp)){
    df[i,] <- temp[i,]
  }
  return(df)
}

get_limma_ven <- function(efit, comp1, comp2){
  dt <- decideTests(efit)
  col1 <- match(comp1,colnames(dt))
  col2 <- match(comp2,colnames(dt))
  de.common <- which(dt[,col1]!=0 & dt[,col2]!=0)
  length(de.common)
  vennDiagram(dt[,c(col1, col2)], circle.col=c("turquoise", "salmon"))
}



limma_common <- function(efit, comp1, comp2){
  dt <- decideTests(efit)
  col1 <- match(comp1,colnames(dt))
  col2 <- match(comp2,colnames(dt))
  de.common <- which(dt[,col1]!=0 & dt[,col2]!=0)
  if(length(de.common) != 0){
    
    e <- new.env()
    name <- load(here("data","gene_description.RData"), envir = e)
    x <- e[[name]]$genes
    
    get_gene_des <- function(y){
      if(!is.na(y) & y %in% x$Symbol){
        return(x[x$Symbol==y & !is.na(x$Symbol), ][1,4])
      } else{
        return(NA)
      }
    }

    temp <- sapply(efit$genes$gene_symbol[de.common], get_gene_des)
    df <- data.frame("DE genes in common" = efit$genes$gene_symbol[de.common],
                     "description" = temp)
    return(df)
  } else{
    return(data.frame("DE genes in common" = "None in common"))
  }
}

limma_ind_DE <- function(efit, comp){
  col <- match(comp,colnames(efit$contrasts))
  return(topTreat(efit, coef=col, n=Inf))
}
get_limma_md <- function(efit, comp){
  col <- match(comp,colnames(efit$contrasts))
  dt <- decideTests(efit)
  plotMD(efit, column=col, status=dt[,col], main=colnames(efit)[col])
}



get_limma_heatmap <- function(efit, comp, dge){
  #dev.off()
  n.replicate <- ncol(dge)/length(unique(dge$samples$group))
  col <- match(comp,colnames(efit$contrasts))
  col1 <- RColorBrewer::brewer.pal(7, "Purples")
  tt <- topTreat(efit, coef=col, n=Inf)
  topgenes <- tt$gene_symbol[1:100]
  i <- which(efit$genes$gene_symbol %in% topgenes)
  mycol <- colorpanel(1000,"blue","white","red")
  lcpm <- cpm(dge, log=TRUE)
  #par(mar=rep(0.01,4))
  heatmap.2(lcpm[i,], scale="row",
            labRow=efit$genes$gene_symbol[i], labCol=dge$samples$group,
            col=mycol, trace="none", density.info="none", dendrogram = "none",
            key.title = "color key",  keysize = 1.5, Rowv = NA, Colv = NA, key = TRUE,
            ColSideColors = col1[rep(1:(ncol(dge)/n.replicate),each=n.replicate)] ) # lmat=rbind(c(3,4),c(2,1)),
}


edgeR_pipeline <- function(dge, design, method, contrast){
  if(method == "glm"){
    fit <- glmFit(dge, design)
    lrt <- list()
    for (i in 1:(ncol(contrast))){
      lrt[[i]] <- glmLRT(fit, contrast =contrast[,i])
    }
    return(lrt)
  } else{
    fit <- glmQLFit(dge, design)
    qlf <- list()
    for (i in 1:(ncol(contrast))){
      qlf[[i]] <- glmQLFTest(fit, contrast =contrast[,i]) 
    }
    return(qlf)
  }
  
}

edgeR_table <- function(result){
  if(length(result) >= 2){
    temp <- cbind(summary(decideTests(result[[1]])),summary(decideTests(result[[2]])))
  }
  if(length(result) > 2){
    for(i in 3:length(result)){
      temp <- cbind(temp,summary(decideTests(result[[i]])))
    }
  }
  
  df <- data.frame(matrix(ncol=dim(temp)[2], nrow=dim(temp)[1]))
  colnames(df) <- colnames(temp)
  rownames(df) <- rownames(temp)
  for(i in 1:nrow(temp)){
    df[i,] <- temp[i,]
  }
  colnames(df) <- gsub("1\\*","" ,colnames(df))
  return(df)
}

edgeR_ven <- function(result, comp1, comp2, contrast){
  col1 <- match(comp1,colnames(contrast))
  col2 <- match(comp2,colnames(contrast))
  dt1 <- decideTests(result[[col1]])
  dt2 <- decideTests(result[[col2]])
  colnames(dt1) <- gsub("1\\*","" ,colnames(dt1))
  colnames(dt2) <- gsub("1\\*","" ,colnames(dt2))
  de.common <- which(dt1[,1]!=0 & dt2[,1]!=0)
  length(de.common)
  vennDiagram(data.frame(dt1[,1],dt2[,1]), circle.col=c("turquoise", "salmon"))
}

edgeR_common <- function(result, comp1, comp2, dge, contrast){
  col1 <- match(comp1,colnames(contrast))
  col2 <- match(comp2,colnames(contrast))
  dt1 <- decideTests(result[[col1]])
  dt2 <- decideTests(result[[col2]])
  colnames(dt1) <- gsub("1\\*","" ,colnames(dt1))
  colnames(dt2) <- gsub("1\\*","" ,colnames(dt2))
  de.common <- which(dt1[,1]!=0 & dt2[,1]!=0)
  if(length(de.common) != 0){
    e <- new.env()
    name <- load(here("data","gene_description.RData"), envir = e)
    x <- e[[name]]$genes
    
    get_gene_des <- function(y){
      if(!is.na(y) & y %in% x$Symbol){
        return(x[x$Symbol==y & !is.na(x$Symbol), ][1,4])
      } else{
        return(NA)
      }
    }
    
    temp <- sapply(dge$genes$gene_symbol[de.common], get_gene_des)
    df <- data.frame("DE genes in common" = dge$genes$gene_symbol[de.common],
                     "description" = temp)
    return(df )
  } else{
    return(data.frame("DE genes in common" = "None in common"))
  }
}



edgeR_ind_DE <- function(result, comp, contrast){
  col <- match(comp,colnames(contrast))
  return(topTags(result[[col]], n=Inf))
}

edgeR_md <- function(result, comp, contrast){
  col <- match(comp,colnames(contrast))
  result[[col]]$comparison <- gsub("1\\*","" ,result[[col]]$comparison)
  plotMD(result[[col]])
}



edgeR_heatmap <- function(result, comp, dge, contrast){
  #dev.off()
  par(mar=rep(0.01,4))
  n.replicate <- ncol(dge)/length(unique(dge$samples$group))
  col <- match(comp,colnames(contrast))
  col1 <- RColorBrewer::brewer.pal(7, "Purples") 
  tt <- topTags(result[[col]], n=Inf)
  topgenes <- tt$table$gene_symbol[1:100]
  i <- which(dge$genes$gene_symbol %in% topgenes)
  mycol <- colorpanel(1000,"blue","white","red")
  lcpm <- cpm(dge, log=TRUE)
  heatmap.2(lcpm[i,], scale="row",
            labRow=dge$genes$gene_symbol[i], labCol=dge$samples$group,
            col=mycol, trace="none", density.info="none", dendrogram = "none",
            key.title = "color key", keysize = 1.5, Rowv = NA, Colv = NA, key = TRUE,
            ColSideColors = col1[rep(1:(ncol(dge)/n.replicate),each=n.replicate)])
}

get_gene_symbol_lis <- function(dge, threshold){
  genesymbols <- dge$genes$gene_symbol
  genesymbollist <- list()
  unq <- unique(genesymbols)
  unq <- unq[!is.na(unq)] # getting rid of na
  for(i in unq) {
    sel <- genesymbols == i & !is.na(genesymbols)
    if(sum(sel)>threshold) genesymbollist[[i]] <- which(sel)
  }
  if(length(genesymbollist)==0){
    return(NULL)
  }
  return(genesymbollist)
}


do_fry <- function(dge, comp ,contrast, genesymbollist, design){
  col <- match(comp,colnames(contrast))
  fry.res <- fry(dge, index=genesymbollist, design, contrast=contrast[,col])
  return(fry.res)
}

get_barcode_limma <- function(efit, comp,  gene_set1, genesymbollist){
  col <- match(comp,colnames(efit$t))
  return(barcodeplot(efit$t[,col], index=genesymbollist[[gene_set1]], main=paste(comp, gene_set1,sep = "-")))
}

get_barcode_edgeR <- function(result, comp, gene_set1, genesymbollist, contrast){
  col <- match(comp,colnames(contrast))
  return(barcodeplot(result[[col]]$table$logFC, index=genesymbollist[[gene_set1]] ,main=paste(comp,  gene_set1,sep = "-")))
}

