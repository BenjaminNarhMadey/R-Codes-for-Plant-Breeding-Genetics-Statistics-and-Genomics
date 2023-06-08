### Gene Expression Analysis ####
#Install packages ####
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")

suppressMessages(library("DESeq2"))
suppressMessages(library("pasilla"))

####Load data####

incountfile <- "C:/Users/8th gen L480/OneDrive/Desktop/Sequencing Informatics/3.0 Sequencing Informatics/Differential Gene Expression Analysis/pasilla_counts.csv"
incounttable <- read.table(incountfile, sep = ",", header = TRUE, row.names = 1)

inannotfile <- "C:/Users/8th gen L480/OneDrive/Desktop/Sequencing Informatics/3.0 Sequencing Informatics/Differential Gene Expression Analysis/pasilla_annotation.csv"
inannottable <- read.table(inannotfile, sep = ",", header = TRUE, row.names = 1, 
                           stringsAsFactors = FALSE)
head(incounttable)
head(inannottable)
rownames(inannottable)
colnames(incounttable)

## Remove the "fb" part of the rownames of the annotation table
rownames(inannottable) <- gsub("fb", "", rownames(inannottable))
## Then reorder the counttable to match the order of the annotation table
incounttable <- incounttable[,rownames(inannottable)]

rownames(inannottable)
colnames(incounttable)
identical(rownames(inannottable), colnames(incounttable))

## Using N number of reads in S number of samples
min_number_reads <- 2
min_number_samples <- round(ncol(incounttable)*0.5)
filtered_counttable <- incounttable[rowSums(incounttable > 2) > min_number_samples,]

# Set the "treatment" group and "control" groups####
treatlabel <- "treated"
controllabel <- "untreated"
# Grab the column with the group information
DESeqannottable <- inannottable[,"condition",drop=FALSE]
DESeqannottable[DESeqannottable[,1] == treatlabel, 1] <- "treat"
DESeqannottable[DESeqannottable[,1] == controllabel, 1] <- "control"
DESeqannottable[,1] <- as.factor(DESeqannottable[,1])
colnames(DESeqannottable) <- "DESeq_label"
DESeqannottable

dds <- DESeqDataSetFromMatrix(countData = filtered_counttable,
                              colData = DESeqannottable,
                              design = ~ DESeq_label)

dds <- DESeq(dds)
DEseqres <- as.data.frame(results(dds))
head(DEseqres)
head(results(dds))


## Load files####

incountfile <- "C:/Users/8th gen L480/OneDrive/Desktop/Sequencing Informatics/3.0 Sequencing Informatics/Differential Gene Expression Analysis/pasilla_counts.csv"
incounttable <- read.table(incountfile, sep = ",", header = TRUE, row.names = 1)

inannotfile <- "C:/Users/8th gen L480/OneDrive/Desktop/Sequencing Informatics/3.0 Sequencing Informatics/Differential Gene Expression Analysis/pasilla_annotation.csv"
inannottable <- read.table(inannotfile, sep = ",", header = TRUE, row.names = 1, 
                           stringsAsFactors = FALSE)

## Remove the "fb" part of the rownames of the annotation table
rownames(inannottable) <- gsub("fb", "", rownames(inannottable))
## Then reorder the counttable to match the order of the annotation table
incounttable <- incounttable[,rownames(inannottable)]

## Using N number of reads in S number of samples
min_number_reads <- 2
min_number_samples <- round(ncol(incounttable)*0.5)
filtered_counttable <- incounttable[rowSums(incounttable > 2) > min_number_samples,]

# Set the "treatment" group and "control" group
treatlabel <- "treated"
controllabel <- "untreated"
# Grab the column with the group information
DESeqannottable <- inannottable[,"condition",drop=FALSE]
DESeqannottable[DESeqannottable[,1] == treatlabel, 1] <- "treat"
DESeqannottable[DESeqannottable[,1] == controllabel, 1] <- "control"
DESeqannottable[,1] <- as.factor(DESeqannottable[,1])
colnames(DESeqannottable) <- "DESeq_label"

###Control ####

## Grab a column we want to control by - "type"
inannottable
controlcols <- inannottable[,"type", drop = FALSE]


## Adding failsafe to factorize any character columns for the sake of DEseq formula
# controlcols[sapply(controlcols, is.character)] <- lapply(controlcols[sapply(controlcols, is.character)], as.factor)
controltab = data.frame(controlcols[match(rownames(DESeqannottable), 
                                          rownames(controlcols)),,drop=FALSE], stringsAsFactors = TRUE)
DESeqannottable = merge(DESeqannottable, controltab, by = "row.names", sort = FALSE)
DESeqannottable = data.frame(DESeqannottable[,2:ncol(DESeqannottable)], 
                             row.names = DESeqannottable[,1])
colnames(DESeqannottable)[2:ncol(DESeqannottable)] =
  paste0(rep("control_var_", ncol(controltab)), seq(ncol(controltab)))
DESeqannottable


## Craft the design string and then the dds object

designstring <- paste0("~",paste(colnames(DESeqannottable)[2:ncol(DESeqannottable)], 
                                 collapse = " + "), " + DESeq_label")
designstring
dds <- DESeqDataSetFromMatrix(countData = filtered_counttable,
                              colData = DESeqannottable,
                              design= as.formula(designstring))
# Note that I am suppressing Messages and Warnings just to clean up the output
dds <- suppressMessages(DESeq(dds))
DEseqres <- as.data.frame(results(dds))
head(DEseqres)


 
outDEseqfile <- "C:/Users/8th gen L480/OneDrive/Desktop/Sequencing Informatics/3.0 Sequencing Informatics/Differential Gene Expression Analysis/deseq_out.csv"
write.table(DEseqres,outDEseqfile , sep = ",", row.names = TRUE, col.names = NA)


install.packages("ComplexHeatmap")