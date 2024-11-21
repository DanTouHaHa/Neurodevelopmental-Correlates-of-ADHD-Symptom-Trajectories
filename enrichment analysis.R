
#rm(list=ls())
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.17")
# BiocManager::install("clusterProfiler")  #for enrichment
# BiocManager::install("topGO")  #for plot
# BiocManager::install("Rgraphviz")
# BiocManager::install("pathview") #for KEGG pathway
# BiocManager::install("org.Hs.eg.db") #for gene annotation.

library(BiocManager)
library(clusterProfiler)
library(topGO)
library(Rgraphviz)
library(pathview)
library(org.Hs.eg.db)
library(ggplot2)
library(dplyr)
library(readxl)
pacman::p_load("BiocManager","clusterProfiler","topGO","Rgraphviz","org.Hs.eg.db","ggplot2","stringr","Cairo","cowplot",
               "msigdbr","rrvgo","simplifyEnrichment","GOSemSim","ReactomePA","DOSE","readxl", 'aPEAR',
               install=F , update=F, character.only=F)
pacman::p_load_gh("disgenet2r")

dir.root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.work <- paste0(dir.root,'/Users/hwj/Documents/E/Fudan_Luoqiang_RestMDD_metaProject/848MDD_794NC/数据分析/数据整理统计分析/富集分析基因表达/R语言富集分析/富集分析/ADHD_Trajectory')
###### Delta_FU2_Inattention_Old_LateOnset_Positive.xlsx
out_probe_join11 <- read_excel('20240628Hipp_Thick_Inattention_MeanThickforAllenBrain_PLS_variance_max.xlsx')

###### Delta_FU2_Inattention_Old_Persistent_Positive.xlsx
# out_probe_join11 <- read_excel(file.path(paste0('Delta_FU2_Inattention_Old_Persistent_Positive.xlsx')))

###### Delta_FU2_Inattention_Old_Remitting_Positive.xlsx
#out_probe_join11 <- read_excel(file.path(paste0('Delta_FU2_Inattention_Old_Remitting_Positive.xlsx')))

###### Delta_Delta_Inattention_New_LateOnset_Positive.xlsx
# out_probe_join11 <- read_excel(file.path(paste0('Delta_Delta_Inattention_New_LateOnset_Positive.xlsx')))

###### Delta_Delta_Inattention_New_Remitting_Positive.xlsx
#out_probe_join11 <- read_excel(file.path(paste0('Delta_Delta_Inattention_New_Remitting_Positive.xlsx')))

egenes.symble <- as.character(out_probe_join11$geneSymbol[which(out_probe_join11$PLS3Z > 1.96)]) #set a threshhood of gene-Z score
file_name <- paste0(dir.work,'ADHD_estimate')

egenes.entrez <- mapIds(x = org.Hs.eg.db, keys = egenes.symble, keytype = "SYMBOL", column="ENTREZID")
egenes.entrez <- na.omit(egenes.entrez)

#### kegg ----
enrich.kegg <- enrichKEGG(
  gene = egenes.entrez,
  organism = "hsa",
  keyType = "kegg", # one of "kegg", 'ncbi-geneid', 'ncib-proteinid' and 'uniprot'
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2,
  pAdjustMethod = "BH",
  # universe,
  minGSSize = 10,
  maxGSSize = 500,
  use_internal_data = FALSE)
enrich.kegg.y <- setReadable(enrich.kegg, OrgDb = org.Hs.eg.db, keyType="ENTREZID")

write.table(enrich.kegg.y@result, file = file.path(paste0('20240628Hipp_Thick_Inattention_MeanThickforAllenBrain_Positive_KEGGResult.csv')),row.names = FALSE, quote = FALSE,sep = ',')
# barplot(enrich.kegg, showCategory = 20, title = 'KEGG Enrichment')
#pic_dot <- dotplot(enrich.kegg, showCategory=20, title = 'KEGG Enrichment')
#pic_dot
#pdf(file = file.path(paste0('ADHD_kegg_Z_percent1_1%.pdf')),width=7,height=7,fonts = NULL,family="Times")
#pic_dot
#dev.off()

###### GO ----
go <- enrichGO(gene = egenes.entrez,
               OrgDb = org.Hs.eg.db,
               keyType = "ENTREZID",
               ont = "ALL",
               pvalueCutoff = 0.05,
               pAdjustMethod = 'BH',
               qvalueCutoff = 0.2,
               readable = TRUE)
write.table(go@result, file = file.path(paste0('20240628Hipp_Thick_Inattention_MeanThickforAllenBrain_Positive_GOResult.csv')),
            row.names = FALSE, quote = FALSE,sep = ',')

#pic_dot <- dotplot(go, split="ONTOLOGY")+ facet_grid(ONTOLOGY~.,scale="free")
#pic_dot
#pdf(file = file.path(paste0('ADHD_GO_Z_percent1_1%.pdf')),width=10,height=7,family="Times")
#pic_dot
#dev.off()
