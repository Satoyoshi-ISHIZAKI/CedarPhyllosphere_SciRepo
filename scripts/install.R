#install packages needed (not via "install.packages")

install.packages("lmtest")
install.packages("ggnewscale")
install.packages("gtsummary")
install.packages("webshot2")
install.packages("ggeffects")
install.packages("ggtern")
install.packages("chromote")

##Bioconductor
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.19")

##phyloseq
if(!requireNamespace("BiocManager")){
  install.packages("BiocManager")
  }
BiocManager::install("phyloseq")

##qiime2R
if (!requireNamespace("devtools", quietly = TRUE)){install.packages("devtools")}
devtools::install_github("jbisanz/qiime2R")

##fantaxtic
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_github("gmteunisse/fantaxtic")

##ggnested
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_github("gmteunisse/ggnested")

##biomformat
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("biomformat")

#XLconnect
install.packages("XLConnect")

#readGenAlEx
install.packages("devtools")
devtools::install_github("douglasgscofield/readGenalex")

#SummarizedExperiment
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("SummarizedExperiment")

#ANCOMBC
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ANCOMBC")

#microbiome
##needed for ANCOM-BC
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("microbiome")