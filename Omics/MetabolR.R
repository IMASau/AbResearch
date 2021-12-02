
# to install shiny run:
# install.packages("shiny")
# load the Shiny package
library("shiny")

# start the App from the previously downloaded folder, e.g.
runApp("C:/cloudstor/Manuscripts/Paua/PauaQuenching/Metabolite-Investigator-master/Metabolite-Investigator-master")


## Seems to need separate ID's for Cohorts and Batch IDs
## 
## 
## 



metanr_packages <- function(){
 metr_pkgs <- c("impute", "pcaMethods", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "SSPA", "sva", "limma", "KEGGgraph", "siggenes","BiocParallel", "MSnbase", "multtest","RBGL","edgeR","fgsea","devtools","crmn")
 list_installed <- installed.packages()
 new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
 if(length(new_pkgs)!=0){if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  BiocManager::install(new_pkgs)
  print(c(new_pkgs, " packages added..."))
 }
 
 if((length(new_pkgs)<1)){
  print("No new packages added...")
 }
}

metanr_packages()

devtools::install_github("xia-lab/OptiLCMS", build = TRUE, build_vignettes = FALSE, build_manual =TRUE)

# Step 2: Install MetaboAnalystR with documentation
devtools::install_github("xia-lab/MetaboAnalystR", build = TRUE, build_vignettes = TRUE, build_manual =T)

# Load the MetaboAnalystR package
library("MetaboAnalystR")

##----------------------------------------------------------------------------##
library(mixOmics) 


data <- read.csv("your_data.csv", row.names = 1, header = TRUE)
