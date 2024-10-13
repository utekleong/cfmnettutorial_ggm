#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(psychonetrics)
library(qgraph)

#loading relevant data from full Wave 1 dataset:
data <- read.csv("./data/data_full.csv") %>% 
  select(W1_Conspiracy_1, W1_Conspiracy_2, W1_Conspiracy_3, W1_Conspiracy_4, W1_Conspiracy_5, W1_LOC_Chance_Total, W1_LOC_PO_Total)

#################################################################
##                  Creating adjacency matrix                  ##
#################################################################
#creating "powerlessness" adjacency matrix:
conf_structure_po <- matrix(0, nrow = 7, ncol =7)
labels <- c("cons1", "cons2", "cons3", "cons4", "cons5", "loc_chance", "loc_po")
rownames(conf_structure_po) <- labels
colnames(conf_structure_po) <- labels
conf_structure_po[1:6,7] <- 1
conf_structure_po[7,1:6] <- 1 
conf_structure_po[1:5,1:5] <- 1 #general conspiracist mentality
diag(conf_structure_po) <- 1 #ensuring diagonals are "1"s

##################################################################
##                  Fitting confirmatory model                  ##
##################################################################
#fitting confirmatory models for "powerlessness":
colnames(data) <- labels #relabeling network dataset for consistency
cfmnetwork_po<- psychonetrics::ggm(data,
                                        estimator = "FIML",
                                        omega = conf_structure_po)

results_cfmnetwork_po <- cfmnetwork_po %>% 
  runmodel()

#obtaining model fit
results_cfmnetwork_po %>% fit()

#plotting the network
qgraph((getmatrix(results_cfmnetwork_po, matrix = "omega", threshold = TRUE, alpha = 0.05)),
       labels = labels,
       layout = "spring",
       theme = "colorblind")