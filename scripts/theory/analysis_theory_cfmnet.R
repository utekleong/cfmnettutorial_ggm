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
#creating adjacency matrix:
conf_structure_po <- matrix(0, nrow = 7, ncol =7)
nodelabels <- data.frame(labels = c("cons1", "cons2", "cons3", "cons4", "cons5", "loc_chance", "loc_po"),
                         grouping = c("CT", "CT", "CT", "CT", "CT", "LOC", "LOC"))
rownames(conf_structure_po) <- nodelabels$labels
colnames(conf_structure_po) <- nodelabels$labels

conf_structure_po[1:5,7] <- 1 #powerful others subscale associated with the five items of the Conspiracy Mentality Scale
conf_structure_po[7,1:5] <- 1 #to ensure symmetry

conf_structure_po[6,7] <- 1 #powerful others subscale associated with chance subscale of the Locus of Control scale
conf_structure_po[7,6] <- 1 #to ensure symmetry
conf_structure_po[1:5,1:5] <- 1 #five items of the Consipiracy Mentality Scale are form the same measure, so they should be associated with one another

diag(conf_structure_po) <- 1 #ensuring diagonals are "1"s

##################################################################
##                  Fitting confirmatory model                  ##
##################################################################
#fitting confirmatory model:
colnames(data) <- nodelabels$labels #relabeling network dataset for consistency
confirmatoryModel<- psychonetrics::ggm(data,
                                        estimator = "FIML",
                                        omega = conf_structure_po)

#running the model:
confirmatoryModel <- confirmatoryModel %>% 
  runmodel()

#obtaining model fit:
confirmatoryModel %>% fit()

#plotting the network:
qgraph((getmatrix(confirmatoryModel, matrix = "omega", threshold = TRUE, alpha = 0.05)),
       labels = nodelabels$labels,
       groups = nodelabels$grouping,
       layout = "spring",
       legend = FALSE,
       theme = "colorblind",
       filename = "cfmnetwork_theory", filetype = "png", width = 20, height = 20)
