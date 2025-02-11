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
nodelabels <- data.frame(labels = c("CT1_PublicNotInformed", "CT2_PoliticiansMotives", "CT3_GovMonitoring", "CT4_SecretActivities", "CT5_SecretOrgs",
                                    "LOC_Chance", "LOC_PowfOthers"),
                         grouping = c("CT", "CT", "CT", "CT", "CT", "LOC", "LOC"))
rownames(conf_structure_po) <- nodelabels$labels
colnames(conf_structure_po) <- nodelabels$labels

conf_structure_po[1:5,7] <- 1 #Powerful others subscale is associated with the five items of the Conspiracy Mentality Scale. Adds these associations to the upper triangle of the matrix. 
conf_structure_po[7,1:5] <- 1 #Adds these associations to the lower triangle of the matrix, to ensure symmetry.

conf_structure_po[6,7] <- 1 #Powerful others subscale is associated with the Chance subscale of the Locus of Control scale. Adds this association to the upper triangle of the matrix. 
conf_structure_po[7,6] <- 1 #Adds this association to the lower triangle of the matrix, to ensure symmetry.
conf_structure_po[1:5,1:5] <- 1 #The five items of the Conspiracy Mentality Questionnaire are each associated with each other. Each item should also associate with itself.

conf_structure_po[6,6] <- 1 #Chance subscale of the Locus of Control scale is associated with itself.
conf_structure_po[7,7] <- 1 #Powerful others subscale of the Locus of Control scale is associated with itself.

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
       palette = "pastel",
       filename = "cfmnetwork_theory", filetype = "png", width = 20, height = 20)
