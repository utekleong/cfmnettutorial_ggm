#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(psychonetrics)
library(qgraph)
library(psych)

#loading data:
data_full <- read.csv("./data/data_full.csv")

#subsetting data to Wave 2:
data_W1 <- data_full %>% 
  filter(W1_Present == "1") %>% 
  select(pid, starts_with("W1"))

#further subsetting data to variables of interest:
networkdata <- data_W1 %>% 
  select(W1_Gender, W1_GAD_1:W1_GAD_7)

#giving meaningful names:
names(networkdata) <- c("gender", "GAD1_Anxious", "GAD2_WorryControl", "GAD3_Worry", "GAD4_Relax", "GAD5_Restless", "GAD6_Annoyed", "GAD7_Fear")
# write.csv(networkdata, file = "./data/data_clean_W1.csv", row.names = FALSE)

##################################################################
##               Estimating "exploratory" network               ##
##################################################################
#1 = Male
#2 = Female
#3 = Transgender
#4 = Prefer not to say
#5 = Other

#subsetting data to males:
networkdata_male <- networkdata %>% 
  filter(gender == 1) %>% 
  select(-gender)

#estimating network:
exploratoryModel <- psychonetrics::ggm(networkdata_male,
                              estimator = "FIML")

#running the model:
exploratoryModel <- exploratoryModel %>% 
  runmodel()

#creating data frame of labels:
nodelabels <- data.frame(labels = c("Anxious", "WorryControl", "Worry", "Relax", "Restless", "Annoyed", "Fear"))

#plotting network:
plot <- qgraph((getmatrix(exploratoryModel, matrix = "omega", threshold = TRUE, alpha = 0.05)),
       labels = nodelabels$labels,
       layout = "spring",
       legend = FALSE,
       theme = "colorblind",
       color = "pink",
       filename = "gender_expnetwork", filetype = "png", width = 20, height = 20)

# extracting adjacency matrix from the exploratory network to be used in confirmatory network analysis:
adjmatrix <- 1*((getmatrix(exploratoryModel, matrix = "omega", threshold = TRUE, alpha = .05) !=0))
# write.csv(adjmatrix, file = "./data/adjmatrix.csv", row.names = TRUE)

# extracting plot layout from the exploratory network to be used in confirmatory network analysis:
plotlayout <- plot$layout
# write.csv(plotlayout, file = "./data/plotlayout.csv", row.names = FALSE)