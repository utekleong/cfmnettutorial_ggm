#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(psychonetrics)
library(psych)

#loading data:
data_full <- read.csv("./data/data_full.csv")

#subsetting data to Wave 2:
data_W2 <- data_full %>% 
  filter(W2_Present == "1") %>% 
  select(pid, starts_with("W2"))

#further subsetting data to variables of interest:
networkdata <- data_W2 %>% 
  select(W2_Age_year, W2_INFO_1:W2_INFO_9, W2_Vaccines3) %>% 
  mutate(W2_Vaccines3_r = 6 - W2_Vaccines3, .keep = "unused") #reverse scoring W2_Vaccines3

#giving meaningful names:
names(networkdata) <- c("age", "newspapers", "tv", "radio", "websites", "socmed", "doctor", "healthprof",
                        "gov", "famfriend", "vacdistrust")
# write.csv(networkdata, file = "./data/data_clean_W2.csv", row.names = FALSE)

##################################################################
##               Estimating "exploratory" network               ##
##################################################################
#subsetting data to middle to late adulthood:
networkdata_old <- networkdata %>% 
  filter(age >= 35) %>% 
  select(-age)

#estimating network:
exploratoryModel <- psychonetrics::ggm(networkdata_old,
                              estimator = "FIML")

#running the model:
exploratoryModel <- exploratoryModel %>% 
  runmodel()

#specifying node grouping:
grouping <- list("Information Sources" = c(1:9),
                 "Feelings about Vaccination" = 10)

#creating data frame of labels and short description for legend
nodelabels <- data.frame(labels = c("newspapers", "tv", "radio", "websites", "socmed", "doctor", "healthprof",
                                    "gov", "famfriend", "vacdistrust"),
                         variable_description_short = c("Newspapers", "Television", "Radio", "Internet websites", "Social media", "Doctor", "Other health professionals",
                                                        "Government agencies", "Family or friends", "Distrust in vaccines"))

#plotting network:
plot <- qgraph((getmatrix(exploratoryModel, matrix = "omega", threshold = TRUE, alpha = 0.05)),
       labels = nodelabels$labels,
       groups = grouping,
       layout = "spring",
       legend = FALSE,
       theme = "colorblind",
       filename = "expnetwork", filetype = "png", width = 20, height = 20)

# extracting adjacency matrix from the exploratory network to be used in confirmatory network analysis:
adjmatrix <- 1*((getmatrix(exploratoryModel, matrix = "omega", threshold = TRUE, alpha = .05) !=0))
# write.csv(adjmatrix, file = "./data/adjmatrix.csv", row.names = TRUE)

# extracting plot layout from the exploratory network to be used in confirmatory network analysis:
plotlayout <- plot$layout
# write.csv(plotlayout, file = "./data/plotlayout.csv", row.names = FALSE)