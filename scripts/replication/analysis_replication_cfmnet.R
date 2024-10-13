#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(psychonetrics)
library(qgraph)

#loading cleaned Wave 2 data:
networkdata <- read.csv("./data/data_clean_W2.csv")

#subsetting data to young adulthood:
networkdata_young <- networkdata %>% 
  filter(age <= 34) %>% 
  select(-age)

#################################################################
##              Estimating "confirmatory" network              ##
#################################################################
# pulling adjacency matrix from exploratory network:
adjmatrix <- as.matrix(read.csv("./data/adjmatrix.csv", row.names = 1))

# fitting confirmatory network model:
confirmatoryNetwork <- psychonetrics::ggm(networkdata_young, omega = adjmatrix)

# running the model:
confirmatoryNetwork <- confirmatoryNetwork %>% 
  runmodel()

# obtaining model fit:
confirmatoryNetwork %>% fit

#specifying node grouping:
grouping <- list("Information Sources" = c(1:9),
                 "Feelings about Vaccination" = 10)

#creating data frame of labels and short description for legend
nodelabels <- data.frame(labels = c("newspapers", "tv", "radio", "websites", "socmed", "doctor", "healthprof",
                                    "gov", "famfriend", "vacdistrust"),
                         variable_description_short = c("Newspapers", "Television", "Radio", "Internet websites", "Social media", "Doctor", "Other health professionals",
                                                        "Government agencies", "Family or friends", "Distrust in vaccines"))

#loading plot layout of exploratory model
plotlayout <- as.matrix(read.csv("./data/plotlayout.csv"))

#plotting confirmatory network
plot_young <- qgraph(getmatrix(confirmatoryNetwork, "omega", threshold = TRUE, alpha = 0.05),
                  groups = grouping,
                  layout = plotlayout,
                  labels = nodelabels$label,
                  legend = FALSE,
                  theme = "colorblind",
                  filename = "cfmnetwork_replication", filetype = "png", width = 20, height = 20)
