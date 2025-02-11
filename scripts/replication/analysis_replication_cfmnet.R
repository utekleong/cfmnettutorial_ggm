#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(psychonetrics)
library(qgraph)

#loading cleaned Wave 2 data:
networkdata <- read.csv("./data/data_clean_W1.csv")

#subsetting data to females

#1 = Male
#2 = Female
#3 = Transgender
#4 = Prefer not to say
#5 = Other

networkdata_female <- networkdata %>% 
  filter(gender == 2) %>% 
  select(-gender)

#################################################################
##              Estimating "confirmatory" network              ##
#################################################################
# pulling adjacency matrix from exploratory network:
adjmatrix <- as.matrix(read.csv("./data/adjmatrix.csv", row.names = 1))

# fitting confirmatory network model:
confirmatoryNetwork <- psychonetrics::ggm(networkdata_female, omega = adjmatrix)

# running the model:
confirmatoryNetwork <- confirmatoryNetwork %>% 
  runmodel()

# obtaining model fit:
confirmatoryNetwork %>% fit

#creating data frame of labels:
nodelabels <- data.frame(labels = c("Anxious", "WorryControl", "Worry", "Relax", "Restless", "Annoyed", "Fear"))

#loading plot layout of exploratory model:
plotlayout <- as.matrix(read.csv("./data/plotlayout.csv"))

#plotting confirmatory network:
plot<- qgraph(getmatrix(confirmatoryNetwork, "omega", threshold = TRUE, alpha = 0.05),
                  layout = plotlayout,
                  labels = nodelabels$label,
                  legend = FALSE,
                  theme = "colorblind",
                  color = "pink",
                  filename = "gender_cfmnetwork", filetype = "png", width = 20, height = 20)
