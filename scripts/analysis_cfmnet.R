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

# obtaining average pairwise sample size for young adult subsample
noNA <- !is.na(networkdata_young)
noNAmat <- t(noNA) %*% noNA
n_pairwise <- mean(noNAmat[lower.tri(noNAmat)])

# fitting confirmatory network model with psychonetrics
# since exploratory model used spearman, should use spearman in confirmatory model for consistency (supply covs and nobs arguments instead of data)
cfmnetwork <- ggm(covs = cor(networkdata_young, use = "pairwise.complete.obs", method = "spearman"),
                  nobs = n_pairwise,
                  omega = adjmatrix)
results_cfmnetwork <- cfmnetwork %>% runmodel()

# obtaining model fit indices
results_cfmnetwork %>% fit %>% 
  filter(Measure == "df" |Measure == "chisq" | Measure == "rmsea" | Measure == "tli" | Measure == "cfi")

#specifying node grouping:
grouping <- list("Information Sources" = c(1:9),
                 "Feelings about Vaccination" = 10)

#creating data frame of labels and short description for legend
nodelabels <- data.frame(labels = c("newspapers", "tv", "radio", "websites", "socmed", "doctor", "healthprof",
                                    "gov", "famfriend", "vacdistrust"),
                         variable_description_short = c("Newspapers", "Television", "Radio", "Internet websites", "Social media", "Doctor", "Other health professionals",
                                                        "Government agencies", "Family or friends", "Distrust in vaccines"))

plotlayout <- as.matrix(read.csv("./data/plotlayout.csv"))

#plotting confirmatory network
plot_young <- qgraph(getmatrix(results_cfmnetwork, "omega", threshold = TRUE, alpha = 0.05),
                  groups = grouping,
                  layout = plotlayout,
                  cut = 0,
                  palette = "pastel",
                  vsize = 8,
                  labels = nodelabels$label,
                  nodeNames = nodelabels$variable_description_short,
                  legend.cex = 1,
                  curve = 0.8,
                  curveAll = TRUE,
                  filename = "cfmnetwork", filetype = "jpeg", width = 20, height = 20,
                  theme = "colorblind")
