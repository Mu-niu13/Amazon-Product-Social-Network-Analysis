# Loadaing Library
library(readr)
library(igraph)
library(rsample)

# Reading Data
data <- read.table("amazon0601.txt")

# Re-name
colnames(data) <- c("From", "To")

# Splitting data
data_split <- initial_split(data, prop = 0.05)
data_split_training <- training(data_split)

# Convertting into igraph
data_ig <- graph_from_data_frame(data_split_training, directed = FALSE)

# Basic Elements
vertices <- V(data_ig)
edges <- E(data_ig)
weights <- E(data_ig)$weight
degree <- degree(data_ig)
in_degree <- degree(data_ig, mode = "in")
out_degree <- degree(data_ig, mode = "out")
clustering_coef <- transitivity(data_ig, type = "local")
path_lengths <- distances(data_ig)
diameter <- diameter(data_ig)
components <- components(data_ig)

# When plotting, there are too many nodes, we should consider filttering
# 1. Setting threshold
# 2. Setting layout
