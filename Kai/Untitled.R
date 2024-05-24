# Loading Library
library(readr)
library(igraph)
library(rsample)

## 1. Load and Prepare the Data
# Reading Data
data <- read.table("amazon0601.txt")

# Re-name
colnames(data) <- c("From", "To")

# Splitting data
data_split <- initial_split(data, prop = 0.003)
data_split_training <- training(data_split)

# Convertting into igraph
data_ig_original <- graph_from_data_frame(data, directed = FALSE)
data_ig_splitted <- graph_from_data_frame(data_split_training, directed = FALSE)

# Basic Elements
# Represents individuals in the data set
vertices <- V(data_ig_splitted)

# Relationship between the individuals
edges <- E(data_ig_splitted)

# Strength of the interaction; here, the return is Null - no weights
weights <- E(data_ig_splitted)$weight 

# Number of edges connected to each vertex
degree <- degree(data_ig_splitted)

# The data is undirected, hence in_degree = out_degree
in_degree <- degree(data_ig_splitted, mode = "in")
out_degree <- degree(data_ig_splitted, mode = "out")

# Degree to which modes tend to cluster together
clustering_coef <- transitivity(data_ig_splitted, type = "local") # Many NaN

# The lengths of the shortest paths between each pair of vertext
path_lengths <- distances(data_ig_splitted)

# Longest shortest path in the network
diameter <- diameter(data_ig_splitted)

# Subsets of the network where any two vertices are connected by a path
components <- components(data_ig_splitted)

# 2. Generate Induced Subgraphs
set.seed(194)
sub.network1 <- induced.subgraph(data_ig_original, sample(V(data_ig_original), 200))


# 4. Analyze Network Metrics
hist(degree(sub.network1, mode = "in"),
     breaks=1:vcount(sub.network1)-1, 
     main="Indegree histogram")

hist(degree(sub.network1, mode = "out"),
     breaks=1:vcount(sub.network1)-1, 
     main="Outdegree histogram")

plot(sub.network1, vertex.size=10,
     vertex.label.cex = 0.4,
     edge.arrow.size = 0.1)

  
# When plotting, there are too many nodes, we should consider filttering
# 1. Setting threshold
# 2. Setting layout
