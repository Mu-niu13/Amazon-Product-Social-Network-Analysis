# Loading Library
library(readr)
library(igraph)
library(rsample)

## 1. Load and Prepare the Data
# Reading Data
setwd("/Users/kai/Documents/GitHub/Amazon-Product-Social-Network-Analysis/Data")
data <- read.table("amazon0601.txt")
code_book <- read.csv("meta_data.csv")
code_book <- as.data.frame(code_book)
data[data$"From" == 4,]

# Re-name
colnames(data) <- c("From", "To")

# Sampling
data <- subset(data, From > 0)
data <- subset(data, To > 0)

# 1 to 2, 3, 4, 155, 185, 233, 234, 235, 3943
# 2 to 1
# 3 to 1
df <- data[data$'From' %in% c(1, 2, 3, 7) | data$'To' %in% c(1, 2, 3, 7) ,]





#df <- data[data$'From' %in% c(1, 2, 3, 4) | data$'To' %in% c(2, 3, 4, 155, 185, 233, 234, 235, 3943), ]


data_ig <- graph_from_data_frame(df)

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

# Betweenness
betweenness(data_ig_splitted)





# 4. Analyze Network Metrics
# Adj
edglist <- matrix(unlist(df), ncol = 2)
edglist.igraph <- graph.edgelist(edglist, directed = TRUE)
edglist.adjacency <- as_adj(edglist.igraph)
edglist.adjacency
df

hist(degree(sub.network1, mode = "in"),
     breaks=1:vcount(sub.network1)-1, 
     main="Indegree histogram")

hist(degree(sub.network1, mode = "out"),
     breaks=1:vcount(sub.network1)-1, 
     main="Outdegree histogram")

plot(data_ig, vertex.size=10,
     vertex.label.cex = 0.4,
     edge.arrow.size = 0.1)









