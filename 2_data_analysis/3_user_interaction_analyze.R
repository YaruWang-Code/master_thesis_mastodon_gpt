# prepare and load data ####
rm(list = ls())

library(arrow)
library(dplyr)
library(igraph)
library(viridis)
library(RColorBrewer)
library(ggsci)


# Specify the path to your Feather file
feather_file_path <- "data\\9_mastodon_user_interaction_data.feather"

# Read data from the Feather file
user_interaction <- arrow::read_feather(feather_file_path)

# build network ####

edges <- user_interaction %>%
  select(source, target, weight)


edges <- aggregate(weight ~ source + target, data = edges, FUN = sum, na.rm = TRUE)


graph <- graph.data.frame(edges, directed = TRUE)

## check basic information of network ####

# check nodes number

# check nodes number
V(graph)
E(graph)
edge_density(graph)

par(mfrow=c(1,1))
# check the edge weight 
table(E(graph)$weight)
mean(E(graph)$weight)

sum(E(graph)$weight<mean(E(graph)$weight))

# Check average degree
mean(degree(graph))


# central node ####

## pagerank centrality ####
# Apply the PageRank centrality algorithm
pagerank_values <- page.rank(graph, directed = TRUE, weights = E(graph)$weight)

# Access PageRank scores for each node
pagerank_scores <- pagerank_values$vector


## top central nodes ####
# Get the indices of the top 10 nodes based on PageRank scores
top_nodes <- head(order(-pagerank_scores), 20)

# Print the top 10 nodes and their Pagerank scores
top_nodes_data <- data.frame(
  NodeID = V(graph)$name[top_nodes],
  PagerankScore = pagerank_scores[top_nodes]
)


in_degree <- degree(graph, mode = "in")
out_degree <- degree(graph, mode = "out")

table(in_degree)
table(out_degree)
mean(in_degree)
mean(out_degree)


par(mar= c(4.55, 10, 1, 2) + 0.1)
par(mfrow=c(1,3))

h <- hist(E(graph)$weight, freq = TRUE,
          xlab="Weight of Edges",
          ylab = "Frequency of Edges",main="",cex.lab=2, cex.axis=2,breaks = 23,xlim = c(0,50))

hist(in_degree, freq = TRUE,
     xlab="N of Indegree",
     ylab = "Frequency of Nodes",main="",cex.lab=2, cex.axis=2,breaks = 23)

hist(out_degree, freq = TRUE,
     xlab="N of Outdegree",
     ylab = "Frequency of Nodes",main="",cex.lab=2, cex.axis=2,breaks = 23)



# Create a dataframe with node IDs and their in-degree and out-degree
degree_df <- data.frame(NodeID = V(graph)$name, InDegree = in_degree, OutDegree = out_degree)

top_nodes_data <- merge(top_nodes_data, degree_df, by = "NodeID", all.x = TRUE)

top_nodes_data <- top_nodes_data[order(-top_nodes_data$PagerankScore), ]


write_feather(top_nodes_data, "data\\10_top_nodes_data.feather")

write.csv(top_nodes_data, file = "data\\10_top_nodes_data_23.csv", row.names = FALSE)



### plot color, size design ####

# Set vertex color attribute in the graph
# V(graph)$color <- vertex_colors

vertex.size.pagerank <- (10 * pagerank_scores /max(pagerank_scores) + 1.5)
edge.weight <- edge_attr(graph, "weight")
edge.weight.normal <- edge.weight / max(edge.weight)
edge.weight.color.percent <- 0.67 * (1 - edge.weight.normal * 0.7)
edge.weight.color <- rgb(edge.weight.color.percent,edge.weight.color.percent,edge.weight.color.percent)



# Assign colors to nodes based on pagerank score

node_colors <- heat.colors(length(unique(vertex.size.pagerank)))[as.factor(vertex.size.pagerank)]
palette <- heat.colors(length(unique(vertex.size.pagerank)))

# Reverse the order of colors
palette <- rev(palette)

# Map node degrees to colors using the reversed palette
node_colors <- palette[as.factor(vertex.size.pagerank)]

# Visualize the graph with color representing PageRank scores
plot(
  graph,
  layout =  layout_nicely,
  vertex.color = node_colors,
  vertex.size = vertex.size.pagerank,
  vertex.label = NA,
  edge.width = edge.weight.color.percent,
  edge.color = edge.weight.color,
  edge.arrow.size=0.2,
  edge.arrow.width=0.3
)


# find main / core network ####

main_ntw_accts <- V(graph)$name[top_nodes]

ntw_complete <- FALSE
while (!ntw_complete) {
  
  connected_interact <- user_interaction[(user_interaction$source %in% main_ntw_accts) | (user_interaction$target %in% main_ntw_accts), ]
  updated_main_ntw_accts <- union(connected_interact$source, connected_interact$target)
  
  if (length(updated_main_ntw_accts)==length(main_ntw_accts)) {
    ntw_complete <- TRUE
  }
  main_ntw_accts <- updated_main_ntw_accts
}


## build main network graph ####

central_edges <- connected_interact %>%
  select(source,target,weight)

central_graph <- graph.data.frame(central_edges, directed = TRUE)

## plot main network graph ####

## pagerank centrality ####
# Apply the PageRank centrality algorithm
pagerank_values <- page.rank(central_graph, directed = TRUE, weights = E(central_graph)$weight)

# Access PageRank scores for each node
pagerank_scores <- pagerank_values$vector


### plot color, size design ####

vertex.size.pagerank <- (10 * pagerank_scores /max(pagerank_scores) + 1.5)
edge.weight <- edge_attr(graph, "weight")
edge.weight.normal <- edge.weight / max(edge.weight)
edge.weight.color.percent <- 0.67 * (1 - edge.weight.normal * 0.7)
edge.weight.color <- rgb(edge.weight.color.percent,edge.weight.color.percent,edge.weight.color.percent)

# Reverse the order of colors
palette <- rev(palette)

# Map node degrees to colors using the reversed palette
node_colors <- palette[as.factor(vertex.size.pagerank)]

# Visualize the graph with color representing PageRank scores
plot(
  central_graph,
  layout =  layout_nicely,
  vertex.color = node_colors,
  vertex.size = vertex.size.pagerank,
  vertex.label = NA,
  edge.width = edge.weight.color.percent,
  edge.color = edge.weight.color,
  edge.arrow.size=0.2,
  edge.arrow.width=0.3
)


# community detection ####

clusters <- cluster_walktrap(central_graph,steps = 20000)
membership <- membership(clusters)

length(unique(membership))

plot(central_graph, 
     layout =  layout_nicely,
     vertex.color =  clusters$membership + 2, 
     vertex.size = vertex.size.pagerank, 
     vertex.label = NA,
     edge.color = edge.weight.color,
     edge.arrow.size=0.2,
     edge.arrow.width=0.3)


plot(central_graph, 
     layout =  layout_nicely,
     vertex.color =  clusters$membership + 2, 
     vertex.size = vertex.size.pagerank, 
     vertex.label = NA,
     edge.color = edge.weight.color,
     edge.arrow.size=0.2,
     edge.arrow.width=0.3,
     mark.groups = list(which(clusters$membership == 1), 
                        which(clusters$membership == 2), 
                        which(clusters$membership == 3), 
                        which(clusters$membership == 4), 
                        which(clusters$membership == 5), 
                        which(clusters$membership == 6),
                        which(clusters$membership == 7), 
                        which(clusters$membership == 8), 
                        which(clusters$membership == 9), 
                        which(clusters$membership == 10), 
                        which(clusters$membership == 11),
                        which(clusters$membership == 12),
                        which(clusters$membership == 13), 
                        which(clusters$membership == 14), 
                        which(clusters$membership == 15), 
                        which(clusters$membership == 16), 
                        which(clusters$membership == 17), 
                        which(clusters$membership == 18),
                        which(clusters$membership == 19), 
                        which(clusters$membership == 20), 
                        which(clusters$membership == 21), 
                        which(clusters$membership == 22), 
                        which(clusters$membership == 23),
                        which(clusters$membership == 24),
                        which(clusters$membership == 25), 
                        which(clusters$membership == 26), 
                        which(clusters$membership == 27), 
                        which(clusters$membership == 28), 
                        which(clusters$membership == 29), 
                        which(clusters$membership == 30),
                        which(clusters$membership == 31), 
                        which(clusters$membership == 32), 
                        which(clusters$membership == 33), 
                        which(clusters$membership == 34), 
                        which(clusters$membership == 35),
                        which(clusters$membership == 36),
                        which(clusters$membership == 37), 
                        which(clusters$membership == 38), 
                        which(clusters$membership == 39), 
                        which(clusters$membership == 40), 
                        which(clusters$membership == 41), 
                        which(clusters$membership == 42),
                        which(clusters$membership == 43), 
                        which(clusters$membership == 44), 
                        which(clusters$membership == 45), 
                        which(clusters$membership == 46), 
                        which(clusters$membership == 47),
                        which(clusters$membership == 48),
                        which(clusters$membership == 49), 
                        which(clusters$membership == 50), 
                        which(clusters$membership == 51), 
                        which(clusters$membership == 52), 
                        which(clusters$membership == 53), 
                        which(clusters$membership == 54),
                        which(clusters$membership == 55), 
                        which(clusters$membership == 56), 
                        which(clusters$membership == 57), 
                        which(clusters$membership == 58), 
                        which(clusters$membership == 59),
                        which(clusters$membership == 60),
                        which(clusters$membership == 61), 
                        which(clusters$membership == 62), 
                        which(clusters$membership == 63), 
                        which(clusters$membership == 64), 
                        which(clusters$membership == 65), 
                        which(clusters$membership == 66)
     )
)



# merge membership ####
V(central_graph)$membership <- membership

df <- get.data.frame(central_graph, "vertices")


## label proporation ####
clusters_label <- cluster_label_prop(central_graph)
membership_label <- membership(clusters_label)
length(unique(membership_label))


# Get the degrees of all nodes in the graph
degrees <- degree(graph_community)
sort(degrees, decreasing = TRUE)
mean(degrees)

# Identify nodes with degrees less than the threshold
nodes_to_remove <- which(degrees < 10)

# Remove nodes with degrees less than the threshold
graph <- delete_vertices(graph, nodes_to_remove)


simplified_graph <- simplify(graph_community, remove.loops = TRUE, remove.multiple = TRUE)

# Run the Greedy Modularity Algorithm on the simplified graph
clusters <- cluster_fast_greedy(simplified_graph)

# Get the membership vector (each node belongs to a specific community)
membership <- membership(clusters)

# Print the community assignments
print(membership)

plot(simplified_graph, vertex.color = membership, vertex.size = 10, main = "Community Detection")
