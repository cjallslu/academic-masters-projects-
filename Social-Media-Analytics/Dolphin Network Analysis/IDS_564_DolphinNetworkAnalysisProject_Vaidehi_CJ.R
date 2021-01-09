#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:/Study Material Sem-2/Social Media and Network Analysis/Project/CSV_Dolphins"
setwd(dir_path)
# clear everything out of memory
rm(list=ls()) 

library(igraph)
#library(dplyr)
#library(tidyverse)      
#library(intergraph) 
#library(network)    
#library(sna)        
#library(viridis)    
#library(scales) 

detach(package:sna)
detach(package:dplyr)
detach(package:intergraph)
detach(package:network)
detach(package:viridis)
detach(package:scales)

dolphin_gml = read.graph(file="http://users.dimi.uniud.it/~massimo.franceschet/teaching/datascience/network/R/dolphin.gml", format="gml")
summary(dolphin_gml)
dolphins_net <- asNetwork(dolphin_gml)
class(dolphins_net)

#-----------------------------------------------------------------------------------------------

#Reading the data
dolphinEdges <- read.csv("Dolphins_Edges.csv")
dolphinVertices <- read.csv("Dolphins_Vertices.csv")

#Creating dataframe
dolphin_dframe <- graph_from_data_frame(dolphinEdges, vertices= dolphinVertices, directed=FALSE)

# Edges
ecount(dolphin_dframe)
# Vertices
vcount(dolphin_dframe)

#Plotting the network
set.seed(1)
get.vertex.attribute(dolphins_net, "label")
plot(dolphins_net, vertex.cex = 1.5, 
     vertex.col = "#39558C", vertex.border = "white", 
     edge.col = "gray75", edge.lwd = 0.01)
title(main = "Adjusted network plot", line = -2, cex.main = 0.9)

plot(dolphins_net, vertex.cex = 2.3, 
     vertex.col = "#32648EFF", vertex.border = "white", 
     edge.col = "gray75", edge.lwd = 0.01,
     label = get.vertex.attribute(dolphins_net, "label"),
     label.pos = 5, # center the label 
     label.col = "gray100",
     label.cex = ifelse(nchar(get.vertex.attribute(dolphins_net, "label")) > 4, 0.4, 0.5), # adjust label size depending on the label length
     pad = -3) # enlarge the plot area to prevent node overlap
title(main = "Dolphins Network", line = 1, cex.main = 0.9)

#------------------------------------------------------------------------------------------------

#Gender distribution
V(dolphin_gml)
E(dolphin_gml)

V(dolphin_gml)$label
V(dolphin_gml)$sex

layout0 <- layout.fruchterman.reingold(dolphin_gml)
plot(dolphin_gml, layout=layout0, vertex.label=NA, vertex.size=5)

#Count of male and female dolphins
male = V(dolphin_gml)$sex == "M"
sum(male)
female = V(dolphin_gml)$sex == "F"
sum(female)
unkown = V(dolphin_gml)$sex == "U"
sum(unkown)

V(dolphin_gml)$color = "white"
V(dolphin_gml)[male]$color = "blue"
V(dolphin_gml)[female]$color = "pink"
plot(dolphin_gml, layout=layout0, vertex.label=NA, vertex.size=10)

#-------------------------------------------------------------------------------------------------

# Check whether Self_loops exist, as do multiple edges
is.simple(dolphin_dframe) 

#Network Connection
is.connected(dolphin_dframe)

#Network Strength
is.connected(dolphin_dframe, mode="weak")

#-------------------------------------------------------------------------------------------------

E(dolphin_dframe)$weight <-1
E(dolphin_dframe)$weight

inv_weight<-1/log(E(dolphin_dframe)$weight  + 1)
num_weight<-E(dolphin_dframe)$weight 
length(inv_weight)
E(dolphin_dframe)$weight <-inv_weight

summary(dolphin_dframe)

#Network Layout

layout1 <- layout.fruchterman.reingold(dolphin_dframe, dim =3)
plot(dolphin_dframe,weight=inv_weight,edge.size=5,edge.width=0.7,edge.arrow.width=0.5,
     layout=layout1,vertex.size=10,vertex.color='lightpink',edge.color='black',vertex.label= dolphin_dframe$Gender)
title("Fruchterman-Reingold Layout")

layout2 <- layout.circle(dolphin_dframe)
plot(dolphin_dframe,weight=inv_weight,edge.size=5,edge.width=0.7,edge.arrow.width=0.5,
     layout=layout2,vertex.size=10,vertex.color='lightpink',edge.color='black',vertex.label=dolphin_dframe$Gender)
title("Circle Layout")

#------------------------------------------------------------------------------------------------

#Community Detection

#1.Girvan-Newman Algorithm
girvan_comm <- edge.betweenness.community(dolphin_dframe, weights=inv_weight)
girvan_size <- sizes(girvan_comm)
cm_edge <- membership(girvan_comm)

plot(girvan_comm, dolphin_dframe, vertex.edge= NA, vertex.size=10,vertex.label=NA)+ title(main = "Girvan Newman Algorithm")

#2.Walktrap Algorithm
walktrap<-walktrap.community(dolphin_dframe,weights=E(dolphin_dframe)$weight)
cm_walktrap <- membership(walktrap)
plot(walktrap,dolphin_dframe,vertex.label=NA, vertex.size=10)+ title(main = "Walk-Trap Algorithm")

#3.Fast Greedy Algorithm
fast <- fastgreedy.community(dolphin_dframe, weights=E(dolphin_dframe)$weight)
cm_fast <- membership(fast)
plot(fast,dolphin_dframe,vertex.label=NA, vertex.size=10)+ title(main = "Fast Greedy Algorithm")

#---------------------------------------------------------------------------------------------------

#Degree of each Node/Dolphin
plot(degree(dolphin_dframe),xlab = "Nodes", ylab = "Degree")
x<- degree(dolphin_dframe)
max(x)
min(x)

#Degree Distribution
plot(degree_distribution(dolphin_dframe),xlab = "Nodes", ylab = "Degree Distribution")

# Embeddedness 
round(constraint(dolphin_dframe, nodes=V(dolphin_dframe)), digits=4)

# Node betweenness
n <-round(betweenness(dolphin_dframe, v=V(dolphin_dframe), directed = FALSE, nobigint =TRUE, normalized = FALSE))
max(n)
#dolphin-SN100 has the max node betweenness

# Edge betwenness
e <- edge.betweenness(dolphin_dframe, e=E(dolphin_dframe), directed = FALSE)

# Closeness Centrality
close_center <- closeness(dolphin_dframe, mode="all", weights=NA)
close_center
max(close_center)
min(close_center)
sorted<- sort(close_center, decreasing=TRUE)[1:15]
plot(sorted, xlab="Nodes", ylab="Closeness")

#Eigen Vector Centrality
e<-eigen_centrality(dolphin_dframe, directed=T, weights=NA)
sort(eigen_centrality(dolphin_dframe, directed=T, weights=NA)$vector, decreasing=TRUE)[1:15]

#---------------------------------------------------------------------------------------------------

# Plotting Centralities

# Degree centrality
deg <- degree(dolphins_net, gmode = "graph") %>% as_tibble() %>% rename(deg = value)
# Closeness centrality
close <- closeness(dolphins_net, gmode = "graph", cmode = "undirected") %>% as_tibble() %>% rename(close = value)
# Betweenness centrality
btwnnes <- betweenness(dolphins_net, gmode = "graph", cmode = "undirected") %>% as_tibble() %>% rename(btwnnes = value)
# Eigenvector centrality
eign <- evcent(dolphins_net, gmode = "graph") %>% as_tibble() %>% rename(eign = value)
# Set centrality measures as vertex attributes
set.vertex.attribute(dolphins_net, c("deg", "close", "btwnnes", "eign"), c(deg, close, btwnnes, eign))

par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
set.seed(0)
plot(dolphins_net,
     vertex.cex = get.vertex.attribute(dolphins_net, "deg")/6,
     vertex.col = "#440154", vertex.border = "white", 
     edge.lwd = 0.01, edge.col = "gray75")
title("Degree", line = - 1, cex.main = 0.9)

set.seed(0)
plot(dolphins_net,
     vertex.cex = get.vertex.attribute(dolphins_net, "close")*5,
     vertex.col = "#2D718E", vertex.border = "white", 
     edge.lwd = 0.01, edge.col = "gray75")
title("Closeness", line = - 1, cex.main = 0.9)

set.seed(0)
plot(dolphins_net,
     vertex.cex = get.vertex.attribute(dolphins_net, "btwnnes")/100,
     vertex.col = "#94D840", vertex.border = "white", 
     edge.lwd = 0.01, edge.col = "gray75")
title("Betweenness", line = - 1, cex.main = 0.9)

set.seed(0)
plot(dolphins_net, 
     vertex.cex = get.vertex.attribute(dolphins_net, "eign")*10,
     vertex.col = "#1F968B", vertex.border = "white",
     edge.lwd = 0.01, edge.col = "gray75")
title("Eigenvector", line = - 1, cex.main = 0.9)

#--------------------------------------------------------------------------------------------------

# Clustering
transitivity(dolphin_dframe, weights = inv_weight)

# Local clustering coefficients
clustering_dolphin <- transitivity(dolphin_dframe, type="local", vids=V(dolphin_dframe))
clustering_dolphin

# Avg. path length and diameter
average.path.length(dolphin_dframe, directed=FALSE)
diameter(dolphin_dframe)
diameter(dolphin_dframe, weights= num_weight)
diameter(dolphin_dframe, weights= inv_weight)

#---------------------------------------------------------------------------------------------------

#Cliques
table(sapply(maximal.cliques(dolphin_dframe), length))

#Plotting the cliques with size 5
cliq_max <- maximal.cliques(dolphin_dframe)
cliq_largest <- largest.cliques(dolphin_dframe)
cliq <- c(cliq_largest[[1]],cliq_largest[[2]],cliq_largest[[3]])

cliq_plot<- induced.subgraph(graph=dolphin_dframe,vids=(cliq))
plot(cliq_plot,main="Cliques with size 5")

#Plotting the cliques with size 4
cliq_four <- max_cliques(dolphin_dframe, min = 4, max = 4, subset = NULL,file = NULL)
cliq_four1<- c()
for (i in c(1:13)) {
  cliq_four1 <- c(cliq_four1,cliq_four[[i]])
}
cliq_four1

cliq_4_plot <- induced.subgraph(graph=dolphin_dframe,vids=(cliq_four1))
plot(cliq_4_plot,vertex.label=V(cliq_4_plot)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,
     vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="Cliques of size 4")

#Plotting the cliques with size 3
cliq_three <- max_cliques(dolphin_dframe, min = 3, max = 3, subset = NULL,file = NULL)
cliq_three1<- c()
for (i in c(1:30)) {
  cliq_three1 <- c(cliq_three1,cliq_three[[i]])
}
cliq_three1

cliq_3_plot <- induced.subgraph(graph=dolphin_dframe,vids=(cliq_three1))
plot(cliq_3_plot,vertex.label=V(cliq_3_plot)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="cliques of size 3")

#Plotting the cliques with size 2
cliq_two <- max_cliques(dolphin_dframe, min = 2, max = 2, subset = NULL,file = NULL)
cliq_two1<- c()
for (i in c(1:38)) {
  cliq_two1 <- c(cliq_two1,cliq_two[[i]])
}
cliq_two1

cliq_2_plot <- induced.subgraph(graph=dolphin_dframe,vids=(cliq_two1))
plot(cliq_2_plot,vertex.label=V(cliq_2_plot)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="cliques of size 2")

#---------------------------------------------------------------------------------------------------

# Node betweenness
betweens_dolphin <- round(betweenness(dolphin_dframe, v=V(dolphin_dframe), directed = FALSE, nobigint =TRUE, normalized = FALSE))
between_dframe<-data.frame(betweens_dolphin)
between_dframe<-data.frame(rownames(between_dframe),betweens_dolphin)
colnames(between_dframe)<-c('Vertex','Betweenness')
top10_betweenness<-between_dframe[order(between_dframe$Betweenness,decreasing = T),][1:10,]
bet1<-as.character(top10_betweenness$Vertex)
top10_betweenness_subgraph<-induced_subgraph(dolphin_dframe,vids=bet1)

plot(top10_betweenness_subgraph,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7) + title("Top-10 Nodes with Highest Betweenness")
#--------------------------------------------------------------------------------------------------

# Closeness 
closeness_dolphin<-round(closeness(dolphin_dframe, v=V(dolphin_dframe), normalized = FALSE))
closeness_dframe<-data.frame(closeness_dolphin)
closeness_dframe<-data.frame(rownames(closeness_dframe),closeness_dolphin)
colnames(closeness_dframe)<-c('Vertex','Closeness')
top10_closeness<-closeness_dframe[order(closeness_dframe$Closeness,decreasing = T),][1:10,]
close1<-as.character(top10_closeness$Vertex)
top10_closeness_subgraph<-induced_subgraph(dolphin_dframe,vids=close1)

plot(top10_closeness_subgraph,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7) + title("Top-10 Nodes with Highest Closeness")

#--------------------------------------------------------------------------------------------------

#Embeddedness
constraints <- round(constraint(dolphin_dframe, nodes=V(dolphin_dframe)), digits=4)
constraints_dframe<-data.frame(constraints)
constraints_dframe<-data.frame(rownames(constraints_dframe),constraints)
colnames(constraints_dframe)<-c('Vertex','constraints')
top10<-constraints_dframe[order(constraints_dframe$constraints),][1:20,]
vertex1<-as.character((top10$Vertex))
top10_subgraph<-induced_subgraph(dolphin_dframe,vids=vertex1)

plot(top10_subgraph,vertex.size=15,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7) + title("Top 20 Nodes with Highest Structural Hole")

#--------------------------------------------------------------------------------------------------

# Degree centrality
degree_centrality <- degree(dolphin_dframe)
degree_dframe<-data.frame(degree_centrality)
degree_dframe<-data.frame(rownames(degree_dframe),degree_centrality)
colnames(degree_dframe)<-c('Vertex','Degree')
top10_degree<-degree_dframe[order(degree_dframe$Degree,decreasing = T),][1:10,]
deg1<-as.character(top10_degree$Vertex)
top10_degree_subgraph<-induced_subgraph(dolphin_dframe,vids=deg1)

plot(top10_degree_subgraph,vertex.size=degree(top10_degree_subgraph)*5,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7)+ title("Top 10 Nodes with Highest Degree") 

#-----------------------------------------------------------------------------------------------

# Edge betwenness
edgebetweens_dolphin <-edge.betweenness(dolphin_dframe, e=E(dolphin_dframe), directed = FALSE)

# Local clustering coefficients
clustering_dolphin <- transitivity(dolphin_dframe, type="local", vids=V(dolphin_dframe))

node_frame<-data.frame(betweens_dolphin, constraints, clustering_dolphin, degree_centrality)
a_node<-aggregate(betweens_dolphin ~ clustering_dolphin, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")

a_node<-aggregate(betweens_dolphin ~ degree_centrality, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")

a_node<-aggregate(clustering_dolphin ~ degree_centrality, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")

a_node<-aggregate(constraints ~ degree_centrality, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

#------------------------------------------------------------------------------------------------
