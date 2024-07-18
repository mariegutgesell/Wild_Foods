##Network Analysis and Visualization with R and igraph
##Tutorial
##link: https://kateto.net/netscix2016.html

library(igraph)

##2.1 Create networks --------
##First, create network with 3 nodes, and undirected edges (links) between each node
g1 <- graph(edges = c(1,2, 2,3, 3,1 ), n = 3, directed = F) ##edges indicate where links are e.g., first pair is node 1->2
plot(g1)

g1

#Now with 10 vertices, and directed by default
g2 <- graph (edges = c(1,2, 2,3, 3,1), n = 10)
plot(g2)

#Try with names vertices
g3 <- graph (c("John", "Jim", "Jim", "Jill", "Jill", "John"))
#when the edge list has vertex names, the number of nodes is not needed
plot(g3)

#In named graphs, we can specify isolates by providing a list of their names
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
       isolates=c("Jesse", "Janis", "Jennifer", "Justin") )
plot(g4, edge.arrow.size = .5, vertex.color = "gold", vertex.size = 15, vertex.frame.color = "gray", vertex.label.color = "black", vertex.laabel.cex = 0.8, vertex.label.dist = 2, edge.curved = 0.2)

#The edges of the object
E(g4)

#The vertices of the object
V(g4)

#You can also examine the network matrix directly
g4[]

##2.2 Edge, vertex and network attributes ---------
#You can add attributes to the network, vertices, or edges
V(g4)$name #this was automatically generated when we created the network
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")

E(g4)$type <- "email" # Edge attribute, assign "email" to all edges

E(g4)$weight <- 10    # Edge weight, setting all existing edges to 10

#Examine attributes
edge_attr(g4)
vertex_attr(g4)

graph_attr(g4)

##Another way to set attributes -- can similarly use set_edge_attr(), set_vertex_attr()
g4 <- set_graph_attr(g4, "name", "Email Network")
g4 <- set_graph_attr(g4, "something", "A thing")

graph_attr_names(g4)

graph_attr(g4, "name")
graph_attr(g4)

g4 <- delete_graph_attr(g4, "something")
graph_attr(g4)

plot(g4, edge.arrow.size = .5, vertex.label.color = "black", vertex.label.dist = 1.5, 
     vertex.color = c("pink", "skyblue")[1+(V(g4)$gender == "male")])

plot(g4, edge.arrow.size = .5, vertex.label.color = "black", vertex.label.dist = 1.5, 
     vertex.color = c("pink", "skyblue")[1+(V(g4)$gender == "male")])

#We can simplify our graph to remove loops and multiple edges between the same nodes
#Use edge.attr.comb to indicate how edge attributes are to be combined -- possible options include sum, mean, prod, min, max, firs/list (selects the first/last edges attribue)
#Option ignore says the attributed should be disregared and dropped
g4s <- simplify(g4, remove.multiple = T, remove.loops = F, edge.attr.comb = c(weight = "sum", type = "ignore"))
plot(g4s, vertex.label.dist = 1.5)
g4s                       

#The description of an igraph object starts with up to four letters:
  #D or U, for a directed or undirected graph
  #N for a named graph (where nodes have a name attribute)
  #W for a weighted graph (where edges have a weight attribute)
  #B for a bipartite (two-mode) graph (where nodes have a type attribute)

#The two numbers that follow (7 5) refer to the number of nodes and edges in the graph. The description also lists node & edge attributes, for example:
  #(g/c) - graph-level character attribute
  #(v/c) - vertex-level character attribute
  #(e/n) - edge-level numeric attribute

##2.3 Specific graphs and graph models -----
#Empty graph
eg <- make_empty_graph(40)
plot (eg, vertex.size = 10, vertex.label = NA)

#Full graph
fg <- make_full_graph(40)
plot(fg, vertex.size = 10, vertex.label = NA)

#Simple start graph
st <- make_star(40)
plot(st, vretex.size = 10, vertex.label = NA)

#Tree graph
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size = 10, vertex.label = NA)

#Ring graph
rn <- make_ring(40)
plot(rn, vertex.size = 10, vertex.label = NA)

#Erdos-Renyi random graph model
er <- sample_gnm(n = 100, m = 40) #n is the number of nodes, m is the number of edges
plot(er, vertex.size = 6, vertex.label = NA)

#Watts-Strogatz small-world model
##Creates a lattice (with dim dimensions and size nodes across dimension) and rewires edges randomly with probability p
##The neighborhous in which edges are connected is nei
#3You can allow loops and multiple edges
sw <- sample_smallworld(dim = 2, size = 10, nei = 1, p = 0.1)
plot(sw, vertex.size = 6, vertex.label = NA, layout = layout_in_circle)

#Barabasi-Albert preferential attachment model for scale-free graphs
##n is the number of nodes, power is the power of attachment (1 is linear); m is the number od edges added on each time step
ba <- sample_pa(n = 100, power = 1, m = 1, directed = F)
plot(ba, vertex.size = 6, vertex.label = NA)

#Rewiring a graph
#each_edge() is a rewiring method that changes the edge endpoints uniformly randomly with a probability prob
rn.rewired <- rewire(rn, each_edge(prob = 0.1))
plot(rn.rewired, vertex.size = 10, vertex.label = NA)
#rewire to connect vertices to other vertices at a certain distance
rn.neigh = connect.neighborhood(rn, 5)
plot(rn.neigh, vertex.size = 8, vertex.label = NA)

##Combine graphs (disjoin union, assuming separate vertex sets): %du%
plot(rn, vertex.size = 10, vertex.label = NA)
plot(tr, vertex.size = 10, vertex.label = NA)

plot(rn %du% tr, vertex.size = 10, vertex.label = NA)

