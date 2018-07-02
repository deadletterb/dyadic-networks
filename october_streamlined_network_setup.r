#==========================================================#
#  First, get user input on type, size, and coefficients#
#==========================================================#


#==========================================================#
#  Next, Generate the files needed to generate the network
#  1) Appropriate adjacency matrix, labeled 'adjacency.matrix'
#  2) Attribute matrix, labeled 'attr.vector'
#  3) Edgelist matrix, labeled 'edges.vector'
#==========================================================#
num_rows<-log10(nrow(biadjacency.matrix))#given via the log10 of the number of rows you want
#rate=.05
adjacency.matrix<-create.ring.network(num_rows)
#adjacency.matrix<-create.smallworld.network(num_rows,beta=.04)
#adjacency.matrix<-create.random.network(num_rows,rate=rate) #note: make them connect to at least one
adjacency.matrix<-create.scalefree2.network(n=num_rows,r=3.2)
attr.vector<-setup.3d(num_rows,layout.par)
attr.vector<-update_vertex_count(attr.vector,adjacency.matrix,layout.par)
color.vector<-color.degree.heatmap(attr.vector,layout.par)  
attr.vector[,7]<-color.vector
layout.par["lines"]<-TRUE
edgelist<-create.edgelist(adjacency.matrix)
network_plot3d(attr.vector,edgelist,layout.par)

#temp<-sort(attr.vector[,6],decreasing=TRUE)
#==========================================================================
#
#  This is the section for quick changing layout appearances
#
#==========================================================================

#row.names(temp)<- a vector of names, otherwise numbered. 
layout.par["lines"]<-FALSE 
layout.par["heat"]<-FALSE
layout.par["axes"]<-TRUE
layout.par["invert"]<-TRUE

layout.par["threshold"]<-sqrt(max(joe[,6]))
network_plot3d(attr.vector,edgelist,layout.par)


#==========================================================================
# This section creates a non-bipartite card deck
#==========================================================================

num_rows <- 52
adjacency.matrix <- create.cards.52()    # creates a non-bipartite graph of 52 elements
layout.par["threshold"]<-7   # might have to set this to 52 on some sorts?
# currently set mid-way between sets (deg=13) and suits (deg = 4)
attr.vector<-setup.3d(num_rows,layout.par)
attr.vector<-update_vertex_count(attr.vector,adjacency.matrix,layout.par)
color.vector<-color.cards(temp) # 52 cards
attr.vector[,7]<-color.vector

layout.par["lines"]<-TRUE
edges.vector<-lines_edge_list(adjacency.matrix,attr.vector)
network_plot3d(attr.vector,edges.vector,layout.par)


#==========================================================================
# creates a bipartite graph of 52 cards, 13 numbered relations, 4 suits, and 'face-cards' (70 items)
#==========================================================================

num_rows <- 1.84
adjacency.matrix <- create.cards.bipartite()   
layout.par["threshold"]<- 3   # This makes sets (deg = 4) and up into hubs. 
# Might need to define hubs differently if hubs in the bipartite sense can have degree less than network elements
color.vector<-matrix("red",nrow=69,ncol=1)
color.vector[53:65,1]<-"blue"
color.vector[66:69,1]<-"orange"
# color.vector[70,1]<-"yellow"

attr.vector<-setup.3d(num_rows,layout.par)
attr.vector<-update_vertex_count(attr.vector,adjacency.matrix,layout.par)
attr.vector[66:69, 5]<-.8
attr.vector[,7]<-color.vector
layout.par["lines"]<-TRUE
edgelist<-create.edgelist(adjacency.matrix)

network_plot3d(attr.vector,edgelist,layout.par)

#==========================================================================
# creates a bipartite graph of 52 cards, 13 numbered relations, 4 suits, and membership in the set (71 items)
#==========================================================================

num_rows <- 1.85
adjacency.matrix <- create.cards.bipartite_2()   
layout.par["threshold"]<- 3   # This makes sets (deg = 4) and up into hubs. 
# Might need to define hubs differently if hubs in the bipartite sense can have degree less than network elements
color.vector<-matrix("red",nrow=round(10^num_rows),ncol=1) # all the nodes representing cards are red
color.vector[53:65,1]<-"blue" # all the nodes representing sets are blue
color.vector[66:69,1]<-"orange" # all the nodes representing suits are orange
 color.vector[70:71,1]<-"yellow" # the meta-nodes of "sets" and "suits" are yellow

attr.vector<-setup.3d(num_rows,layout.par)
attr.vector<-update_vertex_count(attr.vector,adjacency.matrix,layout.par)
attr.vector[70:71,6]<-17
attr.vector[,7]<-color.vector
layout.par["lines"]<-TRUE
edgelist<-create.edgelist(adjacency.matrix,attr.vector)
clear3d()
network_plot3d(attr.vector,edgelist,layout.par)

attr.vector<-sort.ontology.sort(adjacency.matrix, attr.vector, edgelist, layout.par, 1, 52)
