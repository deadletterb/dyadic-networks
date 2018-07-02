# January 2018 project
# 
#   First, run setup_function.R
#   Then, run setup_function.R
#   Then, find the file name by browsing to the file, right clicking, and looking at the properties. 
#   For example:  C:/Users/deadletter/Downloads/1000 biadjacency.csv
#   You will need to concatenate the file name (in the editable box) with the pathname (Windows 10)
#   
#   My CSV files come from doing google drive exports of M x 20, where M is the number of rows in the test.
#   Use read.csv to create a file 
#
#==========================================================================================================
#
#
#
#==========================================================================================================
`10000.biadjacency` <- read.csv("C:/Users/deadletter/Downloads/10000 biadjacency.csv", header=FALSE)
'10000.biadjacency' <- read.csv("c:/users/deadletter/Downloads/10000 biadjacency.csv", header=FALSE)
'1000.biadjacency'  <- read.csv("c:/users/deadletter/Downloads/1000 biadjacency.csv", header=FALSE)

movies.clean <- read.csv("C:/Users/deadletter/Downloads/movies clean.csv") # gets the entire movies with their labels
x<-30000                                                                    # setup the number of movies you want to select, max about 46,000
biadjacency.matrix <- as.matrix(movies.clean [2:(x+1),5:21])               # biadjacency.matrix <- biadjacency[1:100]
layout.par["bipartite_size"]<-ncol(biadjacency.matrix)                     # a couple of times you need to know how many categoricals there were.
                                                                           # the term 'gephi.edgeslist' is so that everything created here can be exported to gephi for comparison.
# gephi.edgelist <- convert.bipartite(biadjacency.matrix)                  # This version uses the networking package, which unfortunately
                                                                           # limits it to about 16,000 rows. Use the new bipartite edgemaker 
                                                                           # to do rstudio work without moving over to big adjacencies
gephi.edgelist <- biadjacency.edgemaker(biadjacency.matrix)
write.csv(gephi.edgelist, file="gephi.edgelist.csv",row.names=FALSE)

#==========================================================================================================
#
#    As of this point, you've got an edgelist for gephi - after this setup for R, in order to create 
#    neighborhood lists from which to do the layouts
#   
#
#
#=========================================================================================================

num_rows<-log10(nrow(biadjacency.matrix) + ncol(biadjacency.matrix))
attr.vector<-setup.3d(num_rows, layout.par)
attr.vector<-bipartite_vertex_setup(attr.vector,biadjacency.matrix,layout.par)  #this REQUIRES the biadjacency
#attr.vector[1:20,]
color.vector<-color.degree.heatmap(attr.vector,layout.par)  
attr.vector[,7]<-color.vector
layout.par["lines"]<-TRUE
edgelist<-gephi.edgelist

network_plot3d(attr.vector,edgelist,layout.par)
nw<-network(gephi.edgelist,directed=FALSE)
plot.network(nw)                                                               # this built-in is almost always useless.

#================================================================================================
#
#  As of this point, well prepared attr.vectors should be ready to do layouts on.
#================================================================================================


attr.vector2<-bipartite.sort.surface.sort( attr.vector,  edgelist, layout.par, mod = 3)
attr.vector2<-sort.surface.hub(            attr.vector, edgelist, layout.par, mod = 1)
attr.vector2<-sort.out.sort.in(            attr.vector,  edgelist, layout.par, mod = 1)

layout.par["iterations"]<-10

attr.vector[,6]<-log2(attr.vector[,6])  # use this when wanting to change degree to a log scale - also can use column 8

#==============================
# debugging
#
#==============================

layout.par["lines"]<-FALSE
layout.par["lines"]<-TRUE

q<-nrow(attr.vector)
#=================================================================================
#
#  This is the debugger for playing with geo-mean, normalize, recenter, and various target list
#
#=================================================================================
attr.vector2<-attr.vector3
hood_list<-hood_list2



#network_plot3d(attr.vector,edgelist,layout.par)
#attr.vector2[,6]<-log10(attr.vector2[,6])
p<-max(attr.vector2[,6])
n<-min(attr.vector2[,6])
#attr.vector2[,5]<- (1 - attr.vector2[,5]/p)

attr.vector2<-random_sphere(attr.vector2,layout.par)          # if you want to restart a functioning graph to a random sphere
  network_plot3d(attr.vector2,new_edgelist,layout.par)

  
  for(i in 1:layout.par["iterations"]){
attr.vector2<-recenter(attr.vector2)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-normalize(attr.vector2,1:18,radius=7)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, 18:q)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-recenter(attr.vector2)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-normalize(attr.vector2,18:q,radius=3)
#   network_plot3d(attr.vector2,new_edgelist,layout.par)
   
attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, 1:17)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
  attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, 18:q)
  network_plot3d(attr.vector2,new_edgelist,layout.par)

    attr.vector2<-recenter(attr.vector2)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
  attr.vector2<-normalize(attr.vector2,1:18,radius=7)
#  network_plot3d(attr.vector2,new_edgelist,layout.par)
  attr.vector2<-normalize(attr.vector2,18:q,radius=3)
    network_plot3d(attr.vector2,new_edgelist,layout.par)
  print(i)
  }  
  
  
  ## when you get back, work on adding sizes to the nodes based on frequency of occurrence in the master hood list
  
attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, 18:q)
  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-recenter(attr.vector2)
  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-normalize(attr.vector2,1:18,radius=1.75)
  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-geo_mean(hood_list,attr.vector2,TRUE,18:q)
  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-recenter(attr.vector2)
  network_plot3d(attr.vector2,new_edgelist,layout.par)
attr.vector2<-normalize(attr.vector2,18:q,radius=1)




attr.vector2<-normalize(attr.vector2,18:q,radius=1)

attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, seq(1:17))
attr.vector2<-recenter(attr.vector2)

attr.vector2<-normalize(attr.vector2,1:17, radius=1.75)
attr.vector2<-geo_mean(hood_list,attr.vector2,TRUE,seq(1:q))
attr.vector2<-recenter(attr.vector2)

attr.vector2<-normalize(attr.vector2,18:q,radius=1)

attr.vector2<-normalize_subjective(attr.vector2,1:q,layout.par)

#attr.vector2<-recenter(attr.vector2)

attr.vector2<-normalize(attr.vector2,18:q,radius=1)

attr.vector2<-geo_mean(hood_list,attr.vector2,TRUE,18:q)

network_plot3d(attr.vector2,new_edgelist,layout.par)


attr.vector2<-geo_mean(hood_list, attr.vector2,TRUE, seq(1:q))
attr.vector2<-recenter(attr.vector2)


attr.vector2<-normalize(attr.vector2,18:q)
attr.vector2<-geo_mean(hood_list, attr.vector2, TRUE, seq(1:17))

network_plot3d(attr.vector2,edgelist,layout.par)


#=======================================================
#
#  This is the section for pulling UNIQUE neighborhoods out. 
#
#
#=======================================================

nw<-network(edgelist,directed = FALSE)
hood_list<-get_neighborhoods(nw)
unique_hood_list<-unique(hood_list)

temp<-vector(length=length(unique_hood_list))
for (i in 1:length(hood_list)){
  for (j in 1:length(hood_list)){if (identical(hood_list[j],unique_hood_list[i])==TRUE) temp[i]<-temp[i] + 1}
}

convert_unique_to_edgelist<-function(unique_hood_list){

  
  # This function takes the list of genre couplings - all movies with the same genres - and makes a network to display of only those. 
  # It turns a hood_list into an edgelist. 
  
  temp1<-matrix(0,nrow=1,ncol=2)
  temp2<-matrix(0,nrow=1,ncol=2)
  #print(temp2)

  for (i in 18:length(unique_hood_list)){
    #print(i)
    for (j in 1:length(unique_hood_list[[i]])){
      #print(temp2)
      temp2[1,1]<-i
      temp2[1,2]<-unique_hood_list[[i]][j]
      temp1<-rbind(temp1,temp2)
  
          }
  }
  temp1<-temp1[2:nrow(temp1),]
  return(temp1)  # returns a symmetrical edgelist for the new network. 
  
  # NOW SET UP A NEW ATTR.VECTOR FOR THIS NEW GROUP, INCLUDING the frequencies of these new ontologies.
}
new_edgelist<-convert_unique_to_edgelist(unique_hood_list)
write.csv(new_edgelist,file = "new_gephi_edgelist.csv", row.names = FALSE)
nw2<-network(new_edgelist,directed=FALSE)
hood_list2<-get_neighborhoods(nw2)
num_rows<-log10(length(unique_hood_list))
attr.vector3<-setup.3d(num_rows, layout.par)
q<-round(10^num_rows)

# figure out how to give degrees from edgelist?
for(i in 1:max(new_edgelist[,1])){
 temp[i]<- sum(new_edgelist[,1]==i)
}
for(i in 1:layout.par["bipartite_size"]){
  temp[i]<-sum(new_edgelist[,2]==i)
}
color.vector<-color.degree.heatmap(attr.vector3,layout.par)  
attr.vector3[,7]<-color.vector
attr.vector3<-resize(attr.vector3,layout.par)
network_plot3d(attr.vector3,new_edgelist,layout.par)

attr.vector3<-sort.surface.hub(            attr.vector3, new_edgelist, layout.par, mod = 1)


write.csv(temp3,       file = "node_size.csv"         , row.names = FALSE)
