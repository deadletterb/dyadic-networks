#
# In this section, we'll try a new approach. 
# create only an attribute vector of rows (N) with x,y,z,w,r,degree, color, size
# create an ontology matrix which is N x M, where M is a changing number indicating how 
# many ontological groupings there have been
# Also need to create the method by which one ADDS a grouping, including...
# Need a matrix M x 8 with the attributes of the ontologies. 
# 
# If done properly, this allows us a relatively 'endless' number of hierarchical 
# chains of ontological order that can be added at to any time - getting us the 'on the fly' we always needed. 
# 

num_rows<-1.2042
attr.vector<-create_ontology_based_network(num_rows,layout.par)
attr.vector<-spiral(attr.vector,3)

clear3d()
plot_nodes(attr.vector,layout.par)
onto.augment.vector<-create.membership.vector(num_rows) #create the generic membership node which will be used to add other groupings
#attr.vector

onto.augment.vector<-make.groups(attr.vector,onto.augment.vector,layout.par)
#onto.attr.vector<-rbind(onto.attr.vector, c(0,0,0,1,1,1,"yellow", .1))

onto.augment.vector

x<-ncol(onto.augment.vector)
onto.attr.vector<- create.onto.attr.vector(x) # create a single row onto.vector for the ontology hypernodes.


temp.adjacency<-make.adjacency(onto.augment.vector)

plot_nodes(onto.attr.vector,layout.par)
plot_nodes(temp.adjacency, layout.par)
onto.attr.vector<-sort.ontology.sort(attr.vector,onto.augment.vector,layout.par, mod=1)


