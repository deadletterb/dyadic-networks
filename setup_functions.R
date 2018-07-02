assign_degree<-function(temp){ #===========================================# This section generates a number of links for each node, sampling from normalized k^-r
  temp[,2]<-runif(nrow(temp))                # Generates a random number for each node
  temp[,2]<-sort(temp[,2],decreasing=TRUE)    #
  for (i in 1:nrow(temp)){                   # For each node
    temp [i,3]<-temp[i,2]                   # Places the random number in column 3
    k<-0                                    # 
    while (temp[i,3]>=0){                   # Until it reaches 0, subtract one row's probability from Column 1
      k<- k+1                             #
      temp[i,3]<-temp[i,3]-temp[k,1]} #
    
    if (temp[i,3]<=0){temp[i,3]<-k}         # This is where the degree is assigned for that probability
  }
return(temp)}

biadjacency.edgemaker <- function(biadj){
  # Given a biadjacency.matrix of MxN dimensions, creates a dataset of dimensions Q x 2, where Q is
  # the sum of all lines in the biadjacency matrix, I think. 
  q<- sum(biadj[1,])
  r<- nrow(biadj)
  s<- ncol(biadj)
  temp0 <- matrix (0,nrow = 1, ncol = 2)
  colnames(temp0)<-c("Source","Target")
  for (i in 1:r){
    for (j in 1:s){
      if (biadj[i,j] == 1) {
        #print(i)
        #print(j)
        temp<-matrix(0,nrow = 1, ncol = 2)
        temp[1,1]<- (i + s)   # later replace this 1 with i to change which row begins the edgelist.
        temp[1,2]<- j   # this edge points TOWARDS the bipartite node, as all edges do -> 
                        # FROM semantic node TOWARDS categorical nodes
        #print(temp)
        temp0 <- rbind(temp0, temp)
        }
      }    
    }

  #print(temp0)
  temp0 <- temp0 [2:nrow(temp0),]
  return(temp0)
}

bipartite_vertex_setup <- function(attr,biadj,layout.par){
  x<-nrow(attr)
  y<-ncol(biadj)
  radius<-layout.par["radius"]
  attr[,6]<-1
  for (i in 1:y){
    attr[i,6]<-sum(biadj[,i])
    attr[i,5]<-radius
    }
  
  for (i in (y+1):(x)){
    attr[i,6]<-sum(biadj[(i-y),1:y])
    attr[i,5]<-radius
    if (i%%100==0){print( i)}}
  attr<-resize(attr,layout.par)
  
  temp<-sort(attr[,6],decreasing=TRUE)
  barplot(temp)
  #, add=TRUE)
  return(attr)
}

bipartite.sort.surface.sort<-function(attr, edges, layout.par, mod=0){
  # This function assumes the following items:
  # the standard attribute vector containing
  # X, Y, Z, 1, Radius, Degree, color
  # an attribute vector for the ontology nodes
  # a matrix of the ontologies onto the nodes
  
  # layout.par - useful for global changes like size, etc.
  # the modulo of how often to display the sort
  # an _ontology vector_ of size N objects x as many as the power set (but normally much smaller) of _groupings_ each node could be in.
  # in a properly formatted bipartite style matrix, this is the rows beyond the N elements
  # we need the number of elements to discern the ontologies in the augmented matrix from the 
  
  # The general method of the sort is to place the objects onto the sphere, and then place the hyper-nodes representing the ontogies
  # ontologies ARE/CAN BE forced to a radius based on the ordinality - 
  # the FULL ordinality is membership, ie all nodes, and is the center. 
  # Less ordinality is closer to the surface
  # having established the basic nodes + hypernodes, each group is sent to its geometric mean and re-normalized, in order of
  # all nodes first
  # low-ordinal groupings
  # higher ordinal groupings
  # recenter, renormalize
  
  lines<-layout.par["lines"]
  if (mod==0){mod<-layout.par["iterations"]+1}      # setting mod = 0 makes it not graph till the end.
  iterations<-layout.par["iterations"]

  # The attr.vector that is passed has already been setup to have the first rows equal the columns of the biadjacency.
  
  N<-nrow(attr)
  Q<-layout.par["bipartite_size"]
  x<-N+Q
  nw<-network(edges,directed=FALSE)
  hood_list<-get_neighborhoods(nw)                             # NEED TO GET NEIGHBORHOODS FROM AN EDGELIST
  layout.par["lines"]<- FALSE
  
  for (i in 1:iterations){
    #print(i)
    attr<-geo_mean(hood_list,attr,TRUE,1:N)                 
    #attr<-geo_mean(hood_list,attr,TRUE,1:N)                     # all elements to their geometric mean
    #attr<-normalize_subjective(attr,1:N,layout.par)                                   # then normalize
    attr<-geo_mean(hood_list,attr,TRUE,1:Q)                    # ontologies go to their geometric mean
    
    attr<-recenter(attr)                                        # necessary to recenter before normalizing or it all creeps to the side
    #attr<-normalize_subjective(attr,1:x,layout.par)
    
    attr<-normalize(attr,1:N)                                  # blows it out from the new center. In this first sort, they all move
    #attr<-geo_mean(hood_list, attr,TRUE,hub_list)                             # only hyperedges go to their geometric mean, OR use above, only network objects get normalized
    if (i%%mod==0){network_plot3d(attr,edges,layout.par)}
    ##ani.pause()
  }
  layout.par["lines"]<-lines                                      #If wanted, turns the lines back on for the final graph.
  #attr<-pendulums(attr,layout.par)
  #clear3d()
  network_plot3d(attr,edges,layout.par)  
  return(attr)
}

color.cards<-function(df){
  temp<-matrix(0,nrow=nrow(df),ncol=2)
  temp[1:13,1]<-"red"
  temp[1:13,2]<-"Hearts"
  temp[14:26,1]<-"pink"
  temp[14:26,2]<-"Diamonds"
  temp[27:39,1]<-"gray"
  temp[27:39,2]<-"Spades"
  temp[40:52,1]<-"black"
  temp[40:52,2]<-"Clubs"
  return(temp)}

color.degree.heatmap<-function(attr,layout.par){
  x<-nrow(attr)
  if(layout.par["heat"]==TRUE){color.palette<-heat.colors(max(attr[,6]))
                               #print(color.palette)
  }#This line creates a color plaette, heat map style, from white (stubs) to red (hubs)
  else{color.palette<-terrain.colors(max(attr[,6]))} #same, different color palette
  
  if (layout.par["invert"]==TRUE){
    new.color.palette<-matrix(0,nrow=length(color.palette),ncol=2)    
    new.color.palette[,1]<-as.vector(color.palette)
    #print(new.color.palette)
    for (i in 1:length(color.palette)){
      new.color.palette[i,2]<-new.color.palette[(length(color.palette)-i+1),1]
    }
    color.palette<-new.color.palette[,2]
  }
  for (i in 1:x){
    attr[i,7]<-color.palette[attr[i,6]]   # Sets the color of this hub according to a linear scale on the heat map.    
  }
  #print(attr[,9])
  return(attr[,7]) # Changed to return a color vector only to be consistent with other usages
}

convert.bipartite <- function(biadjacency){
  # This function takes a MxN biadjacency and creates a M+N square matrix. It then fills 
  # core parts with the biadjacency and transposed biadjacency.
  # It then uses R's network package to convert that enormous adjacency matrix to an edge list
  # it will also convert that adjacency into a readable edgelist for gephi, and then 
  # export that into a file
  m <- nrow(biadjacency)
  #print (m)
  n <- ncol(biadjacency)
  #print (n)
  q <- m + n
  temp<-matrix(0,nrow = q,ncol = q)
  #print (temp[1,])
  temp[1:m, (m + 1):q]<-as.matrix(biadjacency)
  #print(temp[1,])
  #temp[(m + 1):q, 1:m]<-t(biadjacency)
  #print(nrow(temp))
  
  edgelist<-create.edgelist(temp)
  
  return(edgelist)
  
}

convert.bipartite.reverse <- function(biadjacency){
  #Suppose we have 50 nodes (m) by 20 genres (n). The matrix q would be 70. 
  
  #Then, the temp would be: temp [1:50, 51:70]<-biadjacency.matrix
  #temp [51:70; 1:50]
  
  
  # This function takes a MxN biadjacency and creates a M+N square matrix. It then fills 
  # core parts with the biadjacency and transposed biadjacency.
  # It then uses R's network package to convert that enormous adjacency matrix to an edge list
  # it will also convert that adjacency into a readable edgelist for gephi, and then 
  # export that into a file
  m <- nrow(biadjacency)
  #print (m)
  n <- ncol(biadjacency)
  #print (n)
  q <- m + n
  temp<-matrix(0,nrow = q,ncol = q)
  #print (temp[1,])
  temp[1:m, (m + 1):q]<-as.matrix(biadjacency)
  #print(temp[1,])
  temp[(m + 1):q, 1:m]<-t(biadjacency)
  #print(nrow(temp))
  
  bob<-network(temp)
  temp<-as.matrix(bob,matrix.type="adjacency") 
  return(temp)
  }

create.cards.52<-function(){
  temp<-matrix(0,nrow=52,ncol=52)
  temp[1:13,1:13]<-1
  temp[14:26,14:26]<-1
  temp[27:39,27:39]<-1
  temp[40:52,40:52]<-1
  # these lines remove self-loops
  for (i in 1:52){
    temp[i,i]<-0
  }    
  for (i in 1:52){
    for (j in 1:3){
      temp[i,(j*13+i)%%52]<-1
    }
  }
return(temp)}

create.cards.bipartite<-function(){
  # This function creates a hypergraph of a deck of cards
  # in a SIMPLE network - as dyads and nodes. It happens to be bipartite 
  # but for this purpose is not treated as bipartite - there are simply lots of zeroes
  
  # 52 cards are linked to 18 hubs.
  # Thirteen hubs are numbers
  # Four hubs represent suits
  # One hub represents 'face cards' (to experiment with assymetry)
  
  temp<-matrix(0,nrow=69,ncol=69,dimnames=list(card.names,card.names))
  temp2<-matrix(0,nrow=52,ncol=17)
  for (i in 1:13){
    temp2[i,14]<-1
  }
  for (i in 14:26){
    temp2[i,15]<-1
  }
  for (i in 27:39){
    temp2[i,16]<-1
  }
  for (i in 40:52){
    temp2[i,17]<-1
  }
  
  # uses modulo division %% to put a 1 in only the correct spots.
  
  # So nodes []
  
  for (i in 1:52){
    temp2[i,((i-1)%%13+1)]<-1
  }
  #for (j in 0:3){
  #    for (i in 10:12){
  #        temp2[
  #        # 1:52      # This makes the last node touch all 52 (membership in the group)
  #        18          # This makes this node touch only itself
  #        #(13*j+i)   # This makes this node touch face cards
  #        ,18]<-1
  #    }
  #}                      #that part creates the face-card relationship
  temp[1:52,53:69]<-temp2
  temp[53:69,1:52]<-t(temp2)
return(temp)}

create.cube.network<-function(j=3,k=4,l=5){
  # Because it is creating a 3d lattice, 
  # a convention for matrices is necessary. 
  # L will be 'levels', and jxk will be the size of one level.
  # Therefore, beginning with the bottom of the 3d lattice, and moving upwards
  # through the shape, each level will be represented by a jxk matrix appended to the bottom. 
  # total matrix size: (j*l)x(k)
  # It might be smart to do this by edgelist, then create a network object from edgelist, then export the adjacency.
  
  edge_list_length=6*j*k*l  
  edges<-matrix(0,
                nrow=edge_list_length,              # This matrix will be for the entire thing
                ncol=2)
  #=========================================================================
  target_list<-matrix(0,nrow=1,ncol=1)
  for (s in 1:l){         # for this layer
    for (q in 1:j){     # for this row
      for (r in 1:k){ # for this column
        target_list<-cbind(target_list,((s-1)*(j*k)+(q-1)*k+r))   #((i-1)*(j*k)+(m-1)*k+n) - (i-1)*(j*k) 
      } 
    }
  }
  target_list<-(as.matrix(target_list[1,2:(ncol(target_list))]))  #The columns were appended to above - this makes it a vertical vector of proper length
  
  #print(target_list)  
  #=========================================================================
  # This makes edgelist, the full lattice version (with world-wrap)
  for (i in 1:nrow(edges)){
    edges[(6*(i-1)%/%6+1):(6*(i-1)%/%6+6),1]<-target_list[((i-1)%/%6+1),1]
    if (i%%6==1){edges[i,2]<-target_list[((i-1)%/%6+1),1]-1}
    if (i%%6==2){edges[i,2]<-target_list[((i-1)%/%6+1),1]+1}
    if (i%%6==3){edges[i,2]<-target_list[((i-1)%/%6+1),1]-k}
    if (i%%6==4){edges[i,2]<-target_list[((i-1)%/%6+1),1]+k}
    if (i%%6==5){edges[i,2]<-target_list[((i-1)%/%6+1),1]-(j*k)}
    if (i%%6==0){edges[i,2]<-target_list[((i-1)%/%6+1),1]+(j*k)}
  }
  
  edges[,2]<-edges[,2]%%nrow(target_list)
  for(i in 1:nrow(edges)){if (edges[i,2]==0){edges[i,2]<-j*k*l}}  #turns zeroes into legitimate names of nodes?
  #=========================================================================
  # This section will prune connections that are made between layers
  # The end of each row needs to be pruned to the N+1 connection
  # the end of each column needs to be pruned of the N+

  #=========================================================================
  # here is where the edgelist gets turned into an adjacency
  bob<-network.initialize(j*k*l)
  network.edgelist(edges,bob)
  temp<-as.matrix(bob,type=adjacency)
  #for (i in 1:nrow(temp)){
  #   for (j in 1:nrow(temp)){
  #       if (temp[i,j]==1){temp[j,i]<-1}
  #   }
  #}
  
  return(temp)}

create.edgelist<-function(adj){ bob<-network(adj)
  temp<-as.matrix(bob,matrix.type="edgelist") #now temp[,1:2] are all edges
  #print(temp[1,1])
  return(temp)
}

create.random.network<-function(num_rows, direct=TRUE, loops_=FALSE, rate=.1,vnames=seq_len(round(10^num_rows))){
  #===============================================================================
#if directed is not exactly the phrase "TRUE" it initalizes symmetrically. 
#Same with 'loops' - only the exact phrase "TRUE" (in caps) creates some ones in the diagonals
#if dimnames has a list of length equal to number_of_vertices it puts those names into the matrix names, otherwise
#it automatically creates a lettered list.
#===============================================================================
x <-round(10^num_rows)

  temp <- matrix(nrow=x,ncol=x,dimnames=list(vnames,vnames))
  
  i <- 1
  while (i < (x + 1)){
    j <- ifelse(direct ==TRUE, 1,i)
    while (j < (x + 1)){
      ifelse (i==j,
              ifelse (loops_ == TRUE, 
                      temp[i,j] <- rbinom(1,1,rate),
                      temp[i,j]<-0),
              temp[i,j] <- rbinom(1,1,rate)) # this line randomly selects the connection/not connection
      
      if (direct != TRUE){temp[j,i] <- temp[i,j]}#this line sets the symmetry.
      
      j <- j + 1
    }
    i <- i + 1}
return(temp)}

create.ring.network<-function(num_rows){
  # this function attaches each node to the next two and previous two.
  x <- round(10^num_rows)
  temp<-matrix(0,nrow=x,ncol=x)
  temp[1,2:3]<-1
  temp[1,(x-1):x]<-1
  temp[2,1:4]<-c(1,0,1,1)
  temp[2,x]<-1
  for (i in 3:(x-2)){
    temp[i,(i-2):(i+2)]<-c(1,1,0,1,1)
  }
  
  #print(temp)
  temp[(x-1),(x-3):x]<-c(1,1,0,1)
  temp[(x-1),1]<-1
  temp[x,(x-2):(x-1)]<-1
  temp[x,1:2]<-1
return(temp)}

create.scalefree.network<-function(n=2,r=2.3){
  # First, we create a matrix with the same number of rows as df
  # Column 1 is used to store the probability 1/k^-r, as in the literature. 
  # This is re-normalized according to the discrete probabilities for 10^n nodes (so n=2 is 100 nodes)
  
  # Each node gets a random number in Column 2.
  # Then, for t random number, it checks subtracting the probability for each row till it reaches less than "0"
  # Meaning that for this random number probability, this is where it would be in the pdf
  #
  # Part 3 matches stubs left (Column 4) to generate an adjacency matrix
  # Since it sorted the nodes in order, it looks in all later rows of Column 4 which are non-zero. 
  # This becomes a list of linkable partners, from which a sample is taken of the appropriate size for that
  # node's degree distribution. Hubs are therefore filled before stubs. Need code for when last available nodes have unfillable stubs (already are linked)
  #
  #===========================================# 
  number_rows <- round(10^n)                  #
  temp<-matrix(0,nrow=number_rows,4)          # generates a matrix in which computations will be carried out
  adjacency_matrix<-matrix(0,                 # generates an empty adjacency matrix of the same size
                           nrow=number_rows,  # 
                           ncol=number_rows)  #
  
  #===========================================# 
  j<- 0
  for (i in 1:number_rows){j<-j+i^-r}         # finds the normalizing factor for this number of elements
  for (i in 1:number_rows){                   #
    temp[i,1]<-(i^-r)/j}                    # Normalizes the pdf to one
  
  temp<-assign_degree(temp)# For the purposes of debugging, this very important procedure is now a function
  if (sum(temp[,3]%%2==1)){temp[nrow(temp),3]<-temp[nrow(temp),3]+1}
  #
  #print(temp[,3])         # DEBUGGED TO HERE
  #===========================================# 
  temp[,4]<-temp[,3]                          # This will be where it keeps track of how many links it has left.
  barplot(temp[,3])                           # Graph for debugging
  # print(temp[,3])
  #
  # 
  #
  #
  #===========================================#
  # This section assigns link partners
  for (i in 1:number_rows){
    avail_to_link<-vector("list",0)             
    links_to_make<-temp[i,4]                # Stores the number of links in a variable
    #print(cbind(i,links_to_make))
    temp[i,4]<- 0                           # Then deletes it to avoid self-links
    for (k in 1:number_rows){
      if (temp[k,4]>0){
        avail_to_link<-cbind(avail_to_link,k)}}
    #print(c(length(avail_to_link),links_to_make))
    if(length(avail_to_link)>=links_to_make){
      link_partners<-sample(avail_to_link,links_to_make)} # IF POSSIBLE, takes links from 'next available' 
    else {
      #   Need to first make sure you get _enough_ samples.
      #   Then, for the list of numbers, ADD them to k, the row you are on, and then %% number_rows - in essence advanced 
      link_partners<-round(runif(links_to_make,1,num_rows),0)  # IF THAT RUNS OUT takes (degree) links from the entire network with even probability
      link_partners<-(link_partners+k)%%number_rows
    }
    link_partners<-as.matrix(link_partners)
    #print(link_partners)
    # 
    for (j in link_partners){
      temp[j,4]<-temp[j,4]-1
      adjacency_matrix[i,j]<-1            # Creates a link for that row to the appropriate partners. rowsum should equal degree distribution.
      adjacency_matrix[j,i]<-1            # For symmetry
    }
  }
  
  print(c(sum(adjacency_matrix),"links in this network"))
  return(adjacency_matrix)
}

create.scalefree2.network<-function(n=2,r=2.3){
  # First, we create a matrix with the same number of rows as df
  # In this version, we generate a random number for each node, then sort them large to small.
  # 
  
  # Each node gets a random number in Column 2.
  # Then, for t random number, it checks subtracting the probability for each row till it reaches less than "0"
  # Meaning that for this random number probability, this is where it would be in the pdf
  #
  # Part 3 matches stubs left (Column 4) to generate an adjacency matrix
  # Since it sorted the nodes in order, it looks in all later rows of Column 4 which are non-zero. 
  # This becomes a list of linkable partners, from which a sample is taken of the appropriate size for that
  # node's degree distribution. Hubs are therefore filled before stubs. Need code for when last available nodes have unfillable stubs (already are linked)
  #
  #===========================================# 
  number_rows <- round(10^n)                  #
  temp<-matrix(0,nrow=number_rows,1)          # generates a matrix in which computations will be carried out
  adjacency_matrix<-matrix(0,                 # generates an empty adjacency matrix of the same size
                           nrow=number_rows,  # 
                           ncol=number_rows)  #
  
  #===========================================# 
  
  # Each row will generate a random number evenly distributed between 
  temp[,1]<-round(10^(log10(runif(number_rows))/(-r)))
  temp[,1]<-sort(temp[,1],decreasing=TRUE)
  if (sum(temp[,1])%%2==1){temp[number_rows,1]<-temp[number_rows,1]+1}
  # print(temp[,1])
  barplot(temp[,1]) # this part is accurate - the PROJECTED number of links matches the netlogo
  
  
  # 
  #
  #
  #===========================================#
  # This section assigns link partners
  for (i in 1:number_rows){
    #print (i)
    avail_to_link<-vector("list",0)             
    links_to_make<-temp[i,1]                # Stores the number of links this particular row needs
    
    #print(cbind(i,links_to_make))
    temp[i,1]<- 0                           # Then deletes itself to avoid self-links
    #print(temp)
    for (k in 1:number_rows){
      if (temp[k,1]>0){
        avail_to_link<-cbind(avail_to_link,k)}}
    #print(c(length(avail_to_link),links_to_make))
    if(length(avail_to_link)>=links_to_make){
      link_partners<-sample(avail_to_link,links_to_make)} # IF POSSIBLE, takes links from 'next available' 
    else {
      print("warning: there were not enough available nodes to make links to")
      #   Need to first make sure you get _enough_ samples.
      #   Then, for the list of numbers, ADD them to k, the row you are on, and then %% number_rows - in essence advanced 
      #link_partners<-round(runif(links_to_make,1,num_rows),0)  # IF THAT RUNS OUT takes (degree) links from the entire network with even probability
      #link_partners<-(link_partners+k)%%number_rows
    }
    link_partners<-as.matrix(link_partners)
    #print(link_partners)
    # 
    for (j in link_partners){
      temp[j,1]<-temp[j,1]-1
      
      adjacency_matrix[i,j]<-1            # Creates a link for that row to the appropriate partners. rowsum should equal degree distribution.
      adjacency_matrix[j,i]<-1            # For symmetry
    }
  }
  
  print(c(sum(adjacency_matrix)/2,"links in this network"))
  #for (i in 1:nrow(adjacency_matrix)){
  #  temp[i,3]<-sum(adjacency_matrix[i,])
  #}
  #print (temp[,3])
  return(adjacency_matrix)
  
  # return(temp2)
}

create.smallworld.network<-function(num_rows,beta){
  # this function attaches each node to the next two and previous two.
  x <- round(10^num_rows)
  temp<-matrix(0,nrow=x,ncol=x)
  temp[1,2:3]<-1
  temp[1,(num_rows-1):num_rows]<-1
  temp[2,1:4]<-c(1,0,1,1)
  temp[2,num_rows]<-1
  
  for (i in 3:(num_rows-2)){
    temp[i,(i-2):(i+2)]<-c(1,1,0,1,1)
  }
  
  temp[(num_rows-1),(num_rows-3):num_rows]<-c(1,1,0,1)
  temp[(num_rows-1),1]<-1
  temp[num_rows,(num_rows-2):(num_rows-1)]<-1
  temp[num_rows,1:2]<-1
  
  # =====================================
  #
  #  This next section handles re-wiring
  #
  
  for (i in 1:nrow(temp)){
    avail_to_link<-vector("list",0)             
    if (rbinom(1,1,beta)> 0) {
      for (k in 1:num_rows){
        if (temp[i,k]>0){
          avail_to_link<-cbind(avail_to_link,k)}}
      print (avail_to_link)}
    
    # this section needs to rewire one of the node's connections with 
    # probability beta
  }
  return(temp)}

create.library.network<-function(num_rows){
  # This function will generate a theoretical library of num_rows elements. ___________________________________________
  # To do this, it will first generate a matrix of size num_rows
  # Then it will generate a set of hypernodes in such a way as to both partition the library
  # as well as a certain selection of hybrid relations which span those partitions. 
return(temp)}

create.membership.vector<-function(num_rows){
  x<-round(10^num_rows)
  temp<-matrix(1,nrow=x, ncol=1)
  return(temp)
}

create.onto.vector<-function(attr.vector, temp, layout.par){
  plot_nodes(attr.vector, layout.par)
  ids<-plot3d(attr.vector[,1:3], type="s", col=attr.vector[,7], rad=attr.vector[,8])
  temp2<-if(interactive()){
    selectpoints3d(ids["data"], multiple=TRUE, value=FALSE)
  }
  #print(temp2[,2])
  temp.vector<-matrix(0, nrow=nrow(temp), ncol=1)
  for(i in temp2[,2]){  # this makes a vector with 1s corresponding the involved nodes
    temp.vector[i,1]<-1
  }
  
  temp4<-apply(temp, 2, function(x)all(x==temp.vector))#tests for previous membership of this group 
  #print(temp4) debugger for checking if already been selected.
  if(any(temp4)==TRUE) 
  {print("this groups already exists")}
  else
  {temp<-cbind(temp,temp.vector)
  }
  
  
  #print(temp)
  
  return(temp)}

degree_scaler<-function(df,layout.par){
  # This function looks at the degree and calculates the relative radius for each node.
  # The node with the max hub should be ALMOST 0
  # nodes with degree = 2 are radius = 1
  # nodes with degree = 1 are 1.25
  # It stores the information in df[,4], the homogenous variable. 
  
  # Mathematically, it maps 
  radius<-layout.par["radius"]
  max_degree<-max(df[,6])
  for(i in 1:nrow(df)){
    if(df[i,6]!=1){
      df[i,5]<-radius*(1-log(df[i,6]-1)/log(max_degree))
    }
    else{
      df[i,5]<-1.1   # puts the pendulums here
    }
  }
  return(df)}

distances<-function(df){
  temp<-sqrt(df[,1]^2+df[,2]^2+df[,3]^2)
  return(temp)
}

edge_locations<-function(attr,edges){
  temp2<-matrix(0,nrow=3*nrow(edges),ncol=3)
  for(i in 1:nrow(edges)){
    #print(df[1,1:3])
    temp2[(3*i-2),1:3]<-c(attr[edges[i,1],1],attr[edges[i,1],2],attr[edges[i,1],3])
    temp2[(3*i-1),1:3]<-c(attr[edges[i,2],1],attr[edges[i,2],2],attr[edges[i,2],3])
    temp2[(3*i),1:3]<-NA
  }
  return(temp2)
}

extraction_degree_list<-function(df,layout.par,deg=FALSE){
  target_list<-vector("numeric",0)
  temp_hubs_list<-unique(unlist(df[,6])) #makes a unique list
  for (i in 1:length(temp_hubs_list)){ # checks each for above threshold
    if(temp_hubs_list[i]>layout.par["threshold"]){
      if(deg==FALSE){target_list<-cbind(target_list,i)}
      else {target_list<-cbind(target_list,temp_hubs_list[i])}
    }
  }
  return(target_list)      
}

geo_mean<-function(hood_list,attr,batch=TRUE,target_list,recolor=FALSE){
  # This function is a surface-sort
  # It takes each point and finds the geometric mean of that point's neighborhood 
  # (only implemented symmetric now), easy to modify later
  # If batch=FALSE it moves on, re-normalizes to the geometric mean of the entire network
  # iterations controls how many sets.
  # if batch=TRUE(default), it finds out where each one would go, sends them there, then re-normalizes.
  # Note: it temporarily borrows columns 5-8
  temp_locations_matrix<-matrix(0,nrow=nrow(attr),ncol=3)
  if(batch==FALSE){
    for (i in target_list){
      
      attr[i,1]<-mean(attr[hood_list[[i]],1])
      attr[i,2]<-mean(attr[hood_list[[i]],2])
      attr[i,3]<-mean(attr[hood_list[[i]],3])
    }
  }else   # the next section does the above as a batch
  {for (i in target_list){
    temp_locations_matrix[i,1]<-mean(attr[hood_list[[i]],1])
    temp_locations_matrix[i,2]<-mean(attr[hood_list[[i]],2])
    temp_locations_matrix[i,3]<-mean(attr[hood_list[[i]],3])
    }
  for (i in target_list){
    #print(i)
    attr[i,1:3]<-temp_locations_matrix[i,1:3]     # moves all nodes to their geometric mean simultaneously
    }
  }
  if (recolor==TRUE){recolor(attr,target_list)} #if you want, it can turn only the targets blue.
  return(attr)
}

get_neighborhoods<-function(nw){
  temp<-vector(length=network.size(nw))
  #print(nrow(adj))
  #print(length(temp))
  for (i in 1:network.size(nw)){
    temp[i]<-list(get.neighborhood(nw,i))
  }
  return(temp)
}

hub_extraction<-function(df,k){
  # This function simply returns the list of rows
  # which have df[,6]==k
  target_list<-vector("list",0)
  for (i in 1:nrow(df)){if(df[i,6]==k){target_list<-cbind(target_list,i)}
  }
return(target_list)}

hubs_list<-function(df,layout.par){
  target_list<-vector("numeric",0)
  for (i in 1:nrow(df)){
    if(df[i,6]>layout.par["threshold"]){
      target_list<-cbind(target_list,i)
    }
  }
  return(target_list)}

make.adjacency<-function(onto.vector){
  #this function takes the vector of N rows and Q columns
  # and creates an adjacency vector of N+Q
  N<-nrow(onto.vector)
  Q<-ncol(onto.vector)
  x<-N+Q
  print(x)
  temp<-matrix(0, nrow=x, ncol=x)
  temp[(N+1):x, 1:N]<-t(onto.vector)
  temp[1:N, (N+1):x]<-onto.vector
  return(temp)}

make.attr<-function(attr.vector,onto.attr.vector){
  N<-nrow(attr.vector)
  Q<-nrow(onto.attr.vector)
  x<-N+Q
  temp<-matrix(0,nrow=x,ncol=8)
  colnames(temp)<-c("X","Y","Z","W","R","degree","color","size")
  temp<-data.frame(temp)
  temp[1:N,]<-attr.vector
  temp[(N+1):x,]<-onto.attr.vector
  return(temp)}

make.groups<-function(attr.vector, temp, layout.par){
  plot_nodes(attr.vector, layout.par)
  ids<-plot3d(attr.vector[,1:3], type="s", col=attr.vector[,7], rad=attr.vector[,8])
  temp2<-if(interactive()){
    selectpoints3d(ids["data"], multiple=TRUE, value=FALSE)
  }
  #print(temp2[,2])
  temp.vector<-matrix(0, nrow=nrow(temp), ncol=1)
  for(i in temp2[,2]){  # this makes a vector with 1s corresponding the involved nodes
    temp.vector[i,1]<-1
  }
  
  temp4<-apply(temp, 2, function(x)all(x==temp.vector))#tests for previous membership of this group 
  #print(temp4) debugger for checking if already been selected.
  if(any(temp4)==TRUE) 
  {print("this groups already exists")}
  else
  {temp<-cbind(temp,temp.vector)
  }
}

network.layout.3drandsphere<-function(attr,edgelist,layout.par){
    #function takes the data frame
    #it assigns random numbers to the data.frame(like setup.3d)
    #it calls threedtwod with minimal=FALSE propagates the data-frame
    #with random numbers, etc. It also plots. Used as data.frame<-network.layout.3drandom(data.frame)
    temp<-matrix(0,nrow=nrow(attr),ncol=4)
    temp[,1]<-runif(nrow(attr),0,layout.par["radius"]) # Picks a random distance less than Radius
    temp[,2]<-runif(nrow(attr),0,2*pi)                   # Picks a random azimuth.
    temp[,3]<-runif(nrow(attr),-.5*pi,.5*pi)                  # Picks a random altitude
    temp[,4]<-temp[,1]*cos(temp[,3])                # stores the XY diagonal briefly.
    #print(temp)
    attr[,1]<-cos(temp[,2])*temp[,4]                  # X is r cos theta cos phi
    attr[,2]<-sin(temp[,2])*temp[,4]                  # y is r sin theta cos phi
    attr[,3]<-temp[,1]*sin(temp[,3])
    #df<-update_vertex_count(df,layout.par)
    #df[,7]<-color.degree.heatmap(df,heat=layout.par["heat"],invert=layout.par["invert"])
    clear3d()
    attr<-normalize(attr, 1:nrow(attr), TRUE)
    network_plot3d(attr,edgelist,layout.par)
    return(attr)
  }

network.layout.sphere<-function(df,layout.par,noisy=FALSE){
  #function takes the data frame
  #it assigns random numbers to the data.frame(like setup.3d)
  #it calls threedtwod with minimal=FALSE propagates the data-frame
  #with random numbers, etc. It also plots. Used as data.frame<-network.layout.3drandom(data.frame)
  r <- layout.par["Radius"]
  
  df[,1] <- runif(nrow(df),-1,1)          #The first number is any number lower than r
  df[,2] <- runif(nrow(df),-(r^2-df[,1]^2),(r^2-df[,1]^2))          # so select a second number less than that.
  df[,3]<- runif(nrow(df),-(r^2-df[,1]^2-df[,2]^2),(r^2-df[,1]^2-df[,2]^2))
  
  df[,3] <- ((rbinom(nrow(df),1,.5)-.5)/.5)*sqrt(r^2-df[,1]^2-df[,2]^2)
  df[,1:13]<-threedtwod(df[,1:13],layout.par) #this tool should take 13 columns of location info and adjust it for the 3d locations assigned above.
  
  bob<-network(joe[,15:(nrow(joe)+14)])
  #plot.network(bob,coord=df[,7:8],suppress.axes=FALSE,ylim=c(-2,2), xlim=c(-2,2))
  if(noisy==TRUE){return(df)}else{return(df[,12:13])}
}

network_plot3d<-function(attr,edgelist,layout.par="layout.par"){
  # This function will use 'plot3d' to display networks.
  # First, it plots the points as spheres. Radius of 
  # spheres can be related to the number of links (hubs are big)
  # The 'natural' sphere size is '3' or 1/20th of the plot.
  #df<-update_vertex_count(df,layout.par) #sets size according to degree distribution
  lines<-layout.par["lines"]
  #print(df[1:4,1:7])
  #df[,6]<-.5*df[,6] #re-scales to a nice size for radius = 1
  #resize(attr,layout.par)
  clear3d()
  plot3d(attr[,1:3],
         col=attr[,7],
         type="s",
         size=attr[,8],
         xlim=c(-layout.par["xlim"],layout.par["xlim"]),
         ylim=c(-layout.par["ylim"],layout.par["ylim"]),
         zlim=c(-layout.par["zlim"],layout.par["zlim"])
  )
  
  # Now that the points are graphed, we need to get an edgelist
  if(lines==TRUE){edges<-edge_locations(attr,edgelist)
                  lines3d(edges)}
  
}

network_plotnosize<-function(df,layout.par){
  # This function will use 'plot3d' to display networks.
  # First, it plots the points as spheres. Radius of 
  # spheres can be related to the number of links (hubs are big)
  # The 'natural' sphere size is '3' or 1/20th of the plot.
  #df<-update_vertex_count(df,layout.par) #sets size according to degree distribution
  lines<-layout.par["lines"]
  #print(df[1:4,1:7])
  #df[,6]<-.5*df[,6] #re-scales to a nice size for radius = 1
  maximum_node_size<-2
  minimum_node_size<-.35
  x<- sqrt(max(df[,6]))
  plot3d(df[,1:3],
         col=df[,7],
         type="s",
         size=1.4, 
         axes=layout.par["axes"],
         xlim=c(-layout.par["xlim"],layout.par["xlim"]),
         ylim=c(-layout.par["ylim"],layout.par["ylim"]),
         zlim=c(-layout.par["zlim"],layout.par["zlim"])
  )
  
  # Now that the points are graphed, we need to get an edgelist
  if(lines==TRUE){
    lines_nw<-lines_edge_list(df)
    lines3d(lines_nw[,])}
  #return(temp2)
}

normalize<-function(df,target_list,R3=TRUE,radius=layout.par["radius"]){
  # This function is used by both the affine transformation and layouts. If R3 = TRUE it uses the fourth column (normally homogenous coordinate)
  # to store the radius. When applied on the first three columns of df, it moves every point to a normalized sphere. It then restores the W component to 1
  # 
  # It is also used in the 3d->2d converter, where the w component is used to apply the Camera_matrix transform (by dividing by W, 
  # points are moved to different locations in YZ)
  # 
  for (i in target_list){
    if(R3==TRUE){df[i,4]<-sqrt(df[i,1]^2+df[i,2]^2+df[i,3]^2)}#
    df[i,1:3]<-df[i,1:3]/df[i,4]*radius
    #df[i,9]<-"red"
  }
  
  if(R3==TRUE){df[,4]<-1}
  
  return(df)
}

normalize_subjective<-function(df,target_list, layout.par){
  for (i in target_list){
    df[i,4]<-(sqrt(df[i,1]^2+df[i,2]^2+df[i,3]^2))/df[i,5]
    #dividing by df[i,5] has no effect if it's 1 - but if rescale_degree
    # has been run, then each node has a normalizing factor already stored there
    
    df[i,1:3]<-df[i,1:3]/df[i,4]*layout.par["radius"]*(            #normalizes them to the stored radius times the asisgned radius 
    df[i,6]
      )
  }
  df[,4]<-1  #Turns the 'w' component back into 1 when calculations are done
  return(df)
}

pendulums <-function(hood_list,attr,layout.par,normalize=FALSE){
  #This function is going to get a stubs-list for 
  # only pendulums - degree = 1
  # What it will do is add to their current location (assumes they share geography with
  # the node to which they are attached) a vector of length l - this should place pendulums 'around'
  #
  #    The normalize parameter either creates a halo of pendulums around the node, or it does this and then pushes 
  #    them to the exterior of the sphere 
  l<- layout.par["pincushion"]*layout.par["radius"]
  threshold<-layout.par["threshold"]
  layout.par["threshold"]<-1
  target_list<-stubs_list(attr,layout.par)            # This could be automated and handed off to this function
  #print(target_list)
  attr<-geo_mean(hood_list,attr,TRUE, target_list)
  #print(target_list)
  temp<-matrix(0,nrow=nrow(attr),ncol=3)
  temp2<-matrix(0,nrow=nrow(attr),ncol=3)
  temp[,1]<-runif(nrow(attr),0,360)                    # Picks a random azimuth.
  temp[,2]<-runif(nrow(attr),-90,90)                   # Picks a random altitude
  temp[,3]<-l*cos(temp[,2])                          # stores the XY diagonal briefly.
  #print(temp)
  temp2[,1]<-cos(temp[,1])*temp[,3]                  # X is r cos theta cos phi
  temp2[,2]<-sin(temp[,1])*temp[,3]                  # y is r sin theta cos phi
  temp2[,3]<-l*sin(temp[,2])
  #print(temp2)
  for (i in target_list){
    attr[i,7]<-"yellow"
    attr[i,1]<-attr[i,1]+temp2[i,1]
    attr[i,2]<-attr[i,2]+temp2[i,2]
    attr[i,3]<-attr[i,3]+temp2[i,3]
  }    
  layout.par["threshold"]<-threshold
  if (normalize==TRUE){
    attr<-normalize_subjective(attr,target_list,layout.par)
  }
  #network_plot3d(joe,layout.par)
  return(attr)
}

random_sphere<-function(attr,layout.par){
  
  print(nrow(attr))
  for(j in 1:3){attr[,j]<-runif(nrow(attr),-layout.par["radius"],layout.par["radius"])}          #The first number is any number lower than r
  attr[,4]<-1
  attr[,5]<-sqrt(attr[,1]^2+attr[,2]^2+attr[,3]^2)
  for (i in 1:nrow(attr)){
    attr[i,1]<-attr[i,1]/attr[i,5]
    attr[i,2]<-attr[i,2]/attr[i,5]
    attr[i,3]<-attr[i,3]/attr[i,5]
  }
return(attr)
  }

recenter<-function(attr){
  #This function takes a set of coordinates and re-centers them to their geometric mean
  center_x <- mean(attr[,1])
  center_y <- mean(attr[,2])
  center_z <- mean(attr[,3])
  
  attr[,1]<-attr[,1]-center_x
  attr[,2]<-attr[,2]-center_y
  attr[,3]<-attr[,3]-center_z
  #df<-normalize(df,TRUE)
  
  return(attr)
}

recolor<-function(df,target_list){
  df[,9]<-"red"
  for (i in target_list){
    df[i,9]<-"blue"
  }
}

rescale_radius<-function(df,target_list, layout.par){
  # This function changes df[,5] to a scaled amount via df[,6], the degree distribution
  # First, it updates the vertex count
  
  df<-update_vertex_count(df,layout.par)
  barplot(df[,6])
  # Next, it calculates once the spanning distance max(df[,6])-layout.par["threshold"]
  threshold<-layout.par["threshold"]
  #print(threshold)
  a<- max(df[,6])-threshold
  #print(a)
  df<-normalize(df,target_list)
  for (i in target_list){
    df[i,4]<-(1-(df[i,6]-threshold)/a)
    df[i,1:3]<-df[i,1:3]*df[i,4]
    df[,5]<-distances(df)}
  df[,4]<-1
  return(df)}

resize<-function(attr,layout.par){
  max_size<-layout.par["max"]
  min_size<-layout.par["min"]
  x<- sqrt(max(attr[,6]))
  for (i in 1:nrow(attr)){
    attr[i,8]<-(log10(attr[i,6])*(max_size-min_size)/(2*log10(x))+min_size)}
  return(attr)
}

rewire_ring_network<-function(df,beta=.05){
  # this function takes a ring network (or any adjacency matrix, really)
  # and randomly rewires every possible node with probability beta. 
  # Does it do this in order? 
  # Answer: it randomly checks each node +- 2 (k/2 if you allow larger ring networks) 
  
  for (i in 1:nrow(df)){
    for (j in ((i+nrow(df)-2)%%nrow(df):(i+nrow(df)-2)%%nrow(df))){}
  }
  
}

setup.3d<-function(num_rows,layout.par=layout.par){
  x <- round(10^num_rows)
  # Columns 1-3 are XYZ
  # Column 4 is 1 is the homogenous coordinate for affine transformations.
  # Column 5 are Radius
  # Column 6 is degree, used for size of the node. 
  # Column 7 is color, used for displaying a particular node being moved.
  # Column 8 is size, derived from degree
  
  temp<-matrix(0,nrow = x, ncol=8)
  temp<-random_sphere(temp,layout.par=layout.par)
  
  
  colnames(temp)<-c("X","Y","Z","W","R","degree","color","size")
  temp2<-data.frame(temp,row.names=row.names(temp))
  return(temp2)
}

setup.layout.par<-function(radius=1,iterations=5,threshold=10,axes=TRUE,xlim=1,ylim=1,zlim=1,lines=TRUE,batch=TRUE,heat=TRUE,invert=TRUE,pincushion=.2,min=.35,max=2,directed=FALSE){
  layout.par<-structure(
    c(
      radius,
      iterations,
      threshold,
      axes,
      radius,          #"xlim"  |
      radius,          #"ylim"  |<-- used for RGL graph 
      radius,          #"zlim"  |  Currently set up to use 'radius'
      lines,
      batch,
      heat,
      invert,
      pincushion,
      min,
      max,
      directed
    )
    ,
    .Names = c(
      "radius",
      "iterations",
      "threshold",
      "axes",
      "xlim",
      "ylim",
      "zlim",
      "lines",
      "batch",
      "heat",
      "invert",
      "pincushion",
      "min",
      "max",
      "directed"
    )
  )   #Pincushion is used to deal with pendulums - it says how far they should be placed from the node to which they are connected.
}

sort.ontology.sort<-function(attr, edges, biadj, layout.par, mod=0){
  # This function assumes the following items:
  # the standard attribute vector containing
  # X, Y, Z, 1, Radius, Degree, color
  # an attribute vector for the ontology nodes
  # a matrix of the ontologies onto the nodes
  
  # layout.par - useful for global changes like size, etc.
  # the modulo of how often to display the sort
  # an _ontology vector_ of size N objects x as many as the power set (but normally much smaller) of _groupings_ each node could be in.
  # in a properly formatted bipartite style matrix, this is the rows beyond the N elements
  # we need the number of elements to discern the ontologies in the augmented matrix from the 
  
  # The general method of the sort is to place the objects onto the sphere, and then place the hyper-nodes representing the ontogies
  # ontologies ARE/CAN BE forced to a radius based on the ordinality - 
  # the FULL ordinality is membership, ie all nodes, and is the center. 
  # Less ordinality is closer to the surface
  # having established the basic nodes + hypernodes, each group is sent to its geometric mean and re-normalized, in order of
  # all nodes first
  # low-ordinal groupings
  # higher ordinal groupings
  # recenter, renormalize
  
  lines<-layout.par["lines"]
  if (mod==0){mod<-layout.par["iterations"]+1}      # setting mod = 0 makes it not graph till the end.
  iterations<-layout.par["iterations"]
  
  
  # generate an adjacency
  # generate an edgelist
  # generate a combined attribute vector
  
  #print(adj)
  N<-nrow(attr)
  Q<-ncol(biadj)
  attr<-make.attr(attr, Q)                          # this takes the attribute vector and adds rows to match the extra ontologies. 
  x<-N+Q
  #print(attr)
  attr<-bipartite_vertex_setup(attr, biadj,layout.par)    # next, set degree for all 
  nw<-network(edges, directed = layout.par["directed"])
  hood_list<-get_neighborhoods(nw)                 # obtain the network neighbors of each element
  edges<-create.edgelist(adj)
  layout.par["lines"]<- FALSE
  attr[N+1,5]<-0                        # sets the radius on the membership node at center
  attr[1:N,5]<-1
  for(i in (N+2):x){attr[i,5]<-.2+(i-N)*.6/Q}
  
  for (i in 1:iterations){
    #print(i)
    attr<-geo_mean(hood_list,attr,TRUE,(N+1):x)                 # since this may be the first usage of a new ontology, send it to its middle
    attr<-geo_mean(hood_list,attr,TRUE,1:N)                     # all elements to their geometric mean
    attr<-normalize_subjective(attr,1:N,layout.par)                                   # then normalize
    
    attr<-geo_mean(hood_list,attr,TRUE,(N+1):x)                    # ontologies go to their geometric mean
    
    attr<-recenter(attr)                                        # necessary to recenter before normalizing or it all creeps to the side
    
    attr<-normalize_subjective(attr,1:x,layout.par)
    
    #attr<-normalize(attr,1:N)                                  # blows it out from the new center. In this first sort, they all move
    #attr<-geo_mean(hood_list, attr,TRUE,hub_list)                             # only hyperedges go to their geometric mean, OR use above, only network objects get normalized
    if (i%%mod==0){network_plot3d(attr,edges,layout.par)}
    #ani.pause()
  }
  layout.par["lines"]<-lines                                      #If wanted, turns the lines back on for the final graph.
  #attr<-pendulums(attr,layout.par)
  #clear3d()
  network_plot3d(attr,edges,layout.par)  
  return(attr)
}

sort.out.sort.in<-function(attr,edges,layout.par,mod=0){
  if (mod==0){mod<-layout.par["iterations"]+1}
  lines<-layout.par["lines"]
  iterations<-layout.par["iterations"]
  threshold<-layout.par["threshold"]
  nw<-network(edges, directed = layout.par["directed"])
  hood_list<-get_neighborhoods(nw)
  #hub_list<-hubs_list(df,layout.par)                                     # hub list creates a list of ALL hubs above threshold
  layout.par["threshold"]<-min(attr[,6])                                  # At the end of the iteration, only pendulums are normalized
  pendulums_list<-stubs_list(attr,layout.par)                         # in non-scale free, the smallest degree.
  layout.par["threshold"]<-11                                          #  in the middle of an iteration, it will re-normalize both pendulums and twos
  both_list<-stubs_list(attr,layout.par)                              # This is to help 'pull' the central hubs back outwards a little bit
  layout.par["threshold"]<-threshold                                      # puts it back so sizes are good
  unique_hub_list<-sort(unique(unlist(attr[,6])),TRUE)                # This line gets all the hub sizes
  unique_hub_list<-unique_hub_list[1:(length(unique_hub_list)-1)]     #Note - this is the list of possible SIZES, later used to call nodes of that degree
  # This eliminates the nodes of length 1 in scale-free networks, or the smallest degree
  layout.par["lines"]<- FALSE
  
  attr<-recenter(attr)
  attr<-normalize(attr,seq(1:nrow(attr)))                                 # All go to the surface of a sphere before iterations begin    
  for (i in 1:iterations){                                                #
    print(i)                                                            #
    attr<-normalize(attr,both_list)                                     # Takes pendulums and 2s and puts them on the outside, to 'pull' the other nodes outwards
    #==============================================================
    #  SORT-OUT - starts at max degree and moves in.
    #==============================================================
    for (k in unique_hub_list){                                         #
      hub_extract<-hub_extraction(attr,k)                             # Automate this - create one procedure to create a list of lists, ie nodes of that hub size
      #print(hub_extract)                                             #
      attr<-geo_mean(hood_list,attr,TRUE,hub_extract)                 # the list of rows with degree k go to their geometric mean. 
    }                                                               #
    attr<-pendulums(hood_list,attr,layout.par,normalize=TRUE)           # This takes each node to its hub's xyz coord, then blows them out to a normalized sphere
    attr<-recenter(attr)                                                #
    attr<-normalize(attr,both_list)                                     # blows it out from the new center. In this first sort, they all move
    
    #network_plot3d(attr,edges,layout.par)
    #==============================================================
    #  THIS IS WHERE THE SORT-IN WILL GO - starts at surface and sorts out.
    #==============================================================
    unique_hub_list2<-sort(unique_hub_list)                         # Reverses the order of the list
    for (k in unique_hub_list2){                                    # as before, gets the list of hubs with that degree
      hub_extract<-hub_extraction(attr,k)                             # 
      #print(hub_extract)                                             #
      attr<-geo_mean(hood_list,attr,TRUE,hub_extract)                 # the list of rows with degree k go to their geometric mean. 
    }
    #==============================================================
    if(i%%mod==0){network_plot3d(attr,edges,layout.par)}
    ani.pause()        
    #df<-normalize(df,pendulums_list)                                   # This is what was ruining the thing before - stubs list was most of nodes
    # this line is basically unneccesary, since the stubs haven't moved since they were normalized.
  }    
  layout.par["lines"]<-lines                                              #If wanted, turns the lines back on for the final graph.
  attr<-pendulums(hood_list,attr,layout.par,FALSE)                                    # This pendulums look makes pendulums star around their node for display
  clear3d()
  network_plot3d(attr,edges,layout.par)
  return(attr)
}

sort.radius.sort<-function(attr,edges,layout.par,mod=0){
  if (mod==0){mod<-layout.par["iterations"]+1}
  iterations<-layout.par["iterations"]
  lines<-layout.par["lines"]
  layout.par["lines"]<-FALSE
  nw<-network(edges, directed = layout.par["directed"])
  hood_list<-get_neighborhoods(nw)  #this takes advantage of the built in package which has a 'get neighborhood' function for networks
  # which can be generated from an edgelist. This is the new standard for moving a biadjacency matrix into a network without a huge augmented adjacency.
  attr<-degree_scaler(attr,layout.par)
  print("first it recenters and normalizes everyone to their proper radius")
  attr<-recenter(attr)
  attr<-normalize_subjective(attr,target_list=1:nrow(attr),layout.par)
  print("Then it moves everyone to their geometric mean - do in order by hubs?")
  for (i in 1:iterations){
    print(i)
    attr<-geo_mean(hood_list,attr,batch=TRUE,target_list=1:nrow(attr))
    attr<-recenter(attr)
    attr<-normalize_subjective(attr,target_list=1:nrow(attr),layout.par) 
    if (i%%mod==0){network_plot3d(attr,edges,layout.par)}
  }
  #df<-pendulums(df,layout.par,FALSE)
  layout.par["lines"]<-lines
  clear3d()
  network_plot3d(attr,edges,layout.par)
  
  return(attr)
}

sort.surface.hub<-function(attr,edges,layout.par,mod=0){
  # THIS FUNCTION SPECIFICALLY REQUIRES A MIDDLING 'THRESHOLD' TO SPLIT INTO STUBS AND HUBS. sUGGESTION: SQRT(MAX(DEGREE))
  threshold<-layout.par["threshold"]
  max_degree<-max(attr[,6])
  min_degree<-min(attr[,6])
  lines<-layout.par["lines"]
  if (mod==0){mod<-layout.par["iterations"]+1}      # setting mod = 0 makes it not graph till the end.
  #print (min_degree)
  #print (threshold)
  #print (max_degree)
  iterations<-layout.par["iterations"]
  nw<-network(edges, directed = layout.par["directed"])
  hood_list<-get_neighborhoods(nw)
  
  #==============================================================================
  # done with networks, now it's all mathematical operations on attribute tables.
  #==============================================================================
  
  hub_list<-hubs_list(attr,layout.par)
  stub_list<-stubs_list(attr,layout.par)
  print("the hubs in this network are:")
  print(hub_list)
    #print(hood_list)
  #extraction_degree_list<-extraction_degree_list(df,layout.par)  # this is now extraction_degree_list
  
  layout.par["lines"]<- FALSE
  
  #hub_rows_list<-vector("list",0)
  #for (k in extraction_degree_list){              # CHANGE THIS SO THAT A LONG LIST IS CREATED, THEN DO ALL AT ONCE
  #    hub_extract<-hub_extraction(df,k)
  #    hub_rows_list<-cbind(hub_rows_list,hub_extract)
  #}
  q<-nrow(attr)
  for (i in 1:iterations){
    print(i)
    attr<-geo_mean(hood_list,attr,TRUE,hub_list)                 # the list of rows above threshold go to geo-mean
    attr<-geo_mean(hood_list,attr,TRUE,stub_list)                              # the stubs go as a pack - this allows the hubs to have more 'weight'
    attr<-recenter(attr)                                          # necessary to recenter before normalizing or it all creeps to the side
    attr<-normalize(attr,stub_list)                       # blows it out from the new center. In this first sort, they all move
    attr<-geo_mean(hood_list,attr,TRUE,hub_list)                 # only hyperedges go to their geometric mean, OR use above, only network objects get normalized
    if (i%%mod==0){network_plot3d(attr,edges,layout.par)}
    #ani.pause()
  }
  layout.par["lines"]<-lines                                      #If wanted, turns the lines back on for the final graph.
  
  clear3d()
  network_plot3d(attr,edges,layout.par)  
  return(attr)
}

sort.surface.sort<-function(attr,edges,layout.par,mod=0){
  if (mod==0){mod<-layout.par["iterations"]+1}
  iterations<-layout.par["iterations"]
  lines<-layout.par["lines"]
  layout.par["lines"]<-FALSE
  nw<-network(edges, directed = layout.par["directed"])
  hood_list<-get_neighborhoods(nw)
  
  attr<-degree_scaler(attr,layout.par)
  print("first it recenters and normalizes everyone to their proper radius")
  attr<-recenter(attr)
  attr<-normalize_subjective(attr,target_list=1:nrow(attr),layout.par)
  print("Then it moves everyone to their geometric mean - do in order by hubs?")
  for (i in 1:iterations){
    print(i)
    attr<-geo_mean(hood_list,attr,batch=TRUE,target_list=1:nrow(attr))
    attr<-recenter(attr)
    attr<-normalize_subjective(attr,target_list=1:nrow(attr),layout.par) 
    if (i%%mod==0){network_plot3d(attr,edges,layout.par)}
  }
  #df<-pendulums(df,layout.par,FALSE)
  layout.par["lines"]<-lines
  clear3d()
  network_plot3d(attr,edges,layout.par)
  
  return(attr)
}

spiral<-function(attr, rings){
  x<-nrow(attr)
  # the total number of spirals determines the total arc - 2pi()*rings
  
  arc<-2*(pi)/(x) # the arc at each step is the number of elements split evenly across all the rotations
  #print(arc)
  for(i in 1:x){
    theta<- -arc/2 + i*arc
    #print(theta)
    attr[i,1]<- cos(rings*theta)*sin(theta/2)
    attr[i,2]<- sin(rings*theta)*sin(theta/2)
    attr[i,3]<- -cos(theta/2)
  } 
  return(attr)}

stubs_list<-function(df,layout.par){
  target_list<-vector("numeric",0)
  for (i in 1:nrow(df)){
    if(df[i,6]<=layout.par["threshold"]){
      target_list<-cbind(target_list,i)
    }
  }
  return(target_list)      
}

update_vertex_count<-function(attr,adj,layout.par){
  if(nrow(adj)!=nrow(attr)){print("Your attributes and adjacency matrices do not match in length.")}
  else{    
    
    x<-nrow(attr)
    radius<-layout.par["radius"]    
    for (i in 1:nrow(attr)){
      attr[i,6]<-sum(adj[i,1:x])
      attr[i,5]<-radius
      if (i%%20==0){print( i)}}
    attr<-resize(attr,layout.par)
  }
  temp<-sort(attr[,6],decreasing=TRUE)
  barplot(temp)
          #, add=TRUE)
  return(attr)
}
