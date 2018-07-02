#==========================================================================
#  This is the section of sorts. Each one returns the dataframe with the actual columns changed
#  This of course could be shunted to another file. 
#  Normalize           - all objects are normalized to distance 1 from the vector 
#  sort.surface.Sort        - all objects go in a batch to their geometric mean, then normalize to a center at the geo-metric mean of the entire network.
#  sort.surface.Hub         - all objects geo-mean, then those below threshold are normalized. Hubs geo-mean again. 
#  Sort.out.sort.in    - Cycling through 1:MaxK, nodes of degree K are moved to their geo-metric mean, giving them a 'turn' to modify the relationship
#  sort.radius.sort    - For a given threshold, o
#  3drandsphere        - a good randomizer for starting iterations
#==========================================================================
#
#
#==========================================================================
network_plot3d(attr.vector2,edgelist,layout.par)
attr.vector<-sort.surface.sort(attr.vector, edgelist, layout.par, mod=1)
attr.vector2<-sort.surface.hub( attr.vector, edgelist, layout.par, mod=1)
attr.vector2<-sort.out.sort.in(attr.vector, edgelist, layout.par, mod=1)
attr.vector<-sort.radius.sort( attr.vector, edgelist, layout.par, mod=1)
#attr.vector<-sort.surface.hub_moving_threshold(adjacency.matrix,attr.vector,edgelist,layout.par,mod=0)
attr.vector<-sort.ontology.sort( adjacency.matrix, attr.vector, edgelist, layout.par, mod=1,52) 
#this is specifically for a 71 node network of cards
# the first 52 items are in group cards, the next 13 are in group 'sets', the next four in 'suits', and the last two are in group 'categories'
layout.par["iterations"]<-1
layout.par["radius"]


layout.par["threshold"]<-100
attr.vector<-network.layout.3drandsphere(attr.vector,edgelist,layout.par)
clear3d()
attr.vector
temp<-sort.radius.sort(adjacency.matrix,attr.vector,edgelist,layout.par,mod=1) 

layout.par["iterations"]<-100
layout.par["iterations"]<-20

layout.par["lines"]<-TRUE

clear3d()
attr.vector<- network.layout.3drandsphere(attr.vector,edgelist,layout.par)
attr.vector<- rescale_radius(attr.vector,seq(1:nrow(attr.vector)),layout.par)







#==========================================================================
#==========================================================================
# DEBUGGING SECTION
# This section is a step by step through most of the procedures that these sorts use.

layout.par["iterations"]<-1 # For debugging, make sure to set to one

# joe<-rescale_radius(joe,seq(1:nrow(joe)),layout.par) # This will soon be run ONLY in geo-mean2 and new-sort, which use degree count to assign a radius

joe<-normalize_subjective(joe,seq(1:nrow(joe)),layout.par) # If joe[,5] has been set with rescale-radius, it will use that. Otherwise, 1
microbenchmark(joe<-geo_mean(joe,batch=TRUE,target_list=seq(1:nrow(joe))))
joe<-recenter(joe)
joe<-normalize_subjective(joe,seq(1:nrow(joe)),layout.par) # This redudancy completes the display loop
debug_hub_list<-hubs_list(joe,layout.par)                  # This section helps debug sort.surface.hub
network_plot3d.debug_pendulums_only(joe,debug_hub_list,layout.par) 
layout.par["threshold"]<-1 #for getting pendulums
debug_stub_list<-stubs_list(joe3,layout.par)

joe3<-geo_mean(joe3,batch=TRUE,target_list=debug_stub_list)
joe3<-normalize_subjective(joe3, target_list=debug_hub_list,layout.par)
joe2<-pendulums(joe,layout.par)
network_plot3d.debug_pendulums_only(joe,joe2,debug_stub_list,layout.par)
layout.par["threshold"]<-sqrt(max(joe3[,6]))
network_plot3d(joe,layout.par)
# Coming up: debugging dual-radius sort and new-sort
#
