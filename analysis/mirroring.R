# mirroring
# 
# Dependent on output from comm_realized.R for file cr_ver1
# 
# Make a new matrix that overlap the two networks "communication realized" and 
# "communication required" which return the following entries 
# (still have to figure out how to code the entries in R in a proper way):    
#  Mirroring type I: "communication realized" = 0, and "communication required" = 0   
#  Mirroring type II:     "communication realized" = 1, and "communication required" = 1                                  
#  Non-mirroring type I:  "communication realized" = 1, and "communication required" = 0 
#  Non-mirroring type II: "communication realized" = 0, and "communication required" = 1
#  
# For the mirroring matrix, we need to compare the two communication matrices.
# To do this, the nodes need to have a common set of names.
# The tech_interface (td) matrix right now has file ids (ID_rev) has nodes.
# Communication realized has originally developer names. 
#

# Test case: Mirroring for version 1 --------------------------------------

# We need td_net1 (tech_interface.R) and cr_ver1(comm_realized.R)
load('cr_ver1.RData')
load('td_netVer1.RData')

# inspect both elements
cr_ver1
# network objects with some dynamic attributes left
cr_ver1 %v% 'vertex.names'
# names as IDs from authorID file
tdn_net1
tdn_net1 %v% 'vertex.names'
# names as IDs from fileIDs
 
# assign names to vertext 
cr_ver1 %v% 'vertex.names2' <- cr_ver1 %v% 'vertex.names'
cr_ver1 %v% 'vertex.names' <- as.character(developers$developer)
cr_ver1 %e% 'active'

# add to tdn_net1 the developers that are part of the project but not included in v1
dev_tdn <- as.character(tdn_net1 %v% 'vertex.names')
# inddicate with FALSE if a developer is not in dev_tdn
developers$tdn <- as.character(developers$developer) %in% dev_tdn
add.vertices.network(td_test, nv = table(developers$tdn)[1]),
                     vattr= list(
                       'vertex.names'= 
                         as.character(developers[developers$tdn == FALSE, 1])
                       ))


# create a matrix for task interface (td) and realized communication (cr)
tdn_m <- as.sociomatrix((tdn_net1))

cr_m <- as.sociomatrix(cr_ver1, 'active')
cr_m[which(is.infinite(cr_m))] <- 0

