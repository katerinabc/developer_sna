# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)


# load data ---------------------------------------------------------------

dv_master <- read_csv("communication_realized_edge_weight_by_version.csv")
ivnet_master <- read_csv("crequired_el.csv")
ivatt_master2 <- read_csv("authatt.csv")

ivatt_master <- unique(ivatt_master2[,-c(1:2, 8)])


# transform csv's into network objects ------------------------------------

names(dv_master)
dv_master
dv <- network(dv_master[,-3], directed=FALSE, ignore.eval=F, names.eval='weights', matrix.type = 'edgelist')


# ivnet_master needs to be summarized to show how often a link exists between two folder owners
ivnet_master <- ivnet_master %>% group_by(folder_owner.x, folder_owner.y) %>% count()
ivnet <- network(ivnet_master, directed=FALSE, ignore.eval=F, names.eval='weights', matrix.type = 'edgelist')


# add missing names
mismatch <- match(as.character(ivnet%v%'vertex.names'), as.character(dv%v%'vertex.names'))
missingdev <- as.character(dv%v%'vertex.names')[-mismatch]

add.vertices(ivnet, 7)

set.vertex.attribute(ivnet, 'vertex.names', value = missingdev, v = which(is.na(ivnet%v%'vertex.names')))

# add attributes to DV ----------------------------------------------------

# To make sure that the attributes are matched to the correct developer, a loop is created. This loop first gets the 
# developer name from the DV network. Based on that the correct job title, location and contract is extracted.  
participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL

# in the following loop information for developers who are members of version 2 is stored in a 
# number of temporary files (all begnning with tmp_).
# If a developer is not member of the version the number 99 is added. 
for (i in network::get.vertex.attribute(dv, 'vertex.names')){
  print(i)
  
  tmp_job <- pull(ivatt_master[ivatt_master$author == i, 3])
  print(tmp_job)
  jobtitle <- cbind(jobtitle, tmp_job)
  
  tmp_loc <- pull(ivatt_master[ivatt_master$author == i, 4])
  print(tmp_loc)
  location <- cbind(location, tmp_loc)
  
  tmp_con <- pull(ivatt_master[ivatt_master$author == i, 5])
  print(tmp_con)
  contract <- cbind(contract, tmp_con)
}

#network::set.vertex.attribute(dv, 'ver2', as.numeric(t(participants)[,1]))
network::set.vertex.attribute(dv, 'jobtitle', as.numeric(t(jobtitle)[,1]))
network::set.vertex.attribute(dv, 'location', as.numeric(t(location)[,1]))
network::set.vertex.attribute(dv, 'contract', as.numeric(t(contract)[,1]))


# create familiarity network ----------------------------------------------


# strength of familiarity is how often two developers worked on a previous version together
# We will first calculate the affiliation matrix of develoepr - group membership
# This will be transformed into a one-mode matrix (developer-developer) where the cell indicates
# how often these two people worked on the same version
familiarity <- ivatt_master2[,c(3, 8)]
A <- Matrix::spMatrix(
  nrow=length(unique(familiarity$author)),
  ncol=length(unique(familiarity$ver2)),
  i = as.numeric(factor(familiarity$author)),
  j = as.numeric(factor(familiarity$ver2)),
  x = rep(1, length(as.numeric(familiarity$author)))
)
row.names(A) <- levels(factor(familiarity$author))
colnames(A) <- levels(factor(familiarity$ver2))

fam_dev <- tcrossprod(as.matrix(A))

fam_dev_net <- network(as.matrix(fam_dev), directed=F, ignore.eval=F, names.eval='weights')
