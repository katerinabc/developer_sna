# create technical interface
#  this was previously called required communication. 
#  A communication is required if file A has a technical dependency in file B.
#  If file A is modified, file B needs to be adapted. 
# 
# clean your workspace first 
rm(list=ls())

# instructions ------------------------------------------------------------

# created matrix: tech_interface

# Needed data:
# task_dependencies(td), containing 
# file id (sender), file id (receiver), weight dependency, software version (old), software version (new)
# 
# 
# 
# developer-file workflow: named: microtask
# file-folder ownership structure: names: fowner
# folder-developer networks: named: downer
# task dependencies: named: taskdep
# required communication between folder owners: named: tech_interface

# preamble ----------------------------------------------------------------

# This file contains functions created for this project
source('functions_developer_sna.R', echo=F)
source('data_import.R', echo=F)
library(statnet)
library(tidyverse)

df <- read_csv('df_modified.csv')
td <- DF2 # created in data_import.R



# step 1: indicarte active and non active ties ----------------------------------------------


#wrk_ver <- 2 # this is a place holder to save on writing. This indicates what version 
# you are working on

# subset DF per version
#dfv2 <- DF %>% filter(DF$ver == wrk_ver)

# focus on activated ties in td. 
# A tie is active if either the sender or receiver file in td is also in df

# add a column to td to indicate if the sender or receiver file is in the version
active_snd <- NULL
active_rc <- NULL
for (i in 1:nrow(td)){
  tmp_version <- td[i, 5]
  tmp_td_sender <- td[i, 1]
  tmp_td_receiver <- td[i, 2]
  
  tmp_df <- df[df$ver == tmp_version,]
  if(tmp_td_sender %in% df$ID_rev){tmp_active_snd <- 1}else{tmp_active_snd <-0}
  if(tmp_td_receiver %in% df$ID_rev){tmp_active_rc <- 1}else{tmp_active_rc <-0}
  
  active_snd <- c(active_snd, tmp_active_snd)
  active_rc <- c(active_rc, tmp_active_rc)
}
save.image(active_snd, 'active_snd.Rdata')
save.image(active_rc, 'active_rc.Rdata')
# add active_snd and active_rc to td

td <- cbind(td, active_snd, active_rc)
td$active_raw <- td$active_snd + td$active_rc # a tie is active if the snd or rc file is modified during that version 
table(td$active_raw)
names(td)
td$active <- td$active_raw
td[td$active_raw == 2, which(names(td) == 'active')] <- 1 # transform the 2 into 1. 2 
table(td$active)

# step 2: create network of required commnication per version -----------------------------------------------------------------

# as the network should be created per version and per time period (e.g., week), this could be a list of networks
# Potential problem: list of networks too big? 
# The nodes of on-active ties need to be present.
# 
# 3/10/19: code copied from version4_mirroring

# td_net is the task dependencies based on file ID. this is a 1 mode network.
# this creates one huge network using all of the task interdepencies (the complete file*file)
# the following edge data is being added:
# weight: the strenght of the technical dependency
# version: the version number in which two files were dependent on each other
# if the tie is active or not
td_net <- network(td[,c(1:3, 5, 9)],matrix.type="edgelist",directed=TRUE, 
                  ignore.eval=FALSE) 

td_net
# step 3:Get network per version ------------------------------------------

table(td_net%e%'ver') # if you run this line you see the number of edges per version

# to subset td_net you need to indicate the edge ids you want to focus on
# You don't need to know the edge ids (eids), but can include a command to filter edges
# based on edge attribute.
# to filer by edgge attribute you type in network_name%e%'edge attribute' == 'filter valued'
# the 'e' between percentage signs tells R to look for edges. To look at verticees (nodes)
# you would type %v%.
# The edge (or node) attributes follows the second per centage sign. This is always in 
# apostrophes. 
# Then you tell R what this attribute should be equal to (==), greater (>) or smaller (<) etc.

td_net1 <- get.inducedSubgraph(td_net,eid=which(td_net%e%'ver'==1))
td_net1

# there is no sense in drawing this network in R. it has 5720 active edges. 
# Drawing would be better in Gephi

# subsetting td_net can take some time. You can save the resulting object so that next time 
# you don't have to subset it again, but just need to load the RData object.
save.image('td_netVer1.RData')


# create the network communication required -------------------------------
# the required communication network is the td_net network. 
# No further modification is necessary. 
