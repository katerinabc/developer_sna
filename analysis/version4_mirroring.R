# task 4 analysis

# clean your workspace first 
rm(list=ls())

# By runnin the next line of code, all the required data objects
# are created.
#source('ownership_developer_sna.R', echo=T)

# Load the cleaned and prepared data
load('clean_data.RData')

# This file contains functions created for this project
source('functions_developer_sna.R', echo=F)

# detach igraph package as it conflicts with ergm pacakge
detach("package:igraph", unload=TRUE)
library(statnet)
# Needed data:
# developer-file workflow: named: microtask
# file-folder ownership structure: names: fowner
# folder-developer networks: named: downer
# task dependencies: named: taskdep
# required communication between folder owners: named: reqcomm


# data --------------------------------------------------------------------

microtask_file <- DF[DF$ver == 4, c(1,2)]
microtask_folder <- DF[DF$ver == 4, c(1,42)]
microtask_owner <- DF[DF$ver == 4, c(1, 45)]
fowner <- DF[DF$ver == 4, c(2, 45)]
downer <- DF[DF$ver == 4, c(42,45)]
taskdep <- task4
reqcomm <- reqcommv4 # error in this file. only weights are attached

# create one set of nodes with names of working in v4 or not
authatt$author<-as.character(authatt$author)
authatt<- rbind(authatt, c(4, "not.modified", 22, 99, 99, 99))
authors_v4 <- authatt[,c(2,1)]
authors_v4$v4 <- authatt$ver == 4

# create networks ---------------------------------------------------------

# microtask_file is a 2 mode network (developer - file). Project this to a dev-dev network
head(microtask_file)
mt_file <- as.matrix(bipart_to_row_projection(microtask_file$author, microtask_file$Filename)[[2]])
# nodes = developer
# edges = number of times developers worked on same file
mtfi_net <- as.network(mt_file, directed=F, matrix.type='a', ignore.eval=F, names.eval='frequency')
mtfi_net %v% 'vertex.names'


# microtask_folder is a 2 mode network (developer - folder). Project this to a dev-dev network
head(microtask_folder)
mt_folder <- as.matrix(bipart_to_row_projection(microtask_folder$author, microtask_folder$folder_names)[[2]])
# nodes = developer
# edges = number of times  developers worked on files located in the same folder
mtfo_net <- as.network(mt_folder, directed=F, matrix.type='a', ignore.eval=F, names.eval='frequency')

head(microtask_owner)
mt_owner <- network(microtask_owner, directed=T, matrix.type='e')
# nodes = developers
# edges = number of times developers worked on a file owned by someone else
gplot(mt_owner, label=mt_owner%v%'vertex.names')

# downer is a 2 mode network (developer - folder). Project this to a dev-dev network
head(downer)
down <- bipart_to_row_projection(downer$new_owner, downer$folder_names)[[2]]
# Great check. Only self-loops
# nodes = developer
# edges = number of times 2 developers own the same folder.  

# make reqcomm into a network. it is already a 1 mode network (dev-dev)
reqc_net <- network(reqcomm, directed=T, matrix.type='e', ignore.eval=F, names.eval='weights')
# take 'not.modified' node out
delete.vertices(reqc_net, which(reqc_net%v%'vertex.names' == 'not.modified'))

# find developers who should be communicating, but are not in mtfo_net
matches <- as.vector(reqc_net%v%'vertex.names' == mtfo_net%v%'vertex.names')
required_developers <- unlist((reqc_net%v%'vertex.names'))
missing_developers <- required_developers[!matches]
add.vertices(reqc_net, nv = 3, vertex.names = missing_developers)
reqc_net %v% 'vertex.names'
reqc_net %v% 'not_in_mfo_net' <- 

# create empty network g ----------------------------------------------

# change format of authoratt from a long into a wide format

library(tidyr)
authattw <- spread(authatt, ver, author)

#Initialize a network object
g<-network.initialize(22)

# add vertex attributes
set.vertex.attribute(g, 'vertex.names', c(unique(as.character(authatt$ID_author)), '22'))
set.vertex.attribute(g, 'developers', c(unique(as.character(authatt$author)), 'not.modified'))
participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL
authors_v4 <- authatt[authatt$ver == 4,]
for (i in get.vertex.attribute(g, 'developers')){
  tmp_ver <- authatt[authatt$author == i, 1]
  if(4 %in% tmp_ver){present <- 1}else{present<-0}
  participants <- cbind(participants, present)
  
  if(4 %in% tmp_ver){tmp_job <- authors_v4[authors_v4$author == i, 4]}else{tmp_job<- 99}
  jobtitle <- cbind(jobtitle, tmp_job)
  
  if(4 %in% tmp_ver){tmp_loc <- authors_v4[authors_v4$author == i, 5]}else{tmp_loc<- 99}
  location <- cbind(location, tmp_loc)
  
  if(4 %in% tmp_ver){tmp_con <- authors_v4[authors_v4$author == i, 6]}else{tmp_con<- 99}
  contract <- cbind(contract, tmp_con)
  
}
set.vertex.attribute(g, 'ver4', t(participants)[,1])
set.vertex.attribute(g, 'jobtitle', t(jobtitle)[,1])
set.vertex.attribute(g, 'location', t(location)[,1])
set.vertex.attribute(g, 'contract', t(contract)[,1])


# copy the empty network and add required communication (technical --------

g_req <- g
# add edge values for required communication
# step 1: modifiy reqcomm names so that instead of the names it shows the ID numbers
# the network packages requirs vertex ids to be sequential numbers starting with 1
reqcomm2 <- reqcomm

# modify tail/sender
for (i in 1:length(reqcomm2$und_from_file_id)){ # could be coded easier
  tmp_dev <- reqcomm2$und_from_file_id[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  reqcomm2$und_from_file_id[i] <- tmp_id
}

# modify head/receiver
for (i in 1:length(reqcomm2$und_to_file_id)){ # could be coded easier
  tmp_dev <- reqcomm2$und_to_file_id[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  reqcomm2$und_to_file_id[i] <- tmp_id
}

# aggregate reqcomm so that for each edge and self-loop only 1 row
reqcomm2_sum<-aggregate(reqcomm2$weight, by=list(reqcomm2$und_from_file_id, reqcomm2$und_to_file_id), sum)

# add edges
add.edges(g_req, tail = reqcomm2_sum$Group.1, head = reqcomm2_sum$Group.2, names.eval = 'req_comm', vals.eval = reqcomm2_sum$x)
g_req


# copy the empty network and add file-coworking information ---------------

g_mt <- g

# add edges
head(microtask_file)

# replace developer name with vertex id
for (i in 1:length(microtask_file$author)){ # could be coded easier
  tmp_dev <- microtask_file$author[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  microtask_file$author[i] <- tmp_id
}

mt_file <- as.matrix(bipart_to_row_projection(microtask_file$author, microtask_file$Filename)[[2]])
# modify matrix into an edgelist. maybe via first creating a network
mtfi_net <- as.network(mt_file, directed=F, matrix.type='a', ignore.eval=F, names.eval='frequency')
# nodes = developer
# edges = number of times developers worked on same file
mtfi_el <- as.edgelist(mtfi_net, attrname = 'frequency')

add.edges(g_mt, mtfi_el[,1], mtfi_el[,2], names.eval = 'freq_collab', vals.eval = mtfi_el[,3])
g_mt


# Copy empty g and add edge ownership -------------------------------------

g_own <- g

# add edges
head(microtask_owner)

# replace developer[author] name with vertex id
for (i in 1:length(microtask_owner$author)){ # could be coded easier
  tmp_dev <- microtask_owner$author[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  microtask_owner$author[i] <- tmp_id
}

# replace developer[new_owner] name with vertex id
for (i in 1:length(microtask_owner$new_owner)){ # could be coded easier
  tmp_dev <- microtask_owner$new_owner[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  microtask_owner$new_owner[i] <- tmp_id
}

# aggregate microtask_owner to have one row per edge
library(dplyr)
mt_own_sum <- as.data.frame(microtask_owner %>% group_by(author, new_owner) %>% mutate(weight = n()))
mt_own_sum <- unique(mt_own_sum)

# modify matrix into an edgelist. maybe via first creating a network
mtown_net <- network(mt_own_sum, directed=T, matrix.type='e', ignore.eval=F, names.eval='freq_own')
# nodes = developer
# edges = number of times developers worked on same file
mtown_el <- as.edgelist(mtown_net, attrname = 'frequency')

add.edges(g_own, mtown_el[,1], mtown_el[,2], names.eval = 'freq_own', vals.eval = mtown_el[,3])
g_own

# descriptives ------------------------------------------------------------

# Number of develoers in version 4
length(unique(microtask$author))

# Barplot: Developer activity
ggplot(microtask, aes(x = author)) + geom_bar() + labs(title="Developer Activity in version 4", x = 'Developer') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

# barplot: Owners of folders
ggplot(downer, aes(x = new_owner)) + geom_bar() + labs(title="Folder Onwership in version 4", x = 'Developer') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

gplot(reqc_net, diag=F, label = reqc_net %v%'vertex.names', edge.lwd=log(reqc_net%e%'weights') )


# valued ergm - basic model -----------------------------------------------

# dependent variable = realized coordination btw develoeprs
# DV = mt_file
# 
# independent variables = required communication based on file dependencies

base <- ergm(mtfo_net ~ sum, response="frequency", reference=~Geometric)


# valued ergo - model building --------------------------------------------

m1 <- ergm(mtfo_net ~ sum 
           + edgecov(downer, form='sum')
           , response="frequency", reference=~Geometric)
