# Analyzing interaction in version 2 (previously coded as version 1.5)

# clean your workspace first 
rm(list=ls())

# By runnin the next line of code, all the required data objects
# are created. This is not necessary if you have a saved RData file (see line 11)
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


# some terms --------------------------------------------------------------

# zero inflation = when you have a sparse matrix with most people not interacting, but when
# two people are interacting the level of interaction is pretty high. control for this by
# adding the term nonzero()

# Edgecov


# data --------------------------------------------------------------------

microtask_file <- DF[DF$ver == 4, c(1,2)] # edgelist: author - file
microtask_folder <- DF[DF$ver == 4, c(1,42)] # edgelist: autor - folder
microtask_owner <- DF[DF$ver == 4, c(1, 45)] # edgelist: author - folder owner (FO)
fowner <- DF[DF$ver == 4, c(2, 45)] # edgelist: file - folder owner (FO)
downer <- DF[DF$ver == 4, c(42,45)] # edgelist: folder - folder owner (FO)
taskdep <- task_v2 # dataset: file id (sender), file id (receiver), weight dependency, software version (old), software version (new)
reqcomm <- reqcomm_v2 # edgelist: FO sender file, FO receiver file, weight dependency

# create one set of nodes with names of working in v2 or not

# complete developer information dataset
authatt$author<-as.character(authatt$author)
authatt<- rbind(authatt, c(4, "external_owner", 22, 99, 99, 99)) # add a row of entries for artificial developer
authors_v2 <- authatt[,c(2,1)]
authors_v2$v2 <- authatt$ver2 == 2 # add logical vector for membership
authatt$v2 <- authatt$ver2 == 2

# all developers, but only unique information (not version memebrship information)
authors_unique <- authatt[!duplicated(authatt$author),]

# authors working in version 2 only
authors_mbr_v2 <- authatt[authatt$ver2 == 2,]  


# create networks ---------------------------------------------------------

# The basis for most of the networks is a bipartite edgelist. This edgelist is sometimes weighted. 
# To transfrom the bipartite edgelist into a one mode network with only developers, the following 
# steps are done:
# bipartite_to_row_projection is a function (see file functions_developer_sna) that creates a 
# matrix based on the egelist. This function only works for non-weighted edgelist (frequency
# edgelist). The output of this function is transformed into a matrix.
# The matrix is transformed into a network with as.network.
# Network description are added as needed.

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
gplot(mtfo_net, label=mtfo_net%v%'vertex.names') # fig not for publication

head(microtask_owner)
mt_owner <- network(microtask_owner, directed=T, matrix.type='e')
# nodes = developers
# edges = number of times developers worked on a file owned by someone else
gplot(mt_owner, label=mt_owner%v%'vertex.names') # this draws the network. Figure is not for publication

# downer is a 2 mode network (developer - folder). 
head(downer)
down <- bipart_to_row_projection(downer$new_owner, downer$folder_names)[[2]]
# Great check. Only self-loops
# nodes = developer
# edges = number of times 2 developers own the same folder.  

# make reqcomm into a network. it is already a 1 mode network (dev-dev)
reqcomm <- as.data.frame(reqcomm)
reqcomm$V3 <- as.numeric(as.character(reqcomm$V3))

# summarize reqcomm per unique node-node pair.
library(dplyr)
reqcomm_grp <- reqcomm %>% group_by(sender_name, receiver_name) %>% summarize(weights = sum(V3))
 
reqc_net <- network(reqcomm_grp, directed=T, matrix.type='e', ignore.eval=F, names.eval='weights')
reqc_net
reqc_net %v%'vertex.names'
head(reqc_net %e%'weights')
# take 'not.modified' node out
#delete.vertices(reqc_net, which(reqc_net%v%'vertex.names' == 'not.modified'))
gplot(reqc_net, label=reqc_net%v%'vertex.names', edge.lwd = log(as.vector(reqc_net%e%'weights'))) # fig not for publication


# find developers who should be communicating, but are not in mtfo_net.
# Add these to the network
matches <- as.vector( (reqc_net%v%'vertex.names') %in% (mtfo_net%v%'vertex.names'))
required_developers <-(reqc_net%v%'vertex.names')
missing_developers <- required_developers[!matches]
add.vertices(mtfo_net, nv = length(missing_developers))
from <- (length(network.vertex.names(mtfo_net)) - (length(missing_developers))) +1
to <- length(network.vertex.names(mtfo_net))
network.vertex.names(mtfo_net)[from:to] <- missing_developers
network.vertex.names(mtfo_net)
reqc_net %v% 'not_in_mfo_net' <- matches

# create empty network g ----------------------------------------------

# change format of authoratt from a long into a wide format. In this way, every developer
# is mentioned once (one row per developer) with a new columns indicating if they are member 
# in a version

library(tidyr)
authattw <- spread(unique(authatt[,-1]), value = author, key = ver2)

nbr_dev <- length(unique(authatt$author)) # set the size of the network to the
# number of developers
 
#Initialize a network object
g<-network.initialize(nbr_dev)

unique(authatt$author) # check if artifical developer already part of list. 
# If not, run the next two lines that are commented out.
# set.vertex.attribute(g, 'vertex.names', c(unique(as.character(authatt$ID_author)), '22'))
# set.vertex.attribute(g, 'developers', c(unique(as.character(authatt$author)), 'external_owner'))

# add vertex attributes. Run the following two lines only if the external developer is included
# in the file authatt. If not, run the two lines above that are commented out. 
set.vertex.attribute(g, 'vertex.names', unique(as.character(authatt$ID_author)))
set.vertex.attribute(g, 'developers', unique(as.character(authatt$author)))

participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL

# in the following loop information for developers who are members of version 2 is stored in a 
# number of temporary files (all begnning with tmp_).
# If a developer is not member of the version the number 99 is added. 
for (i in get.vertex.attribute(g, 'developers')){
  print(i)
  tmp_ver <- authatt[authatt$author == i, 7]
  if(2 %in% tmp_ver){present <- 1}else{present<-0}
  participants <- cbind(participants, present)
  
  if(2 %in% tmp_ver){tmp_job <- authors_mbr_v2[authors_mbr_v2$author == i, 4]}else{tmp_job<- 99}
  jobtitle <- cbind(jobtitle, tmp_job)
  
  if(2 %in% tmp_ver){tmp_loc <- authors_mbr_v2[authors_mbr_v2$author == i, 5]}else{tmp_loc<- 99}
  location <- cbind(location, tmp_loc)
  
  if(2 %in% tmp_ver){tmp_con <- authors_mbr_v2[authors_mbr_v2$author == i, 6]}else{tmp_con<- 99}
  contract <- cbind(contract, tmp_con)
  
}
set.vertex.attribute(g, 'ver2', as.numeric(t(participants)[,1]))
set.vertex.attribute(g, 'jobtitle', as.numeric(t(jobtitle)[,1]))
set.vertex.attribute(g, 'location', as.numeric(t(location)[,1]))
set.vertex.attribute(g, 'contract', as.numeric(t(contract)[,1]))

# After running these lines of code you get an empty network g. It is empty because no edges
# are included. You can check this by typing g (the name of the empty network) in the console.
# The network g only includes the vertices (the 21 real developers and the 1 artificial one) 
# and their attributes. If you like to see the attributes (demographics) about the developers
# comment out the following line.

g %v% 'location' 
# to see other attributes use this command: network_name %v% attribute_name
# the 'v' between percent signs stands for vertex. Use e for edges. The attribute name needs 
# to be given as character. 


# copy the empty network and add required communication (technical --------

g_req <- g

# add edge values for required communication
# step 1: modifiy reqcomm names so that instead of the names it shows the ID numbers
# the network packages requirs vertex ids to be sequential numbers starting with 1
reqcomm2 <- reqcomm_grp

# the values in reqcomm2 need to be turned into character vectors and not factors. 
str(reqcomm2)
reqcomm2$sender_name <- as.character(reqcomm2$sender_name)
reqcomm2$receiver_name <- as.character(reqcomm2$receiver_name)

# modify tail/sender
reqcomm2_snd <- NULL
for (i in 1:length(reqcomm2$sender_name)){ # could be coded easier
  tmp_dev <- reqcomm2$sender_name[i]
  tmp_id <- authors_unique[authors_unique$author == tmp_dev, 3]
  print(paste("step:", i, "and developer id: ", tmp_id))
  #reqcomm2$sender_name[i] <- tmp_id
  reqcomm2_snd <- c(reqcomm2_snd, tmp_id)
}

# modify head/receiver
reqcomm2_rcv <- NULL
for (i in 1:length(reqcomm2$receiver_name)){ # could be coded easier
  tmp_dev <- reqcomm2$receiver_name[i]
  tmp_id <- authors_unique[authors_unique$author == tmp_dev, 3]
  #reqcomm2$receiver_name[i] <- tmp_id
  reqcomm2_rcv <- c(reqcomm2_rcv, tmp_id)
}

# aggregate reqcomm so that for each edge and self-loop only 1 row
#reqcomm2_sum<-aggregate(reqcomm2$weight, by=list(reqcomm2$und_from_file_id, reqcomm2$und_to_file_id), sum)

# add edges
add.edges(g_req, tail = reqcomm2_snd, head = reqcomm2_rcv, 
          names.eval = 'req_comm', vals.eval = reqcomm2$weights)
g_req


# copy the empty network and add file-coworking information ---------------

g_mt <- g

# add edges
head(microtask_file)

# replace developer name with vertex id
for (i in 1:length(microtask_file$author)){ # could be coded easier
  tmp_dev <- microtask_file$author[i]
  tmp_id <- authors_unique[authors_unique$author == tmp_dev, 3]
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
  tmp_id <- authors_unique[authors_unique$author == tmp_dev, 3]
  microtask_owner$author[i] <- tmp_id
}

# replace developer[new_owner] name with vertex id
for (i in 1:length(microtask_owner$new_owner)){ # could be coded easier
  tmp_dev <- microtask_owner$new_owner[i]
  tmp_id <- authors_unique[authors_unique$author == tmp_dev, 3]
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
mtown_el <- as.edgelist(mtown_net, attrname = 'freq_own')

add.edges(g_own, mtown_el[,1], mtown_el[,2], names.eval = 'freq_own', vals.eval = mtown_el[,3])
g_own



# create N2 ---------------------------------------------------------------

# The idea to build the N2 network came after the previous networks have been build. The code
# below is specifically for N2. This is not memory efficient, as networks used above could be
# used in the analysis below. 
#
# N2 is the network which combines interaction between domain ownership, and task 
# interdependencies
# developers own folders. each folder has a number of files. the folders a developer 
# owns make up the domain s/he owns. 
# 
# N2 is composed of 3 matrices: Owernship (MxN), task interdependencies (MxM), and the
# transpose of the ownership matrix
# 
# The owersnhip matrix shows which developer owns what file
# The task interdependences show what files (N) are technically linked to each other
# the transpose of the ownership matrix shows what files are owned by a person
# 
# The data set that shows the file interdependencies is task4. This does not contain file names
# but file ids. As the matrices (ownership and task interdependencies) neeed to be matched
# by rows and columns before they can be multiplied, fowner is not used. Fowner is a matrix
# containing the folder owners and the file names. 
# 
# step 1: multiply MxN matrix with a NxN matrix gets a MxN matric
# step 2: multiply MxN matrix from step 1 with NxM matrix 

# Checks before doing Step 1
domain_own <- DF[,c(45,7)] #new owner - ID-file_und. 
task4 <- DF2[DF2$file == '1.5',]
td <- task4

# when multiplying the ownership matrix w/ the task interdependencies, the columns
# (files) in the ownership matrix need to be in the same order than the files in the
# task owernship file

# let's see in the first 100 entries of ID_file_und in the domain_own file
head(domain_own$ID_File_und, 100)

# the function 'order' sorts the rows by the values in the ID_file_und column
# the assumption is that both files will have ascending file ids
domain_own <- domain_own[order(domain_own$ID_File_und),]
head(domain_own$ID_File_und, 100)

# Let's check the td file to make sure we have the same order
head(td, 10)

# for matrix multiplication, the number of columns in matrix 1 (ownership), need to be equal
# to the number of rows in matrix 2 (task inderdependencies)
dim(domain_own)
dim(td)
# two coding problems appear: First, domain_own is not yet a matrix. It's an edgelist. Second
# domain_own has (or will have) 11938 columns and td has 16527 rows. 

# DECISION: TURN DOMAIN_OWN TO A MATRIX, AND SAME FOR TD. THEN CHECK AGAIN. WHY THIS FIRST?
# in a matrix, every pair of entries is unique. this means that the rows in td should be unique
# and the columns in domain_own also

# edgelist to matrix. 
# domain_own is a 2 mode network (developer - file id). Project this to a dev-dev network
# head(domain_own)
# domain_own_developer <- as.matrix(bipart_to_row_projection(domain_own$new_owner, domain_own$ID_File_und)[[2]])
# domain_own_files <- as.matrix(bipart_to_col_projection(domain_own$new_owner, domain_own$ID_File_und)[[2]])

# weighted edgelist to sociomatrix --> KBC: after our meeting on Nov 16th 2018 we 
# discussed about including/excluding edge weights. The decision was to keep them in
# But for testing purposes, I'm just doing it right now without edge weights
# 
# TODO: INCLUDE EDGE WEIGHTS
# TODO: include time information
# 
#library(amen)
#td_sm <- el2sm(as.matrix(td[,-4])) # this works, but takes long and makes 
# my laptop crash

td_net <- network(td[,1:2],matrix.type="edgelist",directed=TRUE) 

# the next line is nonesense because domain_own is a 2 mode network. The goal of these
# next lines is to reduce td_net a list of vertex that are present in both files. 
domain_own_net <- network(domain_own, directed = F, matrix.type = 'edgelist')

td_net
domain_own_net
# td_net has 135895 vertices whereas domain_own_net has 6155

# workflow: 
# 1. find the common set of vertices
# 2. delete all vertices in td_net that are not in both sets

# the vector domain_own_net%v%'vertex.names' has the node names for the domain_own_network
# These are character ID, ie. numbers that are stored as characters. They also have a
# trailing white space (a white space between the " and the first number). This might 
# create problems when matching.
# Strip trailing white spaces from vertex names

library(stringr)
domain_own_net%v%'vertex.names' <- str_trim(as.character(domain_own_net%v%'vertex.names'))
# domain_own is the basis against which to match the other network
# shared_vertices provides the index of nodes in td_net to keep.
shared_vertices <- which(as.character(td_net%v%'vertex.names') %in% as.character(domain_own_net%v%'vertex.names'))

# delete.vertices(network, vertex IDs) deletes the vertices in the network. The vertex
# IDs are those given to the different nodes (the sequence at which they occur, not
# the file ID). shared_vertices contains the IDs of those that we want to keep. This
# needs to be turned around. 
# After further thinking, another approach is to induce a subgroup 
# (get.induceedSubgraph). That seems to be easier/more logical. 

td_net_sub <- get.inducedSubgraph(td_net, v = shared_vertices)
td_mat <- as.sociomatrix(td_net_sub)

idx <- which(domain_own$ID_File_und %in% as.character(td_net_sub%v%'vertex.names'))
test <- domain_own[idx,]

library(igraph) # detach afterwards
domain_own_bip <- graph.data.frame(domain_own, directed = F)
V(domain_own_bip)$type <- V(domain_own_bip)$name %in% domain_own[,2] #the second column of edges is TRUE type
domain_own_bip

idx <- which(as.character(td_net%v%'vertex.names') %in% as.character(V(domain_own_bip)$name))
# 5761 true's

# add to the index the position of developers. We want to keep them.
vertex_idx <-which(V(domain_own_bip)$name %in% c(as.character(V(domain_own_bip)$name)[1:18], idx))

# subset to keep only those vertices of type TRUE also present in td_net
domain_own_sub <- induced_subgraph(domain_own_bip, vertex_idx, 'auto')
# length is 5779. This includes the 18 developers. Without them it is 5761 file IDs
# 
# Transform bipartite network into affiliation matrix
domain_own_mat <- as_incidence_matrix(domain_own_sub)

# Now we have the two matrices: 
# Ownership (dataset: domain_own_mat) and task interdependencies (td_mat)

# Dimension check: for mat multiplication M1 (col) == M2 (col)
dim(domain_own_mat)
dim(td_mat)

# create the collaboration needed matrix (CN). This is the IV network
cn <-  (domain_own_mat %*% td_mat) %*% t(domain_own_mat)
dim(cn)


# collaboration realized matrix -------------------------------------------
# create the collaboration required matrix (CR). This is the DV network
# 
task_df <- DF[DF$ver == 4,c(1,7)]

# limit task_df to the file id's included in the CN network. We'll be using
# shared_vertices to subset task-df

task_df <- as.matrix(domain_own[domain_own$ID_File_und %in% shared_vertices,])
dim(task_df)
class(task_df) # making sure that the object is a matrix
mode(task_df) # making sure that the object is iin numeric mode. Not the case
# right now. task_df is in character mode. Further test this.
table(sapply(task_df, class))
# which makes sense because it is an edgelist....
# convert edgelist into matrix. I'm using the same code as above.

library(igraph)
task_df_bip <- graph.data.frame(task_df, directed = F)
V(task_df_bip)$type <- V(task_df_bip)$name %in% task_df[,2] #the second column of edges is TRUE type
task_df_bip
# Transform bipartite network into affiliation matrix
task_df_mat <- as_incidence_matrix(task_df_bip)

mode(task_df_mat)
cr <- task_df_mat %*% t(task_df_mat)
dim(cr)

# cr and cn do not have the same dimension. The following developers are not included
# in cr
dimnames(cn)[[1]][which(!dimnames(cn)[[1]] %in% dimnames(cr)[[1]])]

# we are going to add them as vertexes into the bipartiate graph created above. 
# Then we are transforming the bipatite network again into an incidence matrix
V(task_df_bip)$name 
V(task_df_bip)$name  <- stringr::str_trim(as.character(V(task_df_bip)$name)) #strip white spaces
task_df_bip <- add_vertices(task_df_bip, 5, 
                            name = dimnames(cn)[[1]][which(!dimnames(cn)[[1]] %in% dimnames(cr)[[1]])],
                            type = FALSE)
task_df_mat <- as_incidence_matrix(task_df_bip)
dim(task_df_mat)
detach("package:igraph", unload=TRUE)
mode(task_df_mat)
cr <- task_df_mat %*% t(task_df_mat)
dim(cr)

# Ok, we got cr and cn. For ERGM we need to make sure that the rownames in both 
# matrices match.
dimnames(cr)
dimnames(cn)

# Now we'll make sure the rows and columns in cr and cn are in the same order

cr = cr[match(row.names(cn),row.names(cr)), match(colnames(cn),colnames(cr))]

# Let's make sure (this will return 0 if we did it right):
which(rownames(cr) != row.names(cn))

# descriptives ------------------------------------------------------------

# Number of develoers in version 4
length(unique(microtask$author))

# Barplot: Developer activity in version 4
ggplot(DF[DF$ver==4,], aes(x = author)) + geom_bar() + labs(title="Developer activity in version 4",
                                                subtitle ='Number of events developer logged',
                                                x = 'Developer') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('nbr_events_per_develoepr_ver4.png')

# barplot: Owners of folders
ggplot(DF[DF$ver == 4,], aes(x = new_owner)) + geom_bar() + labs(title="Folder Onwership in version 4", 
                                                                 subtitle = 'Number of folder a developer owns',
                                                                 x = 'Developer') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('nbr_folders_per_develoepr_ver4.png')


ggplot(reshape2::melt(cr), aes(Var1, Var2, fill=log(value))) + geom_tile() + 
  labs('Realized collaboration between developers', x = 'Developers', y = 'Developers') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('realized_collab.png') 

ggplot(reshape2::melt(cn), aes(Var1, Var2, fill=log(value))) + geom_tile() + 
  labs('Needed collaboration between developers', x = 'Developers', y = 'Developers') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('needed.collab.png')

# network: Needed collaboration
library(ggraph)
ggraph(igraph::graph.adjacency(cn, mode='directed', weighted=T), layout='lgl') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_label(aes(label = name)) + 
  theme_graph()
igraph::write.graph(igraph::graph.adjacency(cn, mode='directed', weighted=T), 'needed.collab.graphml')

# network: Required collaboration
ggraph(igraph::graph.adjacency(cr, mode='undirected', weighted=T), layout='auto') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_label(aes(label = name)) + 
  theme_graph()
igraph::write.graph(igraph::graph.adjacency(cr, mode='undirected', weighted=T), 'realized.collab.graphml')

# the graphs can be made nicer. Let me know what you like to see.

# correlation between graphs

cr_cn_corr = qaptest(list(cr,cn), gcor)
cr_cn_corr <- netcancor(cr, cn, nullhyp = 'qap', reps = 10000)
summary(cr_cn_corr)
plot(cr_cn_corr$cdist)

# simple linear regression (multiple quadratic assignment procedure)

net.mod.1 <- netlm(cr, cn, reps=10000) # takes some time due to the high number of replication
summary(net.mod.1) 

# check in the summary the coefficient estimate. The result show that needed collaboration 
# has a negative relation to realized collaboration. The estimate is -0.099 with a p value of 0.06

# build statnet networks --------------------------------------------------

# To run ERGM the graphs need to be loaded as network objects from the statnet package

cr_net <- network (cr, directed=FALSE, ignore.eval=F, names.eval='weights')
cr_net
cr_net%e%'weights'

cn_net <- network(cn, directed = TRUE, ignore.eval = F, names.eval = 'weights')
cn_net
cn_net%e%'weights'

# Now we need to add individual attributes to the DV network. These are stored in
# network g created further up (see 'create empty network g' line 133)

g%v%'vertex.names'
g%v%'developers'
cr_net%v%'vertex.names'

# test if the developers are in the same order
g%v%'developers' == cr_net%v%'vertex.names'

# the order of developers in cr_net is not the same than in g. can't simply assign
# attributes. cr_net and cn_net are also missing some developers. 
# First add missing people

# check who is missing 
as.character(g%v%'developers')[which(!as.character(g%v%'developers') %in% as.character(cr_net%v%'vertex.names'))]

# add the extra 4 people
add.vertices(cr_net, 4)
add.vertices(cn_net, 4)
set.vertex.attribute(cr_net, 'vertex.names', value = as.character(g%v%'developers')[which(!as.character(g%v%'developers') %in% as.character(cr_net%v%'vertex.names'))],
                     v = 19:22)
set.vertex.attribute(cn_net, 'vertex.names', value = as.character(g%v%'developers')[which(!as.character(g%v%'developers') %in% as.character(cn_net%v%'vertex.names'))],
                     v = 19:22)
cr_net%v%'vertex.names'
cn_net%v%'vertex.names'


# order the matrices
cr = cr[match(row.names(g),row.names(cr)), match(colnames(g),colnames(cr))]
cn = cn[match(row.names(g),row.names(cn)), match(colnames(g),colnames(cn))]

# add attributes

participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL

# in the following loop information for developers who are members of version 2 is stored in a 
# number of temporary files (all begnning with tmp_).
# If a developer is not member of the version the number 99 is added. 
for (i in get.vertex.attribute(cr_net, 'vertex.names')){
  print(i)
  tmp_ver <- authatt[authatt$author == i, 7]
  if(2 %in% tmp_ver){present <- 1}else{present<-0}
  participants <- cbind(participants, present)
  
  if(2 %in% tmp_ver){tmp_job <- authors_mbr_v2[authors_mbr_v2$author == i, 4]}else{tmp_job<- 99}
  jobtitle <- cbind(jobtitle, tmp_job)
  
  if(2 %in% tmp_ver){tmp_loc <- authors_mbr_v2[authors_mbr_v2$author == i, 5]}else{tmp_loc<- 99}
  location <- cbind(location, tmp_loc)
  
  if(2 %in% tmp_ver){tmp_con <- authors_mbr_v2[authors_mbr_v2$author == i, 6]}else{tmp_con<- 99}
  contract <- cbind(contract, tmp_con)
  
}
set.vertex.attribute(cr_net, 'ver2', as.numeric(t(participants)[,1]))
set.vertex.attribute(cr_net, 'jobtitle', as.numeric(t(jobtitle)[,1]))
set.vertex.attribute(cr_net, 'location', as.numeric(t(location)[,1]))
set.vertex.attribute(cr_net, 'contract', as.numeric(t(contract)[,1]))

# strength of familiarity is how often two developers worked on a previous version together
# We will first calculate the affiliation matrix of develoepr - group membership
# This will be transformed into a one-mode matrix (developer-developer) where the cell indicates
# how often these two people worked on the same version
familiarity <- authatt[,c(2,1)]
familiarity <- familiarity[familiarity$ver < 5,]
library('Matrix')
A <- spMatrix(nrow=length(unique(familiarity$author)),
              ncol=length(unique(familiarity$ver)),
              i = as.numeric(factor(familiarity$author)),
              j = as.numeric(factor(familiarity$ver)),
              x = rep(1, length(as.numeric(familiarity$author))) )
row.names(A) <- levels(factor(familiarity$author))
colnames(A) <- levels(factor(familiarity$ver))
A

fam_dev <- tcrossprod(A)

fam_dev_net <- network(as.matrix(fam_dev), directed=F, ignore.eval=F, names.eval='weights')
fam_dev_net%e%'weights'

# messy graph
famdev <- ggraph(igraph::graph.adjacency(as.matrix(fam_dev), mode='undirected', weighted=T), layout='kk') + 
  geom_edge_link(aes(color = (weight), 
                     start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(2, 'mm')),
                 show.legend=T) + 
  geom_node_label(aes(label = name)) + 
  theme_graph()
ggsave('fam_developers.png', famdev)


# valued ergm - basic model -----------------------------------------------

# dependent variable = realized coordination btw develoeprs
# DV = mt_file
# 
# independent variables = required communication based on file dependencies

base <- ergm(cr_net ~ sum, response="weights", reference=~Geometric)


# valued ergo - model building --------------------------------------------

ggplot(as.data.frame(cr_net%e%'weights'), aes(x =cr_net %e% "weights")) + geom_bar() + 
  labs(title="Distribution of Collaboration values", x = 'Frequency of Collaboration', y = 'Count')

ggplot(as.data.frame(cn_net %e% "weights"), aes(x =cn_net %e% "weights")) + 
  geom_bar() + 
  labs(title="Distribution of needed communication values", 
       x = 'Frequency of Needed Communication', 
       y = 'Count')
hist(g_req %e% "req_comm")

m1a <- ergm(cr_net ~ sum + nonzero()
           + edgecov(cn_net, attrname = 'weights', form='sum')
           , response="weights", reference=~Geometric)
mcmc.diagnostics(m1a) # look at the generated plots. For the left side you want to see kinda straight
# lines, no upward or downward trend. For the right ones you want to see normal curves.
# If you dont' get that, you can't trust the results. The MCMC simulation isn't stable. 
summary(m1a)

m1b <- ergm(cr_net ~ sum + nonzero()
+ edgecov(fam_dev_net, attrname = 'weights', form='sum')
, response="weights", reference=~Geometric)
mcmc.diagnostics(m1b)
summary(m1b)

m2 <- ergm(cr_net ~ sum + nonzero()
              #+ edgecov(fam_dev_net, attrname = 'weights', form='sum')
              + edgecov(cn_net, attrname = 'weights', form = 'sum')
              # attributes 
              + nodematch('jobtitle')
              #+ nodematch('location') 
              #+ nodematch('contract')
              #+ nodefactor('jobtitle')
              #+ nodesqrtcovar(center=T) # individual tendency to work
              # + transitiveweights("min","max","min") 
              # + cyclicalweights("min","max","min")
              
              , response="weights", reference=~Geometric)
mcmc.diagnostics(m2)
summary(m2)

# other terms to include:
# homophily hypothesis
# nodematch('jobtitle'), nodematch('location'), nodematch('contract')
# nodefactor('jobtitle') # 
# nodesqrtcovar(center=T) # individual tendency to work
# transitiveweights("min","max","min") 
# cyclicalweights("min","max","min")

# add control for the MCMC chain
# , control = control.ergm(MCMC.samplesize = 5000,
                                    # MCMC.interval = 2024,
                                    # MCMC.burnin = 50000,
                                    # MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                    # )


# previous notes ----------------------------------------------------------


# converged byt lots of NaN (zero standard deviation) for nonzero nodesqrtcovar, transweight
# atleast, edgeoc req_comm
# 
# based on http://mailman13.u.washington.edu/pipermail/statnet_help/2017/002470.html
# the zero could be because some of the relationships with the effects has 0.
# This is not detected in valued ergm as a sum could be 0.
# this could be because no one as values 1st above the mean --> 7 edge values
# are 1 sd above mean
# atleast(mean_collab + sd_collab)
#+ equalto(max(g_mt%e%'freq_collab', tolerance = sd_collab))


m9.1 <- ergm(g_mt ~ sum# + nonzero()
           + nodematch('jobtitle')
           
           + transitiveweights("min","max","min") 
           
           + edgecov(g_own, attrname = 'freq_own', form='sum')
           + edgecov(g_req, attrname = 'req_comm', form='sum')
           
           , response="freq_collab", reference=~Poisson
           , control = control.ergm(#MCMC.samplesize = 5000,
             #MCMC.burnin = 50000,
             #MCMC.interval = 2024,
             MCMC.prop.weights='0inflated' # to control for skweded degree distribution
             
           ))
mcmc.diagnostics(m9.1)
# model 9.1 isn't too bad. Some wobblyness in the chains, but that might (!) be eliminated 
# with opimizing the mcmc parameters
summary(m9.1)

# explanations
exp(1.45)
# developers have a likelihood of 4.26 to work on the same file
exp(-0.212)
# if developers have the same job title they are 0.808 less likely to work on the same file
# developers who have the same job title have a likelihood of 3.45( 4.26 - 0.808) to work on the same file
exp(-1.22) #transitive weights
# developers who 
exp(-.07)
# developers who work on files owned by someone else, are 0.93 less likely to interact with the owner
# 
# Conway–Maxwell–Poisson ? Fractional moments to take into account over/underdispersion of counts
# 



