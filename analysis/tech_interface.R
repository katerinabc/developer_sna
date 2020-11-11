#  create technical interface
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

df <- read_csv('df_modified.csv') #created in onwership_developer_sna.R
head(df)
td <- DF2 # created in data_import.R
head(td)
dim(td)
# DF2 merges the different file-to-file csv files into one huge data set.
# It then adds a column with the new version numbers
# 
# df is the transformed deverlop-by-file.csv file. It contains information about who owns the file. 


# step 1: indicates active and non active ties ----------------------------------------------

# focus on activated ties in td. 
# A tie is active if either the sender or receiver file in td is also in df.
# df is the dataset with all changes developers have done. 

# The following loop goes through td and checks if the file is in df. The loop begins with row 1 (i = 1)
# If yes, then it's active. 
# If the file is active, the temporary output is 1, unless 0. This temporary output is stored in a 
# placeholder vector. With every loop a new value is added to the placeholder vector.
# Once this is done, the loop goes to the next row
# This is done for the sender and receiver file. 
###################################################
# RUNNING THE LOOP TAKES SEVERAL HOURS            #
# #################################################
active_snd <- NULL # placeholder vector to save sender output of the loop
active_rc <- NULL  # placeholder vector to save receiver output of the loop
for (i in 1:nrow(td)){ # start with row 1 and continue until the last row of td (227429)
#for (i in 1:100){ # this is a shorter loop to test out the code
  tmp_version <- td[i, 5] # retrieve the version number for row i in td. Column 5 = version number
  tmp_td_sender <- td[i, 1] # retrieve the sender file id 
  tmp_td_receiver <- td[i, 2] #retrieve the receiver file it

  # subset df using an index for rows and columns. The subset requires row and column numbers
  # row index: get the row numbers in df where the version number is the same as stored in tmp_version
  # column index: the column which is named ID_rev 
  tmp_df <- df[df$ver == tmp_version, which(names(df) == 'ID_rev')]
  # If the retrieved sender file ID is in df, then set active_snd to 1, unless to 0
  if(tmp_td_sender %in% df$ID_rev){tmp_active_snd <- 1}else{tmp_active_snd <-0}
  # If the retrieved receiver file ID is in df, then set active_rc to 1, unless to 0
  if(tmp_td_receiver %in% df$ID_rev){tmp_active_rc <- 1}else{tmp_active_rc <-0}

  # combine the currently output of active sender (tmp_active_snd) with the already existing 
  # information about active sender file IDs (active_snd). 
  # Same logic for receiver files
  active_snd <- c(active_snd, tmp_active_snd)
  active_rc <- c(active_rc, tmp_active_rc)
}
# as running the loop takes so long, save the output in two Rdata objects.
save(active_snd, active_rc, file ='active_snd_rc.Rdata') 
saveRDS(active_snd, 'active_snd.rds')
saveRDS(active_rc, 'active_rc.rds')

# add active_snd and active_rc to td

# combine active_snd and active_rc. 
# Remember active_snd and active_rc is a column of 1 (file active) and 0 (file not active).
# Active means the file was modified in a given version. 
td <- cbind(td, active_snd, active_rc)

# the pair of files are considered active if the sender file or receiver file were modified in a given version.
# THus sender OR reeiver file needs to be 1.
# Right now we have two columns with information about active or not active. This should be combined into 1 column
# Combine the information in active_snd and active_rc. The result is a numerical vector with numbers 0 (both files not active),
# 1 (one file active) or 2 (both files active)
td$active_raw <- td$active_snd + td$active_rc # a tie is active if the snd OR rc file is modified during that version 
# count the number of not-adctive (0), one-active (1), and both-active (2) pairs of files
table(td$active_raw) 
# 204385 non-active file pairs, 22360 file pairs with 1 active file, 684 active file pairs
#check the column names
names(td) 
# create a new column active. This column has the same content as active_raw (a numerical vector of 0, 1, 2)
td$active <- td$active_raw
# in the column active (just created above), change all 2s into 1s. Now we have a vector of 0s (no file active) and
# 1s (1 or 2 files active)
td[td$active_raw == 2, which(names(td) == 'active')] <- 1 
# count the number of not-active (0) and active (1) files
table(td$active)
dim(td)

# save the result in a r object
saveRDS(td, "td.rds")

# save the result in a csv file. 
write_csv(td, 'td_all.csv')

# filter td to only include active files
td_active <- td[which(td$active == 1), ]
dim(td_active)
write_csv(td_active, 'td_active.csv')
# Step 2: replace file Ids with folder owner names ------------------------


# the required communication network is the td_net network. 
# The file ids (ID_rev) need to be replaced with names of owners
# 
sender <- NULL 
df2 <- data.frame(df)
for(i in 1:length(td$und_from_file_id)){
  #for(i in 1:100){  
  tmp_file <- td$und_from_file_id[i]
  tmp_ver <- td$ver[i]
  
  # should the column in df2 be ID_rev???? No this results in no matches
  #tmp_owner <- data.frame(df[df$ID_File_und == tmp_file && df$ver == tmp_ver, 45])
  tmp_owner <- as.vector(df2[df2$ID_File_und == tmp_file && df2$ver == tmp_ver, 45])
  
  if(is_empty(tmp_owner)){tmp_owner <- 'external owner'}
  
  sender <- c(sender, tmp_owner)
} 

td$sender <- sender

receiver <- NULL 
for(i in 1:length(td$und_to_file_id)){
#for(i in 20:50){  
  tmp_file <- td$und_to_file_id[i]
  tmp_ver <- td$ver[i]
  #print(tmp_file)
  #print(tmp_ver)
  #class(tmp_file)
  #class(tmp_ver)
  #print(df2[df2$ID_File_und == tmp_file,c(1,6,7,45,39)])
  #tmp_owner <- data.frame(df[df$ID_File_und == tmp_file && df$ver == tmp_ver, 45])
  tmp_owner <- as.vector(df2[df2$ID_File_und == tmp_file & df2$ver == as.numeric(tmp_ver), 45])
  #print(tmp_owner)
  if(is_empty(tmp_owner)){tmp_owner <- 'external owner'}
  # 
  tmp_owner <- tmp_owner[1]
  receiver <- c(receiver, tmp_owner)

} 
rm(df2)

td$receiver <- receiver
# MH email from May 9th 2018 says: "please note that variables “und_from_file_id” 
# and “und_to_file_id” in this file [file-by-file.zip], correspond to the 
# variable “ID_File_und” in the 2-model edge-list of developer-by-file."
# The ID is version specific

# step 3: create network of required commnication per version -----------------------------------------------------------------

# as the network should be created per version and per time period (e.g., week),
# this could be a list of networks
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
# 
# take only the relevant column from td: 
# sender, receiver, weight, version, active
# 
# filter td to only include the active ties. this will reduce the computing time
td_act <- td[td$active == 1, ]

# td1_net is .... 
td1_net <- network(td_act[,c(1:3, 5, 9)],matrix.type="edgelist",directed=TRUE, 
                  ignore.eval=FALSE) 
# td2_net is ....
td2_net <- network(td_act[,c(10,11,3, 5, 9)],matrix.type="edgelist",directed=TRUE, 
                   ignore.eval=FALSE) 
td_net

# step 4:Get network per version ------------------------------------------

table(td1_net%e%'ver') # if you run this line you see the number of edges per version

# to subset td_net you need to indicate the edge ids you want to focus on
# You don't need to know the edge ids (eids), but can include a command to filter edges
# based on edge attribute.
# to filer by edge attribute you type in network_name%e%'edge attribute' == 'filter valued'
# the 'e' between percentage signs tells R to look for edges. To look at vertices (nodes)
# you would type %v%.
# The edge (or node) attributes follows the second percentage sign. This is always in 
# quotation marks 
# Then you tell R what this attribute should be equal to (==), greater (>) or smaller (<) etc.

td_net1 <- get.inducedSubgraph(td1_net,eid=which(tdn_net%e%'ver'==1))
td_net1

# there is no sense in drawing this network in R. it has 5720 active edges. 
# Drawing would be better in Gephi

# subsetting td_net can take some time. You can save the resulting object so that next time 
# you don't have to subset it again, but just need to load the RData object.
save.image('tech_interface.RData')


# create the network communication required -------------------------------


