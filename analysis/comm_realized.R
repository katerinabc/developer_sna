# Communication realized script

# update 14.10.20: I THINK THE LOGIC HOW COMM REALIZED IS CREATED IS WRONG. CHECK README FILE AND REDO IF NECESSARY. ALSO CLEAN UP FOLDER. TOO MANY FILES. 
# 
# clean your workspace first 
rm(list=ls())
library(ndtv)

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

# for dynamic network data
library(networkDynamicData) 
library(sna)

#load the dataset. df_modified has been created in the file ownership_developer_sna.R
#df_modified is 
df <- read_csv('df_modified.csv')

# step 0: create communication realized for the complete project ----------
# 
# check script version4_mirroring line, line 580





# step 1: create edgelist -------------------------------------------------

#step 1 creates the data that will be used in the networkDynamicData package. This package is build for longitudinal SNA 

# This is just the edgelist developer * folder owner. The complete data set (df) is subsetted. Only columns author, folder_owner, ver, and Date are used. 
# I subset the dataset and focus on the needed columns to reduce the file site

el <- df %>% select(author, folder_owner, ver, Date)



# com realized csv export -------------------------------------------------

# group_by is a function that lets you group data sets by specfic columns and then run stats on this grouping.
# el_freq groups el by developer (author), folder owner and version. It then counts how often this combination exists. This is the edge weight. 
# For example alberto worked as developer on a file in a folder owned by alberto in version 2 35 times.
el_freq <- el %>% group_by(author, folder_owner, ver) %>% count()
# save the file. You need to specify a file name.t
write_csv(el_freq, 'communication_realized_edge_weight_by_version.csv') 

# this is similar to the previous line, just that date information is taken into account. 
# In version 2, on 14-05-2020 the developer alberto worked 11 times on a file which is owned by Alberto
el_freq_det <- el %>% group_by(author, folder_owner, ver, Date) %>% count()

el_all <- el %>% group_by(author, folder_owner) %>% count()


# comm realized network dynamic analysis ----------------------------------



cr_el <- el # this copies the R object into a new object. I do this to make sure that el remains clean. 

# aggregate per author-folder_owner-version pair
#%>% group_by(author, folder_owner, ver, Date) %>% count() %>% ungroup()

#transform Date into a date column that is easier to work with. 
cr_el$Date <- lubridate::ymd(cr_el$Date) 
cr_el <- arrange(cr_el, Date)
# add an ordered vector ranging from 1 to n = last row. This is done to indicate the order of events
# direction needs to be numeric. 
# 1 is activate edge, 0 is deactivate edge
# If I have a directon (avtive/deactive) I need to manually add deactivation 
# for all edges. Unless they remain active for the whole duration. 
# Use edge.toggles instead of edge.changes
# 
cr_el <- cr_el %>% add_column(time = 1:nrow(cr_el),
                              direction = 1) 

# inspect what was created.
head(cr_el)

# replace authors with ID as networkdynamic works with IDs
developers <- data.frame(developer = as.character(unique(authatt$author)),
                         id = seq(1:length(unique(authatt$author))))

# replace names with ids. Left-join works similar to the lookup functions in excel. The dataset cr_el and developers are joined, by looking for shared identifier. 
# The identifiers are author/developer. In cr_el the column name is author and in developers the column name is developer. The information in these two columns are the same. 
cr_el <- cr_el %>% left_join(developers, by = c('author' = 'developer'))
cr_el <- cr_el %>% left_join(developers, by = c('folder_owner' = 'developer'))
# check the column names of cr_el. For the package one column needs to be tail and the other head. 
names(cr_el) 
names(cr_el)[which(names(cr_el) == "id.x")] <- 'tail'
names(cr_el)[which(names(cr_el) == "id.y")] <- 'head'

#inspect the object
cr_el

#subset cr_el to have only the columns that are needed
cr_el_modified <- as.data.frame(cr_el %>% select(time, tail, head, direction))
# create a networkdynamic object
cr_elm <- as.matrix(cr_el_modified)

# create the dynamic SNA object using networkDynamic
# when loading the dataframe there is an error with the dimnames. KBC thinks
# this is because of the dimnames for rows. 
cr_dyn <- networkDynamic(edge.toggles = cr_elm)

# plot of complete network
plot(cr_dyn)

# this works. Now we change the time period from consecutive to the version
# in the line below if ver is replaced with Date, 
# the network is done per time point

cr_el <- as.data.frame(cr_el %>% select(ver, tail, head, direction))
head(cr_el)
#rename the first column in cr_el to be called time
names(cr_el)[1] <- 'time'
# create a networkdynamic object
cr_elm <- as.matrix(cr_el)
# when loading the dataframe there is an error with the dimnames. KBC thinks
# this is because of the dimnames for rows. 
cr_dyn <- networkDynamic(edge.toggles = cr_elm)
cr_dyn
network.dynamic.check(cr_dyn)
cr_dyn
# plot of network
plot(cr_dyn)


render.d3movie(cr_dyn,
               plot.par=list(displaylabels=T),
               output.mode = 'HTML',
               filename='cr.html')
filmstrip(cr_dyn)
timeline(cr_dyn)



# extract network for version 1 -------------------------------------------
#cr_ver1 <- network.collapse(cr_dyn, at=1)
cr_ver1 <- network.extract(cr_dyn, at=1) # this extracts the network
# for version 1
# cr_dyn has 12 time changes. In cr_dyn the time period is a step
# the step is the version number
cr_ver1
class(cr_ver1)
save.image('cr_ver1.RData') # will save the complete session in this file. 


# extract network for branch 1 -------------------------------------------
# branch 1 is composed of version 1 to 6

cr_b1 <- network.extract(cr_dyn, onset = 1, terminus = 6)
cr_b1
plot(cr_b1)



# extract network for branch 2 --------------------------------------------
# branch 2 is composed of ver 1 - 3 and 7 - 12

cr_b2a <- network.extract(cr_dyn, onset = 1, terminus = 3)
cr_b2b <- network.extract(cr_dyn, onset = 7, terminus = 12)

# create a list of networks based on cr_dyn and then remove
# list entry 4 to 6
cr_dynlist <- get.networks(cr_dyn, start = 1, end = 13)
length(cr_dynlist)

cr_b2 <- networkDynamic(network.list =  cr_dynlist[-c(4:6)])
cr_b2


# testing correctness of networks -----------------------------------------

#cr_b1 and cr_b2 have the same starting point
#collapse networks
# doees not include any weights/frequency
b1_flat <- network.collapse(cr_b1)
b2_flat <- network.collapse(cr_b2)

#similarity btw networks
class(b1_flat)

Jaccard <- function(var1,var2){
  # a and b should be 0-1 arrays
  tab <- table(var1,var2)
  print(tab)
  pa <- tab["1","1"] + tab["1","0"]
  pb <- tab["1","1"] + tab["0","1"]
  pab_ind <- pa*pb/sum(tab)
  J_ind <- pab_ind / (pa + pb - pab_ind)
  cat("Expected Jaccard under independence with these marginals =",
      J_ind, "\n")
  cat("Observed Jaccard = ")
  tab["1","1"]/(tab["1","1"] + tab["0","1"] + tab["1","0"])
}

Jaccard(as.sociomatrix(b1_flat), as.sociomatrix(b2_flat))
# similarity between the the two branches is 0.7
 