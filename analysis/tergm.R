# ERGM: test how much communication realized depends on developer attributes, developer dyadic variables, and required communication
# 
# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 15 March 2021
# updated: 26 March 2021
# 
# TODO:
# add dynamic vertex information

rm(list=ls())

# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)


# model description -------------------------------------------------------------------

# - the DV is realized communication
# - what are the characteristics of the technical stuff the developer owns that predict realized communication: 
#     centrality of file within task dependency network
#     type of task: bug fixing vs. creating new file
# - IV : structural features, reciprocity, developer attributes (hierarchy, gender), 
#     dyadic developer attributes (proximity)
# - file attributes (dyadic attributes (about the files))


# set up in R -------------------------------------------------------------

# files needed - CHECK FOR ACCURACY
# 
# DV: realized communication
# The DV is created in the file comm_realized.R It's a developer-to-developer matrix. 
# The CSV file "communication_realized_edge_weight_by_version.csv" is an weighted edge edgelist 
# describing which developer talks with which other developer

# IV (1): required communication (?) 
# This is the link between a folder owner and a file based on the task dependencies (layers of tech dependencies)
# The file is created in communication required. In short, the file tech dependencies is taken as the basis. 
# In this file all file IDs are replaced with their file owners. This is saved as required communication. 
# The R object is called comm_realized
# 
# IV (2): developer attributes
# this is the file authatt created with the script data_import.R


# hypothesized RS ---------------------------------------------------------

# DV = communication that did happen (communication realized) based on the file - author data
# cr_dyn.RData created in the file comm_realized.R
# IV = communication that should happen (communication required) based on file-file data
# 


# load data ---------------------------------------------------------------
set.seed(seed = 1234)

load('cr_dyn.RData') #based on df, file by author dataset. this shows where interaction did happen
dv <- cr_dyn
load('creq_dyn.RData')
ivnet <- creq_dyn

# developer data + add developer ID
authatt <- read_csv("authatt.csv")
att_master <- unique(authatt[,-c(1)])
developerID <- data.frame(developer = as.character(unique(authatt$author)),
                         id = seq(1:length(unique(authatt$author))))
ivatt_master <- att_master %>% left_join(developerID, by = c('author' = 'developer')) %>% select(-ID_author)


# plot graphs -------------------------------------------------------------

render.d3movie(cr_dyn,
               plot.par=list(displaylabels=T),
               output.mode = 'HTML',
               filename='cr.html')
filmstrip(cr_dyn)
dev.off()
#timeline(cr_dyn)

render.d3movie(ivnet,
               plot.par=list(displaylabels=T),
               output.mode = 'HTML',
               filename='cr.html')
filmstrip(ivnet)
dev.off()


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
# 
# redo this as some people change their attributes in a version. 
for (i in network::get.vertex.attribute(dv, 'vertex.names')){
  print(i)
  
  tmp_job <- pull(ivatt_master[ivatt_master$id == i, 3])
  print(tmp_job)
  jobtitle <- cbind(jobtitle, tmp_job)
  
  tmp_loc <- pull(ivatt_master[ivatt_master$id == i, 4])
  print(tmp_loc)
  location <- cbind(location, tmp_loc)
  
  tmp_con <- pull(ivatt_master[ivatt_master$id == i, 5])
  print(tmp_con)
  contract <- cbind(contract, tmp_con)
}

#network::set.vertex.attribute(dv, 'ver2', as.numeric(t(participants)[,1]))
network::set.vertex.attribute(dv, 'jobtitle', as.numeric(t(jobtitle)[,1]))
network::set.vertex.attribute(dv, 'location', as.numeric(t(location)[,1]))
network::set.vertex.attribute(dv, 'contract', as.numeric(t(contract)[,1]))


# modify atttributes that are dynamic -------------------------------------

#jobtitle

# create familiarity network ----------------------------------------------


# strength of familiarity is how often two developers worked on a previous version together
# We will first calculate the affiliation matrix of develoepr - group membership
# This will be transformed into a one-mode matrix (developer-developer) where the cell indicates
# how often these two people worked on the same version
familiarity <- ivatt_master[,c(7, 6)]
A <- Matrix::spMatrix(
              nrow=length(unique(familiarity$id)),
              ncol=length(unique(familiarity$ver2)),
              i = as.numeric(factor(familiarity$id)),
              j = as.numeric(factor(familiarity$ver2)),
              x = rep(1, length(as.numeric(familiarity$id)))
              )
row.names(A) <- levels(factor(familiarity$id))
colnames(A) <- levels(factor(familiarity$ver2))
A

fam_dev <- tcrossprod(as.matrix(A))

fam_dev_net <- network(as.matrix(fam_dev), directed=F, ignore.eval=F, names.eval='weights')
fam_dev_net
fam_dev_net%e%'weights'

# basic model -------------------------------------------------------------

