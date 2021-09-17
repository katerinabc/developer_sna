# Collaboration  needed or required. 

# author: Katerina Bohle Carbonell
# project owner: Mahdi
# created: 23.11.2020
# updated: 22.03.2021

# clean your workspace first 
rm(list=ls())

# preamble ----------------------------------------------------------------

# This file contains functions created for this project
source('functions_developer_sna.R', echo=F)
source('data_import.R', echo=F) # this loads some files that are not needed and just take up space
rm('DF', 'DF2', 'dat', 'f', 'myFiles', 'myFiles2')

#packages for working with data
library(tidyverse)
library(readr)

# packages for working with SNA data
library(statnet)

# description -------------------------------------------------------------


# Two developers have to interact with each other if developer A owns file Z that is linked to file Y owned by developer B. 
# If file Z or Y are being modified developer A and B need to "communicate/collaborate" with each other by modifying their files.
# In brief, it's the tech dependencies file, but where file IDs are replaced by folder owner 
# the final object should be an edgelist or matrix



# input files -------------------------------------------------------------

# task dependencies file
# This is the basis. The script to create the file is saved in tech_interface.R
# From the script tech_interface.R we need the file td_active.csv
# td_active.csv consists of file ids (sender or receiver) that have been modified.
td <- read_csv('td_active.csv')
td
table(td$active) #checking if td_active really just includes active fiels
td <- td %>% select(und_from_file_id, und_to_file_id, weight, ver) # subset the file to only have the columsn we need
td <- td %>% mutate(und_from_file_id = as.integer(und_from_file_id),
                    und_to_file_id = as.integer(und_to_file_id))

# count the number of times from_file ID and to_file ID have several entries in the same version

td %>% group_by(und_from_file_id, und_to_file_id) %>% tally(sort = T)
td %>% group_by(und_from_file_id, und_to_file_id, ver) %>% tally(sort=T)
# good, every pair just appears once

# folder owner files 
# This is used to replace file IDS with names of developers. This is df_modified.csv. This file is subsetted to only 
# include two columns: name of the owner and file id
ownership <- read_csv('df_modified.csv')
ownership <- ownership %>% select(folder_owner, ID_rev)
ownership <- ownership %>% mutate(ID_rev = as.integer(ID_rev))

# creating collaboration needed matrix -------------------------------------------------------------

# create the collaboration required file based on technical interface
comm_required <- td

# All ID files in the task interface who do not have an owner are replaced with no_owner
# #combine tech interface and ownership file. 
# For every file ID where there is a match between commm_realized and onwership, the folder owner is retrieved
comm_required <- comm_required %>% 
  full_join(ownership, by = c('und_from_file_id' = 'ID_rev')) %>%
  full_join(ownership, by = c('und_to_file_id' = 'ID_rev'))
# filter out all files where the is no owner for the from_file and to_file
# left with 1353 diads out of 23044 diads
comm_required <- comm_required %>% 
  filter(!is.na(folder_owner.x), !is.na(folder_owner.y))
#write.csv(comm_required, 'comm_required_detailed.csv')

# remove self-loops

comm_required <- comm_required  %>%
  filter(! folder_owner.x == folder_owner.y)

# summarize file interaction by folder owners and count the number of owner pairs per version and files combination

# testing the code
comm_required %>% 
  group_by(folder_owner.x, folder_owner.y, ver) %>% 
  count(sort = T) %>% 
  filter(folder_owner.x == 'fabio.boldrin' & ver == 1)
comm_required %>% 
  group_by(folder_owner.x, folder_owner.y, ver) %>% 
  count(n = sum(weight)) %>% 
  filter(folder_owner.x == 'fabio.boldrin' & ver == 1)
comm_required %>% filter(folder_owner.x == 'fabio.boldrin' & folder_owner.y == 'andrea.rana' & ver == 1)

# aggregating data by folder owner pair
# # change ver (the last input in the row group_by) to the required temporal slice. 
comm_required_sum <- comm_required %>% 
  group_by(folder_owner.x, folder_owner.y, ver) %>% 
  count(sum_weights = sum(weight))

write_csv(comm_required_sum, 'comm_required_sum.csv')
save(comm_required_sum, file = 'comm_required_sum.RData')


# create network dynamic object -------------------------------------------

creq_el <- comm_required_sum %>% add_column(direction = 1)
names(creq_el)[which(names(creq_el) == 'ver')] <- 'time' 


# inspect what was created.
head(creq_el)

# replace authors with ID as networkdynamic works with IDs
developers <- data.frame(developer = as.character(unique(authatt$author)),
                         id = seq(1:length(unique(authatt$author))))

# replace names with ids. Left-join works similar to the lookup functions in excel. The dataset cr_el and developers are joined, by looking for shared identifier. 
# The identifiers are author/developer. In cr_el the column name is author and in developers the column name is developer. The information in these two columns are the same. 
creq_el <- creq_el %>% left_join(developers, by = c('folder_owner.x' = 'developer'))
creq_el <- creq_el %>% left_join(developers, by = c('folder_owner.y' = 'developer'))
# check the column names of cr_el. For the package one column needs to be tail and the other head. 
names(creq_el) 
names(creq_el)[which(names(creq_el) == "id.x")] <- 'tail'
names(creq_el)[which(names(creq_el) == "id.y")] <- 'head'

#inspect the object
creq_el
write_csv(creq_el, 'crequired_el.csv')

#subset cr_el to have only the columns that are needed
creq_el_modified <- as.data.frame(creq_el %>% ungroup() %>% select(time, tail, head, direction))

# create a networkdynamic object
creq_elm <- as.matrix(creq_el_modified)
head(creq_elm)

# create the dynamic SNA object using networkDynamic
# when loading the dataframe there is an error with the dimnames. KBC thinks
# this is because of the dimnames for rows. 
creq_dyn <- networkDynamic(edge.toggles = creq_elm)

save(creq_dyn, file = 'creq_dyn.RData')
# plot of complete network
plot(creq_dyn)

