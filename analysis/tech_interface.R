# create technical interface
# 
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


# tech interface matrix per version ---------------------------------------

wrk_ver <- 2 # this is a place holder to save on writing. This indicates what version 
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
  
  active_snd <- tmp_td_sender
  active_rc <- tmp_td_receiver
}

# 
