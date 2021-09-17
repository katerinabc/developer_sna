# Begin with a clean environment
rm(list=ls())

# Define ownership network
library(pathological)
library(tidyverse)
source('data_import.R', echo=T)


# Create dataset own. This dataset describes ownership of folders based on when they were first
# created. 
# I included files that were created (action == A) and those who were modified (action = M). 
# I did this, as several files, when they first appeared in DF had as action Modified. When removing
# duplicates, the first entry is always kept. This means files that were first created and then 
# modified, the first entry (thus when they were created) is kept. 
# using the shorter filename (column 40), author (column 1). column 39 contains ID rev.
# ID rev is needed to make sure that the files are sorted properly. 
# 
# the data frame own is a look up data frame to check who first created a folder. It does
# not contain folder ownership transfer when members change.
# 
# create a numeric version of the column version
DF$ver <- as.numeric(DF$ver)
# make sure DF is sorted
DF <- DF[order(DF$ver),]

#create the object OWN using 3 columns: author (1), ID rev (39) and filename(40)
own <- DF[,c(1,39, 40)] 
#check dimensions
dim(own)
# sort the files
own <- own[order(own$ID_rev),] 
# check for duplicate filenames and remove them
table(duplicated(own[,2])) 
own <- own[!duplicated(own[,2]),] 

#check the column names of OWN
names(own)
# remove the colum ID rev as not needed in the future
own <- own[,-2] 

get_folder_name <- function(x){
  # function to decompose the file path and get the last folder. 
  # Decompose path returns the directory name, the file name, and file extension.
  # The dirname is what we want as it contains the complete file path without the extension.
  # The fuction 'decompose_path' creates a data frame.
  # Of this data frame we take the first column, the 'dirname'
  # From the dirname we want the file path. decompose_path adds the local working directory
  # to the path. That needs to be removed first. 
  # The file path of the files in the DF are returned. 
  # 
  # An easier way might have been to gsub (replace) everthing that comes after the "."
  x <- pathological::decompose_path(x)
  x <- x[,1]
  x <- gsub('/Users/katerinadoyle/Documents/gitrepo/developer_sna/analysis/',
            '', x)[1]
  return(x)
}

# To the data frame OWN we are going to add the names of the folder. This is done by 
# looping through each row in DF. lapply applies the function 'get_folder_name' to all
# entries (rows) in the second column of OWN. Unlist "flattens" the list lapply create
own$folder_names <- unlist(lapply(own[,2], get_folder_name)) # returns the foldername
write_csv(own, "ownership_file_created.csv") # looks good

# remove duplicates from the folder_names file. The first instance is always kept
# this will be the folder owner.x1
dim(own)
head(own)
own <- own[!duplicated(own[,3]),]
dim(own)

# To add folder owners to DF, three steps will be done. 
# 1. add one column to DF with folder_names. These folder names will be based on the filename
# 2. match folder name with folder name in own
# 3 return the author of the matched folder name in step 2
# 
# the function decompose_path throws an error if there are duplicate folder 
# names. to avoid this, loop over folder names, create vector with folder names (tmp2)
# add tmp2 to DF 
# 
# STEP 1: add folder names columns to DF
tmp2 <- NULL
for (i in 1:nrow(DF)){
  tmp <- get_folder_name(DF[i,40])
  tmp2 <- c(tmp2, tmp)
}

DF$folder_names <- tmp2

# STEP 2 and 3: Match folder names and add folder_owner to DF

folder_owner <- NULL
for (i in 1:nrow(DF)){
  # get the folder name from the developer-by-file file
  tmp_foldername <- DF[i, 42]
  
  # match the folder name from step 1 with the folder name from the own object,
  # and return the name of the folder owner
  # assignes the name of the folder owner to the temporary object tmp_flder_owner
  if (length(own[own$folder_names == tmp_foldername,1])==0){tmp_folder_owner <- 'error'}
  # error added when folder owner not found in previous version. The if statement is hard to read. 
  # If the return of matching folder names from DF in own is of length 0 (no match), then call the folder owner 'error'
  # own$folder_names == tmp_foldernames: this is matching folder names.
  # The previous line is embedded in own[matched results, 1]. The number after the comma tells R to return
  # the value in column 1 for the row that contained the matched results in the object own.  
  else{
    tmp_folder_owner <- own[own$folder_names == tmp_foldername,1]
  }
  # add the folder owner to a new vector
  folder_owner <- c(folder_owner, tmp_folder_owner)
}

DF$folder_owner <- folder_owner

# solving the errors
# some files are not assigned an owner (folder owner == error). This is because the file was not
# created but modified even thought it was the first time it appeared. 
write_csv(DF, 'df_check.csv')
# For example, in df_check you can see that the first 10 files have all been modified at the exact same time
# but only 3 have been created (action == A), the rest was modified. df is sorted by id_rev.
# On 10/9/19 I changed the way own was created by including all files. 


# create a logical vector indicating if the folder owner is member in the project version
members_vec <- NULL
for (i in 1:nrow(DF)){
  tmp_owner <- DF[i, 43] # stores the name of the owner
  tmp_version <- DF[i, 6] # stores the version number
  if(tmp_owner %in% authatt[authatt$ver == tmp_version,2]){ 
    # creates a logical vector (TRUE/FALSE) if an author is member in a version
    tmp_member <- TRUE
  }
  else{
    tmp_member <- FALSE
  }
  members_vec <- c(members_vec, tmp_member)
}

DF$members <- members_vec # adds the logical vector to DF. 

# visualize folder ownerships  

ggplot(DF, aes(x = folder_owner, fill = as.factor(ver))) + geom_bar() + 
  scale_fill_discrete(guide=guide_legend(title="Version Number")) +
  coord_flip() + 
  labs("Ownership distribution by software version", 
       x = 'Frequency', y = 'Developer')
ggsave("onwership_frequency_all_versions.png")

# across the complete software 
#table folder names X author
# Commented out as ugly graph + takes long time to run
# folder_contribution <- DF[,c(1,6, 42)] %>% 
#   count(folder_names, ver, author)%>%
#   spread(author, n, fill = 0) %>%
#   ggplot() + geom_tile()
# legend is mixed up for x axes and y axes. Not that necessary. 
# I will improve graph if needed. 

library(dplyr)
contr_now <- DF %>% group_by(ver, folder_names, author) %>% summarize(contribution = n())
ggplot(contr_now, aes(contribution)) + geom_bar()
contr_now_mod <- contr_now[-contr_now$contribution == 0,]
#ggplot(contr_now_mod, aes(author, folder_names, fill = contribution)) + geom_tile() # too many variables

# create ownership folders ------------------------------------------------

# At this point DF contains the authors (those who created or modified a file), 
# the folder names, and the folder owners (based on who first created a folder).
# 
# Folder ownership has been so far assigned based on who first created a folder. This ownership has been 
# applied to all folders regardless if the developer is a member of the version. 
# Now we need to change the folder owners for those who left the project.
# Gaining Ownership if the owner left is based on contribution to folders.
# ownership can not be re-gained by re-joining the team.

# checking that folder owners are in character types and not categories
DF$folder_owner <- as.character(DF$folder_owner) 
authatt$author <- as.character(authatt$author)


# Membership in a project is indicated through the logical vector members. 
# row names are folder owners, first column (FALSE) means the owner is not a member
# of a version while being assigned membership. So alberto has been assigned 
# ownership to 218 folders while not being part of the version team. He has been assigned
# ownership to 76 folders while being part of the team. 
table(DF$folder_owner, DF$members) # how often has a developer be assigned to a folder, but isn't member in that version. 

# top contributor per version per folder

# CREATE A LOOK UP TABLE
# This is creating a lookup table with the following information: 
# group DF by version number, then folder names, and then developer and add a column with info about
# how often the developer made a contribution to a specific folder in a specific version.
contr_now <- DF %>% group_by(ver, folder_names, author) %>% summarize(contribution = n())

# transform the tiddy table created in the previous line into a data frame and keep only those developers who made the 
# highest contribution to a folder in a version.
# in top_n, 1 means to return only 1 row. As 1 is positive it returns the row with the highest value
# in top_n, the second argument telsl R how to sort the column. 
top_contr_now <- contr_now %>%group_by(ver, folder_names) %>% top_n(1, contribution)

# creating an index with row numbers of those developers who are not a member in 
# version x but owner of a folder that has been modified in version x
idx_nonmembers <- which(DF$members == FALSE)


# copy the folder owner names. These will be overwritten if the folder owner is not part of a version
DF$new_owner <- DF$folder_owner

#subset DF by those who developers who are not a member in a given version but owner of a folder
# not sure this is used further down
nonmembers <- DF[idx_nonmembers, c(1:3,42:45)] # this was first folder 4

# take care: if A makes highest contribution to folder XYZ, this ownership is transfered to the folders
#  across all version
 
# Assigning new folder owners. This will be done per version
# 
# A while loop is created going through the sequence of numbers in the 
# index idx_nonmembers. The while  loop makes sure that this is only done
# while the version number is below 7. This takes care of ownership assignment
# in branche 1

# i is the row number in DF indicating a folder owner who is not a member of the version.
# For every i, get the version number (column 6), folder name (column 42)
# Subset 1: Get all developers who contributed to tmp_folder using the lookup table
# top_contr_now. Store this in tmp_contributors
# Subset 2: Subset the data file tmp_contributors to only get the developers who 
# contributed to tmp_version. Then, pick the first one with the 
# highest amount of contributions. Assign this name to 'new owner' (column 45 in DF)

sink("new_owner_test.txt") #sink writes the output to a text file for inspection
# used to test the loop. Commented out once code is working

a <- 1 # a is a placeholder to sequence over idx_nonmembers
  while(DF[idx_nonmembers[a],6] < 7){
    i <- idx_nonmembers[a]
    tmp_version <- DF[i, 6] # store the version 
    print(paste("Version: ", tmp_version, "index:", i )) # checking if it works
    tmp_folder <- DF[i, 42] # store the folder name
    # 
    # get all rows with tmp_folder before version 7
    tmp_idx <- which(DF$folder_names == tmp_folder & DF$ver < 7)
    # subset rows by only taken those at or after i
    # i is in the row number index in DF.
    tmp_idx <- tmp_idx[which(tmp_idx >= i)] # error ???
    
    # find the top contributor for tmp_folder in version tmp_version
    tmp_contributors <- top_contr_now[top_contr_now$ver == tmp_version,]
    tmp_contributors <- tmp_contributors[tmp_contributors$folder_names == tmp_folder,] # Subset 1
    tmp_folder_contributor <- tmp_contributors[ # Subset 2
      tmp_contributors$ver == tmp_version,3][[1]][1] # pick first developer when 2 tie in contribution
    
    # assign top contributors to all tmp_folder instances after i
    DF[tmp_idx,45] <- tmp_folder_contributor
    a = a+1
    # 26/09/2019: this loop works. only error is andrea.rana making a change to 
    # id_rev 38960. small bug fix. delete this row.
  }

# delete from idx_nonmebers all index for version 4 to version 6 as
# folder ownership in version 7 needs to reflect ownership in version 1-3
idx_nonmembers_branche2 <- which(DF$members == FALSE & ( DF$ver < 4 | DF$ver > 6) )

# the next loop first re-runs folder ownership for version 1 to 3, then jumps to version 7
# branch 2: version 1-3 and then 7 to 11

a <- 1
for (a in 1:length(idx_nonmembers_branche2)){ 
  i <- idx_nonmembers_branche2[a]
  tmp_version <- DF[i, 6]
  #print(paste("Version: ", tmp_version, "; index:", i ))
  #if(tmp_version > 3 | tmp_version < 7){next} #maybe here the problem???? 
  #idx is filtered so this might not be necessary
  print(paste("Version: ", tmp_version, "; Index: ", i))
  tmp_folder <- DF[i, 42] # store the folder name
  # does the line above take care of trickling down the folder ownership reassignment to
  # all folders with that name
  # 
  # get all rows with tmp_folder
  tmp_idx <- which(DF$folder_names == tmp_folder)
  # subset rows by only taken those after i
  tmp_idx <- tmp_idx[which(tmp_idx >= i)]
  # find the top contributor for tmp_folder in version tmp_version
  # first subset top_contr_now by version
  tmp_contributors <- top_contr_now[top_contr_now$ver == tmp_version,]
  tmp_contributors <- tmp_contributors[tmp_contributors$folder_names == tmp_folder,] # Subset 1
  tmp_folder_contributor <- tmp_contributors[ # Subset 2
    tmp_contributors$ver == tmp_version,3][[1]][1] # pick first developer when 2 tie in contribution
  
  # assign top contributors to all tmp_folder instances after i
  DF[tmp_idx,45] <- tmp_folder_contributor
  a = a+1
}

sink()

#View(DF[DF$members == FALSE, c(43:45)]) # visual inspection of folder owner changes

table(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45])
# 1672 trues. it's getting worse and worse. I think error is when subsetting tmp_contributions
# 26/09/2019: getting better. fewer errors. 1509 trues, and 2465 false
# 26/09/2019: took out the if statement testing for tmp_version as the idx is already
# filtered. after this, error rate is 130 (out of 3974 rows)
130/(3844+130)
# error rate: 3%
# error rate per version

# trouble shooting error
idx <- which(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45])
View(head(DF[DF$members == FALSE, ][idx,]))
write.csv(DF[DF$members == FALSE, ][idx,c(1,3,6,39,43:45)], 'df_check.csv')

# I'm going to use id_rev 20216 to trouble shot
DF[DF$ID_rev == 20216,43:45]
which(DF$ID_rev == 20216) #id 1916
# folder owner is chiara.moretti, version 2
View(authatt) # chiara is only in version 1
tst_folder <- DF[DF$ID_rev == 20216, 42] # col 42 is folder_names
# let's check top contribution file
top_contr_now[top_contr_now$folder_names == tst_folder,]
# returns data frame with two rows. first row is version 1, second row is verion 2
# potential problem: top_contr_now not subsetting by version
# solution: I added a line to first subset top_contr_now by tmp_version
# error 2 was in how tmp_idx was subsetted. It needed to include all rows with index
# greater than or equal to i
# error 3: in second loop ownership is not assigned to another person. it's like the 
# index is skipped. every time I test the loop with one i, it works. I run the loop
# and the numerb of wrong assignments decreases. 
DF[DF$ID_rev == 163043,c(6, 43:45)]
which(DF$ID_rev == 163043) 
tst_folder <- DF[DF$ID_rev == 163043, 42][1] #
top_contr_now[top_contr_now$folder_names == tst_folder,]


# error rate per version
DF[DF$members == FALSE, ][idx,c(1,3,6,39,43:45)] %>% group_by(ver) %>% 
  #count()
  summarize(n = n()) %>%
  mutate(freq = round(n/sum(n)*100,2))

DF %>% select(c(1,6,43:45)) %>%
  group_by(ver) %>% mutate(nrow_vers = n()) %>%
  ungroup() %>%
  filter(DF$members == FALSE) %>%
  filter(folder_owner == new_owner) %>%
  #filter(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45]) %>%
  group_by(ver) %>%
  mutate(false_rows = n()) %>%
  select(ver, nrow_vers, false_rows) %>% unique() %>%
  mutate(error_rate = round(false_rows/nrow_vers*100,2)) %>%
  write_csv("error_rate_ownership_assignment.csv")
# errors in version 3 to 6. ALl error rates below 10 %. 
# biggest issue in version 4
# mark these folder_owners as 'external member'
idx <- which(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45])
head(DF[DF$members == FALSE,][idx, 43:45])
DF[DF$members == FALSE,][idx,45] <- 'external member'


ownership_change <- reshape2::melt(table(DF$folder_owner, DF$new_owner))
ownership_change <- ownership_change[-which(as.character(ownership_change$Var1) == 
                                              as.character(ownership_change$Var2)),]
ownership_change <- ownership_change[!ownership_change$value == 0,]

ggplot(ownership_change, aes(x = Var1, y = Var2, fill = value)) + geom_raster() + 
  labs(title="Change in ownership of folders", x = 'Original Owner', y = 'New Owner') +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('ownership_change.png')

write_csv(DF, "df_modified.csv" )


# create a file owner & file name (n*m) matrix ----------------------------

# load the file df_modified as DF if starting a new session
DF <- read_csv("df_modified.csv")

# subset DF created above to only take the column with the file name and the folder owner 
# the column filename is the shorter version of Filename
nm <- DF %>% select(new_owner, filename, ver)

write_csv(nm, 'ownership_matrix_csv')

# add weights to the edgelist by counting how often developer-file pair is in nm. 
# This drops the column version
nm_all <- nm %>% group_by(new_owner, filename) %>% count()
#inspect
nm_all

# create one huge matrix
#load the igraph package
library(igraph)
# create a 2 mode graph from nm_all, excluding the weight column. This will be added later manually
g <- graph.data.frame(nm_all %>% select(-n), directed = F)
#the second column of edges is TRUE type (file names are set as type == TRUE)
V(g)$type <- V(g)$name %in% nm_all[,2] 
# add edge weights.
# pull means to pull out the data from the tibble object nm_all. 
# as.numeric makes sure the data is in number format
E(g)$weight <- as.numeric(pull(nm_all[,3]))
g

# you can plot g, but best not in R. It's better do it in Gephi. 
# R might crash as it is too big. 

# transform the graph into a matrix. 
# The sparse=FALSE argument tells it to show the 0s in the adjacency matrix. 
# the attr= n argument tells it to show the edge value
g_all <- get.incidence(g, sparse = TRUE) # this will cause R to crash
# 
# As I can't pull out the complete network at once, I could
# option 1: take out all nodes that aren't in the comm realized network
# option 2: pull out only a subset and then glue the matrices together





# OLD CODE ----------------------------------------------------------------


# go to file 'tech interface' to create the networks

# # Create network of required coordination ---------------------------------
# 
# 
# # file-by-file are the technical dependencies between software files. THese are stored in
# # DF2 for the complete software (all versions)
# # task_v2 show the technical dependencies for version 4
# # the id's in task 4 correspond to the IDs in 'ID_File_und' in DF and IDs in data set
# # 'files_xx'
# # earlier information: IDs are unique per version. 
# 
# # goal: in task_v2 replace the file_id with the owner's name
# # 
# #file 1.5 is version 2 according to the new version coding 
# # schema - Mahdi 7/9/18
# task_v2 <- DF2[DF2$file == "1.5",] 
# 
# # this is the lookup table, containing the ID (col 7) and the folder owner name (col 45)
# ownership_withid <- DF[, c(7, 45)] 
# ownership_withid <- ownership_withid[order(ownership_withid$ID_File_und),]
#  
# # match the IDs in reqcomm_v2 with the IDs in the lookup table ownership_withid. 
# # match returns a numerical vector. The number indicates the position (row number)
# # when the first match happened. In other words it checks in which row an ID mentioned 
# # in reqcomm_v2 appears in ownership_withid$ID_FILE_und. This row number is used to 
# # assign the correct owner from the vector ownership_withinid$new_owner
# 
# # This might be a bit weird, but I'm going to do mini steps to make sure it workds. 
# # Get a vector of sender and receivers. this vector contains the file ID
# sender_id <- task_v2[,1] # column 1 is und_from_file_id
# receiver_id <- task_v2[,2] # column 2 is und_to_file_id
# 
# write.csv(data.frame(sender_id = sender_id, receiver_id = receiver_id), 'sender_receiver_id_ver4.csv')
# # replace the file id with the folder owner names.
# sender_name <- ownership_withid$new_owner[match(sender_id, ownership_withid$ID_File_und)]
# receiver_name <- ownership_withid$new_owner[match(receiver_id, ownership_withid$ID_File_und)]
# write.csv(data.frame(sender_id = sender_name, receiver_name = receiver_id), 'sender_receiver_name_ver4.csv')
# 
# table(is.na(sender_name)) # 12958 instances of NA
# table(is.na(receiver_name)) # 14720 instaces of NA
# 
# # the files which don't have a folder owner name (no match) are NA for the moment. 
# # Replace NA with the original file ID number for testing purposes
# snd_na_idx <- which(is.na(sender_name)) # this returns row numbers
# sender_name[snd_na_idx] <- sender_id[snd_na_idx] # this replaces NA with the original file ID
# 
# rcv_na_idx <- which(is.na(receiver_name)) # this returns row numbers
# receiver_name[rcv_na_idx] <- receiver_id[rcv_na_idx] # this replaces NA with the original file ID
# 
# write.csv(data.frame(sender_id = sender_name, receiver_name = receiver_id), 'sender_receiver_name2_ver4.csv')
# 
# # TODO: check Mahdi's comment replace file IDs with owner from previous version
# # first file with no onwer has the file ID 52. 
# head(ownership_withid)
# ownership_withid %>% filter(ownership_withid$ID_File_und == 52) # no file 52
# # ownership_withid only for version 2?
# # No check line 242. ownership_withid is a subset of DF. DF is the complete dataset. o
# # ownership_withid only contains the columns 7 (ID_File_und) and 45 (new owners)
# # 
# # create the micro task communication edgelist
# reqcomm_v2 <- cbind(sender_name, receiver_name, task_v2$weight)
# View(reqcomm_v2)
# 
# # count number of file IDs that have no owner
# # length(snd_na_idx) + length(rcv_na_idx) # 27678 error here. These are row numbers, not file IDs
# 
# # proportion of files with no owner
# length(snd_na_idx)/dim(reqcomm_v2)[1] # 0.78
# length(rcv_na_idx)/dim(reqcomm_v2)[1] # 0.89
# 
# # files with no assigned folder owner
# no_folder_owner <- c(sender_id[snd_na_idx],receiver_id[rcv_na_idx])
# length(unique(no_folder_owner)) # 2657 files --> need artificial name as this will be a big network
# 
# length(unique(DF$ID_File_und)) # number of unique file IDs in microtask DF file
# length(unique(c(DF2$und_from_file_id, DF2$und_to_file_id))) # number of unique file IDs in task dependency file
# 
# length(unique(c(sender_id[snd_na_idx],receiver_id[rcv_na_idx])))/length(unique(c(DF2$und_from_file_id, DF2$und_to_file_id)))
# # 0.06 files in task dependencies are not matched. 
# 
# tail(sort(table(no_folder_owner)))
# # Ideal workflow: find out number of files with no owner. decide if keep file ID as artifical
# # folder owner name or replace with artifical name. 
# head(no_folder_owner)
# ggplot(as.data.frame(no_folder_owner), aes(x = no_folder_owner)) + geom_bar() + labs(x='folder_id')
# 
# # the id's that are not matched do not appear in the dataset DF
# # test assumption with id 43748, 36927, 15330
# DF[DF$ID_File_und == 15330, c(1:4, 41, 45)]
# DF[DF$ID_File_und == 43748, c(1:4, 41, 45)]
# DF[DF$ID_File_und == 36927, c(1:4, 41, 45)]
# 
# # turn the id's for which no owner was identified into 'external_owner'
# reqcomm_v2[snd_na_idx,1] <- 'external_owner'
# reqcomm_v2[rcv_na_idx,2] <- 'external_owner'
# 
# reqcomm_v2 <- as.data.frame(reqcomm_v2) # transform dataset from matrix into data frame
# names(reqcomm_v2)[3] <- 'weights' # assign weights to ownership dependencies
# 
# # remove all temporrary files
# rm(list=ls()[grep('tmp', ls())])
# 
# save.image("~/Documents/gitrepo/developer_sna/analysis/clean_data.RData")
