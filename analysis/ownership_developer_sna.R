# Define ownership network
library(pathological)
source('data_import.R', echo=T)

# Create dataset 'own. This dataset contains the file when they were created (action = A) 
# using the shorter filename (column 40), author (column 1). column 39 contains ID rev.
# ID rev is needed to make sure that the files are sorted properly. 
# 
# the data frame own is a look up data frame to check who first created a folder. It does
# not contain folder onwership transfer when members change.
own <- DF[DF$action == 'A',c(1,39, 40)] # ME: Use filename column with short filenames
dim(own)
own <- own[order(own$ID_rev),] # sort the files
table(duplicated(own[,2])) # check for duplicate filenames
own <- own[!duplicated(own[,2]),] # remove duplicates


names(own)
own <- own[,-2] # remove the colum ID rev as not needed in the future

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

# To the data frame own we are going to add the names of the folder. This is done by 
# looping through each row in DF. lapply applies the function 'get_folder_name' to all
# entries (rows) in own[,2]
own$folder_names <- unlist(lapply(own[,2], get_folder_name)) # returns the foldername

# TOD: check if it correctly identifies file names without a file. when someone creates a folder

# remove duplicates from the folder_names file. The first instance is always kept
# this will be the folder owner.
# 
# 4/9 KBC: not sure why I needed to remove duplicates again. A reason could be that
# certain files have the same name, but different extensions. 
dim(own)
head(own)
own <- own[!duplicated(own[,3]),]
dim(own)
head(own)

# add to DF one column with folder_names
# the function decompose_path throws an error if there are dupliate folder 
# names. to avoid this, loop over folder names, create vector with names
# and add to DF 
tmp2 <- NULL
for (i in 1:nrow(DF)){
  tmp <- get_folder_name(DF[i,40])
  tmp2 <- c(tmp2, tmp)
}

DF$folder_names <- tmp2

# Add folder_owner to the file DF
# 
# The following steps are applied: 
# 1. Get the folder owner for a folder based on who first created the folder. 
# 2. Check if folder owner member in this version. 
# 2.a If yes, no changes 
# 2.b If no get new folder owner. New folder owner is person who worked on the file 
# in the current version


folder_owner <- NULL
for (i in 1:nrow(DF)){
  # get the folder name from the developer-by-file file
  tmp_foldername <- DF[i, 42]
  # match the folder name from step 1 with the folder name from the own object,
  # and return the name of the folder owner
  # assignes the name of the folder owner to the temporary object tmp_flder_owner
  if (length(own[own$folder_names == tmp_foldername,1])==0){tmp_folder_owner <- 'error'}
  # error added when folder owner not found in previous version
  else{
    tmp_folder_owner <- own[own$folder_names == tmp_foldername,1]
  }
  # add the folder owner to a new vector
  folder_owner <- c(folder_owner, tmp_folder_owner)
}

DF$folder_owner <- folder_owner

# create a logical vector indicating if FO is member in the project version
members_vec <- NULL
for (i in 1:nrow(DF)){
  tmp_owner <- DF[i, 43]
  tmp_version <- DF[i, 6]
  if(tmp_owner %in% authatt[authatt$ver == tmp_version,2]){
    tmp_member <- TRUE
  }
  else{
    tmp_member <- FALSE
  }
  members_vec <- c(members_vec, tmp_member)
}

DF$members <- members_vec

# visualize folder ownerships  

ggplot(DF, aes(x = folder_owner, fill = as.factor(ver))) + geom_bar() + 
  scale_fill_discrete(guide=guide_legend(title="Version Number")) +
  coord_flip() + 
  labs("Ownership distribution by software version", 
       x = 'Frequency', y = 'Developer')
ggsave("onwership_frequency_all_versions.png")

# per folder and version, calculate how much a developer contributed
# make sure to take care of the two software branches
# 
folder_contribution <- reshape2::melt(table(DF[,c(1,42)]))
head(folder_contribution)
folder_contribution$folder_names <- stringr::str_sub(folder_contribution$folder_names, start= -5)
folder_contribution <- folder_contribution[-which(folder_contribution$value == 0),]
dim(folder_contribution)
ggplot(folder_contribution, aes(x = author, y = folder_names)) + 
  geom_tile(aes(fill = value)) + labs(title="Developer contribution to folders", 
                                      x = 'Developers',
                                      y = 'Folder Names') +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
ggsave("developer_contribution_per_folder.png")

ggplot(folder_contribution, aes(value)) + geom_bar() + facet_wrap(~author) + 
  labs("Authors contribution to folders",
       x= 'Level of Contribution per folder',
       y = 'Count of contribution levels')
ggsave("facet_wrap_developer_contribution_per_folder.png")


# create ownership of folders per version.
# Ownership is based on contribution to folders
# ownership can not be re-gained by re-joining the team.

## make sure folder owners are in character typs and not categories
DF$folder_owner <- as.character(DF$folder_owner) 
authatt$author <- as.character(authatt$author)

# how often has a developer be assigned to a folder, but isn't member in that version
table(DF$folder_owner, DF$members)

#top contributor per version per folder
library(dplyr)
# look up table
# This is creating a lookup table with the following information: 
# group DF by version number, folder names and authors and add a column with info about
# how often the author made a contribution to a specific folder in a specific version.
contr_now <- DF %>% group_by(ver, folder_names, author) %>% summarize(contribution = n())
# transform the tiddy table into a data frame and keep only the top 1 (highest) contribution
contr_now <- as.data.frame(contr_now %>%group_by(ver, folder_names) %>% top_n(1, contribution))

# creating an index with row numbers of those developers who are not a member in 
# version x but author of a folder that has been modified in version x
idx_nonmembers <- which(DF$members == FALSE)

# copy the folder owner names. These will be overwrittenusing the loop if necessary 
DF$new_owner <- DF$folder_owner
 
# Assigning new folder owners.
# A loop is created going through the sequence of numbers in the index idx_nonmembers
# For every i, get the version number (column 6), folder name (column 42)
# Subset 1: Get all developers who contributed to tmp_folder using the lookup table
# contr_now
# Subset 2: Subset the data file tmp_contributors to only get the developers who 
# contributed to tmp_version Of this list of developers, pick the first one with the 
# highest amount of contributions. Assign this name to 'new owner' (column 45 in DF)
# sink("new_owner_test.txt") #sink writes the output to a text file for inspection
# used to test the loop. Commented out once code is working

for (i in idx_nonmembers){
  tmp_version <-DF[i,6]
  tmp_folder <- DF[i, 42]
  tmp_contributors <- contr_now[contr_now$folder_names == tmp_folder,]
  tmp_folder_contributor <- tmp_contributors[
    tmp_contributors$ver == tmp_version,3][1] # pick first developer when 2 tie in contribution
  
  DF[i,45] <- tmp_folder_contributor
  #print(i)
  #print(c(tmp_version, tmp_folder, tmp_contributors, tmp_folder_contributor))
  
}
#sink()

View(DF[DF$members == FALSE, c(43:45)]) # visual inspection of folder owner changes

table(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45])
# none true. 

ownership_change <- reshape2::melt(table(DF$folder_owner, DF$new_owner))
ownership_change <- ownership_change[-which(as.character(ownership_change$Var1) == 
                                              as.character(ownership_change$Var2)),]
ownership_change <- ownership_change[!ownership_change$value == 0,]

ggplot(ownership_change, aes(x = Var1, y = Var2, fill = value)) + geom_raster() + 
  labs(title="Change in ownership of folders", x = 'Original Owner', y = 'New Owner') +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('ownership_change.png')



# Create network of required coordination ---------------------------------


# file-by-file are the technical dependencies between software files. THese are stored in
# DF2 for the complete software (all versions)
# task_v2 show the technical dependencies for version 4
# the id's in task 4 correspond to the IDs in 'ID_File_und' in DF and IDs in data set
# 'files_xx'
# earlier information: IDs are unique per version. 

# goal: in task_v2 replace the file_id with the owner's name
# 
#file 1.5 is version 2 according to the new version coding 
# schema - Mahdi 7/9/18
task_v2 <- DF2[DF2$file == "1.5",] 

# this is the lookup table, containing the ID (col 7) and the folder owner name (col 45)
ownership_withid <- DF[, c(7, 45)] 
ownership_withid <- ownership_withid[order(ownership_withid$ID_File_und),]
 
# match the IDs in reqcomm_v2 with the IDs in the lookup table ownership_withid. 
# match returns a numerical vector. The number indicates the position (row number)
# when the first match happened. In other words it checks in which row an ID mentioned 
# in reqcomm_v2 appears in ownership_withid$ID_FILE_und. This row number is used to 
# assign the correct owner from the vector ownership_withinid$new_owner

# This might be a bit weird, but I'm going to do mini steps to make sure it workds. 
# Get a vector of sender and receivers. this vector contains the file ID
sender_id <- task_v2[,1]
receiver_id <- task_v2[,2]

# replace the file id with the folder owner names.
sender_name <- ownership_withid$new_owner[match(sender_id, ownership_withid$ID_File_und)]
receiver_name <- ownership_withid$new_owner[match(receiver_id, ownership_withid$ID_File_und)]

# the files which don't have a folder owner name (no match) are NA for the moment. 
# Replace NA with the original file ID number for testing purposes
snd_na_idx <- which(is.na(sender_name))
sender_name[snd_na_idx] <- sender_id[snd_na_idx]

rcv_na_idx <- which(is.na(receiver_name))
receiver_name[rcv_na_idx] <- receiver_id[rcv_na_idx]

# create the micro task communication edgelist
reqcomm_v2 <- cbind(sender_name, receiver_name, task_v2$weight)
View(reqcomm_v2)

# count number of file IDs that have no owner
length(snd_na_idx) + length(rcv_na_idx) # 27678

# files with no assigned folder owner
no_folder_owner <- c(sender_id[snd_na_idx],receiver_id[rcv_na_idx])
length(unique(no_folder_owner)) # 2657 files --> need artificial name as this will be a big network

tail(sort(table(no_folder_owner)))
# Ideal workflow: find out number of files with no owner. decide if keep file ID as artifical
# folder owner name or replace with artifical name. 
ggplot(as.data.frame(no_folder_owner), aes(x = no_folder_owner)) + geom_bar()

# the id's that are not matched do not appear in the dataset DF
# test assumption with id 43748, 36927, 15330
DF[DF$ID_File_und == 15330, c(1:4, 41, 45)]
DF[DF$ID_File_und == 43748, c(1:4, 41, 45)]
DF[DF$ID_File_und == 36927, c(1:4, 41, 45)]

# turn the id's for which no owner was identified into 'external_owner'
reqcomm_v2[snd_na_idx,1] <- 'external_owner'
reqcomm_v2[rcv_na_idx,2] <- 'external_owner'

reqcomm_v2 <- as.data.frame(reqcomm_v2) # transform dataset from matrix into data frame
names(reqcomm_v2)[3] <- 'weights' # assign weights to ownership dependencies

# remove all temporrary files
rm(list=ls()[grep('tmp', ls())])

save.image("~/Documents/gitrepo/developer_sna/analysis/clean_data.RData")
