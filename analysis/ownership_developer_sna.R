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

length(members_vec)

DF$members <- members_vec

# visualize folder ownerships  

ggplot(DF, aes(x = folder_owner, fill = as.factor(ver))) + geom_bar() + 
  scale_fill_brewer(type="qual", guide=guide_legend(title="Version Number")) +
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

ggplot(folder_contribution, aes(value)) + geom_bar() + facet_wrap(~author)
ggsave("facet_wrap_developer_contribution_per_folder.png")


# create ownership of folders per version.
# ownership can not be re-gained
# workflow: 
# 1. take subset of folders with modification in version x
#own <- DF[,c(1, 3, 6, 39,40,42,43)]
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
# 
# Assigning new folder owners.
# A loop is creating going through the sequence of numbers in the index idx_nonmembers
# For every i, get the version number (column 6), folder name (column 42)
# Subset 1: Get all developers who contributed to tmp_folder using the lookup table
# contr_now
# Subset 2: Subset the data file tmp_contributors to only get the developers who 
# contributed to tmp_version Of this list of developers, pick the first one with the 
# highest amount of contributions. Assign this name to 'new owner' (column 45 in DF)
#sink("new_owner_test.txt") #sink writes the output to a text file for inspection
# used to test the loop. 

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

View(DF[DF$members == FALSE, c(43:45)])

table(DF[DF$members == FALSE, 43] == DF[DF$members == FALSE, 45])
# 2x the new owner is the same as the old owner, and not in the version. 
# inspect

View(DF[(DF$members == FALSE) & (DF$folder_owner == DF$new_owner),c(1,3, 6:7, 43:45)])
# paride.ciatto is in version 6 - 13 --> he must be in version 5. he is listd there several times
# longbow.liu is in versino 7 - 13

View(DF[DF$author == "paride.ciatto", c(1,3, 6:7)])
View(DF[DF$author == "longbow.liu", c(1,3, 6:7)])


ownership_change <- reshape2::melt(table(DF$folder_owner, DF$new_owner))
ownership_change <- ownership_change[-which(as.character(ownership_change$Var1) == 
                                              as.character(ownership_change$Var2)),]
ownership_change <- ownership_change[!ownership_change$value == 0,]

ggplot(ownership_change, aes(x = Var1, y = Var2, fill = value)) + geom_raster() + 
  labs(title="Change in ownership of folders", x = 'Original Owner', y = 'New Owner') +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('ownership_change.png')

# Create network of required coordination
# file-by-file are the technical dependencies between software files. THese are stored in
# DF2 for the complete software (all versions)
# task4 show the technical dependencies for version 4
# the id's in task 4 correspond to the IDs in 'ID_File_und' in DF and IDs in data set
# 'files_xx'
# earlier information: IDs are unique per version. 
# goal: in task4 replace the file_id with the owner's name

task4 <- DF2[DF2$file == "1.5.csv",]

head(task4)
# subset task4 to the technical dependencies
reqcommv4 <- task4[,c(1:2)]

# this is the lookup table, containing the ID (col 7) and the folder owner name (col 45)
ownership_withid <- DF[DF$ver == 4 , c(7, 45)] 

# It needs to be the complete dataset (DF) for when a file hasn't been modified in this
# version. if the file hasn't been modified in ver4, it will not be shown in 
# dev4_raw. This will result in NA for when matching needed and required 
# communication
 

# match the IDs in reqcomm4 with the IDs in the lookup table ownership_withid. 
# match returns a numerical vector. The number indicates the position (row number)
# when the first match happened. In other words it checks in which row an ID mentioned 
# in reqcommv4 appears in ownership_withid$ID_FILE_und. This row number is used to 
# assign the correct owner from the vector ownership_withinid$new_owner
reqcommv4[] <- ownership_withid$new_owner[match(
  unlist(reqcommv4), ownership_withid$ID_File_und)]

reqcommv4$weight <- task4[,3] # add the weights of the technical dependencies

# check how many files were not matched. 
table(is.na(reqcommv4))

head(DF[DF$ver == 4, c(7,45)])
DF[DF$ID_File_und == 8721, c(6, 7,45)]
DF[DF$ID_File_und == 36927, c(6, 7,45)]
DF[DF$ID_File_und == 58806, c(6, 7,45)]
DF[DF$ID_File_und == 122539, c(6, 7,45)]

# OLD EXPLANATION. STILL VALID ??
# the files in reqcommv4 who have NA have not been modified in version 4. as the Id's are 
# unique per version it is not possible to indicate to what folder this file belongs.
# therefore it is not possible to assign ownership. 
# a fictious developer ('notmodified') is assigned to these files

# NEW EXPLANATION: 
# The IDs that have not been matched with a developer name have not been modified in 
# ver 4, but might have been in a previous version. check the previous version etc
# remember the splitting of software !!!

idx <- which(is.na(reqcommv4$und_from_file_id))
reqcommv4[idx,1] <- 'not.modified'

idx <- which(is.na(reqcommv4$und_to_file_id))
reqcommv4[idx,2] <- 'not.modified'


# remove all temporrary files
rm(list=ls()[grep('tmp', ls())])

save.image("~/Documents/gitrepo/developer_sna/analysis/clean_data.RData")
