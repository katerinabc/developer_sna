# Define ownership network

source('data_import.R', echo=T)

own <- DF[DF$action == 'A',c(1,39, 40)] # ME: Use filename column with short filenames
dim(own)
own <- own[order(own$ID_rev),]
table(duplicated(own[,2]))
own <- own[!duplicated(own[,2]),] # remove duplicates
library(pathological)

names(own)
own <- own[,-2]

get_folder_name <- function(x){
  x <- pathological::decompose_path(x)
  x <- x[,1]
  x <- gsub('/Users/katerinadoyle/Documents/gitrepo/developer_sna/analysis/',
            '', x)[1]
  return(x)
}

own$folder_names <- unlist(lapply(own[,2], get_folder_name)) # returns the foldername

# TOD: check if it correctly identifies file names without a file. when someone creates a folder

# remove duplicates from the folder_names file. The first instance is always kept
# this will be the folder owner
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
#folder owner member of version

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
DF$folder_owner <- as.character(DF$folder_owner)

authatt$author <- as.character(authatt$author)

# how often has a developer be assigned to a folder, but isn't member in that version
table(DF$folder_owner, DF$members)

#top contributor per version per folder
library(dplyr)
# look up table
contr_now <- DF %>% group_by(ver, folder_names, author) %>% summarize(contribution = n())
contr_now <- as.data.frame(contr_now %>%group_by(ver, folder_names) %>% top_n(1, contribution))

idx_nonmembers <- which(DF$members == FALSE)

DF$new_owner <- DF$folder_owner
for (i in idx_nonmembers){
  #print(i)
  tmp_version <-DF[i,6]
  tmp_folder <- DF[i, 42]
  tmp_contributors <- contr_now[contr_now$ver == tmp_version,]
  tmp_folder_contributor <- tmp_contributors[
    tmp_contributors$folder_names == tmp_folder,3][1] # pick first developer when 2 tie in contribution
  DF[i,45] <- tmp_folder_contributor
  
}

ownership_change <- reshape2::melt(table(DF$folder_owner, DF$new_owner))
ownership_change <- ownership_change[-which(as.character(ownership_change$Var1) == 
                                              as.character(ownership_change$Var2)),]
ownership_change <- ownership_change[!ownership_change$value == 0,]

ggplot(ownership_change, aes(x = Var1, y = Var2, fill = value)) + geom_raster() + 
  labs(title="Change in ownership of folders", x = 'Original Owner', y = 'New Owner') +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('ownership_change.png')

# Create network of required coordination based
# file file-by-file are the technical dependencies between software files
# task4 show the technical dependencies for version 4
# the id's in task 4 are unique per version
# question: if a file wasn't modified it is not listed in developer-by-file, but in file-by-file
# and the id in file-by-file is not the same than the file would have had in the previous version
# if yes, How do I know the owner of that
# 
# goal: in task4 replace the file_id with the owner's name

task4 <- DF2[DF2$file == "1.5.csv",]

head(task4)
#data frame that needs to be converted
reqcommv4 <- task4[,c(1:2)]
# this is the lookup table, containing the ID
# and the folder owner name


ownership_withid <- DF[DF$ver == 4 , c(7, 45)] 

# It needs to be the complete dataset (DF) for when a file hasn't been modified in this
# version. if the file hasn't been modified in ver4, it will not be shown in 
# dev4_raw. This will result in NA for when matching needed and required 
# communication
reqcommv4[] <- ownership_withid$new_owner[match(
  unlist(reqcommv4), ownership_withid$ID_File_und)]
reqcommv4$weight <- task4[,3]

table(is.na(reqcommv4))

head(DF[DF$ver == 4, c(7,45)])
DF[DF$ID_File_und == 8721, c(6, 7,45)]
DF[DF$ID_File_und == 36927, c(6, 7,45)]
DF[DF$ID_File_und == 58806, c(6, 7,45)]
DF[DF$ID_File_und == 122539, c(6, 7,45)]

# the files in reqcommv4 who have NA have not been modified in version 4. as the Id's are 
# unique per version it is not possible to indicate to what folder this file belongs.
# therefore it is not possible to assign ownership. 
# a fictious developer ('notmodified') is assigned to these files

idx <- which(is.na(reqcommv4$und_from_file_id))
reqcommv4[idx,1] <- 'not.modified'

idx <- which(is.na(reqcommv4$und_to_file_id))
reqcommv4[idx,2] <- 'not.modified'


# remove all temporrary files
rm(list=ls()[grep('tmp', ls())])

save.image("~/Documents/gitrepo/developer_sna/analysis/clean_data.RData")
