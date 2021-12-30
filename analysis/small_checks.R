# Checks and small tests
# 


# file_id checks ----------------------------------------------------------

#files "developer-by-file_xx" you could also find the column "ID_File_und" 
#which is the correspondence of IDs in the files "file-by-file_xx" and "files_xx"

# developer-by-file_xx is the file DF (script: data_import)
# file_by_file is the file DF2 (task dependencies) (script: data_import)
# file_xx is the file id_names (script transforming_file_id_names)

# unique names
nrow(id_names) == length(unique(id_names$und_ent_id))
gi
#how many folders without owner
length(DF[DF$new_owner == "not.modified", 43])



# ownership assignment ----------------------------------------------------

# take this step apart
reqcommv4_2 <- task4[,c(1:2)] # for testing purpose
#reqcommv4_2[] <- ownership_withid$new_owner[match(
#  unlist(reqcommv4_2), ownership_withid$ID_File_und)]

matchidtest <- match(
  unlist(reqcommv4_2), ownership_withid$ID_File_und, nomatch= "no.owner")
View(matchidtest)
length(matchidtest)
dim(reqcommv4_2)
length(unlist(reqcommv4_2))
matchidtest <- as.data.frame(matchidtest) # remember this is the row number inownership_withid
matchidtest$original_id <- unlist(reqcommv4_2)
matchidtest$matchedid <- ownership_withid$ID_File_und[matchidtest$matchidtest]
str(matchidtest)
View(matchidtest)

ownership_withid[215,]


# error 1: new onwership assignment. sometimes new owner is same as old owner, and not in 
# version --> solution: edited the code. Now this happens twice
# error 2: 


library(readr)
Ver5 <- read_csv("~/Documents/gitrepo/developer_sna/data/developer-by-file/developer-by-file_1.5.2.csv")
table(Ver5$author)
View(Ver5)