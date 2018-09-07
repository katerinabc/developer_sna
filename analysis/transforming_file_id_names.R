# exchanging id numbers in the file-by-file file by file names

# this is the original file with id numbers for files.
# the information in 'und_from_file_id' and 'und_to_file_id' are the file ids. The first
# column is the head file, and the 2nd column is the tail file. The tail file depends on
# information in the head file. 
# the weight indicates how often the tail file depends on input from the head file.
head(task4)

# the file id_names contains the id numbers of files per version) and the file this ID
# is referring to. 
# further down this file will also be called at idn
id_names <- read.csv("~/Documents/gitrepo/developer_sna/data/understandID_entName/files_1.5.csv", header=T, stringsAsFactors = F)
head(id_names)
str(id_names)

# comment my Mahdi:  filenames in the developer_file dataset has two or three
# additional high-level folders in the beginning of the string. 
# But you could simply decompose the string and remove the first 2 (or 3) higher level 
# foldernames and ignore them in matching.
# 
# Let's test if Mahdi is right :-)

matching_names <- data.frame(und_ent_id  = DF[DF$ver == 4, 7],
                             DF_fname  =  DF[DF$ver ==4, 2],
                             file_author = DF[DF$ver == 4,1],
                             file_owner = DF[DF$ver == 4,45])
                             
# turn the factors into characters
matching_names$DF_fname <- as.character(matching_names$DF_fname)
matching_names$file_author <- as.character(matching_names$file_author)
matching_names$file_owner <- as.character(matching_names$file_owner)

str(matching_names) #check if all ok

# merge information from matching names with the ones from id_names, 
matching_names <- merge(matching_names, id_names, key = 'und_ent_id')
#inspect the new file
head(matching_names)
# the file names coming from the DF file begins with '/trunk/src/developments/'  
# it has 3 extra levels

# Test 2a: Count number of strings in filename
filenames_bytes <- data.frame( ent_name_byte = stri_numbytes(matching_names$ent_name),
                               DF_name_byte = stri_numbytes(matching_names$DF_fname))
filenames_bytes$diff <- filenames_bytes$ent_name_byte - filenames_bytes$DF_name_byte
ggplot(filenames_bytes, aes(x = diff)) + geom_bar()
dim(filenames_bytes)
# of those filenames that can be merged by ID 
# (see line 'merge(matching_names, id_names, key = 'und_ent_id')')
# the filename from the id_name dataset are less long. 
# This mans that in the original file, DF, there are folder levels added that are not 
# included in id_names
# 
# Test 2b: Count number of strings in filename 
# Check if the observation from test 2a is also the case for IDs that did not match
# As the ID didn't match, we are going to rely on a set of stats to give a partial answer

id_names_byte <- stri_numbytes(id_names$ent_name)
DF_name_byte <-  stri_numbytes(DF$Filename)

lapply(list(id_names_byte, DF_name_byte), Hmisc::describe)
# on average the DF names are longer.
ggplot(as.data.frame(id_names_byte), aes(x = id_names_byte)) + geom_bar()
ggplot(as.data.frame(DF_name_byte), aes(x = DF_name_byte)) + geom_bar()

# DF names are generally longer. 

# the merge deletes row for which there is no match in DF. 
# but these need to be kept in 

# find for ent_name partial match in DF[DF4ver==4,] and return owner. This CAN return 
# owners who are not members of version 4. test this aferwards.

# This looks at every filename in the dataset id_names, and checks for matches in
# the DF dataset. It returns the row number of every observation in DF that matches
id_names_authors <- sapply(id_names$ent_name, function(x) 
  grep(x, DF$Filename))
# two issues: sometimes no matches. and sometimes matches many file. Its a list of list
# the number it returns is the row number. that can be used to retrieve the owner
# the many matches is not a problem. We procede like before, as the data is ordered,
# and retrieve the owner of the first instance the file appeared in DF

id_names$owners1 <- "none"
for (i in 1:length(id_names_authors)){
  rownumber <- id_names_authors[[i]][1]
  if(length(rownumber) > 0){id_names[i,3] <- DF[rownumber, 45]}
}

# how to match the longer string in id_names in the shorter string DF$filename.
# part of the beginning of DF$filename result in no string --> match with the end of the string

# does it matter if the filename strings contain the backslash ?
# to test if the backslash creates problem with matching, let's remove it.
id_names$ent_name_no_slash <- gsub("[^A-Za-z0-9]", "", id_names$ent_name)
DF$Filename_no_slash <- gsub("[^A-Za-z0-9]", "", DF$Filename)
# that wasn't helpful after all. The problem is not the backslash, but the additional 
# folders in some filename. 

id_names$short_names <- gsub("applications", "", id_names$ent_name)
id_names$short_names <- gsub("PlantVisorPRO|dataaccesslayer|services|", "", id_names$short_names)
head(id_names$short_names,10)
# it could work...
# 
# on further thought and inspection, shouldn't everything before the /src or the c_source
# be removed in the file id_names ?
?pmatch

# in some way I should be able to match substrings
id_names$ent_name[1] %in% DF$filename # returns TRUE. An ownere is assigned
id_names$ent_name[2] %in% DF$filename # returns FALSE. no ownere is assigned
table(DF$filename %in% id_names$ent_name[2]) # returns 11938x FALSE
# but %in% looks for exaxt matches

pmatch("src", id_names$ent_name_no_slash) # this should check if 'src' is included in
# any items in id_names$ent_name_no_slash. It returns NA which means no partial matches. 
# That doesn't make sense

