# data import
# 
# # Libraries ---------------------------------------------------------------
library(sna)
library(network)
library(foreign)
library(igraph)
#library(ergm.userterms)
library(ggplot2)

source('functions_developer_sna.R')
# data --------------------------------------------------------------------

# load the microtasks
myFiles = list.files(path="~/Documents/gitrepo/developer_sna/data/developer-by-file", 
                     pattern="*.csv", full.names = T)
DF <- NULL
for (f in myFiles) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", stringsAsFactors = F)
  #filename <- gsub(".csv", '', unlist(strsplit(f,split="_",fixed=T))[3])
  #filename2 <- gsub(".csv", '', filename)
  dat$file <- gsub(".csv", '', unlist(strsplit(f,split="_",fixed=T))[3])
  DF <- rbind(DF, dat)
}

# correct the version numbers
version_transformation <- cbind(ori = as.character(c('1.4', '1.4build1', '1.4build3', '1.5', '1.5.2', '1.5.3', "1.5.4", "1.5.5", "2.0.0" , "2.0.1", "2.0.2", "2.0.3", "2.1.0")),
                                      new = as.character(c(1,   1,               1,     2,    3,       4, 5     , 6,     7,     8,     9,    10 , 11)))

DF$ver <- version_transformation[,2][match(DF$file, version_transformation[,1])]

# take out files with no ID number. Error with Understand
DF <- DF[-which(is.na(DF$ID_File_und)),]

#load the task dependencies
myFiles2 = list.files(path="~/Documents/gitrepo/developer_sna/data/file-by-file", 
                      pattern="*.csv", full.names = T)
DF2 <- NULL
for (f in myFiles2) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", stringsAsFactors = F)
  dat$file <- unlist(strsplit(f,split="_",fixed=T))[3]
  dat$file <- gsub(".csv", '', unlist(strsplit(f,split="_",fixed=T))[3])
  DF2 <- rbind(DF2, dat)
}

DF2$ver <- version_transformation[,2][match(DF2$file, version_transformation[,1])]
  
# author attributes
authatt <- read.csv("~/Documents/gitrepo/developer_sna/data/authors_022018.csv", sep=",", header=T)

# the version numbers need to be adapted according to Mahdi's new schema
version_transformation <- cbind(version_transformation, 
                                ver_author = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)))
version_transformation <- version_transformation[,c(1,3,2)]
# check if new version numbering is ok, then override all ver values
authatt$ver2 <- as.numeric(version_transformation[,3][match(authatt$ver, version_transformation[,2])])
authatt$ver <- as.numeric(version_transformation[,3][match(authatt$ver, version_transformation[,2])])

# take care. check structure. lots of variables read as characters



