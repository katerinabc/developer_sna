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
  dat$file <- unlist(strsplit(f,split="_",fixed=T))[3]
  DF <- rbind(DF, dat)
}

# take out files with no ID number. Error with Understand

DF <- DF[-which(is.na(DF$ID_File_und)),]

#load the task dependencies
myFiles2 = list.files(path="~/Documents/gitrepo/developer_sna/data/file-by-file", 
                      pattern="*.csv", full.names = T)
DF2 <- NULL
for (f in myFiles2) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", stringsAsFactors = F)
  dat$file <- unlist(strsplit(f,split="_",fixed=T))[3]
  DF2 <- rbind(DF2, dat)
}

# author attributes
authatt <- read.csv("~/Documents/gitrepo/developer_sna/data/authors_022018.csv", sep=",", header=T)


# take care. check structure. lots of variables read as characters
# # create DV network -------------------------------------------------------
# 
# # DF is edgelist (instance list) in the format: author - file 
# bg <- graph.empty(directed = F)
# node.out <- unique(DF$author) 
# node.in <- unique(DF$Filename) 
# bg <- add.vertices(bg,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
# bg <- add.vertices(bg,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
# edge.list.vec <- as.vector(t(as.matrix(data.frame(DF[,c(1:2)]))))
# bg <- add.edges(bg,edge.list.vec)
# bg
# #View(get.incidence(bg))
# 
# pr <- bipartite.projection(bg) 
# 
# developer <- get.adjacency(pr$proj1,sparse=FALSE,attr="weight")
# #tasks <- get.adjacency(pr$proj2,sparse=FALSE,attr="weight")
# 
# # developer network
# dev_v1 <- devnetwork(DF, 1)
# 
# 
# # Add attributes to developer network -------------------------------------
# 
# #check if name in author att same order as in network file
# match(rownames(developer), authatt$author) #not a match. sort authatt file
# match(authatt$author, rownames(developer))
# authatt <- authatt[match(row.names(developer), authatt$author),]
# 
# developer_net <- network(developer, directed=F, matrix.type="a",ignore.eval=FALSE, names.eval="frequency")
# 
# developer_net%v%"id" <- authatt$ID_author
# developer_net%v%"title" <- authatt$jobtitle_raw
# developer_net%v%"loc" <- authatt$location
# developer_net%v%"contract" <- authatt$contract


# File-by-file ------------------------------------------------------------


