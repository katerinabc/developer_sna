# Script for Collaboration Analysis
# Research Owner: Mahdi
# latest update: 21st May 2018
# 
# 

# questions ---------------------------------------------------------------

# actual communication patterns
# 
#clean up
rm(list=ls())
# Libraries ---------------------------------------------------------------
library(sna)
library(network)
library(statnet)
library(foreign)
library(ergm)
library(ergm.userterms)
library(ggplot2)

# data --------------------------------------------------------------------

# load the microtasks
myFiles = list.files(path="~/Documents/gitrepo/developer_sna/data/developer-by-file", 
                     pattern="*.csv", full.names = T)
DF <- NULL
for (f in myFiles) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(DF, dat)
}

#load the task dependencies
myFiles2 = list.files(path="~/Documents/gitrepo/developer_sna/data/file-by-file", 
                     pattern="*.csv", full.names = T)
DF2 <- NULL
for (f in myFiles2) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF2 <- rbind(DF2, dat)
}

# author attributes
authatt <- read.csv("~/Documents/gitrepo/developer_sna/data/authors_022018.csv", sep=",", header=T)


# create DV network -------------------------------------------------------

# DF is edgelist (instance list) in the format: author - file 
bg <- graph.empty(directed = F)
node.out <- unique(DF$author) 
node.in <- unique(DF$Filename) 
bg <- add.vertices(bg,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
bg <- add.vertices(bg,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
edge.list.vec <- as.vector(t(as.matrix(data.frame(DF[,c(1:2)]))))
bg <- add.edges(bg,edge.list.vec)
bg
View(get.incidence(bg))

pr <- bipartite.projection(bg) 

developer <- get.adjacency(pr$proj1,sparse=FALSE,attr="weight")
tasks <- get.adjacency(pr$proj2,sparse=FALSE,attr="weight")

# developer network
# function to get network data for one software version
devnetwork <- function(dataset, version_number){
  small_subset <- dataset[dataset$ver ==version_number,]
  bg <- graph.empty(directed = F)
  node.out <- unique(small_subset$author) 
  node.in <- unique(small_subset$Filename) 
  bg <- add.vertices(bg,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
  bg <- add.vertices(bg,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
  edge.list.vec <- as.vector(t(as.matrix(data.frame(small_subset[,c(1:2)]))))
  bg <- add.edges(bg,edge.list.vec)
  pr <- bipartite.projection(bg) 
  return(get.adjacency(pr$proj1,sparse=FALSE,attr="weight"))
}

draw_dev_net <- function(network, )
dev_v1 <- devnetwork(DF, 1)


# Add attributes to developer network -------------------------------------

#check if name in author att same order as in network file
match(rownames(developer), authatt$author) #not a match. sort authatt file
authatt <- authatt[match(row.names(developer), authatt$author),]

developer_net <- network(developer, directed=F, matrix.type="a",ignore.eval=FALSE, names.eval="frequency")

developer_net%v%"id" <- authatt$ID_author
developer_net%v%"title" <- authatt$jobtitle_raw
developer_net%v%"loc" <- authatt$location
developer_net%v%"contract" <- authatt$contract

# visualization -----------------------------------------------------------
library(igraph)
library(ggraph)

#better in Gephi for complete data ?

names(DF2)
g2 <- graph.adjacency(dev_v1, mode="directed", weighted=TRUE)
g2

ggraph(g2) + # raw depiction. improve for publication
  geom_edge_link(aes(edge_width = weight)) + #width = size arguemtn?
  geom_node_point()

dev_igraph <- graph.adjacency(developer, weighted=T)
V(dev_igraph)$location <- authatt$location
V(dev_igraph)$title <- authatt$jobtitle_raw

ggraph(dev_igraph) + 
  geom_edge_link(aes(edge_width = weight/1000)) + 
  geom_node_point(aes(shape = as.factor(location), color = as.factor(title)))
ggsave("developer_1_mode_projection.png")

# Network description -----------------------------------------------------

#size, degree nbr networks, 
#


# Task 1: replicate study 1------------------------------------------------------------------
# replicate Sosa (2004) study @ network level 

# log-linear p1 model --> why ?



# Task 2 ------------------------------------------------------------------
# logit p* --> do ergm directly --> valued ergm
# 
# hypothesized effects: 
# 
# DV: team collaboration
# Network variable: 2 mode network of developer and workflow
# Step 1: project into 1 mode network (developer - developer assumed communication?)
# the DV network is developer
# can't use task attribute
# only use author attributes and structural effects  

# the names of the model is in the format of {task number} {model number}. 
# t1m1 would be task 1 model 1

t2m1 <- ergm(developer_net ~ sum + 
               nodematch("loc") + nodematch("title") # homophily theory
             , response="frequency", reference=~Geometric)
summary(t2m1)
mcmc.diagnostics(t2m1)
#cross correlation could be a bit better
#autocorrelation is good. good normal distribution of values

# lets add some structural features: mutuality, triadic closure
# 1. nodesqrtcovar measures individual propensity to engage in a task. It is the valued
# version of kstar
# 2. triadic closure measures individual propensity to form triangles.
# transitiveweights(twopath, combine, affect) and cyclicalweights statistics.
#
t2m2 <-  ergm(developer_net ~ sum + nonzero()
                #+ nodematch("loc") + nodematch("title") # homophily theory
               # + nodesqrtcovar(center=T)
                #+ transitiveweights("min","max","min")
               # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m2.pdf")
summary(t2m2)
mcmc.diagnostics(t2m2)
dev.off()

t2m3<-  ergm(developer_net ~ sum + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              #+ transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m3.pdf")
summary(t2m3)
mcmc.diagnostics(t2m3)
dev.off()

# notes: error: infinite values in x
#
t2m4 <-  ergm(developer_net ~ sum + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m4.pdf")
summary(t2m4)
mcmc.diagnostics(t2m4)
dev.off()

# converges with no error. 
# I tested also adding cyclicalweights("min","max","min") 
# that resulted in an error "infinite or missing values in 'x'"
# sample space not properly used.
# is this ok for valued ergms?
# 
t2m5 <-  ergm(developer_net ~ sum + nonzero()
              + nodematch("loc") + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m5.pdf")
summary(t2m5)
mcmc.diagnostics(t2m5)
dev.off()
#Error in eigen(crossprod(x1c), symmetric = TRUE) : 
# infinite or missing values in 'x'

t2m6 <-  ergm(developer_net ~ sum + nonzero()
              + nodematch("loc") # homophily theory
              #+ nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              , response="frequency", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) : infinite or missing values in 'x'
t2m7 <-  ergm(developer_net ~ sum + nonzero()
            #  + nodematch("loc") # homophily theory
              + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              , response="frequency", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) : infinite or missing values in 'x'

t2m8 <-  ergm(developer_net ~ sum + nonzero()
              + nodematch("loc", diff=T) # homophily theory
             # + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              , response="frequency", reference=~Geometric)

# Error in eigen(crossprod(x1c), symmetric = TRUE) : infinite or missing values in 'x'

t2m9 <-  ergm(developer_net ~ sum + nonzero()
             # + nodematch("loc", diff=F) # homophily theory
              + nodematch("title", diff=T) # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              , response="frequency", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) : infinite or missing values in 'x'
# 
# note 29/05/2018: t2m4 was the last one that converged without an error
# what to do with mcmc diagnostics plots???

pdf("t2m5.pdf")
summary(t2m6)
mcmc.diagnostics(t2m6)
dev.off()

# the effect size of nonzero is NA. Run model 4 again without nonzero effect
# 

t2m4.1 <-  ergm(developer_net ~ sum 
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m4_1.pdf")
summary(t2m4.1)
mcmc.diagnostics(t2m4.1)
dev.off()
# Error in eigen(crossprod(x1c), symmetric = TRUE) : 

t2m4.2 <-  ergm(developer_net ~ sum + nonzero()
                + absdiffcat("title")
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)

# Error in eigen(crossprod(x1c), symmetric = TRUE) :

t2m4.3 <-  ergm(developer_net ~ sum + nonzero()
                + absdiffcat("loc")
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) 
t2m4.4 <-  ergm(developer_net ~ sum + nonzero()
                + nodematch("contract")
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) : 
# error means NA in correlation matrix. can't compute eigen values

table(is.na(authatt))
table(is.na(developer))

# no NA in authatt and developer

table(is.na(developer_net%v%"contract"))
table(is.na(developer_net%v%"loc"))
table(is.na(developer_net%v%"title"))
table(is.na(developer_net%e%"frequency"))

# no NA in the data. Then I must have infinite values in x
# maybe I have this issue: 
# Warning: Parameter space constrints 
# What happens if we simulate from a geometric-
# reference ERGM with all coe cients set to 0?

# Option 1: try with other reference distritbution (Poission)
# if not work, limit parameter space to max theoretical number of frequency

t2m4.5 <-  ergm(developer_net ~ sum + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
               + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              # + cyclicalweights("min","max","min")
              , response="frequency", reference=~Poisson)
# this works. great. but is it the right reference distribution?
pdf("t2m4_5.pdf")
summary(t2m4.5)
mcmc.diagnostics(t2m4.5)
dev.off() 
# autocorrelation is a bit off. It doesn't look like an
# upward or downward trend. 
# diff mcmc sampling criteria could fix the problem

t2m4.6 <-  ergm(developer_net ~ sum + nonzero()
              + nodematch("loc") 
              + nodematch("title") # homophily theory
              + nodematch("contract")
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Poisson)
# Approximate Hessian matrix is singular

t2m4.7 <-  ergm(developer_net ~ sum + nonzero()
              + nodematch("loc") 
             # + nodematch("title") # homophily theory
            #  + nodematch("contract")
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Poisson)
# MCMLE estimation did not converge after 20 iterations

t2m4.8 <-  ergm(developer_net ~ sum + nonzero()
             # + nodematch("loc") 
              + nodematch("title") # homophily theory
             # + nodematch("contract")
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Poisson)
# Approximate Hessian matrix is singular
# 
pdf("t2m4_8.pdf")
summary(t2m4.8)
mcmc.diagnostics(t2m4.8)
dev.off() 
# mcmc smapling total off for nonzero parameter

t2m4.9 <-  ergm(developer_net ~ sum + nonzero()
             # + nodematch("loc") 
             # + nodematch("title") # homophily theory
              + nodematch("contract")
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Poisson)

# It seems like the attribute contract results in a worser model based on the mcmc
# Decisions: 
# 1. test t2m4.9. 
# 2. increase mcmc sample size for t2m4.7
# 3. test a odel with location and contract


# failed models -----------------------------------------------------------


t2m5 <-  ergm(developer_net ~ sum + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
               + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
# error Error in eigen(crossprod(x1c), symmetric = TRUE) : 
# infinite or missing values in 'x'
# nonzero has NA as estimate. take out?
# 
# pdf("t2m5.pdf")
# mcmc.diagnostics(t2m5)
# dev.off()

t2m6 <-  ergm(developer_net ~ sum# + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)

t2m7 <-  ergm(developer_net ~ sum# + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min", "max", "min")
              #+ cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)


t2m7 <-  ergm(developer_net ~ sum + nonzero()
              #+ nodematch("loc") + nodematch("title") # homophily theory
              + nodesqrtcovar(center=T)
              + transitiveweights("min","max","min")
              + cyclicalweights("min","max","min")
              , response="frequency", reference=~Geometric)
pdf("t2m6.pdf")
summary(t2m6)
mcmc.diagnostics(t2m6)
dev.off()


# Task 3 ------------------------------------------------------------------
# longitudinal


