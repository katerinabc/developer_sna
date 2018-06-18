# Script for Collaboration Analysis
# Research Owner: Mahdi
# latest update: june 18th 
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
devnetwork <- function(dataset, version_number=NULL){
  require(igraph, quietly=T)
  small_subset <- dataset[dataset$ver == version_number,]
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

devnetwork_version <- function(dataset){
  require(igraph, quietly=T)
  bg <- graph.empty(directed = F)
  node.out <- unique(dataset$author) 
  node.in <- unique(dataset$Filename) 
  bg <- add.vertices(bg,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
  bg <- add.vertices(bg,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
  edge.list.vec <- as.vector(t(as.matrix(data.frame(dataset[,c(1:2)]))))
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
# 1. test t2m4.9.  --> done: results: approximate hessian matrix is signular 
# insufficient sample size or highly correlated terms
# 2. increase mcmc sample size for t2m4.7
# 3. test a odel with location and contract (including all author attributes) + higher mcmc sample size

t2m4.10 <-  ergm(developer_net ~ sum + nonzero()
                + nodematch("loc") 
                + nodematch("title") # homophily theory
                + nodematch("contract")
                + nodesqrtcovar(center=T)
                + transitiveweights("min","max","min")
                + cyclicalweights("min","max","min")
                , response="frequency", reference=~Poisson
                , control = control.ergm(MCMC.samplesize = 50000))

# run t2m4.7 again with higher sample size
gof_t2m410 <- gof(t2m4.10) # gof for valued ergm not implemented
# no gof for valued ergms
summary(gof_t2m410)
pdf("results_t2m410.pdf")
summary(t2m4.10)
mcmc.diagnostics(t2m4.10)
plot(gof_t2m410)
dev.off()

t2m4.11 <-  ergm(developer_net ~ sum + nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 + cyclicalweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(#MCMC.samplesize = 10000,
                                          MCMC.prop.weights='0inflated')
                )
#Error in eigen(crossprod(x1c), symmetric = TRUE) : infinite or missing values in 'x'

# exploration
# mean and variance of dyad values
mean(developer_net%e%'frequency')
min(developer_net%e%'frequency')
max(developer_net%e%'frequency')
var(developer_net%e%'frequency')

t2m4.12 <-  ergm(developer_net ~ sum + nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 + cyclicalweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(#MCMC.samplesize = 50000,
                                          MCMC.prop.weights='0inflated') # to control for skweded degree distribution
)
# warning: approximate hessian matrix is signular. 
# increase sample size to 5,000 and see
t2m4.12 <-  ergm(developer_net ~ sum + nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 + cyclicalweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 5000,
                   MCMC.prop.weights='0inflated') # to control for skweded degree distribution
)

# correlation btw transitiveweights, cyclicalweights, nodesqrtcovar
kcycle <- sna::kcycle.census(developer_net, mode="graph", maxlen=4)
cor(kcycle[[1]][2,],kcycle[[1]][3,])
# correlationis 0.989
# decision: take out cycleweights (abritrary decision)
t2m4.12 <-  ergm(developer_net ~ sum + nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 2000,
                                          MCMC.prop.weights='0inflated') # to control for skweded degree distribution
)

# convergence not achieved after 20 iterations

t2m4.12b <-  ergm(developer_net ~ sum + nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 5000,
                                          MCMC.prop.weights='0inflated', # to control for skweded degree distribution
                                          init =coef(t2m4.12)
                                          )
)
# convergence not achieved after 20 iterations
pdf('diag_t2m412b.pdf')
mcmc.diagnostics(t2m4.12b)
dev.off()
# the only thing that looks weird is the non-zero coefficient

t2m4.13 <-  ergm(developer_net ~ sum #+ nonzero()
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(#MCMC.samplesize = 2000,
                                          MCMC.prop.weights='0inflated') # to control for skweded degree distribution
)
pdf('diag_t2m413.pdf')
mcmc.diagnostics(t2m4.13)
dev.off()
# potential downward trend for nodematch and transitive weights


t2m4.13b <-  ergm(developer_net ~ sum
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 2024
                                          , MCMC.interval = 2024
                                          , MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                        #  ,init = coef(t2m4.13)
                 ) 
                 
)

pdf('diag_t2m413.pdf')
mcmc.diagnostics(t2m4.13b)
dev.off()

# sudden jump in log likelihood improvements from 1.3 to 10.6 and then down again 
# pretty crappy model
# clear downward trend for nodematch and upward trend for nodesqrt
# adding nonzero doens't make sense based on previous results. It remains one value

# decision: chaned uniform homophily for title to differential heterophily
t2m4.14 <-  ergm(developer_net ~ sum
                  + nonzero
                  + nodematch("loc") 
                  + nodematch("title", diff=T) # differential homophily
                  + nodematch("contract")
                  + nodesqrtcovar(center=T)
                  + transitiveweights("min","max","min")
                  , response="frequency", reference=~Poisson
                  , control = control.ergm(#MCMC.samplesize = 2024
                                           #, MCMC.interval = 2024
                                            MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                           #  ,init = coef(t2m4.13)
                  ) 
                  
)
pdf('diag_t2m414.pdf')
mcmc.diagnostics(t2m4.14b)
dev.off()

t2m4.14b <-  ergm(developer_net ~ sum
                 + nonzero
                 + nodematch("loc") 
                 + nodematch("title", diff=T) # differential homophily
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 2024, 
                                          MCMC.interval = 2024,
                                          MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                          ,init = coef(t2m4.14)
                 ) 
                 
)
# it looks like this would never converge

t2m4.15 <-  ergm(developer_net ~ sum
                  + nonzero
                  + greaterthan(mean(developer))
                  + nodematch("loc") 
                  + nodematch("title", diff=F) # differential homophily
                  + nodematch("contract")
                  + nodesqrtcovar(center=T)
                  + transitiveweights("min","max","min")
                  , response="frequency", reference=~Poisson
                  , control = control.ergm(MCMC.samplesize = 2024, 
                                           MCMC.interval = 2024,
                                           MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                         
                  ) 
                  
)
pdf('diag_t2m415.pdf')
mcmc.diagnostics(t2m4.15)
dev.off()

t2m4.15 <-  ergm(developer_net ~ sum
                 + nonzero
                 + greaterthan(quantile(developer, 0.85)[[1]]) # effect about being in the top 10 % of collaboration workflows
                 + nodematch("loc") 
                 + nodematch("title", diff=F) # differential homophily
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 5000, 
                                          MCMC.interval = 2024,
                                          MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                          
                 ) 
                 
)
pdf('diag_t2m415_e.pdf')
print(summary(t2m4.15))
mcmc.diagnostics(t2m4.15)
dev.off()
# with greaterthan set to top 20 % of values, I get a singular hessian matrix.
# the error didn't appear with greater than set to top 10

# the last model that converged was model 12b. check diagnostics

t2m4.12c <-  ergm(developer_net ~ sum + nonzero()
                  + nodematch("loc") 
                  + nodematch("title") # homophily theory
                  + nodematch("contract")
                  + nodesqrtcovar(center=T)
                  + transitiveweights("min","max","min")
                  , response="frequency", reference=~Poisson
                  , control = control.ergm(MCMC.samplesize = 8000,
                                           MCMC.interval = 2024,
                                           MCMC.prop.weights='0inflated', # to control for skweded degree distribution
                                           init =coef(t2m4.12)
                  )
)

pdf('diag_t2m412_c.pdf')
print(summary(t2m4.12c))
mcmc.diagnostics(t2m4.12c)
dev.off()

t2m4.12d <-  ergm(developer_net ~ sum + nonzero()
                  + nodematch("loc") 
                  + nodematch("title") # homophily theory
                  + nodematch("contract")
                  + nodesqrtcovar(center=T)
                  + transitiveweights("min","max","min")
                  , response="frequency", reference=~Poisson
                  , control = control.ergm(MCMC.samplesize = 10000,
                                           MCMC.interval = 2024,
                                           MCMC.prop.weights='0inflated', # to control for skweded degree distribution
                                           init =coef(t2m4.12c)
                  )
)

pdf('diag_t2m412_d.pdf')
print(summary(t2m4.12d))
mcmc.diagnostics(t2m4.12d)
dev.off()
#still no convergence. Abandon this model

t2m4.16 <-  ergm(developer_net ~ sum + 
                  + atmost(8)
                  + nodematch("loc") 
                  + nodematch("title") # homophily theory
                  + nodematch("contract")
                  + nodesqrtcovar(center=T)
                  + transitiveweights("min","max","min")
                  , response="frequency", reference=~Poisson
                  , control = control.ergm(MCMC.samplesize = 5000,
                                           #MCMC.interval = 2024,
                                           MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                                          )
                  )
# replaced nonzero effect with atmost(8). This extends the nonzero effect to
# nearly 50 % of the dyads. the nonzero effect never mixed well--> no convergence

t2m4.16 <-  ergm(developer_net ~ sum
                 + atmost(8)
                 + nodematch("loc") 
                 + nodematch("title") # homophily theory
                 + nodematch("contract")
                 + nodemix(c("loc", "title"))
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(#MCMC.samplesize = 5000,
                                          MCMC.interval = 2024,
                                          MCMC.burnin = 50000,
                                          MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                 )
)
# t2m4.16 run 1
# no convergence. added equalto(11283) to account for the highest value of relationship
# a lot of variance in the atmost.8 effect. but not an upward or downward trend
# no standard error for greaterthan. The reason is no (or insufficient variance) ?

# t2m4.16 run 2
# no convergence. take out the greaterthan and equalto terms. No std error, no variance

# t2m4.16 run 3
# no convergence. effect ofr atmost8 doesn't do well. leave out completely
# but rest wasn't that bad

# t2m4.16 run 4
# without a term such as atmost(8) the mcmc chains don't mix. ther eis a upward or downward trend
# ? cycliccalties, edges, interval, add absdiffcat for differences in hierarchy
# nodemix: interaction between two attributes

# t2m4.16 run 5
# added interaction btw location and title (nodemix). Added atmost(8) back
# system computational singular

# t2m4.16 run 6
# instead of atmost use atleast as a more restrictive form of nonzero
# no convergence.

# t2m4.16 run 7
# increased sample size
# changed term nodematch title to absdiffcat to account for hierarchical differences
t2m4.16 <-  ergm(developer_net ~ sum
                 + atleast(8)
                 + nodematch("loc") 
                 + absdiffcat("title", base =c(1:5)) 
                 + nodematch("contract")
                 + nodesqrtcovar(center=T)
                 + transitiveweights("min","max","min")
                 , response="frequency", reference=~Poisson
                 , control = control.ergm(MCMC.samplesize = 5000,
                   MCMC.interval = 2024,
                   MCMC.burnin = 50000,
                   MCMC.prop.weights='0inflated' # to control for skweded degree distribution
                 )
)
summary(t2m4.16)
mcmc.diagnostics(t2m4.16)


# continue t2m12d with higher burnout? that should reduce geweke stats, but what about convergence


# analysis v4 -------------------------------------------------------------

# testing of non-mirroring hypothesis
# dependent network: developer-by-developer (realized coordination)
# IV-1: needed coordination based on task dependencies
# IV-2: ownership dependencies: the one who created the folder owns the files
# in the folder, but not the subfolders

# create DV
dev4_raw <- read.csv('data/developer-by-file/developer-by-file_1.5.csv', header=T, sep=',')
dev4 <- devnetwork(DF, 4) # add here type of node

# modified and added network
dev4_a_raw <- dev4_raw[dev4_raw$action == "A",]
dev4-a <- devnetwork_version(dev4_a_raw) # error: invalid vertex names

dev4_m_raw <- dev4_raw[dev4_raw$action == "M",]

# finish later do later

# quick visualization
library(ggraph)
dev4g<- graph.adjacency(dev4, mode='undirected', weighted=T)

ggraph(dev4g, layout='kk') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_label(aes(label = name)) + 
  theme_graph()
#not the nicest. 
#one arrow with no label
V(dev4g)$'name'
dev4g

# needed coordination network
task4 <- read.csv('data/file-by-file/file-by-file_1.5.csv', header=T, sep=",")
head(task4)

# create ownership file
# the ownership file structure provides info about which developers should be taking with each other
own <- DF[,c(1,2,40)]
dim(own)
#table(own[,2] == own[,3])
own <- own[!duplicated(own[,2]),]
dim(own)

library(pathological)
#get the folders
own$folder_names <- decompose_path(own[,2])[,2]


tmp2 <- NULL
for (i in 1:nrow(DF)){
  tmp <- decompose_path(DF[i,2])[,1]
  tmp2 <- c(tmp2, tmp)
}

DF$folder_names <- tmp2

# definition ownership: the one who first created the folder
ownership <- DF[!duplicated(DF$folder_names), c(1,42)]
ownership <- ownership[,c(2,1)]

folder_owner <- NULL
for (i in 1:nrow(DF)){
  tmp_foldername <- DF[i, 42]
  tmp_folder_owner <- ownership[ownership$folder_names == tmp_foldername,2]
  folder_owner <- c(folder_owner, tmp_folder_owner)
}

DF$folder_owner <- folder_owner

ggplot(DF, aes(x = folder_owner, fill = ver)) + geom_bar() + 
  scale_fill_brewer(type="qual")+
  coord_flip()
ggsave("onwership_frequency.png")

# in the file file-by-file replace the file_id with the owner's name
head(task4)

req_communication <- task4[,c(1:2)] #data frame that needs to be converted
ownership4 <- DF[DF$ver == 4, c(7, 43)] #lookup table
req_communication[] <- ownership4$folder_owner[match(
  unlist(req_communication), ownership4$ID_File_und)]





# the developers who worked on a file (id number)
file_author <- dev4_raw[,c(1,7)]
head(file_author)
ggplot(file_author, aes(x = author)) + geom_bar() + coord_flip()
ggsave("developer_total_activity_ver4.png")




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


