# ERGM: test how much communication realized depends on developer attributes, developer dyadic variables, and required communication
# 
# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 23 November 2020
# updated: 2nd November 2021
# problem to run model. unable to fork: resources temporarily unavailable: 
# memory issue
# 
# TODO 
# how to interpret the nonzero estimate.


# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)


# model description -------------------------------------------------------------------

# - the DV is realized communication
# - what are the characteristics of the technical stuff the developer owns that predict realized communication: 
#     centrality of file within task dependency network
#     type of task: bug fixing vs. creating new file
# - IV : structural features, reciprocity, developer attributes (hierarchy, gender), 
#     dyadic developer attributes (proximity)
# - file attributes (dyadic attributes (about the files))


# set up in R -------------------------------------------------------------

# files needed
# 
# DV: realized communication
# The DV is created in the file comm_realized.R It's a developer-to-developer matrix. 
# The CSV file "communication_realized_edge_weight_by_version.csv" is an weighted edge edgelist 
# describing which developer talks with which other developer

# IV (1): required communication (?) 
# This is the link between a folder owner and a file based on the task dependencies (layers of tech dependencies)
# The file is created in communication required. In short, the file tech dependencies is taken as the basis. 
# In this file all file IDs are replaced with their file owners. This is saved as required communication. 
# The R object is called comm_realized
# 
# IV (2): developer attributes
# this is the file authatt created with the script data_import.R


# hypothesized RS ---------------------------------------------------------

# required communication pos RS realized communication
# 
# 


# load data ---------------------------------------------------------------
set.seed(seed = 1234)

dv_master <- read_csv("communication_realized_edge_weight_by_version.csv")
ivnet_master <- read_csv("crequired_el.csv")
ivatt_master2 <- read_csv("authatt.csv")

ivatt_master <- unique(ivatt_master2[,-c(1:2, 8)])


# transform csv's into network objects ------------------------------------

names(dv_master)
dv_master
dv <- network(dv_master[,-3], directed=FALSE, ignore.eval=F, names.eval='weights', matrix.type = 'edgelist')
dv
dv%v%'vertex.names'
dv%e%'weights'

names(ivnet_master)
ivnet_master

# ivnet_master needs to be summarized to show how often a link exists between two folder owners
ivnet_master <- ivnet_master %>% group_by(folder_owner.x, folder_owner.y) %>% count()
ivnet <- network(ivnet_master, directed=FALSE, ignore.eval=F, names.eval='weights', matrix.type = 'edgelist')
ivnet
ivnet%v%'vertex.names'
ivnet%e%'weights'

# add missing names
mismatch <- match(as.character(ivnet%v%'vertex.names'), as.character(dv%v%'vertex.names'))
missingdev <- as.character(dv%v%'vertex.names')[-mismatch]

add.vertices(ivnet, 7)

set.vertex.attribute(ivnet, 'vertex.names', value = missingdev, v = which(is.na(ivnet%v%'vertex.names')))
ivnet%v%'vertex.names'

# add attributes to DV ----------------------------------------------------

# To make sure that the attributes are matched to the correct developer, a loop is created. This loop first gets the 
# developer name from the DV network. Based on that the correct job title, location and contract is extracted.  
participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL

# in the following loop information for developers who are members of version 2 is stored in a 
# number of temporary files (all begnning with tmp_).
# If a developer is not member of the version the number 99 is added. 
for (i in network::get.vertex.attribute(dv, 'vertex.names')){
  print(i)
  
  tmp_job <- pull(ivatt_master[ivatt_master$author == i, 3])
  print(tmp_job)
  jobtitle <- cbind(jobtitle, tmp_job)
  
  tmp_loc <- pull(ivatt_master[ivatt_master$author == i, 4])
  print(tmp_loc)
  location <- cbind(location, tmp_loc)
  
  tmp_con <- pull(ivatt_master[ivatt_master$author == i, 5])
  print(tmp_con)
  contract <- cbind(contract, tmp_con)
}

#network::set.vertex.attribute(dv, 'ver2', as.numeric(t(participants)[,1]))
network::set.vertex.attribute(dv, 'jobtitle', as.numeric(t(jobtitle)[,1]))
network::set.vertex.attribute(dv, 'location', as.numeric(t(location)[,1]))
network::set.vertex.attribute(dv, 'contract', as.numeric(t(contract)[,1]))


# create familiarity network ----------------------------------------------


# strength of familiarity is how often two developers worked on a previous version together
# We will first calculate the affiliation matrix of develoepr - group membership
# This will be transformed into a one-mode matrix (developer-developer) where the cell indicates
# how often these two people worked on the same version
familiarity <- ivatt_master2[,c(3, 8)]
A <- Matrix::spMatrix(
              nrow=length(unique(familiarity$author)),
              ncol=length(unique(familiarity$ver2)),
              i = as.numeric(factor(familiarity$author)),
              j = as.numeric(factor(familiarity$ver2)),
              x = rep(1, length(as.numeric(familiarity$author)))
              )
row.names(A) <- levels(factor(familiarity$author))
colnames(A) <- levels(factor(familiarity$ver2))
A

fam_dev <- tcrossprod(as.matrix(A))

fam_dev_net <- network(as.matrix(fam_dev), directed=F, ignore.eval=F, names.eval='weights')
fam_dev_net
fam_dev_net%e%'weights'


# valued ergm --> direction abandoned -------------------------------------

# on October 7th 2021 the team decided to abandon valued ergm
# and use binary data for the DV. The following valued ERGMS are
# not further developed

# a geometric reference distribution is taken as the valued data represents
# count data.
# 

#the summary returns the density of the valued network
summary(dv ~ sum, response="weights", reference=~Geometric)

dev01 <- ergm(dv ~ sum, response="weights", reference=~Geometric)
summary(dev01)
# the chance of an additional realized communication tie is exp(-0.13), 
# the intercept for sum
exp(-0.135) #87%


dev02 <- ergm(dv ~ sum + nonzero
                + edgecov(ivnet, attrname = 'weights', form = 'sum'),
              response="weights", reference=~Geometric)
# Error (1) in if (any(low.drop.theta)) 
# message(paste("Observed statistic(s)",  : 
# missing value where TRUE/FALSE needed
# Solution: KBC: I added nonzero
# On re-run got a different error:
# Error in svd(X) : infinite or missing values in 'x'
# Outcome: algorithm starts, but stops at 3rd iteraction
# Error in svd(X) : infinite or missing values in 'x'
# 3/8/21: model runs (tried 3 times)
summary(dev02)
exp(0.004013)

# the chance of an additional realized communication tie (variable: sum; 
# similar to intercept) is sum: exp (-0.05)
# the chance of an additional realized communication tie, if there is a 
# required communication tie (ivnet) exp(0.004) = 0.4%; sig = 0.0235
# 
dev03 <- ergm(dv ~ sum + nonzero
              + edgecov(ivnet, attrname = 'weights', form = 'sum') 
              + edgecov(fam_dev_net, attrname = 'weights', form = 'sum'), 
              response="weights", reference=~Geometric)
summary(dev03)
# model converges. the variables have a very small influence
# ivnet: exp(0.009)
# fam_dev: exp(0.008)

dev04 <- ergm(dv ~ sum + nonzero
              + edgecov(ivnet, attrname = 'weights', form = 'sum') 
              + edgecov(fam_dev_net, attrname = 'weights', form = 'sum')
              #+ nodefactor(attr = 'jobtitle'),
              + nodematch(attr = 'location', diff = TRUE), 
              response="weights", reference=~Geometric)
summary(dev04)
# error Error in qr.default(v, tol = tol) : 
# NA/NaN/Inf in foreign function call (arg 1)


# dichotomize data --------------------------------------------------------

# Distribution of values

ggplot(dv_master, aes(x=n)) + geom_bar()
# power law distribution. 
# One outlier: > 1200
# A lot of people between 0 and 125

quantile(dv_master$n, probs = seq(0, 1, 0.1))
# 50th:n= 8
# 70th: n = 24.8
# 90th: n = 89.6


adorn_cumulative <- function(dat, colname, dir = "down"){
  
  if(!missing(colname)){
    colname <- rlang::enquo(colname)
  } else if("valid_percent" %in% names(dat)) {
    colname <- rlang::sym("valid_percent")
  } else if("percent" %in% names(dat)){
    colname <- rlang::sym("percent")
  } else {
    stop("\"colname\" not specified and default columns valid_percent and percent are not present in data.frame dat")
  }
  
  target <- dplyr::pull(dat, !! colname)
  
  if(dir == "up"){
    target <- rev(target)
  }
  
  dat$cumulative <- cumsum(ifelse(is.na(target), 0, target)) + target*0 # an na.rm version of cumsum, from https://stackoverflow.com/a/25576972
  if(dir == "up"){
    dat$cumulative <- rev(dat$cumulative)
    names(dat)[names(dat) %in% "cumulative"] <- "cumulative_up"
  }
  dat
}


janitor::tabyl(dv_master, n) %>% 
  adorn_cumulative() %>%
  janitor::adorn_pct_formatting(digits=2) %>%
  write_csv('frequency distribution DV net.csv')

janitor::tabyl(dv_master, n) %>% 
  adorn_cumulative() %>%
  janitor::adorn_pct_formatting(digits=2) %>%
  write_csv('frequency distribution DV net.csv')
 
# frequency distribution per version

ggplot(dv_master, aes(x=n, fill = factor(ver))) + 
  geom_bar() +
  facet_wrap(~ ver)

dv_quantiles <- NULL
for (i in 1:max(dv_master$ver)) {
  tmp <- dv_master[dv_master$ver == i,]
  tmp2 <- quantile(tmp$n, probs = seq(0, 1, 0.1))
  dv_quantiles <- cbind(dv_quantiles, tmp2)
}
colnames(dv_quantiles) <- paste("ver", seq(1:max(dv_master$ver)))
dv_quantiles

write_csv(as_tibble(dv_quantiles), "dv_quantiles.csv", row.names=TRUE)

#reshape from wide to long for ggplot
dv_quantiles <- as_tibble(dv_quantiles) %>% pivot_longer(cols = starts_with("ver"), names_to ="version", values_to="value")

ggplot(dv_quantiles, aes(value, version, color = version)) + 
  geom_point() 

ggplot(dv_master, aes(x = n, factor(ver))) + 
  geom_violin() + 
  ggsave('frequency distribution DV net.png')

# I think the cut-off should be dynamic for each version. Maybe something like 
# mean + standard deviation



# contining with valued ERGM  ---------------------------------------------

# focus on including structural variables

dev05 <- ergm(dv ~ sum + nonzero #intercept
              + edgecov(ivnet, attrname = 'weights', form = 'sum') 
              # + sociality, # result sin NA/NaN/Inf in foreign function call
              + nodecovar # NA/NaN/Inf in foreign function call 
              , response="weights", reference=~Geometric)



## 25th March 2021 ---------------------------------------------------------

# This analysis wasn't stable. A lot of errors. Can't trace it back 
# to its source. 
dev02 <- ergm(dv ~ sum + nonzero 
              + edgecov(ivnet, attrname = 'weights', form = 'sum') 
              + edgecov(fam_dev_net, attrname = 'weights', form = 'sum'),
              response="weights", reference=~Geometric)
# Error in eigen(crossprod(x1c), symmetric = TRUE) : 
# infinite or missing values in 'x'
dev02 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min"), 
              response="weights", reference=~Geometric)
# Error in qr.default(v, tol = tol) : NA/NaN/Inf in foreign function call (arg 1)
dev02 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min") + 
                edgecov(ivnet), 
              response="weights", reference=~Geometric)
# Error in svd(X) : infinite or missing values in 'x'
dev02 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min") + 
                edgecov(ivnet) + 
                nodefactor(attr = 'jobtitle') + 
                nodematch(attr = 'location', diff = FALSE), 
              response="weights", reference=~Geometric)
# Error in svd(X) : infinite or missing values in 'x'
# maybe there is an error in ivnet. that would be annoying. 
dev02 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min") + 
                nodefactor(attr = 'jobtitle') + 
                nodematch(attr = 'location', diff = FALSE), 
              response="weights", reference=~Geometric)
summary(dev02)
# some homophily (positive job factor effect), but also heterophily (negative job factor effect)
exp(-0.24) #transitive weights
exp(0.05) # job title 1 (senior developer)
exp(-0.76) # job title 2
exp(-1.66) # job title 5

dev03 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min") + 
                nodefactor(attr = 'jobtitle') + 
                nodematch(attr = 'location', diff = FALSE) + 
                edgecov(fam_dev_net, attrname = 'weights', form = 'sum'), 
              response="weights", reference=~Geometric)
summary(dev03)
# NA/NaN/Inf in foreign function call (arg 1)
# jll
# THOUGHT:  adding edgecov result in this error.
# correlation dv and fam_dev_net
gcor(dv, fam_dev_net) # correlation is 0.27
gcor(dv, ivnet)
gcor(fam_dev_net, ivnet)

quantile(fam_dev_net%e%'weights', probs = seq(0, 1, 0.1))
# I'm going to dichotomize the weights in fam_dev_net into 0 (no familiarity) if weight <=5
# 50th percentile  == 4 
fam_weights2 <- fam_dev_net%e%'weights'
fam_weights2[fam_weights2 < 5] <- 0
fam_weights2[fam_weights2 > 4] <- 1
fam_dev_net%e%'weights2' <- fam_weights2
fam_dev_net%e%'weights2'

dev03 <- ergm(dv ~ sum + 
                transitiveweights(twopath="min",combine="max",affect="min") + 
                nodefactor(attr = 'jobtitle') + 
                nodematch(attr = 'location', diff = FALSE) + 
                edgecov(fam_dev_net, attrname = 'weights2', form = 'sum'), 
              response="weights", reference=~Geometric)
dev03 <- ergm(dv ~ sum + 
                edgecov(fam_dev_net, attrname = 'weights', form = 'sum'), 
              response="weights", reference=~Geometric)
# this find a solution. Run it again to see if it was flucke of chance
# Maybe an interaction between transitiveweights and edgecov?
summary(dev03)
exp(0.009) #familiarity
dev04 <- ergm(dv ~ sum + 
                edgecov(ivnet, attrname = 'weights', form = 'sum'), 
              response="weights", reference=~Geometric)
summary(dev04)
exp(dev04$coef[2]) # 1.002 --> has hardly any impact. 
