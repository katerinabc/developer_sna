# Rsiena
# 
# author: Katerina Bohle Carbonell
# 
# created: 12-12-2021
# updated: 24-01-2022
# 
# dependent on
# helper functions: ("functions_developer_sna.R", echo = F)

# TUTORIAL FOR RSIENA: 
# scripts: https://www.stats.ox.ac.uk/~snijders/siena/siena_scripts.htm

rm(list = ls()) # clean everything
set.seed(1234)

# TODO: 

# packages ----------------------------------------------------------------

library(RSiena)
library(network)
library(dplyr)


source("functions_developer_sna.R", echo = F)
#load('data for rsiena.RData') # loads all data, but this is an undirected network

# load data ---------------------------------------------------------------

#source("creating_networks.R", echo = F)



# uncomment everything until 'descriptive analysis if you have problems
# loading the RData file and need to create the data objects
dv_master <- readr::read_csv("communication_realized_edge_weight_by_version.csv")

ivatt_master2 <- readr::read_csv("authatt.csv")
ivatt_master2 <- ivatt_master2[,-c(1:2)]
all_developers <- as.vector(unique(ivatt_master2$author))

ivatt_master <- unique(ivatt_master2 %>% select(-ver2))
ivnet_master <- readr::read_csv("crequired_el.csv")
# columnn names information
# time = version
# sum_weights = sum of edges between dev.x and dev.y for each version
# n = sum of weights
# weights = number of connections between two files


# create joiner/leavers network ----------------------------------------------


# familiarity <- ivatt_master2 %>% select(author, ver2)
# A <- Matrix::spMatrix(
#   nrow=length(unique(familiarity$author)),
#   ncol=length(unique(familiarity$ver2)),
#   i = as.numeric(factor(familiarity$author)),
#   j = as.numeric(factor(familiarity$ver2)),
#   x = rep(1, length(as.numeric(familiarity$author)))
# )
# row.names(A) <- levels(factor(familiarity$author))
# colnames(A) <- levels(factor(familiarity$ver2))
# #A
# 
# memb <- NULL
# for (i in 1:(length(dv_first))){
#   tmp <- tcrossprod(as.matrix(A[,i]))
#   tmp[which(tmp != 0)] <- 11
#   tmp[which(tmp == 0)] <- 10
#   
#   # tmp <- as(tmp, 'dgTMatrix')
#   
#   memb[[i]] <- tmp
#   
# }

#membership <- lapply(memb, function(x) as(x, 'dgTMatrix'))

#memb_net <- varDyadCovar(membership)


# specify Rsiena objects DV --------------------------------------------------
#nodeset <- as.vector((dv_first[[1]] %v% 'vertex.names'))


dv_modified <-
  dv_master %>%
  group_by(ver) %>%
  mutate(tie_first = if_else(n > quantile(n)[2], 1, 0)
  )

dv_first <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- netpanel(thresholdname = "tie_first",
                  ver = i, x = "author", y = "folder_owner",
                  data = dv_modified,
                  mode_directed= TRUE,
                  as_network = FALSE)
  
  dv_first[[i]] <- tmp
  
}


#dv_mat <- lapply(dv_first, function(x) as.matrix(x)) 
dv_mat <- dv_first

# add structural zeros

# let's do a long and inefficient for-loop as I'm too tired to think 
# how to do it better

# adding structural zeros
# we do not add structural zeros because there is informal coordination happening outside
# of project membership and folder ownership
# 
# # we begin with t = 1 (could turn it into a function and use lapply)
# for(k in 1:length(dv_mat)){
#   for(i in 1: ncol(dv_mat[[k]])){
#     for(j in 1:ncol(dv_mat[[k]])){
#       if(memb[[k]][i,j] == 10)
#         {dv_mat[[k]][i,j] <- 10}
#       #else {dv_mat[[1]][i,j] <- dv_mat[[1]][i,j]} # do nothing
#     }
#   }
# }



dv <- sienaNet(array(c(dv_mat[[1]], dv_mat[[2]], dv_mat[[3]],
                             dv_mat[[4]], dv_mat[[5]], dv_mat[[6]],
                             dv_mat[[7]], dv_mat[[8]], dv_mat[[9]],
                             dv_mat[[10]], dv_mat[[11]]), 
                           dim =c(21, 21, 11)))


print(dv)


# contributions information -----------------------------------------------

# something about most active person --> check code in ownership file

# add developer information -----------------------------------------------
# copied from tergm

# add network attributes

# in the following loop information for developers who are members of version 2 is stored in a
# number of temporary files (all beginning with tmp_).
# If a developer is not member of the version the number 99 is added.
#participants <- NULL

dv_first <- lapply(dv_mat, function(x) network(x, directed=TRUE))

for (i in 1:11){
  jobtitle <- NULL
  location <- NULL
  contract <- NULL
  #member <- NULL
  member2 <- NULL
  print(i)
  for (j in network::get.vertex.attribute(dv_first[[i]], 'vertex.names')){
    # print(j)
    
    atmp <- as.vector(ivatt_master2 %>% 
      filter(author == j & ver2 == i) %>%
      select(jobtitle_raw, location, contract, ver2) %>%
      unique())
      
    # atmp <- as.vector(unique(ivatt_master2[ivatt_master2$author == j &
    #                                          ivatt_master2$ver2 == i,
    #                                        3:6]))
    if (purrr::is_empty(atmp)) {atmp <- tibble(jobtitle_raw = NA,
                                         location = NA,
                                         contract = NA
                                         #member = "no"
                                         )}
    
    #print(atmp)
    jobtitle <- c(jobtitle, pull(atmp[1]))
    # print( jobtitle)
    location <- c(location, pull(atmp[2]))
    # print(location)
    contract <- c(contract, pull(atmp[3]))
    # print(contract)
    # member <- c(member, pull(atmp[4]))
    # print(member)
    
    memtmp <- if_else(j %in% pull(ivatt_master2 %>% 
                                    filter(ver2 == i) %>% 
                                    select(author)),
                      1, 0)
    
    member2 <- c(member2, memtmp)
    
  }
  network::set.vertex.attribute(dv_first[[i]], 'jobtitle', as.numeric(jobtitle))
  network::set.vertex.attribute(dv_first[[i]], 'location', as.numeric(location))
  network::set.vertex.attribute(dv_first[[i]], 'contract', as.numeric(contract))
  #network::set.vertex.attribute(dv_first[[i]], 'membership', as.numeric(member))
  network::set.vertex.attribute(dv_first[[i]], 'membership', as.numeric(member2))
  
}
# a note on membership
# a 0 in membership means they did not join this project
# people in version 1 work on version 2

# specify Rsiena objects covariates -----------------------------------------------

# for every changing indepdent variable we need to create a matrix
# with n rows (actors) and m columns (time periods)

# locations - does not change
loc <- NULL
#rownames(loc) <- dv_first[[1]] %v% 'vertex.names'
for (i in 1: length(dv_mat)){
  
  tmp <- dv_first[[i]] %v% 'location'
  
  loc <- cbind(loc, tmp)
}

location <- coCovar(loc[,1])

# jobtitle - changes per version
jobt <- NULL

for (i in 1: (length(dv_mat) - 1)){
  
  tmp <- dv_first[[i]] %v% 'jobtitle'
  
  jobt <- cbind(jobt, tmp)
}
colnames(jobt) <- seq(1:10)
rownames(jobt) <- seq(1:21)

jobtitle <- varCovar(jobt)

# contract - changes per version
cont <- NULL
for (i in 1: length(dv_mat)){
  
  tmp <- dv_first[[i]] %v% 'contract'
  
  cont <- cbind(cont, tmp)
}

colnames(cont) <- seq(1:11)
rownames(cont) <- seq(1:21)
contract <- varCovar(cont)

# # membership - changes per version
mem <- NULL
for (i in 1: length(dv_mat)){

  tmp <- dv_first[[i]] %v% 'membership'

  mem <- cbind(mem, tmp)
}

colnames(mem) <- seq(1:11)
rownames(mem) <- seq(1:21)

membership <- varCovar(mem)




# specify Rsiena objects dyadic covariates --------------------------------------------

# #  IV (technical dependencies) --------------------------------------------
# 
# # colnames in iv not the same than in dv. fix for code to work
colnames(ivnet_master)[3] <- "ver"

iv_modified <-
  ivnet_master %>%
  group_by(ver) %>%
  mutate(tie_first = if_else(sum_weights > 0, 1, 0))

iv_first <- list()
for (i in 1: max(iv_modified$ver)){
  #print(i)
  tmp <- dynmatric(thresholdname = "tie_first",
                   ver = i,
                   x = "folder_owner.x",
                   y = "folder_owner.y",
                   data = iv_modified)
  
  iv_first[[i]] <- tmp
  
}

#iv_first10 <- iv_first[-length(iv_first)]
#iv_first <- lapply(iv_first, function(x)ifelse())
iv_mat <- list()
for(i in 1: length(iv_first)){
  iv_mat[[i]] <- iv_first[[i]]
  iv_mat[[i]][iv_mat[[i]] ==2 ] <- 1
}


#iv_mat <- lapply(iv_mat, function(x) as(x, "dgTMatrix")) 


iv <- sienaNet(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[3]],
                       iv_mat[[4]], iv_mat[[5]], iv_mat[[6]],
                       iv_mat[[7]], iv_mat[[8]], iv_mat[[9]],
                       iv_mat[[10]], iv_mat[[11]]), 
                     dim =c(21, 21, 11)))

# create rsiena formula ---------------------------------------------------

mydata <- sienaDataCreate(dv, iv, 
                          jobtitle, location, contract, membership)# define data


# DV read in as a UNIDRECTED network
# IV read in as a DIRECTED network
print01Report(mydata, modelname = 'developer_coordination' )

myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model')

# get effects -------------------------------------------------------------

myeff <- getEffects( mydata )
myeff # default effects
effectsDocumentation(myeff)


# The rule of thumb is that all t-ratios for convergence
# should ideally be less than 0.1 in absolute value,
# and the "Overall maximum convergence ratio" should be less than 0.25;
# this signifies good convergence of the algorithm.
# 
# first check convergence, then p-values
# 
# 
# model 0 - teting  -----------------------------------------------------------------

# testing with some simple effects
myeff <- includeEffects(myeff, sameX, interaction1 = 'contract')
myeff <- includeEffects(myeff, sameX, interaction1 = 'jobtitle')
myeff <- includeEffects(myeff, sameX, interaction1 = 'location')
myeff <- includeEffects(myeff, sameX, interaction1 = 'membership')

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# good convergences

# model 2 -----------------------------------------------------------------
# 
# include coevolution  effects
# no need to include creation and endow (maintenance) effect as it
# results in colinearity together with evaluation effect
# 
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', interaction1 = 'iv')
myeff <- includeEffects( myeff,
                         crprod, type = 'creation', interaction1 = 'iv',
                         include=FALSE) 
myeff


# to exclude effects type:
# myeff <- includeEffects( myeff, # the existing effects
#transTriads, include = FALSE) # tran


ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# no convergence. t-ratio for endow dv:iv above 0.1
# estimate for these two factors very high:
# rate constant dv (period 6): 48.4
# endow dv:iv 33.1
# 
# not sure if the best course of action is to add more effects
# as its the start of the model or to remove some

# let's first add the other side of the co-evolution effects
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         name = "iv",
                         interaction1 = 'dv') 
myeff <- includeEffects( myeff,
                         crprod, type = 'creation',
                         name = "iv",
                         interaction1 = 'dv', 
                         include=FALSE)

myeff

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# convergence still not achieved. 
# high estimate for dv rate for period 6


# control variable --------------------------------------------------------

myeff <- includeEffects(myeff, altX, interaction1 = 'membership')
# forming ties more likely if alter is a member
# 
myeff <- includeEffects(myeff, egoX, interaction1 = 'membership')
# forming ties more likely if ego is a member

# or sameX or egoXaltX or membership Triad
ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
#warning: standard errors are not reliable
myeff <- includeEffects(myeff, altX, interaction1 = 'membership', include=FALSE)
myeff <- includeEffects(myeff, egoX, interaction1 = 'membership', include = FALSE)

# sameX
# same covariate, which can also be called covariate-related identity
#  (sameX), defined by the number of ties of i to all other actors j 
#  who have exactly the same value on
myeff <- includeEffects(myeff, sameX, interaction1 = 'membership')
ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans


# model 4 -----------------------------------------------------------------

# I need to do something with period 6
# 
# the test for time heterogeneity tests if the restricted model
# (same mechanism in every time period) is true
tt2 <- sienaTimeTest(ans)
summary(tt2)
# TimeTest constructed a null hypothesis with 8 estimated parameters
# and 72 dummy variables to be tested.
# However, there are 1 linear dependencies between these.
# 
# This may be because some of the parameters are already
# interactions with time dummies or other time variables.
# Automatic discovery of dependencies yielded the exclusion of effect
# 28 . iv: dv  (creation effect)
# 
# how to interpret this?
plot(tt2, effects=1)
plot(tt2, effects=2)
plot(tt2, effects=4)
plot(tt2, effects=5)
plot(tt2, effects=6)

# density for DV and IV is time varying. 
# add it to the mode

myeff <- includeTimeDummy(myeff, density, timeDummy="6")

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

tt2 <- sienaTimeTest(ans)
summary(tt2)


# model 3 -----------------------------------------------------------------

# structural effects on DV

myeff <- includeEffects( myeff,
                         transTrip, include=TRUE)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# model  4 -----------------------------------------------

# other covariates
myeff <- includeEffects(myeff, sameX, interaction1 = 'location') 
# homophily, close proximity 
myeff <- includeEffects(myeff, sameX, interaction1 = 'contract')
# homophily
myeff <- includeEffects(myeff, absDiffX, interaction1 = 'jobtitle')
# seniority effect

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans


# model 5 -----------------------------------------------------------------

myeff <- includeEffects(myeff, inPop) 
myeff <- includeEffects(myeff, transTrip, name = 'iv') 
myeff <- includeEffects(myeff, inPop, name ='iv') #skewed pop 

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans
# overall convergence decreased
# convergence for iv variables decreased
myeff <- includeEffects(myeff, transTrip, name = 'iv', include=FALSE) 
# excluding closure for iv, as iv is based on technical dependencies
# 
# what would make sense is file i dependent on file k and h, 
# and file k and h interdependent 
# (kind of like a hierarchy among files)
# but this is transitive triplet!
# 
# let's keep it in for the moment and turn the inpop into a sqrt version
# because the data is heavily skewed

myeff <- includeEffects(myeff, transTrip, name = 'iv', include=TRUE) 
myeff <- includeEffects(myeff, inPop, include=FALSE)
myeff <- includeEffects(myeff, inPopSqrt) 
myeff <- includeEffects(myeff, inPop, name ='iv', include=FALSE) #skewed pop 
myeff <- includeEffects(myeff, inPopSqrt, name ='iv') #skewed pop 


ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans


myeff <- includeEffects(myeff, crprodRecip, name="iv", interaction1 = "dv")
myeff <- includeEffects(myeff, inPopIntn, name = "iv", interaction1 = "dv")
# effect of indegree in IV on indegree in DV, but DV is 
# undirected so can't model that. 
# 
# include agreement on IV leads to conecton in DV (p. 157)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans
# convergence is bad. 
# maybe there is no reciprocity given the data?

myeff <- includeEffects( myeff,recip, include=FALSE)
myeff <- includeEffects( myeff,recip, name = "iv", include=FALSE)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# convergence is still crap. 
# option 1: no coevolution via endow (maintenance function)
# option 2: no co-evolution. only IV influencing DV but not hte other way
# option 3: only coevolution via rate function.
# 
# let's test option 3

myeff <- includeEffects(myeff, crprodRecip, name="iv", interaction1 = "dv", include=FALSE)
myeff <- includeEffects(myeff, inPopIntn, name = "iv", interaction1 = "dv", include=FALSE)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans
# better convergence
# the estimate for maintenance of ties is high. 
# let's simplify it and get rid of endow effect and instead 
# include eval
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         interaction1 = 'iv', 
                         include=FALSE)
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'eval', 
                         interaction1 = 'iv', 
                         include=TRUE)


myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         name = "iv",
                         interaction1 = 'dv', include=FALSE)
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'eval', 
                         name = "iv",
                         interaction1 = 'dv', include=TRUE)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# this reduced convergence for effects in the IV network
# but not in the DV network

myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         name = "iv",
                         interaction1 = 'dv', include=TRUE)
myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'eval', 
                         name = "iv",
                         interaction1 = 'dv', include=FALSE)

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# let's remove the structural terms from IV except density and
#  endow and see. their convergence is low

myeff <- includeEffects(myeff, inPopSqrt, transTrip, name ='iv', include=FALSE) 

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# if this didn't help, let's put them back in 
# tinker with the algoritm

# ok that improved convergence, but I'm not happy not having any
# structural network effects.  

myeff <- includeEffects(myeff, inPopSqrt, name ='iv', include=TRUE) 

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans

# overall covergence is slightly above threshold. 

ans1 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns =ans)
ans1

ans2 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns = ans1)
ans2


# checking model ----------------------------------------------------------


# I was thinking about adding "inPopIntn" again in the model but this
# is converging and will have to do for the moment.

# correlation check for collinearity
# correlation of 0.954 for DV network between 
# outdegree and sameX (location)
# but standard errors aren't too high for these variables so include them
# 
# re-run the model using the Generalized method of moments (GMoM) algo
# which is better for co-evolution models
myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    gmm = TRUE,
                                    nsub = 2,
                                    n3 = 5000)
myeff <- getEffects( mydata )
myeff # default effects
effectsDocumentation(myeff)

myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         name = "iv",
                         interaction1 = 'dv') 

myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'eval', 
                         interaction1 = 'iv', 
                         include=TRUE)

myeff <- includeEffects( myeff, inPopSqrt, transTrip, include=TRUE)

myeff <- includeTimeDummy(myeff, density, timeDummy="6")

myeff <- includeEffects(myeff, sameX, interaction1 = 'membership')
myeff <- includeEffects(myeff, sameX, interaction1 = 'location') 
# homophily, close proximity 
myeff <- includeEffects(myeff, sameX, interaction1 = 'contract')
# homophily
myeff <- includeEffects(myeff, absDiffX, interaction1 = 'jobtitle')
# seniority effect
myeff <- includeEffects(myeff, inPopSqrt, name ='iv') #skewed pop 
# 
myeff <- includeGMoMStatistics(myeff, newrecip, persistrecip)
myeff <- includeGMoMStatistics(myeff, newrecip, persistrecip, name ="iv")

save.image("~/Documents/gitrepo/developer_sna/analysis/rsiena_data_model.RData")

ans3 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
#Lapack routine dgesv: system is exactly singular: U[14,14] = 0

myeff <- includeGMoMStatistics(myeff, newrecip, include=FALSE)
myeff <- includeGMoMStatistics(myeff, newrecip, include=FALSE, name ="iv")

ans3 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
#same error message
myeff <- includeGMoMStatistics(myeff, persistrecip, include=FALSE, name ="iv")
ans3 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
#same error message --> do not include gmm effects
ans3 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
# same error --> abandon (could do MoM or Bayes but no time)
myeff <- includeGMoMStatistics(myeff, persistrecip, include=FALSE)


# model for paper ---------------------------------------------------------

myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    gmm = FALSE,
                                    nsub = 2,
                                    n3 = 5000)

myeff <- getEffects( mydata )
myeff # default effects
effectsDocumentation(myeff)

myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'endow', 
                         name = "iv",
                         interaction1 = 'dv') 

myeff <- includeEffects( myeff, # the existing effects
                         crprod, type = 'eval', 
                         interaction1 = 'iv', 
                         include=TRUE)

myeff <- includeEffects( myeff, inPopSqrt, transTrip, include=TRUE)

myeff <- includeTimeDummy(myeff, density, timeDummy="6")

myeff <- includeEffects(myeff, sameX, interaction1 = 'membership')
myeff <- includeEffects(myeff, sameX, interaction1 = 'location') 
# homophily, close proximity 
myeff <- includeEffects(myeff, sameX, interaction1 = 'contract')
# homophily
myeff <- includeEffects(myeff, absDiffX, interaction1 = 'jobtitle')
# seniority effect
myeff <- includeEffects(myeff, inPopSqrt, name ='iv') #skewed pop 


ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans
ans2 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns = ans)
# *** Standard errors not reliable ***
#   The following is approximately a linear combination 
# for which the data carries no information:
#   -1 * beta[12]
# It is advisable to drop one or more of these effects.
# Warning message:
#   In phase3.2(z, x) :
#   *** Warning: Covariance matrix not positive definite *** 
#   
#   solution: I'm re-running the ans2 to avoid the error
# (potentially problems with model and data 
# --> on 2nd run all ok)
ans3 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns = ans2)
ans3

myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    gmm = FALSE,
                                    nsub = 4,
                                    n3 = 5000)
ans4 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns = ans3)
ans4
# estimate for iv rate (period 3) is higher than for other periods
# 13 compared to 7 (period 4) or 4 (period 5)
tt3 <- sienaTimeTest(ans4, effects=c(12))
summary(tt3)

# let's remove reciprocity (again) given the high standard errors.
# for DV the estimate for reciprocity is
# DV estimate: -9.7; standard error: 70.85
# IV estimate: 2.97; standard error: 0.36

myeff <- includeEffects( myeff,recip, include=FALSE)

ans5 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans5



# Decision: add a time dummy for period 3 and period 6 to control for
# branching
# branch 1: 1-6
# branch 2: 1-2, 7-11 

myeff <- includeTimeDummy(myeff, density, timeDummy="3,6")
myeff <- includeTimeDummy(myeff, density, name ="iv", timeDummy="3,6")
ans6 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans6

# testing if including reciprocity for DV results in a better model
myeff <- includeEffects( myeff,recip, include=TRUE)
ans7 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)
ans7

# overall convergence is good, but convergence for reciprocity
# is only resonable (0.1291)

#myeff <- includeEffects( myeff,recip, name = "iv", include=FALSE)
ans8 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, prevAns = ans7, returnDeps=TRUE)
ans8

# standard error of recip is very large (35) and estimate is -8.63
# let's look at correlations
sink('ans8.txt')
summary(ans8)
sink()

# DV network: none


# GOF ---------------------------------------------------------------------

gof1 <- sienaGOF(ans8, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="dv")
summary(gof1)
plot(gof1)
# bad fit especially for indegree < 4

gof2 <- sienaGOF(ans8, 
                 OutdegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="dv")
summary(gof2)
plot(gof2)
# better fit than for indegree
# fit for outdegree < 4 could be better

gof3 <- sienaGOF(ans8, 
                 mixedTriadCensus, 
                 verbose=TRUE, join=TRUE,
                 varName=c("dv", "iv"))
summary(gof3)
plot(gof3)
# p-value is 0 --> poor fit
# is this because of the sparse networks? 

gof4 <- sienaGOF(ans8, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="iv")
summary(gof4)
plot(gof4)
# pvalue is 0, bad fit
# indegrees btw 1 and 4 underestimated

gof5 <- sienaGOF(ans8, 
                 OutdegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="iv")
summary(gof5)
plot(gof5)
# pval is 0.006, model fit not too bad. 

# model improvements ------------------------------------------------------

myeff <- includeEffects(myeff, isolateNet, include=TRUE)
myeff <- includeEffects(myeff, isolateNet, name = "iv", include=TRUE)
myeff <- includeEffects(myeff, inPop, include=TRUE)
myeff <- includeEffects(myeff, inPop, name = 'iv', include=TRUE)

ans8 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, returnDeps=TRUE)
ans8

# no overall convergence (0.8), but convergence of individual effects
# except reciprocity (dv)
# the standard error for iv rate (period 2 & 8) are above 600
# I could run again, but not sure that would help...
# rerun it 2 times and it improved but not below 0.25
ans10 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, returnDeps=TRUE, prevAns = ans9)
ans10

# i had another look at developer_coordinatin.txt and noticed no 
# reciprocal ties in DV network. 
# I went back to the raw data and noticed that there should be some. modified the 
# function and recreated the dv network. 
# the reciprocal tie count is very (!) low (2 in ver 1 and 4 in ver 7)

# skipped all the previous testing and effect selection and directly 
# rebuild the model and continued with ans10. 
# with ans10 convergence was low
ans11 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, returnDeps=TRUE, prevAns = ans10)
ans11

# very slow improvement in convergence. 
# let's remove reciprocity for DV

myeff <- includeEffects( myeff,recip, include=FALSE)
ans12 <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, returnDeps=TRUE)
ans12

# this doesn't seem to work at all. 
# 
# let's brake it up into branch 1 and branch 2


#myeff <- includeTimeDummy(myeff, density, timeDummy="6")


# another effect would be: outIso (total isolate)
# 
# branch 1 ----------------------------------------------------------------

b1_jobtitle <- varCovar(jobt[,1:6])
b1_contract <- varCovar(cont[,1:6])
b1_membership <- varCovar(mem[,1:6])

b1_dv <- sienaNet(array(c(dv_mat[[1]], dv_mat[[2]], dv_mat[[3]],
                          dv_mat[[4]], dv_mat[[5]], dv_mat[[6]]), 
                        dim =c(21, 21, 6)))

b1_iv <- sienaNet(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[3]],
                          iv_mat[[4]], iv_mat[[5]], iv_mat[[6]]),
                        dim =c(21, 21, 6)))

b1_iv <- varDyadCovar(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[3]],
                              iv_mat[[4]], iv_mat[[5]]), 
                            dim =c(21, 21, 5)))

branch1 <- sienaDataCreate(b1_dv, b1_iv, 
                           b1_jobtitle, location, b1_contract, b1_membership)# define data

print01Report(branch1, modelname = 'branch1_developer_coordination' )

b1eff <- getEffects( branch1 )
b1eff # default effects
effectsDocumentation(b1eff)


# check myalgorithm. They should be the same for both branches. 
# Uncomment these lines to if the project 'myalgorithm' 
# needs to be created
myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    nsub = 4,
                                    n3 = 5000)


b1eff <- includeEffects( b1eff, crprod, type = 'endow', name = "b1_iv",interaction1 = 'b1_dv')
b1eff <- includeEffects( b1eff, crprod, type = 'eval', interaction1 = 'b1_iv', include=TRUE)
b1eff <- includeEffects( b1eff, inPopSqrt, transTrip, include=TRUE)
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_membership')
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'location')
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_contract')
b1eff <- includeEffects(b1eff, absDiffX, interaction1 = 'b1_jobtitle')
b1eff <- includeEffects(b1eff, inPopSqrt, name ='b1_iv') #skewed pop
b1eff <- includeEffects(b1eff, isolateNet, include=TRUE)
b1eff <- includeEffects(b1eff, isolateNet, name = "b1_iv", include=TRUE)
b1eff <- includeEffects(b1eff, inPop, include=TRUE)
b1eff <- includeEffects(b1eff, inPop, name = 'b1_iv', include=TRUE)
# remove reciprocity for dv
b1eff <- includeEffects( b1eff,recip, include=FALSE)

b1_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE)
b1_ans
# overall convergence still bad (0.85) but convergence for
# individual effects is good. Aslo no huge SE anymore
# b2_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b1_ans)
# b2_ans #better
# b3_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b2_ans)
# b3_ans #better
# save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")
# b4_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b3_ans)
# b4_ans #better
# save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")
# b5_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b4_ans)
# b5_ans #better
# save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")
# b6_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b5_ans)
# b6_ans #converged
# save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

# Instead of rerunning the model, load the final object (b6_ans)
load("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

gofb1_1 <- sienaGOF(b6_ans, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_dv")
summary(gofb1_1)
plot(gofb1_1)
# good. p.value 0.137

gofb1_2 <- sienaGOF(b6_ans, 
                 OutdegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_dv")
summary(gofb1_2)
plot(gofb1_2)
# goodish (p = 0.005)
# fit for outdegree > 1 could be better

# gofb1_3 <- sienaGOF(b6_ans, 
#                     TriadCensus, 
#                     verbose=TRUE, join=TRUE,
#                     varName="b1_dv")
# summary(gofb1_3)
# plot(gofb1_3)
# Error in neighborsHigher[[i]] : subscript out of bounds
#Timing stopped at: 2.66 0.272 2.949

gofb1_4 <- sienaGOF(b6_ans, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_iv")
summary(gofb1_4)
plot(gofb1_4)
# pvalue is 0.637, good


gofb1_5 <- sienaGOF(b6_ans, 
                 OutdegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_iv")
summary(gofb1_5)
plot(gofb1_5)
#pvaue of 0.147. good

# gofb1_6 <- sienaGOF(b6_ans, 
#                     TriadCensus, 
#                     verbose=TRUE, join=TRUE,
#                     varName="b1_iv")
# summary(gofb1_6)
# plot(gofb1_6)
# Error in neighborsHigher[[i]] : subscript out of bounds
# Timing stopped at: 0.051 0.005 0.102

save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

tt <- sienaTimeTest(b6_ans)
summary(tt)

# p-value of 0.006 (period 2) and of 0 (period 1 or period 4)

b1eff <- includeTimeDummy(b1eff, density, timeDummy="1,4")

b7_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b6_ans)
b7_ans #converged
save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

tt2 <- sienaTimeTest(b7_ans)
summary(tt2)
b1eff <- includeTimeDummy(b1eff, density, timeDummy="1,2,4")
b7_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE)
b7_ans 
# b7 didn't converge. no need to test for 
# time heterogeneity
tt2 <- sienaTimeTest(b7_ans)
summary(tt2)

# time heterogeneity seems to be still a major problem

b1eff <- includeTimeDummy(b1eff, transTrip, timeDummy="1,2,4")


b7_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b6_ans)
b8_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE, prevAns = b7_ans)
b8_ans 

# convergence is way off. abandon
b1eff <- includeTimeDummy(b1eff, transTrip, timeDummy="1, 2, 4", include=FALSE)
b7_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
# run the following lines several times until converged
b8_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, prevAns=b7_ans)
# converged with overall ratio 0.22
# se of iv rate 2 also dropped to acceptable levels

tt3 <- sienaTimeTest(b7_ans, effects = 6:15)
summary(tt3)
# time heterogeneity: pval = 0.0037
# but none of the variables jump out for incl by pval
# period 1 seems to be an issue.

b1eff <- includeTimeDummy(b1eff, absDiffX, interaction1 = "b1_jobtitle", timeDummy="1", include=TRUE)

tt4 <- sienaTimeTest(b7_ans, effects = 24:29)
summary(tt4)
# no time heterogeneity problem

b7_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

b8_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, prevAns=b7_ans)
b8_ans
save.image("~/Documents/gitrepo/developer_sna/analysis/branch1_results.RData")

tt5 <- sienaTimeTest(b8_ans, effects = 6:15)
summary(tt5)
# time heterogeneity: pval = 0.0167


sink("branch1_finalresult.txt")
b8_ans
sink()
siena.table(b8_ans, type="html")

# # Request the plot to go to a file:
# png(filename="selectionTable_ans2.png", width=1000,height=800)
# # make the plot:
# selectionTable.plot(ans2, b1data, b1_dv, b1_iv, 1:4)
# # close the plot file:
# graphics.off()

res <- Multipar.RSiena(b8_ans, 6)

# testing significance of parameters

chi <- NULL
df <- NULL
pval <- NULL
oneside <- NULL
ef <- NULL
for (i in c(1:30)){
  tmp <- Multipar.RSiena(b8_ans, i)
  tmp <- unlist(tmp)
  
  chi <- c(chi, as.numeric(tmp[[1]]))
  df <- c(df, as.numeric(tmp[[2]]))
  pval <- c(pval, as.numeric(tmp[[3]]))
  oneside <- c(oneside, as.numeric(tmp[[4]]))
  ef <- c(ef, tmp[[5]])
}


b8_results <- tibble(effect = b8_ans$effects$effectName,
                     estimate = round(b8_ans$theta,3),
                     st.error = round(b8_ans$se,3),
                     zscore = round(b8ans_est/b8ans_se,3),
                     chi = chi,
                     df = df,
                     pval = round(pval,3),
                     oneside= round(oneside,3),
                     ef = ef)
b8_results %>% readr::write_csv("branch1_results.csv")


# branch 1 no coevolution -------------------------------------------------



b1_iv <- varDyadCovar(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[3]],
                              iv_mat[[4]], iv_mat[[5]]), 
                            dim =c(21, 21, 5)))

branch1 <- sienaDataCreate(b1_dv, b1_iv, 
                           b1_jobtitle, location, b1_contract, b1_membership)# define data

print01Report(branch1, modelname = 'branch1_developer_coordination_nocoevolution' )

b1eff <- getEffects( branch1 )
b1eff # default effects
effectsDocumentation(b1eff)

b1eff <- includeEffects( b1eff, inPopSqrt, transTrip, include=TRUE)
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_membership')
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'location')
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_contract')
b1eff <- includeEffects(b1eff, absDiffX, interaction1 = 'b1_jobtitle')
b1eff <- includeEffects(b1eff, X, interaction1 = 'b1_iv')
b1eff <- includeEffects(b1eff, XRecip, interaction1 = 'b1_iv')
b1eff <- includeEffects(b1eff, WWX, interaction1 = 'b1_iv')
b1eff <- includeEffects(b1eff, OutWWX, interaction1 = 'b1_iv')

b9_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, 
                  batch=TRUE, returnDeps=TRUE)
b9_ans

# excluding reciprocity as not in data
b1eff <- includeEffects( b1eff,recip, include=FALSE)
b1eff <- includeEffects(b1eff, XRecip, interaction1 = 'b1_iv', include=FALSE)

b9_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, 
                  batch=TRUE, returnDeps=TRUE)
b9_ans
# converged

gofb1_6 <- sienaGOF(b9_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_6)
plot(gofb1_6)
# good. p.value 0.005, underestimate for indeg > 0

gofb1_7 <- sienaGOF(b9_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_7)
plot(gofb1_7)
#pval: 0.01, too many 0's andnoth enough 1 & 2

gofb1_8 <- sienaGOF(b9_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_8)
plot(gofb1_8)
#pval 0.004, need more 021D

b1eff <- includeEffects( b1eff, inPop, include=TRUE)
b9_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, 
                  batch=TRUE, returnDeps=TRUE)
b9_ans # converged on 1st run


gofb1_6 <- sienaGOF(b9_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_6)
plot(gofb1_6)
# good. 0.246

gofb1_7 <- sienaGOF(b9_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_7)
plot(gofb1_7, center=FALSE, scale=FALSE)
#pval: 0.015; outdegree 2 not well modeled

gofb1_8 <- sienaGOF(b9_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_8)
plot(gofb1_8)
#pval 0.007

b1eff <- includeEffects( b1eff, balance, include=TRUE)
b9_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE,
                  returnDeps=TRUE, prevAns=b9_ans)
b9_ans # converged on 2nd run

# doing GOF

gofb1_6 <- sienaGOF(b9_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_6)
plot(gofb1_6)
# good. 0.216

gofb1_7 <- sienaGOF(b9_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_7)
plot(gofb1_7, center=FALSE, scale=FALSE)
#pval: 0.01; outdegree 2 not well modeled

gofb1_8 <- sienaGOF(b9_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_8)
plot(gofb1_8)
#pval 0.212

b1eff <- includeEffects( b1eff, inIsDegree, include=TRUE)
# in-isolate Outdegree e↵ect, (inIsDegree), the (additional) 
# out-degree (or activity) effect for actors w
b9_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, 
                   returnDeps=TRUE, prevAns=b9_ans)
b9_ans # converged on 4 run

gofb1_6 <- sienaGOF(b9_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_6)
plot(gofb1_6) #p0.301

gofb1_7 <- sienaGOF(b9_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_7)
plot(gofb1_7, center=FALSE, scale=FALSE)
#pval: 0.072; outdegree 1 & 2 not well modeled


gofb1_8 <- sienaGOF(b9_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b1_dv")
summary(gofb1_8)
plot(gofb1_8) #p0.785


tt6 <- sienaTimeTest(b9_ans)
summary(tt6)
# time heterogeneity is an issue
# period 1 and 4 impacted
# effects: balance, location, membership and density
# including time effects ruins the model
# 
# 
b1eff <- includeTimeDummy(b1eff, density, timeDummy="1,4, 5", include=TRUE)
b10_ans <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE,
                  returnDeps=TRUE, prevAns = b10_ans)
b10_ans # converged on 2nd run

tt7 <- sienaTimeTest(b10_ans, effects = 1:13)
summary(tt7)


# test is positive for period 1 --> 
# remove period 1 OR add a time dummy for all of period 1 variables. 

# b1 no coevolution no ver 1 ----------------------------------------------


b1_dv_2345 <- sienaNet(array(c(dv_mat[[2]], dv_mat[[3]],
                          dv_mat[[4]], dv_mat[[5]], dv_mat[[6]]), 
                        dim =c(21, 21, 5)))

b1_iv_2345 <- varDyadCovar(array(c(iv_mat[[2]], iv_mat[[3]],
                              iv_mat[[4]], iv_mat[[5]]), 
                            dim =c(21, 21, 4)))
b1_jobtitle_2345 <- varCovar(jobt[,2:6])
b1_contract_2345 <- varCovar(cont[,2:6])
b1_membership_2345 <- varCovar(mem[,2:6])


branch1_2345 <- sienaDataCreate(b1_dv_2345, b1_iv_2345, 
                           b1_jobtitle_2345, location, 
                           b1_contract_2345, b1_membership_2345)# define data

b1eff_2345 <- getEffects( branch1_2345 )
b1eff_2345 # default effects
effectsDocumentation(b1eff_2345)

b1eff_2345 <- includeEffects(b1eff_2345, inPopSqrt, transTrip, include=TRUE)
b1eff_2345 <- includeEffects(b1eff_2345, sameX, interaction1 = 'b1_membership_2345')
b1eff_2345 <- includeEffects(b1eff_2345, sameX, interaction1 = 'location')
b1eff_2345 <- includeEffects(b1eff_2345, sameX, interaction1 = 'b1_contract_2345')
b1eff_2345 <- includeEffects(b1eff_2345, absDiffX, interaction1 = 'b1_jobtitle_2345')
b1eff_2345 <- includeEffects(b1eff_2345, X, interaction1 = 'b1_iv_2345')
b1eff_2345 <- includeEffects(b1eff_2345, WWX, interaction1 = 'b1_iv_2345')
b1eff_2345 <- includeEffects(b1eff_2345, OutWWX, interaction1 = 'b1_iv_2345')

#b1eff_2345 <- includeEffects( b1eff_2345, Xrecip, include=FALSE, interaction1 = 'b1_iv_2345')

b1eff_2345 <- includeEffects( b1eff_2345, recip, include=FALSE)
b1eff_2345 <- includeEffects( b1eff_2345, inPop, include=TRUE)
b1eff_2345 <- includeEffects( b1eff_2345, balance, include=TRUE)
b1eff_2345 <- includeEffects( b1eff_2345, inIsDegree, include=TRUE)

b11_ans <- siena07(myalgorithm, data=branch1_2345, effects=b1eff_2345, batch=TRUE,
                   returnDeps=TRUE)
b11_ans # converged on 2nd run

# that doesn't work at all. SE of density and in-isolate outdegree 
# shoots up. Remove inPop
b1eff_2345 <- includeEffects( b1eff_2345, inIsDegree, include=FALSE)

b11_ans <- siena07(myalgorithm, data=branch1_2345, effects=b1eff_2345, batch=TRUE,
                   returnDeps=TRUE)
# better. No total convergence, but no very high SE
b12_ans <- siena07(myalgorithm, data=branch1_2345, effects=b1eff_2345, batch=TRUE,
                   returnDeps=TRUE, prevAns = b11_ans)
b12_ans # converged


tt7 <- sienaTimeTest(b12_ans, effects = 1:12)
summary(tt7)
# no time homogeneity

b1eff_2345 <- includeTimeDummy(b1eff_2345, density, timeDummy="2,3", include=TRUE)
b1eff_2345 <- includeTimeDummy(b1eff_2345, sameX, interaction1 = 'location', timeDummy="2,3", include=TRUE)

b12_ans <- siena07(myalgorithm, data=branch1_2345, effects=b1eff_2345, batch=TRUE,
                   returnDeps=TRUE)
b12_ans <- siena07(myalgorithm, data=branch1_2345, effects=b1eff_2345, batch=TRUE,
                   returnDeps=TRUE, prevAns=b12_ans)

tt8 <- sienaTimeTest(b12_ans, effects = 1:12)
summary(tt8)

# time homogeneity just ok ( p = 0.07)


# now GOF for branch 1 (no version 1, no coevolution) ---------------------


# (smaller p values are worse)
# 
gof1 <- sienaGOF(b12_ans, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_dv_2345")
summary(gof1)
plot(gof1)
# p = 0.46 => good

gof2 <- sienaGOF(b12_ans, 
                 OutdegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="b1_dv_2345")
summary(gof2)
plot(gof2)
# p = 0.065, fit for outdegree < 4 could be better

gof3 <- sienaGOF(b12_ans, 
                 TriadCensus, 
                 verbose=TRUE, join=TRUE,
                 varName=c("b1_dv_2345"))
summary(gof3)
plot(gof3)
# p-value is 0.193 --> good fit


# effect test -------------------------------------------------------------


chi <- NULL
df <- NULL
pval <- NULL
oneside <- NULL
ef <- NULL
for (i in c(1:16)){
  tmp <- Multipar.RSiena(b12_ans, i)
  tmp <- unlist(tmp)
  
  chi <- c(chi, as.numeric(tmp[[1]]))
  df <- c(df, as.numeric(tmp[[2]]))
  pval <- c(pval, as.numeric(tmp[[3]]))
  oneside <- c(oneside, as.numeric(tmp[[4]]))
  ef <- c(ef, tmp[[5]])
}


b12_results <- tibble(effect = b12_ans$effects$effectName,
                     estimate = round(b12_ans$theta,3),
                     st.error = round(b12_ans$se,3),
                     zscore = round(estimate/st.error,3),
                     chi = chi,
                     df = df,
                     pval = round(pval,3),
                     oneside= round(oneside,3),
                     ef = ef)
b12_results %>% readr::write_csv("branch1_no_coev_no_ver1_results.csv")


# interpretation:
# Balance: If two developers are connected to the same person, 
# at t1, they  are less likely to start forming a tie at t2
# Indegree pop+ sqr: There is a U-shaped RS btw indegree
# at t1 and at t2. 





# branch 2 ----------------------------------------------------------------

b2_jobtitle <- varCovar(jobt[,c(1:2, 7:10)])
b2_contract <- varCovar(cont[,c(1:2, 7:11)])
b2_membership <- varCovar(mem[,c(1:2, 7:11)])

b2_dv <- sienaNet(array(c(dv_mat[[1]], dv_mat[[2]], dv_mat[[7]],
                          dv_mat[[8]], dv_mat[[9]], dv_mat[[10]],
                          dv_mat[[11]]), 
                        dim =c(21, 21, 7)))

b2_iv <- sienaNet(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[7]],
                          iv_mat[[8]], iv_mat[[9]], iv_mat[[10]],
                          iv_mat[[11]]), 
                        dim =c(21, 21, 7)))

branch2 <- sienaDataCreate(b2_dv, b2_iv, 
                           b2_jobtitle, location, b2_contract, b2_membership)# define data

print01Report(branch2, modelname = 'branch2_developer_coordination' )

b2eff <- getEffects( branch2 )
b2eff # default effects
effectsDocumentation(b2eff)


# check myalgorithm. They should be the same for both branches
myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    nsub = 4,
                                    n3 = 5000)


b2eff <- includeEffects( b2eff, crprod, type = 'endow', name = "b2_iv",interaction1 = 'b2_dv')
b2eff <- includeEffects( b2eff, crprod, type = 'eval', interaction1 = 'b2_iv', include=TRUE)
b2eff <- includeEffects( b2eff, inPopSqrt, transTrip, include=TRUE)
b2eff <- includeEffects(b2eff, sameX, interaction1 = 'b2_membership')
b2eff <- includeEffects(b2eff, sameX, interaction1 = 'location')
b2eff <- includeEffects(b2eff, sameX, interaction1 = 'b2_contract')
b2eff <- includeEffects(b2eff, absDiffX, interaction1 = 'b2_jobtitle')
b2eff <- includeEffects(b2eff, inPopSqrt, name ='b2_iv') #skewed pop
b2eff <- includeEffects(b2eff, isolateNet, include=TRUE)
b2eff <- includeEffects(b2eff, isolateNet, name = "b2_iv", include=TRUE)
b2eff <- includeEffects(b2eff, inPop, include=TRUE)
b2eff <- includeEffects(b2eff, inPop, name = 'b2_iv', include=TRUE)

b2eff <- includeEffects( b2eff,recip, include=FALSE)
b2eff

b2_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE)
b3_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE, prevAns = b2_ans)
save.image("~/Documents/gitrepo/developer_sna/analysis/branch2.RData")
# convergence is less good for branch2 than for branch 1
# 
b4_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE, prevAns = b3_ans)
b5_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE, prevAns = b4_ans)
save.image("~/Documents/gitrepo/developer_sna/analysis/branch2.RData")
b6_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE, prevAns = b5_ans)
b6_ans
# this does not work...time dummies? first a simple model
# 
b2eff <- getEffects( branch2 )

b6_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE)
b6_ans
# ok convergence, now let's add control variables
effectsDocumentation(b2eff)

b2eff <- includeEffects(b2eff, sameX, interaction1 = 'b2_membership')
b2eff <- includeEffects(b2eff, sameX, interaction1 = 'location')
b2eff <- includeEffects(b2eff, sameX, interaction1 = 'b2_contract')
b2eff <- includeEffects(b2eff, absDiffX, interaction1 = 'b2_jobtitle')
b6_ans <- siena07(myalgorithm, data=branch2, effects=b2eff, batch=TRUE, returnDeps=TRUE)
b6_ans
save.image("~/Documents/gitrepo/developer_sna/analysis/branch2.RData")

gofb2_1 <- sienaGOF(b6_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_dv")
summary(gofb2_1) #no fit (p =0)
plot(gofb2_1)
# good. p.value 0.118
# underestimated for indegree > 2

gofb2_2 <- sienaGOF(b6_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_dv")
summary(gofb2_2)
plot(gofb2_2)
# goodish (p = 0.002)
# fit for outdegree > 1 

gofb2_3 <- sienaGOF(b6_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_dv")
summary(gofb2_3) # p = 0
plot(gofb2_3)
# triad 012, 021C (two paths) not enough
# triad 021U (2-instar) and 003 (empty triad) too many 


gofb2_4 <- sienaGOF(b6_ans, 
                    IndegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_iv")
summary(gofb2_4)
plot(gofb2_4)
# pvalue is 0.004
# indegree > 1 & < 5 underestimate


gofb2_5 <- sienaGOF(b6_ans, 
                    OutdegreeDistribution, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_iv")
summary(gofb2_5) #p = 0.003
plot(gofb2_5)
# undestimate > 0 and < 4

gofb2_6 <- sienaGOF(b6_ans, 
                    TriadCensus, 
                    verbose=TRUE, join=TRUE,
                    varName="b2_iv")
#summary(gofb2_6) #p = 0.003
#plot(gofb2_6)
#subscript out of bounds

b2eff <- includeEffects( b2eff, crprod, type = 'endow', name = "b2_iv",interaction1 = 'b2_dv')
b2eff <- includeEffects( b2eff, crprod, type = 'eval', interaction1 = 'b2_iv', include=TRUE)
