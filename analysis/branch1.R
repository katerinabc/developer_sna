# # Rsiena for branch 1
# 
# Testing co-evolution of collaboration network and task 
# interdependence
# 
# author: Katerina Bohle Carbonell
# 
# created: 25-01-2022
# updated: 12-04-2022
# 
# dependent on
# helper functions: ("functions_developer_sna.R", echo = F)

# TUTORIAL FOR RSIENA: 
# scripts: https://www.stats.ox.ac.uk/~snijders/siena/siena_scripts.htm

rm(list = ls()) # clean everything
set.seed(1234)

# TODO: 
# transform actor attributes to characters

# packages ----------------------------------------------------------------

library(RSiena)
library(network)
library(dplyr)


source("functions_developer_sna.R", echo = F)
#load('data for rsiena.RData') # loads all data, but this is an undirected network

# load data ---------------------------------------------------------------

#source("creating_networks.R", echo = F)

# The csv file has been created in the file 'comm_realized.R'
# It contains all edge values
dv_master <- readr::read_csv("communication_realized_edge_weight_by_version.csv")
ivnet_master <- readr::read_csv("crequired_el.csv")
ivatt_master2 <- readr::read_csv("authatt.csv")

# modify attribute data ---------------------------------------------------


# CHANGE from numbers to letters to avoid creating an artificla hierarchy
# for job titles 
# for location
# for contract

# fct_relabel automatically relabels the numbers into letters
# the order is the same (1 = a, 2 = b, 3 = c etc)
# the variable (here location) is first transformed from a numerical
# variable to a categorical variable (called factor in R)
# then the numbers are transformed to letters using fct_relabel
# 
# 
# DECISION: KBC DECIDED TO NOT DO THIS AS BUG IN RSIENA COVAR CREATION
# CAN'T USE A FACTOR
# 
# ivatt_master2$location <- as.factor(ivatt_master2$location)
# ivatt_master2$location<- forcats::fct_relabel(
#   as.factor(ivatt_master2$location), function(x) letters[1:length(levels(ivatt_master2$location))])
# 
# ivatt_master2$jobtitle_raw <- as.factor(ivatt_master2$jobtitle_raw)
# ivatt_master2$jobtitle_raw<- forcats::fct_relabel(
#   as.factor(ivatt_master2$jobtitle_raw), 
#   function(x) letters[1:length(levels(ivatt_master2$jobtitle_raw))])
# 
# ivatt_master2$contract <- as.factor(ivatt_master2$contract)
# ivatt_master2$contract<- forcats::fct_relabel(
#   as.factor(ivatt_master2$contract), 
#   function(x) letters[1:length(levels(ivatt_master2$contract))])


ivatt_master2 <- ivatt_master2[,-c(1:2)]
all_developers <- as.vector(unique(ivatt_master2$author))

ivatt_master <- unique(ivatt_master2 %>% select(-ver2))

# specify Rsiena objects DV --------------------------------------------------

# dv_modified is dichotomized DV network. 
# It is dichotomized using the first quantile.
# the if_else statement dichotomzies the data:
# Ties that are above the 25th percentile are turned to a 1, 
# everything else is a 0
dv_modified <-
  dv_master %>%
  group_by(ver) %>%
  mutate(tie_first = if_else(n > quantile(n)[2], 1, 0)
  )

# In this loop, for every version a matrix is created. The function 'netpanel'
# is specificially creted for this project. It is stored in functions_developer_sna.R.
# thresholdname = name of column
# ver = version
# x = name of column with sender 
# y = name of column with targed
# data = the dataset 
# mode_directed = if the network should be directed
# as_network = if the return object should be a network or a matrix

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

# dv is a siena object. it combines all the DV matrices
# dim tells siena the dimension of each matrix and how many maatrics
# are part of this wave
dv <- sienaNet(array(c(dv_mat[[1]], dv_mat[[2]], dv_mat[[3]],
                       dv_mat[[4]], dv_mat[[5]], dv_mat[[6]],
                       dv_mat[[7]], dv_mat[[8]], dv_mat[[9]],
                       dv_mat[[10]], dv_mat[[11]]), 
                     dim =c(21, 21, 11)))

print(dv)
branch1_dv <- sienaNet(array(c(dv_mat[[3]],
                            dv_mat[[4]], dv_mat[[5]], dv_mat[[6]]), 
                          dim =c(21, 21, 4)))

# add developer information -----------------------------------------------
# copied from tergm

# in the following loop information for developers who are members of version 2 is
#  stored in a number of temporary files (all beginning with tmp_).
# If a developer is not member of the version the number 99 is added.
#participants <- NULL
#


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
    
    if (purrr::is_empty(atmp)) {atmp <- tibble(jobtitle_raw = NA,
                                               location = NA,
                                               contract = NA
                                               #member = "no"
    )}
    
    #print(atmp)
    jobtitle <- c(jobtitle, (pull(atmp, jobtitle_raw)))
    #jobtitle <- c(jobtitle, as.character(pull(atmp, jobtitle_raw)))
    # print( jobtitle)
    location <- c(location, (pull(atmp, location)))
    #location <- c(location,as.character(pull(atmp, location)))
    # print(location)
    contract <- c(contract, (pull(atmp, contract)))
    #contract <- c(contract, as.character(pull(atmp, contract)))
    # print(contract)
    # member <- c(member, pull(atmp[4]))
    # print(member)
    
    memtmp <- if_else(j %in% pull(ivatt_master2 %>% 
                                    filter(ver2 == i) %>% 
                                    select(author)),
                      1, 0)
    
    member2 <- c(member2, memtmp)
    
  }
  network::set.vertex.attribute(dv_first[[i]], 'jobtitle', jobtitle)
  network::set.vertex.attribute(dv_first[[i]], 'location', location)
  network::set.vertex.attribute(dv_first[[i]], 'contract', contract)
  #network::set.vertex.attribute(dv_first[[i]], 'membership', as.numeric(member))
  network::set.vertex.attribute(dv_first[[i]], 'membership', member2)
  
}
# a note on membership
# a 0 in membership means they did not join this project
# people in version 1 work on version 2

# specify Rsiena objects covariates -----------------------------------------------

# create the changing and constant covariates 
# for every changing independent variable we need to create a matrix
# with n rows (actors) and m columns (time periods)
# 
# at first the variables are created for version 1 until version 11. 
# I explain afterwards how to create these siena objects for only 
# branch 1.

# There might be a bug in the code. CoCovar requires the input vector to be 
# (1) a vector AND (2) numeric OR a factor. But a vector can not be factor
# Don't use covariates as weights

# locations - does not change
loc <- NULL
for (i in 1: length(dv_mat)){
  tmp <- as.factor(dv_first[[i]] %v% 'location')
  loc <- cbind(loc, tmp)
}
#location <- as.factor(loc[,1])
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


# to create a variable covariate only for branch 1
# you need to indicate which colummns to use.
# for example if you only use version 3 until version 6,
# limit the columns to 3:6
b1_jobtitle <- varCovar(jobt[,3:6])
b1_contract <- varCovar(cont[,3:6])
b1_membership <- varCovar(mem[,3:6])



# specify Rsiena objects dyadic covariates --------------------------------------------

# #  IV (technical dependencies) --------------------------------------------
# 
# # colnames in iv not the same than in dv. fix for function to work
colnames(ivnet_master)[3] <- "ver"


# the code follows the same structure as for DV.
# the function dynmatrix creates directed networks and returns a matrix
# 
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

iv_mat <- list()
for(i in 1: length(iv_first)){
  #fixes an issue that some ties are 2 and not 1. 
  iv_mat[[i]] <- iv_first[[i]]
  iv_mat[[i]][iv_mat[[i]] ==2 ] <- 1
}


# to add IV as another network variable, create another sienaNet object. 
iv <- sienaNet(array(c(iv_mat[[1]], iv_mat[[2]], iv_mat[[3]],
                       iv_mat[[4]], iv_mat[[5]], iv_mat[[6]],
                       iv_mat[[7]], iv_mat[[8]], iv_mat[[9]],
                       iv_mat[[10]], iv_mat[[11]]), 
                     dim =c(21, 21, 11)))

branch1_iv <- sienaNet(array(c(iv_mat[[3]],
                       iv_mat[[4]], iv_mat[[5]], iv_mat[[6]]), 
                     dim =c(21, 21, 4)))

# iv is currently build in such a way that IV(ver1) is happening at the same
# time than DV(ver1). Based on this Rsiena will test:
# impact of iv(ver1) on iv(ver2)
# impact of dv(ver1) on dv(ver2)
# impact of iv(ver1) on dv(ver2)
# impact of dv(ver1) on iv(ver2)
# 
# If you want to change this, you need to change how iv is build. For example:

iv_2 <- sienaNet(array(c(iv_mat[[2]], iv_mat[[3]],
                       iv_mat[[4]], iv_mat[[5]], iv_mat[[6]],
                       iv_mat[[7]], iv_mat[[8]], iv_mat[[9]],
                       iv_mat[[10]], iv_mat[[11]], iv_mat[[11]]), 
                     dim =c(21, 21, 11)))
# in iv_2, the first wave is now version 2. We still need 11 waves as this
# network is happened at the same time than the DV network. version 11 
# therefore exists twice. That might create other problems down the road
# as now there is no change in one network from t10 to t11

# create rsiena formula ---------------------------------------------------

# mydata combines all the siena objects into one dataset
# DV read in as a DIRECTED network
# IV read in as a DIRECTED network
branch1 <- sienaDataCreate(branch1_dv, branch1_iv, 
                          b1_jobtitle, location, b1_contract, b1_membership)


# This report is a simple text file that shows  you basic info about
# your data. It is good practice to have a look at it and see if everything
# makes sense
print01Report(branch1, modelname = 'developer_coordination' )

# define features of the algorithm. Here we just add a project name
# but keep everything else as default values
myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model')

# get effects -------------------------------------------------------------

# create an effect object. This will be used to tell siena which effects
# should be tested
b1eff <- getEffects( branch1 )
# show the list of effects
b1eff
# gives you a html document listening all the effects you can add. 
# it opens in a webbrowser.
effectsDocumentation(b1eff)


# When running a model:
# Step 1: add the effects you want to test using includeEffects
#  (more about this below)

# Step 2: Check if the model converged. If not run it again (explained below)
# For converence the rule of thumb is that all t-ratios for convergence
# should ideally be less than 0.1 in absolute value,
# and the "Overall maximum convergence ratio" should be less than 0.25;
# this signifies good convergence of the algorithm.

# Step 3: Check time heterogeneity
# 
# Step 4: Check goodness of fit
#  (ideally before doing step 5 change the algorithm to run longer to make sure
#  your results are stable)
# Step 5: check p-values of estimates and see what is significant


# model 1: basic model ----------------------------------------------------

# here we are testing only the standard effects:
# the effect object always includes outdegre and reciprocity for DV and IV.
# I decided to include another basic structural model (TransTrip) 
# as a standard network structure
# the effect name is transTrip
# include is set to TRUE as I want to include it. Change include to FALSE
# if you want to remove an effect
# the name needs to be added for transTrip effect for IV network to differentiate
# it from the other transTrip network. 
b1eff <- includeEffects( b1eff, transTrip, include=TRUE)
b1eff <- includeEffects( b1eff, transTrip, name='branch1_iv', include=TRUE)
#ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
#ans1

# have a quick loog at the convergence ratios, estimates and SE. As this is not
# the final model it doesnt' really matter what they are exactly.
# However if they are off by a lot this might indicate problems
# further down the line.


# model 2: add co-evolution  ----------------------------------------------

# it would be good to read the rsiena manual p 13-15 to understand this
# with siena 3 types of tie changes can be analyzed
# 1. evaluation of ties: this tests if ties are present at t2
# that weren't present at t1
# 2. creation of ties: this tests if ties are created at t2
# that didn't exists at t1. It only takes account new ties
# 3. endowment of ties: This tests if ties are maintained at t2
# that already exists at t1.
# 
# Example 1:
# imagine you want to test the impact IV (t0) has on DV(t1). 
# You are using the network evaluation function. 
# A positive estimate will tell you that if:
# developer i and j had an IV tie in t0 they are likely to have a DV tie at t1
# developer i and j had NO IV tie in t0 they are likely to have a DV tie at t1
# Here you assume that there is no difference between creating
# a tie and maintaining an existing tie.
# 
# 
# Example 2:
# imagine you want to test the impact IV (t0) has on DV(t1). 
# You are using the network creation function. 
# A positive estimate will tell you that if:
# developer i and j had NO IV tie in t0 they are likely to have a DV tie at t1


# Adding the co-evolution effect
# this is where the html file with all the effecs is necessary
# the function to include an effect is 'includeEffects'
# crprod is the name of the effect. As there are several crprod's effect you need to specify it
# to specify it add the name and interaction. All that information is in the html document

b1eff <- includeEffects( b1eff,  crprodRecip, crprod, betweenPop, name = "branch1_iv",interaction1 = "branch1_dv" )
b1eff <- includeEffects( b1eff, crprodRecip, crprod, betweenPop, name = "branch1_dv", interaction1 = "branch1_iv" )

# the output will show you if the effect was added. You can also run b1eff to see all of the effects
b1eff

# run the model. as model ans1 was the very basic model I decided to overwrite it
#ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
#ans1

# convergence is good.
# large SE for iv (period 1), but it's only twice the estimate.
# maybe an issue later on.


# add actor effects -------------------------------------------------------

b1eff <- includeEffects(b1eff, sameX, interaction1 = 'location', include=TRUE)
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_contract', include=TRUE)
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_membership', include=TRUE)
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_jobtitle', include=TRUE)

b1eff <- includeEffects(b1eff, sameX, interaction1 = 'location', include=TRUE, name = "branch1_iv")
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_contract', include=TRUE, name = "branch1_iv")
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_membership', include=TRUE, name = "branch1_iv")
b1eff <- includeEffects(b1eff, sameX, interaction1 = 'b1_jobtitle', include=TRUE, name = "branch1_iv")


ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
ans1

# 12/04/2022
# t-ratios for eval branch1_dv: reciprocity with branch1_iv
# and branch1_dv: betweenness^(1/2) branch1_iv popularity
# are just above 0.1.
# On a 2nd run convergence was ok
# On a 3rd run maxium convergence was 0.2570
# On a 4th run, t-ratio for branch1_dv: reciprocity 0.1190

ans2 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, prevAns = ans1)
ans2


# time homogeneity for ans1 -----------------------------------------------

tt <- sienaTimeTest(ans2, effects = c(4, 6:11, 17:24))
summary(tt)

# check summary(tt)
# the joint significance test should be non-significant.
# then I go and check for where the problem is: 
# What effect (effect-wise) and what period (period wise)
# Here it looks like period 1 (ver 3) and 2 (ver 4)
# and effect density and transitive triplets

# have a look at this script for more information:
# https://www.stats.ox.ac.uk/~snijders/siena/RscriptSienaTimeTest.R
# The script has been created by Professor Tom Snijders
# more scripts are available here:
# https://www.stats.ox.ac.uk/~snijders/siena/siena_scripts.htm

# result for time-test
# joint significance test is significant
# effect wise issues: membership & location for dv
# period wise: period 1 and 2

# to include a time dummy you need to add
b1eff <- includeTimeDummy(b1eff, sameX, interaction1 = 'location', timeDummy="2")
b1eff <- includeTimeDummy(b1eff, sameX, interaction1 = 'b1_membership', timeDummy="2")


# new model with time variables -------------------------------------------

ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
ans1

# that's even worse. 
b1eff <- includeTimeDummy(b1eff, sameX, interaction1 = 'location', timeDummy="2", include = FALSE)
b1eff <- includeTimeDummy(b1eff, sameX, interaction1 = 'b1_membership', timeDummy="2", include = FALSE)

# included a timedummy for density for time period 1 and 
# dv network because the effect wise pval for density is 0 
# and pvalue for period 1 is 0.001, and
# pval for dummy (dv X density) is 0.001
# controlled for refernece point (period 1) 
# instead of period 2 as more issues seem to be at the start
b1eff <- includeTimeDummy(b1eff, density, timeDummy="1")

ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
ans1

# convergence slightly bad (0.2576)
b1eff <- includeTimeDummy(b1eff, density, timeDummy="1", include = FALSE)

b1eff <- includeTimeDummy(b1eff, egoX, density, timeDummy = "all")

ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
ans2 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, prevAns = ans1)
# standard errors not reliable
# advised to drop reciprocity (which matches
# previous experience running models and hte data
# )

b1eff <- includeEffects(b1eff, recip, include=FALSE)

ans1 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE)
ans2 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, prevAns = ans1)
ans2
# t-ratio for 1 effect betweeness sqr is -0.1084....
# rest is ok

# GOF for ans1 ------------------------------------------------------------

# to calculate GOF the function to run the siena model needs to be 
# modified. You need to add returnDeps=TRUE 

# goodness of fit is explained in the Rsiena manual on p. 58 - 60
ans2 <- siena07(myalgorithm, data=branch1, effects=b1eff, batch=TRUE, returnDeps=TRUE)

#the function tests how far away the simulated points are from 
# the real points. The further away = the worse the goodness of fit
# here this is tested using the distribution of indegrees
gof1a <- sienaGOF(ans2, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="branch1_dv")
summary(gof1a)
plot(gof1a) # pvalue of 0 => bad
# no fit, 
# indegree of 0 overestimated
# indegrees above 2 underestimated

gof1b <- sienaGOF(ans2, 
                 IndegreeDistribution, 
                 verbose=TRUE, join=TRUE,
                 varName="branch1_iv")
summary(gof1b)
plot(gof1b) #pvalue of 0.103 -> good


# the p value is 0. This means the model is very bad. 
# looking at the plot you can see that the current model:
# does not model properly 0-indegrees and overestimates the number
# of actors who have an indegree of 3 or higher.

# You should run gof also for outdegree distribution and triads.
# Look at everything that is not well estimated and then decide
# what structural effects you can add that will help you achieve 
# a better model fit

gof2a <- sienaGOF(ans2, 
                  OutdegreeDistribution, 
                  verbose=TRUE, join=TRUE,
                  varName="branch1_dv")
summary(gof2a)
plot(gof2a) # pvalue of 0.011 => bad
# no fit, 
# indegree of 0 overestimated
# indegrees above 2 underestimated

gof2b <- sienaGOF(ans2, 
                  OutdegreeDistribution, 
                  verbose=TRUE, join=TRUE,
                  varName="branch1_iv")
summary(gof2b)
plot(gof2b) #pvalue of 0.141 -> good\

gof3a <- sienaGOF(ans2, 
                  TriadCensus, 
                  verbose=TRUE, join=TRUE,
                  varName="branch1_dv")
summary(gof3a)
plot(gof3a) # pvalue of 0.045 => bad
# no fit, 
# hard to see from the plot

gof3b <- sienaGOF(ans2, 
                  TriadCensus, 
                  verbose=TRUE, join=TRUE,
                  varName="branch1_iv")
summary(gof3b)
plot(gof3b) #pvalue of 0.17 -> good


# re-run model for papr ---------------------------------------------------

# once GOF is good, modify the algorithm to run longer. 
# rerun the final model, do all the checks again (convergence, 
# time homogeneity, gof). If all is ok you can go ahead and 
# see if the effects are significant.
myalgorithm <- sienaAlgorithmCreate(projname = 'developer_coordination_model',
                                    gmm = FALSE,
                                    nsub = 4,
                                    n3 = 5000)

# add here the code to re-run everything. 


# testing if effects are significant --------------------------------------
# 
siena.table(ans2, type = "html",
            vertLine=TRUE, tstatPrint=FALSE, sig=TRUE, d=3, nfirst=NULL)

# # the following lines are copied from the rsiena website (linked above in the
# # section on time homogeneity)
# chi <- NULL
# df <- NULL
# pval <- NULL
# oneside <- NULL
# ef <- NULL
# for (i in c(1:14)){ # you need to modify this line based on the number
#   # of effects  you have. You can see how many effects you have by
#   # inspecting your effect object (b1eff) or your results.
#   # for the simple model (ans1) there are only 14 effects.
#   tmp <- Multipar.RSiena(ans1, i)
#   tmp <- unlist(tmp)
#   
#   chi <- c(chi, as.numeric(tmp[[1]]))
#   df <- c(df, as.numeric(tmp[[2]]))
#   pval <- c(pval, as.numeric(tmp[[3]]))
#   oneside <- c(oneside, as.numeric(tmp[[4]]))
#   ef <- c(ef, tmp[[5]])
# }
# 
# 
# ans1_results <- tibble(effect = ans1$effects$effectName,
#                       estimate = round(ans1$theta,3),
#                       st.error = round(ans1$se,3),
#                       chi = chi,
#                       df = df,
#                       pval = round(pval,3),
#                       oneside= round(oneside,3),
#                       ef = ef)
# 
# #this saves the estimate, p value etc in a csv file
# ans1_results %>% readr::write_csv("GIVE IT A NAME THAT  YOU CAN REMEMBER.csv")

