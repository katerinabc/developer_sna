# TERGM: test how much communication realized depends on developer attributes, developer dyadic variables, and required communication
# 
# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 15 March 2021
# updated: 6 Dec 2021
# 
# TODO add joiners to the team 
# TODO add vertex information
# TODO convert NA in vector attributes to ??

rm(list=ls())
set.seed(seed = 1)
# packages ----------------------------------------------------------------

library(tergm)
library(tsna)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)

source("functions_developer_sna.R", echo = F)

load('data for tergm.RData') # loads all data

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
# 
# all networks are dichotomized using a median split


# hypothesized RS ---------------------------------------------------------



# load data ---------------------------------------------------------------

source("creating_networks.R", echo = F)



# uncomment everything until 'descriptive analysis if you have problems
# loading the RData file and need to create the data objects
ivatt_master2 <- read_csv("authatt.csv")
ivatt_master2 <- ivatt_master2[,-c(1:2)]
all_developers <- as.vector(unique(ivatt_master2$author))

ivatt_master <- unique(ivatt_master2 %>% select(-ver2))
ivnet_master <- read_csv("crequired_el.csv")
# columnn names information
# time = version
# sum_weights = sum of edges between dev.x and dev.y for each version
# n = sum of weights
# weights = number of connections between two files

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


iv_g <- lapply(iv_first, function(x) 
  network(x, directed=T))

ivdyn <- networkDynamic(network.list = iv_g,
                        #onsets = seq(0, 10, 1),
                        #termini = seq(1, 11, 1),
                        #start = 1,
                        #end = 11
                        )
ivdyn

# 
# 
# prep DV network ---------------------------------------------------------



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
                  data = dv_modified)

  dv_first[[i]] <- tmp

}

for (i in 1:length(dv_first)){
  set.edge.value(dv_first[[i]], 'required_comm', value = iv_first[[i]])
  # the edge values are only set for active edges
}



# add developer information -----------------------------------------------


# add network attributes

# in the following loop information for developers who are members of version 2 is stored in a
# number of temporary files (all beginning with tmp_).
# If a developer is not member of the version the number 99 is added.
#participants <- NULL

for (i in 1:11){
  jobtitle <- NULL
  location <- NULL
  contract <- NULL
  member <- NULL
  print(i)
  for (j in network::get.vertex.attribute(dv_first[[i]], 'vertex.names')){
 # print(j)

  atmp <- as.vector(unique(ivatt_master2[ivatt_master2$author == j &
                                  ivatt_master2$ver2 == i,
                                3:5]))
  if (nrow(atmp) == 0) {atmp <- tibble(jobtitle_raw = NA,
                                       location = NA,
                                       contract = NA)}
  #print(atmp)
  jobtitle <- c(jobtitle, pull(atmp[1]))
  print( jobtitle)
  location <- c(location, pull(atmp[2]))
  print(location)
  contract <- c(contract, pull(atmp[3]))
  print(contract)

  memtmp <- if_else(j %in% pull(ivatt_master2[ivatt_master2$ver2 == i, 1]), 1, 0)

  member <- c(member, memtmp)

  }
  network::set.vertex.attribute(dv_first[[i]], 'jobtitle', as.numeric(jobtitle))
  network::set.vertex.attribute(dv_first[[i]], 'location', as.numeric(location))
  network::set.vertex.attribute(dv_first[[i]], 'contract', as.numeric(contract))
  network::set.vertex.attribute(dv_first[[i]], 'membership', as.numeric(member))

}
# a note on membership
# a 0 in membership means they did not join this project
# people in version 1 work on version 2

# # add cumulative familiarity ----------------------------------------------
# 
# # do this later
# 
# # create dynamic network object (DV) -------------------------------------------
# 
# netdyn <- networkDynamic(network.list = dv_first,
#                            #onsets = seq(0, 10, 1),
#                            #termini = seq(1, 11, 1),
#                            start = 1, 
#                            end = 11,
#                          create.TEAs = TRUE
#                          )
# netdyn
# #edge.TEA.names ??? needed

# descriptive & plot ------------------------------------------------------------

round(tSnaStats(netdyn, snafun = 'gden'),2)
round(tSnaStats(netdyn, snafun = 'gtrans' ),2)
#round(tSnaStats(nd_first, snafun = 'degree' ),2)
tEdgeDensity(netdyn, mode = 'duration')

# uncomment these lines to generate the video
# render.d3movie(netdyn,
#                plot.par=list(displaylabels=T),
#                output.mode = 'HTML',
#                filename='cr.html',
#                #plot.par=list(displaylabels=T)
#                )
#filmstrip(netdyn, frames = 11)
#timeline(netdyn)

proximity.timeline(netdyn,
                   default.dist = 6,
                   mode = 'sammon',
                   labels.at = 17,vertex.cex = 4)
# each line represents a developer. Lines that are nearby are 
# developers who are nearby in the network. 
# A lot of criss-cross indicates big structural changes in the network
# changes happen at version 3 and version 6. 
# After version 6 the structure remains stable





# tergm -------------------------------------------------------------------

#testcase. 
m1 <- tergm(netdyn ~ #edgecov(ivdyn) + 
                    edge.ages() + 
                     Form(~edges) +
                     Persist(~edges),
                   times = c(1:5),
                   estimate = "CMLE",
                   verbose=TRUE
)

mcmc.diagnostics(m1)
summary(m1)
gof(m1)

# basis case. Interpretation not useful

# How to inlcude the required communitcation????
m1 <- tergm(netdyn ~ 
              nodefactor('membership') + 
              #edgecov(iv_first[[1]]) + 
              #edgecov('required_comm.active') + 
              
               
              Form(~edges +
                     degree(1) + 
                     cyclicalties # triad, non-hierarchical
                    # edgecov('required_comm') # attribute not recognized
                    
                   ) +
              Persist(~edges + 
                        degree(1) + 
                        cyclicalties
                      ),
            times = c(1:5),
            estimate = "CMLE",
            verbose=TRUE
)

mcmc.diagnostics(m1)
summary(m1)

# degree 1 tests if ties are more likely to form/dissolve when 
# developers have 1 degree (are not isolates); this tests a tendency against
# isolates
# 
# 
# 