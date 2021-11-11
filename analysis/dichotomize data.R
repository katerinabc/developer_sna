rm(list=ls())

# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 04 November 2021
# updated: 09 November 2021
# 
# TODO check if names correctly added in original network
 

# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)



# create networks ---------------------------------------------------------

source("creating_networks.R")
all_developers <- as.vector(dv %v% 'vertex.names')

# calculate thresholds ---------------------------------------------------------

dv_thresholds <- 
  dv_master %>% 
  group_by(ver) %>% 
  summarize(mean = mean(n),
            median = median(n),
            threequarters = quantile(n)[4],
            sd = sd(n),
            mean_std = (mean(n) + sd(n)))

dv_thresholds


# dichotomize networks ----------------------------------------------------

# dichotomize edgelist
dv_modified <- 
  dv_master %>% 
  group_by(ver) %>% 
  mutate(tie_mean = if_else(n > mean(n), 1, 0),
         tie_median = if_else(n > median(n), 1, 0),
         tie_three4th = if_else(n > quantile(n)[4], 1, 0),
         tie_three4th = if_else(n > quantile(n)[4], 1, 0),
         tie_mean_std = if_else(n > (mean(n) + sd(n)), 1, 0)
         )

# count number of edges per network
dv_modified %>% 
  select(starts_with('tie_')) %>% 
  group_by(ver) %>%
  pivot_longer(cols = starts_with("tie"),
               names_to="tie",
               values_to="edge") %>%
  group_by(ver, tie) %>%
  summarize(mean = sum(edge)) %>%
  pivot_wider(names_from = tie, 
              values_from = mean) %>%
  write_csv("dv_sum_edges_by_threshold.csv")

cutnetworks <- function(thresholdname, version) {
  colnumbers <- match(c("author", "folder_owner", "ver", thresholdname), 
                      colnames(dv_modified))
  tmp1 <- dv_modified[dv_modified$ver == version, colnumbers]
  
  # create network
  net <- network(tmp1[,-3], directed=F, matrix.type='edgelist')
  
  # add missing developers
  existing_dev <- as.vector(net %v% 'vertex.names')
  missing_dev <- all_developers[-match(net %v% 'vertex.names', dv %v% 'vertex.names')]
 # net <- add.isolates(net, length(missing_dev), return.as.edgelist = T)
  net <- as.network(add.isolates(net, length(missing_dev)), matrix.type = 'edgelist')
  net %v% 'vertex.names' <- c(existing_dev, missing_dev)
  net %v% 'degree' <- degree(net, gmode = "graph", cmode="freeman")
  net %v% 'betweenness' <- betweenness(net, gmod = "graph", cmode = "undirected")
  return(net)
  
  #newlist <- list()
  # for (i in 1:1 ){
  #   print(i)
  # tmp1 <- tmp1[dv_modified$ver == i,]
  # tmpnet <- network(tmp1[,-3], 
  #                   directed=FALSE,
  #                   matrix.type = 'edgelist') 
  # 
  # }
}



# original valued network -------------------------------------------------

# load('cr_dyn.RData') #based on df, file by author dataset. this shows where interaction did happen
# dv_original <- cr_dyn
# 
# # save dv_original as a list of networks
# dv_original <- get.networks(cr_dyn, start = 1, end = 12)

# in dv_orignal developer names replaced by developer ID
# load developer names and id

#devnames <- read_csv("developer_name_id.csv")

# replace ID with developer name in dv_original

dv_original <- NULL
for (i in 1: max(dv_modified$ver) {
  colnumbers <- match(c("author", "folder_owner", "ver", "n"), 
                    colnames(dv_modified))
  tmp1 <- dv_modified[dv_modified$ver == i, colnumbers]
  
  # create network
  net <- network::network(tmp1[,-3], directed=F, 
                 ignore.eval=F, names.eval='weights', matrix.type = 'edgelist')
  
  # add missing developers
  existing_dev <- as.vector(net %v% 'vertex.names')
  missing_dev <- all_developers[-match(net %v% 'vertex.names', dv %v% 'vertex.names')]
  
  existing_vertx_id <- length(existing_dev) + 1

  net <- add.vertices(net, length(missing_dev))
  net <- set.vertex.attribute(net, 
                       'vertex.names', 
                       value = as.vector(missing_dev),
                       v = c(existing_vertx_id: 21))
  

  net <- as.network(add.isolates(net, length(missing_dev)), matrix.type = 'edgelist')
  net %v% 'vertex.names' <- c(existing_dev, missing_dev)
  net %v% 'degree' <- degree(net, gmode = "graph", cmode="freeman")
  net %v% 'betweenness' <- betweenness(net, gmod = "graph", cmode = "undirected")
}




# network cut-off = mean --------------------------------------------------



#test <- cutnetworks("tie_mean", version = 1)

dv_mean <- list()
for (i in 1: max(dv_master$ver)){
  print(i)
  tmp <- cutnetworks("tie_mean", version = i)
  
  dv_mean[[i]] <- tmp
  
}
dv_mean

lapply(dv_mean, function(x) plot(x)) # it works
lapply(dv_mean, function(x) plot(as.vector(x %v% 'degree')))

#colors <- RColorBrewer::brewer.pal(11, name = "Set3")
ggplot(data.frame(value=dv_mean[[1]] %v% 'degree', 
                 person = 1:21), aes(x = person, y = value)) + geom_point()



# testing correlation between networks ------------------------------------
result <- NULL
for (i in 1:2){
  original <- dv_original[[i]]
  modified <- dv_mean[[i]]
  
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  
  result <- c(result, tmp)
}




summary(qaptest(list(dv_original,
                     dv_mean[,,i]), gcor, g1 = 1, g2 = 2))$test
