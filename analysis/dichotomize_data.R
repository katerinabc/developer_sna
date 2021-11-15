rm(list=ls())

# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 04 November 2021
# updated: 15 November 2021
# 
# 
# debugging
# degree values in dv_original are different to degree values in dv_mean, 
# dv_median
# 
# degree values in dv_mean are the same than in dv_median
# 
# 
# TODO 

# Explanation of workflow using "dichotomize using the mean"
# as the basis
# 
# 1. calculate edges
# for each version calculate the mean ver_mean
# for each version if the edge value is > ver_mean then edge = 1 unless 0
# 
# 2. create networks
# for each version create the network using the edgelist created in step 1
# look for which developer were missing
# add the missing developers
# calculate degree centrality, and betweenness
# 
# 3. create a dataset for each dichotomization with rows as developers and 
# columns as SNA metrics
# 
# 4. calculate correlations

# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)



# create networks ---------------------------------------------------------

source("creating_networks.R", echo = F)
all_developers <- as.vector(dv %v% 'vertex.names')



# functions ---------------------------------------------------------------

#adjmat_to_edgelist.matrix <- function(graph, undirected, keep.isolates) {
  # as.integer(factor(diffnet$meta$ids[fakeDynEdgelist[,1]], diffnet$meta$ids))
  out <- adjmat_to_edgelist_cpp(methods::as(graph, "dgCMatrix"), undirected)
  
  # If keep isolates
  if (keep.isolates) {
    N <- 1:nvertices(graph)
    test <- which(!(N %in% unlist(out[,1:2])))
    
    # If there are isolates
    if (length(test)) out <- rbind(out, cbind(test, 0, 0))
  }
  
  return(out)
} # not needed

cutnetworks <- function(thresholdname, version) {
  # this function dichotomizes the networks 
  
  # set columns we need to use
  colnumbers <- match(c("author", "folder_owner", "ver", thresholdname), 
                      colnames(dv_modified))
  
  # subset the main dataset
  tmp1 <- dv_modified[dv_modified$ver == version, colnumbers]
  # remove all edges with no tie (cell value = 0)
  tmp1 <- tmp1[!tmp1[,4] == 0, ]
  
  # create network
  g <- igraph::graph_from_data_frame(tmp1[,-3], directed=FALSE) # here something goes wrong
  
  # who is part of the network (existing_dev) and who is missing (missing_dev)
  existing_dev <- as.vector(igraph::V(g)$name) 
  missing_dev <- all_developers[-match(as.vector(igraph::V(g)$name), dv %v% 'vertex.names')]
  # add the missing developers
  g <- igraph::add.vertices(g, 
                            nv = length(missing_dev),
                            attr = list(name = missing_dev))
  
  # sort developers alphabetically --> this step is not working
  # problem: how to sort a matrix alphabetically --> check standford tutorial
  # 
  # transform network into matrix
  g <- igraph::as_adjacency_matrix(g) # this is not sorted alphabetically
  
  # transform matrix into edgelist. It keeps isolates, but removes names
  #el <- netdiffuseR::adjmat_to_edgelist(g, undirected=FALSE) 
  
  mat <- as.matrix(g)
  #g <- igraph::graph.adjacency(mat)
  #el <- igraph::as_data_frame(g, what = "both")

  net <- network(mat, directed=F)
  
  # el <- as.edgelist(net, output = "tibble", vnames = "vertex.names") #this removes isolates
  # as.matrix.network.edgelist(net, as.sna.edgelist=T)
  # el <- el[sort]
  # net <- network(el)
  
  # calculate network metrics
  net %v% 'degree' <- degree(net, gmode = "graph", cmode="freeman", rescale = TRUE)
  net %v% 'betweenness' <- betweenness(net, gmod = "graph", cmode = "undirected", rescale = TRUE)
  
  # return the network
  return(net)
}


builddf <- function(networklist, df = NULL){
  # create a dataset with rows as person and network metrics as columns
  
  # setup the dataframe with information from version 1
  df <- tibble(person   = networklist[[1]] %v% 'vertex.names', 
               mean_deg = networklist[[1]] %v% 'degree',
               mean_btw = networklist[[1]] %v% 'betweenness')
  colnames(df) <- c("person", 
                     paste(colnames(df)[[2]], 1, sep="_"),
                     paste(colnames(df)[[3]], 1, sep="_")
  )
  
  for (i in 2:length(networklist)){
    
    # loop through all versions and save the data
    tmp <- networklist[[i]]
    tib <- tibble(person   = tmp %v% 'vertex.names', 
                  mean_deg = tmp %v% 'degree',
                  mean_btw = tmp %v% 'betweenness')
    colnames(tib) <- c("person", 
                       paste(colnames(tib)[[2]], i, sep="_"),
                       paste(colnames(tib)[[3]], i, sep="_")
    )
    df <- df %>% full_join(tib, by=c("person"= "person")) 
  }
  return(df)
}

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


# original valued network -------------------------------------------------



dv_original <- NULL
for (i in 1: max(dv_modified$ver)) {
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
  

  #net <- as.network(add.isolates(net, length(missing_dev)), matrix.type = 'edgelist')
  net %v% 'vertex.names' <- c(existing_dev, missing_dev)
  net %v% 'degree' <- degree(net, gmode = "graph", cmode="freeman", rescale = TRUE)
  net %v% 'betweenness' <- betweenness(net, gmod = "graph", cmode = "undirected", rescale = TRUE)
  
  dv_original[[i]] <- net
}

df_original <- builddf(dv_original)



# dichotomize networks --------------------------------------------------

#test <- cutnetworks("tie_mean", version = 1)

# create the networks with dichotomization = mean
dv_mean <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_mean", version = i)
  
  dv_mean[[i]] <- tmp
  
}
dv_mean

# pull out centrality and betweennes and store in a dataset
df_mean <- builddf(networklist = dv_mean) # error in this code

#lapply(dv_mean, function(x) plot(x)) # it works
#lapply(dv_mean, function(x) plot(as.vector(x %v% 'degree')))

#colors <- RColorBrewer::brewer.pal(11, name = "Set3")
#ggplot(data.frame(value=dv_mean[[1]] %v% 'degree', person = 1:21), aes(x = person, y = value)) + geom_point()



# create the networks with dichotomization = median
 
dv_median <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_median", version = i)
  
  dv_median[[i]] <- tmp
  
}
# pull out centrality and betweennes and store in a dataset
df_med <- builddf(networklist = dv_median)


# create the networks with dichotomization = 75th percentile
#tie_three4th

dv_three4th <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_three4th", version = i)
  
  dv_three4th[[i]] <- tmp
  
}
# pull out centrality and betweennes and store in a dataset
df_three4 <- builddf(networklist = dv_three4th)


# creating df with degree for all datasets --------------------------------

# create vector for column names
colnames_corr <- paste("ver", 
                       sort(rep(seq(1:11),2)),
                       rep(c("deg", "bet"), 11), sep="")

# calculate the correlations
# 
# dichotomization = mean
corr_mean <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  #print(x)
  y = pull(df_mean[,i])
  y = ifelse(is.nan(y), 0, y)

  #print(y)
  
  tmpcor <- cor.test(x, y)
  #summary(tmpcor)
  
  corr_mean <- c(corr_mean, tmpcor$estimate)

}
names(corr_mean) <- colnames_corr
corr_mean


# dichotomization = median
corr_med <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_med[,i])
  
  tmpcor <- cor.test(x, y)$estimate
  #tmpcor <- cor.test(x, y)$p.value
  corr_med <- c(corr_med, tmpcor)
  
}
names(corr_med) <- colnames_corr
corr_med

# dichotomization = 75th percentile
corr_three4 <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_three4[,i])
  
  tmpcor <- cor.test(x, y)$estimate
  #tmpcor <- cor.test(x, y)$p.value
  corr_three4 <- c(corr_three4, tmpcor)
  
}
names(corr_three4) <- colnames_corr
corr_three4

# put all the correlations into one dataset

df_correlations <- tibble(mean = corr_mean,
                          med = corr_med,
                          three4 = corr_three4)

df_correlations[, 'index'] <- colnames_corr

write_csv(df_correlations, "correlations btw networks based on degree and betweenness.csv")

degindex <- grep('deg', df_correlations$index)
btwindex <- grep('bet', df_correlations$index)

# range of correlations for cutoff = mean and sne metric = degree
min(df_correlations[degindex, 1], na.rm=T) #-0.177, excl. NA
max(df_correlations[degindex, 1], na.rm=T) # 0.1461214

# range of correlations for cutoff = mean and sne metric = betweenness
min(df_correlations[btwindex, 1], na.rm=T) #-0.1112168, excl. NA
max(df_correlations[btwindex, 1], na.rm=T) # 0.4248222

# range of correlations for cutoff = median and sne metric = degree
min(df_correlations[degindex, 2], na.rm=T) # 0.181
max(df_correlations[degindex, 2], na.rm=T) # 0.855

# range of correlations for cutoff = median and sne metric = betweenness
min(df_correlations[btwindex, 2], na.rm=T) # 0.141
max(df_correlations[btwindex, 2], na.rm=T) # 0.99

# range of correlations for cutoff = 75th percentile and sne metric = degree
min(df_correlations[degindex, 3], na.rm=T) # -0.07
max(df_correlations[degindex, 3], na.rm=T) # 0.71

# range of correlations for cutoff = 75th percentile and sne metric = betweenness
min(df_correlations[btwindex, 3], na.rm=T) # -0.10
max(df_correlations[btwindex, 3], na.rm=T) # 0.32

df_cor_lg <- df_correlations %>% pivot_longer(cols = c('mean', 'med', 'three4'),
                                              names_to = 'cutoff',
                                              values_to ='value')

ggplot(df_cor_lg, aes(x = index, y = value, color = cutoff)) + 
  geom_point() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between dichotomized and valued networks",
       x = 'version number and SNA metric', y = 'correlation') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggsave('scatterplot_correlations_network_metrics.png')


ggplot(df_cor_lg[!is.na(df_cor_lg$value),], 
       aes(x = cutoff, y = value, color = cutoff)) + 
  geom_boxplot() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between dichotomized and valued networks",
       x = 'version number and SNA metric', y = 'correlation') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave('boxplot_correlations_network_metrics.png')

# testing correlation between networks ------------------------------------

result <- NULL
for (i in 2:length(dv_original)){
  original <- dv_original[[i]]
  
  # calcualte qap for mean
  modified <- dv_mean[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  result <- c(result, tmp)
  
  # calculate qap for median
  modified <- dv_median[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  result <- c(result, tmp)
  
  # caculate qap for 75th percentile
  modified <- dv_three4th[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  result <- c(result, tmp)
  
}

result <- as.data.frame(result)
# rownames(result) <- paste(rep.int(c("mean", "median", "three4th"), times = length(seq(2:11))), 
#                           "version", 
#                           rep.int(seq(from = 2, to =11, by= 1), times = rep(3, length(c(2:11)))))

result$cutoff <- paste(rep.int(c("mean", "median", "three4th"), times = length(seq(2:11))))
result$version <- rep.int(seq(from = 2, to =11, by= 1), times = rep(3, length(c(2:11))))
