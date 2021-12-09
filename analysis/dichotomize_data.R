rm(list=ls())

# author: Katerina Bohle Carbonell
# project owner: Mahdi
# 
# created: 04 November 2021
# updated: 30 November 2021
# 
# 
# debugging

# TODO edge count, density, and triad count for each version for cutoff = 50th percentile

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
# 
# 
# DICHOTOMIZE

# packages ----------------------------------------------------------------

library(readr)
library(tidyverse)
library(statnet)



# create networks ---------------------------------------------------------

source("creating_networks.R", echo = F)
all_developers <- as.vector(dv %v% 'vertex.names')



# functions ---------------------------------------------------------------


source("functions_developer_sna.R", echo = F)

# calculate thresholds ---------------------------------------------------------

dv_thresholds <- 
  dv_master %>% 
  group_by(ver) %>% 
  summarize(mean = mean(n),
            median = median(n),
            threequarters = quantile(n)[4],
            sd = sd(n),
            mean_std = (mean(n) + sd(n)), 
            half = quantile(n)[3],
            firstquarter = quantile(n)[2])

dv_thresholds %>% print(n=Inf)
dv_thresholds %>% write_csv("thresholds for dv.csv")

# prep dichotomize networks ----------------------------------------------------

# dichotomize edgelist
dv_modified <- 
  dv_master %>% 
  group_by(ver) %>% 
  mutate(tie_mean = if_else(n > mean(n), 1, 0),
         tie_median = if_else(n > median(n), 1, 0),
         tie_three4th = if_else(n > quantile(n)[4], 1, 0),
         tie_mean_std = if_else(n > (mean(n) + sd(n)), 1, 0),
         tie_median2 = if_else(n  >= median(n), 1, 0),
         tie_half = if_else(n > quantile(n)[3], 1, 0),
         tie_1st = if_else(n > quantile(n)[2], 1, 0)
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

#---

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

#---
  
dv_median2 <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_median2", version = i)
  
  dv_median2[[i]] <- tmp
  
}
# pull out centrality and betweennes and store in a dataset
df_median2 <- builddf(networklist = dv_median2)

#---
#

dv_half <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_half", version = i)
  
  dv_half[[i]] <- tmp
  
}
# pull out centrality and betweennes and store in a dataset
df_half <- builddf(networklist = dv_half)

#----

dv_first <- list()
for (i in 1: max(dv_master$ver)){
  #print(i)
  tmp <- cutnetworks("tie_1st", version = i)
  
  dv_first[[i]] <- tmp
  
}
# pull out centrality and betweennes and store in a dataset
df_first <- builddf(networklist = dv_first)


# creating df with degree for all datasets --------------------------------

# create vector for column names
colnames_corr <- paste(rep(c("deg", "deg", "bet", "bet"), 11), 
                       rep(c("est", "pval"), 22), 
                       sort(rep(seq(1:11),4)),
                       sep="_")

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
  
  corr_mean <- c(corr_mean, tmpcor$estimate, tmpcor$p.value)

}
names(corr_mean) <- colnames_corr
corr_mean

# ---

# dichotomization = median
corr_med <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_med[,i])
  
  tmpcor <- cor.test(x, y)
  #tmpcor <- cor.test(x, y)$p.value
  corr_med <- c(corr_med, tmpcor$estimate, tmpcor$p.value)
  
}
names(corr_med) <- colnames_corr
corr_med

# ---
# 
# 
# dichotomization = 75th percentile
corr_three4 <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_three4[,i])
  
  tmpcor <- cor.test(x, y)
  #tmpcor <- cor.test(x, y)$p.value
  corr_three4 <- c(corr_three4, tmpcor$estimate, tmpcor$p.value)
  
}
names(corr_three4) <- colnames_corr
corr_three4

# ---

# dichotomize above median
corr_med2 <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_median2[,i])
  
  tmpcor <- cor.test(x, y)
  #tmpcor <- cor.test(x, y)$p.value
  corr_med2 <- c(corr_med2, tmpcor$estimate, tmpcor$p.value)
  
}
names(corr_med2) <- colnames_corr
corr_med2

# ---

#dichotomize 50th percentile

corr_half <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_half[,i])
  
  tmpcor <- cor.test(x, y)
  #tmpcor <- cor.test(x, y)$p.value
  corr_half <- c(corr_half, tmpcor$estimate, tmpcor$p.value)
  
}
names(corr_half) <- colnames_corr
corr_half

# ---

# dichotomize 25th percentile

corr_first <- NULL
for (i in 2: ncol(df_original)){
  
  x = pull(df_original[,i])
  y = pull(df_first[,i])
  
  tmpcor <- cor.test(x, y)
  #tmpcor <- cor.test(x, y)$p.value
  corr_first <- c(corr_first, tmpcor$estimate, tmpcor$p.value)
  
}
names(corr_first) <- colnames_corr
corr_first

# ---

# put all the correlations into one dataset

df_correlations <- tibble(mean = corr_mean,
                          med = corr_med,
                          three4 = corr_three4,
                          med2 = corr_med2,
                          half = corr_half,
                          first = corr_first)

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

df_cor_lg <- df_correlations %>% pivot_longer(cols = c('mean', 'med', 'three4', 'med2', 'half', 'first'),
                                              names_to = c('cutoff'),
                                              values_to = c('value'))

# df_correlations %>% pivot_longer(cols = index,
#                                  names_to=type,
#                                  names_pattern = '(.*)(\\d)')
#                                  #names_pattern = '(\\w+)_(est)_(\\d)',
#                                  #values_to = "value"
#                                  )

df_cor_lg <- df_cor_lg %>% 
  #mutate(degree = grepl(df_cor_lg$index, "deg"))
  mutate(type = strsplit(index, "_")) %>%
  unnest(cols = c(type)) %>% 
  mutate(version = str_extract(index, "\\d{1,2}")) %>%
  #filter(type %in% c("est", "pval")) %>%
  mutate(metric = substr(index, 1, 3)) %>%
  filter(type %in% c('est', 'pval'))



estimates <- df_cor_lg %>%
  mutate(estimate = value) %>%
  filter(type == "est") %>%
  select(-index, -value, -type)

pvalue <-   df_cor_lg %>%
  mutate(pvalue = value) %>%
  filter(type == "pval") %>%
  select(-index, -value, -type) %>%
  add_column(sig = if_else(pvalue$pvalue < 0.05, 1, 0))

df_cor2 <- full_join(estimates, pvalue)

# df_cor2 <- df_cor_lg %>% 
#   select(-index) %>% 
#   pivot_wider(id_cols = cutoff,
#               names_from = type,
#               values_from=value)
# 
#   cols = c('mean', 'med', 'three4', 'med2', 'half', 'first'),
#                                               names_to = c('cutoff'),
#                                               values_to = c('value'))
#                                               

ggplot(df_cor2, aes(x = version, 
                    y = estimate, 
                    alpha = sig,
                    color = cutoff)) + 
  geom_point(size = 2) + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between dichotomized and valued networks",
       x = 'version number and SNA metric', y = 'correlation',
       caption = "Values that are not significant are not shown"
       ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggsave('scatterplot_correlations_network_metrics.png', height = 9.72, width = 11.2, units = "in")


ggplot(df_cor2, 
       aes(x = cutoff, 
           y = estimate, 
           fill = cutoff,
           )) + 
  geom_boxplot() +
  geom_point(aes(x = cutoff, y = pvalue, color = as.factor(sig))) + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between dichotomized and valued network metrics",
       subtitle = "significant pvalues (> 0.05) shown as blue dots",
       x = 'Network type', y = 'correlation / pvalue'
       )
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave('boxplot_correlations_network_metrics.png', height = 9.72, width = 11.2, units = "in")

# testing correlation between networks ------------------------------------

qapresult <- NULL
for (i in 2:length(dv_original)){
  original <- dv_original[[i]]
  
  # calcualte qap for mean
  modified <- dv_mean[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  # calculate qap for median
  modified <- dv_median[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  # caculate qap for 75th percentile
  modified <- dv_three4th[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  # caculate qap for above median
  modified <- dv_median2[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  # caculate qap for 50th percentile
  modified <- dv_half[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  # caculate qap for 25th percentile
  modified <- dv_first[[i]]
  tmp <- summary(qaptest(list(original, modified), gcor, g1=1, g2=2))$test
  qapresult <- c(qapresult, tmp)
  
  
  
}

qapresult <- as.data.frame(qapresult)
# rownames(result) <- paste(rep.int(c("mean", "median", "three4th"), times = length(seq(2:11))), 
#                           "version", 
#                           rep.int(seq(from = 2, to =11, by= 1), times = rep(3, length(c(2:11)))))

qapresult$cutoff <- paste(rep.int(c("mean", "median", "three4th", 'med2', 'half', 'first'), times = length(seq(2:11))))
qapresult$version <- rep.int(seq(from = 2, to =11, by= 1), times = rep(3, length(c(2:11))))


ggplot(qapresult, aes(x = version, y = qapresult, color = cutoff)) + 
  geom_point() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between valued and dichotomized network",
       x = 'version number', y = 'correlation') + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggsave('scatterplot_correlations_between_network.png')


ggplot(qapresult, aes(x = cutoff, y = qapresult, color = cutoff)) + 
  geom_boxplot() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation between dichotomized and valued networks",
       x = 'Cut-off values for networks', y = 'QAP correlation') + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave('boxplot_correlations_between_network.png', width = 10, height = 6.82, unit = "in")

# testing correlations within a network -----------------------------------

qapwithinnet <- NULL
for (i in 1:10){
  # calculate qap original network t and t+1
  tmp1 <- dv_original[[i]]
  tmp2 <- dv_original[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)

  # calculate qap mean network t and t+1
  tmp1 <- dv_mean[[i]]
  tmp2 <- dv_mean[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
  # calculate qap median network t and t+1
  tmp1 <- dv_median[[i]]
  tmp2 <- dv_median[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
  # calculate qap 75th percentile network t and t+1
  tmp1 <- dv_three4th[[i]]
  tmp2 <- dv_three4th[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
  # calculate qap above median t and t+1
  tmp1 <- dv_median2[[i]]
  tmp2 <- dv_median2[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
  # calculate qap 50th percentile network t and t+1
  tmp1 <- dv_half[[i]]
  tmp2 <- dv_half[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
  # calculate qap 25th percentile network t and t+1
  tmp1 <- dv_first[[i]]
  tmp2 <- dv_first[[i+1]]
  res <- summary(qaptest(list(tmp1, tmp2), gcor, g1=1, g2=2))$test
  qapwithinnet <- c(qapwithinnet, res)
  
}

qapwithinnet <- as.data.frame(qapwithinnet)
rownames(qapwithinnet) <- paste(c("original", "mean", "median", "three4th", "above_median", "half", "first"),
                                rep(c('ver1-2', 'ver2-3', 'ver3-4', 'ver4-5', 'ver5-6',
                            'ver6-7', 'ver7-8', 'ver8-9', 'ver9-10', 'ver10-11'), 
                            times = rep(7, 10)), 
                            sep="_")

qapwithinnet$cutoff <- paste(rep.int(c("original","mean", "median", "three4th", "above_median", "half", "first"),
                                     times = length(seq(2:11))))
qapwithinnet$version <- rep(c('ver1-2', 'ver2-3', 'ver3-4', 'ver4-5', 'ver5-6',
                              'ver6-7', 'ver7-8', 'ver8-9', 'ver9-10', 'ver10-11'), 
                            times = rep(7, 10))

ggplot(qapwithinnet, aes(x = cutoff, y = qapwithinnet, color = cutoff)) + 
  geom_point() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation within a network",
       caption = "Correlates ver1 with ver2, ver2 with ver 3 for original network and the 3 types of dichotomzied networks",
       x = 'Network type', y = 'correlation') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggsave('scatterplot_correlations_within_network.png', width = 10, height = 6.82, unit = "in")


ggplot(qapwithinnet, aes(x = cutoff, y = qapwithinnet, color = cutoff)) + 
  geom_boxplot() + 
  scale_color_brewer(type = 'qual', palette = 6) + 
  theme_minimal() + 
  labs(title = "Correlation within a networks",
       caption = "Correlates ver1 with ver2, ver2 with ver 3 for original network and the 3 types of dichotomzied networks",
       x = 'Cutoff values for networks', y = 'correlation') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave('boxplot_correlations_within_network.png', width = 10, height = 6.82, unit = "in")



# number of edges per version: first ---------------------------------------------

dv_first

nd_first <- networkDynamic(network.list = dv_first,
                           #onsets = seq(0, 10, 1),
                           #termini = seq(1, 11, 1),
                           start = 1, 
                           end = 11)
nd_first
round(tSnaStats(nd_first, snafun = 'gden'),2)
round(tSnaStats(nd_first, snafun = 'gtrans' ),2)
#round(tSnaStats(nd_first, snafun = 'degree' ),2)
tEdgeDensity(nd_first, mode = 'duration')
# 17% of edges are active at any point of time
tEdgeFormation(nd_first)
tEdgeDissolution(nd_first)
tiedDuration(nd_first, mode = 'duration') # why are some 0? 
#nd_first_compressed <- timeProjectedNetwork(nd_first)
#plot(timeProjectedNetwork(nd_first), label = 'vertex.names')

ndtv::filmstrip(nd_first,displaylabels=FALSE, 
                slice.par = list(start = 1, 
                                 end = 11,
                                 interval = 1,
                                 aggregate.dur = 1,
                                 rule = 'latest'))

# number of edges per version: half ---------------------------------------------

dv_half

nd_half <- networkDynamic(network.list = dv_half,
                           #onsets = seq(0, 10, 1),
                           #termini = seq(1, 11, 1),
                           start = 1, 
                           end = 11)
nd_half
round(tSnaStats(nd_half, snafun = 'gden'),2)
round(tSnaStats(nd_half, snafun = 'gtrans' ),2)
#round(tSnaStats(nd_half, snafun = 'degree' ),2)
tEdgeDensity(nd_half, mode = 'duration')
# 17% of edges are active at any point of time
tEdgeFormation(nd_half)
tEdgeDissolution(nd_half)
tiedDuration(nd_half, mode = 'duration') # why are some 0? 
#nd_first_compressed <- timeProjectedNetwork(nd_half)
#plot(timeProjectedNetwork(nd_half), label = 'vertex.names')

ndtv::filmstrip(nd_half,displaylabels=FALSE, 
                slice.par = list(start = 1, 
                                 end = 11,
                                 interval = 1,
                                 aggregate.dur = 1,
                                 rule = 'latest'))
