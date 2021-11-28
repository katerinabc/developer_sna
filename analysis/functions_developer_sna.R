# functions
# 
# 

# for dichotomize network -------------------------------------------------

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


# function to get network data for one software version -------------------


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


# 2mode edgelist into matrix ----------------------------------------------

bipart_to_row_projection <- function(df_name_mode1, df_name_mode2){
  # code from https://solomonmessing.wordpress.com/2012/09/30/working-with-bipartiteaffiliation-network-data-in-r/
require('Matrix')
A <- spMatrix(nrow=length(unique(df_name_mode1)),
              ncol=length(unique(df_name_mode2)),
              i = as.numeric(factor(df_name_mode1)),
              j = as.numeric(factor(df_name_mode2)),
              x = rep(1, length(as.numeric(df_name_mode1))) 
              )
row.names(A) <- levels(factor(df_name_mode1))
colnames(A) <- levels(factor(df_name_mode2))

Arow <- tcrossprod(A)
return(list(A, Arow))

}


bipart_to_col_projection <- function(df_name_mode1, df_name_mode2){
  # code from https://solomonmessing.wordpress.com/2012/09/30/working-with-bipartiteaffiliation-network-data-in-r/
  require('Matrix')
  A <- spMatrix(nrow=length(unique(df_name_mode1)),
                ncol=length(unique(df_name_mode2)),
                i = as.numeric(factor(df_name_mode1)),
                j = as.numeric(factor(df_name_mode2)),
                x = rep(1, length(as.numeric(df_name_mode1))) 
  )
  row.names(A) <- levels(factor(df_name_mode1))
  colnames(A) <- levels(factor(df_name_mode2))
  
  Acol <- tcrossprod(t(A))
  return(list(A, Acol))
  
}


# vlookup -----------------------------------------------------------------

#Vlookup in R
#Version 0.3 November 12, 2013
#Return senesical results if return column is a factor
#Version 0.2 November 11, 2013
#Require first column of table to be numeric if range lookup is being done
#Change defaults to larger=FALSE
#Julin Maloof

# https://gist.github.com/jnmaloof/7367450
# 
vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}


