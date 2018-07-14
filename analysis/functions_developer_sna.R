# functions

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

