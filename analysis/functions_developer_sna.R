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


