##' Get Children
##' 
##' This function takes a commodity tree and a vector of node ids and returns a 
##' vector of these nodes as well as all the descendants (or ancestors) of these
##' nodes.
##' 
##' @param commodityTree A data.table with parent and child node IDs 
##'   (corresponding to the IDs in nodes) which specify the commodity tree 
##'   structure.
##' @param parentColname The column name of commodityTree which contains the ID 
##'   of the parent node.
##' @param childColname The column name of commodityTree which contains the ID 
##'   of the child node.
##' @param topNodes Vector containing the node names/ids for which the children 
##'   are sought.
##' @param parentFlag Logical.  If TRUE (default) this function gets all 
##'   descendants.  If FALSE, it gets all ancestors.
##'   
##' @return A numeric vector containing topNodes as well as all descendants (or
##'   ancestors) of topNodes.
##' 
##' @export
##' 

getChildren = function(commodityTree, parentColname, childColname, topNodes,
                       parentFlag = TRUE){
  
  ## Data Quality Checks
  stopifnot(is(commodityTree, "data.table"))
  stopifnot(c(parentColname, childColname) %in% colnames(commodityTree))
  
  oldNodes = topNodes
  newNodes = topNodes
  while(length(newNodes) > 0){
    if(parentFlag){
      newNodes = commodityTree[get(parentColname) %in% oldNodes,
                               unique(get(childColname))]
    } else {
      newNodes = commodityTree[get(childColname) %in% oldNodes,
                               unique(get(parentColname))]
    }
    newNodes = setdiff(newNodes, oldNodes)
    oldNodes = c(oldNodes, newNodes)
  }
  oldNodes
}