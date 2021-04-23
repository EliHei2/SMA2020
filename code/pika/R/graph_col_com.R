#  2021-03-07 09:43 
#  elihei  [<eheidari@student.ethz.ch>]
# /Volumes/Projects/scGCN/code/R/scGCNUtils/R/graph_col_com.R

#' A wrapper for `igraph::plot.igraph` for node communities (discrete color values)
#'
#' @description
#' Visualizes a graph and colors its nodes based on the communities.
#'
#' @param graph An igraph object.
#' @param communities A character vector of the same length as the number of nodes indicating node communities.
#' @param labels A character vector of the same length as the number of nodes indicating node labels. default = `''`.
#' @param lay An igraph valid network layout. default = 'NULL'.
#' @param title A string indicating the title of the graph plot.
#'
#' @author Elyas Heidari
#'
#'
#' @return An igraph plot.
#' @export
#'
#' @importFrom  igraph cluster_louvain plot.igraph
#' 
graph_col_com <- function(graph, communities, labels='', lay=NULL, title=NULL){
    nice_cols   = c(
    "#FF7F00", "#FDB462",  "#BEAED4", "#00bfff",
    "#7BAFDE", "#E78AC3", "#B2DF8A",  "#8DD3C7",
    "#B17BA6", "#d4b7b7",  "#33A02C", "#808000", 
    "#55A1B1", "#A6761D", "#E6AB02", "#aeae5c")
    if(is.null(lay))
        lay = layout_nicely(graph)
    igraph::V(graph)$color = nice_cols[communities]
    v = igraph::V(graph)
    p = plot.igraph(
        graph,
        vertex.size = 6,
        layout = lay,
        vertex.label = labels,
        vertex.frame.color = igraph::V(graph)$color,
        vertex.label.family = 'Helvetica',
        vertex.label.dist = 0,
        vertex.label.cex = .25,
        vertex.label.font = .5,
        vertex.label.color = '#585d59',
        main=title)
    p
}

