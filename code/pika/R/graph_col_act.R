#  2021-03-07 09:59 
#  elihei  [<eheidari@student.ethz.ch>]
# /Volumes/Projects/scGCN/code/R/scGCNUtils/R/graph_col_act.R

#' A wrapper for `igraph::plot.igraph` for node activities (continuous color values)
#'
#' @description
#' Visualizes a graph and colors its nodes based on their values (e.g., activities).
#'
#' @param graph An igraph object.
#' @param values A numeric vector of the same length as the number of nodes indicating node values.
#' @param contrast A positive numeric indicating the contrast of node colors (the higher the value the more the contrast). default=1.
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

graph_col_act <- function(graph, values, contrast=1, lay, title){
    grp_range = c(min(values)^(contrast)/sum(values^(contrast)), max(values)^(contrast)/sum(values^(contrast)))
    grp_vals  = seq(grp_range[1],grp_range[2],length.out=9)
    grp_cols  = circlize::colorRamp2(grp_vals, viridis::viridis(9))
    igraph::V(graph)$color = grp_cols(values^(contrast)/sum(values^(contrast)))
    p = plot.igraph(graph,
        vertex.size = 5,
        layout = lay,
        vertex.frame.color = igraph::V(graph)$color,
        vertex.label = "",
        main=title)
    p
}
