# 2020-07-23 17:59 
# elihei  [<eheidari@student.ethz.ch>]
#/Volumes/Projects/scGCN/code/R/scGCNUtils/R/compare_networks.R


#' Compare two networks
#'
#' @description
#' Compares two networks either globally (node clusters) or locally (exact shared edges).
#'
#' @param ref Either a numeric or character vector, if `method = 'global'`, or an adjacency matrix, if `method = 'local'`.
#' @param comp Either a numeric or character vector, if `method = 'global'`, or an adjacency matrix, if `method = 'local'`. 
#' Should be of the same size as `ref`.
#' @param ... Any additional arguments.
#'
#'
#' @author Elyas Heidari
#'
#' @section Additional arguments:
#' \describe{
#' \item{log}{A logical indicating whether to log the computation times.}
#' \item{verbose}{A logical indicating whether to print out the computation steps.}
#' }
#'
#' @return A SingleCellExperiment object with visualized comparison of graphs (`$comparegraphs$vis`) and a coherency score (`$comparegraphs$com_score`).
#' @export
#'
#'
#'
#' @importFrom  mclust adjustedRandIndex
#' @import  tidyverse
#' @importFrom  tictoc toc tic.log


compare_networks <- function(ref, comp, method=c('global', 'local'), ...){
    # TODO: message
    # set params
    params = list(...)
    if(is.list(params[[1]]))
        params = params[[1]]
    params %<>% set_params('compare_networks', 1)
    stofifnot('incompatible lengths'= length(ref) != length(comp))
    if(method == 'global')
        score = adjustedRandIndex(ref, comp)
    if(method == 'local')
        score = (ref * comp) %>% sum %>% `/`(dim(ref)[1] * dim(ref)[2])
    # log
    toc(log=TRUE, quiet=TRUE)
    tic_log = tic.log(format = TRUE)
    messagef(tic_log[[length(tic_log)]], verbose=params$verbose, log=params$log)
    score
}