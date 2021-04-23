#  2020-07-20 08:37 
#  elihei  [<eheidari@student.ethz.ch>]
# /Volumes/Projects/scGCN/code/R/scGCNUtils/R/subset_genes.R


#' Gene subset selection
#'
#' @description
#' Subsets genes to be used as the network nodes
#'
#' @param sce A SingleCellExperiment to select a subset of genes from. 
#' @param ... Any additional arguments.
#'
#' @section Additional arguments:
#' \describe{
#' \item{n}{An integer indicating the number of genes to be selected.}
#' \item{method}{selection based on transcription factors, highly variable genes, or randomly or a combination of these. (possible values = `c('tf', 'hvg', 'rand')`).}
#' }
#' 
#' @return The SingleCellExperiment object with the subset of genes selected.
#' @export
#'
#'
#'
#' @importFrom  tictoc toc tic.log
#' @import  tidyverse

subset_genes <- function(sce, ...){
    # set params
    params = list(...)
    if(is.list(params[[1]]))
        params = params[[1]]
    params %<>% set_params('subset_genes', 1)
    genes = rownames(sce)
    # subset on genes 
    if(params$n > length(genes)){ # n > #genes
        'n > #genes, sets n = #genes' %>% warning
        params$n = length(genes)
        metadata(sce)$params$subset_genes = params
        sce = sce[genes, ]
        # log
        toc(log=TRUE, quiet=TRUE)
        messagef(tic.log(format = TRUE)[[1]], verbose=params$verbose, log=params$log)   
        return(sce)
    }
    if('tf' %in% params$method){ # transcription factors
        tf = rownames(sce)[rowData(sce)$is_TF]
        if(params$n > length(tf)){ # n > #TFs
            'n > #TFs, sets n = #TFs' %>% warning
            params$n = length(tf)
            metadata(sce)$params$subset_genes = params
            sce = sce[tf, ]
            # log
            toc(log=TRUE, quiet=TRUE)
            messagef(tic.log(format = TRUE)[[1]], verbose=params$verbose, log=params$log)
            return(sce)
        }
        sce = sce[tf, ]
        genes = tf
    }
    if('hvg' %in% params$method){ # highly variable genes
        sce = sce[order(rowData(sce)$gene_var, decreasing=T),]
        genes = rownames(sce)
    }
    if('rand' %in% params$method){ # random genes
        genes = sample(genes, params$n, replace=F)
    }
    # add metadata fields
    metadata(sce)$params$subset_genes = params
    genes = genes[1:params$n]
    sce   = sce[genes, ]
    # log
    toc(log=TRUE, quiet=TRUE)
    tic_log = tic.log(format = TRUE)
    messagef(tic_log[[length(tic_log)]], verbose=params$verbose, log=params$log)
    sce
}