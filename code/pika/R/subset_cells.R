#  2020-07-19 20:39 
#  elihei  [<eheidari@student.ethz.ch>]
# /Volumes/Projects/scGCN/code/R/scGCNUtils/R/select_celltypes.R


#' Cell subset selection
#'
#' @description
#' Subsets cells from the SingleCellExperiment object.
#'
#' @param sce A SingleCellExperiment to select a subset of cells from. 
#' @param ... Any additional arguments.
#'
#' @section Additional arguments:
#' \describe{
#' \item{cell_types}{a string vector including a subset of cell types. Set `cell_types='all'` to select from all cell types.}
#' \item{min_num}{minimum number of cells (in the train dataset) for a cell type to be selected.}
#' \item{min_rel_prop}{minimum reletive proportion of number of cells (in the train dataset) for a cell type to be selected.}
#' \item{balanced}{a logical indicating if subsampling should be balanced (i.e., each cell type).}
#' }
#' 
#' @return The SingleCellExperiment object with the subset of cells selected.
#' @export
#'
#'
#'
#' @importFrom  tictoc toc tic.log
#' @import  tidyverse


subset_cells <- function(sce, ...) {
    # set params
    params = list(...)
    if(is.list(params[[1]]))
        params = params[[1]]
    params %<>% set_params('subset_cells', 1)
    # colData to as.data.table
    col_data = colData(sce) %>%
        as.data.table(keep.rownames=T) %>%
        setnames('rn', 'id') 
    # select cell types
    if(params$cell_types == 'all')
        params$cell_types = unique(col_data$cell_type)
    # subset to shared cell types
    params$cell_types = col_data_list %>%
        map(~.x$cell_type) %>%
        purrr::reduce(intersect) %>%
        intersect(params$cell_types)
    col_data %<>% .[cell_type %in% params$cell_types]
    # subset based on cut-offs
    type_freqs = col_data %>% .$cell_type %>% table
    type_props = type_freqs / max(type_freqs)
    params$cell_types = names(which(type_freqs > params$min_num & type_props > params$min_rel_prop))
    col_data %<>% .[cell_type %in% params$cell_types]
    # balance cell type abaundances
    if(params$balanced){
        type_freqs = col_data %>% .$cell_type %>% table
        min_freq   = min(type_freqs)
        col_data  %<>% .[, .SD[sample(1:dim(.SD)[1], min_freq)], by = 'cell_type']
    }
    sce = sce[, col_data$id]
    # compute gene vars for the subset sce
    rowData(sce)$gene_var = modelGeneVar(sce)
    # add metadata fields
    metadata(sce)$params$subset_cells = params
    # log
    toc(log=TRUE, quiet=TRUE)
    tic_log = tic.log(format = TRUE)
    messagef(tic_log[[length(tic_log)]], verbose=params$verbose, log=params$log)
    sce
}
