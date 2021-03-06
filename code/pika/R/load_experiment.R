# 2020-07-19 08:29 
# elihei  [<eheidari@student.ethz.ch>]
#/Volumes/Projects/scGCN/code/R/scGCNUtils/R/load_experiment.R

#' Load a single cell experiment
#'
#' @description
#' Loads an experiment and write it into a SingleCellExperiment object. Also preprocesses data with `scran`
#'
#' @param base_dir A string indicating the experiment's base directory.
#' @param tag A string indicating the experiment's tag. Should be name of a folder containing train and test folders each of which including `counts.txt`, `cells.txt`, `genes.txt`.
#' @param ... Any additional arguments.
#'
#' @details `counts.txt` contains the counts matrix, `cells.txt` contains cell ids, `genes.txt` contains gene ids.
#'
#' @author Elyas Heidari
#'
#' @section Additional arguments:
#' \describe{
#' \item{spec}{A string indicating the sapience. possible values = `c('cattle', 'chicken','chimpanzee', 'dog_domestic', 'frog_western clawed', 'human', 'mouse_laboratory','rat', 'zebrafish')`.}
#' \item{log}{A logical indicating whether to log the computation times.}
#' \item{verbose}{A logical indicating whether to print out the computation steps.}
#' }
#'
#' @return A SingleCellExperiment with:
#' \describe{
#' \item{rowData(.)$is_TF}{indicating if the gene corresponding to the row is a transcription factor.}
#' \item{rowData(.)$gene_var}{Modeled variance of the log-expression profiles for each gene, based on `scran::modelGeneVar`.}
#' }
#' @export
#'
#'
#'
#' @importFrom  SingleCellExperiment SingleCellExperiment
#' @import  tidyverse
#' @importFrom  tictoc toc tic.log
#' @importFrom  scran modelGeneVar
#' @importFrom  scuttle logNormCounts 
#' @importFrom  furrr future_map


load_experiment <- function(base_dir, tag, ...) {
    # set params
    params = list(...)
    if(is.list(params[[1]]))
        params = params[[1]]
    # initialization
    exp_dir    = file.path(base_dir, tag)
    sce_state  = 1
    log_tag    = NULL
    if(params$log){
        log_tag = paste(format(Sys.Date(), '%Y%m%d'), format(Sys.time(), "%X"), sep='_') %>%
            gsub(':', '', .) %>% 
            paste(tag, ., sep='_')
        sprintf('%s.log', log_tag) %>% file.create
        params$log = log_tag
    }
    params %<>% set_params('load_experiment', 1)
    # load a single assay
    load_assay <- function(assay_tag){
        # load data
        assay_dir  = file.path(base_dir, assay_tag)
        counts_mat = file.path(assay_dir, 'counts.txt')  %>% readMM
        cells      = file.path(assay_dir, 'cells.txt')   %>% fread %>% c %>% .[[1]]
        genes      = file.path(assay_dir, 'genes.txt')   %>% fread %>% c %>% .[[1]]
        colnames(counts_mat) = cells
        rownames(counts_mat) = genes
        # matrix + metadata --> SCE 
        colData    = file.path(assay_dir, 'colData.txt') %>% fread %>% DataFrame
        rownames(colData) = colData$id
        colData    %<>% .[,setdiff(colnames(colData), 'id')]
        stopifnot(dim(colData)[1] == dim(counts_mat)[2])
        colData$tag = assay_tag
        sce         = SingleCellExperiment(
                        assays=list(counts=counts_mat), 
                        colData=DataFrame(colData))
        rownames(sce) %<>% tolower
        # message
        sprintf('--      read %s with %d cells and %d genes', assay_tag, dim(sce)[2], dim(sce)[1]) %>%
            messagef(verbose=params$verbose, log=log_tag)
        sce 
    }
    # load the sce
    sce   = load_assay(tag)
    # add rowData
    sce   = logNormCounts(sce)
    # TODO: attach as internal data
    tfs   = sprintf(file.path(base_dir,'prior/homo_genes_%s.txt'), params$spec) %>%
        fread(header=F) %>% .$V1
    rowData(sce)$is_TF    = rownames(sce) %in% tfs
    rowData(sce)$gene_var = modelGeneVar(sce)
    # define metadata fields
    metadata(sce)$input  = list(train=list(), test=list())
    metadata(sce)$output = list(train=list(), test=list())
    metadata(sce)$vis    = list()
    metadata(sce)$tag    = tag
    metadata(sce)$params$load_experiment = params
    # message
    sprintf('--      return sce %s with %d cells and %d genes', tag, dim(sce)[2], dim(sce)[1]) %>%
        messagef(verbose=params$verbose, log=log_tag)
    # log
    toc(log=TRUE, quiet=TRUE)
    tic_log = tic.log(format = TRUE)
    messagef(tic_log[[length(tic_log)]], verbose=params$verbose, log=params$log)
    sce
}
