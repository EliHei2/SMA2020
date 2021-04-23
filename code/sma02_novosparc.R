

run_novosparc <- function(
    input_dir, output_dir, tag, atlas_locs_f, atlas_mtx_f, expr_mtx_f,
    ncells=1000, nns=5, nnt=5, alpha=0.8, epsilon=0.005, seed=1996, overwrite=FALSE){

    novosparc_c = sprintf('python3 code/sma02_novosparc_run.py 
    --data_dir %s 
    --out_dir %s 
    --tag %s 
    --atlas_locs_f %s
    --atlas_mtx_f %s 
    --expr_mtx_f %s 
    --ncells %s 
    --nns %s 
    --nnt %s 
    --alpha %s 
    --epsilon %s 
    --seed %s',
    input_dir,
    output_dir, 
    tag,
    atlas_locs_f,
    atlas_mtx_f,
    expr_mtx_f,
    ncells, 
    nns, 
    nnt,
    alpha,
    epsilon,
    seed) %>% 
        str_replace_all('\\n', '')
    if(overwrite || !dir.exists(file.path(output_dir, tag)))
        system(novosparc_c)
}

ns2sce <- function(dir, sce_origin, sce_ref, tag){
    print(tag)
    obs = fread(file.path(dir, 'obs.csv')) %>%
        .[,-1] %>%
        t
    obsm = fread(file.path(dir, 'obsm.csv')) %>%
        as.matrix
    pred_locs = as.matrix(obs) %*% obsm
    cells = rownames(pred_locs)
    nngraph_pred = cccd::nng(pred_locs, k=3)
    V(nngraph_pred)$name = cells
    sce_origin = sce_origin[, cells]
    nngraph_real = cccd::nng(data.table(x=sce_origin$x_coord, y=sce_origin$y_coord), k=3)
    V(nngraph_real)$name = cells
    # adj_pred = as_adj(nngraph_pred)
    # adj_real = as_adj(nngraph_real)
    # d = NetworkDistance::nd.dsd(list(adj_real,adj_pred), out.dist = TRUE, type = 'NLap')
    colData(sce_origin)$nncomm_pred = nncomm_pred = nngraph_comm(nngraph_pred)
    colData(sce_origin)$nncomm_real = nncomm_real = nngraph_comm(nngraph_real)
    colData(sce_origin)$x_pred = pred_locs[, 1]
    colData(sce_origin)$y_pred = pred_locs[, 2]
    metadata(sce_origin)$nngraph_pred = nngraph_pred
    metadata(sce_origin)$nngraph_real = nngraph_real
    metadata(sce_origin)$ari = pdfCluster::adj.rand.index(nncomm_real, nncomm_pred)
    # metadata(sce_origin)$dist = d$D[1,2] 

    gene_scores = fread(file.path(dir, 'gene_scores.csv')) %>%
        .[, .(GENE = genes, mI)] %>%
        DataFrame
    rownames(gene_scores) = gene_scores$GENE
    rowData(sce_origin)$GENE = rownames(sce_origin)
    rowData(sce_origin)$mI   = NA
    rowData(sce_origin)[gene_scores$GENE,]$mI = gene_scores$mI
    

    gene_names = fread(file.path(dir, 'var.csv')) %>%
        .[-1,] %>%
        .$V1
    X = fread(file.path(dir, 'X.csv')) %>%
        as.matrix %>%
        t %>%
        apply(2, function(x) (x)/(mean(x))) 
    rownames(X) = gene_names
    colnames(X) = colnames(sce_ref)
    assays(sce_ref)[[paste0('counts_', tag)]] <<- X
    diff_mtx = (assays(sce_ref)[['counts_real']] - X) ^ 2
    rowData(sce_origin)$MSE[rownames(sce_ref)]  = rowMeans(diff_mtx) ^ (1/2)
    colData(sce_ref)[[paste0(tag, '_MSE')]] <<- colMeans(diff_mtx) ^ (1/2)
    metadata(sce_origin)$MSE = metadata(sce_ref)[[paste0(tag, '_MSE')]] <<- mean(diff_mtx) ^ (1/2)
    sce_origin
}

