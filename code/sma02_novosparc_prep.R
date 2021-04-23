#  2021-04-16 10:10 
#  elihei  [<eheidari@student.ethz.ch>]
# /Volumes/Projects/SMA2020/code/sma02_novosparc_prep.R

### setup
#### libs
suppressMessages(source('code/scGCN/dep.R')) 
source('code/sma00_utils.R')
source('code/sma01_eda.R')
#### input dirs
exprs_f    = 'data/exprs.Rds'
metadata_f = 'data/metadata.Rds'
umap_f     = 'data/E8.5_joint_UMAP_df.Rds'
nngraph_f  = 'data/E8.5_neighbourGraph_1.3.Rds'
#### output dirs
save_dir   = 'data/sma02_novosparc'
dir.create(save_dir)
expr_mat_f  = file.path(save_dir, 'mtx_%s.txt')
locations_f = file.path(save_dir, 'locs_%s.txt')

### preproc
nngraph  = nngraph_f %>% readRDS 
cells    = names(V(nngraph))
exprs    = exprs_f %>% readRDS %>% 
    as.matrix %>% .[, cells]  %>% t 
metadata = metadata_f %>% readRDS %>% as.data.table %>%
    setnames('uniqueID', 'cell') %>%
    setkey('cell') %>%
    .[cells] %>%
    .[, embryo := paste(embryo, z, sep='_')]
embryos       = unique(metadata$embryo)
cells_list   = embryos %>%
    map(~unlist(metadata[embryo == .x, 'cell'])) %>%
    setNames(embryos)
nngraph_list = cells_list %>% 
    map(~induced.subgraph(nngraph, .x))
exprs_list   = cells_list %>% 
    map(~exprs[.x, ]) %>%
    map(t) %>%
    map(as.data.table, keep.rownames=T) %>%
    map(~setnames(.x, 'rn', ''))
metadata %>% setkey('cell')
locs_list    = cells_list %>%
    map(~metadata[.x, .(x_coord=x_global_affine, y_coord=y_global_affine)])
nncomm_list  = nngraph_list %>% 
    map(nngraph_comm) %>%
    setNames(NULL) %>%
    unlist
metadata$nncomm = paste(metadata$embryo, nncomm_list[metadata$cell], sep='_') 
meta_list    = cells_list %>%
    map(~metadata[cell %in% .x, ])
    
### save output
names(cells_list) %>% map(~fwrite(exprs_list[[.x]], sprintf(expr_mat_f, .x)), row.names = TRUE)
names(cells_list) %>% map(~fwrite(locs_list[[.x]], sprintf(locations_f, .x)), row.names = TRUE)