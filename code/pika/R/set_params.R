# 2020-07-23 08:59 
# elihei  [<eheidari@student.ethz.ch>]
#/Volumes/Projects/scGCN/code/R/scGCNUtils/R/set_params.R




set_params <- function(params, name, level){
    if(is.null(params$verbose))
        params$verbose = TRUE
    prefix = ifelse(level == 1, '+       ', '--      ')
    sprintf('%srun %s', prefix, name) %>% messagef(params$verbose, params$log)
    if(level == 1)
        tic.clearlog()
    sprintf('%s%s', prefix, name) %>% tic
    if(name == 'load_experiment'){
        # TODO: message
        if(is.null(params$spec))
            params$spec = 'human'
        return(params)
    }
    if(name == 'subset_cells'){
        # TODO: message
        if(is.null(params$cell_types))
            params$cell_types   = 'all'
        if(is.null(params$min_rel_prop))
            params$min_rel_prop = 0
        if(is.null(params$min_num))
            params$min_num      = 0
        if(is.null(params$balanced))
            params$balanced     = TRUE
        return(params)
    }
    if(name == 'subset_genes'){
        # TODO: message
        if(is.null(params$method))
            params$method = 'hvg'
        if(is.null(params$n))
            params$n = 100
        return(params)
    }
    if(name == 'construct_graph'){
        if(is.null(params$method))
            params$method  = 'ggm'
        if(is.null(params$agg_fun))
            params$agg_fun = 'intersect'
        if(!is.null(params$log))
            params$ggm$log  = params$tf$log = params$coexp$log = params$vis$log = params$log
        return(params)
    }
    if(name == 'ggm_net'){
        if(is.null(params$threshold))
            params$threshold = 0.05
        if(is.null(params$method))
            params$method = 'glasso'
        if(is.null(params$significance))
            params$significance = 0.05
        if(is.null(params$mc.cores))
            params$mc.cores = detectCores()/2
        if(is.null(params$rho))
            params$rho_vals = 0.1
        else
            params$rho_vals = params$rho
        return(params)
    }
    if(name == 'tf_net'){
        if(is.null(params$spec))
            params$spec = 'human'
        return(params)
    }
    if(name == 'coexp_net'){
        if(is.null(params$num_edges))
            if(is.null(params$min_rel_prop) & is.null(params$min_mi))
                params$num_edges = dim(mtx)[2] * 5 # TODO: rationalize this
        if(is.null(params$min_rel_prop))
            params$min_rel_prop = 0
        if(is.null(params$min_mi))
            params$min_mi       = 0 
        if(is.null(params$mc.cores))
            params$mc.cores     = detectCores()/2
        return(params)
    }    
    if(name == 'adj2graph'){
        if(is.null(params$directed))
            params$directed    = FALSE
        if(is.null(params$community))
            params$community   = TRUE
        if(is.null(params$betweenness))
            params$betweenness = FALSE
        if(is.null(params$plot))
            params$plot        = TRUE
        if(is.null(params$vis_network))
            params$vis_network = FALSE
        return(params)
    }
    if(name == 'compare_networks'){
        if(!is.null(params$log))
            params$redim_mtx$log = params$log
        return(params)
    }  
    if(name == 'redim_mtx'){
        if(is.null(params$method))
            params$method      = 'umap'
        if(is.null(params$plot))
            params$plot        = TRUE
        if(!is.null(params$log))
            params$plot_args$log = params$log
        if(params$verbose)
            params$plot_args$verbose = TRUE  
        return(params)
    }  
    if(name == 'plot_redim_mtx'){
        if(length(params$colors) <= 1)
            params$colors = '#b9c2d9'
        if(length(params$shapes) <= 1)
            params$shapes = 1
        if(is.null(params$col.label))
            params$col.label = 'color'
        if(is.null(params$shape.label))
            params$shape.label = 'shape'
        if(is.null(params$legend))
            params$legend = FALSE
        if(is.null(params$palette))
            params$palette = 'Dark2'
        return(params)
    }      
}