
run_novosparc <- function(
    input_dir, output_dir, tag, atlas_locs_f, atlas_mtx_f, expr_mtx_f,
    ncells=1000, nns=5, nnt=5, alpha=0.8, epsilon=0.005, seed=1996){

    novosparc_c = sprintf('python code/sma02_novosparc_run.py 
    --data_dir %s \
    --out_dir %s \
    --tag %s \
    --atlas_locs_f %s \
    --atlas_mtx_f %s \
    --expr_mtx_f %s \
    --ncells %s \
    --nns %s \
    --nnt %s \
    --alpha %s \
    --epsilon %s \
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
    seed)
    print(novosparc_c)
    system(novosparc_c)
}