# utilities
suppressPackageStartupMessages({
  library('RColorBrewer')
  library('circlize')
  library('magrittr')
})

# nice_cols   = CATALYST:::.cluster_cols
nice_cols   = c(
    "#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72", "#B17BA6", 
    "#FF7F00", "#FDB462", "#E7298A", "#E78AC3", "#33A02C", "#B2DF8A", 
    "#55A1B1", "#8DD3C7", "#A6761D", "#E6AB02", "#7570B3", "#BEAED4", 
    "#666666", "#999999", "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", 
    "#808000", "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00"
    )

# define factor orderings
lesion_ord  = c("WM", "NAWM", "AL", "CAL", "CIL", "RL", "GM", "NAGM", "GML")
lesion_cols = c(
    RColorBrewer::brewer.pal(6, 'Reds'),
    RColorBrewer::brewer.pal(3, 'Blues')
    ) %>% setNames(lesion_ord)


disease_ord     = c("CTR", "PPMS", "RRMS", "SPMS", "MS")
disease_cols    = RColorBrewer::brewer.pal(5, 'Greys') %>% setNames(disease_ord)

matter_ord      = c('WM', 'GM')
matter_cols     = c('grey80', 'grey30') %>% setNames(matter_ord)
# matter_ord      = c('WM', 'WMGM', 'GM')
# matter_cols     = c('grey80', 'yellow', 'grey30') %>% setNames(matter_ord)

sex_ord         = c('F', 'M', 'unknown')
sex_cols        = c('red', 'orange', 'grey') %>% setNames(sex_ord)

outlier_ord     = c('ok', 'outlier')
outlier_cols    = c('white', 'black') %>% setNames(outlier_ord)

source_ord      = c('Amsterdam BB', 'Edinburgh BB', 'UK MS TB', 'Nat19', 'Rowitch19')
source_cols     = c(
    RColorBrewer::brewer.pal(5, 'Reds')[seq.int(3)+2],
    RColorBrewer::brewer.pal(4, 'Blues')[seq.int(2)+2]
    ) %>% setNames(source_ord)

# define colours
broad_ord   = c("Oligodendrocytes", "OPCs / COPs", 
  "Astrocytes", "Microglia", "Excitatory neurons", "Inhibitory neurons", 
  "Endothelial cells", "Pericytes", "Immune")
broad_short = list(`Oligodendrocytes`='oligo', `OPCs / COPs`='opc_cop', 
  `Astrocytes`='astro', `Microglia`='micro', `Excitatory neurons`='excit', 
  `Inhibitory neurons`='inhib', `Endothelial cells`='endo', 
  `Pericytes`='peri', `Immune`='immune')
broad_cols  = c(nice_cols[seq.int(length(broad_ord))], 'grey') %>% 
    setNames(c(broad_ord, 'soup'))

# function for making nice colours
cols_fn <- function(mat, res, pal, pal_dir, range=c('has_zero', 'symmetric', 'natural')) {
    # check inputs
    stopifnot(pal_dir %in% c(-1,1))
    range       = match.arg(range)

    # check values
    n_vals      = length(unique(mat))
    if (n_vals == 1) {
        pal_cols    = .get_pal_cols(pal, 3)
        cols        = pal_cols[[1]]
        names(cols) = unique(mat)
        return(cols)
    }

    # check inputs
    sgn         = 1
    if (range=='has_zero') {
        assert_that( all(mat >= 0) | all(mat <= 0) )
        if (all(mat <= 0) ) {
            sgn     = -1 
            mat     = mat * -1
        }
        max_val     = mat %>% as.vector %>% max %>% `/`(res) %>% ceiling %>% `*`(res)
        max_val     = round(max_val, 3)
        min_val     = 0
    } else if (range=='symmetric') {
        max_val     = mat %>% as.vector %>% abs %>% max %>% `/`(res) %>% ceiling %>% `*`(res)
        max_val     = round(max_val, 3)
        min_val     = -max_val
    } else if (range=='natural') {
        max_val     = mat %>% as.vector %>% max %>% `/`(res) %>% ceiling %>% `*`(res)
        min_val     = mat %>% as.vector %>% min %>% `/`(res) %>% floor %>% `*`(res)
        max_val     = round(max_val, 3)
        min_val     = round(min_val, 3)
    }

    # define colours
    seq_vals    = seq(min_val, max_val, res)
    n_cols      = length(seq_vals)
    pal_cols    = .get_pal_cols(pal, n_cols)
    if ( length(pal_cols) < n_cols ) {
        n_cols      = length(pal_cols)
        seq_vals    = seq(min_val, max_val, length.out=n_cols)
    }

    # define colours
    if (pal_dir == -1)
        pal_cols    = rev(pal_cols)

    # make colour function
    if (sgn == 1)
        cols    = colorRamp2(seq_vals, pal_cols)
    else
        cols    = colorRamp2(-seq_vals, rev(pal_cols))

    return(cols)
}

.get_pal_cols <- function(pal_str = c('viridis', 'magma', 'Blues', 'BuGn', 
    'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 
    'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 
    'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 
    'Set1', 'Set2', 'Set3'), n_cols) {
    pal_str     = match.arg(pal_str)
    if ( pal_str == 'viridis' ) {
        pal_cols    = str_remove(viridis(n_cols), 'FF$')
    } else if ( pal_str == 'magma' ) {
        pal_cols    = str_remove(magma(n_cols), 'FF$')
    } else {
        suppressWarnings({pal_cols = brewer.pal(n_cols, pal_str)})
    }

    return(pal_cols)
}

# loading metadata with factor labels
load_meta_dt <- function(meta_f, outlier_samples = NULL) {
  meta_dt = meta_f %>% 
    fread %>%
    setnames('library_id', 'sample_id') %>%
    .[, lesion_type := factor(lesion_type, levels=lesion_ord)] %>%
    .[, matter := factor(matter, levels=matter_ord)] %>%
    .[, sex := factor(sex, levels=c('F', 'M'))] %>%
    .[, pmi_cat := factor(cut(post_mortem_m/60, breaks=c(0,1,12,1000), 
        labels=c('01H', '12H', '12H+'))) ] %>%
    .[ is.na(post_mortem_m),  pmi_cat := '12H' ] 
  patients_dt = meta_dt[, .(patient_id, age)] %>% 
    unique %>%
    .[, age_norm := scale(age) ]
  meta_dt     = merge(meta_dt, patients_dt[, .(patient_id, age_norm)], 
    by='patient_id')

  # add outlier samples
  if (!is.null(outlier_samples))
    meta_dt[, outlier := ifelse(sample_id %in% outlier_samples, 'outlier', 'ok') ]

  return(meta_dt)
}