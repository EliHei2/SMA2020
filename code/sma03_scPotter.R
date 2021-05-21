

split_cells <- function(vec, train_prop=0.9){
    train = sample(vec, floor(length(vec)*train_prop), replace=FALSE)
    list(train = train, test=setdiff(vec, train))
}

save_exp <- function(
    root_path,
    adj,
    data_train,
    data_test,
    classes_train, 
    classes_test,
    feature_names 
){
    data.table(as.matrix(adj)) %>% fwrite(file.path(root_path, 'adj.txt'))

    classes_train = classes_train[order(class_),]
    data.table(t(data_train[classes_train$cell_id,])) %>%
        fwrite(file.path(root_path, 'data_train.txt'))
    classes_train %>%
        fwrite(file.path(root_path, 'classes_train.txt'))

    classes_test = classes_test[order(class_),]
    data.table(t(data_test[classes_test$cell_id,])) %>%
        fwrite(file.path(root_path, 'data_test.txt'))
    classes_test %>%
        fwrite(file.path(root_path, 'classes_test.txt'))

    genes %>% write(file.path(root_path, 'feature_names.txt'))
    colnames(classes_train$cell_id) %>% write(file.path(root_path, 'cells_train.txt'))
    colnames(classes_test$cell_id) %>% write(file.path(root_path, 'cells_test.txt'))
    unique(classes_train$class_) %>% write(file.path(root_path, 'classes.txt'))
}