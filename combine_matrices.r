combine_matrices_by_column <- function(matrix1, matrix2, pseudo_fudge=10000, order_rows=TRUE, order_columns=TRUE){

    # perform the merge
    comb_matrix<- merge(matrix1, matrix2, by="row.names", all=TRUE)

    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Row.names
    comb_matrix$Row.names <- NULL
    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    pseudo_count <- min(comb_matrix, na.rm=TRUE)/pseudo_fudge # find the min real value; that num/pseudo_fudge = pseudo_count value
    comb_matrix[is.na(comb_matrix)] <- pseudo_count # replace NA with pseudo_count
    
    # order columns
    if( order_rows==TRUE){
        ordered_rownames <- order(rownames(comb_matrix))
        comb_matrix <- comb_matrix[ordered_rownames,]
    }

    # order rows
    if( order_columns==TRUE){
        ordered_colnames <- order(colnames(comb_matrix))
        comb_matrix <- comb_matrix[,ordered_colnames]
    }
    
    return(comb_matrix)
}


combine_matrices_by_row <- function(matrix1, matrix2, pseudo_fudge=10000, order_rows=TRUE, order_columns=TRUE){

    # perform the merge
    comb_matrix<- merge(matrix1, matrix2, by="col.names", all=TRUE)

    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Col.names
    comb_matrix$Col.names <- NULL
    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    pseudo_count <- min(comb_matrix, na.rm=TRUE)/pseudo_fudge # find the min real value; that num/pseudo_fudge = pseudo_count value
    comb_matrix[is.na(comb_matrix)] <- pseudo_count # replace NA with pseudo_count
    
    # order columns
    if( order_rows==TRUE){
        ordered_rownames <- order(rownames(comb_matrix))
        comb_matrix <- comb_matrix[ordered_rownames,]
    }

    # order rows
    if( order_columns==TRUE){
        ordered_colnames <- order(colnames(comb_matrix))
        comb_matrix <- comb_matrix[,ordered_colnames]
    }
    
    return(comb_matrix)
}
