combine_matrices_by_column <- function(matrix1, matrix2, export=NA, use_fudge=FALSE, pseudo_fudge=10000, na_2=NA, from_file=FALSE, order_rows=TRUE, order_columns=TRUE){

    # import data from file if that option is selected
    if(from_file==TRUE){
        matrix1<-import_metadata(matrix1)
        matrix2<-import_metadata(matrix2)
    }
        
    # perform the merge
    comb_matrix<- merge(matrix1, matrix2, by="row.names", all=TRUE)

    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Row.names
    comb_matrix$Row.names <- NULL
    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    if ( is.na(na_2)==FALSE ){
        comb_matrix[is.na(comb_matrix)] <- na_2 # replace NA with pseudo_count
    }
    if ( use_fudge==TRUE ){
        pseudo_count <- min(comb_matrix, na.rm=TRUE)/pseudo_fudge # find the min real value; that num/pseudo_fudge = pseudo_count value
        comb_matrix[is.na(comb_matrix)] <- pseudo_count # replace NA with pseudo_count
    }

    
    
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

    if( is.na(export)==FALSE ){
        output_name <- gsub(" ", "", paste(export, ".merged_data.txt"))
        export_data(comb_matrix, output_name)
    }
    
    return(comb_matrix)
}




combine_matrices_by_row <- function(matrix1, matrix2, pseudo_fudge=10000, from_file=FALSE, order_rows=TRUE, order_columns=TRUE){

    # import data from file if that option is selected
    if(from_file==TRUE){
        matrix1<-import_metadata(matrix1)
        matrix2<-import_metadata(matrix2)
    }

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

    if( is.na(export)==FALSE ){
        output_name <- gsub(" ", "", paste(export, ".merged_data.txt"))
        export_data(comb_matrix, output_name)
    }
    
    return(comb_matrix)
}



import_metadata <- function(group_table){ #, group_column, sample_names){
    metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
        read.table(
            file=group_table,row.names=1,header=TRUE,sep="\t",
            colClasses = "character", check.names=FALSE,
            comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
        )
    )
}



export_data <- function(data_object, file_name){
  write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}

