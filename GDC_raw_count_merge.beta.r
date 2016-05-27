GDC_raw_count_merge <- function( id_list="my_id_list", my_rot="no", order_rows=TRUE,  order_columns=TRUE, debug=FALSE)
    
{                       
    ### MAIN ###
    ###### load the neccessary packages
    if ( is.element("matlab", installed.packages()[,1]) == FALSE ){ install.packages("matlab") }    
    library(matlab)

    if(debug==TRUE){print("Made it here 1")}
    
    my_ids <- flatten_list(as.list(scan(file=id_list, what="character")))

    if(debug==TRUE){print("Made it here 2")}
    
    my_data_matrix <- data.matrix()

    if(debug==TRUE){print("Made it here 3")}
    
    # read through the files and build out the data matrix
    for ( i in 1:length(my_ids) ){
        if(debug==TRUE){print(paste("Processing sample (", i, ")"))}
        print(paste("First read: ", i))
        my_sample_matrix  <- data.matrix(read.table(file=my_ids[i], row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE))
        colanmes(my_sample_matrix) <- my_ids[i]
        my_data_matrix <- combine_matrices_by_column(my_data_matrix, my_sample_matrix)
    }

    if(debug==TRUE){print("Made it here 4")}
    
    # rotate the matrix if that option is selected
    if( identical(my_rot, "yes")==TRUE ){
        my_data_matrix <- rot90(rot90(rot90(my_data_matrix)))
    }

    if(debug==TRUE){print("Made it here 5")}
    
    # output the matrix as a flat file
    fileout_name <- gsub(" ", "", paste(id_list, ".merged_data.txt"))
    export_data(my_data_matrix, fileout_name)
    
}



### SUBS ###                    
import_data <- function(file_name){
    data.matrix(read.table(file_name, row.names=NA, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
}



export_data <- function(data_object, file_name){
    write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}



flatten_list <- function(some_list){
    flat_list <- unlist(some_list)
    flat_list <- gsub("\r","",flat_list)
    flat_list <- gsub("\n","",flat_list)
    flat_list <- gsub("\t","",flat_list)
}    



combine_matrices_by_column <- function(matrix1, matrix2, order_rows=TRUE, order_columns=TRUE){

    # perform the merge
    comb_matrix<- merge(matrix1, matrix2, by="row.names", all=TRUE)

    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Row.names
    comb_matrix$Row.names <- NULL
    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    
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
