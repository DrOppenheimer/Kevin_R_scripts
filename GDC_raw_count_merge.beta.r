GDC_raw_count_merge <- function( id_list="my_id_list", my_rot="no", pseudo_fudge=NA, order_rows=TRUE,  order_columns=TRUE, debug=FALSE, verbose=FALSE, remove_tag=".htseq.counts")
    
{                       
    ### MAIN ###
    ###### load the neccessary packages
    if ( is.element("matlab", installed.packages()[,1]) == FALSE ){ install.packages("matlab") }    
    library(matlab)

    if(debug==TRUE){print("Made it here 1")}
    
    my_ids <- flatten_list(as.list(scan(file=id_list, what="character")))

    if(debug==TRUE){print("Made it here 2")}
   
    # read through the files and build out the data matrix
    for ( i in 1:length(my_ids) ){
        print(paste("Processing sample (", i, ")", "of", "[", length(my_ids),"]"))
        if( i==1 ){ # on first sample, create the data matrix
            if(verbose==TRUE){print(paste("Processing sample (", i, ")"))}
            my_data_matrix <- data.matrix(read.table(file=my_ids[i], row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE))
            colnames(my_data_matrix) <- my_ids[i]
        }else{ # for all additional samples add on to the existing matrix
            if(verbose==TRUE){print(paste("Processing sample (", i, ")"))}
            my_sample_matrix  <- data.matrix(read.table(file=my_ids[i], row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE))
            colnames(my_sample_matrix) <- my_ids[i]
            my_data_matrix <- combine_matrices_by_column(my_data_matrix, my_sample_matrix)
        }
    }

    if(debug==TRUE){print("Made it here 4")}

    # remove tag from colnames
    colnames(my_data_matrix) <- gsub(remove_tag, "", colnames(my_data_matrix))

    # substitute for missing counts - NA by default
    if( is.na(pseudo_fudge)==TRUE ){}else{
        pseudo_count <- min(my_data_matrix, na.rm=TRUE)/pseudo_fudge # find the min real value; that num/pseudo_fudge = pseudo_count value
        comb_matrix[is.na(my_data_matrix)] <- pseudo_count # replace NA with pseudo_count
    }
    
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
