stat_results_to_input <- function(stats_matrix, output_type="file"){

    my_data <- import_data(stats_matrix)

    col_names <- vector()
    delete_index <- vector()

    for ( i in 1:ncol(my_data) ){

        col_name <- colnames(my_data)[i]
    
        if( regexpr(pattern="group_mean$", perl=TRUE, col_name) > 0 ){
            delete_index <- c(delete_index, -i)
        }else{
            c(delete_index, -i)
        }
    
        if( regexpr(pattern="group_sd$", perl=TRUE, col_name) > 0 ){
            delete_index <- c(delete_index, -i)
        }else{
            c(delete_index, -i)
        }
    
        if( regexpr(pattern="stat$", perl=TRUE, col_name) > 0 ){
            delete_index <- c(delete_index, -i)
        }else{
            c(delete_index, -i)
        }
    
        if( regexpr(pattern="p$", perl=TRUE, col_name) > 0 ){
            delete_index <- c(delete_index, -i)
        }else{
            c(delete_index, -i)
        }
    
        if( regexpr(pattern="fdr$", perl=TRUE, col_name) > 0 ){
            delete_index <- c(delete_index, -i)
        }else{
            c(delete_index, -i)
        }

        col_names <- c(col_names, unlist( strsplit(col_name, split="::"))[1] )
    }

    new_matrix <- my_data[,delete_index]
    colnames(new_matrix) <- col_names[delete_index]
    rownames(new_matrix) <- rownames(my_data)

    if( identical(output_type, "file") == TRUE ){
        output_filename <- gsub(" ", "", paste(stats_matrix, ".STATS_REMOVED.txt"))
        export_data(new_matrix, file=output_filename)
    }else{
        return(new_matrix)
    }
    
}




import_data <- function(file_name)
{
  data.matrix(read.table(file_name, row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
}

export_data <- function(data_object, file_name){
  write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}


