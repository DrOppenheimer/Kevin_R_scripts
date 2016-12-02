project_download_and_merge_data <- function(projects, data_type="HTSeq - Counts", package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc"), rows_to_remove=c("__alignment_not_unique","__ambiguous","__no_feature","__not_aligned","__too_low_aQual"), cleanup=TRUE, debug=FALSE, log="my_log.txt"){

    ## selected_projects <- c(
    ##     "TCGA-BRCA",
    ##     "TCGA-COAD",
    ##     "TCGA-HNSC",
    ##     "TCGA-KICH",
    ##     "TCGA-KIRC",
    ##     "TCGA-KIRP",
    ##     "TCGA-LIHC",
    ##     "TCGA-LUAD",
    ##     "TCGA-LUSC",
    ##     "TCGA-UCEC",
    ##     "TCGA-BLCA"
    ## )
   
    ## test_projects <- c(
    ##     "TCGA-CHOL",
    ##     "TCGA-ESCA"
    ## )
     
    # project_download_and_merge_data("TCGA-CHOL", cleanup=FALSE, debug=TRUE)
    # project_download_and_merge_data(test_projects, cleanup=TRUE, debug=TRUE)
    # project_download_and_merge_data(selected_projects, cleanup=TRUE, debug=TRUE)
    
    write(paste( "Begin log", date() ), file=log, append=FALSE)
    
    # make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }
   
    # download the data
    download_all_from_GDC(projects, data_type, output="default", rows_to_remove=rows_to_remove, cleanup=cleanup, debug=debug, log=log) 
}

download_all_from_GDC <- function(projects, data_type, output, rows_to_remove, cleanup, debug, log){ ### 11-29-16
    for (p in projects) {

        if( debug==TRUE ){ write(paste("Processing:", p), file=log, append=TRUE) }
        
        # delete any pre-exisiting count files
        file_list <- dir(pattern=".htseq.counts.gz$")
        if( debug==TRUE ){
            write("made it here (0)", file=log, append=TRUE)
            TEST.file_list <<- file_list
        }
        if ( length(file_list) > 0 ){
            for ( i in file_list){
                unlink( i )
            }
        }
        
        # data type needs to be reformatted for the url
        data_type_url <- url_encode(data_type)
        if( debug==TRUE ){ write("made it here (1)", file=log, append=TRUE) }
        
        # create output name or use default
        data_type_filename <- gsub(" ", "", data_type)  
        if( identical( output, "default" )==TRUE ){
            output_filename=paste(p, ".merged.", data_type_filename, ".txt", sep="", collapse="")
        }else{
            output_filename <- output
        }
        if( debug==TRUE ){ write("made it here (2)", file=log, append=TRUE) }
        
        print(paste("Starting", p))
        if( debug==TRUE ){ write("made it here (3)", file=log, append=TRUE) }

        # download time (start)
        tictoc::tic()
        
        # get the list of files for the project
        my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22", data_type_url,"%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
        if( debug==TRUE ){
            write("made it here (4)", file=log, append=TRUE)
            TEST.my_call <<- my_call
        }
        
        my_call.json <- fromJSON(getURL(my_call))
        if( debug==TRUE ){
            write("made it here (5)", file=log, append=TRUE)
            TEST.my_call.json <<- my_call.json
        }
        
        UUID.list <- unlist(my_call.json$data$hits)
        if( debug==TRUE ){
            write("made it here (6)", file=log, append=TRUE)
            TEST.UUID.list <<- UUID.list
        }
        
        for(j in UUID.list) { # check this part - file returned has its own UUID - agrees for data and metadata, but is not same as UUID in list here (UUID.list)
            print(paste0(j, ": ", j))
            system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                         j,
                         "'",
                         sep=""))
        }
        write(paste("Done downloading", p), file=log, append=TRUE)
        # download time (end)
        elapsed_time <- tictoc::toc()
        elapsed_time <- elapsed_time$toc - elapsed_time$tic
        write(paste("Download time: ", elapsed_time), file=log, append=TRUE)

        
        if( debug==TRUE ){ write("made it here (7)", file=log, append=TRUE) }
    
        # merge files into a matrix file, delete the intermediates
        ###file_list <- paste(UUID.list, ".htseq.counts.gz", sep="")
        # merge time (start)
        tictoc::tic()
        write(paste("Merging files from", p), file=log, append=TRUE)
        file_list <- dir(pattern=".htseq.counts.gz$")
        output_matrix <- matrix()
        column_names <- vector(mode="character")
        file_count <- 0
        # merge with "merge" (use merge function if the rownames do not match, and cbind if they do)
        for ( i in file_list ){
            if( debug==TRUE ){ write("made it here (8)", file=log, append=TRUE) }
            if ( file_count==0 ){
                input_matrix <- import_metadata( i )
                column_names <- c( column_names, gsub(".htseq.counts.gz$", "", i) )
                output_matrix <- input_matrix
                file_count =+ 1
            }else{
                if( debug==TRUE ){ print(paste("Merging (with merge) ", i)) }
                input_matrix <- import_metadata( i )
                column_names <- c( column_names, gsub(".htseq.counts.gz$", "", i) )
                if( identical( rownames(output_matrix),  rownames(input_matrix)) == TRUE  ){
                    if( debug==TRUE ){
                        print("rownames identical")
                        my_dim <- dim(output_matrix)
                        print(my_dim)
                    }
                    output_matrix <- cbind(output_matrix, input_matrix)
                }else{
                    output_matrix <- combine_matrices_by_column(output_matrix, input_matrix)
                    if( debug==TRUE ){
                        print("rownames NOT identical")
                        my_dim <- dim(output_matrix)
                        print(my_dim)
                    }
                }
            }
        }
        # merge time (end)
        elapsed_time <- tictoc::toc()
        elapsed_time <- elapsed_time$toc - elapsed_time$tic
        write(paste("Merge time: ", elapsed_time), file=log, append=TRUE)
        
        if( debug==TRUE ){ write("made it here (9)", file=log, append=TRUE) }
    
        # add the column names
        colnames(output_matrix) <- column_names
        if( debug==TRUE ){
            write("made it here (10)", file=log, append=TRUE)
            TEST.column_names <<- column_names
        }

    
        # sort rows and columns
        # order rows
        ordered_colnames <- order(colnames(output_matrix))
        output_matrix <- output_matrix[,ordered_colnames]
        #colnames(output_matrix) <- colnames(output_matrix)[ordered_colnames]
        if( debug==TRUE ){ write("made it here (11)", file=log, append=TRUE) }
    
        # order columns
        ordered_rownames <- order(rownames(output_matrix))
        output_matrix <- output_matrix[ordered_rownames,]
        #rownames(output_matrix) <- rownames(output_matrix)[ordered_rownames]
        if( debug==TRUE ){ write("made it here (12)", file=log, append=TRUE) }
    
        print(paste("Done merging", p))

        # remove selected rows
        for ( i in rows_to_remove ) {
            output_matrix <- output_matrix[!rownames(output_matrix) %in% c(i), ]
        }

        # export merged data
        export_data(output_matrix, output_filename)

        # cleanup
        if( cleanup==TRUE ){
            file_list <- dir(pattern=".htseq.counts.gz$")
            if ( length(file_list) > 0 ){
                for ( i in file_list){
                    unlink( i )
                }
            }
        }
    
    }

}


# function to combine input from the inividually download matrices
combine_matrices_by_column <- function(matrix1, matrix2, export=NA, use_fudge=FALSE, pseudo_fudge=10000, na_2=NA, from_file=FALSE, order_rows=FALSE, order_columns=FALSE, merge_sort=FALSE){
    # import data from file if that option is selected
    if(from_file==TRUE){
        matrix1<-import_metadata(matrix1)
        matrix2<-import_metadata(matrix2)
    }
    # perform the merge
    comb_matrix<- merge(matrix1, matrix2, by="row.names", all=TRUE, sort=merge_sort)
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



# function to import data and metadata --- better than old import_data as it can hand numerical and nominal data
import_metadata <- function(group_table){ #, group_column, sample_names){
    metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
        read.table(
            file=group_table,row.names=1,header=TRUE,sep="\t",
            colClasses = "character", check.names=FALSE,
            comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
        )
    )
}



# function to export data
export_data <- function(data_object, file_name){
  write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}


## http://stackoverflow.com/questions/29820029/how-to-combine-multiple-matrix-frames-into-one-using-r
## m1 <- matrix(c('tp53','apc','c1','c2'),2);
## m2 <- matrix(c('tp53','col2a1','d1','d2'),2);
## m3 <- matrix(c('tp53','wt1','e1','e2'),2);
## m <- Reduce(function(x,y) merge(x,y,1,all=T),list(m1,m2,m3));
## m;
## ##       V1 V2.x V2.y   V2
## ## 1    apc   c2 <NA> <NA>
## ## 2   tp53   c1   d1   e1
## ## 3 col2a1 <NA>   d2 <NA>
## ## 4    wt1 <NA> <NA>   e2









