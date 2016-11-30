project_download_and_merge_data <- function(projects, data_type="HTSeq - Counts", package_list=c("urltools","RJSONIO","RCurl"), debug=FALSE){

    # project_download_and_merge_data("TCGA-LUAD", debug=TRUE)
    
    # make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }
   
    # download the data
    download_all_from_GDC(projects, data_type, output="default", debug) 
}

download_all_from_GDC <- function(projects, data_type, output, debug){ ### 11-29-16
    for (p in projects) {

        # delete any pre-exisiting count files
        file_list <- dir(pattern=".htseq.counts.gz$")
        if( debug==TRUE ){
            print("made it here (0)")
            TEST.file_list <<- file_list
        }
        if ( length(file_list) > 0 ){
            for ( i in file_list){
                unlink( i )
            }
        }

        #break
        
        # data type needs to be reformatted for the url
        data_type_url <- url_encode(data_type)
        if( debug==TRUE ){ print("made it here (1)") }
        
        # create output name or use default
        data_type_filename <- gsub(" ", "", data_type)  
        if( identical( output, "default" )==TRUE ){
            output=paste(p, ".merged.", data_type_filename, ".txt", sep="", collapse="")
        }
        if( debug==TRUE ){ print("made it here (2)") }
        
        print(paste("Starting", p))
        if( debug==TRUE ){ print("made it here (4)") }

        my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22", data_type_url,"%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
        if( debug==TRUE ){
            print("made it here (5)")
            TEST.my_call <<- my_call
        }
        
        my_call.json <- fromJSON(getURL(my_call))
        if( debug==TRUE ){
            print("made it here (6)")
            TEST.my_call.json <<- my_call.json
        }
        
        UUID.list <- unlist(my_call.json$data$hits)
        if( debug==TRUE ){
            print("made it here (7)")
            TEST.UUID.list <<- UUID.list
        }
        
        for(j in UUID.list) { # check this part - file returned has its own UUID - agrees for data and metadata, but is not same as UUID in list here (UUID.list)
            print(paste0(j, ": ", j))
            system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                         j,
                         "'",
                         sep=""))
        }
        print(paste("Done downloading", p))
    }
    
    
    #return(UUID.list)

    # merge files into a matrix file, delete the intermediates
    #file_list <- paste(UUID.list, ".htseq.counts.gz", sep="")
    print(paste("Merging files from", p))
    file_list <- dir(pattern=".htseq.counts.gz$")
    output_matrix <- matrix()
    for ( i in file_list ){
        input_matrix <- import_metadata( i )
        output_matrix <- combine_matrices_by_column(output_matrix, input_matrix)
    }
    print(paste("Done merging", p))

    # export merged data
    export_data(output_matrix, data_type_filename)

    # cleanup
    if ( length(file_list) > 0 ){
        for ( i in file_list){
            unlink( i )
        }
    }
    
}



# function to combine input from the inividually download matrices
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













