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
   
    test_projects <- c(
        "TCGA-CHOL",
        "TCGA-ESCA"
    )


# load scripts
#source("~/git/Kevin_R_scripts/GDC_metadata_tools.12-5-16.r")

source("~/git/Kevin_R_scripts/calculate_pco.r")
source("~/git/Kevin_R_scripts/preprocessing_tool.r")
source("~/git/Kevin_R_scripts/calc_stats.r")
source("~/git/Kevin_R_scripts/render_calculated_pcoa.r")
source("~/git/Kevin_R_scripts/")





combine_matrices_by_column <- function(matrix1, matrix2, func_order_rows=FALSE, func_order_columns=FALSE, func_debug=FALSE){

    # perform the merge
    comb_matrix<- merge(data.frame(matrix1), data.frame(matrix2), by="row.names", all=TRUE, sort=FALSE) # column ordering ("sort") is controlled by wrapper options
    
    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Row.names
    comb_matrix$Row.names <- NULL

    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    
    # order columns
    if( func_order_rows==TRUE){
        ordered_rownames <- order(rownames(comb_matrix))
        comb_matrix <- comb_matrix[ordered_rownames,]
    }
    
    # order rows
    if( func_order_columns==TRUE){
        ordered_colnames <- order(colnames(comb_matrix))
        comb_matrix <- comb_matrix[,ordered_colnames]
    }
    
    comb_matrix <- as.matrix(comb_matrix)
    
    return(comb_matrix)
}


get_project_UUIDs <- function(
    projects,
    data_type="HTSeq - Counts", # "Methylation Beta Value"
    package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc"),
    cleanup=TRUE,
    debug=FALSE,
    log="default",
    write_to_file = TRUE,
    output_filename_prefix= "my_uuid_list",
    output_include_timestamp=FALSE,
    output_filename_extension="UUID_list.txt",
    output_log_extension="log"
)
{

    ### SUBS ###

    # function to export UUIDs
    export_UUIDs <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE, eol="\n")
    }

    ### MAIN ###
    
    # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
    # create the log file
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension)
    }else{
        log_filename <- paste0(output_filename_prefix,".", output_log_extension)
    }

    # make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }
    
    # data type needs to be reformatted for the url
    data_type_url <- url_encode(data_type)
    if( debug==TRUE ){ write("made it here (1)", file=log, append=TRUE) }

    # create list to hold the UUIDs
    UUID_list <- vector(mode="character")

    for ( p in projects ){

        # create API call to get the UUIDs
        if( identical(data_type, "HTSeq - Counts")){
            my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22", data_type_url,"%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
        }else if ( identical(data_type, "Methylation Beta Value") ){
            my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22data_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22", data_type_url,"%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
        }else{
            stop(paste0("data_type ( ", data_type, " ) is not recognized"))
        }
 
        if( debug==TRUE ){ print(paste0("my_call :: ", my_call)) }
        
        my_call.json <- fromJSON(getURL(my_call))        
        project.UUID_list <- unlist(my_call.json$data$hits)
        UUID_list <- c(UUID_list, project.UUID_list)

    }

    # write the list of UUIDs to a file or return as vector
    if( write_to_file ==TRUE){

        # create output filename
        if( output_include_timestamp==TRUE ){
            output_filename <- paste0( output_filename_prefix, ".", my_timestamp, ".", output_filename_extension)
        }else{
            output_filename <- paste0( output_filename_prefix, ".", output_filename_extension)
        }

        export_UUIDs(UUID_list, output_filename)
            
    }else{
        return(UUID_list)
    }
    
}







download_and_merge_RNASeq_data_from_UUID <- function(
    UUID_list,
    #output_prefix = "my_merged_DATA",
    list_is_file=TRUE,
    package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc"),
    rows_to_remove=c("__alignment_not_unique","__ambiguous","__no_feature","__not_aligned","__too_low_aQual"),
    dl_file_pattern=".htseq.counts.gz$", # HumanMethylation # 
    cleanup=TRUE,
    #log="default",
    debug=FALSE,
    output_filename_prefix= "my_RNASeq_data",
    output_include_timestamp=FALSE,
    output_filename_extension="DATA.txt",
    output_log_extension="log"
    
    
){

    ### SUBS ###

    # function to import data or metadata -- does not alter non-numerical data 
    import_metadata <- function(group_table, my_header=FALSE){ #, group_column, sample_names){
        metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
            read.table(
                file=group_table,row.names=1,header=my_header,sep="\t",
                colClasses = "character", check.names=FALSE,
                comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
            )
        )
    }
    
    # function to export data
    export_data <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    }
    
    ### MAIN ###
    
    # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
    # create the log file
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension)
    }else{
        log_filename <- paste0(output_filename_prefix,".", output_log_extension)
    }

    # make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }

    # get the UUID list from file or vector
    if( list_is_file==TRUE ){
        UUID_list_filename <- UUID_list
        UUID_list <- scan(file=UUID_list, what="character")
    }else{
        UUID_list <- list
        UUID_list_filename <- "UUID_list"
    }

    # create output filename
    if( output_include_timestamp==TRUE ){
        output_filename <- paste0( output_filename_prefix, ".", my_timestamp, ".", output_filename_extension)
    }else{
        output_filename <- paste0( output_filename_prefix, ".", output_filename_extension)
    }

    # delete any pre-exisiting count files
    write(paste0("Deleting any previous files with pattern = ", dl_file_pattern), file=log_filename, append=FALSE)
    file_list <- dir(pattern=dl_file_pattern)
    if( debug==TRUE ){
        write("made it here (0)", file=log, append=TRUE)
        TEST.file_list <<- file_list
    }
    if ( length(file_list) > 0 ){
        for ( i in file_list){
            unlink( i )
        }
    }
    
    # download the individual files
    elapsed_time <- tictoc::tic()
    for(UUID in UUID_list) { # check this part - file returned has its own UUID - agrees for data and metadata, but is not same as UUID in list here (UUID.list)
        if( file.exists("curl_log.txt")==TRUE ){ unlink("curl_log.txt") } 
        system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                     UUID,
                     "'",
                     " > curl_log.txt",
                     sep=""))
        filename_temp <- scan(file="curl_log.txt", what="character")
        UUID_filename <- filename_temp[5]
        write(paste0("Done downloading file with UUID :: ", UUID, " and FILENAME :: ", UUID_filename), file=log_filename, append=TRUE)
    }
    elapsed_time <- tictoc::toc()
    elapsed_time <- elapsed_time$toc - elapsed_time$tic
    write(paste("Download time: ", elapsed_time), file=log_filename, append=TRUE)

    # merge files into a single table
    elapsed_time <- tictoc::tic()
    write(paste("Merging files"), file=log_filename, append=TRUE)
    file_list <- dir(pattern=dl_file_pattern)
    output_matrix <- matrix()
    column_names <- vector(mode="character")
    file_count <- 0
    # merge with "merge" (use merge function if the rownames do not match, and cbind if they do)
    for ( i in file_list ){
        if ( file_count==0 ){
            write(paste0("Starting merge with :: ", i), file=log_filename, append=TRUE)
            input_matrix <- import_metadata( i )
            column_names <- c( column_names, gsub(dl_file_pattern, "", i) )
            if( debug==TRUE ){
                print(paste0("FILENAME ::", i, " ___ ", "COLUMN-NAME :: ", column_names[i]) )
            }
            output_matrix <- input_matrix
            file_count =+ 1
        }else{
            input_matrix <- import_metadata( i )
            column_names <- c( column_names, gsub(dl_file_pattern, "", i) )
            if( debug==TRUE ){
                print(paste0("FILENAME ::", i, " ___ ", "COLUMN-NAME :: ", column_names[i]) )
            }
            if( identical( rownames(output_matrix),  rownames(input_matrix)) == TRUE ){
                write("rownames identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with cbind) :: ", i), file=log_filename, append=TRUE)
                output_matrix <- cbind(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }else{
                write("rownames NOT identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with combine_matrices_by_column/merge) :: ", i), file=log_filename, append=TRUE)
                output_matrix <- combine_matrices_by_column(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }
        }
    }

    # add the column names
    colnames(output_matrix) <- column_names
    
    # order columns
    ordered_colnames <- order(colnames(output_matrix))
    output_matrix <- output_matrix[,ordered_colnames]
        
    # order rows
    ordered_rownames <- order(rownames(output_matrix))
    output_matrix <- output_matrix[ordered_rownames,]
    
    # remove selected rows
    for ( i in rows_to_remove ) {
        output_matrix <- output_matrix[!rownames(output_matrix) %in% c(i), ]
    }

    # merge time (end)
    elapsed_time <- tictoc::toc()
    elapsed_time <- elapsed_time$toc - elapsed_time$tic
    write(paste("Merge time: ", elapsed_time), file=log_filename, append=TRUE)

    # export the merged data
    export_data(output_matrix,output_filename)
    write(paste0("Wrote files imported from ", UUID_list_filename, " and wrote them to ", output_filename), file=log_filename, append=TRUE)
                                   
    # cleanup
    if( cleanup==TRUE ){
        file_list <- dir(pattern=dl_file_pattern)
        if ( length(file_list) > 0 ){
            for ( i in file_list){
                unlink( i )
            }
        }
        write("Performed cleanup", file=log_filename, append=TRUE)
    }else{
        write("Cleanup was disabled", file=log_filename, append=TRUE)
    }

    write("DONE", file=log_filename, append=TRUE)
    
}









download_and_merge_Methylation_data_from_UUID <- function(
    UUID_list,
    #output_prefix = "my_merged_DATA",
    list_is_file=TRUE,
    package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc"),
    rows_to_remove=c("__alignment_not_unique","__ambiguous","__no_feature","__not_aligned","__too_low_aQual"),
    dl_file_pattern="HumanMethylation", #
    extract_value="Beta_value",
    average_by="Gene_Symbol",
    cleanup=TRUE,
    #log="default",
    debug=FALSE,
    output_filename_prefix= "my_METHYLATION_data",
    output_include_timestamp=FALSE,
    output_filename_extension="DATA.txt",
    output_log_extension="log"
){

    ### SUBS ###

    ## ## if( debug==TRUE ){ print("made it here (-8)")  }
    
    # function to import data or metadata -- does not alter non-numerical data 
    import_metadata <- function(group_table, my_header=TRUE){ #, group_column, sample_names){
        metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
            read.table(
                file=group_table,row.names=1,header=my_header,sep="\t",
                colClasses = "character", check.names=FALSE,
                comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
            )
        )
    }
    ## ## if( debug==TRUE ){ print("made it here (-7)")  }
    
    # function to export data
    export_data <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    }
    ## ## if( debug==TRUE ){ print("made it here (-6)")  }
    
    ### MAIN ###
    
    ## create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    ## ## if( debug==TRUE ){ print("made it here (-5)")  }
    
    ## create the log file
    ## ## if( debug==TRUE ){ print(paste0("output_filename_prefix: ", output_filename_prefix)); print(paste0("output_log_extension: ", output_log_extension)); }
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension)
    }else{
        log_filename <- paste0(output_filename_prefix, ".", output_log_extension)
    }
    ## ## if( debug==TRUE ){ write("made it here (-4)", file=log_filename, append=TRUE); print("made it here (-4)")  }
    
    ## make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }
    ## ## if( debug==TRUE ){ write("made it here (-3)", file=log_filename, append=TRUE); print("made it here (-3)") }

    ## get the UUID list from file or vector
    if( list_is_file==TRUE ){
        UUID_list_filename <- UUID_list
        UUID_list <- scan(file=UUID_list, what="character")
    }else{
        UUID_list <- list
        UUID_list_filename <- "UUID_list"
    }
    ## ## if( debug==TRUE ){ write("made it here (-2)", file=log_filename, append=TRUE); print("made it here (-2)") }
    
    ## create output filename
    if( output_include_timestamp==TRUE ){
        output_filename <- paste0( output_filename_prefix, ".", my_timestamp, ".", output_filename_extension)
    }else{
        output_filename <- paste0( output_filename_prefix, ".", output_filename_extension)
    }
    ## ## if( debug==TRUE ){ write("made it here (-1)", file=log_filename, append=TRUE); print("made it here (-1)") }
    
    ## delete any pre-exisiting count files
    write(paste0("Deleting any previous files with pattern = ", dl_file_pattern), file=log_filename, append=FALSE)
    file_list <- dir(pattern=dl_file_pattern)
    ## ## if( debug==TRUE ){
    ## ##     write("made it here (0)", file=log_filename, append=TRUE); ; print("made it here (0)")
    ## ##     TEST.file_list <<- file_list
    ## ## }
    if ( length(file_list) > 0 ){
        for ( i in file_list){
            unlink( i )
        }
    }

    ## ## if( debug==TRUE ){ write("made it here (1)", file=log_filename, append=TRUE); print("made it here (1)") }
    
    ## download the individual files
    elapsed_time <- tictoc::tic()
    for(UUID in UUID_list) { # check this part - file returned has its own UUID - agrees for data and metadata, but is not same as UUID in list here (UUID.list)
        if( file.exists("curl_log.txt")==TRUE ){ unlink("curl_log.txt") } 
        system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                     UUID,
                     "'",
                     " > curl_log.txt",
                     sep=""))
        filename_temp <- scan(file="curl_log.txt", what="character")
        UUID_filename <- filename_temp[5]
        write(paste0("Done downloading file with UUID :: ", UUID, " and FILENAME :: ", UUID_filename), file=log_filename, append=TRUE)
    }
    elapsed_time <- tictoc::toc()
    elapsed_time <- elapsed_time$toc - elapsed_time$tic
    write(paste("Download time: ", elapsed_time), file=log_filename, append=TRUE)

    ## ## if( debug==TRUE ){ write("made it here (2)", file=log_filename, append=TRUE); print("made it here (2)") }
                       
    ## merge files into a single table
    elapsed_time <- tictoc::tic()
    write(paste("Merging files"), file=log_filename, append=TRUE)
    file_list <- dir(pattern=dl_file_pattern)
    output_matrix <- matrix()
    column_names <- vector(mode="character")
    file_count <- 0
    if( debug==TRUE ){ print(paste0("file :: ", file_count)) }
    ## merge with "merge" (use merge function if the rownames do not match, and cbind if they do)

    ## ## if( debug==TRUE ){ write("made it here (3)", file=log_filename, append=TRUE); print("made it here (3)") }
    
    for ( i in file_list ){
        if ( file_count==0 ){
            write(paste0("Starting merge with :: ", i), file=log_filename, append=TRUE)
            input_matrix <- import_metadata( i )
            column_names <- c( column_names, gsub(dl_file_pattern, "", i) )
            ## ## if( debug==TRUE ){
            ## ##     print(paste0("FILENAME ::", i, " ___ ", "COLUMN-NAME(s) :: ", c(average_by,extract_value)) )
            ## ## }

            ## ## if( debug==TRUE ){ write("made it here (4)", file=log_filename, append=TRUE); print("made it here (4)") }
            
            ## extract the selected column of values (beta values by default) - also pull out colum that will be used to condense values by averaging
            subselected_input_matrix <- as.matrix(input_matrix[,c(average_by,extract_value)], ncol=1)
            ## ## if(debug==TRUE){TEST.subselected_input_matrix <<- subselected_input_matrix}
            rownames(subselected_input_matrix) <- rownames(input_matrix)
            colnames(subselected_input_matrix) <- c(average_by, extract_value)
            ## hash values that will be averaged, then average them
            sample_hash <- hash()

            ## ## if( debug==TRUE ){ TEST.sample_hash <<- sample_hash; TEST.subselected_input_matrix <<- subselected_input_matrix }
            ## ## if( debug==TRUE ){ write("made it here (4.1a)", file=log_filename, append=TRUE); print("made it here (4.1a)") }
            
            for ( j in 1:nrow(subselected_input_matrix) ){
                ## ## if( debug==TRUE )( print(paste0("file :: ", file_count, " :::: j :: ", j)) )
                ## If the key is new:
                if ( is.null( sample_hash[[  subselected_input_matrix[j,average_by]  ]] ) ){
                    sample_hash[[  subselected_input_matrix[j,average_by]  ]] <- c( subselected_input_matrix[j,extract_value] )
                }else{
                ## If the key is already in the hash
                    sample_hash[[  subselected_input_matrix[j,average_by]  ]] <- c( sample_hash[[  subselected_input_matrix[j,average_by]  ]], subselected_input_matrix[j,extract_value] )
                }
                
                ## ## if( debug==TRUE ){ TEST.sample_hash <<- sample_hash }
                ## ## if( debug==TRUE ){ write("made it here (4.2a)", file=log_filename, append=TRUE); print("made it here (4.2a)") }
            }
            averaged_values <- matrix( nrow=length( keys(sample_hash) ), ncol=1 )
            rownames( averaged_values ) <- keys( sample_hash )
            colnames( averaged_values ) <- paste0( extract_value,".averaged" )
            for ( k in 1:length( keys(sample_hash) ) ){
                averaged_values[k,] <- mean(as.numeric(sample_hash$k))
                ## ## if( debug==TRUE ){ write("made it here (4.3a)", file=log_filename, append=TRUE); print("made it here (4.3a)") }
            }
            output_matrix <- averaged_values
            file_count =+ 1
            if( debug==TRUE ){ print(paste0("file :: ", file_count)) }
            
            
            
            ## ## if( debug==TRUE ){ write("made it here (5)", file=log_filename, append=TRUE); print("made it here (5)") }
            
            ## ## EXAMPLE for how I want the hash averaging to work
            ## > test_hash <- hash()
            ## > for (i in 1:nrow(test3)){ test_hash[[ test3[i,2] ]] <- c( test_hash[[ test3[i,2] ]], test3[i,2]  ) }
            ## > test_hash
            ## <hash> containing 2 key-value pair(s).
            ##   one : one one
            ##   two : two
            ## > for (i in 1:nrow(test3)){ test_hash[[ test3[i,2] ]] <- c( test_hash[[ test3[i,2] ]], test3[i,1]  ) }
            ## > test3
            ##     c_1 c_2  
            ## r_1 "1" "one"
            ## r_2 "1" "one"
            ## r_3 "1" "two"
            ## > test_hash <- hash()
            ## > for (i in 1:nrow(test3)){ test_hash[[ test3[i,2] ]] <- c( test_hash[[ test3[i,2] ]], test3[i,1]  ) }
            ## > test_hash
            ## <hash> containing 2 key-value pair(s).
            ##   one : 1 1
            ##   two : 1                
            
            
        }else{
            input_matrix <- import_metadata( i )
            column_names <- c( column_names, gsub(dl_file_pattern, "", i) )
            ## ## if( debug==TRUE ){
            ## ##     print(paste0("FILENAME ::", i, " ___ ", "COLUMN-NAME :: ", column_names[i]) )
            ## ## }
            
            ## extract the selected column of values (beta values by default) - also pull out colum that will be used to condense values by averaging
            subselected_input_matrix <- as.matrix(input_matrix[,c(average_by,extract_value)], ncol=1)
            rownames(subselected_input_matrix) <- rownames(input_matrix)
            colnames(subselected_input_matrix) <- c(average_by, extract_value)
            ## hash values that will be averaged, then average them
            sample_hash <- hash()

            ## ## if( debug==TRUE ){ TEST.sample_hash <<- sample_hash; TEST.subselected_input_matrix <<- subselected_input_matrix }
            ## ## if( debug==TRUE ){ write("made it here (4.1b)", file=log_filename, append=TRUE); print("made it here (4.1b)") }

            for ( j in 1:nrow(subselected_input_matrix) ){
                ## ## if( debug==TRUE )( print(paste0("file :: ", file_count, " :::: j :: ", j)) )
                ## If the key is new:
                if ( is.null( sample_hash[[  subselected_input_matrix[j,average_by]  ]] ) ){
                    sample_hash[[  subselected_input_matrix[j,average_by]  ]] <- c( subselected_input_matrix[j,extract_value] )
                }else{
                    ## If the key is already in the hash
                    sample_hash[[  subselected_input_matrix[j,average_by]  ]] <- c( sample_hash[[  subselected_input_matrix[j,average_by]  ]], subselected_input_matrix[j,extract_value] )
                }
                
                ## ## if( debug==TRUE ){ TEST.sample_hash <<- sample_hash }
                ## ## if( debug==TRUE ){ write("made it here (4.2b)", file=log_filename, append=TRUE); print("made it here (4.2b)") }
            }
            averaged_values <- matrix( nrow=length( keys(sample_hash) ), ncol=1 )
            rownames( averaged_values ) <- keys( sample_hash )
            colnames( averaged_values ) <- paste0( extract_value,".averaged" )
            for ( k in 1:length( keys(sample_hash) ) ){
                averaged_values[k,] <- mean(as.numeric(sample_hash$k))
            }
            
            input_matrix <- averaged_values 
            
            if( identical( rownames(output_matrix),  rownames(input_matrix)) == TRUE ){
                write("rownames identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with cbind) :: ", i), file=log_filename, append=TRUE)
                output_matrix <- cbind(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }else{
                write("rownames NOT identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with combine_matrices_by_column/merge) :: ", i), file=log_filename, append=TRUE)
                output_matrix <- combine_matrices_by_column(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }   

            ## ## if( debug==TRUE ){ write("made it here (6)", file=log_filename, append=TRUE); print("made it here (6)") }
            file_count =+ 1
            if( debug==TRUE ){ print(paste0("file :: ", file_count)) }
        }
    }
    
    ## add the column names
    colnames(output_matrix) <- column_names
    
    ## order columns
    ordered_colnames <- order(colnames(output_matrix))
    output_matrix <- output_matrix[,ordered_colnames]
    
    ## order rows
    ordered_rownames <- order(rownames(output_matrix))
    output_matrix <- output_matrix[ordered_rownames,]
    
    ## remove selected rows
    for ( i in rows_to_remove ) {
        output_matrix <- output_matrix[!rownames(output_matrix) %in% c(i), ]
    }
    
    ## merge time (end)
    elapsed_time <- tictoc::toc()
    elapsed_time <- elapsed_time$toc - elapsed_time$tic
    write(paste("Merge time: ", elapsed_time), file=log_filename, append=TRUE)
    
    ## export the merged data
    export_data(output_matrix,output_filename)
    write(paste0("Wrote files imported from ", UUID_list_filename, " and wrote them to ", output_filename), file=log_filename, append=TRUE)
    
    ## cleanup
    if( cleanup==TRUE ){
        file_list <- dir(pattern=dl_file_pattern)
        if ( length(file_list) > 0 ){
            for ( i in file_list){
                unlink( i )
            }
        }
        write("Performed cleanup", file=log_filename, append=TRUE)
    }else{
        write("Cleanup was disabled", file=log_filename, append=TRUE)
    }
    
    write("DONE", file=log_filename, append=TRUE)
    
}




        
        
        
        
        
        
        





download_and_merge_metadata_from_UUID <- function(
    UUID_list,
    #output_prefix = "my_merged_METAdata",
    list_is_file=TRUE,
    package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc", "matlab"),
    rows_to_remove=c("pagination.count", "pagination.from", "pagination.page", "pagination.pages", "pagination.size", "pagination.sort", "pagination.total"),
    dl_file_pattern=".htseq.counts.gz$",
    #cleanup=TRUE,
    #log="default",
    rot_90=TRUE,
    debug=FALSE,
    output_filename_prefix= "my_metadata",
    output_include_timestamp=FALSE,
    output_filename_extension="METAdata.txt",
    output_log_extension="log"
    
){

    ### SUBS ###

    ## # function to import data or metadata -- does not alter non-numerical data 
    ## import_metadata <- function(group_table){ #, group_column, sample_names){
    ##     metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
    ##         read.table(
    ##             file=group_table,row.names=1,header=FALSE,sep="\t",
    ##             colClasses = "character", check.names=FALSE,
    ##             comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
    ##         )
    ##     )
    ## }
    
    # function to export data
    export_data <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    }

    # function to flatten metadata json into a much simpler list
    flatten_list <- function(some_list){
        flat_list <- unlist(some_list)
        flat_list <- gsub("\r","",flat_list)
        flat_list <- gsub("\n","",flat_list)
        flat_list <- gsub("\t","",flat_list)
    }

    get_mapping <- function(mapping="https://gdc-api.nci.nih.gov/files/_mapping"){
        my_call.json <- fromJSON(getURL(mapping))
        default_expand <- my_call.json$expand
        expand_list <- paste(unlist(default_expand), collapse=",")
        default_fields <- my_call.json$fields
        fields_list <- paste(unlist(default_expand), collapse=",")
        my_mapping <- list("expand_list" = expand_list, "fields_list" = fields_list)
        return(my_mapping)
    }


    
    ### MAIN ###
    
    # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
   # create the log file
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension)
    }else{
        log_filename <- paste0(output_filename_prefix,".", output_log_extension)
    }

    # make sure packages in list are installed and sourced
    for (i in package_list){
        if ( is.element(i, installed.packages()[,1]) == FALSE ){ install.packages(i) }
        library(i,character.only = TRUE)
    }

    # get the UUID list from file or vector
    if( list_is_file==TRUE ){
        UUID_list_filename <- UUID_list
        UUID_list <- scan(file=UUID_list, what="character")
    }else{
        UUID_list <- UUID_list
        UUID_list_filename <- "UUID_list"
    }

    # create output filename
    if( output_include_timestamp==TRUE ){
        output_filename <- paste0( output_filename_prefix, ".", my_timestamp, ".", output_filename_extension)
    }else{
        output_filename <- paste0( output_filename_prefix, ".", output_filename_extension)
    }

    # get the mapping
    mapping <- get_mapping()
    
    # get the metadata and merge into a single table
    elapsed_time <- tictoc::tic()
    write(paste("Retireving metadata"), file=log_filename, append=TRUE)
    output_matrix <- matrix()
    column_names <- vector(mode="character")
    first_UUID=TRUE

    for ( UUID in UUID_list) { 

        my_call <- paste0(
            "https://gdc-api.nci.nih.gov/files?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22",
            UUID,
            "%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&defaults&expand=",
            mapping$expand_list,
            "&fields=",
            mapping$fields_list
            )
        my_call.json <- fromJSON(getURL(my_call))
        missing_fields <- my_call.json$warnings # this will be added to an optional debug log
        populated_fields <- my_call.json$data
        flat_metadata_list <- flatten_list(my_call.json$data)

        # loop in here to check to make sure that redundnat metadata keys have the same values -- elements: 
       
        # duplicated elements: flat_metadata_list[ duplicated(names(flat_metadata_list)) ]
        # unique elements: flat_metadata_list[ unique(names(flat_metadata_list)) ]
        # check if values are unique: flat_metadata_list[ names(flat_metadata_list)=="hits.cases.samples.portions.slides.updated_datetime" ]

        # Assuming that duplicated keys have the same values:

        unique_metadata_names <- unique(names(flat_metadata_list))
        unique_metadata_list <- flat_metadata_list[ unique_metadata_names ]
        names(unique_metadata_list) <- unique_metadata_names

        file_name <- unique_metadata_list[ 'hits.file_name' ]
        column_names <- c( column_names, gsub(dl_file_pattern, "", file_name) )

        input_matrix <- as.matrix(unique_metadata_list)
        
        if( first_UUID==TRUE){
            output_matrix <- input_matrix
            first_UUID <- FALSE
        }else{
            if ( identical( rownames(output_matrix),  rownames(input_matrix)) == TRUE ){
                write("rownames identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with cbind) :: ", UUID), file=log_filename, append=TRUE)
                output_matrix <- cbind(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }else{
                write("rownames NOT identical", file=log_filename, append=TRUE)
                write(paste0("Merging (with combine_matrices_by_column/merge) :: ", UUID), file=log_filename, append=TRUE)
                output_matrix <- combine_matrices_by_column(output_matrix, input_matrix)
                my_dim <- dim(output_matrix)
                write(paste0("output matrix dim :: ", my_dim), file=log_filename, append=TRUE)
            }
        }

    }

    # add the UUIDS to the matrix
    ## new_rownames <- c("UUID", rownames(output_matrix))

    if(debug==TRUE){
         TEST.input_matrix <<- input_matrix
         TEST.output_matrix <<- output_matrix
    }

    ## stop()

    ## # add the UUIDs to the output_matrix
    ## ### already there : hits.file_id
    ## output_matrix <- rbind(UUID_list, output_matrix)
    ## rownames(output_matrix)[1] <- "UUID"
    ## if(debug==TRUE){TEST.output_matrix <<- output_matrix}
    
    # label the columns
    colnames(output_matrix) <- column_names
    
    # order columns
    ordered_colnames <- order(colnames(output_matrix))
    output_matrix <- output_matrix[,ordered_colnames]
        
    # order rows
    ordered_rownames <- order(rownames(output_matrix))
    output_matrix <- output_matrix[ordered_rownames,]

    # remove selected rows
    for ( i in rows_to_remove ) {
        output_matrix <- output_matrix[!rownames(output_matrix) %in% c(i), ]
    }
    
    # rotate metadata file -- so it's format required for other package tools
    if( rot_90==TRUE ){
        output_matrix <- rot90(rot90(rot90(output_matrix)))
    }

    # export the metadata data
    export_data(output_matrix,output_filename)
    write(paste0("Wrote files imported from ", UUID_list_filename, " and wrote them to ", output_filename), file=log_filename, append=TRUE)

    elapsed_time <- tictoc::toc()
    elapsed_time <- elapsed_time$toc - elapsed_time$tic
    write(paste("Metadata download time: ", elapsed_time), file=log_filename, append=TRUE)

    write("DONE", file=log_filename, append=TRUE)
                                   
}



      


get_UUIDS_and_metadata_for_repeat_cases <- function(
    metadata_table,
    #output_UUID_list="default",
    #output_metadata="default",
    table_is_file=TRUE,
    output_metadata_prefix= "my_metadata",
    output_metadata_extension="SUBSELECTED.METAdata.txt",
    output_UUID_prefix= "my_uuid_list",
    output_UUID_extension= "SUBSELECTED.UUID_list.txt",
    output_log_extension="log",
    output_include_timestamp=FALSE,
    debug=FALSE
){

    ### SUBS ###

    # function to import data or metadata -- does not alter non-numerical data 
    import_metadata <- function(group_table, my_header=FALSE){ #, group_column, sample_names){
        metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
            read.table(
                file=group_table,row.names=1,header=my_header,sep="\t",
                colClasses = "character", check.names=FALSE,
                comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
            )
        )
    }
    
    # function to export UUIDs
    export_UUIDs <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE, eol="\n")
    }

    export_data <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    }
    
    ### MAIN ###

    # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
    # create default filename for output UUIDs

    # create output filename
    if( output_include_timestamp==TRUE ){
        output_UUID_list <- paste0( output_UUID_prefix, ".", my_timestamp, ".", output_UUID_extension)
    }else{
        output_UUID_list <- paste0( output_UUID_prefix, ".", output_UUID_extension)
    }

    # create log filename
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0( output_UUID_prefix, ".", my_timestamp, ".", output_log_extension)
    }else{
        log_filename <- paste0( output_UUID_prefix, ".", output_log_extension)
    }

    # create default filename for outout metadata
    if( output_include_timestamp==TRUE ){
        output_metadata <- paste0( output_metadata_prefix, ".", my_timestamp, ".", output_metadata_extension)
    }else{
        output_metadata <- paste0( output_metadata_prefix, ".", output_metadata_extension)
    }
    
    # import metadata (as file or from matrix object)
    if( table_is_file ==TRUE ){
        my_metadata <- import_metadata(metadata_table, my_header=TRUE)
    }else{
        my_metadata <- metadata_table
    }
    
    # get list of caseids that appear more than once (assume these to be pairs or groups of samples from the same patient)
    duplicated_cases_bool <- duplicated(my_metadata[ ,'hits.cases.case_id' ])
    duplicated_case_ids <- unique(my_metadata[ duplicated_cases_bool==TRUE, 'hits.cases.case_id' ])

    if ( length(duplicated_case_ids)==0 ){
        write("There are no cases with paired data in this project", file=log_filename)
    }else{

        row_names <- vector(mode="character")
        subset_metadata_matrix <- matrix()
        first_case=TRUE
        for( i in 1:nrow(my_metadata) ){
            if( my_metadata[i,'hits.cases.case_id'] %in% duplicated_case_ids ){
                if ( first_case==TRUE ){
                    subset_metadata_matrix <- my_metadata[i,]
                    if(debug==TRUE){TEST.first_subset <<- subset_metadata_matrix}
                    row_names <- rownames(my_metadata)[i]
                    first_case=FALSE
                }else{
                    subset_metadata_matrix <- rbind( subset_metadata_matrix, my_metadata[i,] )
                    if(debug==TRUE){TEST.subset_metadata_matrix <<- subset_metadata_matrix}
                    row_names <- c( row_names, rownames(my_metadata)[i])
                }
            }
        }
        rownames( subset_metadata_matrix ) <- row_names
        
        ## # order columns
        ## ordered_colnames <- order(colnames(subset_metadata_matrix))
        ## subset_metadata_matrix <- subset_metadata_matrix[,ordered_colnames]
        
        ## order rows
        ordered_rows <- order(subset_metadata_matrix[,'hits.cases.case_id'])
        subset_metadata_matrix <- subset_metadata_matrix[ordered_rows,]
        
        ## export metadata matrix and UUID list as separate files (for use with other tools)
        export_data(subset_metadata_matrix,output_metadata)
    
        duplicated_cases_UUIDs <- as.list( subset_metadata_matrix[,'hits.file_id'] )
        duplicated_cases_UUIDs <- unlist(unname(duplicated_cases_UUIDs))
        export_UUIDs(duplicated_cases_UUIDs,output_UUID_list)
        
    }

}




multi_analysis_wrapper <- function(
    project_list="test_list.txt",
    UUID_list_is_file=TRUE, 
    create_directory_per_project=TRUE,
    debug=FALSE,
    output_filename_prefix="my_log",
    output_log_extension="log",
    output_include_timestamp=FALSE
)
{
    ## # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
    # create the log file
    main_dir <- getwd()
    if( output_include_timestamp==TRUE ){
        log_filename <- paste0(main_dir, "/", output_filename_prefix, ".", my_timestamp ,".", output_log_extension)
    }else{
        log_filename <- paste0(main_dir, "/", output_filename_prefix,".", output_log_extension)
    }

    write("Begin log:", file=log_filename, append=FALSE)
    
    # import the list of projects
    if( UUID_list_is_file ){
        projects <- scan(file=project_list, what="character")
    }else{
        projects <- project_list
    }
    write("Imported UUID list", file=log_filename, append=TRUE)

    # main loop - iterate through each of the projects
    for( project in projects){
        write(paste0("Starting to process: ", project), file=log_filename, append=TRUE)

        # create a directory for each project if that option is selected
        #main_dir <- getwd()
        if( create_directory_per_project==TRUE ){
            if( debug==TRUE ){
                print(paste0("main_dir :: ", main_dir))
                print(paste0("project :: ", project))
            }
            dir.create(file.path(main_dir, project), showWarnings = FALSE)
            setwd(file.path(main_dir, project))
            write(paste0("Created Directory: ", project, " in ", main_dir), file=log_filename, append=TRUE)
        }

        # get the list of UUIDs for a filetype (RNASeq counts) for the project
        get_project_UUIDs(
            projects=project,
            output_filename_prefix=project
        )
        UUID_list_filename <- paste0(project, ".UUID_list.txt")
        write(paste0("Got project UUIDs"), file=log_filename, append=TRUE)
        
        ### Loop to continue only if the UUID list has entries
        my_UUID_list <- scan(UUID_list_filename, what="character")
        if( length(my_UUID_list != 0) ){
            write(paste0("UUID list is longer than 0, proceeding"), file=log_filename, append=TRUE)
        
            ### use the UUIDs to get the abundance data
            download_and_merge_data_from_UUID(
                UUID_list=UUID_list_filename,
                output_filename_prefix=project
            )
            data_filename <- paste0(project, ".DATA.txt")
            write(paste0("Downloaded DATA"), file=log_filename, append=TRUE)

            ### use the UUIDs to get the corresponding metadata
            download_and_merge_metadata_from_UUID(
                UUID_list=UUID_list_filename,
                output_filename_prefix=project
            )
            metadata_filename <- paste0(project, ".METAdata.txt")
            write(paste0("Downloaded METAdata"), file=log_filename, append=TRUE)

            ### create a subselected metadata file and list of UUIDs that correspond to the cases with more than one file (e.g. cancer(s) vs normal(s))
            get_UUIDS_and_metadata_for_repeat_cases(
                metadata_table=metadata_filename,
                output_metadata_prefix=project,
                output_UUID_prefix=project,
            )
            subselected_UUID_list_filename <- paste0(project, ".SUBSELECTED.UUID_list.txt")
            subselected_metadata_filename <- paste0(project, ".SUBSELECTED.METAdata.txt")
            write(paste0("Got susbselected UUID list and METAdata"), file=log_filename, append=TRUE)
            
            ### download data for the subselected metadata and UUID list
            download_and_merge_data_from_UUID(
                UUID_list=subselected_UUID_list_filename,
                output_filename_prefix= paste0(project, ".SUBSELECTED")
            )
            subselected_data_filename <- paste0(project, ".SUBSELECTED.DATA.txt")
            write(paste0("Got subselected DATA"), file=log_filename, append=TRUE)

            ### standard preprocessing to remove low abudance count data and normalized with DESeq
            preprocessing_tool(data_in=subselected_data_filename)
            preprocessed_subselected_data_filename <- paste0(project, ".SUBSELECTED.DATA.txt.DESeq_blind.PREPROCESSED.txt")
            write(paste0("Performed preprocessing"), file=log_filename, append=TRUE)
            
            ### stats (KW - as most have more than two groups, one normal and 1-3 cancer)
            calc_stats(
                data_table=preprocessed_subselected_data_filename,
                metadata_table=subselected_metadata_filename,
                metadata_column="hits.cases.samples.sample_type"
            )
            stat_results_filename <- paste0(project, ".SUBSELECTED.DATA.txt.DESeq_blind.PREPROCESSED.txt.Kruskal-Wallis.hits.cases.samples.sample_type.STATS_RESULTS.txt")
            write(paste0("Performed stats"), file=log_filename, append=TRUE)
            
            ### Calculate raw PCoA
            calculate_pco(file_in=preprocessed_subselected_data_filename)
            raw_PCoA_filename <- paste0(project, ".SUBSELECTED.DATA.txt.DESeq_blind.PREPROCESSED.txt.euclidean.PCoA")
            write(paste0("Calculated raw PCoA"), file=log_filename, append=TRUE)
            ## # static viz of PCoA
            ##     render_calcualted_pcoa(
            ##         PCoA_in=raw_PCoA_filename,
            ##         metadata_table=subselected_metadata_filename,
            ##         use_all_metadata_columns=TRUE
            ##     )
            ## write(paste0("Created static vizualizations of PCoA"), file=log_filename, append=TRUE)    

        }else{

            ### message if there are no UUIDs for teh project and filetype specified
            unlink(UUID_list_filename)
            output_filename <- paste0(project, "._HAS_NO_UUIDs_OF REQUESTED_TYPE.txt")
            write("There are no UUIDs for the requested project and filetype", file=output_filename)
            write(paste0("There are no UUIDs for the requested project and filetype, skipping to the next project"), file=log_filename, append=TRUE)
        }

            
        ### go back to the main directory if the work was completed in a created, project named directory
        if( create_directory_per_project==TRUE ){
            setwd( main_dir ) 
        }
        
        write(paste0("Completed processing of : ", project), file=log_filename, append=TRUE)
    
    }

    write(paste0("Processed all projects; Log END"), file=log_filename, append=TRUE)

}





### Function to calculate cancer ratio from multiple stats files and compile the results into a
calc_expression_ratios <- function(
    list_of_stats_files,
    list_is_file=TRUE,
    output_filename="default",
    numerator_column="Primary Tumor::group_mean",
    denominator_column="Solid Tissue Normal::group_mean",
    fdr_column="Kruskal-Wallis::fdr",
    FDR_threshold=1E-05,
    take_top=NA,
    remove_var_NA_rows=FALSE,
    remove_var_0_rows=FALSE,
    use_filename_prefix_as_column_header=TRUE,
    debug=FALSE,
    output_log_file="default",
    sort_rows=FALSE,
    sort_columns=FALSE
){

    ### SUBS ###
    
    # function to import data or metadata -- does not alter non-numerical data 
    import_metadata <- function(group_table, my_header=FALSE){ #, group_column, sample_names){
        metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
            read.table(
                file=group_table,row.names=1,header=my_header,sep="\t",
                colClasses = "character", check.names=FALSE,
                comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
            )
        )
    }
    
    # function to export data
    export_data <- function(data_object, file_name){
        write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
    }

    # function to combin columns into a matrix ( robust with respect to data type in the matrix (i.e. supports character, numerical, combination ... ) )
    combine_matrices_by_column <- function(matrix1, matrix2, func_order_rows=FALSE, func_order_columns=FALSE, func_debug=FALSE){
        # perform the merge
        comb_matrix<- merge(data.frame(matrix1), data.frame(matrix2), by="row.names", all=TRUE, sort=FALSE) # column ordering ("sort") is controlled by wrapper options
        # undo garbage formatting that merge introduces
        rownames(comb_matrix) <- comb_matrix$Row.names
        comb_matrix$Row.names <- NULL
        ###colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2)) ## I think this can be removed # 12-13-16
        # order columns
        if( func_order_rows==TRUE){
            ordered_rownames <- order(rownames(comb_matrix))
            comb_matrix <- comb_matrix[ordered_rownames,]
        }
        # order rows
        if( func_order_columns==TRUE){
            ordered_colnames <- order(colnames(comb_matrix))
            comb_matrix <- comb_matrix[,ordered_colnames]
        }
        comb_matrix <- as.matrix(comb_matrix)
        return(comb_matrix)
    }
    
    ### MAIN ###

    ## # create a timestamp
    my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))
    
    ## # create log file
    if( identical(output_log_file,"default") == TRUE ){
        output_log_filename <- paste0("calc_expression_ratios_log.", my_timestamp, ".log")
    }else{
        output_log_filename <- output_log_file
    }

    write("Start log", file=output_log_filename, append=FALSE)
    
    ## # import list of stat files
    if( list_is_file==TRUE ){
        stat_files <- scan(file=list_of_stats_files, what="character")
    }else{
        stat_files <- list_of_stats_files
    }
    if(debug==TRUE){TEST.stat_files <<- stat_files}
    
    #output_row_names <- vector(mode="character")
    output_col_names <- vector(mode="character")
    output_matrix <- matrix()
    first_file=TRUE
    
    for( stat_file in stat_files ){

        ## # import stat data from single file
        stat_data <- import_metadata( stat_file, my_header=TRUE )

        if(debug==TRUE){print(paste0(stat_file))}

        ## # a little bit of hacky debugging
        ## # die if the metadata does not contain the numerator column
        if( ( numerator_column %in% colnames(stat_data) == FALSE ) ){
            error_string <- paste0("The numerator field : ( ", numerator_column, " ) is not in file : ( ", stat_file," )")
            write(error_string, file=output_log_filename, append=TRUE)
            stop(error_string)
        }
        ## # die if the metadata does not contain the denominator column
        if( ( denominator_column %in% colnames(stat_data) == FALSE ) ){
            error_string <- paste0("The numerator field : ( ", denominator_column, " ) is not in file : ( ", stat_file, " )")
            write(error_string, file=output_log_filename, append=TRUE)
            stop(error_string)
        }
        
        ## # calculate the ratio
        ratio_vector <- as.numeric(stat_data[ ,numerator_column ]) / as.numeric(stat_data[ ,denominator_column ])
        names(ratio_vector) <- rownames(stat_data)
        if(debug==TRUE){TEST.ratio_vector <<- ratio_vector}

        ## # NOT OPTIONAL - remove rows that don't meet the FDR filter
        ## # determine the values to cull (via FDR threshold)
        fdr_rows_to_remove <-  as.numeric(stat_data[ ,fdr_column]) > FDR_threshold
        # perform culling
        fdr_ratio_vector <- ratio_vector[ fdr_rows_to_remove==FALSE ] 
        if(debug==TRUE){TEST.fdr_ratio_vector <<- fdr_ratio_vector}
        if(debug==TRUE){print(paste0("Length of FDR passing vector: ", length(fdr_ratio_vector)))}

        ## # loop to skip sample if it contained no data that passed the FDR filter
        if( length(fdr_ratio_vector) > 0 ){

            write(paste0("INPUT: ( ", stat_file, " ) HAS ( ", length(fdr_ratio_vector), " ) values that pass the FDR filter"), file=output_log_filename, append=TRUE)
            
            ## convert vector to a matrix
            temp_matrix <- as.matrix(fdr_ratio_vector, ncol=1)
            
            if( first_file==TRUE ){
                ## # start the output matrix
                output_matrix <- temp_matrix
                ## # start the vector with output column names
                if( use_filename_prefix_as_column_header==TRUE ){
                    filename_prefix <- strsplit(stat_file, split="\\.")[[1]][1]
                    output_col_names <- filename_prefix
                }else{
                    output_col_names <- stat_file
                }
                first_file=FALSE
            }else{
                ## # append output matrix
                output_matrix <- combine_matrices_by_column(output_matrix, temp_matrix)
                ## # append output matrix column names
                if( use_filename_prefix_as_column_header==TRUE ){
                    filename_prefix <- strsplit(stat_file, split="\\.")[[1]][1]
                    output_col_names <- c(output_col_names, filename_prefix)
                }else{
                    output_col_names <- c(output_col_names, stat_file)
                }
            }

        }else{
            write(paste0("INPUT: ( ", stat_file, " ) HAS NO ROWS THAT PASS FDR_threshold ( ", FDR_threshold, " )" ), file=output_log_filename, append=TRUE)
        }
        
    }

    ## # Operations performed on the complete output matrix
        
    ## # add column names to the output matrix
    colnames( output_matrix ) <- output_col_names
    
    if( debug==TRUE ){ TEST.output_matrix <<- output_matrix}
    
    ## # calculate var for each row for possible filtering
    row_var <- vector()
    for( i in 1:nrow( output_matrix ) ){
        row_var <- c(row_var, var(as.numeric(output_matrix[i,]), na.rm=TRUE))
    }
    row_var <- as.numeric(row_var)
    
    if(debug==TRUE){TEST.row_var.raw <<- row_var}
    
    ## # (optional) remove rows with var = na
    if( remove_var_NA_rows==TRUE ){
        na_rows_to_remove <-  is.na( row_var )
        ## # perform culling
        output_matrix <- output_matrix[ na_rows_to_remove==FALSE, ] 
        ## # update row_var accordingly
        row_var <- row_var[ na_rows_to_remove==FALSE ]
        log_text <- paste0("INPUT: ( ", stat_file, " ) HAS ( ", length(row_var), " ) ROWS that pass the NA filter" )
        write(log_text, file=output_log_filename, append=TRUE)
        print(log_text)
    }
    
    if(debug==TRUE){TEST.row_var.sans_na <<- row_var}
    if( debug==TRUE ){ TEST.output_matrix.sans_na <<- output_matrix}
    
    ## # (optional) remove rows with var = 0
    if( remove_var_0_rows==TRUE ){
        zero_rows_to_remove <- row_var==0
        if(debug==TRUE){
            print(paste0("Number rows to remove with var = 0 : ", length(zero_rows_to_remove)))
            TEST.zero_rows_to_remove <<- zero_rows_to_remove
        }
        if( length(TEST.zero_rows_to_remove) > 0 ){
            output_matrix <- output_matrix[ zero_rows_to_remove==FALSE, ]
            log_text <- paste0("INPUT HAS ( ", nrow(output_matrix), " ) ROWS that pass the NA filter" ) 
            write(log_text, file=output_log_filename, append=TRUE)
            print(log_text)
        }else{
            log_text <- paste0("INPUT HAS NO ROWS that pass the NA filter" ) 
            write(log_text, file=output_log_filename, append=TRUE)
            print(log_text)
        }
    }

    if( debug==TRUE ){ TEST.output_matrix.sans_na.sans_0 <<- output_matrix}
    
    ## # order columns
    if( sort_columns == TRUE){
        ordered_colnames <- order(colnames(output_matrix))
        output_matrix <- output_matrix[,ordered_colnames]
    }
    
    ## # order rows
    if( sort_rows == TRUE){
        ordered_rownames <- order(rownames(output_matrix))
        output_matrix <- output_matrix[ordered_rownames,]
    }
    
    ## # create filename for output
    if( identical(output_filename, "default") ){
        output_matrix_filename <- paste0(list_of_stats_files, ".RATIOS.txt")
    }else{
        output_matrix_filename <- output_filename 
    }
    
    ## # export data
    export_data( output_matrix, output_matrix_filename )
    
}










## subselect_data_by_list <- function(
##     data_table,
##     UUID_list,
##     data_is_file=TRUE,
##     UUID_list_is_file=TRUE,
##     UUID_rowname="hits.file_id",
##     output_filename="default"
##     ### Needs the metadata too, just do another dl for now
    
## )
## {
##     ### SUBS ###

##     # function to import data or metadata -- does not alter non-numerical data 
##     import_metadata <- function(group_table, my_header=FALSE){ #, group_column, sample_names){
##         metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
##             read.table(
##                 file=group_table,row.names=1,header=my_header,sep="\t",
##                 colClasses = "character", check.names=FALSE,
##                 comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
##             )
##         )
##     }
   
##     export_data <- function(data_object, file_name){
##         write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
##     }
    
##     ### MAIN ###

##     # load the data
##     if( data_is_file==TRUE ){
##         my_data <- import_metadata(data_table, my_header=TRUE)
##     }else{
##         my_data <- data_table
##     }

##     # load list of UUIDs for subselection (e.g. produced by get_UUIDS_and_metadata_for_repeat_cases )
##     if( UUID_list_is_file==TRUE){
##         my_UUID_list <- scan(file=UUID_list, what="character")
##     }else{
##         my_UUID_list <- UUID_list
##     }

##     # create a timestamp
##     my_timestamp <- gsub(":", "-",(gsub("__", "_", (gsub(" ", "_",date())))))

##     # create filename for output
##     if( identical(output_filename, "default") ){
##         output_filename <- paste0("SUBSELECTED_DATA.", my_timestamp, ".txt")
##     }
    
##     subselected_data_matrix <- matrix()
##     first_column=TRUE
##     row_names <- vector(mode="character")
##     for( i in ncol(my_data) ){
        
##         if( first_column==TRUE){
##             if( my_UUID_list[i] %in% my_data[ UUID_rowname, ]){
##                 my_row <-  which( my_data[  UUID_rowname, ] == my_UUID_list[i] )  
##                 subselected_data_matrix <- my_data[ , which( my_data[  UUID_rowname, ] == my_UUID_list[i] ), drop=FALSE]
##                first_column=FALSE
##            }
##         }else{
##             subselected_data_matrix <- cbind(subselected_data_matrix,my_data[ , which( my_data[  UUID_rowname, ] == my_UUID_list[i] ), drop=FALSE])

##         }
           
##     }

##     export_data(subselected_data_matrix,output_filename)
        
## }
    
