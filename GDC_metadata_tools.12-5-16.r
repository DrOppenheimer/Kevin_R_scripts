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


combine_matrices_by_column <- function(matrix1, matrix2, func_order_rows=FALSE, func_order_columns=FALSE, func_debug=FALSE){

    # perform the merge
    comb_matrix<- merge(data.frame(matrix1), data.frame(matrix2), by="row.names", all=TRUE)
    if(func_debug==TRUE){
        print("Made it here (3.6.1)")
        print(paste("MATRIX_1", dim(matrix1)))
        print(paste("MATRIX_2",dim(matrix2)))
        print(paste("MATRIX_C",dim(comb_matrix)))
        #print(colnames(matrix1))
        #print(colnames(matrix2))
        matrix3 <<- comb_matrix
    }
    
    # undo garbage formatting that merge introduces
    rownames(comb_matrix) <- comb_matrix$Row.names
    comb_matrix$Row.names <- NULL

    matrix4 <<- comb_matrix

    #if(func_debug==TRUE){print(paste("MATRIX DIM:", dim(comb_matrix)))}
    colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
    #if(func_debug==TRUE){print("Made it here (3.6.2)")}
    
    # order columns
    if( func_order_rows==TRUE){
        ordered_rownames <- order(rownames(comb_matrix))
        comb_matrix <- comb_matrix[ordered_rownames,]
    }
    #if(func_debug==TRUE){print("Made it here (3.6.3)")}

    # order rows
    if( func_order_columns==TRUE){
        ordered_colnames <- order(colnames(comb_matrix))
        comb_matrix <- comb_matrix[,ordered_colnames]
    }
    #if(func_debug==TRUE){print("Made it here (3.6.4)")}

    #if(func_debug==TRUE){ matrix5 <<- comb_matrix }

    comb_matrix <- as.matrix(comb_matrix)
    
    return(comb_matrix)
}


get_project_UUIDs <- function(
    projects,
    data_type="HTSeq - Counts",
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
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension, ".txt")
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
        my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22", data_type_url,"%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")

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







download_and_merge_data_from_UUID <- function(
    UUID_list,
    #output_prefix = "my_merged_DATA",
    list_is_file=TRUE,
    package_list=c("urltools","RJSONIO","RCurl", "hash", "tictoc"),
    rows_to_remove=c("__alignment_not_unique","__ambiguous","__no_feature","__not_aligned","__too_low_aQual"),
    dl_file_pattern=".htseq.counts.gz$",
    cleanup=TRUE,
    #log="default",
    debug=FALSE,
    output_filename_prefix= "my_data",
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
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension, ".txt")
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
        log_filename <- paste0(output_filename_prefix, ".", my_timestamp ,".", output_log_extension, ".txt")
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
   # output_log_extension="log",
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
        
    # order rows
    ordered_rows <- order(subset_metadata_matrix[,'hits.cases.case_id'])
    subset_metadata_matrix <- subset_metadata_matrix[ordered_rows,]

    # export metadata matrix and UUID list as separate files (for use with other tools)
    export_data(subset_metadata_matrix,output_metadata)
    
    duplicated_cases_UUIDs <- as.list( subset_metadata_matrix[,'hits.file_id'] )
    duplicated_cases_UUIDs <- unlist(unname(duplicated_cases_UUIDs))
    export_UUIDs(duplicated_cases_UUIDs,output_UUID_list)

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
    
