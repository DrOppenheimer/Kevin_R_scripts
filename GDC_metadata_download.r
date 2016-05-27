GDC_metadata_download <- function( id_list="my_id_list", my_rot="no")
    
{                       
    ### MAIN ###
    ###### load the neccessary packages
    if ( is.element("RJSONIO", installed.packages()[,1]) == FALSE ){ install.packages("RJSONIO") }
    if ( is.element("RCurl", installed.packages()[,1]) == FALSE ){ install.packages("RCurl") }
    if ( is.element("matlab", installed.packages()[,1]) == FALSE ){ install.packages("matlab") }    
    library(RJSONIO)
    library(RCurl)
    library(matlab)
    
    my_ids <- flatten_list(as.list(scan(file=id_list, what="character")))
    my_keys <- list(type="character")
    
    # read through once to get the keys
    for ( i in 1:length(my_ids) ){
        print(paste("First read: ", i))
        my_call <- gsub(" ", "", paste("https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22", my_ids[i], "%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=demographic,diagnoses,diagnoses.treatments,family_histories,exposures"))

        my_call.json <- fromJSON(getURL(my_call))

        my_call.list <- flatten_list(my_call.json)

        my_keys <- unique(c(names(my_call.list), my_keys))

        #print(length(my_keys))
    }

    # Read second time to generate the metadata matrix
    my_metadata_matrix <- matrix(NA, length(my_keys), length(my_ids))
    rownames(my_metadata_matrix) <- my_keys
    colnames(my_metadata_matrix) <- my_ids
    

    for ( i in 1:length(my_ids) ){
        print(paste("Second read: ", i))
        my_call <- gsub(" ", "", paste("https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22", my_ids[i], "%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=demographic,diagnoses,diagnoses.treatments,family_histories,exposures"))

        my_call.json <- fromJSON(getURL(my_call))

        my_call.list <- flatten_list(my_call.json)
             
        for ( j in 1:length(my_call.list) ){
            #print( my_call.list[j])
            my_metadata_matrix[ names(my_call.list[j]), my_ids[i] ] <- my_call.list[j]
        }

    }

    output_name <- gsub(" ", "", paste(id_list, ".GDC_metadata.txt"))

    if( identical(my_rot, "yes")==TRUE ){
        my_metadata_matrix <- rot90(rot90(rot90(my_metadata_matrix)))
    }


    export_data(my_metadata_matrix, output_name)
    
}

### SUBS ###                    
export_data <- function(data_object, file_name){
    write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}

flatten_list <- function(some_list){
    flat_list <- unlist(some_list)
    flat_list <- gsub("\r","",flat_list)
    flat_list <- gsub("\n","",flat_list)
    flat_list <- gsub("\t","",flat_list)
}
