GDC_metadata_download <- function( id_list="my_id_list", my_rot="no", debug=FALSE, verbose=FALSE)
    
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



# mapping
# https://gdc-api.nci.nih.gov/files/_mapping
# https://gdc-docs.nci.nih.gov/API/Users_Guide/Search_and_Retrieval/#constructing-queries


# original example from Michael
# https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%2207218202-2cd3-4db1-93e7-071879e36f27%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=demographic,diagnoses,diagnoses.treatments,family_histories,exposures

# Project endpoint
# curl 'https://gdc-api.nci.nih.gov/projects?from=1&size=2&sort=project.project_id:asc&pretty=true'



# Files endpoint
# curl 'https://gdc-api.nci.nih.gov/files?from=1&size=2&sort=file_size:asc&pretty=true'

# Cases endpoint
# curl 'https://gdc-api.nci.nih.gov/cases?filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22submitter_id%22%2C%22value%22%3A%5B%22TCGA-BH-A0EA%22%5D%7D%7D%5D%7D%0A%0A&pretty=true'
https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%2207218202-2cd3-4db1-93e7-071879e36f27%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=project.program




# Annotations endpoint
# curl 'https://gdc-api.nci.nih.gov/annotations?from=1&size=2&pretty=true'
# example # https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22  ID  %22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=demographic,diagnoses,diagnoses.treatments,family_histories,exposures







### From Michael F. 5-25-16
## Hi Kevin.  I figured out a way to do this, but it actually takes a couple of steps unfortunately.  I have asked around a bit and confirmed that there is probably not a more elegant way.  If others want to traverse this path commonly we may want to consider implementing a simpler method here.  I added the json files just to show you how they are set up.

## Michael



## 1.  First you will want to get the case ID associated with this bam file.
## curl 'https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%2207218202-2cd3-4db1-93e7-071879e36f27%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id'

## {
##    "op" : "=" ,
##    "content" : {
##        "field" : "files.file_id" ,
##        "value" : [ "07218202-2cd3-4db1-93e7-071879e36f27" ]
##    }
## }




## 2.  Once you have the case ID you make another API query to get the clinical file UUID
## curl 'https://gdc-api.nci.nih.gov/files?filters=%7b%0d%0a++++%22op%22%3a+%22and%22%2c%0d%0a++++%22content%22%3a+%5b%0d%0a++++++++%7b%0d%0a++++++++++++%22op%22%3a+%22%3d%22%2c%0d%0a++++++++++++%22content%22%3a+%7b%0d%0a++++++++++++++++%22field%22%3a+%22cases.case_id%22%2c%0d%0a++++++++++++++++%22value%22%3a+%5b%0d%0a++++++++++++++++++++%22ce8612ab-3149-4a6a-b424-29c0c21c9b8b%22%0d%0a++++++++++++++++%5d%0d%0a++++++++++++%7d%0d%0a++++++++%7d%2c%0d%0a++++++++%7b%0d%0a++++++++++++%22op%22%3a+%22%3d%22%2c%0d%0a++++++++++++%22content%22%3a+%7b%0d%0a++++++++++++++++%22field%22%3a+%22data_category%22%2c%0d%0a++++++++++++++++%22value%22%3a+%5b%0d%0a++++++++++++++++++++%22Clinical%22%0d%0a++++++++++++++++%5d%0d%0a++++++++++++%7d%0d%0a++++++++%7d%0d%0a++++%5d%0d%0a%7d%0d%0a&pretty=true'

## {
##     "op": "and",
##     "content": [
##         {
##             "op": "=",
##             "content": {
##                 "field": "cases.case_id",
##                 "value": [
##                     "ce8612ab-3149-4a6a-b424-29c0c21c9b8b"
##                 ]
##             }
##         },
##         {
##             "op": "=",
##             "content": {
##                 "field": "data_category",
##                 "value": [
##                     "Clinical"
##                 ]
##             }
##         }
##     ]
## }




## 3.  Download the Clinical XML using the clinical file id
## curl 'https://gdc-api.nci.nih.gov/data/2fa187dc-3e54-4236-b98f-9117f41d903f'



#curl 'https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22cases.project.project_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22TARGET-AML%22+%5d%0d%0a+++%7d%0d%0a%7d&fields=sample_ids&pretty=true&size=1000'

