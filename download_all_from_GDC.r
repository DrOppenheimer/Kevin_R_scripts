download_all_from_GDC <- function(projects){ ### 11-29-16
    for (p in projects) {
        my_call <- paste0("https%3a%2f%2fgdc-api.nci.nih.gov%2ffiles%3ffields%3dfile_id%26size%3d99999%26pretty%3dtrue%26filters%3d%7b%0d%0a++++%22op%22%3a%22and%22%2c%0d%0a++++%22content%22%3a%5b%7b%0d%0a++++++++%22op%22%3a%22in%22%2c%0d%0a++++++++%22content%22%3a%7b%0d%0a++++++++++++%22field%22%3a%22analysis.workflow_type%22%2c%0d%0a++++++++++++%22value%22%3a%5b%22HTSeq+-+Counts%22%5d%0d%0a++++++++%7d%0d%0a++++%7d%2c%7b%0d%0a++++++++%22op%22%3a%22in%22%2c%0d%0a++++++++%22content%22%3a%7b%0d%0a++++++++++++%22field%22%3a%22files.data_format%22%2c%0d%0a++++++++++++%22value%22%3a%5b%22TXT%22%5d%0d%0a++++++++%7d%0d%0a++++%7d%2c%7b%0d%0a++++++++%22op%22%3a%22%3d%22%2c%0d%0a++++++++%22content%22%3a%7b%0d%0a++++++++++++%22field%22%3a%22cases.case_id%22%2c%0d%0a++++++++++++%22value%22%3a%5b%22", p, "%22%5d%0d%0a++++++++%7d%0d%0a++++%7d%5d%0d%0a%7d")
        my_call.json <- fromJSON(getURL(my_call))
        UUID.list <- unlist(my_call.json$data$hits)
        for(j in UUID.list) {
            print(paste0(j, ": ", j))
            system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                         j,
                         "'",
                         sep=""))
        }
    }
}
