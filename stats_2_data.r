stats_2_data <<- function(stats_matrix)
    {
        # remove columns that contain stats
        stats_matrix <- stats_matrix[ , -grep( "::group_mean$", colnames(stats_matrix)) ]
        stats_matrix <- stats_matrix[ , -grep( "::group_sd$", colnames(stats_matrix)) ]
        stats_matrix <- stats_matrix[ , -grep( "::stat$", colnames(stats_matrix)) ]
        stats_matrix <- stats_matrix[ , -grep( "::p$", colnames(stats_matrix)) ]
        stats_matrix <- stats_matrix[ , -grep( "::fdr$", colnames(stats_matrix)) ]

        # remove group name appended to data name
        new_colnames <- vector(mode="character")
        for (i in 1:length(colnames(stats_matrix))){
            new_colnames <- c(new_colnames, strsplit(colnames(stats_matrix)[i], split="::")[[1]][1]  )
        }
        colnames(stats_matrix) <- new_colnames

        # return the matrix that only contains the data
        return(stats_matrix)
  
    }
