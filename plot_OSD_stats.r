plot_OSD_stats <- function(file_in, expected_hds_per_pg=3, save_table=FALSE, debug=FALSE){

    # package with hist function that forces number of bins (does not default to "pretty")
    library(GLDEX)
    
    # for (i in dir(pattern=".json$")){plot_OSD_usage(file_in=i, save_table=TRUE)}
    print(paste("Processing ", file_in))
    
    library(RJSONIO)
    # import json
    # file_in = "ceph_pg_and_osd-2015-12-13.json"
    perf_stats.json <<- fromJSON(file_in)

    # flatten json to table
    num_records <- length(perf_stats.json)/2
    perf_stats.matrix <- matrix #(NA, num_records*3,3)
    #colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
    #rownames(perf_stats.matrix) <- rep("row",nrow(perf_stats.matrix))
    my_osd_index = 0
    for ( i in 1:num_records ){
        pg <- perf_stats.json[ (i*2)-1 ]
        pg_record <- perf_stats.json[ (i*2) ]  
        
        if ( length(pg_record$osds) != 3 ){ warning(paste("Page group", pg, "has", length(pg_record$osds), "OSDs, not 3")) }
        
        for ( j in 1:length(pg_record$osds) ){

            if(debug==TRUE){pg_record.test <<- pg_record}
            
            #handle case when page group is there, but has no OSDs (or at least no data for them)
            if( length(pg_record$osds) == 0){
                if(debug==TRUE){print(paste("num_osds_in_pg:", length(pg_record$osds)))}
                ## my_osd_index <- my_osd_index + 1
                ## my_osd       <- pg_record$osds[[j]]['osd']
                ## my_pfull     <- pg_record$osds[[j]]['percent_full']
                ## t(sapply(1:perf_stats.matrix, function(n) c(my_osd_index, my_osd, my_pfull)))
            }else{
                if(debug==TRUE){print(paste("num_osds_in_pg:", length(pg_record$osds)))}
                my_osd_index <- my_osd_index + 1
                my_osd       <- pg_record$osds[[j]]['osd']
                my_pfull     <- pg_record$osds[[j]]['percent_full']
                t(sapply(1:perf_stats.matrix, function(n) c(my_osd_index, my_osd, my_pfull)))
            }
       
	}
    }

    # label the first table
    colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
    my_rownames <- ""
    for (i in 1:nrow(perf_stats.matrix)){
        my_rownames <- c(my_rownames, paste("osd_index", i, sep=""))
    }
    if(debug==TRUE){my_rownames.test <<- my_rownames}
    rownames(perf_stats.matrix) <- my_rownames
    
    if( debug==TRUE ){
        perf_stats.matrix.test <<- perf_stats.matrix
    }

    if(debug==TRUE){
       stop("You asked me to stop")
    }
    
    num_pgs <- length(unique(rownames(perf_stats.matrix)))
    num_osds <- length(unique(perf_stats.matrix[,'osd']))
    
    pgs_per_osd <- num_pgs/num_osds
    
    pgs_per_osd.matrix <- matrix(NA, num_osds,2)
    colnames(pgs_per_osd.matrix) <- c("num_pgs","percent_full")
    rownames(pgs_per_osd.matrix) <- unique(perf_stats.matrix[,'osd'])

    if( debug==TRUE ){
        pgs_per_osd.matrix.test <<- pgs_per_osd.matrix
        pgs_per_osd.matrix.test <<- pgs_per_osd.matrix
        print(paste("num_osds:", num_osds))
    }

    # Generate second table (OSDS)
    for ( i in 1:num_osds){
        osd_stats <- as.matrix(perf_stats.matrix[which(perf_stats.matrix[,'osd']==rownames(pgs_per_osd.matrix)[i]),])
        if( debug==TRUE ){
            osd_stats.test <<- osd_stats
        }
        print(osd_stats)
        if( is.na(osd_stats[1,'percent_full'])==TRUE ){
            print(paste("osd: ", rownames(pgs_per_osd.matrix)[i], " is NA - value set to 1000"))
            osd_stats[,'percent_full'] <- 1000
        }
        osd.num_pgs <- nrow(osd_stats)
        osd.percent_full <- osd_stats[1,'percent_full']
        if ( sd(osd_stats[,'percent_full']) != 0 ){
            print(paste("osd: ", rownames(pgs_per_osd.matrix)[i], " pg % full values do not match"))
        }
        pgs_per_osd.matrix[i,1] <- osd.num_pgs
        pgs_per_osd.matrix[i,2] <- osd.percent_full
    } 

        
        
        ##                                 # write (new) table by osd's
        ## osd_table_filename <- paste(file_in, ".osd_stats.txt", sep="")
        ## writeLines( c( paste("# num_pgs", num_pgs, sep="\t"),
        ##               paste("# num_osds", num_osds, sep="\t"),
        ##               paste("# stats per pg:"),
        ##               ), con = osd_table_filename, sep = "\n")
        ## write.table(pgs_per_osd.matrix, file = osd_table_filename, col.names=NA, sep="\t", quote=FALSE, append=TRUE)

       
        
	# write original table
    png ( filename= paste(file_in, ".disk_usage.png", sep=""), width = 1200, height = 1000)
    if(save_table==TRUE){
        write.table(perf_stats.matrix, file = paste(file_in,".perf_stats.matrix.txt", sep=""), col.names=NA, sep="\t", quote=FALSE)
    }
    # create hist
    my_mean <- mean(as.numeric(perf_stats.matrix[,"percent_full"]), na.rm=TRUE)
    num_na <- sum(is.na(perf_stats.matrix[,"percent_full"]))
    expected_num_hd <- num_records*3
    real_num_hd <- expected_num_hd - num_na
    my_mean <- mean(as.numeric(perf_stats.matrix[,"percent_full"]), na.rm=TRUE)
    num_na <- sum(is.na(perf_stats.matrix[,"percent_full"]))
    expected_num_hd <- num_records*3
    real_num_hd <- expected_num_hd - num_na
    histsu(as.numeric(perf_stats.matrix[,"percent_full"]), main=paste(file_in, ":: Percent Full by OSD"), xlab=paste("Percent Full (red line = mean ::", round(my_mean, digits=2), ")"), ylab="num OSDs", breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100), cex=2, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2, mgp=c(2, 0.5, 0)) # GDEX hist # cex.label does not work
    # add line with avg OSD used ()
    abline(v=my_mean, col="red", lwd=10)
    dev.off()
}
