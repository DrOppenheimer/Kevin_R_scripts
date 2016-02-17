plot_HD_usage <- function(file_in, expected_hds_per_pg=3, save_table=FALSE, debug=FALSE){

    # package with hist function that forces number of bins (does not default to "pretty")
    library(GLDEX)
    
    # for (i in dir(pattern=".json$")){plot_HD_usage(file_in=i, save_table=TRUE)}
    print(paste("Processing ", file_in))
    
    library(RJSONIO)
    # import json
    # file_in = "ceph_pg_and_osd-2015-12-13.json"
    perf_stats.json <<- fromJSON(file_in)

    # flatten json to table
    num_records <- length(perf_stats.json)/2
    perf_stats.matrix <- matrix(NA, num_records*3,3)
    colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
    rownames(perf_stats.matrix) <- rep("row",nrow(perf_stats.matrix))
    for ( i in 1:num_records ){
        pg <- perf_stats.json[ (i*2)-1 ]
        pg_record <- perf_stats.json[ (i*2) ]  
        
        if ( length(pg_record$osds) != 3 ){ warning(paste("Page group", pg, "has", length(pg_record$osds), "HDs, not 3")) }
        
        for ( j in 0:(length(pg_record$osds)-1) ){

            # handle case when page group is there, but has no HDs (or at least no data for them)
            if( length(pg_record$osds) == 0){
                if(debug==TRUE){print(paste("num_hds_in_pg:", length(pg_record$osds)))}
                perf_stats.matrix[(i*length(pg_record$osds))-j, 1] <- "Empty pg? (no data)"	
                perf_stats.matrix[(i*length(pg_record$osds))-j, 2] <- "Empty pg? (no data)"
                perf_stats.matrix[(i*length(pg_record$osds))-j, 3] <- "Empty pg? (no data)"
            }else{
                if(debug==TRUE){print(paste("num_hds_in_pg:", length(pg_record$osds)))}
                rownames(perf_stats.matrix)[ (i*length(pg_record$osds))-j ] <- pg
                perf_stats.matrix[(i*length(pg_record$osds))-j, 1] <- i	
                perf_stats.matrix[(i*length(pg_record$osds))-j, 2] <- pg_record$osds[[j+1]]["osd"]
                perf_stats.matrix[(i*length(pg_record$osds))-j, 3] <- as.numeric( gsub("%", "", pg_record$osds[[j+1]]["percent_full"]) )
            }
                
		## rownames(perf_stats.matrix)[ (i*3)-2 ] <- pg
		## perf_stats.matrix[(i*3)-2, 1] <- i	
		## perf_stats.matrix[(i*3)-2, 2] <- pg_record$osds[[1]]["osd"]
		## perf_stats.matrix[(i*3)-2, 3] <- as.numeric( gsub("%", "", pg_record$osds[[1]]["percent_full"]) )

		## rownames(perf_stats.matrix)[ (i*3)-1 ] <- pg
		## perf_stats.matrix[(i*3)-1, 1] <- i
		## perf_stats.matrix[(i*3)-1, 2] <- pg_record$osds[[2]]["osd"]
		## perf_stats.matrix[(i*3)-1, 3] <- as.numeric( gsub("%", "", pg_record$osds[[2]]["percent_full"]) )

		## rownames(perf_stats.matrix)[ (i*3) ] <- pg
		## perf_stats.matrix[(i*3), 1] <- i
		## perf_stats.matrix[(i*3), 2] <- pg_record$osds[[3]]["osd"]
		## perf_stats.matrix[(i*3), 3] <- as.numeric( gsub("%", "", pg_record$osds[[3]]["percent_full"]) )

	}
	# write file out
        png ( filename= paste(file_in, ".disk_usage.png", sep=""), width = 1200, height = 1000)
        if(save_table==TRUE){
            write.table(perf_stats.matrix, file = paste(file_in,".perf_stats.matrix.txt", sep=""), col.names=NA, sep="\t", quote=FALSE)
        }
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
    histsu(as.numeric(perf_stats.matrix[,"percent_full"]), main=paste(file_in, ":: Percent Full by HD"), xlab=paste("Percent Full (red line = mean ::", round(my_mean, digits=2), ")"), ylab="num HDs", breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100), cex=2, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2, mgp=c(2, 0.5, 0)) # GDEX hist # cex.label does not work
	# add line with avg HD used ()
    abline(v=my_mean, col="red", lwd=10)
    dev.off()
}

