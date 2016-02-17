plot_OSD_stats <- function(file_in, expected_hds_per_pg=3, save_table=FALSE, debug=FALSE){

    # package with hist function that forces number of bins (does not default to "pretty")
    library(GLDEX)
    
    # for (i in dir(pattern=".json$")){plot_OSD_usage(file_in=i, save_table=TRUE)}
    print(paste("Processing ", file_in))
    
    library(RJSONIO)
    # import json
    # file_in = "ceph_pg_and_osd-2015-12-13.json"
    perf_stats.json <- fromJSON(file_in)
    #if(debug==TRUE){perf_stats.json.test <<- perf_stats.json}

   # figure out number of rows for table
    num_records <- length(perf_stats.json)/2
    num_table_rows <- 0
    for  ( i in 1:num_records ){
        pg <- perf_stats.json[ (i*2)-1 ]
        pg_record <- perf_stats.json[ (i*2) ]  
        for ( j in 1:(length(pg_record$osds)) ){
            num_table_rows <- num_table_rows + 1
        }
    }
    #if(debug==TRUE){print(paste("num rows: ", num_table_rows))}
    if(debug==TRUE){print(paste("made it here 1"))}
    
    # flatten json to table
    perf_stats.matrix <- matrix(NA, num_table_rows, 3)
    #if(debug==TRUE){ print(paste("(1) class(perf_stats.matrix): ",class(perf_stats.matrix))) }
    colnames(perf_stats.matrix) <- c("pg","osd","percent_full")
    rownames(perf_stats.matrix) <- rep("row", num_table_rows)
    output_line = 0
    for ( i in 1:num_records ){
        #if(debug==TRUE){print(paste("i:", i))}
        pg <- perf_stats.json[ (i*2)-1 ]
        #if(debug==TRUE){ print(pg) }
        pg_record <- perf_stats.json[ (i*2) ]  
        #if(debug==TRUE){pg_record.test <<- pg_record}
        
        if( length(pg_record$osds)==0 ){
            warning(paste("Page group", pg, "has", length(pg_record$osds), "OSDs, not 3"))
        }else{
        
            for ( j in 1:length(pg_record$osds) ){
                output_line <- output_line + 1
                
                rownames(perf_stats.matrix)[ output_line ] <- paste("row_", output_line, sep="" )
                perf_stats.matrix[output_line, 1] <- as.character(pg)
                perf_stats.matrix[output_line, 2] <- as.character( pg_record$osds[[j]]["osd"] )
                perf_stats.matrix[output_line, 3] <- as.numeric( gsub("%", "", pg_record$osds[[j]]["percent_full"]) )
              
            }
        }
    }

    #if(debug==TRUE){perf_stats.matrix.test <<- perf_stats.matrix}
    if(debug==TRUE){print(paste("made it here 2"))}
    
    num_pgs <- length(unique(rownames(perf_stats.matrix)))
    num_osds <- length(unique(perf_stats.matrix[,'osd']))
    
    pgs_per_osd <- num_pgs/num_osds
    
    pgs_per_osd.matrix <- matrix(NA, num_osds,2)
    colnames(pgs_per_osd.matrix) <- c("num_pgs","percent_full")
    rownames(pgs_per_osd.matrix) <- unique(perf_stats.matrix[,'osd']) # was osd
    
    #if( debug==TRUE ){
    #    pgs_per_osd.matrix.test <<- pgs_per_osd.matrix
    #    pgs_per_osd.matrix.test <<- pgs_per_osd.matrix
    #    print(paste("num_osds:", num_osds))
    #}

    if(debug==TRUE){print(paste("made it here 3"))}
    
    # Generate second table (OSDS)
    for ( i in 1:num_osds){
        osd_stats <- as.matrix(perf_stats.matrix[which(perf_stats.matrix[,'osd']==rownames(pgs_per_osd.matrix)[i]),])

        if( debug==TRUE ){
            osd_stats.test <<- osd_stats
            print(i)
            print(osd_stats)
        }
        
        if( is.na(osd_stats[1,'percent_full'])==TRUE ){
            print(paste("osd: ", rownames(pgs_per_osd.matrix)[i], " is NA - value set to NA"))
            osd_stats[,'percent_full'] <- NA
        }

        osd.num_pgs <- nrow(osd_stats)
        osd.percent_full <- osd_stats[1,'percent_full']

        #if ( sd(osd_stats[,'percent_full']) != 0 ){
        #    print(paste("osd: ", rownames(pgs_per_osd.matrix)[i], " pg % full values do not match"))
        #}

        pgs_per_osd.matrix[i,1] <- osd.num_pgs
        pgs_per_osd.matrix[i,2] <- osd.percent_full
    } 

    if(debug==TRUE){print(paste("made it here 4"))}  
        
    # write (new) table by osd's
    osd_table_filename <- paste(file_in, ".osd_usage_stats.txt", sep="")
    writeLines(
        c(
            paste("# num_pgs", num_pgs, sep="\t"),
            paste("# num_osds", num_osds, sep="\t"),
            paste("# stats_per_pg:"),
            paste("# OSD\tnum_pgs\tpercent_full")
        ),
        con = osd_table_filename, sep = "\n"
    )

    pgs_per_osd.matrix.test <<- pgs_per_osd.matrix

    #data_matrix <- data_matrix[,order(colnames(data_matrix))]
    pgs_per_osd.matrix <- pgs_per_osd.matrix[order(as.numeric(rownames(pgs_per_osd.matrix))),]

    # write the OSD based table
    write.table(pgs_per_osd.matrix, file = , col.names=FALSE, sep="\t", quote=FALSE, append=TRUE)

    # write original table
    if(save_table==TRUE){
        write.table(perf_stats.matrix, file = paste(file_in,".pg_usage_stats.txt", sep=""), col.names=NA, sep="\t", quote=FALSE)
    }

    if(debug==TRUE){print(paste("made it here 5"))}
                                      
    # create original (pg based ) image
    png ( filename= paste(file_in, ".PG_usage_hist.png", sep=""), width = 1200, height = 1000)
    my_mean <- mean(as.numeric(perf_stats.matrix[,"percent_full"]), na.rm=TRUE)
    num_na <- sum(is.na(perf_stats.matrix[,"percent_full"]))
    histsu(as.numeric(perf_stats.matrix[,"percent_full"]), main=paste(file_in, ":: Percent Full by Placement Group"), xlab=paste("Percent Full (red line = mean ::", round(my_mean, digits=2), ")"), ylab="num Placement Groups", breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100), cex=2, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2, mgp=c(2, 0.5, 0)) # GDEX hist # cex.label does not work
    # add line with avg OSD used ()
    abline(v=my_mean, col="red", lwd=10)
    dev.off()

    if(debug==TRUE){print(paste("made it here 6"))}

    # Create image from the OSD stats
    png ( filename= paste(file_in, ".OSD_usage_hist.png", sep=""), width = 1200, height = 1000)
    my_mean <- mean(as.numeric(pgs_per_osd.matrix[,"percent_full"]), na.rm=TRUE)
    num_na <- sum(is.na(pgs_per_osd.matrix[,"percent_full"]))
    histsu(as.numeric(pgs_per_osd.matrix[,"percent_full"]), main=paste(file_in, ":: Percent Full by OSD"), xlab=paste("Percent Full (red line = mean ::", round(my_mean, digits=2), ")"), ylab="num OSDs", breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100), cex=2, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2, mgp=c(2, 0.5, 0)) # GDEX hist # cex.label does not work
    # add line with avg % full ()
    abline(v=my_mean, col="red", lwd=10)
    dev.off()

    if(debug==TRUE){print(paste("made it here 7"))}
    
                                        # Create hist of PG/OSD
    png ( filename= paste(file_in, ".OSD_per_PG_hist.png", sep=""), width = 1200, height = 1000)
    my_mean <- mean(as.numeric(pgs_per_osd.matrix[,"num_pgs"]), na.rm=TRUE)
    num_na <- sum(is.na(pgs_per_osd.matrix[,"num_pgs"]))
    histsu(as.numeric(pgs_per_osd.matrix[,"num_pgs"]), main=paste(file_in, ":: Num PGs by OSD"), xlab=paste("Num PGs (red line = mean ::", round(my_mean, digits=2), ")"), ylab="num OSDs", breaks=20, cex=2, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2, mgp=c(2, 0.5, 0)) # GDEX hist # cex.label does not work
    # add line with avg num PGs used ()
    abline(v=my_mean, col="red", lwd=10)
    dev.off()

    if(debug==TRUE){print(paste("made it here 8"))}
    
#For each day - 4 panels -- 3 with hists - 1 with summary of stats

    png (filename= paste(file_in, ".legend.png", sep=""), width = 1200, height = 1000)
    plot.new()
    legend(x="center", cex=5, legend=c(
                                  file_in,
                                  paste("num_osds: ", num_osds, sep=""),
                                  paste("num_pgs: ", num_pgs, sep="")
    )
           )
           
    
    dev.off()
    
    
}








#### Tried something alittle differently here -- writing arrray line by line - does not work yet
 ## # flatten json to table
 ##    num_records <- length(perf_stats.json)/2
 ##    perf_stats.matrix <- matrix #(NA, num_records*3,3)
 ##    #colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
 ##    #rownames(perf_stats.matrix) <- rep("row",nrow(perf_stats.matrix))
 ##    my_osd_index = 0
 ##    for ( i in 1:num_records ){
 ##        pg <- perf_stats.json[ (i*2)-1 ]
 ##        pg_record <- perf_stats.json[ (i*2) ]  
        
 ##        if ( length(pg_record$osds) != 3 ){ warning(paste("Page group", pg, "has", length(pg_record$osds), "OSDs, not 3")) }
        
 ##        for ( j in 1:length(pg_record$osds) ){

 ##            if(debug==TRUE){pg_record.test <<- pg_record}
            
 ##            #handle case when page group is there, but has no OSDs (or at least no data for them)
 ##            if( length(pg_record$osds) == 0){
 ##                if(debug==TRUE){print(paste("num_osds_in_pg:", length(pg_record$osds)))}
 ##                ## my_osd_index <- my_osd_index + 1
 ##                ## my_osd       <- pg_record$osds[[j]]['osd']
 ##                ## my_pfull     <- pg_record$osds[[j]]['percent_full']
 ##                ## t(sapply(1:perf_stats.matrix, function(n) c(my_osd_index, my_osd, my_pfull)))
 ##            }else{
 ##                if(debug==TRUE){print(paste("num_osds_in_pg:", length(pg_record$osds)))}
 ##                my_osd_index <- my_osd_index + 1
 ##                my_osd       <- pg_record$osds[[j]]['osd']
 ##                my_pfull     <- pg_record$osds[[j]]['percent_full']
 ##                t(sapply(1:perf_stats.matrix, function(n) c(my_osd_index, my_osd, my_pfull)))
 ##            }
       
 ##        }
 ##    }
 ## # label the first table
 ## colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
 ## my_rownames <- ""
 ## for (i in 1:nrow(perf_stats.matrix)){
 ##     my_rownames <- c(my_rownames, paste("osd_index", i, sep=""))
 ## }
 ## if(debug==TRUE){my_rownames.test <<- my_rownames}
 ## rownames(perf_stats.matrix) <- my_rownames
    
 ## if( debug==TRUE ){
 ##     perf_stats.matrix.test <<- perf_stats.matrix
 ## }

 ## if(debug==TRUE){
 ##    stop("You asked me to stop")
 ## }
