plot_HD_usage <- function(file_in){

	library(RJSONIO)
	# import json
	# file_in = "ceph_pg_and_osd-2015-12-13.json"
	perf_stats.json <<- fromJSON(file_in)

	# flatten json to table
	num_records <- length(perf_stats.json)/2
	perf_stats.matrix <- matrix("", num_records*3,3)
	colnames(perf_stats.matrix) <- c("osd_index","osd","percent_full")
	rownames(perf_stats.matrix) <- rep("row",nrow(perf_stats.matrix))
	for ( i in 1:num_records ){
		pg <- perf_stats.json[ (i*2)-1 ]
		pg_record <- perf_stats.json[ (i*2) ]  

		if ( length(pg_record$osds) != 3 ){ stop(paste("Page group", pg, "has", length(pg_record$osds), "HDs, not 3")) }

		rownames(perf_stats.matrix)[ (i*3)-2 ] <- pg
		perf_stats.matrix[(i*3)-2, 1] <- i	
		perf_stats.matrix[(i*3)-2, 2] <- pg_record$osds[[1]]["osd"]
		perf_stats.matrix[(i*3)-2, 3] <- as.numeric( gsub("%", "", pg_record$osds[[1]]["percent_full"]) )

		rownames(perf_stats.matrix)[ (i*3)-1 ] <- pg
		perf_stats.matrix[(i*3)-1, 1] <- i
		perf_stats.matrix[(i*3)-1, 2] <- pg_record$osds[[2]]["osd"]
		perf_stats.matrix[(i*3)-1, 3] <- as.numeric( gsub("%", "", pg_record$osds[[2]]["percent_full"]) )

		rownames(perf_stats.matrix)[ (i*3) ] <- pg
		perf_stats.matrix[(i*3), 1] <- i
		perf_stats.matrix[(i*3), 2] <- pg_record$osds[[3]]["osd"]
		perf_stats.matrix[(i*3), 3] <- as.numeric( gsub("%", "", pg_record$osds[[3]]["percent_full"]) )

	}
	# write file out
	png ( filename= paste(file_in, ".disk_usage.png", sep=""), width = 1000, height = 1000)

	write.table(perf_stats.matrix, file = "perf_stats.matrix.txt", col.names=NA, sep="\t", quote=FALSE)
	# create hist
	my_mean <- mean(as.numeric(perf_stats.matrix[,"percent_full"]), na.rm=TRUE)
	num_na <- sum(is.na(perf_stats.matrix[,"percent_full"]))
	expected_num_hd <- num_records*3
	real_num_hd <- expected_num_hd - num_na
	my_mean <- mean(as.numeric(perf_stats.matrix[,"percent_full"]), na.rm=TRUE)
	num_na <- sum(is.na(perf_stats.matrix[,"percent_full"]))
	expected_num_hd <- num_records*3
	real_num_hd <- expected_num_hd - num_na
	hist(as.numeric(perf_stats.matrix[,"percent_full"]), main=paste(file_in, ":: Percent Full by HD"), xlab=paste("Percent Full (red line = 		mean ::", round(my_mean, digits=2), ")"), ylab="num HDs")
	# add line with avg HD used ()
	abline(v=my_mean, col="red")
	dev.off()
}
