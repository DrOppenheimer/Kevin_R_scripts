fdr_hist <<- function(stats_matrix, num_bins=20, figure_width = 11, figure_height = 8.5, line_position=0.05){

    if ( is.element("GLDEX", installed.packages()[,1]) == FALSE ){ install.packages("GLDEX") }    
    library(GLDEX)
    
    image_out = gsub(" ", "", paste( deparse(substitute(stats_matrix)), ".FDR_HIST.png"))
    
    png(filename=image_out, width = figure_width, height = figure_height, pointsize = 12, res = 300 , units = "in")
    
    histsu(as.numeric(stats_matrix[,ncol(stats_matrix)]), main=paste( deparse(substitute(stats_matrix)), "FDR histogram"), xlab=paste("FDR; RED =", line_position), ylab="Frequency", breaks=num_bins) # GDEX hist
    #plot(my_hist, log="y", type='h', lwd=10, lend=2)
    abline(v=line_position, col="red", lwd=10)

    dev.off()

}


