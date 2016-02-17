my_file="/Users/kevin/Documents/Projects/Perf_stats/rw_results.txt"
my_file="rw_results.txt"



setwd("/Users/kevin/Documents/Projects/Perf_stats")
source("~/git/CDIS_GEUVADIS_analysis/import_data.r")


my_data <- data.matrix(
	read.table(
		my_file,
		skip=1,
		#row.names=FALSE,
		header=TRUE,
		sep="\t",
		comment.char="",
		quote="",
		check.names=FALSE))

nrep=max(my_data[,'Repeat'])
nfiles=unique(my_data[,'# File'])
new_data <- matrix(NA,nrep, ...)

previous_repeat <- 0
previous_file <- my_data[1,'# File']
for ( i in 1:nrow(my_data) ){

                                        # loop for each file
    if(identical(previous_file, my_data[1,'# File'])==TRUE){
        #keep going
    
    
        if( my_data[,'Repeat'] > previous_repeat ){
        # continue if Repeat keeps increasing,
            previous_file <- my_data[i, 'File']
            previous_repeat <- my_data[i,'Repeat']
        
        
            if( my_data[,'Repeat']==1 ){
            # get values once for each repeat group
                my_file <- 
                my_date
                my_size
                my_operation
                
            }
            
            my_tt
                my_tr_G
                my_tr_M
            
       
        }else{
        # if not - compute avg, stdev, write to new data, reset vector
        }
        
    }else{
        # Proceed to next file
        previous_file <- my_data[i, 'File']
    }
            
    }

}
                                        }#vector for each value

# File	Date_stamp	size(Gb)	Operation	Transfer_time	Transfer_rate(Gb/s)	Transfer_rate(Mb/s)	Repeat





}



#remove the first two lines (contain comments)
my_data <- mydata[,-1:2]
