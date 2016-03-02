### s3://griffin-objstore.opensciencedatacloud.org/asgc-geuvadis/ERR188021_2.fastq.gz


### https://signpost.opensciencedatacloud.org/alias/?start=ark:/31807/DC0-0024f8ad-a124-4bea-8c69-ea803d1cf29b

### https://signpost.opensciencedatacloud.org/alias/?limit=10

### This works
# https://signpost.opensciencedatacloud.org/alias/?limit=1024

### this will crash
# https://signpost.opensciencedatacloud.org/alias/?limit=1025

# https://signpost.opensciencedatacloud.org/alias/?start=ark:/31807/DC0-0024f8ad-a124-4bea-8c69-ea803d1cf29b?limit=10

### use & to do multiple querries
# https://signpost.opensciencedatacloud.org/alias/?start=ark:/31807/DC0-0024f8ad-a124-4bea-8c69-ea803d1cf29b&limit=1

library(RJSONIO)
########################################################################
########################################################################
# function to cummulatively access all of the ids that are (publically?) availabale in the id service
add_chunk <- function(my_host, my_limit, joined_aliases, debug=FALSE){
  
  last_alias <- joined_aliases[ length(joined_aliases) ]
  
  next_chunk_url <- paste(my_host, "?", "limit=", my_limit, "&start=", last_alias, sep="", collapse="") 
  if(debug==TRUE){print(next_chunk_url)}
  next_chunk_json <- fromJSON(next_chunk_url)
  next_chunk_aliases <- next_chunk_json$aliases
  if(debug==TRUE){next_chunk_aliases.test<<-next_chunk_aliases}  
  
  joined_aliases <- unique(c(joined_aliases, next_chunk_aliases))
  
  if( length(next_chunk_aliases)==my_limit ){
    my_status=0
    if(debug==TRUE){print(paste("my_status: ", my_status))}
    if(debug==TRUE){print(paste("length chunk:", length(next_chunk_aliases)))}
    my_list <- list("my_status"=my_status,"joined_aliases"=joined_aliases)
    return(my_list)
  }else{
    my_status=1
    if(debug==TRUE){print(paste("my_status: ", my_status))}
    if(debug==TRUE){print(paste("length chunk:", length(next_chunk_aliases)))}
    my_list <- list("my_status"=my_status,"joined_aliases"=joined_aliases)
    return(my_list)
  }
  
}
########################################################################
########################################################################
# bit of code uses the function above to get all ark aliases
my_limit= "100"
my_start= ""
my_host="https://signpost.opensciencedatacloud.org/alias/"
first_chunk_url = paste(my_host, "?", "limit=", my_limit, sep="", collapse="")
first_chunk_json <- fromJSON(first_chunk_url)
joined_aliases <- unique(first_chunk_json$aliases)
my_status=0
chunk_count=1
while (my_status==0){
  
  next_chunk <- add_chunk(my_host, my_limit, joined_aliases, debug=TRUE)
  joined_aliases <- next_chunk$joined_aliases
  my_status <- next_chunk$my_status 
  chunk_count = chunk_count + 1
  print(paste("chunk count: ", chunk_count, sep=""))
    
}
length(joined_aliases)
########################################################################
########################################################################

########################################################################
########################################################################
# go through the records and find the ones that have urls
my_file = "some_log.txt"
write(paste( c("ark", "num_md5", "first_md5", "num_urls", "urls ..."), collapse="\t"), file=my_file)
for (i in 1:length(joined_aliases)){
  
  my_json_url <- paste(my_host, joined_aliases[i], sep="", collapse="") 
  #print(my_json_url)  
  my_json <- fromJSON(my_json_url)
  my_urls <- my_json$urls
  num_urls <- length(my_urls)
  if( num_urls > 0 ){
    line_out <- paste( c(my_json$name, length(my_json$hashes), my_json$hashes, length(my_json$urls), my_json$urls), collapse="\t")
    #print(line_out)
    write(line_out, file=my_file, append=TRUE)
    print(i)
  }
  
}
########################################################################
########################################################################




