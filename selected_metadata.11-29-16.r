# download the metadata for multiple projects


selected_projects <- c(
    "TCGA-BRCA",
    "TCGA-COAD",
    "TCGA-HNSC",
    "TCGA-KICH",
    "TCGA-KIRC",
    "TCGA-KIRP",
    "TCGA-LIHC",
    "TCGA-LUAD",
    "TCGA-LUSC",
    "TCGA-UCEC",
    "TCGA-BLCA"
)






get_GDC_project_ids <- function(){

    my_projects <- vector(mode="character")
    
    #first_call, get number of projects then make a second to get them all
    first_call <- "https://gdc-api.nci.nih.gov/projects"
    first_call.json <- fromJSON(getURL(first_call))
    number_projects <- first_call.json$data$pagination$total
    second_call <- paste("https://gdc-api.nci.nih.gov/projects?size=",number_projects,sep="")
    second_call.json <- fromJSON(getURL(second_call))

    for (i in 1:length(second_call.json$data$hits)){
        my_projects <- c(my_projects, second_call.json$data$hits[[i]]$project_id)
    }

    return(my_projects)

}







### Source Ronald and Mert scripts
RandM_scripts <- dir(path="~/git/Ronald-and-Mert/", pattern=".r$")
RandM_scripts <- c(RandM_scripts, dir(path="~/git/Ronald-and-Mert/", pattern=".r$"))
for (i in 1:length(RandM_scripts)){
	print(paste("sourcing", RandM_scripts[i]))
	script_path <- paste("~/git/Ronald-and-Mert/", RandM_scripts[i], sep="")
	source(script_path)
	print("DONE")
}


# get the list of UUIDs for the project(s)
# export_listof_UUIDs(project="TCGA-LUAD")
for (i in 1:length(selected_projects)){
  export_listof_UUIDs(project=selected_projects[i])  
} 

# get the metadata 
#get_GDC_metadata("TCGA-LUAD_file_UUIDs")
UUID_file_list <- dir(pattern="_UUIDs$")
for (i in 1:length(UUID_file_list)){
    print(paste("current project", UUID_file_list[i]))
    get_GDC_metadata(UUID_file_list[i])
}
# failed on LUAD? try
for (i in 8:length(UUID_file_list)){
    print(paste("current project", UUID_file_list[i]))
    get_GDC_metadata(UUID_file_list[i])
}
## > for (i in 9:length(UUID_file_list)){
## +     print(paste("current project", UUID_file_list[i]))
## +     get_GDC_metadata(UUID_file_list[i])
## + }
## [1] "current project TCGA-LUAD_file_UUIDs"
## Read 594 items
## [1] "Processing sample ( 1 : 001c6345-176c-4ab3-a0c8-27384879a8ff )"
##   % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
##                                  Dload  Upload   Total   Spent    Left  Speed
##   0     0    0     0    0     0      0      0 --:--:--  0:00:02 --:--:--     0curl: (7) Failed to connect to cloud-proxy port 3128: No route to host
## Error in fromJSON(content, handler, default.size, depth, allowComments,  : 
##   invalid JSON input




# get the data 
get_GDC_metadata("TCGA-LUAD_file_UUIDs")




setwd("/Users/kevin/Documents/Projects/summer_students/2016/LUAD_smoke")
#setwd("/Users/kevin/Documents/Projects/summer_students/2016/LUSC_smoke")

RandM_scripts <- dir(path="~/git/Ronald-and-Mert/", pattern=".r$")
RandM_scripts <- c(RandM_scripts, dir(path="~/git/Ronald-and-Mert/", pattern=".r$"))
for (i in 1:length(RandM_scripts)){
	print(paste("sourcing", RandM_scripts[i]))
	script_path <- paste("~/git/Ronald-and-Mert/", RandM_scripts[i], sep="")
	source(script_path)
	print("DONE")
}

LUSC
# export_listof_UUIDs
export_listof_UUIDs(project="TCGA-LUAD")


# get the metadata 
get_GDC_metadata("TCGA-LUAD_file_UUIDs")





### FROM  https://github.com/RonaldHShi/Ronald-and-Mert/download_from_GDC.ronald.R


download_normal_from_GDC <- function(projects) {
# download normal sample HTSeq Counts data for a given project
# 
# Args:
#   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  for (p in projects) {
    print(p)
    my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22cases.samples.sample_type%22%2C%22value%22%3A%5B%22Blood+Derived+Normal%22%2C%22Solid+Tissue+Normal%22%2C%22Bone+Marrow+Normal%22%2C%22Fibroblasts+from+Bone+Marrow+Normal%22%2C%22Buccal+Cell+Normal%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22analysis.workflow_type%22%2C%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22files.data_format%22%2C%22value%22%3A%5B%22TXT%22%5D%7D%7D%2C%7B%22op%22%3A%22%3D%22%2C%22content%22%3A%7B%22field%22%3A%22cases.project.project_id%22%2C%22value%22%3A%5B%22", p, "%22%5D%7D%7D%5D%7D")
    my_call.json <- fromJSON(getURL(my_call))
    UUID.list <- unlist(my_call.json$data$hits)
    for(j in UUID.list) {
      print(paste0(j, ": ", j))
      system(paste0("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                  j, "'"))
    }
  }
}

download_cancer_from_GDC <- function(projects) {
  # download cancer sample HTSeq Counts data for a given project
  # 
  # Args:
  #   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  for (p in projects) {
    my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22cases.samples.sample_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22Primary+Tumor%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
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


download_data_from_GDC <- function(project_name){

}










### OTHER NOTES

### place this in a file (from https://gdc-docs.nci.nih.gov/API/Users_Guide/Search_and_Retrieval/#example-http-post-request )

{
    "filters":{
        "op":"and",
        "content":[
            {
                "op":"in",
                "content":{
                    "field":"cases.submitter_id",
                    "value":[
                        "TCGA-CK-4948",
                        "TCGA-D1-A17N",
                        "TCGA-4V-A9QX",
                        "TCGA-4V-A9QM"
                    ]
                }
            },
            {
                "op":"=",
                "content":{
                    "field":"files.data_type",
                    "value":"Gene Expression Quantification"
                }
            }
        ]
    },
    "format":"tsv",
    "fields":"file_id,file_name,cases.submitter_id,cases.case_id,data_category,data_type,cases.samples.tumor_descriptor,cases.samples.tissue_type,cases.samples.sample_type,cases.samples.submitter_id,cases.samples.sample_id,analysis.workflow_type,cases.project.project_id,cases.samples.portions.analytes.aliquots.aliquot_id,cases.samples.portions.analytes.aliquots.submitter_id",
    "size":"1000"
}

### then reference that file with --data (be sure to use the "@" prefix)
curl --request POST --header "Content-Type: application/json" --data @some_file.txt 'https://gdc-api.nci.nih.gov/files' > response.json





###



### Decode encode urls
if ( is.element("urltools", installed.packages()[,1]) == FALSE ){ install.packages("urltools") }
library(urltools)

my_encoded_url <- "https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22cases.samples.sample_type%22%2C%22value%22%3A%5B%22Blood+Derived+Normal%22%2C%22Solid+Tissue+Normal%22%2C%22Bone+Marrow+Normal%22%2C%22Fibroblasts+from+Bone+Marrow+Normal%22%2C%22Buccal+Cell+Normal%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22analysis.workflow_type%22%2C%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22files.data_format%22%2C%22value%22%3A%5B%22TXT%22%5D%7D%7D%2C%7B%22op%22%3A%22%3D%22%2C%22content%22%3A%7B%22field%22%3A%22cases.project.project_id%22%2C%22value%22%3A%5B%22TCGA-LUAD%22%5D%7D%7D%5D%7D"

#my_decoded_url <- url_decode(my_encoded_url) # urltools package
my_decoded_url <- URLdecode(my_encoded_url) # base

#my_re_encoded_url <- url_encode(my_decoded_url) # urltools package # creates invaliud call
my_re_encoded_url <- URLencode(my_decoded_url) # base # seems to work even though call is not identical

identical( my_encoded_url, my_re_encoded_url)        
                                        # These differe -- see which characters differe:
Reduce(setdiff, strsplit(c(my_encoded_url, my_re_encoded_url), split = ""))
# "?" "=" "&" "+"



url_decode(urls)

url_encode(urls)





    









kkeegan [2:48 PM]  
nice - is it on https://github.com/uc-cdis/Ronald-and-Mert ?

ronaldhshi [2:50 PM]  
I haven't been working on that fork, I've just been pushing everything to my own master branch so it's at: https://github.com/RonaldHShi/Ronald-and-Mert

[2:51]  
Also the rows of the metadata table are the filenames of the data stripped of their ".htseq.counts" ending. I could try changing them to some other kind of ids if that would help Michael help us identify what all the columns in the table mean

kkeegan [2:52 PM]  
they're fine as they are

[2:52]  
maybe just stick a comment in the code (if you didn't already) indicating how they're generated

ronaldhshi [2:53 PM]  
Most of the metadata code was developed by you; Mert and I just added a little bit of it to get a complete query, but I can add a comment in anyway

kkeegan [2:55 PM]  
                                        (please do add the comment, I am definitely a fan of 10 lines of comment for each line of code ^_^) everything is in download_from_GDC.ronald.R ?









                                        download_from_GDC <- function(list.of.UUIDs = F,
                              project_id = F, 
                              analysis.workflow_type = "HTSeq - Counts"
                              UUIDs.file = F,
                              cases.samples.sample_type = "Primary Tumor",
                              size_lim = F) {
  # download HTSeq Counts data for a given project
  # 
  # Args:
  #   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  vector_of_files <- unlist(get_UUIDs_from_project(project))
  if(size_lim && size_lim < length(vector_of_files)) {
    vector_of_files <- vector_of_files[sample(1:length(vector_of_files), size_lim)]
  }
  for(j in 1:length(vector_of_files)) {
    print(paste0(j, ": ", vector_of_files[j]))
    system(paste("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                 vector_of_files[j],
                 "'",
                 sep=""))
  }
}



download_all_from_GDC <- function(projects) {
# download normal sample HTSeq Counts data for a given project
# 
# Args:
#   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  for (p in projects) {
    print(p)
    my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22cases.samples.sample_type%22%2C%22value%22%3A%5B%22Blood+Derived+Normal%22%2C%22Solid+Tissue+Normal%22%2C%22Bone+Marrow+Normal%22%2C%22Fibroblasts+from+Bone+Marrow+Normal%22%2C%22Buccal+Cell+Normal%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22analysis.workflow_type%22%2C%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22files.data_format%22%2C%22value%22%3A%5B%22TXT%22%5D%7D%7D%2C%7B%22op%22%3A%22%3D%22%2C%22content%22%3A%7B%22field%22%3A%22cases.project.project_id%22%2C%22value%22%3A%5B%22", p, "%22%5D%7D%7D%5D%7D")
    my_call.json <- fromJSON(getURL(my_call))
    UUID.list <- unlist(my_call.json$data$hits)
    for(j in UUID.list) {
      print(paste0(j, ": ", j))
      system(paste0("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                  j, "'"))
    }
  }
}
                                        


                                        
download_normal_from_GDC <- function(projects) {
# download normal sample HTSeq Counts data for a given project
# 
# Args:
#   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  for (p in projects) {
    print(p)
    my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%22op%22%3A%22and%22%2C%22content%22%3A%5B%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22cases.samples.sample_type%22%2C%22value%22%3A%5B%22Blood+Derived+Normal%22%2C%22Solid+Tissue+Normal%22%2C%22Bone+Marrow+Normal%22%2C%22Fibroblasts+from+Bone+Marrow+Normal%22%2C%22Buccal+Cell+Normal%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22analysis.workflow_type%22%2C%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%7D%7D%2C%7B%22op%22%3A%22in%22%2C%22content%22%3A%7B%22field%22%3A%22files.data_format%22%2C%22value%22%3A%5B%22TXT%22%5D%7D%7D%2C%7B%22op%22%3A%22%3D%22%2C%22content%22%3A%7B%22field%22%3A%22cases.project.project_id%22%2C%22value%22%3A%5B%22", p, "%22%5D%7D%7D%5D%7D")
    my_call.json <- fromJSON(getURL(my_call))
    UUID.list <- unlist(my_call.json$data$hits)
    for(j in UUID.list) {
      print(paste0(j, ": ", j))
      system(paste0("curl --remote-name --remote-header-name 'https://gdc-api.nci.nih.gov/data/", 
                  j, "'"))
    }
  }
}

download_cancer_from_GDC <- function(projects) {
  # download cancer sample HTSeq Counts data for a given project
  # 
  # Args:
  #   project: vector of files that contain project_ids or 
  #            a vector of projects
  #
  # Returns:
  #   (None)
  #   Downloads all HTSeq Counts files as *.gz, need to unzip
  for (p in projects) {
    my_call <- paste0("https://gdc-api.nci.nih.gov/files?fields=file_id&size=99999&pretty=true&filters=%7B%0D%0A%09%22op%22%3A%22and%22%2C%0D%0A%09%22content%22%3A%5B%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22cases.samples.sample_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22Primary+Tumor%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22analysis.workflow_type%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22HTSeq+-+Counts%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22in%22%2C%0D%0A%09%09%22content%22%3A%7B%0D%0A%09%09%09%22field%22%3A%22files.data_format%22%2C%0D%0A%09%09%09%22value%22%3A%5B%22TXT%22%5D%0D%0A%09%09%09%7D%0D%0A%09%09%7D%2C%7B%0D%0A%09%09%22op%22%3A%22%3D%22%2C%0D%0A%09++++%22content%22%3A%7B%0D%0A%09++++%09%22field%22%3A%22cases.project.project_id%22%2C%0D%0A%09++++%09%22value%22%3A%5B%22", p, "%22%5D%0D%0A%09++++%7D%0D%0A%09%7D%5D%0D%0A%7D")
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


