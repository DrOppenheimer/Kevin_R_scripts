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
