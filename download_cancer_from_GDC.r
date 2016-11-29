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
