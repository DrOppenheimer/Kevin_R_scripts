# Load necessary libraries
library(shiny)
library(googlesheets4)
library(DT)
library(shinyjs)

# Authenticate with Google
gs4_auth()

# Specify the URL or ID of your Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/..."

# function to download the data
download_wine_data <- function(sheet_url){
  wine_data <- read_sheet(sheet_url)
  reformatted_wine_data <- matrix( data = NA, nrow=dim(wine_data)[1], ncol=dim(wine_data)[2] )
  for (i in 1:nrow(wine_data)){
    reformatted_wine_data[i,1] <- wine_data[[i,1]]
    reformatted_wine_data[i,2] <- wine_data[[i,2]]
    reformatted_wine_data[i,3] <- wine_data[[i,3]]
    reformatted_wine_data[i,4] <- wine_data[[i,4]]
    reformatted_wine_data[i,5] <- wine_data[[i,5]]
    reformatted_wine_data[i,6] <- as.character(wine_data[[i,6]])
    reformatted_wine_data[i,7] <- wine_data[[i,7]]
    reformatted_wine_data[i,8] <- wine_data[[i,8]]
    reformatted_wine_data[i,9] <- wine_data[[i,9]]
    reformatted_wine_data[i,10] <- wine_data[[i,10]]
    reformatted_wine_data[i,11] <- as.character(wine_data[[i,11]])
    reformatted_wine_data[i,12] <- wine_data[[i,12]]
    reformatted_wine_data[i,13] <- as.character(wine_data[[i,13]])
  }
  dimnames(reformatted_wine_data) <- dimnames(wine_data)
  wine_data <- reformatted_wine_data
  wine_data <- as.data.frame(wine_data)
  rm(reformatted_wine_data)
  return(wine_data)
} 

# function to upload the data
upload_wine_data <- function(wine_data, sheet_url, sheet_name = "Current Inventory") {
  print(paste("Writing to:\n", sheet_url ))
  write_sheet(data = wine_data, ss = sheet_url, sheet = sheet_name)
  print(paste("DONE"))
}

# function to remove a wine
remove_wine <- function(wine_data, row_number=1) {
  wine_data[row_number, 8:13] <- NA #matrix()
  return(wine_data)
  }

# function to edit a wine
edit_wine <- function(wine_data, 
                      row_number=1, 
                      producer="Karma Vista", 
                      nomen="SoCo Grigio", 
                      varietals="Pinot Grigio", 
                      vintage="bought 2023", 
                      wineType="White", 
                      notes=""){
  wine_data[row_number, 8] <- producer
  wine_data[row_number, 9] <- nomen
  wine_data[row_number, 10] <- varietals
  wine_data[row_number, 11] <- vintage
  wine_data[row_number, 12] <- wineType
  wine_data[row_number, 13] <- notes
  return(wine_data)
}


# use the download function to get the wine data
wine_data <- download_wine_data(sheet_url)

# use the upload function to "put" data wine data
upload_wine_data(wine_data, sheet_url)

# use the delete function to delete the first wine
wine_data <- remove_wine(wine_data, row_number=1)

# use the edit function to edit the first wine
wine_data <- edit_wine(wine_data, 
                       row_number=1, 
                       producer="Karma Vista", 
                       nomen="SoCo Grigio", 
                       varietals="Pinot Grigio", 
                       vintage="bought 2023", 
                       wineType="White",
                       notes="")

# Read the data from the specified sheet
wine_data <- read_sheet(sheet_url)

# Reformat the data
reformatted_wine_data <- matrix( data = NA, nrow=dim(wine_data)[1], ncol=dim(wine_data)[2] )
for (i in 1:nrow(wine_data)){
  #for (j in 1:ncol(wine_data)){
  
  #print(i)    
  #print(paste( "[", i, ",", j, "]"))
  #print(wine_data[[i,j]][[1]])
  
  reformatted_wine_data[i,1] <- wine_data[[i,1]]
  reformatted_wine_data[i,2] <- wine_data[[i,2]]
  reformatted_wine_data[i,3] <- wine_data[[i,3]]
  reformatted_wine_data[i,4] <- wine_data[[i,4]]
  reformatted_wine_data[i,5] <- wine_data[[i,5]]
  reformatted_wine_data[i,6] <- as.character(wine_data[[i,6]])
  reformatted_wine_data[i,7] <- wine_data[[i,7]]
  reformatted_wine_data[i,8] <- wine_data[[i,8]]
  reformatted_wine_data[i,9] <- wine_data[[i,9]]
  reformatted_wine_data[i,10] <- wine_data[[i,10]]
  reformatted_wine_data[i,11] <- as.character(wine_data[[i,11]])
  reformatted_wine_data[i,12] <- wine_data[[i,12]]
  reformatted_wine_data[i,13] <- as.character(wine_data[[i,13]])
}
dimnames(reformatted_wine_data) <- dimnames(wine_data)
wine_data <- reformatted_wine_data
wine_data <- as.data.frame(wine_data)
rm(reformatted_wine_data)

# UI for Shiny app
titleText <- paste("We have ", colnames(wine_data)[6], "!")
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selected-wine {
        font-family: 'Times New Roman', Times, serif;
      }
    "))
  ),
  titlePanel(titleText),
  sidebarLayout(
    sidebarPanel(
      uiOutput("columnList"),
      width = 2,
      br(),
      selectInput("wineSelection", "Select Wine:", choices = NULL),
      actionButton("deleteWineBtn", "Delete Wine"),
      actionButton("editWineBtn", "Edit Wine")
    ),
    mainPanel(
      DTOutput("table"),
      br(),
      verbatimTextOutput("selectedWine"),
      textInput("producer", "Producer", ""),
      textInput("nomen", "Nomen", ""),
      textInput("varietals", "Varietal(s)", ""),
      textInput("vintage", "Vintage", ""),
      textInput("wineType", "Type", ""),
      textAreaInput("notes", "Notes", "")
    )
  )
)

# Server logic for Shiny app
server <- function(input, output, session) {
  output$columnList <- renderUI({
    checkboxGroupInput("columnsToShow", "Select Columns to Show", choices = colnames(wine_data),
                       selected = colnames(wine_data)[7:13])
  })
  
  output$table <- renderDT({
    req(input$columnsToShow)
    selected_columns <- input$columnsToShow
    datatable(wine_data[, selected_columns], options = list(pageLength = 5))
  })
  
  observe({
    updateSelectInput(session, "wineSelection", choices = 1:nrow(wine_data))
  })
  
  output$selectedWine <- renderPrint({
    req(input$wineSelection)
    selected_row <- wine_data[input$wineSelection, 7:13]
  })
  
  observeEvent(input$deleteWineBtn, {
    showModal(
      modalDialog(
        title = "Confirmation",
        "Are you sure you want to delete this wine?",
        footer = tagList(
          actionButton("confirmDeleteBtn", "Confirm", class = "btn-danger"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  observeEvent(input$confirmDeleteBtn, {
    req(input$wineSelection)
    wine_data[input$wineSelection, 8:13] <- matrix()
    #write_sheet(data = wine_data, ss = sheet_url, sheet = "Current Inventory")
    session$reload()
    removeModal()
  })
  
  observeEvent(input$editWineBtn, {
    showModal(
      modalDialog(
        title = "Edit Wine",
        fluidRow(
          column(6, textInput("producer", "Producer", "")),
          column(6, textInput("nomen", "Nomen", "")),
          column(6, textInput("varietals", "Varietal(s)", "")),
          column(6, textInput("vintage", "Vintage", "")),
          column(6, textInput("wineType", "Type", "")),
          column(12, textAreaInput("notes", "Notes", ""))
        ),
        footer = tagList(
          actionButton("saveEditBtn", "Save", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  observeEvent(input$saveEditBtn, {
    req(input$wineSelection)
    wine_data[input$wineSelection, 8] <<- input$producer
    print(paste("MADE IT HERE ", 8))
    wine_data[input$wineSelection, 9] <<- input$nomen
    print(paste("MADE IT HERE ", 9))
    wine_data[input$wineSelection, 10] <<- input$varietals
    print(paste("MADE IT HERE ", 10))
    wine_data[[input$wineSelection, 11]][[1]] <- input$vintage
    print(paste("MADE IT HERE ", 11))
    wine_data[input$wineSelection, 12] <<- input$wineType
    print(paste("MADE IT HERE ", 12))
    wine_data[[input$wineSelection, 13]][[1]] <<- input$notes  
    print(paste("MADE IT HERE ", 13))
    #write_sheet(data = wine_data, ss = sheet_url, sheet = "Current Inventory")
    session$reload()
    removeModal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
