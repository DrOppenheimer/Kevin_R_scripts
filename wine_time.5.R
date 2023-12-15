# Load necessary libraries
library(shiny)
library(googlesheets4)
library(DT)
library(shinyjs)

# Preload empty data to some objects
wine_data <- matrix()
selected_data <- 0

# Authenticate with Google
gs4_auth()

# Specify the URL or ID of your Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/..."

# function to download the data and do some formatting to undo google wonkiness
download_wine_data <- function(sheet_url) {
  cat("DOWNLAODING DATA FROM\n", sheet_url, "\n")
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
  cat("DONE")
  return(wine_data)
}

# function to print the line number for debugging
print_line_number <- function() {
  frame <- sys.frame(1)  # Get the current frame in the call stack
  calls <- sys.calls()  # Get the call stack
  cat("Line number:", sys.nframe(), "\n") # Print the line number
}

# function to upload the data
upload_wine_data <- function(my_data = wine_data,
                             sheet_url,
                             sheet_name = "Current Inventory") {
  cat("UPLOADING TO:\n", sheet_url, "\n")
  write_sheet(data = my_data, ss = sheet_url, sheet = sheet_name)
  cat("DONE")
}

# function to remove a wine
remove_wine <- function(my_data=wine_data, row_number=1) {
  my_data[row_number, 8:13] <- NA #matrix()
  print(my_data[row_number, 8:13])
  return(my_data)
}

# function to edit a wine
edit_wine <- function(my_data=wine_data,
                      row_number=1,
                      producer="Karma Vista",
                      nomen="SoCo Grigio",
                      varietals="Pinot Grigio",
                      vintage="bought 2023",
                      wineType="White",
                      notes=""){
  my_data[row_number, 8] <- producer
  my_data[row_number, 9] <- nomen
  my_data[row_number, 10] <- varietals
  my_data[row_number, 11] <- vintage
  my_data[row_number, 12] <- wineType
  my_data[row_number, 13] <- notes
  print(my_data[row_number, 7:13])
  return(my_data)
}

# here is the actual shiny app code
# The shiny UI
ui <- navbarPage(
  title = "Kevin and Jenny's Lush Management System",
  tabPanel(
    paste("We Have",  wine_data[1,1],"Bottles!!!"),
    # The buttons
    fluidRow(
      column(3,
             actionButton("download_btn", "Download Wine", width = "100%")
      ),
      column(3,
             actionButton("remove_btn", "Remove Wine", width = "100%")
      ),
      column(3,
             actionButton("edit_btn", "Edit Wine", width = "100%")
      ),
      column(3,
             actionButton("upload_btn", "Upload Wine", width = "100%")
      )
    ),
    fluidRow(
      column(12,
             # Display the selected columns and rows of the data matrix
             dataTableOutput("selected_data_table")
      )
    ),
    fluidRow(
      column(12,
             # Display the selected row information
             verbatimTextOutput("selected_row_info")
      )
    ),
    fluidRow(
      #column(2, textInput("location", "Location", "")),
      column(2, textInput("producer", "Producer", "")),
      column(2, textInput("nomen", "Nomen", "")),
      column(2, textInput("varietals", "Varietal(s)", "")),
      column(2, textInput("vintage", "Vintage", "")),
      column(2, textInput("winetype", "wineType", "")),
      column(2, textInput("notes", "Notes", ""))
    )
  )
)

# The server
server <- function(input, output, session) {
  
  observeEvent(input$download_btn, {
    # Code for handling download button click event
    # This block will execute when "Download Wine" button is clicked
    # Add your download logic here
    wine_data <- download_wine_data(sheet_url)
    # Display the first ten rows of the wine_data in a table
    output$selected_data_table <- renderDataTable({
      # Here, 'wine_data' contains the downloaded data matrix
      datatable(wine_data[ 1:nrow(wine_data), 7:13], selection = 'single', options = list(pageLength = 10))  # Displaying the first ten rows with the option to see more
    })
    # Capture the selected row information
    output$selected_row_info <- renderPrint({
      req(input$selected_data_table_rows_selected)
      selected_row <<- input$selected_data_table_rows_selected
      if (length(selected_row) > 0) {
        #cat("Selected Row Data:\n")
        print(wine_data[selected_row, 7:13 ])
      }
    })
  })
  
  observeEvent(input$remove_btn, {
    # Code for handling remove button click event
    # This block will execute when "Delete Wine" button is clicked
    # Add your delete logic here
    wine_data <<- remove_wine(my_data=wine_data, row_number=selected_row)
    output$selected_data_table <- renderDataTable({
      # Here, 'wine_data' contains the downloaded data matrix
      datatable(wine_data[ 1:nrow(wine_data), 7:13], selection = 'single', options = list(pageLength = 10))  # Displaying the first ten rows with the option to see more
    })
  })
  
  observeEvent(input$edit_btn, {
    # Code for handling edit button click event
    wine_data <<- edit_wine(
      my_data = wine_data,
      row_number=selected_row,
      #location=input$location,
      producer=input$producer,
      nomen=input$nomen,
      varietals=input$varietals,
      vintage=input$vintage,
      wineType=input$winetype,
      notes=input$notes)
    output$selected_data_table <- renderDataTable({
      # Here, 'wine_data' contains the downloaded data matrix
      datatable(wine_data[ 1:nrow(wine_data), 7:13], selection = 'single', options = list(pageLength = 10))  # Displaying the first ten rows with the option to see more
      #datatable(wine_data[1:input$rows_to_display, ], options = list(pageLength = input$rows_to_display))
    })
  })
  
  observeEvent(input$upload_btn, {
    # Call the upload_wine_data function with the data and Google Sheets URL
    upload_wine_data(my_data=wine_data,
                     sheet_url,
                     sheet_name = "Current Inventory")
  })
  
  observeEvent(input$selected_data_table_rows_selected, {
    selected_row <<- input$selected_data_table_rows_selected
    # Assuming you want to update input1 based on the first column of selected row
    if (!is.null(selected_row) && length(selected_row) > 0) {
      #updateTextInput(session, "location", value = wine_data[selected_row, 7])
      updateTextInput(session, "producer", value = wine_data[selected_row, 8])
      updateTextInput(session, "nomen", value = wine_data[selected_row, 9])
      updateTextInput(session, "varietals", value = wine_data[selected_row, 10])
      updateTextInput(session, "vintage", value = wine_data[selected_row, 11])
      updateTextInput(session, "type", value = wine_data[selected_row, 12])
      updateTextInput(session, "notes", value = wine_data[selected_row, 13])
    }
  })
  
}

shinyApp(ui = ui, server = server)