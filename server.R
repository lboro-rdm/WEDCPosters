server <- function(input, output, session) {
  
  # Reactive function to read and format the data from the CSV file
  booksData <- reactive({
    csv_file <- "combined_data.csv"
    if (file.exists(csv_file)) {
      read.csv(csv_file, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  # Populate the dropdown with unique collection names
  observe({
    df <- booksData()
    if (!is.null(df)) {
      updateSelectInput(
        session,
        "collectionSelect",
        choices = c("All", unique(df$collection_title)),  # Add "All" option
        selected = "All"
      )
    }
  })
  
  # Reactive function to filter books based on inputs
  filteredBooks <- reactive({
    df <- booksData()
    if (is.null(df)) return(NULL)
    
    # Apply filters
    if (!is.null(input$collectionSelect) && input$collectionSelect != "All" && input$collectionSelect != "") {
      df <- df[df$collection_title == input$collectionSelect, ]
    }
    if (!is.null(input$authorSearch) && input$authorSearch != "") {
      df <- df[grepl(input$authorSearch, df$Author, ignore.case = TRUE), ]
    }
    if (!is.null(input$titleSearch) && input$titleSearch != "") {
      df <- df[grepl(input$titleSearch, df$title, ignore.case = TRUE), ]
    }
    
    # Sort first by collection title, then by sort number, and lastly by title
    if (!is.null(df) && nrow(df) > 0) {
      df$sort <- as.numeric(gsub("^[^0-9]*", "", df$sort))  # Extract numeric part of sort
      df <- df[order(df$collection_title, df$sort, df$title), ]
    }
    
    df
  })
  
  # Reactive function to format the filtered data
  formattedBooks <- reactive({
    df <- filteredBooks()
    if (!is.null(df) && nrow(df) > 0) {
      # Create formatted strings with zebra stripe classes
      formatted_strings <- sapply(1:nrow(df), function(i) {
        paste0(
          "<div style='margin-bottom: 10px;'>", # Add bottom margin
          "<strong><a href='https://hdl.handle.net/", df$hdl[i], 
          "' style='color: #002c3d; text-decoration: underline;' target='_blank' class='hover-underline'>",
          df$title[i], "</a></strong>. ", 
          "<span style='color: #002c3d;'>", df$Author[i], ". (", 
          df$Year[i], ").</span>",
          "</div>"
        )
      })
      # Return the formatted strings as a single string
      paste(formatted_strings, collapse = "")
    } else {
      "No results found."
    }
  })
  
  # Render the filtered and formatted books
  output$bookDetails <- renderUI({
    HTML(formattedBooks())
  })
}
