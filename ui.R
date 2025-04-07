library(shiny)
library(DT)
library(lubridate)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(dplyr)

ui <- tags$html(
  lang = "en",
  fluidPage(
    style = "padding: 0px; margin: 0px;",
    tags$head(
      tags$title("WEDC: Posters"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Black banner
    tags$div(
      class = "black-banner",
      tags$div(
        class = "banner-content",
        tags$a(
          href = "https://www.lboro.ac.uk",
          target = "_blank",
          tags$img(src = "logo.png", class = "uni-logo", alt = "University Logo")
        ),
        tags$span("School of Architecture, Building and Civil Engineering")
      )
    ),
    
    # Blue banner
    tags$div(
      class = "blue-banner",
      tags$div(
        class = "banner-content",
        tags$span("Water Engineering and Development Centre"),
        tags$a(
          href = "https://www.lboro.ac.uk/research/wedc/publications-and-resources/",
          class = "return-link",
          "< Return to Publications and resources"
        )
      )
    ),
    
    # Title section
    tags$div(
      class = "white-banner",
      tags$h1("Posters")
    ),
    
    # Layout for inputs and outputs
    sidebarLayout(
      sidebarPanel(
        style = "margin-left: 20px; padding-right: 20px;",
        
        # Collection Drop-down
        selectInput(
          inputId = "collectionSelect",
          label = "Select a Collection",
          choices = NULL,  # Placeholder, will be updated dynamically
          selected = NULL,
          multiple = FALSE
        ),
        
        # Author Search
        textInput(
          inputId = "authorSearch",
          label = "Search by Author",
          placeholder = "Enter author's name"
        ),
        
        # Title Search
        textInput(
          inputId = "titleSearch",
          label = "Search by Title",
          placeholder = "Enter book or manual title"
        ),
        p(),
        p("The Water Engineering and Development Centre (WEDC) produces and disseminates quality, relevant and accessible knowledge products to meet the needs of academics, policymakers and practitioners working in various aspects of water engineering and development."),
        p("Our books, manuals and other resources represent a substantial body of knowledge in water management, engineering and other international development-related subjects developed over 50 years.")
      ),
      
      mainPanel(
        fluidRow(
          style = "margin-left: 20px; margin-right: 20px;",
          withSpinner(
            uiOutput("bookDetails"),
            type = 3,
            color = "#009BC9",
            color.background = "#FFFFFF"
          )
        )
      )
      ),
      
      tags$div(class = "footer", 
               fluidRow(
                 column(12, 
                        tags$a(href = 'https://doi.org/10.17028/rd.lboro.28525481', 
                               "Accessibility Statement")
                 )
               )
      )
      
    )
  )

