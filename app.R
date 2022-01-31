#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  #creating a hidden download button, since callr requires an input$,
  #but downloadButton does not natively have an input$

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      actionButton("start", "Render & Download", icon = icon("download")),
      downloadButton("download", "Download", style = "visibility:hidden;"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("did_it_work")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  tempdir <- tempdir()

  output_file <- NULL
  long_run <- shiny::eventReactive(input$start, {
    output_file <<- paste0(tempdir, "/test", gsub(" ", "", Sys.time()), ".pdf")
    rx <- callr::r_bg(rmarkdown::render, args = list(input = "template.Rmd",
                                                     output_file = output_file),
                      package = TRUE, supervise = TRUE)
    rx
  }
  )

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  downloaded <- list()

  #output that tracks progress of background task
  check <- reactive({
    if (long_run()$is_alive()) {
      invalidateLater(millis = 1000, session = session)
      x <- "Job running in background"
    } else {
      Sys.sleep(1)
      x <- "Async job in background completed"
      if (!output_file %in% names(downloaded)) {
        shinyjs::click("download")
        downloaded[[output_file]] <- NULL
      }
    }
    return(x)
  })


  output$download <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      file.copy(output_file, file, overwrite = TRUE)
    }
  )

  output$did_it_work <- renderText({
    check()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
