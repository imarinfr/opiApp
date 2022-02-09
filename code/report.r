reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, div(dataTableOutput(ns("records")), style = "font-size: 75%"))),
    fluidRow(br(), column(8, plotOutput(ns("plotres"))), column(4, htmlOutput(ns("textres")))),
    fluidRow(column(8, align = "center", br(), disabled(actionButton(ns("genpdf"), "Generate PDF"))))
  )
}

report <- function(input, output, session) {
  ns <- session$ns
  psel <- reactive(!is.null(input$records_rows_selected))
  reports <- NULL
  record <- reactiveVal(NULL)
  locs <- reactiveVal(NULL)
  res <- reactiveVal(NULL)
  eye <- NULL
  foveadb <- NULL
  ####################
  # OBSERVE
  ####################
  # if selected patient db has changed
  observeEvent(newReports(), {
      newReports(FALSE)
      # get new reports if any exist
      reports <<- getReports(patientTable)
      if(!is.null(reports)) {
        # rearrange and change column names for output
        reportsOut <- reports[,c("id", "eye", "name", "date", "time", "machine", "perimetry", "algorithm", "grid")]
        reportsOut$name <- paste(reports$name, reports$surname)
        # find the output name for grid
        reportsOut$grid <- lapply(reports$grid, function(gg) grids[[gg]]$name)
        names(reportsOut) <- c("ID", "Eye", "Name", "Date", "Time", "Device", "Perimetry", "Algorithm", "Grid")
        output$records <- renderDataTable(reportsOut, rownames = FALSE, server = FALSE, selection = "single",
                                          options = list(pageLength = 5, lengthChange = FALSE))
      }
    }
  )
  ####################
  # EVENTS
  ####################
  # selected patient
  observeEvent(psel(), {
    if(psel()) {
      results <- getResults(reports[input$records_rows_selected,])
      record(results$record)
      res(results$res)
      eye <<- record()$eye
      ll <- grids[[reports$grid[input$records_rows_selected]]]$locs
      ll$th <- as.numeric(record()[paste0("l", 1:nrow(ll))])
      if(eye == "L") ll$x <- -ll$x
      locs(ll)
      foveadb <<- record()$foveadb
      enable("genpdf")
    } else {
      record(NULL)
      locs(NULL)
      disable("genpdf")
    }
  }, ignoreInit = TRUE)
  # selected patient
  observeEvent(input$genpdf, {
    tdate <- format(as.Date(record()$date), "%Y%m%d")
    ttime <- gsub(":", "", record()$time)
    fname <- paste0("../results/pdfs/", paste(record()$id, tdate, ttime, sep = "_"), ".pdf")
    savePDF(fname, record(), locs(), res(), eye, foveadb)
    txt <- "The report has been generated as a PDF file and saved in"
    txt <- paste(txt, substr(fname, 4, nchar(fname)))
    showModal(modalDialog(title = "Report generated", txt, easyClose = TRUE))
  })
  output$plotres <- renderPlot(showPlot(locs(), eye, foveadb))
  output$textres <- renderUI(HTML(generateReport(record(), res(), nrow(locs()))))
}