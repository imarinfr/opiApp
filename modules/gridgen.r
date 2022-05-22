gridgenUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, div(dataTableOutput(ns("griddb")), style = "font-size: 75%")),
      column(6,
        br(), br(),
        fluidRow(
          column(4, disabled(textInput(ns("code"), "Grid code"))),
          column(8, disabled(textInput(ns("name"), "Grid name")))
        ),
        br(), br(),
        fluidRow(
          column(6, actionButton(ns("create"), "Create new grid", width = "100%")),
          column(6, disabled(actionButton(ns("delete"), "Delete selected grid", width = "100%")))
        )
      )
    ),
    br(), br(),
    fluidRow(
      column(5, rHandsontableOutput(ns("gridpts"))),
      column(7, plotOutput(ns("plotgrid"), height = "350px"))
    ),
    fluidRow(
      br(),
      column(3, offset = 3, disabled(actionButton(ns("save"), "Save", width = "100%"))),
      column(3, disabled(actionButton(ns("cancel"), "Cancel", width = "100%")))
    )
  )
}

gridgen <- function(input, output, session) {
  ns <- session$ns
  gridTableProxy <- dataTableProxy("griddb")
  ptsTableProxy  <- dataTableProxy("gridpts")
  griddbChanged <- reactiveVal(FALSE)
  locsChanged <- reactiveVal(FALSE)
  locs <- reactiveVal(NULL)
  ptsTable <- reactiveVal(NULL)
  gsel <- reactive(!is.null(input$griddb_rows_selected))
  selGridRow <- NULL
  gridTable <- datatable(buildGridTable(grids), selection = "single", rownames = FALSE,
                         options = list(pageLength = 5, lengthChange = FALSE)) %>%
    formatStyle(columns = 0, target = "row", lineHeight = "75%")
  #########
  # Outputs
  #########
  output$griddb <- renderDataTable(gridTable)
  output$gridpts <- renderRHandsontable(ptsTable())
  output$plotgrid <- renderPlot(showGrid(locs()))
  ########
  # Events
  ########
  # if selected patient db has changed
  observe({
    if(griddbChanged()) {
      replaceData(gridTableProxy, buildGridTable(grids), rownames = FALSE)
      griddbChanged(FALSE)
      selectPage(gridTableProxy, ceiling(selGridRow / 5))
      selectRows(gridTableProxy, selGridRow)
    }
  }) %>% bindEvent(griddbChanged(), ignoreInit = TRUE)
  observe({
    if(locsChanged()) {
      selectRows(gridTableProxy, selGridRow) 
    } else {
      if(gsel()) {
        updateTextInput(session, "code", value = names(grids)[input$griddb_rows_selected])
        updateTextInput(session, "name", value = grids[[input$griddb_rows_selected]]$name)
        locs(grids[[input$griddb_rows_selected]]$locs)
        ptsTable(fillPtsTable(locs()))
        selGridRow <<- input$griddb_rows_selected
        enable("name")
        enable("delete")
      }
      else {
        updateTextInput(session, "code", value = "", placeholder = "code")
        updateTextInput(session, "name", value = "", placeholder = "name")
        locs(initLocs())
        ptsTable(fillPtsTable(locs(), readOnly = TRUE))
        selGridRow <<- input$griddb_rows_selected
        disable("name")
        disable("delete")
      }
    }
  }) %>% bindEvent(gsel(), ignoreInit = TRUE)
  observe({
    if(!is.null(selGridRow) && grids[[selGridRow]]$name != input$name) {
      locsChanged(TRUE)
      enable("save")
      enable("cancel")
      disable("create")
    }
  }) %>% bindEvent(input$name)
  observe({
    if(input$gridpts$changes$event == "afterCreateRow") {
      r <- input$gridpts$changes$ind + 1
      ll <- locs()
      if(r <= nrow(ll))
        locs[seq(r + 1, nrow(ll) + 1),] <- ll[seq(r,nrow(ll)),]
      ll[r,] <- c(as.numeric(NA), as.numeric(NA), 0)
      rownames(ll) <- c(1:nrow(ll))
      locs(ll)
      ptsTable(fillPtsTable(locs()))
      locsChanged(TRUE)
      enable("save")
      enable("cancel")
      disable("create")
    }
    if(input$gridpts$changes$event == "afterRemoveRow") {
      r <- (input$gridpts$changes$ind + 1):(input$gridpts$changes$ind + input$gridpts$changes$ct)
      ll <- locs()
      ll <- ll[-r,]
      rownames(ll) <- c(1:nrow(ll))
      locs(ll)
      ptsTable(fillPtsTable(locs()))
      locsChanged(TRUE)
      enable("save")
      enable("cancel")
      disable("create")
    }
    if(input$gridpts$changes$event == "afterChange" &
       !is.null(input$gridpts$changes$changes)) {
      r <- input$gridpts$changes$changes[[1]][[1]] + 1
      c <- input$gridpts$changes$changes[[1]][[2]] + 1
      v <- input$gridpts$changes$changes[[1]][[4]]
      if((c < 3 & is.numeric(v)) | is.integer(v)) {
        v <- input$gridpts$changes$changes[[1]][[4]]
        ll <- locs()
        ll[r, c] <- v
        locs(ll)
      }
      ptsTable(fillPtsTable(locs()))
      locsChanged(TRUE)
      enable("save")
      enable("cancel")
      disable("create")
    }
  }) %>% bindEvent(input$gridpts$changes, ignoreInit = TRUE)
  observe({
    showModal(modalDialog(
      fluidRow(
        column(2, textInput(ns("newcode"), label = "Grid code", placeholder = "code")),
        column(4, textInput(ns("newname"), label = "Grid name", placeholder = "name")),
        column(2, numericInput(ns("numrows"), label = "Locations", value = 20))
      ),
      title = "Create new grid",
      footer = tagList(actionButton(ns("createok"), "Create"), modalButton("Cancel"))
    ))
  }) %>% bindEvent(input$create)
  observe({
    removeModal()
    if(input$newcode == "" | input$newname == "")
      errorMessage("Please fill the new grid's code and name")
    else if(input$newcode %in% names(grids))
      errorMessage("There is grid with the same code, please change it")
    else if(input$numrows <= 0)
      errorMessage("Add at least one test location")
    else {
      grids[[length(grids) + 1]] <<- list(name = input$newname,
                                     locs = data.frame(x = rep(as.numeric(NA), input$numrows),
                                                       y = rep(as.numeric(NA), input$numrows),
                                                       w = integer(input$numrows),
                                                       est = integer(input$numrows)))
      names(grids)[length(grids)] <<- input$newcode
      selGridRow <<- length(grids)
      save(grids, file = "config/grids.rda")
      griddbChanged(TRUE)
    }
  }) %>% bindEvent(input$createok)
  observe({
    showModal(modalDialog(
      title = "Delete grid?",
      "Are you sure you want to delete the current?",
      footer = tagList(actionButton(ns("deleteok"), "Yes"), modalButton("No"))
    ))
  }) %>% bindEvent(input$delete)
  observe({
    removeModal()
    grids[length(grids)] <<- NULL
    save(grids, file = "config/grids.rda")
    selGridRow <<- length(grids)
    griddbChanged(TRUE)
  }) %>% bindEvent(input$deleteok)
  observe({
    showModal(modalDialog(
      title = "Save grid?",
      "Are you sure you want to overwrite the grid?",
      footer = tagList(actionButton(ns("saveok"), "Yes"), modalButton("No"))
    ))
  }) %>% bindEvent(input$save)
  observe({
    removeModal()
    ll <- locs()
    # remove empty rows if any
    idx <- which(apply(ll, 1, function(row) all(is.na(row[1]) & is.na(row[2]))))
    if(length(idx) > 0) ll <- ll[-idx,]
    if(any(is.na(ll)))
      errorMessage("Please fill uncomplete rows before saving")
    else {
      grids[[selGridRow]]$name <<- input$name
      # sort
      ll <- ll[order(ll$x),]
      ll <- ll[order(ll$y, decreasing = TRUE),]
      rownames(ll) <- c(1:nrow(ll))
      locs(ll)
      grids[[selGridRow]]$locs <<- locs()
      save(grids, file = "config/grids.rda")
      locsChanged(FALSE)
      griddbChanged(TRUE)
      disable("save")
      disable("cancel")
      enable("create")
    }
    ptsTable(fillPtsTable(locs()))
  }) %>% bindEvent(input$saveok)
  observe({
    showModal(modalDialog(
      title = "Discard changes?",
      "Are you sure you want to discard the changes?",
      footer = tagList(actionButton(ns("cancelok"), "Yes"), modalButton("No"))
    ))
  }) %>% bindEvent(input$cancel)
  observe({
    updateTextInput(session, "code", value = names(grids)[input$griddb_rows_selected])
    updateTextInput(session, "name", value = grids[[input$griddb_rows_selected]]$name)
    locs(grids[[input$griddb_rows_selected]]$locs)
    ptsTable(fillPtsTable(locs()))
    locsChanged(FALSE)
    disable("save")
    disable("cancel")
    enable("create")
    removeModal()
  }) %>% bindEvent(input$cancelok)
}