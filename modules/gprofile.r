gprofileUI <- function(id) {
  ns <- NS(id)
  opiImpl <- "PhoneHMD"
  tagList(
    fluidRow(
      column(3, selectInput(ns("machine"), "OPI implementation", choices = opiImpl, selected = opiImpl)),
      column(3, textInput(ns("name"), "Profile name", placeholder = "name")),
      column(5, br(), offset = 1, rHandsontableOutput(ns("setup")))
    ),
    fluidRow(column(12, htmlOutput(ns("msgconn")))),
    fluidRow(
      column(3, actionButton(ns("init"),  label = "Initialize OPI", width = "100%")),
      column(3, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE))
    ),
    fluidRow(br(),
      column(5, rHandsontableOutput(ns("lut"))),
      column(7, plotOutput(ns("plotlut")))
    ),
    fluidRow(br(),
      column(12, align = "center", hidden(actionButton(ns("save"), label = "Save Gamma correction")))
    )
  )
}

gprofile <- function(input, output, session) {
  ns <- session$ns
  setupTable <- data.frame(from = c(0, 25, 225), by = c(5, 25, 5), to = c(25, 225, 255))
  makeReactiveBinding("setupTable")
  lutTable <- NULL
  makeReactiveBinding("lutTable")
  lutFit <- data.frame(x = 0:255, y = 0)
  # messages for status o connection, etc
  msg <- reactiveVal("Press 'Initialize OPI' to start")
  #########
  # Outputs
  #########
  output$lut <- NULL
  output$plotlut <- NULL
  output$msgconn <- renderText(msg())
  output$setup <- renderRHandsontable(setuptable(setupTable))
  ########
  # Events
  ########
  # initialize OPI
  observe({
    chooseOPI(input$machine)
    pars <- opiGetParams("opiInitialize")
    pars$ip <- appParams$ip
    pars$port <- appParams$port
    pars$lut <- 0:255
    if(is.null(do.call(what = opiInitialize, args = pars)) &&
       is.null(do.call(opiSetBackground, list(bgeye = "B", fixeye = "B", bglum = 0, bgcol = appParams$bgcol)))) {
      msg("OPI connection opened")
      disable("init")
      enable("close")
      showElement("save")
      lutTable <<- generateLUTtable(setupTable)
      lutFit <<- data.frame(x = 0:255, y = 0)
      output$setup <- renderRHandsontable(setuptable(setupTable, readOnly = TRUE))
      output$lut <- renderRHandsontable(luttable(lutTable))
      output$plotlut <- renderPlot(lutPlot(lutTable, lutFit))
      opiInitialized(TRUE)
    } else msg(errortxt("Could not connect to the OPI server"))
  }) %>% bindEvent(input$init, ignoreInit = TRUE)
  # close OPI connection
  observe({
    opiClose()
    msg("OPI connection closed")
    enable("init")
    disable("close")
    hideElement("save")
    enableAll()
    lutTable       <- NULL
    output$lut     <- NULL
    output$plotlut <- NULL
    output$setup <- renderRHandsontable(setuptable(setupTable))
    opiInitialized(FALSE)
  }) %>% bindEvent(input$close, ignoreInit = TRUE)
  # select row
  observe({
    if(opiInitialized())
      do.call(opiSetBackground, list(bgeye = "B", fixeye = "B", bglum = lutTable$pix[input$lut_select$select$r], bgcol = appParams$bgcol))

  }) %>% bindEvent(input$lut_select$select$r, ignoreInit = TRUE)
  observe({
    r <- input$setup$changes$changes[[1]][[1]] + 1
    c <- input$setup$changes$changes[[1]][[2]] + 1
    v <- input$setup$changes$changes[[1]][[4]]
    setupTable[r,c] <<- ifelse(v == "", as.numeric(NA), v)
  }) %>% bindEvent(input$setup$changes$changes)
  # observe changes edit
  observe({
    r <- input$lut$changes$changes[[1]][[1]] + 1
    c <- input$lut$changes$changes[[1]][[2]] + 1
    v <- input$lut$changes$changes[[1]][[4]]
    lutTable[r,c] <<- ifelse(v == "", as.numeric(NA), v)
    # rework fitted data
    fitlong <- reshape(lutTable, direction = "long", idvar = "pix", varying = 2:ncol(lutTable), sep = "")[,c(1,3)]
    fitlong <- fitlong[!is.na(fitlong$lum),] # keep only valid data
    if(length(fitlong$lum) > 12)
      fit <- tryCatch(predict(loess(lum ~ pix, data = fitlong, span = 0.5, degree = 2),
                              newdata = data.frame(pix = min(fitlong$pix):max(fitlong$pix))),
                      error = function(e) NULL)
    else fit <- NULL
    if(!is.null(fit)) {
      fit[fit < 0] <- 0
      lutFit$y[1:length(fit)] <<- fit
    }
  }) %>% bindEvent(input$lut$changes$changes)
  observe({
    if(input$name == "")
      errorMessage("Name for the gamma profile missing")
    else if(any(is.na(lutTable$lum1)))
      errorMessage("Missing measurements. Please complete at least the first column")
    else if(input$name %in% names(gammaf))
      showModal(modalDialog(
        title = "Gamma profile",
        "A gamma profile with this name already exists. Overwrite?",
        footer = tagList(actionButton(ns("overwrite"), "Yes"), modalButton("Cancel"))
      ))
    else { # if new
      # delete empty columns
      new <- list(lut = lutFit, measurements = lutTable[,sapply(lutTable, function(col) !all(is.na(col)))])
      gammaf[[length(gammaf) + 1]] <<- new
      names(gammaf)[length(gammaf)] <<- input$name
      save(gammaf, file = "config/gammaf.rda")
      showModal(modalDialog( title = "Gamma profile", "Gamma profile saved", easyClose = TRUE))
    }
  }) %>% bindEvent(input$save, ignoreInit = TRUE)
  observe({
    removeModal()
    gammaf[[input$name]]$lut <<- lutFit$y
    gammaf[[input$name]]$measurements <<- lutTable[,sapply(lutTable, function(col) !all(is.na(col)))]
    save(gammaf, file = "config/gammaf.rda")
  }) %>% bindEvent(input$overwrite)
}