settingsUI <- function(id) {
  ns <- NS(id)
  opiImpl <- c("PhoneHMD", "Compass", "imo", "Octopus900", "SimHenson", "SimYes", "SimNo")
  # init gamma file choices and selection
  choices  <- names(gammaf)
  if(length(choices) == 0) choices  <- "-"
  else if(appParams$gamma %in% choices) selected <- appParams$gamma
  tagList(
    fluidRow(
      column(3, selectInput(ns("machine"), "OPI implementation", choices = opiImpl, selected = appParams$machine))
    ),
    htmlOutput(ns("device")),
    fluidRow(
      column(3, textInput(ns("ip"), "IP", appParams$ip)),
      column(2, numericInput(ns("port"), "Port", appParams$port)),
      column(7, textInput(ns("O900path"), "Eye Suite Path", appParams$O900path))
    ),
    fluidRow(
      column(4, selectInput(ns("gammaf"), "Gamma function", choices = choices, selected = appParams$gammaf)),
      column(4, radioButtons(ns("O900max"), "Octopus maximum", choiceNames = list("4000 asb", "10000 asb"), choiceValues = list(FALSE, TRUE), selected = appParams$O900max, inline = TRUE)),
      column(4, radioButtons(ns("O900wheel"), "Big Wheel", choiceNames = list("yes", "no"), choiceValues = list(TRUE, FALSE), selected = appParams$O900wheel, inline = TRUE))
    ),
    htmlOutput(ns("pars")),
    fluidRow(
      column(2, colourInput(ns("stcol"), "Stim color", appParams$stcol)),
      column(2, numericInput(ns("bglum"), "Bg lum", appParams$bglum)),
      column(2, colourInput(ns("bgcol"), "Bg color", appParams$bgcol)),
      column(2, selectInput(ns("fixtype"), "Fix type", choices = c("cross", "maltese", "annulus"), selected = appParams$fixtype)),
      column(2, numericInput(ns("fixlum"), "Fix lum", appParams$fixlum)),
      column(2, colourInput(ns("fixcol"), "Fix color", appParams$fixcol))
    ),
    htmlOutput(ns("general")),
    fluidRow(
      column(2, numericInput(ns("dbstep"), "Step (dB)", appParams$dbstep)),
      column(2, numericInput(ns("fprate"), "FP rate", appParams$fprate)),
      column(2, numericInput(ns("fnrate"), "FN rate", appParams$fnrate))
    ),
    fluidRow(
      column(2, numericInput(ns("presTime"), "Pres time", appParams$presTime)),
      column(2, numericInput(ns("respWin"), "Resp window", appParams$respWin)),
      column(2, numericInput(ns("winFloor"), "Window floor", appParams$winFloor)),
      column(2, numericInput(ns("minRespWin"), "Min window", appParams$minRespWin)),
      column(2, numericInput(ns("slidWidth"), "Slid width", appParams$slidWidth)),
      column(2, numericInput(ns("minISI"), "min ISI", appParams$minISI))
    ),
    htmlOutput(ns("specific")),
    fluidRow(
      column(2, numericInput(ns("lumForSize"), "Default lum", appParams$lumForSize)),
      column(2, numericInput(ns("sizeForLum"), "Default size", appParams$sizeForLum)),
      column(2, numericInput(ns("estSD"), "ZEST SD", appParams$estSD)),
      column(2, numericInput(ns("range"), "MOCS range", appParams$range)),
      column(2, numericInput(ns("nreps"), "MOCS reps", appParams$nreps))
    ),
    fluidRow(
      column(3, actionButton(ns("saveSettings"), "Save settings"), offset = 3),
      column(3, actionButton(ns("loadSettings"), "Load settings"))
    )
  )
}

settings <- function(input, output, session) {
  #########
  # Outputs
  #########
  output$device <- renderText("<h5><b>OPI configuration</b></h>")
  output$pars <- renderText("<h5><b>Stimulus and background parameters</b></h>")
  output$general <- renderText("<h5><b>General test parameters</b></h>")
  output$specific <- renderText("<h5><b>Specific test parameters</b></h>")
  ########
  # Events
  ########
  # if any input has changed, then update appParams
  lapply(names(appParams), function(par) {
    observe({
      appParams[[par]] <<- input[[par]]
      settingsChanged(FALSE)
      settingsChanged(TRUE)
    }) %>% bindEvent(input[[par]], ignoreInit = TRUE)
  })
  # special treatment fields
  observe({
    appParams$lut <<- gammaf[[appParams$gammaf]]$lut
  }) %>% bindEvent(input$gammaf)
  observe({
    appParams$O900wheel <<- as.logical(input$O900wheel)
  }) %>% bindEvent(input$O900wheel)
  observe({
    appParams$O900max <<- as.logical(input$O900max)
  }) %>% bindEvent(input$O900max)
  # save default values
  observe({
    save(appParams, file = "config/appParams.rda")
  }) %>% bindEvent(input$saveSettings, ignoreInit = TRUE)
  # load default values
  observe({
    load("config/appParams.rda", envir = environment(server))
    populateDefaults(session)
  }) %>% bindEvent(input$loadSettings, ignoreInit = TRUE)
}
####################
# ROUTINES
####################
# populate screen fields with saved parameter values
populateDefaults <- function(session) {
  updateSelectInput(session, "machine", selected = appParams$machine)
  updateTextInput(session, "ip", value = appParams$ip)
  updateNumericInput(session, "port", value = appParams$port)
  updateTextInput(session, "gammaf", value = appParams$gammaf)
  updateNumericInput(session, "bglum", value = appParams$bglum)
  updateColourInput(session, "bgcol", value = appParams$bgcol)
  updateSelectInput(session, "fixtype", selected = appParams$fixtype)
  updateColourInput(session, "fixlum", value = appParams$fixlum)
  updateColourInput(session, "fixcol", value = appParams$fixcol)
  updateColourInput(session, "stcol", value = appParams$stcol)
  updateNumericInput(session, "dbstep", value = appParams$dbstep)
  updateNumericInput(session, "presTime", value = appParams$presTime)
  updateNumericInput(session, "respWin", value = appParams$respWin)
  updateNumericInput(session, "winFloor", value = appParams$winFloor)
  updateNumericInput(session, "minRespWin", value = appParams$minRespWin)
  updateNumericInput(session, "slidWidth", value = appParams$slidWidth)
  updateNumericInput(session, "minISI", value = appParams$minISI)
  updateNumericInput(session, "fprate", value = appParams$fprate)
  updateNumericInput(session, "fnrate", value = appParams$fnrate)
  updateNumericInput(session, "estSD", value = appParams$estSD)
  updateNumericInput(session, "nreps", value = appParams$nreps)
  updateNumericInput(session, "range", value = appParams$range)
  updateNumericInput(session, "lumForSize", value = appParams$lumForSize)
  updateNumericInput(session, "sizeForLum", value = appParams$sizeForLum)
  updateTextInput(session, "O900path", value = appParams$O900path)
  updateRadioButtons(session, "O900wheel", selected = appParams$O900wheel)
  updateRadioButtons(session, "O900max", selected = appParams$O900max)
}