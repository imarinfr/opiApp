settingsUI <- function(id) {
  # init gamma file choices and selection
  choices  <- dir("../config/", pattern = "*.csv")
  selected <- ""
  if(length(choices) == 0) choices  <- "-"
  else if(appParams$gammaFile %in% choices) selected <- appParams$gammaFile
  ns <- NS(id)
  tagList(
    fluidRow(
      h4("Perimetry type"),
      column(4, selectInput(ns("machine"),  "OPI implementation", choices = c("PhoneVR", "OpenGL", "Octopus900", "SimYes", "SimNo"), selected = appParams$machine)),
      column(3, radioButtons(ns("runType"), "Perimetry type", c("luminance", "size"), appParams$runType)),
      column(5, textInput(ns("resPath"),    "Results path", appParams$resPath)),
    ),
    fluidRow(
      h4("Device parameters"),
      column(4,
        h4("Phone VR"),
        column(12, textInput(ns("phoneIP"),      "Phone IP",   appParams$phoneIP)),
        column(12, numericInput(ns("phonePort"), "Phone Port", appParams$phonePort))
      ),
      column(4,
        h4("openGL"),
        column(6, "work in progress")
      ),
      column(4,
             h4("Octopus 900"),
             column(6, "work in progress")
      )
    ),
    fluidRow(
      h4("Stimulus parameters"),
      column(3, numericInput(ns("bg"),       "Background", appParams$bg)),
      column(3, selectInput(ns("color"),     "Stimulus color:", choices = c("white", "green", "red", "blue"), selected = appParams$color)),
      column(6, selectInput(ns("gammaFile"), "Gamma file:", choices = choices, selected = selected)),
    ),
    fluidRow(
      column(3, numericInput(ns("minlum"),  "Min luminance",  appParams$minlum)),
      column(3, numericInput(ns("maxlum"),  "Max luminance",  appParams$maxlum)),
      column(3, numericInput(ns("mindiam"), "Min diameter",   appParams$mindiam)),
      column(3, numericInput(ns("maxdiam"), "Max diameter",   appParams$maxdiam))
    ),
    h4("ZEST parameters"),
    fluidRow(
      column(3, numericInput(ns("presTime"),        "Pres time (ms):",     appParams$presTime)),
      column(3, numericInput(ns("respWindow"),      "Resp window (ms):",   appParams$respWindow)),
      column(3, numericInput(ns("respWinPed"),      "Resp pedestal (ms):", appParams$respWinPed)),
      column(3, numericInput(ns("respTimesLength"), "Resp list length",    appParams$respTimesLength)),
    ),
    fluidRow(
      column(3, numericInput(ns("minISI"), "min ISI (ms):", appParams$minISI)),
      column(3, numericInput(ns("fprate"), "FP catch rate", appParams$fprate)),
      column(3, numericInput(ns("fnrate"), "FN catch rate", appParams$fnrate))
    ),
    fluidRow(
      column(3, actionButton(ns("saveSettings"), "Save settings"), offset = 3),
      column(3, actionButton(ns("loadSettings"), "Load settings"))
    )
  )
}

settings <- function(input, output, session) {
  ####################
  # EVENTS
  ####################
  # if any input has changed, then update appParams
  lapply(names(appParams), function(par) {
    observeEvent(input[[par]], {
      if(substr(input[["machine"]], 1, 3) == "Sim")
        updateRadioButtons(session, "runType", selected = "luminance")
      appParams[[par]] <<- input[[par]]
      settingsChanged(FALSE)
      settingsChanged(TRUE)
    }, ignoreInit = TRUE)
  })
  # check if input OK or not
  observeEvent(settingsChanged(), {
    if(!dir.exists(appParams$resPath))
      errorMessage("Make sure the folder for the results exists")
    if(substr(appParams$machine, 1, 3) == "Sim")
      appParams$machine <- "Simulation"
  })
  # save default values
  observeEvent(input$saveSettings, {
    save(appParams, file = "../config/appParams.rda")
  }, ignoreInit = TRUE)
  # load default values
  observeEvent(input$loadSettings, {
    load("../config/appParams.rda", envir = environment(server))
    populateDefaults(session)
  }, ignoreInit = TRUE)
}
####################
# ROUTINES
####################
# error message
errorMessage <- function(txt) {
  showModal(modalDialog(
    title = HTML("<span style = 'color:red'>Error Message</span>"),
    HTML(paste0("<span style = 'color:red'>", txt, "</span>")),
    easyClose = TRUE))
}
# populate screen fields with saved parameter values
populateDefaults <- function(session) {
  updateSelectInput(session,  "machine",         selected = appParams$machine)
  updateRadioButtons(session, "runType",         selected = appParams$runType)
  updateTextInput(session,    "resPath",         value    = appParams$resPath)
  updateTextInput(session,    "phoneIP",         value    = appParams$phoneIP)
  updateNumericInput(session, "phonePort",      value     = appParams$phonePort)
  updateNumericInput(session, "bg",              value    = appParams$bg)
  updateSelectInput(session,  "color",           selected = appParams$color)
  updateTextInput(session,    "gammaFile",       value    = appParams$gammaFile)
  updateNumericInput(session, "minlum",          value    = appParams$minlum)
  updateNumericInput(session, "maxlum",          value    = appParams$maxlum)
  updateNumericInput(session, "mindiam",         value    = appParams$mindiam)
  updateNumericInput(session, "maxdiam",         value    = appParams$maxdiam)
  updateNumericInput(session, "presTime",        value    = appParams$presTime)
  updateNumericInput(session, "respWindow",      value    = appParams$respWindow)
  updateNumericInput(session, "respWinPed",      value    = appParams$respWinPed)
  updateNumericInput(session, "respTimesLength", value    = appParams$respTimesLength)
  updateNumericInput(session, "minISI",          value    = appParams$minISI)
  updateNumericInput(session, "fprate",          value    = appParams$fprate)
  updateNumericInput(session, "fnrate",          value    = appParams$fnrate)
}