settingsUI <- function(id) {
  # init gamma file choices and selection
  choices  <- names(gammaf)
  if(length(choices) == 0) choices  <- "-"
  else if(appParams$gamma %in% choices) selected <- appParams$gamma
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectInput(ns("machine"), "OPI implementation", choices = c("PhoneVR", "Octopus900", "SimHenson", "SimYes", "SimNo"), selected = appParams$machine))
    ),
    htmlOutput(ns("device")),
    fluidRow(
      column(3, textInput(ns("ip"), "IP", appParams$ip)),
      column(2, numericInput(ns("port"), "Port", appParams$port)),
      column(7, textInput(ns("O900path"), "Eye Suite Path", appParams$O900path))
    ),
    fluidRow(
      column(4, selectInput(ns("gammaf"), "Gamma function", choices = choices, selected = appParams$gammaf)),
      column(4, radioButtons(ns("O900max"), "Max Luminance", choiceNames = list("4000 asb", "10000 asb"), choiceValues = list("false", "true"), selected = appParams$O900max, inline = TRUE)),
      column(4, radioButtons(ns("O900wheel"), "Big Wheel", choiceNames = list("yes", "no"), choiceValues = list("true", "false"), selected = appParams$O900wheel, inline = TRUE))
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
      column(2, numericInput(ns("minlum"), "Min lum", appParams$minlum)),
      column(2, numericInput(ns("maxlum"), "Max lum", appParams$maxlum)),
      column(2, numericInput(ns("mindiam"), "Min diam", appParams$mindiam)),
      column(2, numericInput(ns("maxdiam"), "Max diam", appParams$maxdiam)),
      column(2, numericInput(ns("fprate"), "FP rate", appParams$fprate)),
      column(2, numericInput(ns("fnrate"), "FN rate", appParams$fnrate)),
      column(2, numericInput(ns("presTime"), "Pres time", appParams$presTime)),
      column(2, numericInput(ns("respWin"), "Resp window", appParams$respWin)),
      column(2, numericInput(ns("minRespWin"), "Min Resp Win", appParams$minRespWin)),
      column(2, numericInput(ns("slidWidth"), "Slid width", appParams$slidWidth)),
      column(2, numericInput(ns("minISI"), "min ISI", appParams$minISI))
    ),
    htmlOutput(ns("specific")),
    fluidRow(
      column(3, numericInput(ns("lumForSize"), "Fixed size for luminance perimetry", appParams$lumForSize)),
      column(3, numericInput(ns("sizeForLum"), "Fixed luminance for size perimetry", appParams$sizeForLum)),
      column(2, numericInput(ns("est"), "Initial estimate", appParams$est)),
      column(2, numericInput(ns("estSD"), "maximum SD (ZEST)", appParams$estSD)),
      column(2, numericInput(ns("nreps"), "Repetitions (MOCS)", appParams$nreps))
    ),
    fluidRow(
      column(3, actionButton(ns("saveSettings"), "Save settings"), offset = 3),
      column(3, actionButton(ns("loadSettings"), "Load settings"))
    )
  )
}

settings <- function(input, output, session) {
  output$device <- renderText("<h5><b>OPI configuration</b></h>")
  output$pars <- renderText("<h5><b>Stimulus and background parameters</b></h>")
  output$general <- renderText("<h5><b>General test parameters</b></h>")
  output$specific <- renderText("<h5><b>Specific test parameters</b></h>")
  ####################
  # EVENTS
  ####################
  # if any input has changed, then update appParams
  lapply(names(appParams), function(par) {
    observeEvent(input[[par]], {
      appParams[[par]] <<- input[[par]]
      settingsChanged(FALSE)
      settingsChanged(TRUE)
    }, ignoreInit = TRUE)
  })
  # special treatment fields
  observeEvent(input$gammaf, {
    appParams$lut <<- gammaf[[appParams$gammaf]]$lut
  })
  observeEvent(input$O900wheel, {
    appParams$O900wheel <<- as.logical(input$O900wheel)
  })
  observeEvent(input$O900max, {
    appParams$O900max <<- as.logical(input$O900max)
  })
  
  # check if input OK or not
  observeEvent(settingsChanged(), {
    disableAllConfigFields()
    if(appParams$machine == "PhoneVR") {
      enableConfigFields(c("device", "ip", "port", "gammaf",
                           "pars", "stcol", "bglum", "bgcol", "fixtype", "fixlum", "fixcol"))
    } else if(appParams$machine == "Octopus900") {
      enableConfigFields(c("device", "port", "O900path", "O900max", "O900wheel"))
    }
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
  updateNumericInput(session, "minlum", value = appParams$minlum)
  updateNumericInput(session, "maxlum", value = appParams$maxlum)
  updateNumericInput(session, "mindiam", value = appParams$mindiam)
  updateNumericInput(session, "maxdiam", value = appParams$maxdiam)
  updateNumericInput(session, "presTime", value = appParams$presTime)
  updateNumericInput(session, "respWin", value = appParams$respWin)
  updateNumericInput(session, "minRespWin", value = appParams$minRespWin)
  updateNumericInput(session, "slidWidth", value = appParams$slidWidth)
  updateNumericInput(session, "minISI", value = appParams$minISI)
  updateNumericInput(session, "fprate", value = appParams$fprate)
  updateNumericInput(session, "fnrate", value = appParams$fnrate)
  updateNumericInput(session, "estSD", value = appParams$estSD)
  updateNumericInput(session, "nreps", value = appParams$nreps)
  updateNumericInput(session, "lumForSize", value = appParams$lumForSize)
  updateNumericInput(session, "sizeForLum", value = appParams$sizeForLum)
  updateNumericInput(session, "est", value = appParams$est)
  updateTextInput(session, "O900path", value = appParams$O900path)
  updateRadioButtons(session, "O900wheel", selected = appParams$O900wheel)
  updateRadioButtons(session, "O900max", selected = appParams$O900max)
}

enableConfigFields <- function(fields)
  lapply(fields, enable)

disableAllConfigFields <- function()
  lapply(c("config", "ip", "port", "gammaf", "O900path", "O900max", "O900wheel",
           "pars", "stcol", "bglum", "bgcol", "fixtype", "fixlum", "fixcol"), disable)