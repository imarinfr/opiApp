library(OPI)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(colourpicker)
library(rhandsontable)
library(DT)
library(RColorBrewer)
library(plotrix)
library(txtq)
library(future)
library(promises)
# prepare to run processes in parallel
plan(multisession, split = TRUE)
# remove all threads at the end of all things
onStop(function() plan(sequential))

source("settings.r", local = TRUE)
source("gprofile.r", local = TRUE)
source("gridgen.r", local = TRUE)
source("patients.r", local = TRUE)
source("client.r", local = TRUE)
source("report.r", local = TRUE)
source("utils.r", local = TRUE)

# global variables
settingsChanged <- reactiveVal(FALSE)
patient <- reactiveVal(list(id = NA, name = NA, surname = NA, age = NA, gender = NA, type = NA))
patientdbChanged <- reactiveVal(FALSE)
opiInitialized <- reactiveVal(FALSE)
newReports <- reactiveVal(FALSE)
ShinySender <- txtq(tempfile())  # Messages from GUI to test server
ShinyReceiver <- txtq(tempfile())  # Messages from test server to GUI

# load global parameters appParams, then the patient db, then the grids
if(!file.exists("../config/appParams.rda"))
  stop("please create file appParams.rda to start using the application")
load("../config/appParams.rda")
if(!file.exists("../config/gammaf.rda"))
  stop("please create file gammaf.rda to start using the application")
load("../config/gammaf.rda")
if(!file.exists("../config/grids.rda"))
  stop("please create file grids.rda to start using the application")
load("../config/grids.rda")
if(!file.exists("../config/patientdb.rda"))
  stop("please create file patientdb.rda to start using the application")
load("../config/patientdb.rda")

ui <- dashboardPage(
  dashboardHeader(title = "OPI app"),
  dashboardSidebar(
    useShinyjs(),
    actionButton("settingsbtn", label = "Settings", icon = icon("cog"), width = "90%"),
    actionButton("gammabtn", label = "Gamma function", icon = icon("list-alt"), width = "90%"),
    actionButton("gridgenbtn", label = "Grid generator", icon = icon("border-none"), width = "90%"),
    actionButton("patientsbtn", label = "Patients", icon = icon("user"), width = "90%", disabled = TRUE),
    actionButton("clientbtn", label = "Run Test", icon = icon("sad-cry"), width = "90%"),
    actionButton("reportbtn", label = "Reports", icon = icon("file-alt"), width = "90%")
  ),
  dashboardBody(
    uiOutput("tab")
  )
)

server <- function(input, output, session) {
  invisible(chooseOpi("Daydream")) # choose the Daydream as the OPI implementation
  # render all pages
  settingsPage <- renderUI({settingsUI("settings")})
  gprofilePage <- renderUI({gprofileUI("gprofile")})
  gridgenPage <- renderUI({gridgenUI("gridgen")})
  patientsPage <- renderUI({patientsUI("patients")})
  clientPage <- renderUI({clientUI("client")})
  reportPage <- renderUI({reportUI("report")})
  # start the modules
  callModule(settings, "settings")
  callModule(gprofile, "gprofile")
  callModule(gridgen, "gridgen")
  callModule(patients, "patients")
  callModule(client, "client")
  callModule(report, "report")
  browsePage <- "patientsPage"
  output$tab <- patientsPage
  ####################
  # EVENTS
  ####################
  # check setting parameters have changed
  observeEvent(settingsChanged(), {
    if(browsePage == "settingsPage") disable("settingsbtn")
    if(browsePage == "patientsPage") disable("patientsbtn")
    load("../config/gammaf.rda")
    load("../config/grids.rda")
    load("../config/patientdb.rda")
    patientdbChanged(TRUE)
    newReports(TRUE)
  })
  # if OPI is initialized, do not allow to browse anywhere
  observeEvent(opiInitialized(), {
    if(opiInitialized()) disableAll()
    else {
      enableAll()
      if(browsePage == "gprofilePage") disable("gammabtn")
      if(browsePage == "gridgenPage") disable("gridgenbtn")
      if(browsePage == "clientPage") disable("clientbtn")
    }
  }, ignoreInit = TRUE)
  # go to settings page
  observeEvent(input$settingsbtn, {
    browsePage <<- "settingsPage"
    output$tab <<- settingsPage
    disable("settingsbtn")
    lapply(c("gammabtn", "gridgenbtn", "patientsbtn", "clientbtn", "reportbtn"), enable)
  })
  # go to gamma profile page
  observeEvent(input$gammabtn, {
    browsePage <<- "gprofilePage"
    output$tab <<- gprofilePage
    disable("gammabtn")
    lapply(c("settingsbtn", "gridgenbtn", "patientsbtn", "clientbtn", "reportbtn"), enable)
  })
  # go to grid generation page
  observeEvent(input$gridgenbtn, {
    browsePage <<- "gridgenPage"
    output$tab <<- gridgenPage
    disable("gridgenbtn")
    lapply(c("settingsbtn", "gammabtn", "patientsbtn", "clientbtn", "reportbtn"), enable)
  })
  # go to patients page
  observeEvent(input$patientsbtn, {
    browsePage <<- "patientsPage"
    output$tab <<- patientsPage
    disable("patientsbtn")
    lapply(c("settingsbtn", "gammabtn", "gridgenbtn", "clientbtn", "reportbtn"), enable)
  })
  # go to client page
  observeEvent(input$clientbtn, {
    browsePage <<- "clientPage"
    output$tab <<- clientPage
    disable("clientbtn")
    lapply(c("settingsbtn", "gammabtn", "gridgenbtn", "patientsbtn", "reportbtn"), enable)
  })
  # go to reports page
  observeEvent(input$reportbtn, {
    browsePage <<- "reportPage"
    output$tab <<- reportPage
    disable("reportbtn")
    lapply(c("settingsbtn", "gammabtn", "gridgenbtn", "patientsbtn", "clientbtn"), enable)
  })
  # close OPI server
  onSessionEnded(function() ShinySender$push(title = "CMD", message = "opiClose"))
}
# run app
shinyApp(ui, server)