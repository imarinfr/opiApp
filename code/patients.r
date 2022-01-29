patientsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(10, offset = 1, div(dataTableOutput(ns("patientdb")), style = "font-size: 85%"))),
    fluidRow(br(),
      column(4, align = "left", actionButton(ns("newPatient"), "Add new patient", width = "100%")),
      column(4, offset = 4, align = "right",  disabled(actionButton(ns("delPatient"),  "Delete selected patient", width = "100%")))
    ),
    fluidRow(br(),
      column(3, align = "center", hidden(disabled(textInput(ns("id"), "ID")))),
      column(4, align = "center", hidden(disabled(textInput(ns("name"), "Name")))),
      column(5, align = "center", hidden(disabled(textInput(ns("surname"), "Surname"))))
    ),
    fluidRow(
      column(3, align = "center", hidden(dateInput(ns("dob"), "Date of Birth", format = "yyyy-mm-dd"))),
      column(2, align = "center", hidden(textInput(ns("type"), "Type"))),
      column(2, align = "center", hidden(radioButtons(ns("gender"), "Gender", choices = c("F", "M"), selected = character(0), inline = TRUE)))
    ),
    fluidRow(
      column(2, align = "center", hidden(textInput(ns("osva"), "VA OS"))),
      column(2, align = "center", hidden(textInput(ns("osrx"), "Rx OS"))),
      column(2, align = "center", hidden(textInput(ns("osoverrx"), "Over-Rx OS"))),
      column(2, align = "center", hidden(textInput(ns("odva"), "VA OD"))),
      column(2, align = "center", hidden(textInput(ns("odrx"), "Rx OD"))),
      column(2, align = "center", hidden(textInput(ns("odoverrx"), "Over-Rx OD")))
    ),
    fluidRow(
      column(6, align = "center", hidden(textInput(ns("osdiagnostic"), "Diagnostic OS:"))),
      column(6, align = "center", hidden(textInput(ns("oddiagnostic"), "Diagnostic OD:")))
    ),
    fluidRow(
      column(6, align = "center", hidden(textInput(ns("oscomments"), "Comments OS:"))),
      column(6, align = "center", hidden(textInput(ns("odcomments"), "Comments OD:")))
    ),
    fluidRow(
      column(6, align = "center", hidden(actionButton(ns("save"), "Save",   width = "100%"))),
      column(6, align = "center", hidden(actionButton(ns("cancel"), "Cancel", width = "100%")))
    )
  )
}

patients <- function(input, output, session) {
  ns <- session$ns
  psel <- reactive(!is.null(input$patientdb_rows_selected))
  newPatient <- FALSE
  ####################
  # OBSERVE
  ####################
  # if selected patient db has changed
  observe(
    if(patientdbChanged()) {
      patientdbChanged(FALSE)
      patientOut        <- patientTable[,1:6]
      patientOut$dob    <- format(as.Date(patientOut$dob), "%Y-%m-%d")
      names(patientOut) <- c("ID", "Name", "Surname", "Date of Birth", "Gender", "Type")
      output$patientdb  <- renderDataTable(patientOut, rownames = FALSE, server = FALSE, selection = "single",
                                           options = list(pageLength = 5, lengthChange = FALSE))
    }
  )
  ####################
  # EVENTS
  ####################
  # selected patient
  observeEvent(psel(), {
    patientChanged(TRUE)
    newPatient <<- FALSE
    enable("newPatient")
    if(psel()) { # check if a patient has been selected or deselected
      enable("delPatient")
      disableMandatoryFields()
      showPatientFields()
      fillPatientFields(session, input$patientdb_rows_selected, patientTable)
      patient <<- selectPatient(input$patientdb_rows_selected, patientTable)
    } else {
      disable("delPatient")
      hidePatientFields()
      patient <<- initPatient()
    }
  }, ignoreInit = TRUE)
  # create new patient
  observeEvent(input$newPatient, { # add new patient
    newPatient <<- TRUE
    disable("newPatient")
    disable("delPatient")
    clearPatientFields(session)
    showPatientFields()
    enableMandatoryFields()
  })
  # delete existing patient
  observeEvent(input$delPatient, { # delete existing patient: need to confirm
    showModal(modalDialog(
      title = "Delete patient?",
      "Are you sure you want to delete the selected patient?",
      footer = tagList(actionButton(ns("delok"), "Yes"), modalButton("No"))
    ))
  }, ignoreInit = TRUE)
  # confirm delete
  observeEvent(input$delok, {
    patientTable <<- deletePatient(input$patientdb_rows_selected, patientTable)
    patientdbChanged(TRUE)
    removeModal()
  }, ignoreInit = TRUE)
  # save patient
  observeEvent(input$save, { # save new patient
    if(newPatient) { # check if mandatory fields are not empty
      checkRes <- checkNewPatient(input, patientTable)
      if(checkRes$saveok) {
        patientTable <<- saveNewPatient(input, patientTable)
        disableMandatoryFields()
        hidePatientFields()
        newPatient <<- FALSE
        enable("newPatient")
        patientdbChanged(TRUE)
      } else errorMessage(checkRes$errtxt)
    } else {
      patientTable <<- saveModifiedPatient(input, patientTable)
      patientdbChanged(TRUE)
    }
  }, ignoreInit = TRUE)
  # cancel and do not create or edit
  observeEvent(input$cancel, { # cancel save and do not save
    enable("newPatient")
    disableMandatoryFields()
    hidePatientFields()
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
# show or hide fields
showPatientFields <- function()
  lapply(c("id", "name", "surname", "dob", "gender", "type",
           "osva", "osrx", "osoverrx", "odva", "odrx", "odoverrx",
           "osdiagnostic", "oddiagnostic",
           "oscomments", "odcomments",
           "save", "cancel"), showElement)
hidePatientFields <- function()
  lapply(c("id", "name", "surname", "dob", "gender", "type",
           "osva", "osrx", "osoverrx", "odva", "odrx", "odoverrx",
           "osdiagnostic", "oddiagnostic",
           "oscomments", "odcomments",
           "save", "cancel"), hideElement)
# enable or disable fields
enableMandatoryFields <- function()
  lapply(c("id", "name", "surname"), enable)
disableMandatoryFields <- function()
  lapply(c("id", "name", "surname"), disable)
# fill record with patient data
fillRecord <- function(input) {
  return(data.frame(id           = input$id,
                    name         = input$name,
                    surname      = input$surname,
                    dob          = input$dob,
                    gender       = input$gender,
                    type         = input$type,
                    osva         = input$osva,
                    osrx         = input$osrx,
                    osoverrx     = input$osoverrx,
                    osdiagnostic = input$osdiagnostic,
                    oscomments   = input$oscomments,
                    odva         = input$odva,
                    odrx         = input$odrx,
                    odoverrx     = input$odoverrx,
                    oddiagnostic = input$oddiagnostic,
                    odcomments   = input$odcomments,
                    stringsAsFactors = FALSE))
}
fillPatientFields <- function(session, idx, patientTable) {
  textFields <- c("id", "name", "surname", "type",
                  "osva", "osrx", "osoverrx", "odva", "odrx", "odoverrx",
                  "osdiagnostic", "oddiagnostic",
                  "oscomments", "odcomments")
  lapply(textFields, function(field) updateTextInput(session, field, value = patientTable[idx,field]))
  updateDateInput(session, "dob", value = patientTable$dob[idx])
  updateRadioButtons(session, "gender", selected = patientTable$gender[idx])
}
# save new patient
checkNewPatient <- function(input, patientTable) {
  saveok <- TRUE
  errtxt <- ""
  if(input$id          == "" ||
     input$name        == "" ||
     input$surname     == "" ||
     length(input$dob) == 0  ||
     is.null(input$gender)) {
    return(list(saveok = FALSE, errtxt = "please add mandatory fields: ID, Name, Surname, Date of Birth, and Gender"))
  } else if(input$id %in% patientTable$id) {
    # check that there is no other record with the same ID
    return(list(saveok = FALSE, errtxt = "Duplicated ID number"))
  } else if(input$id %in% patientTable$id) { # for same id, name, and surname needs to be same
    idx <- which(patientTable$id == input$id)
    if(input$name    != patientTable$name[idx] ||
       input$surname != patientTable$surname[idx]) {
      return(list(saveok = FALSE, errtxt = "Records with the same ID must have the same Name, Surname, and Date of Birth"))
    }
  }
  return(list(saveok = TRUE, errtxt = ""))
}
# save new patient
saveNewPatient <- function(input, patientTable) {
  df           <- fillRecord(input)
  df$created   <- format(Sys.time(), "%m/%d/%Y %H:%M:%S")
  df$modified  <- df$created
  patientTable <- rbind(patientTable, df) # append new record
  patientTable <- patientTable[order(patientTable$id),] # sort data
  save(patientTable, file = "../db/patientdb.rda")
  return(patientTable)
}
# save modified patient
saveModifiedPatient <- function(input, patientTable) {
  idx                <- input$patientdb_rows_selected         # idx of patient to modify
  df                 <- fillRecord(input)
  df$created         <- patientTable$created[idx]
  df$modified        <- format(Sys.time(), "%m/%d/%Y %H:%M:%S")
  patientTable[idx,] <- df                                    # modify record
  patientTable       <- patientTable[order(patientTable$id),] # sort data by ID
  save(patientTable, file = "../db/patientdb.rda")
  return(patientTable)
}
# delete patient
deletePatient <- function(idx, patientTable) {
  patientTable <- patientTable[-idx,] # delete record
  save(patientTable, file = "../db/patientdb.rda")
  return(patientTable)
}
# clear all data from fields
clearPatientFields <- function(session) {
  textFields <- c("id", "name", "surname", "type",
                  "osva", "osrx", "osoverrx", "odva", "odrx", "odoverrx",
                  "osdiagnostic", "oddiagnostic",
                  "oscomments", "odcomments")
  lapply(textFields, function(field) updateTextInput(session, field, value = ""))
  updateDateInput(session, "dob")
  updateRadioButtons(session, "gender", choices = c("F", "M"), selected = character(), inline = TRUE)
}
# select patient
selectPatient <- function(idx, patientTable)
  return(list(id      = patientTable$id[idx],
              name    = patientTable$name[idx],
              surname = patientTable$surname[idx],
              age     = getPatientAge(patientTable$dob[idx], Sys.Date()),
              gender  = patientTable$gender[idx],
              type    = patientTable$type[idx]))
# init selected patient
initPatient <- function()
  return(list(id = NA, name = NA, surname = NA, dob = NA, gender = NA, type = NA))
# get patient's age
getPatientAge <- function(dob, date) {
  dob  <- as.POSIXlt(dob)
  date <- as.POSIXlt(date)
  age  <- date$year - dob$year
  # if month of DoB has not been reached yet, then a year younger
  idx <- which(date$mon < dob$mon)
  if(length(idx) > 0) age[idx]  <- age[idx] - 1
  # if same month as DoB but day has not been reached, then a year younger
  idx <- which(date$mon == dob$mon & date$mday < dob$mday)
  if(length(idx) > 0) age[idx]  <- age[idx] - 1
  return(age)
}