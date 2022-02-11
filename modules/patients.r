patientsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(10, offset = 1, div(dataTableOutput(ns("patientdb")), style = "font-size: 75%"))),
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
  ########
  # Events
  ########
  # if selected patient db has changed
  observe({
    if(patientdbChanged()) {
      patientdbChanged(FALSE)
      patientOut        <- patientTable[,1:6]
      patientOut$dob    <- format(as.Date(patientOut$dob), "%Y-%m-%d")
      names(patientOut) <- c("ID", "Name", "Surname", "Date of Birth", "Gender", "Type")
      patientOut <- datatable(patientOut, selection = "single", rownames = FALSE,
                              options = list(pageLength = 10, lengthChange = FALSE))
      patientOut <- formatStyle(patientOut, columns = 0, target = "row", lineHeight = "75%")
      output$patientdb  <- renderDataTable(patientOut)
    }
  }) %>% bindEvent(patientdbChanged(), ignoreInit = TRUE)
  # selected patient
  observe({
    newPatient <<- FALSE
    enable("newPatient")
    if(psel()) { # check if a patient has been selected or deselected
      enable("delPatient")
      disableMandatoryFields()
      showPatientFields()
      fillPatientFields(session, input$patientdb_rows_selected, patientTable)
      patient(selectPatient(input$patientdb_rows_selected, patientTable))
    } else {
      disable("delPatient")
      hidePatientFields()
      patient(initPatient())
    }
  }) %>% bindEvent(psel(), ignoreInit = TRUE)
  # create new patient
  observe({ # add new patient
    newPatient <<- TRUE
    disable("newPatient")
    disable("delPatient")
    clearPatientFields(session)
    showPatientFields()
    enableMandatoryFields()
  }) %>% bindEvent(input$newPatient)
  # delete existing patient
  observe({ # delete existing patient: need to confirm
    showModal(modalDialog(
      title = "Delete patient?",
      "Are you sure you want to delete the selected patient?",
      footer = tagList(actionButton(ns("delok"), "Yes"), modalButton("No"))
    ))
  }) %>% bindEvent(input$delPatient, ignoreInit = TRUE)
  # confirm delete
  observe({
    patientTable <<- deletePatient(input$patientdb_rows_selected, patientTable)
    patientdbChanged(TRUE)
    removeModal()
  }) %>% bindEvent(input$delok, ignoreInit = TRUE)
  # save patient
  observe({ # save new patient
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
  }) %>% bindEvent(input$save, ignoreInit = TRUE)
  # cancel and do not create or edit
  observe({ # cancel save and do not save
    enable("newPatient")
    disableMandatoryFields()
    hidePatientFields()
  }) %>% bindEvent(input$cancel, ignoreInit = TRUE)
}