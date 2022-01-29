mocsTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, htmlOutput(ns("selected"))),
      column(8,
        fluidRow(
          if(appParams$runType == "luminance")
            column(6, selectInput(ns("size"), "Stimulus Size", choices = c("Size I", "Size II", "Size III", "Size IV", "Size V", "Size VI"), selected = "Size V"))
          else if(appParams$runType == "size")
            column(6, numericInput(ns("lum"), "Stimulus Luminance (cd/m2)", value = 15))
        ),
        fluidRow(
          column(6, selectInput(ns("eye"), "Eye", choices = c("OD", "OS"), selected = "OD")),
          column(6, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[1]))
        )
      )
    ),
    fluidRow(column(12, htmlOutput(ns("msgconn")))),
    fluidRow(
      column(3, actionButton(ns("init"),  label = "Initialize OPI", width = "100%")),
      column(2, actionButton(ns("fovea"), label = "Test fovea", width = "100%", disabled = TRUE)),
      column(3, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE)),
      column(4, align = "center",
        actionButton(ns("run"),   label = "Run", disabled = TRUE),
        actionButton(ns("pause"), label = "Pause", disabled = TRUE),
        actionButton(ns("stop"),  label = "Stop", disabled = TRUE)
      )
    ),
    p(),
    fluidRow(br(),
      column(8, plotOutput(ns("plotres"))),
      column(4, htmlOutput(ns("textres")))
    ),
    fluidRow(br(),
      column(8, textInput(ns("comments"), label = "Comments", width = "100%"))
    ),
    fluidRow(br(),
      column(4, disabled(actionButton(ns("save"), label = "Save", width = "100%"))),
      column(4, disabled(actionButton(ns("cancel"), label = "Cancel", width = "100%")))
    )
  )
}

mocsTest <- function(input, output, session) {
}