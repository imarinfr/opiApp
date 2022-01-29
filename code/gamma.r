gammaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        column(12, numericInput(ns("nreps"), "Repetitions", 3, min = 1, max = 3)),
      ),
      column(8,
        column(4, align = "center", numericInput(ns("fr1"), "Pixel From", 0)),
        column(4, align = "center", numericInput(ns("by1"), "Pixel Step", 5)),
        column(4, align = "center", numericInput(ns("to1"), "Pixel To", 25)),
        column(4, align = "center", numericInput(ns("fr2"), "", 25)),
        column(4, align = "center", numericInput(ns("by2"), "", 25)),
        column(4, align = "center", numericInput(ns("to2"), "", 225)),
        column(4, align = "center", numericInput(ns("fr3"), "", 225)),
        column(4, align = "center", numericInput(ns("by3"), "", 5)),
        column(4, align = "center", numericInput(ns("to3"), "", 255))
      )
    ),
    fluidRow(htmlOutput(ns("msgconn"))),
    fluidRow(
      column(3, actionButton(ns("init"),  label = "Initialize OPI", width = "100%")),
      column(3, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE))
    ),
    fluidRow(br(),
      column(5, rHandsontableOutput(ns("lut"))),
      column(7, plotOutput(ns("plotlut")))
    ),
    fluidRow(br(),
      column(12, align = "center", hidden(shinySaveButton(ns("save"), "Save LUT", "Save LUT as CSV file", filetype = "csv")))
    )
  )
}

gamma <- function(input, output, session) {
  ns <- session$ns
  lutTable <- NULL
  lutFit <- data.frame(x = 0:255, y = 0)
  roots <- c("config" = "../config/", "wd" = ".", "home" = "~")
  makeReactiveBinding("lutTable")
  # messages for status o connection, etc
  msg <- reactiveVal("Press 'Initialize OPI' to start")
  # outputs lut, plot and messages
  output$lut     <- NULL
  output$plotlut <- NULL
  output$msgconn <- renderText(msg())
  ####################
  # EVENTS
  ####################
  # initialize OPI
  observeEvent(input$init, {
    opiParams <- fillOpiParams()
    if(appParams$machine == "PhoneVR" | appParams$machine == "Display")
      opiParams$lut <- 0:255 # LUT is pixel value itself for testing purposes
    do.call(what = opiInitialize, args = opiParams)
    if(appParams$machine == "PhoneVR")
      do.call(opiSetBackground, list(bgeye = "B", bglum = 0, bgcol = appParams$color))
    if(appParams$machine == "Display")
      do.call(opiSetBackground, list(lum = 0, color = appParams$color))
    msg("OPI connection opened")
    disable("init")
    enable("close")
    showElement("save")
    disableAll()
    lutTable <<- generateLUTtable(input$nreps, input$fr1, input$by1, input$to1, input$by2, input$to2, input$by3, input$to3)
    lutFit   <<- data.frame(x = 0:255, y = 0)
    output$lut <- renderRHandsontable(handsontable(lutTable))
    output$plotlut <- renderPlot(lutPlot(lutTable, lutFit))
    opiInitialized(TRUE)
  }, ignoreInit = TRUE)
  # close OPI connection
  observeEvent(input$close, {
    opiClose()
    msg("OPI connection closed")
    enable("init")
    disable("close")
    hideElement("save")
    enableAll()
    lutTable       <- NULL
    output$lut     <- NULL
    output$plotlut <- NULL
    opiInitialized(FALSE)
  }, ignoreInit = TRUE)
  # update fields from pixel level to keep segments consistent
  observeEvent(input$to1, updateNumericInput(session, "fr2", value = input$to1))
  observeEvent(input$to2, updateNumericInput(session, "fr3", value = input$to2))
  # select row
  observeEvent(input$lut_select$select$r, {
    if(opiInitialized() & (appParams$machine == "PhoneVR"))
      do.call(opiSetBackground, list(bgeye = "B", bglum = lutTable$pix[input$lut_select$select$r], bgcol = appParams$color))
    if(opiInitialized() & appParams$machine == "Display")
      do.call(opiSetBackground, list(bglum = lutTable$pix[input$lut_select$select$r]))
    
  }, ignoreInit = TRUE)
  # observe changes edit
  observeEvent(input$lut$changes$changes, {
    r <- input$lut$changes$changes[[1]][[1]] + 1
    c <- input$lut$changes$changes[[1]][[2]] + 1
    v <- input$lut$changes$changes[[1]][[4]]
    lutTable[r,c] <<- ifelse(v == "", as.numeric(NA), v)
    # rework fitted data
    fitlong <- reshape(lutTable, direction = "long", idvar = "pix", varying = 2:ncol(lutTable), sep = "")[,c(1,3)]
    fitlong <- fitlong[!is.na(fitlong$lum),] # keep only valid data
    if(length(fitlong$lum) > 8)
      fit <- tryCatch(predict(loess(lum ~ pix, data = fitlong, span = 0.5, degree = 2),
                              newdata = data.frame(pix = min(fitlong$pix):max(fitlong$pix))),
                      error = function(e) NULL)
    else fit <- NULL
    if(!is.null(fit)) {
      fit[fit < 0] <- 0
      lutFit$y[1:length(fit)] <<- fit
    }
  })
  shinyFileSave(input, "save", roots = roots)
  observeEvent(input$save, {
    fname <- parseSavePath(roots, input$save)$datapath
    if(length(fname) > 0) write.table(lutFit$y, file = fname, sep = ",", row.names = FALSE, col.names = FALSE)
  }, ignoreInit = TRUE)
}
# disable and enable all fields
disableAll <- function()
  lapply(c("nreps", "by1", "to1", "by2", "to2", "by3"), disable)
enableAll <- function()
  lapply(c("nreps", "by1", "to1", "by2", "to2", "by3"), enable)
# generate LUT table
generateLUTtable <- function(nreps, fr1, by1, to1, by2, to2, by3, to3) {
  lutTable <- data.frame(pix = unique(c(seq(fr1, to1, by = by1), seq(to1, to2, by = by2), seq(to2, to3, by = by3))))
  lutTable$lum1 <- as.numeric(NA)
  if(nreps > 1) lutTable$lum2 <- as.numeric(NA)
  if(nreps > 2) lutTable$lum3 <- as.numeric(NA)
  return(lutTable)
}
# plot LUT results so far
lutPlot <- function(lutTable, lutFit) {
  pix     <- lutTable$pix
  lum     <- apply(lutTable[,2:ncol(lutTable)], 1, mean, na.rm = TRUE) # mean
  lum2sem <- 2 * apply(lutTable[,2:ncol(lutTable)], 1, sd, na.rm = TRUE) / sqrt(ncol(lutTable) - 1) # 2 SEM
  lum2sem[is.nan(lum2sem)] <- 0
  par(mar = c(8, 4, 6, 1))
  xmax <- ifelse(all(is.nan(lum)), 0, max(lum[!is.nan(lum)]))
  if(xmax < 200) xmax <- 200
  plot(0, 0, typ = "n", xlim = c(0, 255), ylim = c(0, xmax),
       panel.first = grid(), xlab = "pixel value", ylab = "luminance (cd/m2)")
  arrows(pix, lum - lum2sem, pix, lum + lum2sem, length = 0, angle = 90)
  points(pix, lum, pch = 21, bg = "white")
  lines(lutFit$x, lutFit$y, col = "red")
}
# generate handsonetable
handsontable <- function(tab)
  return(hot_col(rhandsontable(tab, rowHeaders = NULL, selectCallback = TRUE, height = 400), 1, readOnly = TRUE))