#######################
# Multipurpose routines
#######################
# error text
errortxt <- function(txt) return(paste("<span style=\"color:#FF0000\">", txt, "</span>"))
# error message
errorMessage <- function(txt) {
  showModal(modalDialog(
    title = HTML("<span style = 'color:red'>Error Message</span>"),
    HTML(paste0("<span style = 'color:red'>", txt, "</span>")),
    easyClose = TRUE))
}
# template of the plot to show
templatePlot <- function(locs, eye, expf = 1.05, lty = 1, lwd = 1,
                         bg = "white", ps = 9, linCol = "lightgray",
                         ellipseColor = "gray92", new = FALSE) {
  rad <- 1.5
  par(mar = c(0, 0, 0, 0), bg = bg, ps = ps, new = new)
  if(eye == "L") x <- -15
  else x <- 15
  xlim <- c(-30, 30)
  ylim <- c(-30, 30)
  if(!all(is.na(locs$x))) {
    if(min(locs$x, na.rm = TRUE) < xlim[1]) xlim[1] <- min(locs$x, na.rm = TRUE)
    if(max(locs$x, na.rm = TRUE) > xlim[2]) xlim[2] <- max(locs$x, na.rm = TRUE)
  }
  if(!all(is.na(locs$y))) {
    if(min(locs$y, na.rm = TRUE) < ylim[1]) ylim[1] <- min(locs$y, na.rm = TRUE)
    if(max(locs$y, na.rm = TRUE) > ylim[2]) ylim[2] <- max(locs$y, na.rm = TRUE)
  }
  xlim <- 10 * sign(xlim) * ceiling(abs(xlim / 10))
  ylim <- 10 * sign(ylim) * ceiling(abs(ylim / 10))
  r <- max(c(xlim, ylim))
  plot(0, 0, typ = "n", xlim = expf * xlim, ylim = expf * ylim, asp = 1,
       axes = FALSE, ann = FALSE, bty = "n")
  draw.ellipse(x, -1.5, 2.75, 3.75, col = ellipseColor, border = ellipseColor)
  lines(c(xlim[1], -rad), c(0, 0), col = linCol, lty = lty, lwd = lwd)
  lines(c(rad, xlim[2]), c(0, 0), col = linCol, lty = lty, lwd = lwd)
  lines(c(0, 0), c(ylim[1], -rad), col = linCol, lty = lty, lwd = lwd)
  lines(c(0, 0), c(rad, ylim[2]), col = linCol, lty = lty, lwd = lwd)
  text(expf * xlim[1], 0, xlim[1], adj = c(1, 0.5), col = linCol)
  text(expf * xlim[2], 0, xlim[2], adj = c(0, 0.5), col = linCol)
  text(0, expf * ylim[1], ylim[1], adj = c(0.5, 1), col = linCol)
  text(0, expf * ylim[2], ylim[2], adj = c(0.5, 0), col = linCol)
  draw.circle(0, 0, rad, border = linCol, lwd = lwd)
  l <- 10
  ang <- seq(0, 2 * pi, length.out = 100)
  while(l <= r) {
    x <- sapply(ang, function(a) l * cos(a))
    y <- sapply(ang, function(a) l * sin(a))
    y[x < xlim[1] | x > xlim[2]] <- NA
    x[x < xlim[1] | x > xlim[2]] <- NA
    x[y < ylim[1] | y > ylim[2]] <- NA
    y[y < ylim[1] | y > ylim[2]] <- NA
    lines(x, y, col = linCol, lty = lty, lwd = lwd)
    l <- l + 10
  }
}
# format seconds to mm:ss
secsToMins <- function(secs) {
  mm <- as.character(secs %/% 60)
  ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
  if(nchar(mm) == 1) mm <- paste0("0", mm)
  if(nchar(ss) == 1) ss <- paste0("0", ss)
  return(paste(mm, ss, sep = ":"))
}
#####################
# Routines for opiApp
#####################
# disable all buttons
disableAll <- function()
  lapply(c("settingsbtn", "gammabtn", "gridgenbtn", "patientsbtn", "staticbtn", "reportbtn"), disable)
# enable all buttons
enableAll <- function()
  lapply(c("settingsbtn", "gammabtn", "gridgenbtn", "patientsbtn", "staticbtn", "reportbtn"), enable)
#######################
# Routines for gprofile
#######################
# generate LUT table
generateLUTtable <- function(setupTable) {
  lutTable <- data.frame(pix = unique(c(seq(setupTable$from[1], setupTable$to[1], by = setupTable$by[1]),
                                        seq(setupTable$from[2], setupTable$to[2], by = setupTable$by[2]),
                                        seq(setupTable$from[3], setupTable$to[3], by = setupTable$by[3]))))
  lutTable$lum1 <- as.numeric(NA)
  lutTable$lum2 <- as.numeric(NA)
  lutTable$lum3 <- as.numeric(NA)
  return(lutTable)
}
# plot LUT results so far
lutPlot <- function(lutTable, lutFit) {
  pix     <- lutTable$pix
  lum     <- apply(lutTable[,2:ncol(lutTable)], 1, mean, na.rm = TRUE) # mean
  lum2sem <- 2 * apply(lutTable[,2:ncol(lutTable)], 1, sd, na.rm = TRUE) / sqrt(ncol(lutTable) - 1) # 2 SEM
  lum2sem[is.na(lum2sem)] <- 0
  par(mar = c(8, 4, 6, 1))
  ymax <- ifelse(all(is.nan(lum)), 0, max(lum[!is.na(lum)]))
  if(ymax < 200) ymax <- 200
  plot(0, 0, typ = "n", xlim = c(0, 255), ylim = c(0, ymax),
       panel.first = grid(), xlab = "pixel value", ylab = "luminance (cd/m2)")
  arrows(pix, lum - lum2sem, pix, lum + lum2sem, length = 0, angle = 90)
  points(pix, lum, pch = 21, bg = "white")
  lines(lutFit$x, lutFit$y, col = "red")
}
# generate handsontables
setuptable <- function(table, readOnly = FALSE) {
  table <- rhandsontable(table, rowHeaders = c("sector 1", "sector 2", "sector 3"), selectCallback = TRUE, height = 100, rowHeaderWidth = 75)
  return(hot_cols(table, format = "1", colWidths = 50, readOnly = readOnly))
}
luttable <- function(table)
  return(hot_col(rhandsontable(table, rowHeaders = NULL, selectCallback = TRUE, height = 400), col = 1, readOnly = TRUE, format = "1"))
######################
# Routines for gridgen
######################
# build grid table
buildGridTable <- function(grids)
  return(data.frame("Code" = names(grids),
                    "Name" = sapply(grids, function(gg) gg$name),
                    "Locations" = sapply(grids, function(gg) nrow(gg$locs)),
                    "Waves" = sapply(grids, function(gg) max(gg$locs$w))))
# init locations
initLocs <- function()
  return(data.frame(x = as.numeric(NA), y = as.numeric(NA), w = as.integer(0), est = as.integer(NA)))
# assemble grid table
fillPtsTable <- function(locs, readOnly = FALSE) {
  locsOut <- locs
  locsOut <- rhandsontable(locsOut, colHeaders = c("X", "Y", "Wave", "Est"),
                           selectCallback = TRUE, height = 300,
                           readOnly = readOnly, contextMenu = TRUE) %>%
    hot_col(col = 1, colWidths = 45) %>%
    hot_col(col = 2, colWidths = 45) %>%
    hot_col(col = 3, format = "1", colWidths = 50) %>%
    hot_col(col = 4, colWidths = 45) %>%
    hot_cols(halign = "htRight", valign = "htMiddle") %>%
    hot_context_menu(allowColEdit = FALSE, allowRowEdit = !readOnly)
  return(locsOut)
}
# show grid points
showGrid <- function(locs) {
  templatePlot(locs = locs, eye = "R")
  if(!all(is.na(locs))) {
    cols <- brewer.pal(8, "Dark2")[1:max(locs$w, na.rm = TRUE)]
    cols[1] <- "#000000"
    iszero <- locs$w == 0
    if(any(iszero))  text(locs$x[iszero], locs$y[iszero], ".", cex = 2)
    if(any(!iszero)) text(locs$x[!iszero], locs$y[!iszero], locs$w[!iszero], col = cols[locs$w[!iszero]], cex = 0.75, font = 2)
  }
}
#######################
# Routines for patients
#######################
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
  save(patientTable, file = "config/patientdb.rda")
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
  save(patientTable, file = "config/patientdb.rda")
  return(patientTable)
}
# delete patient
deletePatient <- function(idx, patientTable) {
  patientTable <- patientTable[-idx,] # delete record
  save(patientTable, file = "config/patientdb.rda")
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
#####################
# Routines for client
#####################
resPlot <- function(res, locs, eye, foveadb, maxlum) {
  if(is.null(locs)) return(NULL)
  alpha <- "CC"
  bglum <- 0.25
  templatePlot(locs, eye, bg = paste0("gray", round(100 * bglum)),
               linCol = "gray50", ellipseColor = "gray10")
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- brewer.pal(8, "Dark2")[1:max(locs$w, na.rm = TRUE)]
  cols[1] <- "#FFFFFF"
  cols <- paste0(cols, alpha)
  fovcol <- paste0("#FFFFFF", alpha)
  ptcex <- 0.5
  if(!is.null(res)) {
    # show current result
    if(res$type == "F") {
      col <- fovcol
      foveadb <- NA
      cex <- 0
    } else {
      if(eye == "L") res$x <- -res$x
      col <- cols[locs$w[res$loc]]
      locs <- locs[-res$loc,]
      cex <- ptcex
    }
    ccol <- res$lum / maxlum * (1 - 1.5 * bglum) + 1.5 * bglum
    ccol <- paste0(gray(ccol), alpha)
    draw.circle(res$x, res$y, radius = res$size / 2, col = ccol, border = NA)
    points(res$x, res$y, pch = 19, cex = cex, col = col)
  }
  text(0, 0, round(foveadb), col = fovcol, font = 2)
  isna <- is.na(locs$th)
  if(any(isna))  points(locs$x[isna], locs$y[isna], pch = 19, cex = ptcex, col = cols[locs$w[isna]])
  if(any(!isna)) text(locs$x[!isna], locs$y[!isna], round(locs$th[!isna]), col = cols[locs$w[!isna]], font = 2)
}
falsePositivePars <- function(machine, perimetry, bglum, maxlum, locs, eye) {
  if(perimetry == "luminance" & machine == "PhoneHMD")
    db <- cdTodb(bglum, maxlum)
  else
    db <- 50
  # generate an invisible stimulus
  loc <- sample(1:nrow(locs), 1)
  x <- ifelse(eye == "L", -locs$x[loc], locs$x[loc])
  y <- locs$y[loc]
  return(data.frame(loc = loc, x = x, y = y, db = db))
}
falseNegativePars <- function(locs, res, eye) {
  # keep only the dimmest stimulus and select one at random
  res <- res[res$type == "N" & res$done & res$seen, c("x", "y", "level")]
  if(nrow(res) == 0) return(NULL)
  ur <- unique(res[,c("x", "y")])
  for(i in 1:nrow(ur))
    res$db[i] <- min(res$level[which(res$x == ur$x[i] & res$y == ur$y[i])])
  idx <- sample(1:nrow(res), 1)
  db <- ifelse(res$db[idx] < 5, 0, res$db[idx] - 5) # TODO criterion. So far, I subtract 5 db
  loc <- which(locs$x == res$x[idx] & locs$y == res$y[idx])
  x <- ifelse(eye == "L", -locs$x[loc], locs$x[loc])
  y <- locs$y[loc]
  # generate a very visible stimulus
  return(data.frame(loc = loc, x = x, y = y, db = db))
}
enableElements <- function(ids) lapply(ids, enable)
disableElements <- function(ids) lapply(ids, disable)
enableRunElements <- function()
  enableElements(c("close", "fovea", "run", "eye", "grid", "perimetry", "algorithm", "lum", "size", "dbstep", "estSD", "nreps", "range"))
disableRunElements <- function()
  disableElements(c("close", "fovea", "run", "eye", "grid", "perimetry", "algorithm", "lum", "size", "dbstep", "estSD", "nreps", "range", "save", "cancel"))
# patient's information to show: id, name, surname, age, gender
parsePatientOutput <- function(patient) {
  if(is.na(patient$id)) {
    txt <- errortxt("Please select a patient first")
  } else {
    txt <- paste0("<strong>Patient ID:</strong> ", patient$id, "</br>")
    txt <- paste0(txt, "<strong>Name:</strong> ",  patient$name, " ", patient$surname, "</br>")
    txt <- paste0(txt, "<strong>Age:</strong> ", patient$age, ". <strong>Gender:</strong> ", patient$gender, "</br>")
    txt <- paste0(txt, "<strong>Type:</strong> ", patient$type)
  }
  return(HTML(txt))
}
# technical test information
parseTechnicalOutput <- function(lut, bglum, maxlum) {
  bgidx <- which.min(abs(lut - bglum))
  bglum <- lut[bgidx]
  maxlum <- lut[which.min(abs(lut - maxlum))]
  mind <- lut[bgidx + 1] - lut[bgidx]
  maxd <- maxlum - bglum
  txt <- paste0("</br><strong>Bg | max lum:</strong> ", round(bglum, 2), " | ", round(maxlum, 2), " cd/m2</br>")
  txt <- paste0(txt, "<strong>min step:</strong> ", round(mind, 2), " cd/m2 (",
                round(cdTodb(mind, maxlum - bglum), 1), " dB)</br>")
  txt <- paste0(txt, "<strong>max step:</strong> ", round(maxd, 2), " cd/m2 (0 dB)</br>")
  return(HTML(txt))
}

# prepare test results to show next to the plot
renderResult <- function(trialRes, res, npoints) {
  if(is.null(trialRes)) {
    respWintxt <- seentxt <- level <- x <- y <- time <- ""
  } else {
    x <- trialRes$x
    y <- trialRes$y
    level <- trialRes$level
    time <- trialRes$time
    respWin <- trialRes$respWin
    seen <- trialRes$seen
    if(trialRes$type == "N" | trialRes$type == "F")
      seentxt <- paste("Stimulus")
    else if(trialRes$type == "FP")
      seentxt <- paste("FP trial")
    else if(trialRes$type == "FN")
      seentxt <- paste("FN trial")
    if(seen) {
      seentxt <- paste(seentxt, "<strong>seen</strong>", "in", time, "ms")
    } else
      seentxt <- paste(seentxt, "<strong>not seen</strong>")
    respWintxt <- paste("Response window was", respWin, "ms")
  }
  if(sum(res$type == "N") == 0) {
    rtsd <- rtm <- ""
    nfinished <- fp <- fpt <- fpp <- fn <- fnt <- fnp <- 0
    npres <- resBelow150 <- resAbove600 <- 0
    tttxt <- tptxt <- "00:00"
  } else {
    npres <- sum(res$type == "N")
    nfinished <- sum(res$type == "N" & res$done)
    # compute false positives and negatives
    fp  <- sum(res$type == "FP" & res$seen)
    fpt <- sum(res$type == "FP")
    fpp <- ifelse(fpt == 0, 0, round(100 * fp / fpt))
    fn  <- sum(res$type == "FN" & !res$seen)
    fnt <- sum(res$type == "FN")
    fnp <- ifelse(fnt == 0, 0, round(100 * fn / fnt))
    # compute response time SD and mean
    rt <- res$time[which(res$type == "N" & res$seen == TRUE)]
    if(length(rt) > 1) { # can only calculate SD if there are more than 2 response times available
      rtm  <- round(mean(rt))
      rtsd <- round(sd(rt))
    } else rtsd <- rtm <- ""
    # compute responses below 150 ms and above 600 ms
    resBelow150 <- sum(res$type == "N" & res$time < 150)
    resAbove600 <- sum(res$type == "N" & res$time > 600 & res$seen == TRUE)
    # calculate test time and pause time
    tttxt <- secsToMins(res$tt[length(res$tt)])  
    tptxt <- secsToMins(res$tp[length(res$tp)])
  }
  if(x != "") x <- paste(x, "degrees")
  if(y != "") y <- paste(y, "degrees")
  if(level != "") level <- paste(round(level, 1), "dB")
  if(rtm != "") rtm <- paste(rtm, "ms")
  if(rtsd != "") rtsd <- paste(rtsd, "ms")
  # get state text
  txt <- paste("<strong>Stimulus x:</strong>", x, "<br/>")
  txt <- paste(txt, "<strong>Stimulus y:</strong>", y, "<br/>")
  txt <- paste(txt, "<strong>Level:</strong>", level, "<br/>")
  txt <- paste0(txt, seentxt, "<br/>")
  txt <- paste0(txt, respWintxt, "<br/>")
  # False positives and negatives
  txt <- paste(txt, "<strong>False Positives:</strong>", fp, "of", fpt)
  txt <- paste0(txt, " (", fpp, "%)<br/>")
  txt <- paste(txt, "<strong>False Negatives:</strong>", fn, "of", fnt)
  txt <- paste0(txt, " (", fnp, "%)<br/>")
  # Response Times
  txt <- paste(txt, "<strong>Responses < 150 ms:</strong>", resBelow150, "<br/>")
  txt <- paste(txt, "<strong>Responses > 600 ms:</strong>", resAbove600, "<br/>")
  txt <- paste(txt, "<strong>Mean Response Time:</strong>", rtm, "<br/>")
  txt <- paste(txt, "<strong>SD of Response Time:</strong>", rtsd, "<br/><br/>")
  # Progress
  txt <- paste(txt, "<strong>Finished:</strong>", nfinished, "of", npoints)
  txt <- paste0(txt, " (", round(100 * nfinished / npoints), "%)<br/>")
  txt <- paste(txt, "<strong>Presentations:</strong>", npres, "<br/>")
  # test time and pause time
  txt <- paste(txt, "<strong>Test Time (mm:ss):</strong>",  tttxt, "<br/>")
  txt <- paste(txt, "<strong>Pause Time (mm:ss):</strong>", tptxt, "<br/>")
  txt <- paste0(txt, "<br/>")
  return(HTML(txt))
}
# prepare results to save
prepareToSave <- function(patient, machine, perimetry, algorithm, grid,
                          eye, background, lum, size, dbstep, estSD, nreps, range,
                          tdate, ttime, comments, res, foveadb, locs) {
  dat <- data.frame(id = patient$id, eye = eye, date = tdate, time = ttime,
                    age = patient$age, type = patient$type, machine = machine, perimetry = perimetry,
                    algorithm = algorithm, grid = grid, background = background, luminance = lum,
                    size = size, dbstep = dbstep, estSD = estSD, nreps = nreps, range = range,
                    fp = NA, fpt = NA, fpr = NA, fn = NA, fnt = NA, fnr = NA,
                    npres = NA, rt150 = NA, rt600 = NA, rtsd = NA, rtm = NA,
                    duration = NA, pause = NA, comments = comments, foveadb = NA)
  # test and pause time
  dat$duration <- secsToMins(res$tt[length(res$tt)])
  dat$pause <- secsToMins(res$tp[length(res$tp)])
  # false positive and false negatives
  dat$fp  <- sum(res$type == "FP" & res$seen)
  dat$fpt <- sum(res$type == "FP")
  dat$fpr <- ifelse(dat$fpt == 0, 0, dat$fp / dat$fpt)
  dat$fn  <- sum(res$type == "FN" & !res$seen)
  dat$fnt <- sum(res$type == "FN")
  dat$fnr <- ifelse(dat$fnt == 0, 0, dat$fn / dat$fnt)
  # compute response time SD and mean
  rt <- res$time[which(res$type == "N" & res$seen == TRUE)]
  dat$rtm <- round(mean(rt))
  dat$rtsd <- round(sd(rt))
  # number of presentations and responses below 150 ms and above 600 ms
  dat$npres <- sum(res$type == "N")
  dat$rt150 <- sum(res$type == "N" & res$time < 150)
  dat$rt600 <- sum(res$type == "N" & res$time > 600 & res$seen == TRUE)
  if(!is.null(foveadb)) dat$foveadb <- foveadb
  # get results for each location
  dat[,paste0("l", 1:nrow(locs))] <- locs$th
  return(dat)
}
#####################
# Routines for report
#####################
# show plot
showPlot <- function(locs, eye, foveadb, ps = 9, new = FALSE) {
  if(is.null(locs)) return(NULL)
  templatePlot(locs = locs, eye = eye, ps = ps, new = new)
  alpha <- "AA"
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- brewer.pal(8, "Dark2")[1:max(locs$w, na.rm = TRUE)]
  cols[1] <- "#000000"
  fovcol <- "black"
  text(0, 0, round(foveadb), col = fovcol, font = 2)
  isna <- is.na(locs$th)
  if(any(isna))  points(locs$x[isna], locs$y[isna], pch = 19, cex = 0.75, col = paste0(cols[locs$w[isna]], alpha))
  if(any(!isna)) text(locs$x[!isna], locs$y[!isna], round(locs$th[!isna]), col = cols[locs$w[!isna]], font = 2)
}
# get all available reports and sort them by date, then time
getReports <- function(patientTable) {
  fnames <- dir("results/", pattern = "*.csv")
  if(length(fnames) == 0) return(NULL)
  fnames <- paste("results", fnames, sep = "/")
  reports <- do.call(rbind, lapply(fnames, function(ff) {
    dat <- read.csv(ff, stringsAsFactors = FALSE)
    return(dat[,setdiff(names(dat), paste0("l", 1:nrow(grids[[dat$grid[1]]]$locs)))])
  }))
  # merge with patient db table to get name, surname, and type of the patient
  reports <- merge(patientTable[,c("id", "name", "surname")], reports, by = "id")
  reports$date <- as.Date(reports$date)
  # sort by date and time
  reports <- reports[order(reports$time, decreasing = TRUE),]
  reports <- reports[order(reports$date, decreasing = TRUE),]
  return(reports)
}
# get record from results
getResults <- function(dat) {
  fname <- paste0("results/", paste(dat$id, dat$grid, sep = "_"), ".csv")
  record <- read.csv(fname, stringsAsFactors = FALSE)
  record <- record[which(record$date == dat$date[1] & record$time == dat$time[1]),]
  fname <- paste0("results/logs/", paste(dat$id, dat$grid, 
                                            gsub("-", "", record$date),
                                            gsub(":", "", record$time), sep = "_"), ".csv")
  res <- read.csv(fname, stringsAsFactors = FALSE)
  return(list(record = record, res = res))
}
# prepare test results to show next to the plot
generateReport <- function(record, res, npoints) {
  if(!is.null(record)) {
    id <- record$id
    age <- record$age
    type <- record$type
    machine <- record$machine
    perimetry <- record$perimetry
    if(perimetry == "luminance") fixedParam <- record$size
    if(perimetry == "size") fixedParam <- record$luminance
    algorithm <- record$algorithm
    if(algorithm == "ZEST") stopValue <- record$estSD
    if(algorithm == "MOCS") stopValue <- record$nreps
    npres <- sum(res$type == "N")
    # compute false positives and negatives
    fp  <- sum(res$type == "FP" & res$seen)
    fpt <- sum(res$type == "FP")
    fpp <- ifelse(fpt == 0, 0, round(100 * fp / fpt))
    fn  <- sum(res$type == "FN" & !res$seen)
    fnt <- sum(res$type == "FN")
    fnp <- ifelse(fnt == 0, 0, round(100 * fn / fnt))
    # compute response time SD and mean
    rt <- res$time[which(res$type == "N" & res$seen == TRUE)]
    if(length(rt) > 1) { # can only calculate SD if there are more than 2 response times available
      rtm  <- round(mean(rt))
      rtsd <- round(sd(rt))
    } else rtsd <- rtm <- ""
    # compute responses below 150 ms and above 600 ms
    resBelow150 <- sum(res$type == "N" & res$time < 150)
    resAbove600 <- sum(res$type == "N" & res$time > 600 & res$seen == TRUE)
    # calculate test time and pause time
    tttxt <- secsToMins(res$tt[length(res$tt)])  
    tptxt <- secsToMins(res$tp[length(res$tp)])
  } else {
    rtsd <- rtm <- id <- age <- type <- machine <- perimetry <- fixedParam <- algorithm <- stopValue <- ""
    fp <- fpt <- fpp <- fn <- fnt <- fnp <- 0
    npres <- resBelow150 <- resAbove600 <- 0
    tttxt <- tptxt <- "00:00"
  }
  if(rtm != "") rtm <- paste(rtm, "ms")
  if(rtsd != "") rtsd <- paste(rtsd, "ms")
  # patient info
  txt <- paste("<strong>Patient ID:</strong>", id, "<br/>")
  txt <- paste(txt, "<strong>Patient Age:</strong>", age, "<br/>")
  txt <- paste(txt, "<strong>Patient Type:</strong>", type, "<br/><br/>")
  # test info
  txt <- paste(txt, "<strong>Device:</strong>", machine, "<br/>")
  txt <- paste(txt, "<strong>Perimetry:</strong>", perimetry, "<br/>")
  if(perimetry == "luminance")
    txt <- paste(txt, "<strong>Fixed Size:</strong>", fixedParam, "\u00B0<br/>")
  else if(perimetry == "size")
    txt <- paste(txt, "<strong>Fixed Luminance:</strong>", fixedParam, "cd/m2<br/>")
  else 
    txt <- paste(txt, "<strong>Wrong perimetry</strong>", "<br/>")
  txt <- paste(txt, "<strong>Algorithm:</strong>", algorithm, "<br/>")
  if(algorithm == "Staircase" || algorithm == "Full Threshold")
    txt <- paste(txt, "<br/>")
  else if(algorithm == "MOCS")
    txt <- paste(txt, "<strong>Number of Repetitions:</strong>", stopValue, "<br/>")
  else if(algorithm == "ZEST")
    txt <- paste(txt, "<strong>Estimate SD:</strong>", stopValue, "dB<br/>")
  else
    txt <- paste(txt, "<strong>Wrong algorithm</strong>", "<br/>")
  txt <- paste0(txt, "<br/>")
  # False positives and negatives
  txt <- paste(txt, "<strong>False Positives:</strong>", fp, "of", fpt)
  txt <- paste0(txt, " (", fpp, "%)<br/>")
  txt <- paste(txt, "<strong>False Negatives:</strong>", fn, "of", fnt)
  txt <- paste0(txt, " (", fnp, "%)<br/>")
  # Response Times
  txt <- paste(txt, "<strong>Responses < 150 ms:</strong>", resBelow150, "<br/>")
  txt <- paste(txt, "<strong>Responses > 600 ms:</strong>", resAbove600, "<br/>")
  txt <- paste(txt, "<strong>Mean Response Time:</strong>", rtm, "<br/>")
  txt <- paste(txt, "<strong>SD of Response Time:</strong>", rtsd, "<br/><br/>")
  # Progress, test time and pause time
  txt <- paste(txt, "<strong>Presentations:</strong>", npres, "<br/>")
  txt <- paste(txt, "<strong>Test Time (mm:ss):</strong>",  tttxt, "<br/>")
  txt <- paste(txt, "<strong>Pause Time (mm:ss):</strong>", tptxt, "<br/>")
  txt <- paste0(txt, "<br/>")
  return(txt)
}
# generate pdf report
savePDF <- function(fname, record, locs, res, eye, foveadb) {
  # width and height in inches
  width  <- 6
  height <- 2.5
  pdf(file = fname, width = width, height = height)
  scrlist <- mountlayout()
  # plot
  screen(scrlist$plot)
  showPlot(locs, eye, foveadb, 5, new = TRUE)
  # info
  screen(scrlist$info)
  dat <- parsetxt(generateReport(record, res, nrow(locs)))
  nblocks <- dat$nblocks
  nrows <- dat$nrows
  dat <- dat$dat
  top <- 1
  lineSep <- top / (sum(nrows) + (nblocks - 1))
  for(i in 1:nblocks) {
    txt <- dat[[i]]
    for(j in 1:nrows[i]) {
      text(0, top, txt[j,1], adj = c(0, 1), font = 2)
      text(1, top, txt[j,2], adj = c(1, 1))
      top <- top - lineSep
    }
    top <- top - lineSep
  }
  close.screen(all.screens = TRUE)
  invisible(dev.off())
}
parsetxt <- function(txt) {
  txt <- strsplit(strsplit(txt, split = "<br/><br/>")[[1]], "<br/>")
  txt <- lapply(txt, function(tt) {
    tt <- gsub("<strong>", "", tt, )
    tt <- strsplit(tt, "</strong>")
  })
  nblocks <- length(txt)
  nrows <- rep(NA, nblocks)
  for(i in 1:nblocks) {
    nrows[i] <- length(txt[[i]])
    txt[[i]] <- do.call(rbind, lapply(txt[[i]], function(tt) data.frame(title = trimws(tt[1]), txt = trimws(tt[2]))))
  }
  return(list(nblocks = nblocks, nrows = nrows, dat = txt))
}
# mount layout for report
mountlayout <- function() {
  # all the boxes are defined in mm divided by the width and height
  # of the page, also in mm
  boxPlot <- c(0.0125, 0.6625, 0.025, 0.975)
  boxInfo <- c(0.6625, 0.9875, 0.025, 0.975)
  scr <- split.screen(rbind(boxInfo, boxPlot))
  return(list(info = scr[1], plot = scr[2]))
}