library(OPI)
library(deldir)
source("modules/serverUtils.r")
load("config/appParams.rda")
load("config/grids.rda")
# setup test
machine <- "SimHenson"
eye <- "R"
perimetry <- "size"
algorithm <- "MOCS"
grid <- "practice"
size <- appParams$size
lum <- appParams$lum
dbstep <- 1
estSD <- appParams$estSD
nreps <- 4
range <- appParams$range

statement <- paste("opiInit", machine)
do.call(what = opiInitialize, args = parseMessage(statement, appParams)$pars)
# set background
if(machine == "PhoneHMD") {
  statement <- paste("opiSetBackground", "B", "B", "cross")
} else {
  statement <- paste("opiSetBackground")
}
do.call(what = opiSetBackground, args = parseMessage(statement, appParams)$pars)

statement <- paste("opiTestInit", eye, perimetry, algorithm, grid,
                   size, lum, dbstep, estSD, nreps, range)
pars <- parseMessage(statement, appParams)$pars
if(grid == "fovea") {
  locs <- data.frame(x = 0, y = 0, w = 1, est = 30)
} else {
  locs <- grids[[pars$grid]]$locs
}
locs$est <- 30
setup <- testSetup(machine, appParams, pars, locs)
states <- setup$states
settings <- setup$settings

print("domain:")
print(states[[1]]$domain)
while(!all(sapply(states, function(s) settings$stopf(s)))) {
  rs <- testStep(states, settings)
  states <- rs$states
  settings <- rs$settings
  print(round(sapply(states, function(s) settings$finalf(s)), 1))
}

statement <- paste("opiTestCatchTrial", "5", "0", "0", "0")
pars <- parseMessage(statement, appParams)$pars
res <- testCatchTrial(settings, pars)

opiClose()