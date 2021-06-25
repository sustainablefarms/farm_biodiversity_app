app <- ShinyDriver$new(".")#./../")
app$snapshotInit("selectlocation-goulburn")
Sys.sleep(0.5)
app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$snapshot()
app$findElement('button[data-dismiss]')$click() #closes the modal
app$snapshot()

app$setInputs(`plotly_click-A` = "M5,0A5,5 0 1,1 0,-5A5,5 0 0,1 5,0Z", allowInputNoBinding_ = TRUE)
app$findElement('g.points path.point')$getName()
app$takeScreenshot()
