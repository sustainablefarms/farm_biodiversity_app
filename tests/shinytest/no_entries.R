app <- ShinyDriver$new("../../", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("no_entries")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal
Sys.sleep(1)
app$snapshot()
