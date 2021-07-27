app <- ShinyDriver$new("../../", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("specify_one_patch")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

app$setInputs(`patch-patch_number_1` = "click")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patchattr-pc_woody3000m` = 15.5)
app$setInputs(`patch-patchattr-pc_woody500m` = 14)
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
Sys.sleep(1)
app$snapshot()
