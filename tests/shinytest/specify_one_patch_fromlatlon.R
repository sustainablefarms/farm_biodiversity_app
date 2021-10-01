app <- ShinyDriver$new("../../", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("specify_one_patch_fromlatlon")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

app$setInputs(`patch-patch_number_1` = "click")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patchattr-getwoodycanopy` = "click") #the first default click of getwoodycanopy button
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patchattr-lon` = "145")
app$setInputs(`patch-patchattr-lat` = "-35")
app$setInputs(`patch-patchattr-yearforcanopy` = "2018")
app$setInputs(`patch-patchattr-showmap` = FALSE)
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patchattr-getwoodycanopy` = "click") #retrieve woody canopy
Sys.sleep(10)
app$snapshot()
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
Sys.sleep(1)
app$snapshot()

# reopen modal to check values saved
app$setInputs(`patch-patch_number_1` = "click")
Sys.sleep(1)
app$snapshot()
