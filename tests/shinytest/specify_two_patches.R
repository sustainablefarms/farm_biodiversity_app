app <- ShinyDriver$new("../../")
app$snapshotInit("specify_two_patches")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

app$setInputs(`patch-choose_n_patches` = "click")
app$snapshot()
app$setInputs(`patch-n_patches` = "2")
app$snapshot()
app$setInputs(`patch-choose_n_patches_execute` = "click")
app$snapshot()
# specify patch 1
app$setInputs(`patch-patch_number_1` = "click")
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
app$snapshot()

# specify and capture modal of patch 2
app$setInputs(`patch-patch_number_2` = "click")
app$snapshot()
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
app$snapshot()
