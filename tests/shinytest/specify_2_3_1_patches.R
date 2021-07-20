app <- ShinyDriver$new("../../")
app$snapshotInit("specify_2_3_1_patches")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

# 2 patches
app$setInputs(`patch-patch_selector-choose_n_patches` = "click")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-n_patches` = "2")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-choose_n_patches_execute` = "click")
Sys.sleep(1)
app$snapshot()

# 3 patches
app$setInputs(`patch-patch_selector-choose_n_patches` = "click")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-n_patches` = "3")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-choose_n_patches_execute` = "click")
Sys.sleep(1)
app$snapshot()

# 1 patch
app$setInputs(`patch-patch_selector-choose_n_patches` = "click")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-n_patches` = "1")
Sys.sleep(1)
app$snapshot()
app$setInputs(`patch-patch_selector-choose_n_patches_execute` = "click")
Sys.sleep(1)
app$snapshot()