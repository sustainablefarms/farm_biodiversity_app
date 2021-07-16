app <- ShinyDriver$new("../../")
app$snapshotInit("goulburn_reference_vs_low_rain")

# dismiss splash screen
app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

# set goulburn
app$setInputs(`plotly_click-region_map` = '[{\"curveNumber\":0,\"pointNumber\":0,\"x\":149.603897167433,\"y\":-34.5156485191709}]', allowInputNoBinding_ = TRUE, priority_ = "input")

# specify the patch
app$setInputs(`patch-patch_number_1` = "click")
app$setInputs(`patch-pc_woody3000m_1` = 15.5)
app$setInputs(`patch-pc_woody500m_1` = 14)
app$setInputs(`patch-choose_patch_attributes_execute` = "click")

# make this reference
app$setInputs(`pred-savetoreference` = "click")
app$setInputs(`pred-usedefaultreference` = FALSE)
app$snapshot()

# compare to lower rainfall
app$setInputs(`yfa-AnnPrec.YfA` = 460)
app$snapshot()
