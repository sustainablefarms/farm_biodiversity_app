app <- ShinyDriver$new("../../", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("predict_goulburn_low_rain")

app$setInputs(overallhelpfake = "click")
app$setInputs(moredetailfake = "click")
app$setInputs(downloadreportfake = "click")
app$setInputs(introfake = "click")
app$findElement('button[data-dismiss]')$click() #closes the modal

# select goulburn
# simulating a plotly click on goulburn below. To change to other locations I suspect changing the pointNumber will suffice
#plotly_click is the event type, 'region_map' is the source. Combined with a "-" give the eventID in plotly::event_data
# the event info I think can be obtained from session$rootScope()$input[[eventID]]
# the value when clicking can be obtained by print(session$rootScope()$input[["plotly_click-region_map"]]) in a reactive in selectionlocation_module
app$setInputs(`plotly_click-region_map` = '[{\"curveNumber\":0,\"pointNumber\":0,\"x\":149.603897167433,\"y\":-34.5156485191709}]', allowInputNoBinding_ = TRUE, priority_ = "input")

# specify the patch
app$setInputs(`patch-patch_number_1` = "click")
app$setInputs(`patch-patchattr-pc_woody3000m` = 15.5)
app$setInputs(`patch-patchattr-pc_woody500m` = 14)
app$setInputs(`patch-choose_patch_attributes_execute` = "click")

# set rainfall
app$setInputs(`yfa-AnnPrec.YfA` = 460)

# capture predictions
Sys.sleep(1) #wait for notifications to close
app$snapshot()
