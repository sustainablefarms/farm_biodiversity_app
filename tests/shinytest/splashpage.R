app <- ShinyDriver$new("../..", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("splashpage")
Sys.sleep(1) # delay hopefully makes the result more consistent
app$snapshot()
