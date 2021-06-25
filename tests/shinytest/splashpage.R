app <- ShinyDriver$new("../..")
app$snapshotInit("splashpage")
Sys.sleep(1) # delay hopefully makes the result more consistent
app$snapshot()
