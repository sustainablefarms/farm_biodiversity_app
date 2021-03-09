# runs a test that specifies manually a single patch (rather than using the default). Then species the low midstorey and woody veg and saves results.
app <- ShinyDriver$new("../../")
app$snapshotInit("specify_a_patch_longer")

app$setInputs(`patch-choose_n_patches` = "click")
app$snapshot()
app$setInputs(`patch-n_patches` = 1, `patch-choose_n_patches_execute` = "click")
app$snapshot()
app$setInputs(`patch-patch_number_1` = "click")
app$setInputs(`patch-pc_midstorey_1` = 0, `patch-pc_woody_veg_1` = 2, `patch-noisy_miner_1` = FALSE)
app$snapshot()
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
app$snapshot()
