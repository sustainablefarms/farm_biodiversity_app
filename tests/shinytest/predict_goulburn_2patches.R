app <- ShinyDriver$new("../../")
app$snapshotInit("predict_goulburn_2patches")

app$setInputs(`location-fake_region_number` = "click")
app$setInputs(`patch-choose_n_patches` = "click")
app$setInputs(`patch-n_patches` = 2)
app$waitForShiny()
app$snapshot()
app$setInputs(`patch-choose_n_patches_execute` = "click")
app$setInputs(`patch-patch_number_1` = "click")
app$setInputs(`patch-noisy_miner_1` = TRUE, `patch-pc_woody_veg_1` = 14, `patch-pc_midstorey_1` = 8)
app$waitForShiny()
app$snapshot()
app$waitForShiny()
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
app$setInputs(`patch-patch_number_2` = "click")
app$setInputs(`patch-noisy_miner_2` = TRUE, `patch-pc_midstorey_2` = 10, `patch-pc_woody_veg_2` = 20)
app$setInputs(`patch-choose_patch_attributes_execute` = "click")
app$waitForShiny()
app$snapshot()