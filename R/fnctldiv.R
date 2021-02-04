# traits plot and prep
#### note that traits are also used in detailed predictions plot ###
# model_data <- load_model_data()
# traits_raw <- readRDS("../sflddata/private/data/raw/bird_traits.rds")
# traits_raw <- traits_raw[, c("CommonName",
#                              primary_diet = "MFScore",
#                              foraging_substrate = "FMScore",
#                              feeding_aggregation = "SSScore",
#                              nesting_aggregation = "BScore",
#                              seasonal_movement = "MScore",
#                              body_size = "CubeRootBodyWeight")]
# species <- model_data$species
# stopifnot(setequal(intersect(traits_raw$CommonName, species), species))
# traits <- traits_raw[traits_raw$CommonName %in% species, ]
# traits <- traits[complete.cases(traits), ] #remove incomplete rows
# stopifnot(0 == sum(duplicated(traits$CommonName))) #stop if species are duplicated (i.e. common name used for species with different functionality)
# stopifnot(setequal(species, traits$CommonName)) #stop if after the above cleaning, some species are not present in the traits data
# 
# rownames(traits) <- traits$CommonName
# traits <- traits[, colnames(traits) != "CommonName"]
# saveRDS(traits, file = "./data/processedtraits.rds")

# compute fctl diversity for mean point
# model_data <- load_model_data()
# new_data_mean <- get_new_data_mean(model_data)
# traits <- readRDS("./data/processedtraits.rds")
# pocc <- msod::apply_to_new_data(msod::poccupy, model_data, new_data_mean,
#                                 funargs = list(lvvfromposterior = FALSE, margLV = FALSE))
# set.seed(5464)
# sampledraws <- sample.int(nrow(model_data$mcmc[[1]]) * length(model_data$mcmc),
#                           size = 100, replace = TRUE)
# pocc <- pocc[,, sampledraws, drop = FALSE]
# fd_meansite <- msod::fd_pocc(traits, t(drop(pocc)), nsim = 1)
# fd_meansite$sampledraws <- sampledraws
# fd_meansite$V <- NULL
# saveRDS(fd_meansite, file = "./data/saved_meansite_fctldiv.rds")

functdivplot <- function(model_data, current_values, points, selected_region){
  newXocc <- newXocc_fromselected(model_data, current_values,
                                  points, selected_region)
  traits <- readRDS("./data/processedtraits.rds")
  fd_meansite <- readRDS("./data/saved_meansite_fctldiv.rds")
  pocc <- msod::apply_to_new_data(msod::poccupy, model_data, newXocc,
                            funargs = list(lvvfromposterior = FALSE, margLV = FALSE))
  pocc <- pocc[,, fd_meansite$sampledraws, drop = FALSE]
  fd_l <- lapply(1:nrow(pocc),
         function(site){
         sitefd <- msod::fd_pocc(traits, t(pocc[site, , , drop = TRUE]), nsim = 1)
         return(as.data.frame(sitefd$E))
         })
  names(fd_l) <- 1:nrow(pocc)
  fd <- dplyr::bind_rows(fd_l, .id = "Patch")
  
  plt_abs <- fd %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-Patch) %>%
    ggplot() +
    # geom_histogram(aes(x = value), alpha = 0.5) +
    stat_summary(aes(x = value, y = Patch),
                 geom = "errorbar",
                 fun.data = median_hilow,
                 # fun.args = list(mult = 2),
                 na.rm = TRUE) +
    stat_summary(aes(x = value, y = Patch),
                 geom = "point",
                 fun.data = median_hilow,
                 # fun.args = list(mult = 2),
                 na.rm = TRUE) +
    facet_wrap(vars(name), scales = "free", ncol = 1) +
    ggtitle("Function Diversity of Each Patch") +
    scale_x_continuous("")
  
  # relative increase
  fd_l_r <- lapply(fd_l, function(x) (100 * x / fd_meansite$E))
  fd_r <- dplyr::bind_rows(fd_l_r, .id = "Patch")
  plt_r <- fd_r %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(-Patch) %>%
    ggplot() +
    # geom_histogram(aes(x = value), alpha = 0.5) +
    stat_summary(aes(x = value, y = Patch),
                 geom = "col",
                 fun.data = median_hilow,
                 na.rm = TRUE) +
    stat_summary(aes(x = value, y = Patch),
                 geom = "errorbar",
                 fun.data = median_hilow,
                 na.rm = TRUE,
                 fatten = 0.7) +
    facet_wrap(vars(name), scales = "fixed", ncol = 1) +
    ggtitle("Functional Diversity Relative to our Mean Site") +
    coord_cartesian(xlim = c(0, 200)) +
    scale_x_continuous("")
  return(list(abs = plt_abs, rel = plt_r))
}