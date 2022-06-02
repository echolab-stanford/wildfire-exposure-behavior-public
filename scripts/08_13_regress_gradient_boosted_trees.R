#-------------------------------------------------------------------------------
# Regress Infiltration on House and Community Characteristics w/ Gradient 
# Boosted Trees
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Read in train and test data
dat_train <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_train.rds"))
dat_test <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_test.rds"))

# Choose lag configuration in 00_utils: est_ldv (default), est_dl, est_nl, est_al
est <- paste0("est_", lag_structure)

# Set model specifications
spec <- c(
  est, 
  # Variables common across specifications
  "ac_type", "pop_density", "ethn_hl", "cdd", "hdd", "pm", 
  # All race variables, excluding race_white for interpretability
  "race_black", "race_aian", "race_asian", "race_nhpi", "race_other", 
  # Indices
  "housing_index",
  "income_median",
  "yrbuilt"
)

cutoffs <- c("100", "250", "500", "nn")
mdls <- vector("list", length(cutoffs))
m <- 1
try(log_file <- file(paste0("~/Desktop/gradient_boosted_trees_", 
                            format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".log"), 
                     open = "at"))
sink(log_file, type="output", append = TRUE, split = TRUE)
sink(log_file, type="message", append = TRUE)

for (cut in cutoffs) {
  # Get data for the given matching cutoff criterion
  dat_train_c <- dat_train %>% 
    filter(cutoff == cut) %>% 
    select(spec)
  dat_test_c <- dat_test %>% 
    filter(cutoff == cut) %>% 
    select(spec)
  
  # Choose resampling method and define parameter search space
  task <- TaskRegr$new(id = "infiltration", backend = dat_train_c, target = est)
  learner <- lrn("regr.xgboost")
  rsmpl_desc <- rsmp("repeated_cv", repeats = 5, folds = 3)
  measures <- list(msr("regr.rsq"), msr("regr.rmse"))
  nf <- length(names(dat_train_c)) - 1
  params <- ps(eta = p_dbl(0.1, 0.1),
               min_child_weight = p_int(10, 200),
               max_depth = p_int(1, 8),
               subsample = p_dbl(0.4, 1),
               nrounds = p_int(80, 80))
  terminator <- trm("combo", list(trm("evals", n_evals = 1000),
                                  # 1 hour = 60 s/min * 60 min/hr
                                  trm("clock_time", stop_time = Sys.time() + 60*60)))
  tuner <- tnr("random_search")
  instance <- TuningInstanceMultiCrit$new(task = task,
                                          learner = learner,
                                          resampling = rsmpl_desc,
                                          measure = measures,
                                          search_space = params,
                                          terminator = terminator)
  
  # Tune hyperparameters
  start_time <- get_start_time()
  set.seed(123)
  res <- tuner$optimize(instance)
  print_time(start_time)
  
  # Choose based on RMSE
  tuned <- res[2]
  
  # Fit gradient boosted trees model
  eta_final <- 0.001
  nrounds_final <- 9000
  set.seed(3473)
  mdl_gb <- xgboost(data = dat_train_c %>% select(-est) %>% as.matrix(),
                    label = pull(dat_train_c, est),
                    eta = eta_final,
                    min_child_weight = tuned$min_child_weight,
                    max_depth = tuned$max_depth,
                    subsample = tuned$subsample,
                    nrounds = nrounds_final)
  
  # Get train performance
  r2_train <- tuned$regr.rsq
  rmse_train <- tuned$regr.rmse
  
  # Predict on test set
  pred_gb <- predict(mdl_gb, dat_test_c %>% select(-est) %>% as.matrix())
  dat_test_c$pred_gb <- pred_gb
  
  # Get test performance
  r2_test <- summary(lm(sprintf("pred_gb ~ %s", est), dat_test_c))$r.squared
  rmse_test <- sqrt(mean((pull(dat_test_c, est) - pred_gb)^2))
  
  # Calculate 5th and 95th percentile values of x
  pctls <- dat_test_c %>% 
    select(-pred_gb, -eval(est)) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "vbl", 
                 values_to = "val") %>%
    group_by(vbl) %>%
    summarize(pctl5 = quantile(val, 0.05),
              pctl95 = quantile(val, 0.95),
              mean = mean(val)) %>%
    ungroup() %>% 
    pivot_longer(cols = c(pctl5, pctl95),
                 names_to = "pctl",
                 names_prefix = "pctl",
                 values_to = "x_pctl")
  
  # Vary each feature from 5th to 95th percentile while holding other features
  # constant at their mean
  pctls_c <- pctls %>% 
    mutate(x = vbl) %>% 
    pivot_wider(names_from = x,
                values_from = mean) %>% 
    fill(-vbl, -pctl, -x_pctl, .direction = "downup")
  
  pctls_v <- pctls %>% 
    mutate(x = vbl) %>% 
    pivot_wider(names_from = x,
                values_from = x_pctl)
  
  x_v <- pctls_v %>% select(-vbl, -mean, -pctl)
  i <- !is.na(x_v)
  x_c <- pctls_c %>% select(-vbl, -pctl, -x_pctl)
  
  pctls_cv <- as.matrix(x_c)
  pctls_cv[i] <- as.matrix(x_v)[i]
  
  pctls <- pctls %>% 
    select(vbl, pctl) %>% 
    bind_cols(as.data.frame(pctls_cv))
  
  # Get predicted infiltration at 5th and 95th percentiles of X
  pctls$pred_gb <- predict(mdl_gb, pctls %>% select(setdiff(spec, est)) %>% as.matrix())
  
  # Calculate dY
  dy <- pctls %>% 
    select(vbl, pctl, pred_gb) %>% 
    pivot_wider(names_from = pctl,
                names_prefix = "pred_pctl",
                values_from = pred_gb) %>% 
    mutate(dY = pred_pctl95 - pred_pctl5)
  
  # Collect model, model configuration details, R2, and dY
  mdls[[m]] <- list(mdl_gb, 
                    model_type = "gradient boosted trees",
                    hyperparameters = list(eta = eta_final,
                                           min_child_weight = tuned$min_child_weight,
                                           max_depth = tuned$max_depth,
                                           subsample = tuned$subsample,
                                           nrounds = nrounds_final),
                    pm_method = pm_method,
                    lag_structure = lag_structure,
                    posterior = posterior,
                    match_method = cut,
                    R2_train = r2_train,
                    RMSE_train = rmse_train,
                    R2_test = r2_test,
                    RMSE_test = rmse_test,
                    dY = dy)
  
  m <- m + 1
}
sink(type = "output")
sink(type = "message")
close(log_file)

# Save models
saveRDS(mdls, file.path(path_infiltration, paste0("models/models_", pm_method, "_GBT.rds")))

# Add to list of models
file_models <- file.path(path_infiltration, "models.rds")
if (file.exists(file_models)) mdls <- readRDS(file_models) %>% setdiff(mdls) %>% c(mdls)

# Save all models
# Takes ~10 minutes
saveRDS(mdls, file_models)
