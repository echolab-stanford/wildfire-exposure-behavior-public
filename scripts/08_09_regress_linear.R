#-------------------------------------------------------------------------------
# Fit Linear Model of Infiltration on House and Community Characteristics
# Written by: Jessica Li
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Read in train and test data
dat_train <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_train.rds"))
dat_test <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_test.rds"))

# Choose lag configuration in 00_utils: est_ldv (default), est_dl, est_al, est_nl
est <- paste0("est_", lag_structure)

# Set cutoff values
cutoffs <- c("100", "250", "500", "nn")
num_cutoffs <- length(cutoffs)

# Set specification
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

dat_train <- dat_train %>% select(spec, cutoff)
dat_test <- dat_test %>% select(spec, cutoff)

mdls <- vector("list", num_cutoffs)
i <- 1
for (cut in cutoffs) {
  # Filter data by cutoff
  dat_train_c <- dat_train %>% filter(cutoff == cut) %>% select(-cutoff)
  dat_test_c <- dat_test %>% filter(cutoff == cut) %>% select(-cutoff)
  
  # Fit model
  mdl <- lm(sprintf("%s ~ .", est), dat_train_c)
  
  # Get train performance
  preds <- mdl$fitted.values
  df <- data.frame(dat_train_c[est], pred = preds)
  r2_train <- summary(lm(sprintf("%s ~ pred", est), df))$r.squared
  rmse_train <- sqrt(mean((pull(dat_train_c, est) - preds)^2))
  
  # Get test performance
  preds <- predict(mdl, dat_test_c)
  df <- data.frame(dat_test_c[est], pred = preds)
  r2_test <- summary(lm(sprintf("%s ~ pred", est), df))$r.squared
  rmse_test <- sqrt(mean((pull(dat_test_c, est) - preds)^2))
  
  # Get betas
  coeffs <- mdl %>% 
    coefficients() %>% 
    stack() %>% 
    rename(vbl = ind, b = values) %>% 
    filter(vbl != "(Intercept)")
  
  # Calculate difference between 5th and 95th percentile values of x
  pctls <- dat_test_c %>% 
    select(setdiff(spec, est)) %>% 
    pivot_longer(cols = everything(),
                 names_to = "vbl",
                 values_to = "val") %>% 
    group_by(vbl) %>% 
    summarize(pctl5 = quantile(val, 0.05),
              pctl95 = quantile(val, 0.95)) %>%
    ungroup()
  
  # Calculate dY
  dy <- coeffs %>% 
    left_join(pctls) %>% 
    mutate(dX = pctl95 - pctl5,
           dY = b*dX) %>% 
    select(vbl, b, pctl5, pctl95, dX, dY)
  
  # Append to list
  mdls[[i]] <- list(mdl,
                    model_type = "linear",
                    pm_method = pm_method,
                    lag_structure = lag_structure,
                    posterior = posterior,
                    match_method = cut,
                    R2_train = r2_train,
                    RMSE_train = rmse_train,
                    R2_test = r2_test,
                    RMSE_test = rmse_test,
                    dY = dy)
  
  i <- i + 1
}

# Save models
if (!dir.exists(file.path(path_infiltration, "models"))) dir.create(file.path(path_infiltration, "models"))
saveRDS(mdls, file.path(path_infiltration, "models/models_", pm_method, "_linear.rds"))

# Add to list of models
file_models <- file.path(path_infiltration, "models.rds")
if (file.exists(file_models)) mdls <- readRDS(file_models) %>% setdiff(mdls) %>% c(mdls)

# Save all models
saveRDS(mdls, file_models)
