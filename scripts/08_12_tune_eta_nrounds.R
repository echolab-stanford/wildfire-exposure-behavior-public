#-------------------------------------------------------------------------------
# Tune eta and nrounds in Gradient Boosted Trees
# Written by: Jessica Li
# Last edited by: Jessica Li
# 
# Set tree hyperparameters to some constants. Vary eta and nrounds. Find metric-
# optimizing combinations for tuning (low nrounds) and training (low eta).
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

# Use same boosting parameters across cutoffs
cut <- cutoffs[1]

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
params <- ps(eta = p_dbl(0.001, 0.3),
             min_child_weight = p_int(10, 200),
             max_depth = p_int(1, 8),
             subsample = p_dbl(0.4, 1),
             nrounds = p_int(20, 20000))
# 0.1 and 80
design_tuning <- data.table(expand.grid(
  eta = c(0.1),
  min_child_weight = 100,
  max_depth = 2,
  subsample = 0.5,
  nrounds = c(30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 170, 200, 220, 250, 300)
))
# 0.001 and 9200
design_training <- data.table(expand.grid(
  eta = c(0.001),
  min_child_weight = 100,
  max_depth = 2,
  subsample = 0.5,
  nrounds = c(8000, 8500, 9000, 9500) # c(5000, 7000, 10000, 12000, 15000, 20000)
))
# design <- design_tuning
design <- design_training
terminator = trm("none")
tuner <- tnr("design_points", design = design)
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

# Save results from each tuning iteration
archive <- as.data.table(instance$archive) %>% select(-starts_with("x_"))

# Plot
p1 <- ggplot(archive, aes(nrounds, regr.rsq, color = eta)) + 
  geom_point()
p2 <- ggplot(archive, aes(nrounds, regr.rmse, color = eta)) + 
  geom_point()

p1
p2

# Based on values
archive %>% filter((regr.rsq == max(regr.rsq)) | (regr.rmse == min(regr.rmse)))
