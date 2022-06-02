#-------------------------------------------------------------------------------
# Tune num.trees in Random Forest
# Written by: Jessica Li
# 
# Set tree hyperparameters to some constants. Vary num.trees. Choose num.trees
# that sufficiently optimizes metrics.
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

# Use same num.trees across cutoffs
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
learner <- lrn("regr.ranger")
rsmpl_desc <- rsmp("repeated_cv", repeats = 5, folds = 3)
measures <- list(msr("regr.rsq"), msr("regr.rmse"))
nf <- length(names(dat_train_c)) - 1
params <- ps(mtry = p_int(2, 2),
             min.node.size = p_int(60, 60),
             max.depth = p_int(7, 15),
             sample.fraction = p_dbl(0.8, 0.8),
             num.trees = p_int(100, 10000))
design <- data.table(expand.grid(
  mtry = 2,
  min.node.size = 60,
  max.depth = 15,
  sample.fraction = 0.8,
  num.trees = c(1000, 3000, 5000, 7000, 10000)
))
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

# archive1 <- archive
# archive <- rbind(archive0, archive1) %>% filter(max.depth == 15)

# Plot
# p1 <- ggplot(archive1, aes(num.trees, regr.rsq, group = max.depth, color = max.depth)) + 
#   geom_point()
# 
# p2 <- ggplot(archive1, aes(num.trees, regr.rmse, group = max.depth, color = max.depth)) + 
#   geom_point()

p1 <- ggplot(archive, aes(num.trees, regr.rsq, group = max.depth, color = max.depth)) +
  geom_smooth(se = FALSE)

p2 <- ggplot(archive, aes(num.trees, regr.rmse, group = max.depth, color = max.depth)) +
  geom_smooth(se = FALSE)

p3 <- ggplot(archive, aes(max.depth, regr.rsq, group = num.trees, color = num.trees)) +
  geom_smooth(se = FALSE)

p4 <- ggplot(archive, aes(max.depth, regr.rmse, group = num.trees, color = num.trees)) +
  geom_smooth(se = FALSE)

p1
p2
p3
p4
