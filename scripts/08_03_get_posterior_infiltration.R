#-------------------------------------------------------------------------------
# Get Posterior Infiltration Estimates
# Written by: Jessica Li, Marissa Childs
# 
# Be sure to run with posterior set to FALSE in settings.
#-------------------------------------------------------------------------------
stopifnot(!posterior)

# Read in infiltration estimates
dat_inf_full <- readRDS(pa_file)

#-------------------------------------------------------------------------------
# Distributed lag model
dat_inf <- dat_inf_full %>%
  mutate(est_dl = ifelse(est_dl < 0 | est_dl > 1, NA, est_dl)) %>% 
  drop_na(est_dl, se_dl)
if (pm_method %in% c("PA", "EPA")) dat_inf <- dat_inf %>% filter(building_type == "sfr")

# Set up named data list
stan_data <- list(
  J = nrow(dat_inf),
  tau_hat = dat_inf$est_dl,
  se_hat = dat_inf$se_dl
)

# Fit model
set.seed(222)
stan_fit <- stan(
  # Stan program
  file = file.path(path_supplementary, "define_bayesian_hierarchical_model.stan"),
  data = stan_data,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warm-up iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores 
  refresh = 0             # no progress shown
)

# Get posteriors' means
post_inf <- summary(stan_fit, pars = "theta")$summary[, c("mean", "se_mean")] %>% 
  as.data.frame() %>% 
  rename(post_est_dl = mean, post_se_dl = se_mean)
dat_inf <- dat_inf %>% 
  mutate(est_dl = post_inf$post_est_dl,
         se_dl = post_inf$post_se_dl)
dat_inf_dl <- dat_inf %>% select(id, est_dl, se_dl)

#-------------------------------------------------------------------------------
# No lag model
dat_inf <- dat_inf_full %>% 
  mutate(est_nl = ifelse(est_nl < 0 | est_nl > 1, NA, est_nl)) %>% 
  drop_na(est_nl, se_nl)
if (pm_method %in% c("PA", "EPA")) dat_inf <- dat_inf %>% filter(building_type == "sfr")

# Set up named data list
stan_data <- list(
  J = nrow(dat_inf),
  tau_hat = dat_inf$est_nl,
  se_hat = dat_inf$se_nl
)

# Fit model
set.seed(222)
stan_fit <- stan(
  # Stan program
  file = file.path(path_supplementary, "define_bayesian_hierarchical_model.stan"),
  data = stan_data,
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warm-up iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores 
  refresh = 0             # no progress shown
)

# Get posteriors' means
post_inf <- summary(stan_fit, pars = "theta")$summary[, c("mean", "se_mean")] %>% 
  as.data.frame() %>% 
  rename(post_est_nl = mean, post_se_nl = se_mean)
dat_inf <- dat_inf %>% 
  mutate(est_nl = post_inf$post_est_nl,
         se_nl = post_inf$post_se_nl)
dat_inf_nl <- dat_inf %>% select(id, est_nl, se_nl)

#-------------------------------------------------------------------------------
# Join posterior estimates and SEs to original data frame
o <- names(dat_inf_full)
dat_inf <- dat_inf_full %>% select(-est_dl, -se_dl, -est_nl, -se_nl)
dat_inf <- list(dat_inf, dat_inf_dl, dat_inf_nl) %>% 
  reduce(left_join) %>% 
  select(o)

# Save updated data
saveRDS(dat_inf, post_file)
