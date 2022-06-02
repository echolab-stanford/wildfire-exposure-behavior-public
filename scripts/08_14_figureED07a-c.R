#-------------------------------------------------------------------------------
# Make Figure with Panels for Prior-Posterior Scatter, dY, and R2
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Choose lag structure
est <- paste0("est_", lag_structure)

#### Panel a ####
# Get prior and posterior infiltration estimates
ests_prior <- readRDS(file.path(path_infiltration, pm_path, "not_posterior", "dat_pa_inf.rds")) %>% select(id, est_prior = est)
ests_posterior <- readRDS(file.path(path_infiltration, pm_path, "posterior", "dat_pa_inf.rds")) %>% select(id, est_posterior = est)
ests <- ests_prior %>% full_join(ests_posterior)

#### Panels b and c ####
# Load models
models <- readRDS(file.path(path_infiltration, "models.rds"))

# Get R2 by model configuration details
r2 <- lapply(models, function(x) x[-1][-(which(names(x) == "dY") - 1)]) %>% 
  map_dfr(as.data.frame) %>% 
  filter(model_type %in% c("random forest", "gradient boosted trees"),
         lag_structure == get("lag_structure", .GlobalEnv),
         pm_method == get("pm_method", .GlobalEnv),
         posterior == get("posterior", .GlobalEnv))

# Get dY by model configuration details
dy <- lapply(models, function(x) x[-1]) %>% 
  map_dfr(as.data.frame) %>% 
  rename_with(str_replace, everything(), "^dY.", "") %>% 
  filter(model_type %in% c("random forest", "gradient boosted trees"),
         lag_structure == get("lag_structure", .GlobalEnv),
         pm_method == get("pm_method", .GlobalEnv),
         posterior == get("posterior", .GlobalEnv)) %>% 
  mutate(vbl = mapvalues(
    vbl, 
    c("ac_type", "area", "baths", "bedrooms", "cdd", "ethn_hl", 
      "food_stamps_received", "hdd", "height", "income_median", "lat", "lon", 
      "occ_renter", "pm", "pop_density", "poverty_below", "race_aian", 
      "race_asian", "race_black", "race_nhpi", "race_other", "stories", 
      "total_val", "yrbuilt", "housing_index", "livelihood_index"),
    c("A/C", "Area", "Baths", "Bedrooms", "CDD", "Hispanic", "Food Stamps", "HDD",
      "Height", "Median Income", "Latitude", "Longitude", "Renters", "PM",
      "Pop. Density", "Below Poverty", "AI/AN", "Asian", "Black", "NHPI",
      "Other Race", "Stories", "Home Value", "Year Built", "Housing Index",
      "Livelihood Index")
  )) %>% 
  mutate(category = case_when(
    vbl %in% c("Hispanic", "Black", "Asian", "NHPI", "AI/AN", "Other Race") ~
      "Demographic",
    vbl %in% c("CDD", "HDD", "Latitude", "Longitude", "PM") ~ "Environmental",
    vbl %in% c("Food Stamps", "Median Income", "Renters", "Population Density", 
               "Below Poverty", "Livelihood Index") ~ "Socioeconomic",
    vbl %in% c("A/C", "Area", "Baths", "Bedrooms", "Height", "Stories", 
               "Home Value", "Year Built", "Housing Index") ~ "Housing"
  ),
  category = factor(category, levels = c("Housing", "Socioeconomic", "Demographic", "Environmental")))
vbl_order <- dy %>% 
  group_by(category, vbl) %>%
  summarize(dY = median(dY)) %>%
  ungroup() %>%
  arrange(category, desc(dY)) %>%
  pull(vbl) %>% 
  unique()
dy <- dy %>% mutate(vbl = factor(vbl, levels = vbl_order))

#-------------------------------------------------------------------------------
#### Plot ####
out_file <- file.path(path_github, "figures/raw/figureED07a-c.pdf")
pdf(out_file, width = 13, height = 6)

# Partition space among panels
layout(matrix(c(1, 2, 3, 3, 3, 3, 3, 3), ncol = 4))

# Make all tick text horizontal
par(las = 1)

# Panel a
plot(ests$est_prior, ests$est_posterior, xlab = "Prior", ylab = "Posterior",
     axes = FALSE, pch = 19, col = rgb(0, 0, 0, 0.15))
text(0.1, max(ests$est_posterior), substitute(R^2 == r2, list(r2 = round(summary(lm(est_posterior ~ est_prior, ests))$r.squared, 4))))
axis(1)
axis(2)

# Panel b
hist(r2$R2_test, xlab = expression(R^2), ylab = "Models", main = "", axes = FALSE)
axis(1)
if (pm_method == "EPA") axis(2)
if (pm_method == "PC") axis(2, at = 0:2, labels = 0:2)

# Panel c
# Get color and shape
color_rf <-rgb(178/255,34/255,34/255)
color_gb <- rgb(65/255,105/255,225/255)
shape_100 <- 1
shape_250 <- 2
shape_500 <- 3
shape_nn <- 4
dy <- dy %>% mutate(color = ifelse(model_type == "random forest", color_rf, color_gb),
                    shape = case_when(match_method == "100" ~ shape_100,
                                      match_method == "250" ~ shape_250,
                                      match_method == "500" ~ shape_500,
                                      match_method == "nn" ~ shape_nn))
plot.new()
plot.window(xlim = c(1, length(levels(dy$vbl)) + 1), ylim = c(min(dy$dY), max(dy$dY)))
points.default(dy$vbl, dy$dY, pch = dy$shape, col = dy$color)
segments(x0 = 0, x1 = length(levels(dy$vbl)) + 0.5, y0 = 0, lty = "dashed")
legend(x = length(levels(dy$vbl)) + 0.85, y = ifelse(pm_method == "EPA", 0.0075, 0.019),
       pch = c(shape_100, shape_250, shape_500, shape_nn), legend = c("100m", "250m", "500m", "1km NN"), 
       bty = "n", xpd = TRUE, yjust = 1)
legend(x = length(levels(dy$vbl)) + 0.5, y = ifelse(pm_method == "EPA", -0.007, -0.018),
       col = c(color_rf, color_gb), legend = c("Random\nForest", "Gradient\nBoosted\nTrees"),
       lty = 1, bty = "n", xpd = TRUE, yjust = 0)

# Keep y-label from overlapping negative signs
par(mar = c(5, 4 + 1, 4, 2) + 0.1)
axis(2)
title(ylab = "Change in predicted infiltration", line = 4)

# x-ticks
dy_labels <- ifelse(str_detect(levels(dy$vbl), " "), 
                    gsub(" ", "\n", levels(dy$vbl)), 
                    paste0(levels(dy$vbl), "\n"))
axis(1, at = unique(dy$vbl)[str_which(unique(dy$vbl), "PM")], labels = expression(PM[2.5]))
par(mgp = c(3, 1.85, 0))
axis(1, at = sort(unique(dy$vbl)), labels = ifelse(str_detect(dy_labels, "PM"), NA, dy_labels))

dev.off()
