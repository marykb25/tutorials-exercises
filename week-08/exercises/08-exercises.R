# ============================================================================
# Multiple regression in R 
#
# The exercises use palmerpenguins::penguins, restricted to Chinstrap penguins.
# Requires tidyverse, broom, and palmerpenguins to be installed.
# ============================================================================

	library(tidyverse)
	library(broom)
	library(palmerpenguins)
	
	theme_set(theme_bw(base_size = 12))

# ----------------------------------------------------------------------------
# 1. Prepare and explore the data
# ----------------------------------------------------------------------------
# Question: How does mean flipper length vary with body mass and sex
# in Chinstrap penguins?
#
# a. Create chinstrap_dat: keep Chinstrap penguins, remove missing values
#    in flipper_length_mm, body_mass_g, bill_depth_mm, and sex, and make
#    female the reference level for sex. Use this sample throughout.

	chinstrap_data <- penguins |>
	  filter(species == "Chinstrap") |>
	  drop_na(flipper_length_mm, body_mass_g, bill_depth_mm, sex) |>
	  mutate(sex = factor(sex, levels = c("female", "male")))

# b. Count the penguins of each sex.

	chinstrap_data |>
	  count(sex)
	
# Answer: 34 females, 34 males 

# c. Plot body mass (x) and flipper length (y), colored by sex.
	
	starter_plot <- ggplot(
	  data = chinstrap_data,
	  mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)
	) +
	  geom_point(alpha = 0.7) +
	  labs(
	    x = "Body Mass (g)",
	    y = "Flipper Length (mm)",
	    color = "Sex",
	    title = "Chinstrap Penguins: flipper length vs. body mass"
	  )
	
	starter_plot

# d. Describe the relationship and the overlap in body-mass ranges.
	
	# Flipper length generally increases with body mass.
	# The observed ranges of both females and males overlap between 3250 to 4150 g. 
	# This shared range is where comparisons between sexes at the same mass will be best supported. 

# ----------------------------------------------------------------------------
# 2. Fit and interpret an additive model 
# ----------------------------------------------------------------------------
# a. Fit an additive model using body mass and sex as predictors.
	
	chinstrap_add <- lm(
	  flipper_length_mm ~ body_mass_g + sex,
	  data = chinstrap_data
	)

# b. Examine coefficients with 95% confidence intervals.

	add_coefficients <- tidy(chinstrap_add, conf.int = TRUE)
	
	add_coefficients

# c. Interpret each coefficient, including units and what is held constant.

	# Intercept = 161.269 mm
	# This is the predicted mean flipper length for a female with body mass 0 g. 
	# body_mass_g = 0.008637 mm/g
	# This means that a 1 g increase in body mass is associated with a predicted 0.008637 mm increase 
	# in mean flipper length, if sex is held constant. The model assumes the same slope for both sexes.
	# sexmale = 4.620 mm
	# This means that at the same body mass, predicted mean flipper length is 4.620 mm greater for males than females. 
	# The model assumes this difference is constant across body mass.

# d. Calculate the predicted change for a 100 g increase in body mass.
	
	change_per_100g <- add_coefficients |>
	  filter(term == "body_mass_g") |>
	  transmute(change_per_100g_mm = estimate * 100)
	
	change_per_100g

# e. Does the sexmale confidence interval include zero? Interpret it.

	add_coefficients |>
	  filter(term == "sexmale") |>
	  select(term, estimate, conf.low, conf.high, p.value)
	
	# The interval is approximately 1.636 to 7.604 mm, so it does not include zero. 
	# Under the additive model and its assumptions, there is evidence of a
	# greater mean flipper length for males at the same body mass.

# f. What assumption is made about slopes?
	
	# Females and males share one body-mass slope and predicted lines
	# are parallel. Therefore, this model does not establish that population
	# slopes are identical.

# ----------------------------------------------------------------------------
# 3. Generate and plot predictions 
# ----------------------------------------------------------------------------
# a. Generate 100 masses within each sex's observed body-mass range.

	chinstrap_grid <- chinstrap_data |>
	  group_by(sex) |>
	  reframe(
	    body_mass_g = seq(
	      min(body_mass_g),
	      max(body_mass_g),
	      length.out = 100
	    )
	  )

# b. Generate predicted means and 95% confidence intervals.
	
	chinstrap_predictions <- predict(
	  chinstrap_add,
	  newdata = chinstrap_grid,
	  interval = "confidence",
	  level = 0.95
	) |>
	  as_tibble()
	
	chinstrap_bands <- bind_cols(chinstrap_grid, chinstrap_predictions)

# c. Plot observations, predictions, and confidence ribbons.

	p_additive <- ggplot(
	  data = chinstrap_data,
	  mapping = aes(x = body_mass_g, y = flipper_length_mm, color = sex)) +
	  geom_ribbon(
	    data = chinstrap_bands,
	    mapping = aes(
	      x = body_mass_g, ymin = lwr, ymax = upr,
	      fill = sex, group = sex),
	    inherit.aes = FALSE,
	    alpha = 0.18) +
	  geom_point(alpha = 0.5) +
	  geom_line(
	    data = chinstrap_bands,
	    mapping = aes(y = fit, group = sex),
	    linewidth = 1.1) +
	  labs(
	    x = "Body Mass (g)",
	    y = "Flipper Length (mm)",
	    color = "Sex", fill = "Sex",
	    title = "Chinstrap Penguins: Additive Model",
	    subtitle = "Predicted means with 95% pointwise confidence intervals"
	  )
	
	p_additive

# d. Why are the predicted lines parallel?
	
	# Because the additive model only fits one shared body-mass slope, the sexmale
	# coefficient shifts the male line vertically without changing its slope.
	
# e. What do the shaded bands describe?
	
	# The shaded bands represent uncertainty in the predicted mean flipper length at each mass
	# for each sex. 

# ----------------------------------------------------------------------------
# 4. Fit and evaluate an interaction model 
# ----------------------------------------------------------------------------
# a. Fit a model that allows the body-mass slope to differ by sex.
	
	chinstrap_int <- lm(
	  flipper_length_mm ~ body_mass_g * sex,
	  data = chinstrap_data
	)

# b. Inspect coefficients and confidence intervals.
	
	int_coefficients <- tidy(chinstrap_int, conf.int = TRUE)
	
	int_coefficients
	
# c. Report the female slope, slope difference, and male slope.
	
	female_slope <- int_coefficients |>
	  filter(term == "body_mass_g") |>
	  pull(estimate)
	
	slope_difference <- int_coefficients |>
	  filter(term == "body_mass_g:sexmale") |>
	  pull(estimate)
	
	slope_summary <- tibble(
	  female_slope_mm_per_g = female_slope,
	  male_minus_female_slope_mm_per_g = slope_difference,
	  male_slope_mm_per_g = female_slope + slope_difference
	)
	
	slope_summary
	
	# Female slope = 0.004883 mm/g
	# Male-minus-female difference = 0.006085 mm/g
	# Male slope = 0.010968 mm/g 
	
# d. Explain the change in the sexmale interpretation.
	
	# In the additive model, sexmale is the constant male-minus-female
	# difference at any body mass. In the interaction model, sexmale now represents the
	# difference specifically at body mass 0 g.
	# Therefore, in an interaction, the difference at mass x is:
	# sexmale coefficient + interaction coefficient * x.

# e. Compare AIC values and calculate the difference.
	
	aic_comparison <- AIC(chinstrap_add, chinstrap_int) |>
	  as_tibble(rownames = "model") |>
	  mutate(delta_AIC = AIC - min(AIC))
	
	aic_comparison

	# Additive AIC is approximately 421.754, while the interaction AIC is
	# approximately 421.324. The interaction model is lower by about 0.430.
	
# f. Examine uncertainty in the interaction coefficient.

	int_coefficients |>
	  filter(term == "body_mass_g:sexmale") |>
	  select(term, estimate, conf.low, conf.high, p.value)

# g. Which model would you select, and why?
	
	# I would select the additive model because the AIC support
	# is similar and the additional interaction coefficient is uncertain.
	# The simpler model is ultimately more parsimonious.
	
# ----------------------------------------------------------------------------
# 5. Examine model diagnostics 
# ----------------------------------------------------------------------------
# Use the additive model for the following diagnostics.
# a. Inspect all four default diagnostic plots.

	plot(chinstrap_add)

# b. Obtain fitted values and residuals.

	chinstrap_results <- augment(chinstrap_add, data = chinstrap_data)
	
# c. Plot residuals against body mass, colored by sex.
	
	p_residuals <- ggplot(
	  data = chinstrap_results,
	  mapping = aes(x = body_mass_g, y = .resid, color = sex)
	) +
	  geom_point(alpha = 0.7) +
	  geom_hline(yintercept = 0, linetype = "dashed") +
	  labs(
	    x = "Body mass (g)",
	    y = "Residual (mm)",
	    color = "Sex",
	    title = "Additive model: residuals versus body mass"
	  )
	
	p_residuals

# d. Describe any patterns in the residuals, including curvature,
#    changes in spread, or differences between sexes.
	
  # There is a fair amount of scatter in the residuals. 
	# There is also a slight curve to the fitted line. 
	# There is still a big overlap between the sexes. 

# e. Examine the Q-Q plot. Are there substantial departures from the line?
	
	# Both tails depart from the line, but they may not be substantial enough to cause an issue. 

# ----------------------------------------------------------------------------
# 6. Center and standardize numeric predictors 
# ----------------------------------------------------------------------------
# New question: How does mean flipper length vary with body mass and
# bill depth?
#
# a. Fit the model in original units.
	
	chinstrap_numeric <- lm(
	  flipper_length_mm ~ body_mass_g + bill_depth_mm,
	  data = chinstrap_data
	)

# b. Center each predictor using its mean.
	
	chinstrap_data <- chinstrap_data |>
	  mutate(
	    body_mass_c = body_mass_g - mean(body_mass_g),
	    bill_depth_c = bill_depth_mm - mean(bill_depth_mm)
	  )

# c. Fit a model with centered predictors.
	
	chinstrap_centered <- lm(
	  flipper_length_mm ~ body_mass_c + bill_depth_c,
	  data = chinstrap_data
	)

# d. Standardize both predictors: subtract the mean, then divide by SD.
	
	chinstrap_data <- chinstrap_data |>
	  mutate(
	    body_mass_z = (body_mass_g - mean(body_mass_g)) / sd(body_mass_g),
	    bill_depth_z = (bill_depth_mm - mean(bill_depth_mm)) / sd(bill_depth_mm)
	  )

# e. Fit a model with standardized predictors.
	
	chinstrap_standardized <- lm(
	  flipper_length_mm ~ body_mass_z + bill_depth_z,
	  data = chinstrap_data
	)

# f. Examine the coefficients from all three models.
#    Interpret the intercept in the centered model and both slopes
#    in the standardized model. Include units and explain what
#    is held constant.
	
	coefficient_comparison <- bind_rows(
	  original = tidy(chinstrap_numeric, conf.int = TRUE),
	  centered = tidy(chinstrap_centered, conf.int = TRUE),
	  standardized = tidy(chinstrap_standardized, conf.int = TRUE),
	  .id = "model") |>
	  select(model, term, estimate, std.error, conf.low, conf.high)
	
	coefficient_comparison
	
	# The centered intercept is approximately 195.824 mm, which is the predicted
	# mean flipper length at the sample's mean mass (3733.088 g) and mean
	# bill depth (18.421 mm). 
	
	# The body_mass_z coefficient is approximately 3.269 mm, which means that a one-SD
	# increase in body mass (about 384.335 g) is associated with a predicted
	# 3.269 mm increase in mean flipper length, holding bill depth constant.
	
	# The bill_depth_z coefficient is approximately 2.161 mm per one-SD 
	# increase in bill depth (about 1.135 mm), as long as body mass is held constant. 

# g. Verify that the fitted values are equal across all three models,
# allowing for numerical rounding.
   
	all.equal(
	  fitted(chinstrap_numeric),
	  fitted(chinstrap_centered)
	)
	
	all.equal(
	  fitted(chinstrap_numeric),
	  fitted(chinstrap_standardized)
	)
	
# h. Explain what happens to the slopes when you center or
#    standardize the predictors.	
	
	# Centering shifts the predictor origin. A 1 g or 1 mm increment still
	# has its original size, so the additive-model slopes are unchanged.
	# Standardizing changes to units of SD. Each new slope
	# equals the original slope multiplied by that predictor's sample SD.
	# Flipper length was not standardized. The response remains in mm.
	# Centered and standardized models have the same intercept because
	# both assign zero to each predictor's sample mean.

# Optional check of the means, SDs, and fit summaries:
	
	
# i. Would standardizing fix curvature or unequal residual variance?
	
  # Standardizing would not fix curvature or unequal residual variance. 
	# Centering and standardizing predictors leave fitted values
	# and residuals unchanged for these equivalent additive linear models.

# ============================================================================
# Optional extension if time:
# compare additive and interaction predictions visually
# ============================================================================
# Generate predictions on the same grid and plot models side by side.
	
