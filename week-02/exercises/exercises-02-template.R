# Exercise 1: R Basics
# Independent practice with objects, vectors, functions, and data frames
#
# Estimated time: 15–20 minutes
#
# Instructions:
# 1. Complete all required sections.
# 2. Write your code below each question.
# 3. Use descriptive object names written in snake_case.
# 4. Run the entire script from top to bottom before finishing.
# 5. Do not delete the questions or instructions.
# 6. Save, commit, and push your completed script.
#
# Name: Mary Brantley
# Date: 09-15-2026


	#### Part 1: Objects and calculations ####
	
	# A coastal survey recorded a water temperature of 16.8 degrees Celsius.
	
	# 1. Create an object named water_temp_c containing this value.
	
	water_temp_c <- 16.8

	# 2. Convert the temperature to Fahrenheit using:
	#
	#    F = C * (9 / 5) + 32
	#
	#    Save the result as water_temp_f.
	
	water_temp_f <- water_temp_c * (9 / 5) + 32
	
	# 3. Print water_temp_c and water_temp_f.
	
	water_temp_c
	water_temp_f
	
	# 4. Round water_temp_f to one decimal place.
	
	round(water_temp_f, digits = 1)

	#### Part 2: Vectors and summary functions ####
	
	# The lengths, in centimeters, of eight fish are:
	
	fish_lengths <- c(24, 31, 28, 35, 42, 30, 37, 33)
	
	# Use R to calculate each of the following.
	
	# 1. The number of fish measured.
	
	length(fish_lengths)
	
	# 2. The mean fish length.
	
	mean(fish_lengths)
	
	# 3. The minimum fish length.
	
	min(fish_lengths)

	# 4. The maximum fish length.
	
	max(fish_lengths)
	
	# 5. The number of fish longer than 32 cm.
	
	length(fish_lengths[fish_lengths > 32])
	
	# 6. Create a vector named fish_over_32 containing only fish
	#    longer than 32 cm.
	
	large_fish <- c(fish_lengths[fish_lengths > 32])
	
	# 7. Convert all fish lengths from centimeters to millimeters
	#    and save the result as fish_lengths_mm.
	
	fish_lengths_mm <- fish_lengths * 10

	#### Part 3: Missing values ####
	
	# Some fish masses were not recorded.
	
	fish_mass <- c(1.2, 1.5, NA, 1.8, 2.1, NA, 1.4)
	
	# 1. Count the number of missing values.
	
	sum(is.na(fish_mass))
	
	# 2. Count the number of observed values.
	
	sum(!is.na(fish_mass))
	
	# 3. Calculate the mean mass while ignoring missing values.
	#    Save the result as mean_fish_mass.
	
	mean_fish_mass <- mean(fish_mass, na.rm = TRUE)
	mean_fish_mass
	
	# 4. Calculate the total mass while ignoring missing values.
	
	sum(fish_mass, na.rm = TRUE)

	#### Part 4: Build a data frame ####
	
	# Create a data frame named survey_data using the information below:
	#
	# station:   A, B, C, D, E, F
	# species:   cod, haddock, cod, hake, haddock, hake
	# length_cm: 41, 33, 38, 27, 36, 30
	# caught:    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE
	
	survey_data <- data.frame(
	  station = c("A", "B", "C", "D", "E", "F"),
	  species = c("cod", "haddock", "cod", "hake", "haddock", "hake"),
	  length_cm = c(41, 33, 38, 27, 36, 30),
	  caught = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
	)
	
	# Inspect the data frame using each function below.
	
	# 1. str()
	
	str(survey_data)
	
	# 2. head()
	
	head(survey_data)
	
	# 3. summary()
	
	summary(survey_data)
	
	#### Part 5: Answer questions with the data frame ####
	
	# 1. What is the mean fish length?
	
	mean(survey_data$length_cm)
	
	# 2. How many observations are cod?
	
	sum(survey_data$species == "cod")
	
	# 3. How many fish were caught?
	
	sum(survey_data$caught == TRUE)

	# 4. Display all observations from fish at least 35 cm long.
	
	survey_data[survey_data$length_cm >= 35, ]
	
	# 5. Display only the observations where a fish was caught.
	
	survey_data[survey_data$caught == TRUE, ]
	
	# 6. What is the mean length of the fish that were caught?
	
	mean(survey_data$length_cm &
	       survey_data$caught == TRUE
	     )
	
	#### Part 6: Debugging ####
	
	# Each example below contains an error.
	# Correct the code, then run the corrected version.
	
	# Error 1
	
	# mean_Length <- 35
	# mean_length
	
	mean_length <- 35
	mean_length
	
	# Error 2
	
	# fish_species <- c("cod", "haddock", hake")
	
	fish_species <- c("cod", "haddock", "hake")
	
	# Error 3

	# round(3.14159 digits = 2)
	
	round(3.14159, digits = 2)

	#### Optional challenge ####
	
	# Create a logical object named large_and_caught that identifies
	# observations that are both:
	#
	# - at least 35 cm long; and
	# - recorded as caught.
	#
	# Then use large_and_caught to display the matching rows
	# of survey_data.
	
	large_and_caught <- survey_data$length_cm >= 35 &
	  survey_data$caught
	
	large_and_caught
	
	survey_data[large_and_caught, ]
	
	#### Reflection ####
	
	# In one or two sentences, describe one R concept or skill
	# that became clearer during this exercise.
	#
	# Response:
	
	# I learned more about displaying different aspects of data frames, 
	# as well as calculating answers I need based on the data frame manipulations.
	
	#### Completion checklist ####
	
	# Before finishing, confirm that:
	#
	# [X] All required code runs without errors.
	# [X] Object names are descriptive and use snake_case.
	# [X] The questions are answered using R code.
	# [X] The completed .R file is saved.
	# [X] The changes are committed and pushed to GitHub.
	#
	# Suggested commit message:
	#
	# Complete R basics exercise
	