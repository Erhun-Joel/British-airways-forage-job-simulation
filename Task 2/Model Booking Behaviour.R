# Loading necessary libraries
library(tidyverse)
library(tidymodels)

# Loading data set
booking.data <-
  read.csv("https://github.com/Erhun-Joel/British-airways-forage-job-simulation/raw/refs/heads/main/Data/customer_booking.csv") %>%
  as_tibble()
booking.data

# Create function to count number of unique discrete variables
discrete.counts <-
function(data){
  
  output.list <- list()                       # Creating output list
  
  cols = colnames(data)                       # Getting out column names for loop
  
  for(i in cols){                             # Implementing for loop
    
    variable.count = data %>% count(get(i))   # Get count for column being ran
    
    vector = variable.count[[2]]              # Get numeric counts in the form of a vector
    
    names(vector) = variable.count[[1]]       # Name vector with variable names
    
    vector = sort(vector)                     # Sort vector
    
    output.list[[i]] = vector                 # Append vector to output.list
    
  }
  
  return(output.list)                         # Return output.list
  
}

# Get counts for categorical variables
discrete.list <-
booking.data %>%
  select(-purchase_lead, -flight_hour, -flight_duration, -length_of_stay, -num_passengers) %>%
  discrete.counts()
discrete.list

# Seems like some columns have too many discrete values
# Some of which includes booking_origin and route
# Lets simply remove them

# Check for NA's
booking.data %>%
  is.na() %>%
  sum()

# Check column types
glimpse(booking.data)

# Create split
set.seed(122)
booking.split <- initial_split(mutate(booking.data, booking_complete = as.factor(booking_complete)), strata = booking_complete)
booking.split

# Get out training data
booking.train <- training(booking.split)
booking.train

# Create folds for testing
set.seed(125)
booking.folds <-
vfold_cv(booking.train, v = 10, strata = booking_complete)
booking.folds

# Create pre-processing recipe steps
basic.recipe <-
recipe(booking_complete ~ ., data = booking.train) %>%
  update_role(route, booking_origin, new_role = "id") %>%
  step_dummy(all_nominal_predictors())
basic.recipe

# Create logistic recipe
log.spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
log.spec

# Train re-samples
log.results <- workflow(
  preprocessor = basic.recipe,
  spec = log.spec
) %>%
  fit_resamples(
    resamples = booking.folds,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
log.results

log.results %>%
  collect_metrics()

# Lets try to improve this metric by two things: variable manipulation and regularization

booking.data %>%
  ggplot(aes(purchase_lead, fill = as.factor(booking_complete))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~booking_complete, nrow = 2) +
  labs(
    x = "Days before Payment",
    y = NULL
  ) +
  theme_minimal()

# Lets see if we cant normalize it
booking.data %>%
  ggplot(aes(log(purchase_lead + 1), fill = as.factor(booking_complete))) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~booking_complete, nrow = 2) +
  labs(
    x = "Days before Payment",
    y = NULL
  ) +
  theme_minimal()
# By taking the log, we normalize it.
# Lets also do the same for length_of_stay

# Creating log transformed variables and offsetting them by 0.5 to avoid infinity values
normalized.recipe <- basic.recipe %>%
  step_log(length_of_stay, purchase_lead, offset = 0.5)
normalized.recipe

# Check for infinity values
normalized.recipe %>%
  prep %>%
  juice %>%
  select(length_of_stay, purchase_lead) %>%
  filter(
    is.infinite(length_of_stay),
    is.infinite(purchase_lead)
  )

# Re-train re-samples
normalized.results <- workflow(
  preprocessor = normalized.recipe,
  spec = log.spec
) %>%
  fit_resamples(
    resamples = booking.folds,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE, save_workflow = TRUE)
  )
normalized.results

# Check out normalized results
normalized.results %>%
  collect_metrics()
# Slight increase observed

# I find it hard to beleive some variables actually affect the payment of a booking order
# i.e., flight duration
booking.data %>%
  ggplot(aes(log(flight_duration), fill = as.factor(booking_complete))) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~booking_complete, scales = "free_y", nrow = 2) +
  labs(
    x = "Duration of Flight",
    y = NULL
  ) +
  theme_minimal()
# Here we see flight duration doesn't really separate booking_complete (FOR NOW!)

# Lets see statistically significant variables from our current model
current.model <-
workflow() %>%
  add_model(log.spec) %>%
  add_recipe(normalized.recipe) %>%
  fit(data = booking.train)
current.model

# Check out p values of columns
current.model %>%
  tidy() %>%
  arrange(p.value) %>%
  filter(p.value > 0.05)
# Here we see that flight_duration actually matters
# Columns with insignificant p values are flight_day, flight_hour and number of passengers
# Lets remove both flight_day and flight_hour for now

# Redefining new recipe
trimmed.recipe <-
recipe(booking_complete ~ ., data = booking.train) %>%
  update_role(route, booking_origin, flight_day, flight_hour, num_passengers, new_role = "id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_log(length_of_stay, purchase_lead, offset = 0.5) %>%
  update_role(trip_type_OneWay, new_role = "id")
trimmed.recipe

# Re-train resamples with new recipe
trimmed.results <- workflow(
  preprocessor = trimmed.recipe,
  spec = log.spec
) %>%
  fit_resamples(
    resamples = booking.folds,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
trimmed.results

# Check out metrics
trimmed.results %>%
  collect_metrics()
# Extremely minor improvements

# Lets see those without statistical significance
workflow() %>%
  add_model(log.spec) %>%
  add_recipe(trimmed.recipe) %>%
  fit(data = booking.train) %>%
  tidy() %>%
  filter(p.value > 0.05)

# Ok, we have tried to improve by simple variable manipulation
# Now lets tune

# Redefine spec for tunning
log.spec.tune <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
log.spec.tune

# Define initial grid
logistic.grid <- grid_regular(penalty(), mixture(), levels = 3)
logistic.grid

# Apply grid to resamples
log.tune.results <-
workflow(
  preprocessor = trimmed.recipe,
  spec = log.spec.tune
) %>%
  tune_grid(
    resamples = booking.folds,
    grid = logistic.grid,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
log.tune.results

# Lets plot to see if a trend for increased performance exists
autoplot(log.tune.results)
# Seems Lasso regularization is undesired.

# Lets perform ridge regression on higher penalty parameters

# Redefine grid
logistic.grid <- grid_regular(penalty(range = c(-1, 2), trans = NULL), levels = 8)
logistic.grid

log.tune.results.again <-
workflow(
  preprocessor = trimmed.recipe,
  spec = log.spec.tune
) %>%
  update_model(
    logistic_reg(engine = "glmnet", penalty = tune(), mixture = 0)
  ) %>%
  tune_grid(
    resamples = booking.folds,
    grid = logistic.grid,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
log.tune.results.again

# Plot results to view trend if available
autoplot(log.tune.results.again)
# Nope, gets worse if any regularisation is used.

# Lets see if a simple tree based model will perform better
rf.spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Re-train resamples on rf.spec
set.seed(123)
rf.results <-
workflow(
  preprocessor = trimmed.recipe,
  spec = rf.spec
) %>%
  fit_resamples(
    resamples = booking.folds,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
rf.results

# Check out results
rf.results %>%
  collect_metrics()
# Does better already.

# Lets now make a simple grid to see if any trend of improved performance can be observed
rf.grid <-
grid_regular(min_n(), finalize(mtry(), booking.train))
rf.grid

# Lets redefine rf.spec
rf.spec <- rand_forest(trees = 1000, min_n = tune(), mtry = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Tune grid on resamples
doParallel::registerDoParallel()
set.seed(114)
rf.tune.results <-
workflow(
  preprocessor = trimmed.recipe,
  spec = rf.spec
) %>%
  tune_grid(
    resamples = booking.folds,
    grid = rf.grid,
    metrics = metric_set(sensitivity, specificity, roc_auc),
    control = control_grid(save_pred = TRUE, verbose = TRUE, parallel_over = "everything")
  )
rf.tune.results

# Check error messages
show_notes(.Last.tune.result)

# Plot out results
autoplot(rf.tune.results)
# Again, it seems leaving it alone is the best strategy

# Lets compute models and produce variable importance skim
log.fit <- workflow(
  preprocessor = trimmed.recipe,
  spec = logistic_reg(engine = "glm")
) %>%
  fit(data = booking.train)
log.fit

# NOTE: Testing and modifying fits probability would not be done because this project is concerned with determining what affects booking completion the most, not prediction accuracy
log.fit %>%
  tidy() %>%
  filter(!str_detect(term, "ntercept")) %>%
  ggplot(aes(estimate, fct_reorder(term, abs(estimate)), fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "Coefficients (Directly Realated to Importance)",
    y = NULL
  ) +
  theme_minimal()
# This shows the magnitude and direction of a variables effect on the log odds of booking complete
# For instance, the log odds and implicationally the probability of completing a booking increases if the customer desires a round trip
# In the other direction, the probability of completing a booking reduces drastically if the booking channel was a mobile phone

# saveRDS(object = log.fit, file = "---")

# Lets see if the random forest gives something similar
set.seed(209)
rf.fit <-
workflow(
  preprocessor = trimmed.recipe,
  spec = rand_forest(trees = 1000) %>%
    set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")
) %>%
  fit(data = booking.train)
rf.fit

# Get out importance magnitudes and visualize
(rf.fit %>%
  extract_fit_parsnip())[["fit"]][["variable.importance"]] %>%
  tidy() %>%
  ggplot(aes(x, fct_reorder(names, x))) +
  geom_col() +
  labs(
    y = NULL,
    x = "Impurity Score"
  ) +
  theme_minimal()
# This shows slightly different terms as most important.
# Varying the importance choice in fits workflow and the seed being set would bring about different importance scores
# Because of this, the logistic model would be preferred for interpretation
# If prediction becomes necessary, the random fit should be used

# saveRDS(object = rf.fit, file = "---")
