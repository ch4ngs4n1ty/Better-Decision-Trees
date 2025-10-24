# Building Faster Decision Trees
# Ethan Chang
# Homework 5
# Compile: Rscript HW_05_Chang_Ethan_main.R


main_folder <- "Florinian_vs_Guilderian_Data_v24.csv" # Initial main folder containicng subfolders with csv files

bisecting_data <- read.csv(main_folder, header = TRUE) # Read in main folder

hem_height <- bisecting_data$HemHt # Height of Hemisphere
bowtie_width <- bisecting_data$BowTieWd # Width of Bowtie
country <- bisecting_data$Country # Country Label

data_all <- data.frame(hem_height, bowtie_width, country) # Combine into one data frame

count_florin <- sum(country == "Florin")
count_guilder <- sum(country == "Guilder")

cat("Florin:", count_florin, " Guilder:", count_guilder, "\n")
print(nrow(data_all))

# === IMPROVED COST FUNCTION ===
# We need to prioritize both size AND purity more equally
# Let's try a different approach that rewards high purity more

# === FINAL COST FUNCTION FIX ===
# We need to strongly penalize low purity decisions
cost_function <- function(subset_country, total_count, alpha = 2.0) {
  node_size <- length(subset_country)
  
  # Handle empty subsets to avoid warnings
  if (node_size == 0) {
    return(-1)  # Very low cost for empty subsets
  }
  
  fraction <- node_size / total_count
  
  majority_count <- table(subset_country)
  purity <- max(majority_count) / node_size
  
  # STRONG penalty for low purity: use purity^2 or higher power
  # This makes low purity decisions have MUCH lower cost
  measure_of_node <- fraction * (purity ^ 3)  # purity cubed strongly penalizes impure decisions
  
  return(measure_of_node)
}

# Let's test this with the problematic decision
cat("=== TESTING FINAL COST FUNCTION ===\n")

# Test the bad decision
bad_subset <- data_all$country[data_all$bowtie_width >= 4.14]
bad_cost <- cost_function(bad_subset, nrow(data_all))
cat("Cost for bowtie_width >= 4.14 (50.6% pure):", round(bad_cost, 3), "\n")

# Test a good decision (100% pure)
good_subset <- data_all$country[data_all$bowtie_width < 5.5]
good_cost <- cost_function(good_subset, nrow(data_all))
cat("Cost for bowtie_width < 5.5 (100% pure):", round(good_cost, 3), "\n")

# Test a medium decision (80% pure)
medium_subset <- data_all$country[data_all$hem_height >= 8.5]
medium_cost <- cost_function(medium_subset, nrow(data_all))
cat("Cost for hem_height >= 8.5 (86.8% pure):", round(medium_cost, 3), "\n")
# Let's test the improved cost function
cat("=== TESTING IMPROVED COST FUNCTION ===\n")

# Test with our problematic first decision
test_subset <- data_all$country[data_all$bowtie_width >= 4.14]
old_cost <- length(test_subset)/nrow(data_all) + (max(table(test_subset))/length(test_subset) * 0.2)
new_cost <- cost_function(test_subset, nrow(data_all), 0.5)

cat("Old cost for bowtie_width >= 4.14:", round(old_cost, 3), "\n")
cat("New cost for bowtie_width >= 4.14:", round(new_cost, 3), "\n")

# Test with a better decision (like the 100% pure ones we found earlier)
good_subset <- data_all$country[data_all$bowtie_width < 5.5]
good_cost <- cost_function(good_subset, nrow(data_all), 0.5)
cat("Cost for bowtie_width < 5.5 (100% pure):", round(good_cost, 3), "\n")

total_count <- nrow(data_all)

# This will be our main recursive function
build_cascade_recursive <- function(current_data, all_original_data, decisions = list(), depth = 1) {
  # Base case: if we've achieved 95% accuracy on all data, stop
  current_accuracy <- calculate_overall_accuracy(all_original_data, decisions)
  if (current_accuracy >= 0.95) {
    cat("🎉 Target reached! Overall accuracy:", round(current_accuracy * 100, 2), "%\n")
    return(decisions)
  }

  # Base case: if no data left to process
  if (nrow(current_data) == 0) {
    cat("No more data to process\n")
    return(decisions)
  }

  cat("\n=== RECURSION DEPTH", depth, "===\n")
  cat("Current overall accuracy:", round(current_accuracy * 100, 2), "%\n")
  cat("Remaining data to classify:", nrow(current_data), "records\n")

  # Find the best threshold decision for the current data
  best_decision <- find_best_threshold_decision(current_data)

  if (is.null(best_decision)) {
    cat("No good decision found - stopping\n")
    return(decisions)
  }

  cat("Best decision found:", best_decision$rule, "\n")
  cat("This handles", best_decision$records_handled, "records\n")
  cat("Purity:", round(best_decision$purity * 100, 1), "%", best_decision$predicted_class, "\n")

  # Add this decision to our cascade
  decisions[[depth]] <- best_decision

  # Get the data that this decision DOESN'T handle (continue processing)
  remaining_data <- get_remaining_data(current_data, best_decision)

  # Recursive call
  decisions <- build_cascade_recursive(remaining_data, all_original_data, decisions, depth + 1)

  return(decisions)
}

# Helper function to create a decision object
create_decision <- function(feature, threshold, direction, current_data) {
  # Extract the relevant feature values and labels
  if (feature == "hem_height") {
    values <- current_data$hem_height
  } else {
    values <- current_data$bowtie_width
  }
  labels <- current_data$country
  
  # Get the subset based on the decision
  if (direction == "less") {
    subset_indices <- values < threshold
  } else {
    subset_indices <- values >= threshold
  }
  
  subset_labels <- labels[subset_indices]
  
  # Skip if subset is empty or too small
  if (length(subset_labels) == 0 || length(subset_labels) < 5) {
    return(NULL)
  }
  
  # Calculate cost and other metrics
  current_size <- nrow(current_data)
  cost <- cost_function(subset_labels, current_size)
  
  # Determine majority class and purity
  class_counts <- table(subset_labels)
  majority_class <- names(which.max(class_counts))
  purity <- max(class_counts) / length(subset_labels)
  
  # === KEY FIX: For rejection cascade, we want to REJECT Guilder ===
  # So we should look for subsets that are Guilder-heavy to remove first
  guild_proportion <- ifelse("Guilder" %in% names(class_counts), 
                            class_counts["Guilder"] / length(subset_labels), 0)
  
  # If this subset is Guilder-heavy, it's good for rejection cascade
  if (guild_proportion > 0.6) {
    predicted_class <- "Guilder"  # We're rejecting these as Guilder
  } else {
    predicted_class <- "Florin"   # We're accepting these as Florin
  }
  
  # Create rule string
  if (direction == "less") {
    rule <- paste(feature, "<", round(threshold, 2))
  } else {
    rule <- paste(feature, ">=", round(threshold, 2))
  }
  
  # Return decision object
  return(list(
    feature = feature,
    threshold = threshold,
    direction = direction,
    rule = rule,
    predicted_class = predicted_class,
    records_handled = length(subset_labels),
    purity = purity,
    cost = cost,
    actual_majority = majority_class  # For debugging
  ))
}

# Function to get remaining data after applying a decision
get_remaining_data <- function(current_data, decision) {

  if (decision$direction == "less") {

    # If decision was "feature < threshold", keep data where "feature >= threshold"
    if (decision$feature == "hem_height") {

      remaining <- current_data[current_data$hem_height >= decision$threshold, ]

    } else {

      remaining <- current_data[current_data$bowtie_width >= decision$threshold, ]
    }
  } else {

    # If decision was "feature >= threshold", keep data where "feature < threshold"
    if (decision$feature == "hem_height") {

      remaining <- current_data[current_data$hem_height < decision$threshold, ]

    } else {
      remaining <- current_data[current_data$bowtie_width < decision$threshold, ]
    }
  }
  return(remaining)
}

# Function to find the best threshold decision for current data
# Let's add debugging to see what's happening in find_best_threshold_decision
find_best_threshold_decision <- function(current_data) {
  best_cost <- -1
  best_decision <- NULL
  
  cat("Searching for best threshold...\n")
  
  # Test thresholds for hem_height
  unique_heights <- sort(unique(current_data$hem_height))
  for (i in 1:(length(unique_heights) - 1)) {
    threshold <- (unique_heights[i] + unique_heights[i + 1]) / 2
    
    # Test "hem_height < threshold"
    test_decision_less <- create_decision("hem_height", threshold, "less", current_data)
    if (!is.null(test_decision_less)) {
      cat(sprintf("  hem_height < %.2f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, test_decision_less$records_handled, 
                  test_decision_less$purity * 100, test_decision_less$predicted_class,
                  test_decision_less$cost))
      if (test_decision_less$cost > best_cost) {
        best_cost <- test_decision_less$cost
        best_decision <- test_decision_less
      }
    }
    
    # Test "hem_height >= threshold"  
    test_decision_geq <- create_decision("hem_height", threshold, "greater_equal", current_data)
    if (!is.null(test_decision_geq)) {
      cat(sprintf("  hem_height >= %.2f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, test_decision_geq$records_handled, 
                  test_decision_geq$purity * 100, test_decision_geq$predicted_class,
                  test_decision_geq$cost))
      if (test_decision_geq$cost > best_cost) {
        best_cost <- test_decision_geq$cost
        best_decision <- test_decision_geq
      }
    }
  }
  
  # Test thresholds for bowtie_width (similar logic)
  unique_widths <- sort(unique(current_data$bowtie_width))
  for (i in 1:(length(unique_widths) - 1)) {
    threshold <- (unique_widths[i] + unique_widths[i + 1]) / 2
    
    # Test "bowtie_width < threshold"
    test_decision_less <- create_decision("bowtie_width", threshold, "less", current_data)
    if (!is.null(test_decision_less)) {
      cat(sprintf("  bowtie_width < %.2f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, test_decision_less$records_handled, 
                  test_decision_less$purity * 100, test_decision_less$predicted_class,
                  test_decision_less$cost))
      if (test_decision_less$cost > best_cost) {
        best_cost <- test_decision_less$cost
        best_decision <- test_decision_less
      }
    }
    
    # Test "bowtie_width >= threshold"
    test_decision_geq <- create_decision("bowtie_width", threshold, "greater_equal", current_data)
    if (!is.null(test_decision_geq)) {
      cat(sprintf("  bowtie_width >= %.2f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, test_decision_geq$records_handled, 
                  test_decision_geq$purity * 100, test_decision_geq$predicted_class,
                  test_decision_geq$cost))
      if (test_decision_geq$cost > best_cost) {
        best_cost <- test_decision_geq$cost
        best_decision <- test_decision_geq
      }
    }
  }
  
  cat("Selected decision:", best_decision$rule, "with cost", best_decision$cost, "\n")
  return(best_decision)
}

# Function to calculate overall accuracy of our cascade on the entire dataset
calculate_overall_accuracy <- function(all_data, decisions) {
  # If no decisions made yet, accuracy is 0
  if (length(decisions) == 0) {
    return(0)
  }
  
  # Apply all decisions to the entire dataset and count correct classifications
  correct_count <- 0
  total_count <- nrow(all_data)
  
  # For each record in the entire dataset, see if our cascade classifies it correctly
  for (i in 1:total_count) {
    record <- all_data[i, ]
    predicted_class <- classify_with_cascade(record, decisions)
    actual_class <- record$country
    
    if (predicted_class == actual_class) {
      correct_count <- correct_count + 1
    }
  }
  
  accuracy <- correct_count / total_count
  return(accuracy)
}

# Helper function to classify a single record using our cascade
classify_with_cascade <- function(record, decisions) {
  # Go through each decision in order
  for (decision in decisions) {
    # Check if this record matches the decision condition
    if (evaluate_decision(record, decision)) {
      return(decision$predicted_class)
    }
  }
  
  # If no decision matches, return a default (we'll improve this later)
  return("Florin")  # Temporary default
}

# Helper function to evaluate a single decision on a record
evaluate_decision <- function(record, decision) {
  feature <- decision$feature
  threshold <- decision$threshold
  direction <- decision$direction
  
  if (feature == "hem_height") {
    value <- record$hem_height
  } else {
    value <- record$bowtie_width
  }
  
  if (direction == "less") {
    return(value < threshold)
  } else {
    return(value >= threshold)
  }
}

tree <- build_cascade_recursive(data_all, data_all)
print(tree)
