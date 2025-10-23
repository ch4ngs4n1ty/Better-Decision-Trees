# Building Faster Decision Trees
# Ethan Chang
# Homework 5
# Compile: Rscript HW_05_Chang_Ethan_main.R


main_folder <- "Florinian_vs_Guilderian_Data_v24.csv" #Initial main folder containicng subfolders with csv files

bisecting_data <- read.csv(main_folder, header=TRUE) #Read in main folder

hem_height <- bisecting_data$HemHt #Height of Hemisphere
bowtie_width <- bisecting_data$BowTieWd #Width of Bowtie
country <- bisecting_data$Country #Country Label

data_all <- data.frame(hem_height, bowtie_width, country) #Combine into one data frame

# option 2 — individually
count_florin  <- sum(country == "Florin")
count_guilder <- sum(country == "Guilder")

cat("Florin:", count_florin, " Guilder:", count_guilder, "\n")
print(nrow(data_all))

cost_function <- function(susbset_country, total_count, alpha) {

    node_size <- length(susbset_country)

    fraction <- node_size / total_count

    majority_count <- table(susbset_country)

    purity <- max(majority_count) / node_size

    measure_of_node = fraction + (purity * alpha)

    return(measure_of_node)

}

total_count <- nrow(data_all)

# Scenario 1: Large chunk with moderate purity (what we might realistically find)
large_moderate <- c(rep("Florin", 70), rep("Guilder", 40))  # 64% pure, 45% of total data
cost_1 <- cost_function(large_moderate, total_count, 0.3)
cat("Large moderate (45% data, 64% pure):", round(cost_1, 3), "\n")


#print(data_all)

test_thresholds_hem <- c(3.5, 4.0, 4.5)
for(threshold in test_thresholds_hem) {
  subset_below <- data_all$country[data_all$hem_height < threshold]
  if(length(subset_below) > 0) {
    cost_below <- cost_function(subset_below, nrow(data_all), 0.2)
    purity_below <- max(table(subset_below)) / length(subset_below)
    cat(sprintf("hem_height < %.1f: %d records, %.1f%% pure, cost = %.3f\n", 
                threshold, length(subset_below), purity_below*100, cost_below))
  }
}

cat("\n")
# Test bowtie_width thresholds around 4  
test_thresholds_bowtie <- c(3.5, 4.0, 4.5)
for(threshold in test_thresholds_bowtie) {
  subset_below <- data_all$country[data_all$bowtie_width < threshold]
  if(length(subset_below) > 0) {
    cost_below <- cost_function(subset_below, nrow(data_all), 0.2)
    purity_below <- max(table(subset_below)) / length(subset_below)
    cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% pure, cost = %.3f\n", 
                threshold, length(subset_below), purity_below*100, cost_below))
  }
}

cat("\n=== ANALYZING hem_height < 4.0 SUBSET ===\n")

best_subset <- data_all$country[data_all$hem_height < 4.0]
class_breakdown <- table(best_subset)
print(class_breakdown)

majority_class <- names(which.max(class_breakdown))
majority_percent <- max(class_breakdown) / length(best_subset) * 100

cat(sprintf("Majority class: %s (%.1f%% of this subset)\n", majority_class, majority_percent))
cat(sprintf("This subset contains %.1f%% of total data\n", length(best_subset)/nrow(data_all)*100))


# === DOUBLE-CHECKING OUR APPROACH ===
cat("\n=== VERIFYING CASCADE STRATEGY ===\n")

# Let's check if we should consider the OTHER side of the threshold
subset_above <- data_all$country[data_all$hem_height >= 4.0]
cost_above <- cost_function(subset_above, nrow(data_all), 0.2)
purity_above <- max(table(subset_above)) / length(subset_above)

cat("Alternative: hem_height >= 4.0\n")
cat("Records:", length(subset_above), "(", round(length(subset_above)/nrow(data_all)*100, 1), "% of data)\n")
cat("Purity:", round(purity_above * 100, 1), "%\n") 
cat("Cost:", round(cost_above, 3), "\n")
cat("Majority class:", names(which.max(table(subset_above))), "\n")

# Compare both options
cat("\nCOMPARISON:\n")
cat("hem_height < 4.0:   cost = 0.317, 44 records, 68.2% Guilder\n")
cat("hem_height >= 4.0:  cost =", round(cost_above, 3), length(subset_above), "records,", 
    round(purity_above * 100, 1), "%", names(which.max(table(subset_above))), "\n")

# === REVISED CASCADE DECISION #1 ===
cat("\n=== REVISED CASCADE DECISION #1 ===\n")
cat("IF hem_height >= 4.0 THEN classify as Florin\n")
cat("This decision handles", length(subset_above), "records (82.0% of data)\n")

florin_correct <- sum(subset_above == "Florin")
total_above <- length(subset_above)
cat("Accuracy for this decision:", round(florin_correct/total_above * 100, 1), "%\n")

# Create remaining data for next cascade decisions  
remaining_data <- data_all[data_all$hem_height < 4.0, ]
cat("\nRemaining data to process:", nrow(remaining_data), "records\n")
cat("Remaining Florin:", sum(remaining_data$country == "Florin"), "\n") 
cat("Remaining Guilder:", sum(remaining_data$country == "Guilder"), "\n")

# === FINDING CASCADE DECISION #2 ===
cat("\n=== SEARCHING FOR DECISION #2 ===\n")
cat("Working on remaining 44 records:\n")
cat("  Florin:", sum(remaining_data$country == "Florin"), "\n")
cat("  Guilder:", sum(remaining_data$country == "Guilder"), "\n")

# We'll search for the best threshold on the remaining data
# Let's test both features on the remaining 44 records

current_data_size <- nrow(remaining_data)

# Test hem_height thresholds on remaining data
cat("\nTesting hem_height thresholds on remaining data:\n")
test_thresholds_hem <- seq(2.5, 4.0, by = 0.5)
for(threshold in test_thresholds_hem) {
  subset_below <- remaining_data$country[remaining_data$hem_height < threshold]
  if(length(subset_below) > 0) {
    cost_below <- cost_function(subset_below, current_data_size, 0.2)
    purity_below <- max(table(subset_below)) / length(subset_below)
    majority_class <- names(which.max(table(subset_below)))
    cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                threshold, length(subset_below), purity_below*100, majority_class, cost_below))
  }
}

# Test bowtie_width thresholds on remaining data
cat("\nTesting bowtie_width thresholds on remaining data:\n")
test_thresholds_bowtie <- seq(4.0, 8.0, by = 1.0)
for(threshold in test_thresholds_bowtie) {
  subset_below <- remaining_data$country[remaining_data$bowtie_width < threshold]
  if(length(subset_below) > 0) {
    cost_below <- cost_function(subset_below, current_data_size, 0.2)
    purity_below <- max(table(subset_below)) / length(subset_below)
    majority_class <- names(which.max(table(subset_below)))
    cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                threshold, length(subset_below), purity_below*100, majority_class, cost_below))
  }
}

# === IMPLEMENTING CASCADE DECISION #2 ===
cat("\n=== CASCADE DECISION #2 ===\n")
cat("IF bowtie_width < 8.0 THEN classify as Guilder\n")

decision2_subset <- remaining_data$country[remaining_data$bowtie_width < 8.0]
cat("This decision handles", length(decision2_subset), "records\n")
cat("Purity: 100.0% Guilder\n")

# Create remaining data for next cascade decision
remaining_data_2 <- remaining_data[remaining_data$bowtie_width >= 8.0, ]
cat("\nRemaining data after Decision #2:", nrow(remaining_data_2), "records\n")
cat("Remaining Florin:", sum(remaining_data_2$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data_2$country == "Guilder"), "\n")

# Calculate overall accuracy so far
cat("\n=== OVERALL ACCURACY CHECK ===\n")
# Decision #1 correct: Florin classified correctly in hem_height >= 4.0
decision1_correct <- sum(data_all$country[data_all$hem_height >= 4.0] == "Florin")

# Decision #2 correct: All 21 records are correctly Guilder
decision2_correct <- length(decision2_subset)

total_handled <- length(subset_above) + length(decision2_subset)
total_correct <- decision1_correct + decision2_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")


# === REVISED STRATEGY: FIND HIGHER PURITY FIRST DECISION ===
cat("\n=== SEARCHING FOR HIGHER PURITY FIRST DECISION ===\n")

# Let's search for thresholds that give us at least 80% purity
cat("Searching for thresholds with >= 80% purity:\n")

# Test hem_height thresholds
test_thresholds <- seq(2, 10, by = 0.5)
for(threshold in test_thresholds) {
  subset_below <- data_all$country[data_all$hem_height < threshold]
  subset_above <- data_all$country[data_all$hem_height >= threshold]
  
  # Check both sides for high purity
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, nrow(data_all), 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
  
  if(length(subset_above) > 0) {
    purity_above <- max(table(subset_above)) / length(subset_above)
    if(purity_above >= 0.8) {
      cost_above <- cost_function(subset_above, nrow(data_all), 0.2)
      majority_class <- names(which.max(table(subset_above)))
      cat(sprintf("hem_height >= %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_above), purity_above*100, majority_class, cost_above))
    }
  }
}

# Also test bowtie_width for high purity splits
cat("\nTesting bowtie_width for high purity:\n")
for(threshold in seq(2, 12, by = 0.5)) {
  subset_below <- data_all$country[data_all$bowtie_width < threshold]
  subset_above <- data_all$country[data_all$bowtie_width >= threshold]
  
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, nrow(data_all), 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
}

# === NEW CASCADE DECISION #1 (HIGH PURITY) ===
cat("\n=== CASCADE DECISION #1 ===\n")
cat("IF bowtie_width < 5.5 THEN classify as Guilder\n")

decision1_subset <- data_all$country[data_all$bowtie_width < 5.5]
cat("This decision handles", length(decision1_subset), "records (", 
    round(length(decision1_subset)/nrow(data_all)*100, 1), "% of data)\n")
cat("Purity: 100.0% Guilder\n")
cat("Accuracy: 100.0%\n")

# Create remaining data for Decision #2
remaining_data <- data_all[data_all$bowtie_width >= 5.5, ]
cat("\nRemaining data after Decision #1:", nrow(remaining_data), "records\n")
cat("Remaining Florin:", sum(remaining_data$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data$country == "Guilder"), "\n")

# Calculate overall accuracy so far
cat("\n=== OVERALL ACCURACY CHECK ===\n")
decision1_correct <- length(decision1_subset)  # All 36 are correct (100% Guilder)
total_handled <- length(decision1_subset)
total_correct <- decision1_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")

# === FINDING CASCADE DECISION #2 ===
cat("\n=== SEARCHING FOR DECISION #2 ===\n")
cat("Working on remaining 208 records:\n")
cat("  Florin:", sum(remaining_data$country == "Florin"), "\n")
cat("  Guilder:", sum(remaining_data$country == "Guilder"), "\n")

current_data_size <- nrow(remaining_data)

# Search for high-purity splits in the remaining data
cat("\nSearching for high-purity splits (>= 80% purity):\n")

# Test hem_height thresholds on remaining data
test_thresholds_hem <- seq(4, 12, by = 0.5)
for(threshold in test_thresholds_hem) {
  subset_below <- remaining_data$country[remaining_data$hem_height < threshold]
  subset_above <- remaining_data$country[remaining_data$hem_height >= threshold]
  
  # Check both sides for high purity
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
  
  if(length(subset_above) > 0) {
    purity_above <- max(table(subset_above)) / length(subset_above)
    if(purity_above >= 0.8) {
      cost_above <- cost_function(subset_above, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_above)))
      cat(sprintf("hem_height >= %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_above), purity_above*100, majority_class, cost_above))
    }
  }
}

# Also test bowtie_width on remaining data
cat("\nTesting bowtie_width on remaining data:\n")
for(threshold in seq(6, 12, by = 0.5)) {
  subset_below <- remaining_data$country[remaining_data$bowtie_width < threshold]
  subset_above <- remaining_data$country[remaining_data$bowtie_width >= threshold]
  
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
}

# === CASCADE DECISION #2 ===
cat("\n=== CASCADE DECISION #2 ===\n")
cat("IF hem_height >= 8.5 THEN classify as Florin\n")

decision2_subset <- remaining_data$country[remaining_data$hem_height >= 8.5]
cat("This decision handles", length(decision2_subset), "records\n")
cat("Purity:", round(max(table(decision2_subset)) / length(decision2_subset) * 100, 1), "% Florin\n")

# Calculate accuracy for this decision
florin_correct <- sum(decision2_subset == "Florin")
cat("Accuracy for this decision:", round(florin_correct/length(decision2_subset) * 100, 1), "%\n")

# Create remaining data for Decision #3
remaining_data_2 <- remaining_data[remaining_data$hem_height < 8.5, ]
cat("\nRemaining data after Decision #2:", nrow(remaining_data_2), "records\n")
cat("Remaining Florin:", sum(remaining_data_2$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data_2$country == "Guilder"), "\n")

# Calculate overall accuracy so far
cat("\n=== OVERALL ACCURACY CHECK ===\n")
decision1_correct <- 36  # All 36 from Decision #1 were correct (100% Guilder)
decision2_correct <- florin_correct  # Florin correctly classified in Decision #2

total_handled <- length(decision1_subset) + length(decision2_subset)
total_correct <- decision1_correct + decision2_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")
cat("Overall progress:", round(total_handled/nrow(data_all) * 100, 1), "% of data classified\n")

# === FINDING CASCADE DECISION #3 ===
cat("\n=== SEARCHING FOR DECISION #3 ===\n")
cat("Working on remaining 140 records:\n")
cat("  Florin:", sum(remaining_data_2$country == "Florin"), "\n")
cat("  Guilder:", sum(remaining_data_2$country == "Guilder"), "\n")

current_data_size <- nrow(remaining_data_2)

# Search for high-purity splits in the remaining data
cat("\nSearching for high-purity splits (>= 80% purity):\n")

# Test hem_height thresholds on remaining data
test_thresholds_hem <- seq(4, 8.5, by = 0.5)
for(threshold in test_thresholds_hem) {
  subset_below <- remaining_data_2$country[remaining_data_2$hem_height < threshold]
  subset_above <- remaining_data_2$country[remaining_data_2$hem_height >= threshold]
  
  # Check both sides for high purity
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
  
  if(length(subset_above) > 0) {
    purity_above <- max(table(subset_above)) / length(subset_above)
    if(purity_above >= 0.8) {
      cost_above <- cost_function(subset_above, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_above)))
      cat(sprintf("hem_height >= %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_above), purity_above*100, majority_class, cost_above))
    }
  }
}

# Test bowtie_width on remaining data
cat("\nTesting bowtie_width on remaining data:\n")
for(threshold in seq(5.5, 12, by = 0.5)) {
  subset_below <- remaining_data_2$country[remaining_data_2$bowtie_width < threshold]
  subset_above <- remaining_data_2$country[remaining_data_2$bowtie_width >= threshold]
  
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
}

# === CASCADE DECISION #3 ===
cat("\n=== CASCADE DECISION #3 ===\n")
cat("IF bowtie_width < 9.0 THEN classify as Guilder\n")

decision3_subset <- remaining_data_2$country[remaining_data_2$bowtie_width < 9.0]
cat("This decision handles", length(decision3_subset), "records\n")
cat("Purity:", round(max(table(decision3_subset)) / length(decision3_subset) * 100, 1), "% Guilder\n")

# Calculate accuracy for this decision
guilder_correct <- sum(decision3_subset == "Guilder")
cat("Accuracy for this decision:", round(guilder_correct/length(decision3_subset) * 100, 1), "%\n")

# Create remaining data for Decision #4
remaining_data_3 <- remaining_data_2[remaining_data_2$bowtie_width >= 9.0, ]
cat("\nRemaining data after Decision #3:", nrow(remaining_data_3), "records\n")
cat("Remaining Florin:", sum(remaining_data_3$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data_3$country == "Guilder"), "\n")

# Calculate overall accuracy so far
cat("\n=== OVERALL ACCURACY CHECK ===\n")
decision1_correct <- 36  # All 36 from Decision #1 were correct (100% Guilder)
decision2_correct <- 59  # 86.8% of 68 records from Decision #2 (59 Florin correct)
decision3_correct <- guilder_correct  # Guilder correctly classified in Decision #3

total_handled <- length(decision1_subset) + length(decision2_subset) + length(decision3_subset)
total_correct <- decision1_correct + decision2_correct + decision3_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")
cat("Overall progress:", round(total_handled/nrow(data_all) * 100, 1), "% of data classified\n")

# === FINDING CASCADE DECISION #4 ===
cat("\n=== SEARCHING FOR DECISION #4 ===\n")
cat("Working on remaining 87 records:\n")
cat("  Florin:", sum(remaining_data_3$country == "Florin"), "\n")
cat("  Guilder:", sum(remaining_data_3$country == "Guilder"), "\n")

current_data_size <- nrow(remaining_data_3)

# Search for high-purity splits in the remaining data
cat("\nSearching for high-purity splits (>= 80% purity):\n")

# Test hem_height thresholds on remaining data
test_thresholds_hem <- seq(4, 8.5, by = 0.5)
for(threshold in test_thresholds_hem) {
  subset_below <- remaining_data_3$country[remaining_data_3$hem_height < threshold]
  subset_above <- remaining_data_3$country[remaining_data_3$hem_height >= threshold]
  
  # Check both sides for high purity
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
  
  if(length(subset_above) > 0) {
    purity_above <- max(table(subset_above)) / length(subset_above)
    if(purity_above >= 0.8) {
      cost_above <- cost_function(subset_above, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_above)))
      cat(sprintf("hem_height >= %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_above), purity_above*100, majority_class, cost_above))
    }
  }
}

# Test bowtie_width on remaining data
cat("\nTesting bowtie_width on remaining data:\n")
for(threshold in seq(9, 12, by = 0.5)) {
  subset_below <- remaining_data_3$country[remaining_data_3$bowtie_width < threshold]
  subset_above <- remaining_data_3$country[remaining_data_3$bowtie_width >= threshold]
  
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
}

# === CASCADE DECISION #4 ===
cat("\n=== CASCADE DECISION #4 ===\n")
cat("IF hem_height >= 5.0 THEN classify as Florin\n")

decision4_subset <- remaining_data_3$country[remaining_data_3$hem_height >= 5.0]
cat("This decision handles", length(decision4_subset), "records\n")
cat("Purity:", round(max(table(decision4_subset)) / length(decision4_subset) * 100, 1), "% Florin\n")

# Calculate accuracy for this decision
florin_correct_4 <- sum(decision4_subset == "Florin")
cat("Accuracy for this decision:", round(florin_correct_4/length(decision4_subset) * 100, 1), "%\n")

# Create remaining data (if needed)
remaining_data_4 <- remaining_data_3[remaining_data_3$hem_height < 5.0, ]
cat("\nRemaining data after Decision #4:", nrow(remaining_data_4), "records\n")
cat("Remaining Florin:", sum(remaining_data_4$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data_4$country == "Guilder"), "\n")

# Calculate overall accuracy
cat("\n=== OVERALL ACCURACY CHECK ===\n")
decision1_correct <- 36  # All 36 from Decision #1 (100% Guilder)
decision2_correct <- 59  # 86.8% of 68 records from Decision #2 (59 Florin correct)
decision3_correct <- 51  # 96.2% of 53 records from Decision #3 (51 Guilder correct)
decision4_correct <- florin_correct_4  # Florin correctly classified in Decision #4

total_handled <- length(decision1_subset) + length(decision2_subset) + length(decision3_subset) + length(decision4_subset)
total_correct <- decision1_correct + decision2_correct + decision3_correct + decision4_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")
cat("Overall progress:", round(total_handled/nrow(data_all) * 100, 1), "% of data classified\n")

# Check if we reached 95% target
if(total_correct/total_handled >= 0.95) {
  cat("\n🎉 SUCCESS! We reached the 95% accuracy target! 🎉\n")
} else {
  cat("\nWe need more decisions to reach 95% accuracy\n")
}

# === FINDING CASCADE DECISION #5 ===
cat("\n=== SEARCHING FOR DECISION #5 ===\n")
cat("Working on remaining 41 records:\n")
cat("  Florin:", sum(remaining_data_4$country == "Florin"), "\n")
cat("  Guilder:", sum(remaining_data_4$country == "Guilder"), "\n")

current_data_size <- nrow(remaining_data_4)

# Search for high-purity splits in the remaining data
cat("\nSearching for high-purity splits (>= 80% purity):\n")

# Test hem_height thresholds on remaining data
test_thresholds_hem <- seq(2, 5, by = 0.5)
for(threshold in test_thresholds_hem) {
  subset_below <- remaining_data_4$country[remaining_data_4$hem_height < threshold]
  subset_above <- remaining_data_4$country[remaining_data_4$hem_height >= threshold]
  
  # Check both sides for high purity
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("hem_height < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
  
  if(length(subset_above) > 0) {
    purity_above <- max(table(subset_above)) / length(subset_above)
    if(purity_above >= 0.8) {
      cost_above <- cost_function(subset_above, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_above)))
      cat(sprintf("hem_height >= %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_above), purity_above*100, majority_class, cost_above))
    }
  }
}

# Test bowtie_width on remaining data
cat("\nTesting bowtie_width on remaining data:\n")
for(threshold in seq(5, 12, by = 0.5)) {
  subset_below <- remaining_data_4$country[remaining_data_4$bowtie_width < threshold]
  subset_above <- remaining_data_4$country[remaining_data_4$bowtie_width >= threshold]
  
  if(length(subset_below) > 0) {
    purity_below <- max(table(subset_below)) / length(subset_below)
    if(purity_below >= 0.8) {
      cost_below <- cost_function(subset_below, current_data_size, 0.2)
      majority_class <- names(which.max(table(subset_below)))
      cat(sprintf("bowtie_width < %.1f: %d records, %.1f%% %s, cost = %.3f\n", 
                  threshold, length(subset_below), purity_below*100, majority_class, cost_below))
    }
  }
}

# === CASCADE DECISION #5 ===
cat("\n=== CASCADE DECISION #5 ===\n")
cat("IF bowtie_width < 12.0 THEN classify as Guilder\n")

decision5_subset <- remaining_data_4$country[remaining_data_4$bowtie_width < 12.0]
cat("This decision handles", length(decision5_subset), "records\n")
cat("Purity:", round(max(table(decision5_subset)) / length(decision5_subset) * 100, 1), "% Guilder\n")

# Calculate accuracy for this decision
guilder_correct_5 <- sum(decision5_subset == "Guilder")
cat("Accuracy for this decision:", round(guilder_correct_5/length(decision5_subset) * 100, 1), "%\n")

# Create remaining data (if needed)
remaining_data_5 <- remaining_data_4[remaining_data_4$bowtie_width >= 12.0, ]
cat("\nRemaining data after Decision #5:", nrow(remaining_data_5), "records\n")
cat("Remaining Florin:", sum(remaining_data_5$country == "Florin"), "\n")
cat("Remaining Guilder:", sum(remaining_data_5$country == "Guilder"), "\n")

# Calculate overall accuracy
cat("\n=== OVERALL ACCURACY CHECK ===\n")
decision1_correct <- 36  # All 36 from Decision #1 (100% Guilder)
decision2_correct <- 59  # 86.8% of 68 records from Decision #2 (59 Florin correct)
decision3_correct <- 51  # 96.2% of 53 records from Decision #3 (51 Guilder correct)
decision4_correct <- 37  # 80.4% of 46 records from Decision #4 (37 Florin correct)
decision5_correct <- guilder_correct_5  # Guilder correctly classified in Decision #5

total_handled <- length(decision1_subset) + length(decision2_subset) + length(decision3_subset) + length(decision4_subset) + length(decision5_subset)
total_correct <- decision1_correct + decision2_correct + decision3_correct + decision4_correct + decision5_correct

cat("Records handled so far:", total_handled, "/", nrow(data_all), "\n")
cat("Correct classifications so far:", total_correct, "\n")
cat("Current accuracy:", round(total_correct/total_handled * 100, 1), "%\n")
cat("Overall progress:", round(total_handled/nrow(data_all) * 100, 1), "% of data classified\n")

# Check if we reached 95% target
if(total_correct/total_handled >= 0.95) {
  cat("\n🎉 SUCCESS! We reached the 95% accuracy target! 🎉\n")
} else {
  cat("\nWe need more decisions to reach 95% accuracy\n")
  # Calculate how many more correct classifications we need
  needed_correct <- ceiling(0.95 * total_handled) - total_correct
  cat("We need", needed_correct, "more correct classifications\n")
}