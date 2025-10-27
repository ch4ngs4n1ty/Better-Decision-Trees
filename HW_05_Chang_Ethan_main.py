# CSCI 420 HW 5: Decision Cascade Classifier (Florin vs Guilder)
# Ethan Chang
# 

import csv
import matplotlib.pyplot as plt

def plot_data_and_decisions(features, labels, cascade_cascade_code):
    """
    Plot the data points and decision boundaries from the cascade
    """
    # Create figure
    plt.figure(figsize=(12, 8))
    
    # Separate features by class for plotting
    florin_hem = []
    florin_bow = []
    guilder_hem = []
    guilder_bow = []
    
    for i in range(len(features)):
        if labels[i] == "Florin":
            florin_hem.append(features[i][0])
            florin_bow.append(features[i][1])
        else:
            guilder_hem.append(features[i][0])
            guilder_bow.append(features[i][1])

    # Plot data points
    plt.scatter(florin_hem, florin_bow, c='blue', label='Florin', alpha=0.7, s=50)
    plt.scatter(guilder_hem, guilder_bow, c='red', label='Guilder', alpha=0.7, s=50)
    
    # Extract decision boundaries from cascade cascade_code
    lines = cascade_cascade_code.split('\n')
    decisions = []
    
    for line in lines:
        if 'if' in line and '<' in line:
            # Parse decision boundaries - handle colons and extra characters
            if 'HemHt' in line:
                parts = line.split('<')
                # Clean up the threshold string - remove colon and any other non-numeric endings
                threshold_str = parts[1].strip().split(':')[0].split()[0]  # Take only the number part
                try:
                    threshold = float(threshold_str)
                    decisions.append(('HemHt', threshold))
                except ValueError:
                    continue  # Skip if we can't convert to float
                    
            elif 'BowTieWd' in line:
                parts = line.split('<')
                # Clean up the threshold string - remove colon and any other non-numeric endings
                threshold_str = parts[1].strip().split(':')[0].split()[0]  # Take only the number part
                try:
                    threshold = float(threshold_str)
                    decisions.append(('BowTieWd', threshold))
                except ValueError:
                    continue  # Skip if we can't convert to float
    
    print(f"Found {len(decisions)} decision boundaries")
    
    # Plot decision boundaries (just the first few to avoid clutter)
    hem_limits = [min([f[0] for f in features]), max([f[0] for f in features])]
    bow_limits = [min([f[1] for f in features]), max([f[1] for f in features])]
    
    # Plot first 5 vertical boundaries (HemHt decisions)
    hem_decisions = [d for d in decisions if d[0] == 'HemHt'][:5]
    for i, (feature, threshold) in enumerate(hem_decisions):
        plt.axvline(x=threshold, color='green', linestyle='--', 
                   alpha=0.7, label=f'HemHt < {threshold:.2f}' if i == 0 else "")
        print(f"HemHt boundary at: {threshold}")
    
    # Plot first 5 horizontal boundaries (BowTieWd decisions)  
    bow_decisions = [d for d in decisions if d[0] == 'BowTieWd'][:5]
    for i, (feature, threshold) in enumerate(bow_decisions):
        plt.axhline(y=threshold, color='orange', linestyle='--', 
                   alpha=0.7, label=f'BowTieWd < {threshold:.2f}' if i == 0 else "")
        print(f"BowTieWd boundary at: {threshold}")
    
    # Formatting
    plt.xlabel('Hem Height (inches)')
    plt.ylabel('Bow Tie Width (inches)')
    plt.title('Spy Classification Data with Decision Boundaries\n(First 5 boundaries of each type shown)')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    # Save the plot
    plt.savefig('decision_boundaries.png', dpi=300, bbox_inches='tight')

    plt.close()

    print("Plot saved as 'decision_boundaries.png'")
    
def extract_left_data(features, labels, feature_index, threshold):
    """Extract data where feature < threshold"""
    left_features = []
    left_labels = []

    for i in range(len(features)):

        if features[i][feature_index] < threshold:

            left_features.append(features[i])
            left_labels.append(labels[i])

    return left_features, left_labels

"""
Extracts the data points focusing on feature >= threshold.
Function is used to get subset of right child node data
after split. 

This function is basically almost similar to node_analysis function.

@params features: list of feature data of hem height and bow tie width
@params labels: list of country labels from data set
@params feature_index: index of feature to split on (0: HemHt, 1: BowTieWd)
@params threshold: threshold value to split on
@return right_features: list of feature data where feature >= threshold
@return right_labels: list of country labels where feature >= threshold
"""
def extract_right_data(features, labels, feature_index, threshold):

    # Initializing empty lists of features and labels on right node
    right_features = [] # Store feature vectors where feature >= threshold
    right_labels = [] # Store labels where feature >= threshold

    # Iterate through all samples to extract based on threshold
    for i in range(len(features)):

        # Check if feature value is greater than or equal to threshold
        # If that's the case, append to right node lists
        if features[i][feature_index] >= threshold:

            right_features.append(features[i]) # Append feature vector to right vectors

            right_labels.append(labels[i]) # Append label to right labels

    # Return the extracted right node data
    return right_features, right_labels

"""
Builds the cascade decision tree recursively to construct
the classifier function cascade_code. Represents the skeleton
of the decision tree with fast decisions and remaining
data for further splitting.
@params features: list of feature data of hem height and bow tie width
@params labels: list of country labels from data set
@params depth: current depth of recursion for indentation
@return cascade_code: string representation of the cascade decision tree
"""
def build_cascade_tree(features, labels, depth=0):

    # Initialize node analysis to get number of total samples, florin count,
    # guilder count, majority class and purity.
    total_samples, florin_count, guilder_count, majority_class, purity = node_analysis(labels)

    #Base Case: When purity reaches to 95% or higher, return pure node decision
    if purity >= 0.95:

        indent = "    " * depth # Indentation based on depth
        return f'{indent}return "{majority_class}"  # Pure node: {purity:.1%} {majority_class}'
    
    # Recursive Case: Find best feature and threshold 
    # Reminder: find_best_split returns (best_feature_idx_index, best_threshold, best_cost)
    best_feature_idx, best_threshold, best_cost = find_best_split(features, labels)
    
    # Feature names for readability
    feature_names = ["HemHt", "BowTieWd"]

    # Split data based on best feature and threshold
    # Reminder: left_analysis is data where feature < threshold
    # right_analysis is data where feature >= threshold
    # Both of analysis values contain (total_count, florinian_count, guilderian_count, majority_class, purity)
    left_analysis, right_analysis = test_split(features, labels, best_feature_idx, best_threshold)

    # Assign variables for easiy readability

    left_purity = left_analysis[4]
    right_purity = right_analysis[4]
    # Determine which side of split will be fast decision
    # If left node has higher purity, make it fast decision
    # Reminder: node_analysis[4] = purity
    # The higher purity is the faster it is to decide

    if left_purity >= right_purity:

        fast_side = "left" # Set left side as purer fast decision
        left_majority_class = left_analysis[3]
        fast_class = left_majority_class # Majority class of left node (node_analysis[3] = majority_class)

        # Get the remaining data for right node for furter recursive processing
        remaining_features, remaining_labels = extract_right_data(features, labels, best_feature_idx, best_threshold)

    else:

        # If right node has higher purity, make it fast decision
        fast_side = "right" # Set right side as purer fast decision
        right_majority_class = right_analysis[3]
        fast_class = right_majority_class # Majority class of left node (node_analysis[3] = majority_class)
        
        # Get the remaining data for left node for furter recursive processing
        remaining_features, remaining_labels = extract_left_data(features, labels, best_feature_idx, best_threshold)

    # Creat indentation based on depth
    indent = "    " * depth
    
    # Build the code string based on which side is the fast decision
    if fast_side == "left":

        # Code string builder
        # If 

        # Reminder: Feature names = ["HemHt", "BowTieWd"]


        cascade_code = f'{indent}if {feature_names[best_feature_idx]} < {best_threshold:.3f}:\n'

        cascade_code += f'{indent}    return "{fast_class}"\n'

        cascade_code += f'{indent}else:\n'

        cascade_code += build_cascade_tree(remaining_features, remaining_labels, depth + 1)

    else:

        cascade_code = f'{indent}if {feature_names[best_feature_idx]} < {best_threshold:.3f}:\n'
        cascade_code += build_cascade_tree(remaining_features, remaining_labels, depth + 1) + '\n'
        cascade_code += f'{indent}else:\n'
        cascade_code += f'{indent}    return "{fast_class}"  # Fast decision'
    
    return cascade_code

"""
The data gets splitted based on feature index and threshold.
Function is used to find the best index of the threshold.
@params features: list of feature data of hem height and bow tie width
@params labels: list of country labels from data set
@returns: Tuple of (best_feature_idx_index, best_threshold, best_cost)
"""
def find_best_split(features, labels):

    # Used to initlized variables to keep track of best split
    best_cost = -float('inf') # Initialize to negative infinity
    best_feature_idx_index = -1 # Initialize to invalid index to find real split
    best_threshold = -1 # Initialize to invalid threshold to find real split
    total_samples = len(features) # Total number of samples

    # Iterate over each feature index (0: HemHt, 1: BowTieWd)
    for feature_idx in range(0,2): 

        # List of all values for current feature in all data samples
        feature_values = []

        # Iterates through all samples to get feature values
        for j in range(len(features)):
            
            # Append feature value at current index
            feature_values.append(features[j][feature_idx])

        # Get unique sorted feature values to get threshold
        # Set used to make values unique
        unique_values = sorted(set(feature_values))

        # Each thresholds will be tested 
        for i in range(0, len(unique_values) - 1):
            
            # Threshold is being calculated as the average of two consecutive unique values
            threshold = (list(unique_values)[i] + list(unique_values)[i + 1]) / 2

            # Data is being splitted into left and right nodes coming from current threshold
            left_analysis, right_analysis = test_split(features, labels, feature_idx, threshold)

            # Cost function is being calculated based on left and right node analysis          
            cost = cost_function(left_analysis, right_analysis, total_samples)

            # Once the cost is greater than best cost, then the value will get updated
            if cost > best_cost:

                best_cost = cost # Best cost updated
                best_feature_idx_index = feature_idx # Best feature updated (HemHt(0) or BowTieWd(1))
                best_threshold = threshold # Best threshold updated

    # Return parameters of best split found
    return best_feature_idx_index, best_threshold, best_cost

"""
Function calculates the cost of a split based on left and right node analysis.
@params left_analysis: analysis of left node after split
@params right_analysis: analysis of right node after split
@params total_samples: total number of samples before split
@return cost: calcualted cost of split (higher is better)
"""
def cost_function(left_analysis, right_analysis, total_samples):

    # After testing alpha values from 0...3, 2.17 provides best accuracy
    alpha = 2.17  
    
    # Calculate scores for left and right nodes
    left_score = (left_analysis[0] / total_samples) + (alpha * left_analysis[4])
    right_score = (right_analysis[0] / total_samples) + (alpha * right_analysis[4])
    
    return max(left_score, right_score)

"""
Splits the data based on feature index and threshold.
@params features: list of feature data of hem height and bow tie width
@params labels: list of country labels from data set
@params feature_index: index of feature to split on (0: HemHt, 1: BowTieWd)
@params threshold: threshold value to split on
@return left_analysis: analysis of left node after split
@return right_analysis: analysis of right node after split
"""
def test_split(features, labels, feature_index, threshold):
    
    # Initialize lists for left and right node labels
    left_labels = [] # Left labels where feature < threshold
    right_labels = [] # Right labels where feature >= threshold

    # Iterate through all samples to split based on threshold
    for i in range(len(features)):

        # If feature value is less than threshold, goes to left node
        if features[i][feature_index] < threshold:

            left_labels.append(labels[i])

        else:

            # Else goes to right node
            right_labels.append(labels[i])

    # Get analysis for left and right nodes
    # Reminder: node_analysis returns total_count, florinian_count, guilderian_count, majority_class, purity
    left_analysis = node_analysis(left_labels) 
    right_analysis = node_analysis(right_labels)

    # Return left and right analysis
    return left_analysis, right_analysis

"""
Computes statistics of the data set coming from
csv file. Used labels to get total count of samples, count of florinians,
count of guilderians, majority class and purity of the node.
@params labels: list of country labels from data set
@return total_count: total number of samples in the data set
@return florinian_count: number of florinians in the data set
@return guilderian_count: number of guilderians in the data set
@return majority_class: majority class label ("Florin" or "Guilder")
@return purity: proportion of majority class in the data set
"""
def node_analysis(labels):

    # Total sample of the data set
    total_count = len(labels)

    # Count of florinians and guilderians
    florinian_count = labels.count("Florin")
    guilderian_count = labels.count("Guilder")

    # Determine majority class by seeing in the data set if there's more florinians or guilderians
    majority_class = "Florin" if florinian_count >= guilderian_count else "Guilder"

    # Calculate purity as the proportion of the majority class
    purity = max(florinian_count, guilderian_count) / total_count

    # All computed values returned
    return total_count, florinian_count, guilderian_count, majority_class, purity

"""
The main function serves as the entry point for the program.
Loads florinian vs guilderian spy data set from a CSV file, 
processes it to extract features and labels and then constructs
it into cascade decision tree classifier.
"""
def main():
    """
    Load Florinian vs Guilderian data from CSV file
    1. Read the CSV file
    2. Parse the data into features and labels
    3. Print the number of data points loaded
    """
    with open ("Florinian_vs_Guilderian_Data_v24.csv", "r") as file:
        reader = csv.reader(file)
        header = next(reader)  # Skip header
        data = [row for row in reader]

    # data[0] = HemHt
    # data[1] = BowTieWd
    # data[2] = Country
    features = [] #List of feature data of hem height and bow tie width
    labels = [] #List of country labels
 
    # Iterates each row coming from the data that's being pulled from csv file
    for row in data:

        hem_ht = float(row[0]) # Each row hem height data (float value)
        bow_tie_wd = float(row[1]) # Each row bow tie width data (float value)
        country = row[2] # Each row country (label) data

        features.append([hem_ht, bow_tie_wd]) # Append hem height and bow tie width to features list as tuple
        labels.append(country) # Append country to labels list

    # Starts the cascade decision tree building process
    cascade_build = build_cascade_tree(features, labels) #Parameters used are features and labels lists

    full_cascade_code = "def classify_spy(HemHt, BowTieWd):\n"
    for line in cascade_build.splitlines():

    # Each line from cascade_cascade_code already has the proper relative indent;
    # we add exactly 4 spaces so it's all inside classify_spy(...)
        full_cascade_code += f"    {line}\n"

    with open("cascade_classifier.py", "w") as f:
        f.write(full_cascade_code)
    
    plot_data_and_decisions(features, labels, cascade_build)


    exec(full_cascade_code, globals())  # Make the classify_spy function available
    
    correct = 0
    total = len(features)
    
    for i in range(total):
        hemht = features[i][0]
        bowtie = features[i][1]
        true_label = labels[i]
        predicted_label = classify_spy(hemht, bowtie)
        
        if predicted_label == true_label:
            correct += 1
    
    accuracy = correct / total
    print(f"Classifier Accuracy: {accuracy:.1%}")


 
    #print(cost_value)
    #print(splitted_data)
    #print(best_split)

    #print(len(features))

if __name__ == "__main__":
    main()
