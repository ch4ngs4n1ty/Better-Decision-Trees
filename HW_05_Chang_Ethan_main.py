# HW 5:
# Ethan Chang
# 

import csv

def extract_left_data(features, labels, feature_index, threshold):
    """Extract data where feature < threshold"""
    left_features = []
    left_labels = []

    for i in range(len(features)):

        if features[i][feature_index] < threshold:

            left_features.append(features[i])
            left_labels.append(labels[i])

    return left_features, left_labels

def extract_right_data(features, labels, feature_index, threshold):
    """Extract data where feature >= threshold"""
    right_features = []
    right_labels = []
    for i in range(len(features)):
        if features[i][feature_index] >= threshold:
            right_features.append(features[i])
            right_labels.append(labels[i])
    return right_features, right_labels

def build_cascade_tree(features, labels, depth=0, max_depth=20):

    total_samples = len(features)

    total, florin_count, guilder_count, majority_class, purity = node_analysis(labels)

    #Base Case: Stop when purity >= 0.95
    if purity >= 0.95 or depth >= max_depth:

        indent = "    " * depth
        return f'{indent}return "{majority_class}"  # Pure node: {purity:.1%} {majority_class}'
    
    best_feature, best_threshold, best_cost = find_best_split(features, labels)
    
    feature_names = ["HemHt", "BowTieWd"]

    left_analysis, right_analysis = test_split(features, labels, best_feature, best_threshold)

    if left_analysis[4] >= right_analysis[4]:

        fast_side = "left"
        fast_class = left_analysis[3]
        remaining_features, remaining_labels = extract_right_data(features, labels, best_feature, best_threshold)
    else:

        fast_side = "right"
        fast_class = right_analysis[3]
        remaining_features, remaining_labels = extract_left_data(features, labels, best_feature, best_threshold)

    indent = "    " * depth
    
    #code = f'{indent}if {feature_names[best_feature]} < {best_threshold:.3f}:\n'

    if fast_side == "left":

        code = f'{indent}if {feature_names[best_feature]} < {best_threshold:.3f}:\n'
        code += f'{indent}    return "{fast_class}"  # Fast decision\n'
        code += f'{indent}else:\n'
        code += build_cascade_tree(remaining_features, remaining_labels, depth + 1)

    else:

        code = f'{indent}if {feature_names[best_feature]} < {best_threshold:.3f}:\n'
        code += build_cascade_tree(remaining_features, remaining_labels, depth + 1) + '\n'
        code += f'{indent}else:\n'
        code += f'{indent}    return "{fast_class}"  # Fast decision'
    
    return code


def find_best_split(features, labels):

    best_cost = -float('inf')
    best_feature_index = -1
    best_threshold = -1
    total_samples = len(features)

    for feature_idx in range(0 , 2): 

        feature_values = []

        for j in range(len(features)):
            
            feature_values.append(features[j][feature_idx])

        unique_values = sorted(set(feature_values))

        for i in range(0, len(unique_values) - 1):

            threshold = (list(unique_values)[i] + list(unique_values)[i + 1]) / 2

            left_analysis, right_analysis = test_split(features, labels, feature_idx, threshold)

            cost = cost_function(left_analysis, right_analysis, total_samples)

            if cost > best_cost:

                best_cost = cost
                best_feature_index = feature_idx
                best_threshold = threshold

    return best_feature_index, best_threshold, best_cost


def cost_function(left_analysis, right_analysis, total_samples):
    alpha = 2.17  # Try different values: 0.1, 0.5, 1.0, 2.0
    
    left_score = (left_analysis[0] / total_samples) + (alpha * left_analysis[4])
    right_score = (right_analysis[0] / total_samples) + (alpha * right_analysis[4])
    
    return max(left_score, right_score)

def test_split(features, labels, feature_index, threshold):
    
    left_labels = []
    right_labels = []

    for i in range(len(features)):

        if features[i][feature_index] < threshold:

            left_labels.append(labels[i])

        else:

            right_labels.append(labels[i])

    left_analysis = node_analysis(left_labels)
    right_analysis = node_analysis(right_labels)

    return left_analysis, right_analysis


"""
"""
def node_analysis(labels):

    total_count = len(labels)

    if total_count == 0:
        return 0, 0, 0, "Unknown", 0.0 
    
    florinian_count = labels.count("Florin")
    guilderian_count = labels.count("Guilder")
    majority_class = "Florin" if florinian_count >= guilderian_count else "Guilder"
    purity = max(florinian_count, guilderian_count) / total_count

    return total_count, florinian_count, guilderian_count, majority_class, purity

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
 
    for row in data:

        hem_ht = float(row[0])
        bow_tie_wd = float(row[1])
        country = row[2]

        features.append([hem_ht, bow_tie_wd])
        labels.append(country)


    #rint(features)

    collected_data = node_analysis(labels)
    splitted_data = test_split(features, labels, 0, 5.0)
    cost_value = cost_function(splitted_data[0], splitted_data[1], collected_data[0])
    best_split = find_best_split(features, labels)
    #print(best_split)

    left_analysis, right_analysis = test_split(features, labels, 1, 4.145)

    cascade_build = build_cascade_tree(features, labels)

    full_code = "def classify_spy(HemHt, BowTieWd):\n"
    for line in cascade_build.splitlines():
    # Each line from cascade_code already has the proper relative indent;
    # we add exactly 4 spaces so it's all inside classify_spy(...)
        full_code += f"    {line}\n"

    with open("cascade_classifier.py", "w") as f:
        f.write(full_code)
    

    exec(full_code, globals())  # Make the classify_spy function available
    
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
