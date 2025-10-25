"""
HW_NN_Kinsman_Thomas_main.py
Decision Tree with Fast Rejection Cascade for Florin vs Guilder Classification
Author: Thomas Kinsman
Date: [Current Date]

This program creates a decision tree with fast rejection cascade to classify
individuals as Florinian or Guilderian based on hem height and bow tie width.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class DecisionTreeCascade:
    """
    A decision tree classifier that builds a fast rejection/acceptance cascade
    to efficiently classify Florinians vs Guilderians.
    """
    
    def __init__(self, alpha=0.1, min_purity=0.95):
        """
        Initialize the decision tree cascade
        
        Parameters:
        alpha (float): Regularization parameter for cost function
        min_purity (float): Minimum purity threshold to stop splitting
        """
        self.alpha = alpha
        self.min_purity = min_purity
        self.tree = None
        self.feature_names = ['HemHt', 'BowTieWd']
        
    def load_data(self, filename):
        """
        Load the classified data from CSV file
        
        Parameters:
        filename (str): Path to the CSV file
        
        Returns:
        pandas.DataFrame: Loaded data with features and labels
        """
        print("Loading classified data...")
        data = pd.read_csv(filename)
        
        # Convert country labels to binary (0 for Guilder, 1 for Florin)
        data['label'] = (data['Country'] == 'Florin').astype(int)
        
        print(f"Loaded {len(data)} records")
        print(f"Florinians: {sum(data['label'])}")
        print(f"Guilderians: {len(data) - sum(data['label'])}")
        
        return data
    
    def explore_data(self, data):
        """
        Explore the basic statistics and distribution of the data
        
        Parameters:
        data (pandas.DataFrame): The loaded dataset
        """
        print("\n=== Data Exploration ===")
        print("\nBasic Statistics:")
        print(data.describe())
        
        print("\nClass Distribution:")
        print(data['Country'].value_counts())
        
        # Check for missing values
        print("\nMissing Values:")
        print(data.isnull().sum())
        
    def calculate_impurity(self, labels):
        """
        Calculate Gini impurity for a set of labels
        
        Parameters:
        labels (array-like): Array of binary labels (0 or 1)
        
        Returns:
        float: Gini impurity value
        """
        if len(labels) == 0:
            return 0
            
        # Calculate proportion of each class
        p_florin = np.mean(labels)
        p_guilder = 1 - p_florin
        
        # Gini impurity: 1 - sum(p_i^2)
        impurity = 1 - (p_florin**2 + p_guilder**2)
        return impurity
    
    def calculate_purity(self, labels):
        """
        Calculate purity as 1 - impurity
        
        Parameters:
        labels (array-like): Array of binary labels
        
        Returns:
        float: Purity value
        """
        impurity = self.calculate_impurity(labels)
        return 1 - impurity
    
    def calculate_information_gain(self, data, left_data, right_data):
        """
        Calculate information gain for a split
        
        Parameters:
        data: Parent node data
        left_data: Left child data
        right_data: Right child data
        
        Returns:
        float: Information gain
        """
        # Calculate parent impurity
        parent_impurity = self.calculate_impurity(data['label'])
        
        # Calculate weighted average of children impurities
        n_left = len(left_data)
        n_right = len(right_data)
        n_total = len(data)
        
        if n_total == 0:
            return 0
            
        left_impurity = self.calculate_impurity(left_data['label'])
        right_impurity = self.calculate_impurity(right_data['label'])
        
        weighted_impurity = (n_left / n_total) * left_impurity + (n_right / n_total) * right_impurity
        
        # Information gain = reduction in impurity
        information_gain = parent_impurity - weighted_impurity
        
        return information_gain

    def improved_cost_function(self, data_subset, total_size):
        """
        Improved cost function that balances size and purity
        
        Parameters:
        data_subset (pandas.DataFrame): Subset of data in the node
        total_size (int): Total size of the dataset
        
        Returns:
        float: Cost value
        """
        if len(data_subset) == 0:
            return -np.inf  # Penalize empty nodes
            
        fraction = len(data_subset) / total_size
        purity = self.calculate_purity(data_subset['label'])
        
        # We want nodes that are both large and pure
        # Use product of fraction and purity to balance both
        base_cost = fraction * purity
        
        # Add bonus for high purity
        purity_bonus = self.alpha * purity
        
        # Penalize very small nodes more aggressively
        size_penalty = 0
        if len(data_subset) < 10:  # Strong penalty for very small nodes
            size_penalty = -0.5
        elif len(data_subset) < 20:  # Moderate penalty for small nodes
            size_penalty = -0.2
            
        total_cost = base_cost + purity_bonus + size_penalty
        
        return total_cost

    def find_best_split(self, data):
        """
        Find the best split using improved cost function
        
        Parameters:
        data (pandas.DataFrame): The dataset to split
        
        Returns:
        dict: Information about the best split found
        """
        best_split = None
        best_cost = -np.inf
        total_size = len(data)
        
        print(f"\nSearching for best split among {len(data)} records...")
        
        # Test both features
        for feature in self.feature_names:
            print(f"  Testing feature: {feature}")
            
            # Get unique values sorted for threshold testing
            unique_vals = np.sort(data[feature].unique())
            
            # Test thresholds between unique values (sample to save time)
            thresholds = []
            for i in range(len(unique_vals) - 1):
                threshold = (unique_vals[i] + unique_vals[i + 1]) / 2
                thresholds.append(threshold)
            
            # Sample thresholds if there are too many
            if len(thresholds) > 50:
                step = len(thresholds) // 50
                thresholds = thresholds[::step]
            
            print(f"    Testing {len(thresholds)} thresholds for {feature}...")
            
            for threshold in thresholds:
                # Split data based on threshold
                left_data = data[data[feature] <= threshold]
                right_data = data[data[feature] > threshold]
                
                # Skip splits that create very small nodes (unless they're very pure)
                if len(left_data) < 5 and len(right_data) < 5:
                    continue
                    
                # Calculate cost for both sides using improved cost function
                left_cost = self.improved_cost_function(left_data, total_size)
                right_cost = self.improved_cost_function(right_data, total_size)
                
                # We want the side with highest cost (big + pure)
                if left_cost > best_cost and len(left_data) >= 10:
                    best_cost = left_cost
                    best_split = {
                        'feature': feature,
                        'threshold': threshold,
                        'direction': 'left',
                        'cost': left_cost,
                        'left_size': len(left_data),
                        'right_size': len(right_data),
                        'left_purity': self.calculate_purity(left_data['label']),
                        'right_purity': self.calculate_purity(right_data['label']),
                        'left_data': left_data,
                        'right_data': right_data
                    }
                
                if right_cost > best_cost and len(right_data) >= 10:
                    best_cost = right_cost
                    best_split = {
                        'feature': feature,
                        'threshold': threshold,
                        'direction': 'right',
                        'cost': right_cost,
                        'left_size': len(left_data),
                        'right_size': len(right_data),
                        'left_purity': self.calculate_purity(left_data['label']),
                        'right_purity': self.calculate_purity(right_data['label']),
                        'left_data': left_data,
                        'right_data': right_data
                    }
        
        return best_split

    def build_cascade(self, data, max_depth=10, current_depth=0):
        """
        Recursively build the fast rejection cascade
        
        Parameters:
        data (pandas.DataFrame): Current subset of data
        max_depth (int): Maximum depth of the tree
        current_depth (int): Current depth in recursion
        
        Returns:
        dict: The decision tree node
        """
        # Stop if we've reached max depth or node is empty
        if current_depth >= max_depth or len(data) == 0:
            majority_class = 'Florin' if np.mean(data['label']) > 0.5 else 'Guilder'
            return {
                'type': 'leaf',
                'class': majority_class,
                'purity': self.calculate_purity(data['label']),
                'size': len(data)
            }
        
        # Calculate current purity
        current_purity = self.calculate_purity(data['label'])
        
        print(f"\nDepth {current_depth}: {len(data)} records, purity: {current_purity:.3f}")
        
        # Stop if we've reached desired purity or node is too small
        if current_purity >= self.min_purity or len(data) < 15:
            majority_class = 'Florin' if np.mean(data['label']) > 0.5 else 'Guilder'
            print(f"  ✓ Stopping: purity {current_purity:.3f} >= {self.min_purity} or size {len(data)} < 15")
            print(f"  Majority class: {majority_class}")
            return {
                'type': 'leaf',
                'class': majority_class,
                'purity': current_purity,
                'size': len(data)
            }
        
        # Find best split
        best_split = self.find_best_split(data)
        
        if best_split is None:
            majority_class = 'Florin' if np.mean(data['label']) > 0.5 else 'Guilder'
            print(f"  No good split found. Leaf: {majority_class}")
            return {
                'type': 'leaf',
                'class': majority_class,
                'purity': current_purity,
                'size': len(data)
            }
        
        print(f"  Best split: {best_split['feature']} {best_split['direction']} {best_split['threshold']:.3f}")
        print(f"  Cost: {best_split['cost']:.3f}, Left: {best_split['left_size']} (purity: {best_split['left_purity']:.3f}), "
              f"Right: {best_split['right_size']} (purity: {best_split['right_purity']:.3f})")
        
        # Split the data
        if best_split['direction'] == 'left':
            left_data = best_split['left_data']
            right_data = best_split['right_data']
        else:
            left_data = best_split['right_data']
            right_data = best_split['left_data']
        
        # Create node - prioritize the high-cost side for the cascade
        node = {
            'type': 'decision',
            'feature': best_split['feature'],
            'threshold': best_split['threshold'],
            'direction': best_split['direction'],
            'cost': best_split['cost'],
            'purity': current_purity,
            'size': len(data),
            'high_cost_child': self.build_cascade(left_data, max_depth, current_depth + 1),
            'low_cost_child': self.build_cascade(right_data, max_depth, current_depth + 1)
        }
        
        return node

    def plot_data_with_splits(self, data, tree, title="Data with Decision Boundaries"):
        """
        Plot the data and show the decision boundaries
        """
        plt.figure(figsize=(12, 8))
        
        # Plot Florin points
        florin_data = data[data['label'] == 1]
        plt.scatter(florin_data['HemHt'], florin_data['BowTieWd'], 
                    c='red', label='Florin', alpha=0.7, s=60, edgecolors='black', linewidth=0.5)
        
        # Plot Guilder points  
        guilder_data = data[data['label'] == 0]
        plt.scatter(guilder_data['HemHt'], guilder_data['BowTieWd'],
                    c='blue', label='Guilder', alpha=0.7, s=60, edgecolors='black', linewidth=0.5)
        
        # Plot decision boundaries from the tree
        self._plot_tree_boundaries(tree, depth=0)
        
        plt.xlabel('Hem Height (inches)')
        plt.ylabel('Bow Tie Width (inches)')
        plt.title(title)
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.show()
    
    def _plot_tree_boundaries(self, node, depth=0, xlim=(2, 12), ylim=(4, 14)):
        """
        Recursively plot decision boundaries from the tree
        """
        if node is None or node['type'] == 'leaf':
            return
            
        feature = node['feature']
        threshold = node['threshold']
        
        # Choose color based on depth
        colors = ['green', 'orange', 'purple', 'brown', 'pink']
        color = colors[depth % len(colors)]
        
        if feature == 'HemHt':
            plt.axvline(x=threshold, color=color, linestyle='--', 
                       linewidth=2, alpha=0.7, label=f'Depth {depth}: HemHt = {threshold:.2f}')
        else:
            plt.axhline(y=threshold, color=color, linestyle='--', 
                       linewidth=2, alpha=0.7, label=f'Depth {depth}: BowTieWd = {threshold:.2f}')
        
        # Recursively plot children
        if 'high_cost_child' in node:
            self._plot_tree_boundaries(node['high_cost_child'], depth + 1)
        if 'low_cost_child' in node:
            self._plot_tree_boundaries(node['low_cost_child'], depth + 1)

    def print_cascade_decisions(self, tree=None, depth=0):
        """
        Print the first few decisions of the cascade in a readable format
        """
        if tree is None:
            tree = self.tree
            
        if tree is None:
            print("No tree to display")
            return
            
        indent = "  " * depth
        
        if tree['type'] == 'leaf':
            print(f"{indent}Leaf: Class = {tree['class']}, Purity = {tree['purity']:.3f}, Size = {tree['size']}")
        else:
            direction_str = "<=" if tree['direction'] == 'left' else ">"
            print(f"{indent}Decision {depth}: IF {tree['feature']} {direction_str} {tree['threshold']:.3f}")
            print(f"{indent}  (Cost: {tree['cost']:.3f}, Purity: {tree['purity']:.3f}, Size: {tree['size']})")
            
            # Only print the high-cost child (the cascade path)
            if 'high_cost_child' in tree:
                self.print_cascade_decisions(tree['high_cost_child'], depth + 1)

    def generate_classifier_code(self, tree=None, depth=0):
        """
        Generate Python code for the classifier
        """
        if tree is None:
            tree = self.tree
            
        if tree is None:
            return "# No classifier generated"
        
        code_lines = []
        indent = "    " * depth
        
        if tree['type'] == 'leaf':
            code_lines.append(f"{indent}return '{tree['class']}'")
        else:
            direction_str = "<=" if tree['direction'] == 'left' else ">"
            condition = f"{tree['feature']} {direction_str} {tree['threshold']}"
            code_lines.append(f"{indent}if {condition}:")
            
            # High cost child (cascade path)
            if 'high_cost_child' in tree:
                child_code = self.generate_classifier_code(tree['high_cost_child'], depth + 1)
                code_lines.append(child_code)
            
            # Low cost child
            code_lines.append(f"{indent}else:")
            if 'low_cost_child' in tree:
                child_code = self.generate_classifier_code(tree['low_cost_child'], depth + 1)
                code_lines.append(child_code)
            else:
                code_lines.append(f"{indent}    return 'Unknown'")
        
        return "\n".join(code_lines)

def main():
    """
    Main function to run the decision tree cascade program
    """
    print("=== Florin vs Guilder Decision Tree Cascade ===")
    print("Initializing classifier...")
    
    # Initialize the classifier with lower alpha to prioritize size over purity
    classifier = DecisionTreeCascade(alpha=0.1, min_purity=0.95)
    
    # Load the data
    try:
        data = classifier.load_data('Florinian_vs_Guilderian_Data_v24.csv')
        
        # Explore the data
        classifier.explore_data(data)
        
        print("\n" + "="*60)
        print("BUILDING FAST REJECTION CASCADE")
        print("="*60)
        
        # Build the cascade tree
        tree = classifier.build_cascade(data, max_depth=8)
        classifier.tree = tree
        
        print("\n" + "="*60)
        print("CASCADE BUILDING COMPLETE")
        print("="*60)
        
        # Display the first few decisions
        print("\nFIRST FEW CASCADE DECISIONS:")
        print("-" * 40)
        classifier.print_cascade_decisions()
        
        # Generate and display classifier code
        print("\nGENERATED CLASSIFIER CODE:")
        print("-" * 40)
        classifier_code = f"""
def classify_person(HemHt, BowTieWd):
    \"\"\"
    Classify a person as Florin or Guilder based on measurements
    \"\"\"
{classifier.generate_classifier_code()}
"""
        print(classifier_code)
        
        # Plot the results
        print("\nGenerating visualization...")
        classifier.plot_data_with_splits(data, tree, "Florin vs Guilder Classification with Decision Boundaries")
        
        # Answer assignment questions
        print("\n" + "="*60)
        print("ASSIGNMENT QUESTIONS")
        print("="*60)
        
        print("\n3a. Cost Function Used:")
        print("   - Improved cost function: (fraction * purity) + alpha * purity")
        print("   - With penalties for small nodes (<10 records)")
        print("   - Alpha = 0.1 to balance size and purity")
        print("   - Decision: Prioritized larger nodes while maintaining reasonable purity")
        
        print("\n3d. Top Three Cascade Decisions:")
        classifier.print_cascade_decisions()
        
        print("\n5. Bonus Visualization:")
        print("   - Plot generated showing data points and decision boundaries")
        print("   - Different colors represent different depth levels")
        print("   - Helps visualize how the cascade partitions the feature space")
        
    except FileNotFoundError:
        print("Error: Data file not found!")
        print("Please ensure 'Florinian_vs_Guilderian_Data_v24.csv' is in the same directory")
        return
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return

if __name__ == "__main__":
    main()