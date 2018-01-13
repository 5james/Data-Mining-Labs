# Data mining laboratory #
Exercises (scripts) made as part of the laboratory:
1. Association Rules 
	* database: Groceries
	* practical purpose: reorganization of products in the store
	* library: arules
	* how: finding all the relationships (Association Rules) between products (like candy bar), then products divided into product types (like snacks) and finally between groups of product types (like edible).
2. Rules Sequences
	* database: database based on https://archive.ics.uci.edu/ml/datasets/Diabetes
	* practical purpose: search for patterns leading to: Hypoglycemia, Hyperglycemia and Hypoglycemia symptoms 
	* library: arulesSequences (with cSPADE)
	* how: discretize all doses of insulin (regular, NPH and UltraLente) and results of blood glucose measurement, then search for all sequences leading to either high / low glucose measurement or Hypoglycemia symptoms 
3. Classification and Classifiers
	* database: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv (varaibles in detail: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/)
	* library: rpart, caret, party
	* practical purpose: build classifier which can determine wine quality based on other physical and chemical factors.
	* how: firstly, discretize qualities on bad, medium and premium; then try to create the best classifier by controlling rpart.control. Classifiers were evaluated using F-measure (with precision and accuracy equally important -> β = 0).
3. Classification and Classifiers
	* database: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv (varaibles in detail: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/)
	* library: rpart, caret, party
	* practical purpose: build classifier which can determine wine quality based on other physical and chemical factors.
	* how: firstly, discretize qualities on bad, medium and premium; then try to create the best classifier by controlling rpart.control. Classifiers were evaluated using F-measure (with precision and accuracy equally important -> β = 0).
3. Clustering
	* database: database based on https://www.kaggle.com/abcsds/pokemon
	* practical purpose: finding best possible clustering for pokemons (without using their types)