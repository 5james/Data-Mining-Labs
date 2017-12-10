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