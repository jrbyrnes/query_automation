User Interface Notes:

Categorization refers to a column that flags unwanted records (such as those corresponding to outliers, or those that have weird / null values)
	-query will only select values that have a null value of categorization column

Projected / weighted column refers to a column with a pre-determined value for scaling the importance of a certain record

Primary refers to the inner constraint, and it subsets the eligible user base. Secondary constrains the eligible userbase.
	-EX: if you want to compare tablet spending of consumer who bought iphones; iphones is primary constraint and tablet is secondary constraint

(3/7/15)
	-currently working on getparams1 which allows for a less hard coded / looser restrictions on query parameters while still maintaining simple UI

