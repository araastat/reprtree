__reprtree__ is a package to implement the concept of 
representative trees from ensembles of tree-based machines
introduced by Banerjee, _et al_, 2012. 

reprtree currently implements the d2 metric (closeness based
on prediction) from Banerjee, _et al_, using either euclidean distance (for numeric predictions) or the 
percentage mismatch (for multi-class classification). The package implements representative trees for random forests currently, and will be extended to bagged trees. 

Plotting representative trees is based on the _tree_ and _plotrix_ packages. Other nicer visualizations will be developed.

_Reference:_ Banerjee M., Ding Y., Noone A.M (2012). 
    Identifying Representative Trees from Ensembles. _Statistics in Medicine_.