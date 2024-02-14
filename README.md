# GLLVM simulations

Brief repository hosting some simulated data sets to test the behaviour of GLLVMs (using `{gllvm}`).

Using Equation 1 from [Niku et al. (2019)](https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13303), we simulated four abundance data sets. The data sets range from an information-rich, likely very few zeroes and high counts to a data set with many zeroes and lower counts (more likely what you would found in a marine, e.g. seabird, data set). We then ran unconstrained ordinations without and with predictors (in our case, we simulated a dummy variable we called 'season').

Finally, we evaluated how the models/ordination plots behaved when including the predictor.