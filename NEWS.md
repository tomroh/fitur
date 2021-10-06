# fitur 0.6.2

* fixing issues with the order of loading functions given the "actuar" package has implemented it's own 'var' and 'sd' functions

# fitur 0.6.1

* shiny gadget for fitting univariate distributions has been added

* added test function for distfun objects

* diagnostic plots now have better checks for distfun objects and lists of
distfun objects

# fitur 0.6.0

* Adding continuous distribution testing functions Kolmogorov-Smirnov, Anderson-Darling, and Cramer-Von Mises

* S3 methods have now been added for distfit objects

* Code reformatting and cleanup

# fitur 0.5.25

* Fixed appearance of plots

* Added plot_density function for comparison pdfs of fitted functions

* Updated argument naming conventions 

# fitur 0.5.20

* DESCRIPTION summary has been updated

* Examples have been updated for fit_univariate, fit_empirical

* fit_empirical_discrete and fit_empirical_continuous are no longer exported

* added plot_qq and plot_pp functions for diagnostic plotting of fits

* Added a vignette Diagnostic Plots for Fitting Distributions

* Introduction vignette has updated examples

# fitur 0.5.0

* Update DESCRIPTION

* Parameter name error checking for fit_univariate_manual

* Fixed bug so that empirical distributions functions are now vectorized

* Consolidated fitting empirical distributions into one function

* Fixed vignette title

* Added BugReports to Description

# fitur 0.4.0

* Added fit_univariate_man for manually specifying parameters and generating a
distribution

* Added function to summarize statistics for distribution inspection

* Added documentation for supported univariate distributions

# fitur 0.3.0

* Changed naming of fitted distribution object (output of fit_univariate) to be 
more explicit e.g. 'd' is now 'dpois' for a fitting a poisson distribution

* Added 'parameters' list item to a fitted distribution object (output of 
fit_univariate)


# fitur 0.2.0

* Added Discrete Uniform distribution functions and ability to fit discrete uniform

* Added cauchy, llogis, logis, invweibull, invgamma to continous distributions that can be fit

# fitur 0.1.0

* Added a `NEWS.md` file to track changes to the package.



