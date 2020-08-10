# Visualization of log-likelihood landscape of PH distribution

This file aims to explain how to run the code. For details about methodology and problem background, please look at the report.pdf

# Configurations
1. Have your R files installed. Refer to https://www.r-project.org/ for downloading
2. (Optional) Have a suitable GUI application installed to manage R files. Rstudio is recommended. 
3. Requires ackage _actuar_, which helps to simulate data of PH distribution and computes the density of PH distribution.
4. Requires package _plotly_, which can help generate high-quality user interactive 3-D plots.
5. R packages can be installed by calling _install.packages("package name")_ in the console of Rstudio.

# Run the code
Just simply open the Visualization.R file and run it.

# Change settings
1. The whole code is divided into three parts. Part 1 is to set a target PH distribution and the random directions to project. Part 2 is functions definition (don't modify). Part 3 is to set the params of visualization
2. Every block of codes is equipped with very detailed comments to explain how to change the current setting to a desired one
3. __Example 1__: Un-comment the block of code beginning from line 45 to line 65 to use the second random directions
4. __Example 2__: Modify code at line 20 to simulate data from different distribution (e.g., can call _rexp_ to simuate data from exponential distribution)