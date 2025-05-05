# LIS4370-Final - IrisTools

Overview
IrisTools is an R package designed to analyze the iris dataset, created as the final project for LIS-4370 R Programming for Data Science. It provides three functions to calculate means, summarize measurements, and visualize data, using S3 classes for enhanced functionality. The package depends on dplyr for data manipulation and ggplot2 for plotting.

Installation
To install IrisTools from GitHub, use the following commands in R:

```R
# Install devtools if not already installed
install.packages("devtools")

# Install IrisTools from GitHub
devtools::install_github("steven.barden/IrisTools")

# Load the package
library(IrisTools)
```
Usage
The package includes three main functions:

Calculate Means by Species
Compute the average measurements for each species in the iris dataset.

```R
means <- calc_iris_means()
print(means)
```

Plot Measurements
Create a scatter plot of Sepal Length vs Petal Length, colored by species.

```R
plot_iris()
```

Summarize Measurements
Generate a summary of minimum, maximum, and mean values for each measurement.

```R
summary <- summarize_iris()
print(summary)
```

For more detailed examples, see the package vignette:

```R
vignette("iris-tools")
```

License
This package is licensed under the MIT License. See the LICENSE file for details.

Repository
GitHub: https://github.com/StevenBarden/LIS4370-Final

