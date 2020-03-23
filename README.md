# Reproducible code for the zero-inflated latent Ising analysis 
## Author: Jie Zhou  

Documentation can be found at `documentation` folder. This is in R documentation format and can be easily accessed through RStudio.  

Detailed documentation as follows:  

## `Binary`: Generate the design matrix for logistic regression  
### Description  
This function is used to generate the design matrix from a the given  categorical observation.  

### Usage  

```r
Binary(MultiLevel, N)
```
### Arguments

Argument      |Description
------------- |----------------
```MultiLevel```     |     refers to the categorical observations
```N```     |     vector indicating the degree of freedom for each categorical variable



## `MultiLevel`: Transform all the variables into categorical variables with a upper bound for degree of freedom

### Description

Only a common upper bound MaxGroup for the degree of freedom is required for the data transformation.

### Usage

```r
MultiLevel(data, MaxGroup, size)
```
### Arguments

Argument      |Description
------------- |----------------
```data```     |     n by p data matrix
```MaxGroup```     |     the common upper bound for the number of levels    

## `netmat`: Generate the adjacent matrix using the zero-inflated latent Ising model

### Description

This is the main function that implement our algorithm.


### Usage

```r
netmat(A, width)
```


### Arguments

Argument      |Description
------------- |----------------
```A```     |     a binary matrix representing the transfomred data
```width```     |     a vector indicating which colomns are belonging to the same categorical variable

## `CuttingPoint`: Transform the continuous data into categorical data


### Description

This function uses  dynamic programming algorithm to transform the zero-inflated continuous variable into categorical variable that minimize the squared Euclidean distance.


### Usage

```r
CuttingPoint(x, N)
```


### Arguments

Argument      |Description
------------- |----------------
```x```     |     vector x representing the observations on continuous variable to be transformed.
```N```     |     N representing the specified number of the levels

