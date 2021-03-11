# mewto
# Model Evaluation with Weighted Threshold Optimization

### mewto is an R package that allows you to experiment with different thresholds for classification of prediction results in the case of binary classification problems and visualize various model evaluation metrics, confusion matrices and the ROC curve. It also allows you to calculate the optimal threshold based on a weighted evaluation criterion.

v1.0

**mewto** currently consists of two functions:

## mewtoApp
This function launches a Shiny application where the user can interactively manipulate the threshold used in binary classification and view the associated metrics, confusion matrix and ROC curve. The app also allows for optimal threshold calculation according to a weighted version of Youden's J-statistic.

In R, simply call the function:

**mewtoApp(_actuals_, _probabilities_)**

_actuals_ - Data of factor type with two levels: "yes" for positive and "no" for negative.

_probabilities_ - Data of numeric type which should represent the probabilities of realization of the positive category.

## mewtoThresh
This function calculates the optimal threshold according to a weighted version of Youden's J-statistic.

**mewtoThresh(_actuals_, _probabilities_, _weight_)**

_actuals_ - Data of factor type with two levels: "yes" for positive and "no" for negative.

_probabilities_ - Data of numeric type which should represent the probabilities of realization of the positive category.

_weight_ - The importance attributed to sensitivity, or formulated differently, to the maximization of the true positives rate.

## Technical details

In the calculation of the optimal threshold, a weighted version of Youden's J-statistic (Youden, 1950) is employed. The optimal cut-off is the threshold that maximizes the distance to the identity (diagonal) line. The function maximizes the metric:

**2 * (w * sensitivity + (1 - w) * specificity)**, where "w" is the "weight" parameter.

Youden's J-statistic has been modified by adding the weighting parameter "w". To keep the values of the modified version of the evaluation criterion consistent with the one calculated by the original version of Youden, a scaling factor of 2 has been used. This particular statistic has been chosen since it is well-suited for weighting, and it is also the default criterion used in the R package pROC.

## Download and installation

#### Online, from Github:

You can download **mewto** directly from Github. To do so, you need to have the **devtools** pachage installed and loaded. Once you are in **R**, run the following commands:

> install.packages("devtools")
> 
> library("devtools")
> 
> install_github("alexandrumonahov/mewto")

You may face downloading errors from Github if you are behind a forewall or there are https download restrictions. To avoid this, you can try running the following commands:

> options(download.file.method = "libcurl")
> 
> options(download.file.method = "wininet")

#### Offline, by manually downloading and installing the package files:

Alternatively, if you cannot download the file through Github, you may also download the binary package file from the link below:

https://github.com/alexandrumonahov/zip/blob/main/mewto_1.0.zip

Place the downloaded file into the working directory of R. The do one of the following:

_Option 1) Run the following command:_

> install.packages('mewto_1.0.zip', repos = NULL, type = "win.binary")

_Option 2) In RStudio:_

Go to the Packages tab in the bottom-right pane and click on "Install". In the pop-up window that appears, click on "Browse" and choose the package mewto.zip that you have just downloaded. Click on "Install".

Once the package is stalled, you can run it using the: **library(mewto)** command.

Alexandru Monahov, 2021
