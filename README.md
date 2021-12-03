## Nike Vaporfly Running Shoes Improvement - Shiny App

### 1. Brief description of the app and its purpose.
  
The purpose of the repo is to use supervised machine learning methods (linear regressions, ensemble methods) to develop different predictive models that focus on predicting the popularity of online news articles given the features of those articles published on Mashable, one of the largest online news sites. Since the target response is a continuous variable, the number of shares, the predictive models are compared with their RMSE value when fitting on the test set. Then, the best predictive model is chosen with the lowest RMSE value when fitting on the test set. We declare a winning model in predicting the online news popularity measures. In addition, this process is automated to run the analysis, perform exploratory data analysis, fit different predictive models, compare different predictive models and find the optimal model for each of the data channels. The articles were collected from six data channels on Mashable: lifestyle, entertainment, business, social media, technology and world.  
  
### 2. A list of R packages needed to run the app

   * rmarkdown
   * tidyverse
   * knitr
   * caret
   * corrplot
   * ggplot2
   * gbm
   * vip

### 3. A line of code to install all the packages used

```markdown
  install.packages("tidyverse", "ggplot2", "caret", "vip", "knitr", "rmarkdown")
```

### 4. The shiny::runGitHub() code that we can copy and paste into RStudio to run the app

```markdown  
  library(rmarkdown)  
  library(tidyverse)
  
  shiny::runGitHub("Vaporfly-Shiny", "peachgal") 
```  
