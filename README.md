## Shiny App - Nike Vaporfly Marathon Performance Improvement Study

### 1. Brief description of the app and its purpose.
  
The purpose of the APP is to create a Shiny App that can be used to explore the chosen data set and implement and compare three predictive modeling fits using the supervised machine learning methods. We will utilize the functionality of the Shiny App to perform exploratory data analysis of the collected marathon athletes data and see if there is a significant effect of Nike Vaporfly shoes on marathon runners' performance. This App is user-friendly and interactive with user interface. Users are able to browse the data and select the choices they want in EDA, model building, model fitting and prediction in the App.
  
### 2. A list of R packages needed to run the app

   * shiny
   * rmarkdown
   * tidyverse
   * caret
   * ggplot2
   * DT
   * vip

### 3. A line of code to install all the packages used

```markdown
  install.packages(c("shiny", "rmarkdown", "tidyverse", "caret", "ggplot2", "DT", "vip"))
```

### 4. The shiny::runGitHub() code that we can copy and paste into RStudio to run the app

```markdown  
  shiny::runGitHub("Vaporfly-Shiny", "peachgal", ref = "main", subdir = "ShinyApp") 
```  
