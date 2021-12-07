#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rmarkdown)
#library(shinydashboard)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe)
shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
shoes_data$year <- cut(shoes_data$year, 5, c("2015", "2016", "2017", "2018", "2019"))
shoes_data$vaporfly <- as.numeric(shoes_data$vaporfly)
shoes_data$vaporfly <- cut(shoes_data$vaporfly, 2, c("No", "Yes"))
shoes_data <- data.frame(shoes_data)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(

    # Application title
    titlePanel("Nike Vaporfly Improves Marathon Performance?"),
    
    tabsetPanel(
        tabPanel(strong("About"), fluid = TRUE,
                 img(src = "images/vaporfly1.png"),
                 h3(strong("Purpose of the APP")),
                 p("The purpose of the APP is to create a Shiny App that can be used to explore the chosen data set and 
                   implement and compare three predictive modeling fits using the supervised machine learning methods. We will 
                   utilize the functionality of the Shiny App to perform exploratory data analysis of the collected marathon 
                   athletes data and see if there is a significant effect of Nike Vaporfly shoes on marathon runners' 
                   performance. This App is user-friendly and interactive with user interface. Users are able to browse the data 
                   and select the choices they want in EDA, model building, model fitting and prediction in the App."),
                 br(),
                 h3(strong("About the Data")), 
                 p("Ever since Nike released a series of new running shoes called the Vaporfly, it has made a huge impact in 
                   the marathon running community. Several studies are done and have reported dramatic speed improvements for 
                   athletes wearing these shoes. Hence, the study collected marathon performance data from a group of 
                   athletes participating in different marathon courses over the period of 2015 to 2019. The data consists of 
                   the athletes' names, marathon courses he/she ran, the year and the date when he/she ran the marathon, 
                   whether or not they were wearing the Vaporfly series shoes when they ran the course, the gender and the 
                   age of the athletes, and most importantly, the times the athletes used to finish the specific race. The 
                   study was interested in whether the athletes made a significant performance improvement or not by wearing 
                   the Nike Vaporfly shoes."),
                 p("More about the data can be found via ",
                    a(href="https://www.researchers.one/article/2020-02-14", "this link"), #target="_blank"
                    ", \"an observational study of the effect of Nike Vaporfly running shoes on marathon performance\"."), 
                 br(),
                 h3(strong("Purpose of the tabs in the APP")), 
                    # When click on the hyperlink, hyperlink opens on the same browser, not a new window
                 h4(strong("About"), "Page"),
                 p("This tab contains the information about the purpose of the APP, the brief discussion of the data and 
                   its source as well as the purposes of each tab/page and sub-tabs/sub-pages in this APP."),
                 h4(strong("Data Exploration"), "Page"),
                 p("This page contains the numerical summaries and the graphical summaries of the data. It has a scatterplot 
                   that describes the athletes' running times from 2015 to 2019, their gender type (color of the dots) and 
                   whether or not they were wearing Vaporfly shoes for the race (shape of the dots). A histogram shows the 
                   distributions of the running time whether or not the athletes were wearing the Vaporfly shoes. A barplot 
                   shows the average runtimes of the athletes according to their gender and whether they were wearing the 
                   Vaporfly shoes or not. A summary statistics of the average running times of the athletes depending on 
                   the gender type, the year and the marathon courses."),
                 br(),
                 h4(strong("Modeling"), "Page - This tab containts three sub-tabs/sub-pages"),
                 h5(strong("* Modeling Information Tab ---"), style = "color:blue;"),
                 p("This page contains the information of the three supervised learning models used to fit the data: multiple 
                   linear regression, regression tree method and random forest method. The three modeling approaches are 
                   discussed here as well as the pros and the cons for each one of them."),
                 h5(strong("* Model Fitting Tab ---"), style = "color:blue;", "This tab contains three sub-tabs/sub-pages, one for each model"),
                 p("This page gives users three more tabs to see the modeling performance of each model fit, one tab for each 
                   model fit. All three sub-tabs provide some functionality for choosing model settings for each model, such 
                   as the values of the tuning parameters and the number of cross-validation folds. In addition, the users 
                   have the ability to choose the proportion where the data is split into the training set and the test set. 
                   This setting is in \"Multiple Linear Regression\" page under \"Model Fitting\" under \"Modeling\", and it 
                   is set for all three model fits. Since the reponse variable is continuous, RMSE, along with other fit 
                   statistics are reported for both training and test sets in each model. Other appropriate summaries about 
                   the model fits such as the variable importance plot from the random forest model and the summary() run on 
                   the multiple regression model fit are reported in their corresponding tabs as well."),
                 h5(strong("* Prediction Tab ---"), style = "color:blue;"),
                 p("The users are given choices on different model fits with different values of the predictors to predict on 
                   the average athletes finishing times given the model fits and the value of each of the predictors."),
                 br(),
                 h4(strong("Data"), "Page"),
                 p("The users can scroll through the data set and subset the data set by selecting multiple columns and rows. 
                   The users can also download and save the data set as a .csv file on their local machine if they choose."),
                 
        ),
        tabPanel(strong("Data Exploration"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("plot_type",
                                     "Plot Type:",
                                     choices = c("Boxplot", "Histogram", "Barplot"),
                                     selected = character(0)),
                         conditionalPanel(condition = "input.plot_type == 'Boxplot'",
                                          selectInput("boxp_pred", 
                                                      "Variable goes on x-axis:", 
                                                      c("Year" = "year", 
                                                        "Marathon" = "marathon"))),
                         conditionalPanel(condition = "input.plot_type == 'Histogram'",
                                          selectInput("hist_pred", 
                                                      "Variable in different colors:",
                                                      c("Vaporfly" = "vaporfly", 
                                                        "Gender" = "sex",
                                                        "Year" = "year"))),
                         conditionalPanel(condition = "input.plot_type == 'Barplot'",
                                          selectInput("barp_pred",
                                                      "Variable in lalala:",
                                                      c("Gender" = "sex",
                                                        "Year" = "year"))),
                         selectInput("summarise", 
                                     "Variables to summarize in the table:",
                                     choices = c("Gender" = "sex", "Year" = "year", "Marathon" = "marathon"),
                                     selected = character(0)),
                         h4(strong("Note: Less finishing times mean faster/better marathon performance of athletes!"),
                            style = "color:blue;")
                     ),
                                          # Show a plot of the generated distribution
                     mainPanel(
                         #plotOutput("distPlot"),
                         plotOutput("myplot"),
                         dataTableOutput("summarytable")
                     )
            
                )
        ),
        tabPanel(strong("Modeling"), fluid = TRUE, 
                 tabsetPanel(
                     tabPanel("Modeling Information", fluid = TRUE,
                              h4(strong("Multiple Linear Regression")),
                              p("Multiple linear regression is a statistical technique that uses several predictor variables, 
                                also called explanatory variables, to predict the values of a response variable. The goal of 
                                multiple linear regression is to model the \"linear\" relationship between the explanatory 
                                variables and the response variables regardless of the forms of the explanatory variables, 
                                for example, polynomial forms or interaction terms. In short, multiple linear regression is 
                                the extension of simple linear regression model because it involves more than one explanatory 
                                variable. "),
                              uiOutput("text1"),
                              h4("Advantage"),
                              p("Linear regression models are easy to understand and interpret. Furthermore, regression models 
                                can be trained efficiently with low computational power compared to other complex algorithms. They 
                                are easy to master."),
                              h4("Disadvantage"),
                              p("Multiple linear regression models assume that relationships between the explanatory variables 
                                and the response variables are \"linear\" which can be violated sometimes due to complexity of 
                                dataset. In real life scenarios, the relationship between the variables of a dataset is not 
                                linear most of the time. Therefore, a stright line will not fit the data properly. In addition, 
                                regression models are sensitive to outliers which can sway the performance of the model. 
                                Linear regression models also assume that the inputs are independent of each other. Hence, 
                                multicollinearity existing in the variables can also mask the model's performance. Thus, explanatory 
                                variables exhibit correlations with others must be removed prior model fitting."),
                              br(),
                              h4(strong("Regression Tree")),
                              p("Tree-based method is to split up predictor space into regions and make different predictions for 
                                each region. If the goal is to predict a continuous response, in our case, the average runtime of 
                                marathon runners, regression tree method is used here, otherwise, classification tree will be used (
                                for classifying memberships). Regression tree splits up the predictor space into regions and usually 
                                uses the mean of the observations in that space predictor region as prediction."), 
                              h4("Advantage"),
                              p("Using tree-based methods, we do not need to include interaction terms between the predictors because 
                                the method automatically seeks for the best value/level of each predictor to make the split and 
                                subsequent splits. Tree-based methods automatically account for interactions between the predictors.
                                They are simple and easy to understand and to interpret the output. No statistical assumptions necessary. 
                                "),
                              h4("Disadvantage"),
                              p("However, tree-based methods tend to grow a \"large\" tree (many nodes), and they are usually required 
                                to be pruned back so that they are not overfitting the data. However, pruning increases bias but decreases 
                                variance and hopefully improves overall prediction strength. This is a bias-variance trade-off. Moreover, 
                                any small changes in data can vastly change the tree. Greedy algorithm is necessary due to no optimal 
                                algorithms."),
                              br(),
                              h4(strong("Random Forest")),
                              p("Random forest model is an extension of a general bagging method that uses bootstrap aggregation which 
                                treats sample as population, resample from the data (non-parametric) (our case here) with replacement 
                                or a fitted model (parametric), produce many many tree fits from multiple samples and then average across 
                                predictions of those trees. Averaging across many tree fits can help decrease the variance over an 
                                individual tree fit because it uses bootstrapping to get multiple samples to fit on. Therefore, we can get 
                                the same observation multiple times within one sample. Instead of using all the predictors for each tree 
                                fit, random forest uses a random subset of predictors for each bootstrap sample/tree fit. 
                                "),
                              h4("Advantage"),
                              p("If a really strong predictor exists in the data, each bootstrap tree will probably use that predictor 
                                for the first split. This will make the tree predictions more correlated with one another. Correlated 
                                multiple tree fits usually mean smaller variance reduction from aggregation/averaging. However, we 
                                want large reduction in variance through averaging, and this mostly happens if the fits are independent 
                                of one another. Random forest models take care of this concern by only using a random subset of predictors 
                                rather than all of them so that the predictions are independent of each other. Hence, it guarantees the 
                                most variation reduction through averaging predictions, and it gains strength in prediction."),
                              h4("Disadvantage"),
                              p("Unfortunately, random forest models are difficult to interpret, and they require more computation time 
                                than the linear models or the single tree models.")
                     ),
                     
################################ Multiple Linear Regression ############################################################## 

                     tabPanel("Model Fitting", fluid = TRUE,
                         tabsetPanel(
                             tabPanel("Multiple Linear Rregression", fluid = TRUE,
                                 
                                 sidebarLayout(
                                     sidebarPanel(
                                         sliderInput("split",
                                                     "Proportion of data split into training set",
                                                     min = 0.45,
                                                     max = 0.85,
                                                     value = 0.70,
                                                     step = 0.01),
                                         
                                         #checkboxInput("mlr_year", "Add one more predictor - Year?"),
                                         br(),
                                         p(strong("Make sure the values of the tuning parameters and others are set for 
                                         regression tree and random forest models as well.", style = "color:red;")),
                                         br(),
                                         h4(strong("Click on \"Submit\" button below to run all 3 models")),
                                         actionButton("submit_mlr", "Submit")
                                     ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         #plotOutput("distPlot"),
                                         verbatimTextOutput("mlrfit"),
                                         dataTableOutput("mlr_rmse")
                                     )
                                     
                                 )
                             ),
                             
######################## Regression Tree ########################################################################
                             tabPanel("Regression Tree", fluid = TRUE,
                                 
                                      sidebarLayout(
                                          sidebarPanel(
                                              h5("The range of the tuning parameter set for cross-validation to run."),
                                              sliderInput("cp",
                                                          "Tuning parameter - cp",
                                                          min = 0.0008,
                                                          max = 0.0013,
                                                          value = c(0.001, 0.0013),
                                                          step = 0.00001),
                                              sliderInput("cv_fold_rt",
                                                          "Cross-Validation Fold",
                                                          min = 1,
                                                          max = 15,
                                                          value = 10)
                                              
                                          ),
                                          # Show a plot of the generated distribution
                                          mainPanel(
                                              #plotOutput("rf.varimportance"),
                                              verbatimTextOutput("regress.tree_fit"),
                                              dataTableOutput("regress.tree_rmse")
                                          )
                                          
                                      )
                                      
                             ),

############## Random Forest ################################################################################################
                             tabPanel("Random Forest", fluid = TRUE,
                                 sidebarLayout(
                                     sidebarPanel(
                                         h5(strong("Random forest model takes a little longer to run than the rest. Be patient!")),
                                         br(),
                                         h5("The range of the tuning parameter set for cross-validation to run."),
                                         sliderInput("mtry",
                                                     "Tuning parameter - mtry",
                                                     min = 3,
                                                     max = 8,
                                                     value = c(4, 6),
                                                     step = 1),
                                         sliderInput("cv_fold_rf",
                                                     "Cross-Validation Fold",
                                                     min = 1,
                                                     max = 6,
                                                     value = 5)
                                         
                                         ),
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                         plotOutput("rf.varimportance"),
                                         #verbatimTextOutput("rf.fit"),
                                         dataTableOutput("rf.rmse")
                                     )
                                          
                                 )
                                 
                             )
                         )
                     ),
############################################ Prediction #######################################################

                     tabPanel("Prediction", fluid = TRUE,
                              sidebarLayout(
                                  sidebarPanel(
                                      h4("Select the type of model for predicting time: "),
                                      radioButtons("fit_model", label = "Select the Model:", 
                                                   choices = c("Multiple Linear Regression", 
                                                               "Regression Tree", 
                                                               "Random Forest"),
                                                   selected = character(0)),
                                      uiOutput("info"),
                                      #h5("Select predictors' value to predict for marathon athletes' average finishing time:"),
                                      selectInput("pred_sex", label = "Gender",
                                                  choices = unique(shoes_data$sex), 
                                                  selected = character(0)),
                                      selectInput("pred_year", label = "Year",
                                                  choices = unique(shoes_data$year),
                                                  selected = character(0)),
                                      selectInput("pred_vaporfly", label = "Vaporfly shoes",
                                                  choices = unique(shoes_data$vaporfly),
                                                  selected = "Yes"),
                                      selectInput("pred_mara", label = "Marathon", 
                                                  choices = unique(shoes_data$marathon),
                                                  selected = "Boston Marathon"),
                                      h4(strong("Press the button below to make a prediction!"), style = "color:blue;"),
                                      actionButton("submit_model", "Predict!")
                                  ),
                                  mainPanel(
                                      #plotOutput("rf.varimportance"),
                                      #verbatimTextOutput("rf.fit"),
                                      textOutput("prediction")
                                  )
                              )
                      
                     )
                     
                     
                 )
                 
        ),
################################## Data & download Data ####################################################################
        tabPanel(strong("Data"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h4("You can subset the data set."),
                         
                         h5("You can select the ", strong("variable(s)/column(s)"), " you would like to view below:"),
                         
                         varSelectInput("variable", label = "Variables to show:", 
                                        shoes_data,
                                        multiple = TRUE),
                         #checkboxGroupInput("variable", label = "Variable(s) to show:",
                         #                   choices = colnames(shoes_data),
                         #                   selected = NULL),
                         conditionalPanel(condition = "input.variable.includes('sex')",
                                          checkboxGroupInput("select_sex", "Filter a Gender Type:",
                                                             c("Female", "Male"),
                                                             selected = c("Female", "Male"))),
                         br(),
                         h4("Save the subsetted dataset to a .csv file"),
                         actionButton("saveData", "Save the data!")
                         #downloadButton("downloadData", "Download")
                 
                     ),
                     # Show a plot and a table output of the summary statistics
                     # Connect to server rendered outputs
                     mainPanel(
                         dataTableOutput("datatable")
                     )
                 )
                 
        )

    # Sidebar with a slider input for number of bins

)))
