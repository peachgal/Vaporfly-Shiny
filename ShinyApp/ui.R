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
#library(shinydashboard)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe)
shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
predictors <- c("marathon", "year", "vaporfly", "sex")
shoes_data <- data.frame(shoes_data)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(

    # Application title
    titlePanel("Nike Vaporfly Improves Runtime?"),
    
    tabsetPanel(
        tabPanel(strong("About"), fluid = TRUE,
                 img(src = "images/vaporfly1.png"),
                 h4(strong("Purpose of the APP")),
                 p("Describe the purpose of the APP"),
                 br(),
                 h4(strong("About the Data")), 
                 p("Briefly discuss the data and "),
                 p("You can find the source of the data via ",
                    a(href="https://www.researchers.one/article/2020-02-14", "this link"), #target="_blank"
                    ", to the observation study about Nike Vaporfly running shoes"), 
                 br(),
                 h4(strong("Purpose of Pages of the APP")), 
                 h4("Tell the user the purpose of each tab (page) of the app "), 
                    # When click on the hyperlink, hyperlink opens on the same browser, not a new window
                 br(),
                 h5("You can create a few bar plots using the radio buttons below."),
                 
                 # Default is the first one keyed in, "Just Classification"
                 radioButtons("barplots_3", label = "Select the Plot Type", 
                              choices = c("Just Classification", "Classification and Unemployed", "Classification and Foreign")),
                 # line break
                 br(),
                 # "sample mean" is BOLD font
                 h5("You can find the ", strong("sample mean"), " for a few variables below:")
                 
        ),
        tabPanel(strong("Data Exploration"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30),
                         radioButtons("plot_type",
                                     "Plot Type:",
                                     choices = c("Scatterplot", "Histogram", "Barplot"),
                                     selected = "Scatterplot"),
                         selectInput("summarise", 
                                     "Variables to summarize",
                                     choices = c("sex", "year", "marathon"),
                                     selected = character(0))
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
                              
                              h4("Advantage"),
                              
                              h4("Disadvantage"),
                              
                              h4(strong("Regression Tree")),
                              
                              h4("Advantage"),
                              
                              h4("Disadvantage"),
                              
                              h4(strong("Random Forest")),
                              
                              h4("Advantage"),
                              
                              h4("Disadvantage"),
                     ),
                     tabPanel("Model Fitting", fluid = TRUE,
                         tabsetPanel(
                             tabPanel("Multiple Linear Rregression", fluid = TRUE,
                                 
                                 sidebarLayout(
                                     sidebarPanel(
                                         sliderInput("split",
                                                     "Proportion of data used in training set",
                                                     min = 0.50,
                                                     max = 0.80,
                                                     value = 0.70,
                                                     step = 0.01),
                                         #radioButtons("plot_type",
                                         #             "Plot Type:",
                                         #             choices = c("Scatterplot", "Histogram", "Barplot"),
                                         #             selected = "Scatterplot"),
                                         #selectInput("mlr_x", 
                                         #            "Select the predictors:",
                                         #            choices = as.list(shoes_data[-4]), #c("vaporfly", "marathon", "year", "sex"),
                                         #            selected = shoes_data[3],
                                         #            multiple = TRUE),
                                         #varSelectInput("mlr_x", label = "Select the predictors:", 
                                         #               choices = as.list(shoes_data[-4]),
                                         #               selected = shoes_data[3],
                                         #               multiple = TRUE),
                                         checkboxInput("mlr_year", "Add one more predictor - Year?"),
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
                             tabPanel("Regression Tree", fluid = TRUE
                                 
                             ),
                             tabPanel("Random Forest", fluid = TRUE,
                                 sidebarLayout(
                                     sidebarPanel(
                                         sliderInput("mtry",
                                                     "Tuning parameter - mtry",
                                                     min = 3,
                                                     max = 8,
                                                     value = c(4, 6),
                                                     step = 1),
                                         # checkboxInput("mlr_year", "Add one more predictor - Year?"),
                                         actionButton("submit_rf", "Submit")
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
                     tabPanel("Prediction", fluid = TRUE
                      
                     )
                     
                     
                 )
                 
        ),
        tabPanel(strong("Data"), fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h4("Scroll through the data set "), 
                            # When click on the hyperlink, hyperlink opens on the same browser, not a new window
                         br(),
                         h5("You can subset the data set (rows and columns)."),
                         
                         # Default is the first one keyed in, "Just Classification"
                         radioButtons("barplots_3", label = "Select the Plot Type", 
                                      choices = c("Just Classification", "Classification and Unemployed", "Classification and Foreign")),
                         # line break
                         # "sample mean" is BOLD font
                         h5("You can select the ", strong("variable(s)/column(s)"), " you would like to view below:"),
                         
                         # Create a drop-down bar, default is age
                         varSelectInput("variable", label = "Variables to show:", 
                                        shoe,
                                        multiple = TRUE),
                         h5("You can filter the ", strong("observations/rows"), " for the subsetted data by inputting 
                            the syntax below:"),
                         #checkboxInput("gender", label = "If you want to subset based on gender type:"),
                         #conditionalPanel("input.gender", 
                         #                 checkboxInput("sub_gender", "Male"))),
                         #submitButton(text = "submit"),
                         numericInput("era", "Year", value = NULL, min = 2015, max = 2019),
                         h4("Downloading (subsetted) Data"),
                         actionButton("downloadData", "Download")
                 
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
