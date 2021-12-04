#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe)
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
                                     value = 30)
                     ),
                                          # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot")
                     )
            
                )
        ),
        tabPanel(strong("Modeling"), fluid = TRUE, 
                 tabsetPanel(
                     tabPanel("Modeling Information", fluid = TRUE
                              
                     ),
                     tabPanel("Model Fitting", fluid = TRUE
                     
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
                         checkboxInput("gender", label = "If you want to subset based on gender type:"),
                         conditionalPanel("input.gender", 
                                          checkboxGroupInput("sub_gender", "Gender Type", choices = c("Female", "Male"))),
                         submitButton(text = "submit"),
                         h4("Downloading (subsetted) Data"),
                         downloadButton("downloadData", "Download")
                 
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
