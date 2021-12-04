#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    getData <- reactive({
        
        if(length(input$variable) == 0) {
            return(shoe)
            } else { 
                
                if(input$gender & input$sub_gender) {
                
                    newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(sex == input$gender)
                    newData
                    
                } else if(input$era) {
                    
                    newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(year == input$era)
                    newData
                } else if(input$gender & input$era) {
                    
                    newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(sex == input$gender, year == input$era)
                    newData
                } else {
                    
                    newData <- shoe %>% dplyr::select(!!!input$variable)
                    newData
                }
            }
    })
    output$datatable <- renderDataTable({
        
        shoeData <- getData()
        shoeData
    })
    
    output$downloadData <- downloadHandler({
        
        filename = function(){
            "vaporfly_data.csv"
        }
        content = function(file){ 
            write.csv(getData(), file, row.names = FALSE)
        }
    })
})
