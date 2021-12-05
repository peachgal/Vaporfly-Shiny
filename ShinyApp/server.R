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
library(caret)
library(DT)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe) 
shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
# sum(is.na(shoes$age))
# shoes$year <- cut(shoes$year, 5, c("2015", "2016", "2017", "2018", "2019"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    plotdata <- shoe %>% filter(vaporfly != "NA")
    output$summarytable <- renderDataTable({
        
        var <- input$summarise # connect to  selectInput - internal name
        table_data <- plotdata[, c("vaporfly", "time_minutes", var), drop = FALSE]
        summ_data <- aggregate(time_minutes ~ vaporfly + table_data[[var]], data = table_data, FUN = mean)
        #summ_data <- table_data %>% group_by(vaporfly, var) %>% summarise(Average = mean(time_minutes))
        summ_data2 <- data.frame(summ_data[, -3], round(summ_data[, 3], 4))
        
        colnames(summ_data2)[2] <- var
        colnames(summ_data2)[3] <- "Average time(minutes)"
        summ_data2
        #GermanCreditSub <- plo[ , c("Class", "InstallmentRatePercentage", var), drop = FALSE]
        #tab <- aggregate(GermanCreditSub[[var]] ~ Class + InstallmentRatePercentage, data = GermanCreditSub, FUN = mean)
        #tab
        #tab2 <- data.frame(tab[, -3], round(tab[, 3], input$rounding))
        #colnames(tab2)[3] <- paste0("Average ", var)
        #tab2
        
    })
    

    
    output$myplot <- renderPlot({
        
        plotdata <- shoe %>% filter(vaporfly != "NA")
        # Making three different bar-plots
        scatter <- ggplot(data = plotdata, aes(x = year, y = time_minutes, color = sex, shape = vaporfly)) + 
            geom_point(size = 2, position = "jitter") + 
            #scale_shape_discrete(name = "") + 
            #coord_cartesian(xlim=c(0, 300000), ylim=c(0, 7500)) +
            #geom_smooth(method = lm, lwd = 2) + 
            guides(color = guide_legend(override.aes = list(size = 8))) + 
            labs(x = "Year", y = "Time (minutes)", title = "Figure 1. Marathon runners' runtimes across years") + 
            theme(axis.text.x = element_text(size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13))
        
        # draw the histogram with the specified number of bins
        second <- ggplot(data = plotdata, aes(x = time_minutes))
        second <- ggplot(data = plotdata, aes(x = time_minutes, color = sex, fill = vaporfly)) +
            geom_histogram() + 
            #coord_cartesian(xlim=c(0, 5000)) + 
            labs(x = "Time (minutes)", title = "Figure 2. Number of deaths in US for each vaccine timeline") + 
            theme(axis.text.x = element_text(size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13)) + 
            facet_wrap(~vaporfly) 
        
        sum_data <- plotdata %>% group_by(marathon, sex, vaporfly) %>% 
            summarise(Average = mean(time_minutes))
        
        third <- ggplot(data = sum_data, aes(x = sex, y = Average, fill = vaporfly)) + 
            geom_bar(stat = "identity", position = "dodge") + 
            labs(x = "Gender", y = "Average Time (minutes)", 
                 title = "Figure 3. ") + 
            scale_fill_discrete(name = "Vaporfly", labels = c("No", "Yes")) + 
            theme(axis.text.x = element_text(size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 13), 
                  axis.title.y = element_text(size = 13), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13)) + 
            coord_flip()
        
        # generate different barplots based on input$barplots_3 from ui.R in radioButtons
        if(input$plot_type == "Scatterplot"){
            
            scatter
        } else if(input$plot_type == "Histogram"){
            
            second
        } else {
            
            third
        }
        
    })
    shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
    
    mlr_data <- reactiveValues({
        set.seed(388588)
        newData <- shoes_data %>% dplyr::select(!!!input$mlr_x, time_minutes)
        vaporfly_index <- createDataPartition(newData$vaporfly, p = input$split, list = FALSE)
        train <- newData[vaporfly_index, ]
        test <- newData[-vaporfly_index, ]
        
        mlr_fit <- train(time_minutes ~ . , 
                         data=train,
                         method = "lm",
                         trControl = trainControl(method = "cv", number = 10),
                         preProcess = c("center", "scale"))
        mlr_summ <- summary(mlr_fit)
        test_pred_mlr <- predict(mlr_fit, newdata = mlr_data$test)
        train_pred_mlr <- predict(cv_fit1, newdata = mlr_data$train)
        train_rmse_mlr <- postResample(train_pred_mlr, obs = mlr_data$train$time_minutes)
        test_rmse_mlr <- postResample(test_pred_mlr, obs = mlr_data$test$time_minutes)
    })
    
    
    output$mlrfit <- renderPrint({
        
        #set.seed(388588)
        #shoes_data <- shoe %>% filter(vaporfly != "NA")
        # sum(is.na(shoes$age))
        # shoes$year <- cut(shoes$year, 5, c("2015", "2016", "2017", "2018", "2019"))
         #dummies <- dummyVars( ~ vaporfly + sex + year + marathon, data = shoes_data)
         # dummy_var <- predict(dummies, newdata = shoes_data)
         #as_tibble(dummy_var)
         #shoes_all <- cbind(dummy_var, shoes_data)
         # shoes_all
        
        #preProcValues <- preProcess(train, method = c("center", "scale"))
        #trainTransformed <- predict(preProcValues, train)
        #testTransformed <- predict(preProcValues, test)
        #trainTrans1 <- trainTransformed %>% select(match_name, marathon, year, time_minutes, vaporfly, sex, age)
        #testTrans1 <- testTransformed %>% select(match_name, marathon, year, time_minutes, vaporfly, sex, age)
        #trainTrans1
        #mlr_fit <- train(time_minutes ~ . , 
        #                 data=mlr_data$train,
        #                 method = "lm",
        #                 trControl = trainControl(method = "cv", number = 10),
        #                 preProcess = c("center", "scale"))
        mlr_data$mlr_summ
        
        #test_pred_mlr <- predict(mlr_fit, newdata = mlr_data$test)
        #train_pred_mlr <- predict(cv_fit1, newdata = mlr_data$train)
        #train_rmse_mlr <- postResample(train_pred_mlr, obs = mlr_data$train$time_minutes)
        #test_rmse_mlr <- postResample(test_pred_mlr, obs = mlr_data$test$time_minutes)
        #train_rmse_mlr
        #test_rmse_mlr
        
    })
    
    getData <- reactive({
        
        if(length(input$variable) == 0) {
            return(shoe)
            } else if(input$era){ 
                
                newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(year == input$era)
                newData
                    
            } #else if(input$era) {
                    
                    #newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(year == input$era)
                    #newData
                #} else if(input$gender & input$era) {
                    
                    #newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(sex == input$gender, year == input$era)
                    #newData
                #} 
              else {
                    
                    newData <- shoe %>% dplyr::select(!!!input$variable)
                    newData
                }
            
    })
    output$datatable <- renderDataTable({
        
        shoeData <- getData()
        shoeData
    })
    
    #observe(input$downloadData, {print(head(shoe))})
    #observeEvent(input$downloadData, {
        
    #    saveData(shoe)
    #})
    #output$downloadData <- downloadHandler({
        
    #    filename = function(){
    #        "vaporfly_data.csv"
    #    }
    #    content = function(file="vaporfly_data.csv"){ 
    #        write.csv(shoe, file, row.names = FALSE)
    #    }
    #})
})
