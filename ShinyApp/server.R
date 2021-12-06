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
library(vip)
library(rmarkdown)

shoe <- read_csv(file = "sampled_shoe.csv", col_names = TRUE)
shoe <- data.frame(shoe) 
shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
shoes_data$year <- cut(shoes_data$year, 5, c("2015", "2016", "2017", "2018", "2019"))
shoes_data$vaporfly <- as.numeric(shoes_data$vaporfly)
shoes_data$vaporfly <- cut(shoes_data$vaporfly, 2, c("No", "Yes"))
# sum(is.na(shoes$age))
# shoes$year <- cut(shoes$year, 5, c("2015", "2016", "2017", "2018", "2019"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

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
        #second <- ggplot(data = plotdata, aes(x = time_minutes))
        second <- ggplot(data = plotdata, aes(x = time_minutes, fill = vaporfly)) + #color = sex
            geom_histogram() + 
            #coord_cartesian(xlim=c(0, 5000)) + 
            labs(x = "Time (minutes)", title = "Figure 2. Runtimes versus Vaporfly") + 
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
                 title = "Figure 3. Average runtime for athletes wearing Vaporfly or not") + 
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
    
    output$text1 <- renderUI({
        
        withMathJax(
            helpText('The general form is
                     $$y=\\beta_0+\\beta_1{x_1}+\\beta_2{x_2}+\\beta_3{x_2^2}+\\beta_4{x_1}{x_2^2}+...+\\epsilon$$')
        )
    })
######################## Multiple Linear Regression ###########################################
    
     #shoes_data <- shoe %>% filter(vaporfly != "NA") %>% select(marathon, year, vaporfly, time_minutes, sex)
     # <- reactive({
     #  newData <- shoes_data %>% select(input$mlr_x, time_minutes)})
    #mlr_data <- reactive({ #input$submit_mlr,
    #    set.seed(388588)
        
        #if(input$mlr_year){
        #    
        #    newData <- shoes_data
        #    newData
        #    
        #} else {
        #    
        #    newData <- shoes_data %>% select(-year)
        #    newData
        #}
        
        #newData <- shoes_data %>% select(!!!input$mlr_x, time_minutes) 
    
        #newData <- mlr_data()$newData
        
        #vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = input$split, list = FALSE)
        #train <- shoes_data[vaporfly_index, ]
        #test <- shoes_data[-vaporfly_index, ]
    
        #mlr_fit <- train(time_minutes ~ . , 
        #                 data=train,
        #                 method = "lm",
        #                 trControl = trainControl(method = "cv", number = 10),
        #                 preProcess = c("center", "scale"))
        #mlr_summ <- summary(mlr_fit)
        #test_pred_mlr <- predict(mlr_fit, newdata = mlr_data$test)
        #train_pred_mlr <- predict(cv_fit1, newdata = mlr_data$train)
        #train_rmse_mlr <- postResample(train_pred_mlr, obs = mlr_data$train$time_minutes)
        #test_rmse_mlr <- postResample(test_pred_mlr, obs = mlr_data$test$time_minutes)
        #value <- list(mlr_summ, train_rmse_mlr, test_pred_mlr)
    #})
    #observeEvent(input$submit_mlr, {})
    data_split <- eventReactive(input$submit_mlr, {
        
        input$split
        
    })
    output$mlrfit <- renderPrint({
        
        set.seed(388588)
        
        #if(input$mlr_year){
        #    
        #    newData <- shoes_data
        #    newData
        #    
        #} else {
        #    
        #    newData <- shoes_data %>% select(-year)
        #    newData
        #}
        
        #newData <- shoes_data %>% select(!!!input$mlr_x, time_minutes) 
        
        #newData <- mlr_data()$newData
        
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        mlr_fit <- train(time_minutes ~ . , 
                         data=train,
                         method = "lm",
                         trControl = trainControl(method = "cv", number = 10),
                         preProcess = c("center", "scale"))
        summary(mlr_fit)
        #test_pred_mlr <- predict(mlr_fit, newdata = test)
        #train_pred_mlr <- predict(mlr_fit, newdata = train)
        #train_rmse_mlr <- postResample(train_pred_mlr, obs = train$time_minutes)
        #test_rmse_mlr <- postResample(test_pred_mlr, obs = test$time_minutes)
        #train_pred_mlr
        #test_rmse_mlr
        #mlr_data()$mlr_summ
        #newData <- mlr_data()$newData
        
        #vaporfly_index <- createDataPartition(newData$vaporfly, p = input$split, list = FALSE)
        #train <- newData[vaporfly_index, ]
        #test <- newData[-vaporfly_index, ]
        
        #mlr_fit <- train(time_minutes ~ . , 
        #                 data=train,
        #                 method = "lm",
        #                 trControl = trainControl(method = "cv", number = 10),
        #                 preProcess = c("center", "scale"))
        #summary(mlr_fit)
        #value[1]
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
        #mlr_data$mlr_summ
        
        #test_pred_mlr <- predict(mlr_fit, newdata = mlr_data$test)
        #train_pred_mlr <- predict(cv_fit1, newdata = mlr_data$train)
        #train_rmse_mlr <- postResample(train_pred_mlr, obs = mlr_data$train$time_minutes)
        #test_rmse_mlr <- postResample(test_pred_mlr, obs = mlr_data$test$time_minutes)
        #train_rmse_mlr
        #test_rmse_mlr
        
    })
    output$mlr_rmse <- renderDataTable({
        
        set.seed(388588)
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        mlr_fit <- train(time_minutes ~ . , 
                         data=train,
                         method = "lm",
                         trControl = trainControl(method = "cv", number = 10),
                         preProcess = c("center", "scale"))
        
        test_pred_mlr <- predict(mlr_fit, newdata = test)
        train_pred_mlr <- predict(mlr_fit, newdata = train)
        train_rmse_mlr <- postResample(train_pred_mlr, obs = train$time_minutes)
        test_rmse_mlr <- postResample(test_pred_mlr, obs = test$time_minutes)
        tablala <- rbind(train_rmse_mlr, test_rmse_mlr)
        row.names(tablala) <- c("Training set", "Test set")
        round(tablala, 4) 
        
    })
    
####################### Regression Tree ###################################################    
    
    output$regress.tree_fit <- renderPrint({
        
        set.seed(388588)
        
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        regress_tree <- train(time_minutes ~ . , 
                              data = train, 
                              method = "rpart", 
                              trControl = trainControl(method = "cv", number = 10),
                              preProcess = c("center", "scale"),
                              #tuneLength = 30)
                              tuneGrid = data.frame(cp = seq(from = input$cp[1], to = input$cp[2], by = 0.00001)))
        regress_tree

    })
    
    output$regress.tree_rmse <- renderDataTable({
        
        set.seed(388588)
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        regress_tree <- train(time_minutes ~ . , 
                              data = train, 
                              method = "rpart", 
                              trControl = trainControl(method = "cv", number = 10),
                              preProcess = c("center", "scale"),
                              #tuneLength = 30)
                              tuneGrid = data.frame(cp = seq(from = input$cp[1], to = input$cp[2], by = 0.00001)))
        
        pred_reg.tree.train <- predict(regress_tree, newdata = train)
        pred_reg.tree <- predict(regress_tree, newdata = test)
        reg.tree.train_rmse <- postResample(pred_reg.tree.train, obs = train$time_minutes)
        reg.tree.test_rmse <- postResample(pred_reg.tree, obs = test$time_minutes)
        reg.tree_table <- rbind(reg.tree.train_rmse, reg.tree.test_rmse)
        row.names(reg.tree_table) <- c("Training set", "Test set")
        round(reg.tree_table, 4)
    })
    
    
    
    
####################### Random Forest #####################################################    
    
    output$rf.varimportance <- renderPlot({
        
        set.seed(388588)
        
        
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        random_f <- train(time_minutes ~ . , data = train,
                          method = "rf",
                          trControl = trainControl(method = "cv", number = 5),
                          preProcess = c("center", "scale"),
                          tuneGrid = data.frame(mtry = input$mtry[1]:input$mtry[2]))

        vip(random_f)

        
    })
    output$rf.rmse <- renderDataTable({
        
        set.seed(388588)
        

        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        random_f <- train(time_minutes ~ . , data = train,
                          method = "rf",
                          trControl = trainControl(method = "cv", number = 5),
                          #preProcess = c("center", "scale"),
                          tuneGrid = data.frame(mtry = input$mtry[1]:input$mtry[2]))
        
        train_pred_rf <- predict(random_f, newdata = train)
        test_pred_rf <- predict(random_f, newdata = test)
        train_rf_rmse <- postResample(train_pred_rf, obs = train$time_minutes)
        test_rf_rmse <- postResample(test_pred_rf, obs = test$time_minutes)
        rf_table <- rbind(train_rf_rmse, test_rf_rmse)
        row.names(rf_table) <- c("Training set", "Test set")
        round(rf_table, 4)

    })
    
############################## Prediction ##################################################################    
    output$prediction <- renderText( {
        
        shoes_data
        if(input$fit_model == "Random Forest"){
            
            pred_fit <- train(time_minutes ~ . , data = shoes_data,
                              method = "rf",
                              trControl = trainControl(method = "cv", number = 5), #"repeatedcv", number = 5, repeats = 3
                              #preProcess = c("center", "scale"),
                              tuneGrid = data.frame(mtry = 3:8))
            pred_fit
            
        } else if(input$fit_model == "Regression Tree"){
            
            pred_fit <- train(time_minutes ~ . , data = shoes_data, 
                                  method = "rpart", 
                                  trControl = trainControl(method = "cv", number = 10),
                                  #preProcess = c("center", "scale"),
                                  tuneGrid = data.frame(cp = seq(from = 0.0010, to = 0.0013, by = 0.00001)))
            pred_fit
            
        } else {
            
            pred_fit <- train(time_minutes ~ vaporfly + sex + marathon + year, 
                             data=shoes_data,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = 10))
            pred_fit
            
        }
        
        temp <- predict(pred_fit, 
                        newdata = data.frame(sex = input$pred_sex, 
                                             vaporfly = input$pred_vaporfly, 
                                             year = input$pred_year, 
                                             marathon = input$pred_mara), 
                        se.fit = TRUE)
        paste("The average running time of the marathon runner is", round(temp, 2), "minutes.", sep = " ")
        
    })
################################ DATA page ##########################################################    
    getData <- reactive( {
        
        if(length(input$variable) == 0) {
            return(shoe)
            } else { 
                
                newData <- shoe %>% dplyr::select(!!!input$variable)
                newData
                    
            } #else if(input$era) {
                    
                    #newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(year == input$era)
                    #newData
                #} else if(input$gender & input$era) {
                    
                    #newData <- shoe %>% dplyr::select(!!!input$variable) %>% filter(sex == input$gender, year == input$era)
                    #newData
                #} 
             # else {
                    
             #       newData <- shoe %>% dplyr::select(!!!input$variable)
             #       newData
             #   }
            
    })
    output$datatable <- renderDataTable({
        
        shoeData <- getData()
        shoeData
    })
    
    observeEvent(input$saveData, {
        write.csv(getData(), "vaporfly_dataset.csv", row.names = FALSE)
        
        
    })
    #observe(input$downloadData, {print(head(shoe))})
    # <- reactive({
        
    #    saveData(shoe)
    #})
    
    #output$downloadData <- downloadHandler({
        
    #    filename = function(){
    #        paste0("vaporfly_dataset", ".csv")
    #    }
    #    content = function(file){ 
    #        write.csv(getData(), file, row.names = FALSE)
    #    }
    #})
})
