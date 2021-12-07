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
library(ggplot2)
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

    output$summarytable <- renderDataTable({
        
        var <- input$summarise # connect to  selectInput - internal name
        table_data <- shoes_data[, c("vaporfly", "time_minutes", var), drop = FALSE]
        summ_data <- aggregate(time_minutes ~ vaporfly + table_data[[var]], data = table_data, FUN = mean)
        #summ_data <- table_data %>% group_by(vaporfly, var) %>% summarise(Average = mean(time_minutes))
        summ_data2 <- data.frame(summ_data[, -3], round(summ_data[, 3], 4))
        
        colnames(summ_data2)[2] <- var
        colnames(summ_data2)[3] <- "Average time (minutes)"
        summ_data2

    })
################################# PLOT ###################################################################################
    output$myplot <- renderPlot({
        
        box_plot <- ggplot(data = shoes_data, aes(x = !!sym(input$boxp_pred), y = time_minutes)) + 
            geom_boxplot(fill = "white") + 
            geom_jitter(aes(color = sex, shape = vaporfly), size = 2) + 
            guides(color = guide_legend(override.aes = list(size = 8)), 
                   shape = guide_legend(override.aes = list(size = 8))) + 
            labs(x = input$boxp_pred, y = "Time (minutes)", title = "Figure 1. Athletes' Marathon Finishing Times") + 
            theme(axis.text.x = element_text(angle = 45, size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13))
        
        # draw the histogram with different colors specified
        hist_gram <- ggplot(data = shoes_data, aes(x = time_minutes, fill = !!sym(input$hist_pred))) + #color = sex
            geom_histogram() + 
            #coord_cartesian(xlim=c(0, 5000)) + 
            labs(x = "Time (minutes)", title = "Figure 2. Athletes' Marathon Finishing Times") + 
            theme(axis.text.x = element_text(size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13)) + 
            facet_wrap(~ vaporfly, labeller = label_both) # try aes_string(facet_wrap(~ input$hist_pred))
        
        sum_data <- shoes_data %>% group_by(year, sex, vaporfly) %>% 
            summarise(Average = mean(time_minutes))
        
        var <- input$barp_pred # connect to  selectInput - internal name
        table_data <- shoes_data[, c("vaporfly", "time_minutes", var), drop = FALSE]
        summ_data <- aggregate(time_minutes ~ vaporfly + table_data[[var]], data = table_data, FUN = mean)
        #summ_data <- table_data %>% group_by(vaporfly, var) %>% summarise(Average = mean(time_minutes))
        #summ_data2 <- data.frame(summ_data[, -3], round(summ_data[, 3], 4))
        
        colnames(summ_data)[2] <- var
        colnames(summ_data)[3] <- "Average time (minutes)"
        
        
        bar_plot <- ggplot(data = summ_data, aes(x = summ_data[[2]], y = summ_data[[3]], fill = summ_data[[1]])) + 
            geom_bar(stat = "identity", position = "dodge") + 
            labs(x = var, y = "Average Time (minutes)", 
                 title = "Figure 3. Average finishing time for athletes wearing Vaporfly or not") + 
            scale_fill_discrete(name = "Vaporfly", labels = c("No", "Yes")) + 
            theme(axis.text.x = element_text(size = 10), 
                  axis.text.y = element_text(size = 10), 
                  axis.title.x = element_text(size = 13), 
                  axis.title.y = element_text(size = 13), 
                  legend.key.size = unit(1, 'cm'), 
                  legend.text = element_text(size = 13), 
                  title = element_text(size = 13)) + 
            coord_flip()
        
        # generate different plots based on input$plot_type from ui.R in radioButtons
        if(input$plot_type == "Boxplot"){
            
            box_plot
        } else if(input$plot_type == "Histogram"){
            
            hist_gram
        } else {
            
            bar_plot
        }
        
    })
    
    output$text1 <- renderUI({
        
        withMathJax(
            helpText('The general form is
                     $$y=\\beta_0+\\beta_1{x_1}+\\beta_2{x_2}+\\beta_3{x_2^2}+\\beta_4{x_1}{x_2^2}+...+\\epsilon$$')
        )
    })
######################## Multiple Linear Regression ###########################################
    
    #observeEvent(input$submit_mlr, {})
    data_split <- eventReactive(input$submit_mlr, {
        
        input$split

    })
    interact_term <- eventReactive(input$submit_mlr, {
        
        input$interact
    })
    tune_cp <- eventReactive(input$submit_mlr, {
        
        input$cp
    })
    tune_cv <- eventReactive(input$submit_mlr, {
        
        input$cv_fold_rt
    })
    tune_mtry <- eventReactive(input$submit_mlr, {
        
        input$mtry
    })
    tune_cv_rf <- eventReactive(input$submit_mlr, {
        
        input$cv_fold_rf
    })
    
    output$mlrfit <- renderPrint({
        
        set.seed(388588)
        
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        if(input$inter_act & interact_term() == "Vaporfly & Gender") {
            
            mlr_fit <- train(time_minutes ~ . + vaporfly:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            summary(mlr_fit)
            
        } else if(input$inter_act & interact_term() == "Marathon & Gender") {
            
            mlr_fit <- train(time_minutes ~ . + marathon:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            summary(mlr_fit)
            
        } else if(input$inter_act & interact_term() == "Both") {
            
            mlr_fit <- train(time_minutes ~ . + vaporfly:sex + marathon:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            summary(mlr_fit)
            
        } else { #length(interact_term()) == 0 or is.null(interact_term())
            
            mlr_fit <- train(time_minutes ~ . , 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            summary(mlr_fit)
        }
        
        #mlr_fit <- train(time_minutes ~ . , 
        #                 data=train,
        #                 method = "lm",
        #                 trControl = trainControl(method = "cv", number = tune_cv()),
        #                 preProcess = c("center", "scale"))
        #summary(mlr_fit)

    })
    `%!in%` <- Negate(`%in%`)
    
    output$mlr_rmse <- renderDataTable({
        
        set.seed(388588)
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        if(length(interact_term()) == 1 & interact_term() == "Vaporfly & Gender") {
            
            mlr_fit <- train(time_minutes ~ . + vaporfly:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            mlr_fit
            
        } else if(length(interact_term()) == 1 & interact_term() == "Marathon & Gender") {
            
            mlr_fit <- train(time_minutes ~ . + marathon:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            mlr_fit
            
        } else if(length(interact_term()) == 2) {
            
            mlr_fit <- train(time_minutes ~ . + vaporfly:sex + marathon:sex, 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            mlr_fit
            
        } else if(length(interact_term()) == 0) {
            
            mlr_fit <- train(time_minutes ~ . , 
                             data=train,
                             method = "lm",
                             trControl = trainControl(method = "cv", number = tune_cv()),
                             preProcess = c("center", "scale"))
            mlr_fit
        }
        
        #mlr_fit <- train(time_minutes ~ . , 
        #                 data=train,
        #                 method = "lm",
        #                 trControl = trainControl(method = "cv", number = tune_cv()),
        #                 preProcess = c("center", "scale"))
        
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
                              trControl = trainControl(method = "cv", number = tune_cv()),
                              preProcess = c("center", "scale"),
                              #tuneLength = 30)
                              tuneGrid = data.frame(cp = seq(from = tune_cp()[1], to = tune_cp()[2], by = 0.00001)))
        regress_tree

    })
    #input$cp[1], input$cp[2]
    output$regress.tree_rmse <- renderDataTable({
        
        set.seed(388588)
        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        regress_tree <- train(time_minutes ~ . , 
                              data = train, 
                              method = "rpart", 
                              trControl = trainControl(method = "cv", number = tune_cv()),
                              preProcess = c("center", "scale"),
                              #tuneLength = 30)
                              tuneGrid = data.frame(cp = seq(from = tune_cp()[1], to = tune_cp()[2], by = 0.00001)))
        
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
                          trControl = trainControl(method = "cv", number = tune_cv_rf()),
                          preProcess = c("center", "scale"),
                          tuneGrid = data.frame(mtry = tune_mtry()[1]:tune_mtry()[2]))

        vip(random_f)

        #input$mtry[1]:input$mtry[2]
    })
    output$rf.rmse <- renderDataTable({
        
        set.seed(388588)
        

        vaporfly_index <- createDataPartition(shoes_data$vaporfly, p = data_split(), list = FALSE)
        train <- shoes_data[vaporfly_index, ]
        test <- shoes_data[-vaporfly_index, ]
        
        random_f <- train(time_minutes ~ . , data = train,
                          method = "rf",
                          trControl = trainControl(method = "cv", number = tune_cv_rf()),
                          #preProcess = c("center", "scale"),
                          tuneGrid = data.frame(mtry = tune_mtry()[1]:tune_mtry()[2]))
        
        train_pred_rf <- predict(random_f, newdata = train)
        test_pred_rf <- predict(random_f, newdata = test)
        train_rf_rmse <- postResample(train_pred_rf, obs = train$time_minutes)
        test_rf_rmse <- postResample(test_pred_rf, obs = test$time_minutes)
        rf_table <- rbind(train_rf_rmse, test_rf_rmse)
        row.names(rf_table) <- c("Training set", "Test set")
        round(rf_table, 4)

    })
    
############################## Prediction ##################################################################    
    which_model <- eventReactive(input$submit_model, {
        
        input$fit_model
        
    })
    pred_var_sex <- eventReactive(input$submit_model, {
        
        input$pred_sex
    })
    pred_var_year <- eventReactive(input$submit_model, {
        
        input$pred_year
    })
    pred_var_vapor <- eventReactive(input$submit_model, {
        
        input$pred_vaporfly
    })
    pred_var_mara <- eventReactive(input$submit_model, {
        
        input$pred_mara
    })
    
    output$prediction <- renderPrint( {
        
        shoes_data
        if(which_model() == "Random Forest"){
            
            pred_fit <- train(time_minutes ~ . , data = shoes_data,
                              method = "rf",
                              trControl = trainControl(method = "cv", number = 5), #"repeatedcv", number = 5, repeats = 3
                              #preProcess = c("center", "scale"),
                              tuneGrid = data.frame(mtry = 3:8))
            pred_fit
            
        } else if(which_model() == "Regression Tree"){
            
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
                        newdata = data.frame(sex = pred_var_sex(), 
                                             vaporfly = pred_var_vapor(), 
                                             year = pred_var_year(), 
                                             marathon = pred_var_mara()), 
                        se.fit = TRUE)
        paste("The average finishing time of such an athlete is approximately ", round(temp, 2), "minutes.", sep = " ")
        
        
    })
    output$info <- renderUI({
        
        text <- paste0("You are making a prediction on the performance of a marathon athlete who is a ", input$pred_sex, 
                       ", wore ", input$pred_vaporfly, " Vaporfly-series running shoes and ran ", input$pred_mara, 
                       " course in ", input$pred_year, ".")
        h4(strong(text))
    })
################################ DATA page ##########################################################    
    getData <- reactive( {
        
        if(length(input$variable) == 0 ) {
            return(shoes_data)
            } else if(length(input$select_sex) == 1) { 
                
                newData <- shoes_data %>% dplyr::select(!!!input$variable) %>% filter(sex == input$select_sex)
                newData
                    
            } else {
                
                newData <- shoes_data %>% dplyr::select(!!!input$variable)
                newData
            }
            
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
