
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    clean_df <- reactive({
    population_dfnew <- population_df
    
    cRatio <- apply(X = population_dfnew, MARGIN = 2, FUN = pMiss)
    
    population_dfnew <- population_dfnew[,cRatio < input$threshold_1]
    
    rRatio <- apply(X = population_dfnew, MARGIN = 1, FUN = pMiss)
    
    population_dfnew <- population_dfnew[rRatio < input$threshold_2,]
    })
    
    train_df <- reactive({
        
        set.seed(seed=input$seed1)
        #subIndex <- caret::createDataPartition(y = clean_df()$DEATHRATE, p = input$train_p, list = FALSE)
        subIndex <- sample(1:nrow(clean_df()), input$train_p*nrow(clean_df()))
        train <- clean_df()[subIndex,]
    })
    
    test_df <- reactive({
        set.seed(seed=input$seed1)
        # subIndex <- caret::createDataPartition(y = clean_df()$DEATHRATE, p = input$train_p, list = FALSE)
        subIndex <- sample(1:nrow(clean_df()), input$train_p*nrow(clean_df()))
        test <- clean_df()[-subIndex,]
    })
    
    rec_0 <- reactive({
        recipes::recipe(DEATHRATE ~., data = train_df()) %>%
        step_rm(COUNTRY) %>% #id is not a predictor
        step_naomit(DEATHRATE, skip=TRUE) %>%
        step_knnimpute(all_predictors(), neighbours = 5) %>%
        step_dummy(all_nominal())
    })
    
    rec <- reactive({
        recipes::recipe(DEATHRATE ~., data = train_df()) %>%
        step_rm(COUNTRY) %>% #id is not a predictor
        step_naomit(DEATHRATE, skip=TRUE) %>%
        step_knnimpute(all_predictors(), neighbours = 5) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_nominal())
    })
    
    rec_1 <- reactive({
        recipes::recipe(DEATHRATE ~., data = train_df()) %>%
        step_rm(COUNTRY) %>% #id is not a predictor
        step_naomit(DEATHRATE, skip=TRUE) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_knnimpute(all_predictors(), neighbours = 5) %>%
        step_dummy(all_nominal())
    })
    
    rec_2 <- reactive({
        recipes::recipe(DEATHRATE ~., data = train_df()) %>%
        step_rm(COUNTRY) %>% #id is not a predictor
        step_naomit(DEATHRATE, skip=TRUE) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_nominal()) %>%
        step_knnimpute(all_predictors(), neighbours = 5)
    })
    
    rec_select <- reactive({
        if(input$recipe == "Only KNN"){rec_0()}
        else if(input$recipe == "Standardization follows KNN"){rec()}
        else if(input$recipe == "KNN follows standardization"){rec_1()}
        else{rec_2()}
    })
    
    model <- reactive({
        rec <- rec_select()
        dat <- train_df()
        caret::train(x = rec, 
                     data = dat, 
                     method = "glmnet")
    })
    
    predict_deathrate <- reactive({
        predict(model(), newdata = test_df())
    })
    
    
    test_residual_df <- reactive({
        test_rdf <- data.frame(id = test_df()$COUNTRY, Y = test_df()$DEATHRATE, Yhat = predict_deathrate())
        test_rdf$Residual <- test_rdf$Y - test_rdf$Yhat
        test_rdf
    })
    
    rmse_test <- reactive({
        caret::RMSE(pred = predict_deathrate(), obs = test_residual_df()$DEATHRATE)
    }) 
    
    plot_range <- reactive({
        rang <- range(c(test_residual_df()$Y, test_residual_df()$Yhat))
    })
    
    output$SummaryA1 <- renderPrint({
        skimr::skim(population_df)
    })
    
    output$SummaryA2 <- renderPrint({
        summary(population_df)
    })


    output$population_table <- DT::renderDataTable({
        DT::datatable(data = population_df)
    })
    
    output$Missing <- renderPlot({
        d <- clean_df()[order(d$COUNTRY, decreasing = FALSE),]
        vis_miss(d, cluster = input$cluster) +
            labs(title = "vis_miss of Ass2 data")
    })
    
    output$gg_miss <- renderPlot({
        
        naniar::gg_miss_upset(clean_df(),nsets = 15) 
    })

    
    output$Boxplot <- renderPlot({

        data_boxplot <- dplyr::select_if(clean_df(), is.numeric) # only select the numeric columns
        data_boxplot <- scale(data_boxplot, center = input$standardise, scale = input$standardise)
        car::Boxplot(formula =~., data = data_boxplot, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                     horizontal = FALSE, outline = input$outliers, ylab = NA,las = 2,
                     col = brewer.pal(n = dim(data_boxplot)[2], name = "RdBu"),
                     range = input$range, main = "Boxplots of car data",
                     id = list(n=Inf, location="avoid"))
    })
    
    output$varImpPlot <- renderPlot({
        r_removedNA <- na.exclude(population_df)  # remove observations contain missing values
        rf_train_model=randomForest(r_removedNA$DEATHRATE~.,data=r_removedNA, mtry=4, ntree=100, importance=TRUE)
          
        varImpPlot(rf_train_model)
    })
    
    output$rpartTree <- renderPlot({
        population_df$MISSINGNESS <- apply(X = is.na(population_df), MARGIN = 1, FUN = sum)
        tree <- train(MISSINGNESS ~ . -COUNTRY, data = population_df, method = "rpart", na.action = na.rpart)
        rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
    })
    
    output$train_table <- DT::renderDataTable({
        DT::datatable(data = train_df())
    })
    
    output$test_table <- DT::renderDataTable({
        DT::datatable(data = test_df())
    })
    
    output$optimise_hyperparameters <- renderPlot({
        set.seed(input$seed2)
        
        plot(model())
    })

    output$predict_performance <- renderPlot({
        ggplot(test_residual_df()) +
            geom_point(mapping = aes(x = Yhat, y = Y)) +
            geom_abline(slope=1, col = "blue") +
            labs(title = paste("Model with dependent X & Y outliers \n The MRSE of the test dataset is ", format(rmse_test(),digits=3)), y = "actual", x = "predicted") +
            coord_fixed(ratio = 1, xlim = plot_range(), ylim = plot_range(), expand = TRUE)
    })
    
    output$modelBased_outliers <- renderPlot({
        ggplot(data = test_residual_df(), mapping = aes(x = Residual), y = 0) +
            geom_boxplot(coef = input$coef, outlier.colour = "red") +
            labs(title = paste("Uni-variable boxplots at IQR multiplier of", input$coef), x = "Residual") +
            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
       
    })
    
    
    

    # output$Corrgram <- renderPlot({
    #     
    #     m <- is.na(clean_df()) + 0 # this is a trick that transforms Logical to Binary
    #     cm <- colMeans(m)
    #     m <- m[, cm > 0 & cm < 1, drop = FALSE] #remove none-missing or all-missing variables
    #     
    #     corrgram(cor(m),
    #              order = input$Group,
    #              abs = input$abs,
    #             )
    #     title(main = "Variable missing value correlation",
    #           sub = "Notice whether variables are missing in sets")
    #     
    # })

#     
#     output$MixedPairs <- renderPlot({
#         GGally::ggpairs(data = pairs_set,  
#                         mapping = ggplot2::aes(colour = "rainbowl"), 
#                         title = "Pairs of set1", progress = FALSE)
#     })
#     
   
    
    # output$MixedPairs2 <- renderPlot({
    #     GGally::ggpairs(data = pairs_set2,  
    #                     mapping = ggplot2::aes(colour = "rainbowl"), 
    #                     title = "Pairs of set2", progress = FALSE)
    # })
    # 
    # output$MixedPairs3 <- renderPlot({
    #     GGally::ggpairs(data = pairs_set3,  
    #                     mapping = ggplot2::aes(colour = "rainbowl"), 
    #                     title = "Pairs of set3", progress = FALSE)
    # })
    # 
    # output$MixedPairs4 <- renderPlot({
    #     GGally::ggpairs(data = pairs_set4,  
    #                     mapping = ggplot2::aes(colour = "rainbowl"), 
    #                     title = "Pairs of set4", progress = FALSE)
    # })

    # 
    # 
    # output$RisingValue <- renderPlot({
    #     d<-data_frame_car[,input$rising]
    #     for (col in 1:ncol(d)) {
    #         d[,col] <- d[order(d[,col]),col] #sort each column in ascending order
    #     }
    #     d <- scale(x = d, scale = TRUE)  # scale so they can be graphed with a shared Y axis
    #     mypalette <- rainbow(ncol(d))
    #     matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
    #     legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    #  })
    # 
    # 
    # output$Mosaic <- renderPlot({
    #     formula <- as.formula(paste("~",paste(input$Mosaic, collapse = " + ")))
    #     vcd::mosaic(formula, data = data_frame_car,
    #                 main = "Any common or uncommon", shade = TRUE, legend = TRUE)
    # })
    # 
    # 

})
