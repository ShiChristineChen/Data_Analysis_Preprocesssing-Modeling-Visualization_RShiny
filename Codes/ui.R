# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        useShinyjs(),
        
        titlePanel("Assignment 2 - Visualising_Shi Chen"
        ),
        tabsetPanel(
            tabPanel("EDA_Summary",
                     verbatimTextOutput(outputId = "SummaryA1"),
                     verbatimTextOutput(outputId = "SummaryA2")
                     
            ),
            
            tabPanel("EDA_Raw Data",
                     DT::dataTableOutput(outputId = "population_table")
            ),
            tabPanel("EDA_Missing Value",
                     sliderInput(
                         inputId = "threshold_1",
                         label = "Excesive missing threshold for variables",
                         min = 0,
                         max = 100,
                         value = 50
                     ),
                     textOutput(outputId = "variable_threshold"),
                     br(),
                     sliderInput(
                         inputId = "threshold_2",
                         label = "Excesive missing threshold for observations",
                         min = 0,
                         max = 100,
                         value = 50
                     ),
                     textOutput(outputId = "observation_threshold"),
                     
                     shinycssloaders::withSpinner(
                     plotOutput(outputId = "Missing")
                     ),
                     checkboxInput(inputId = "cluster", label = "Cluster missingness", value = TRUE),
                     
                     br(),
                     
                     #checkboxInput(inputId = "gg_order", label = "Variables order", value = TRUE),
                     
                     # selectizeInput(inputId = "gg_select_order",
                     #                label = "Select variables order:",
                     #                choices = choiceA,
                     #                multiple = TRUE,
                     #                selected = choices[1]),

                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "gg_miss")
                     )
            ),
                     

            tabPanel("EDA_Boxplot & Outliers",
                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "Boxplot", height = 600)
                     ),
                     checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                     checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                     sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
            ),
              
            tabPanel("Strategy_RF_VIP variables",
                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "varImpPlot", height = 600)
                     )
            ),
            
            tabPanel("Strategy_rpartTree_missing pattern prediction",
                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "rpartTree", height = 600)
                     )
            ),
            
            tabPanel("Model_train/test split",
                     selectizeInput(inputId = "seed1", 
                                    label = "Select set.seed number:", 
                                    choices = seq(1,50,1), 
                                    multiple = TRUE, 
                                    selected = 1),
                     br(),
                     sliderInput(
                         inputId = "train_p",
                         label = "Tain data Percentage",
                         min = 0,
                         max = 1,
                         value = 0.7
                     ),
                     textOutput(outputId = "split"),
                     br(),
                     DT::dataTableOutput(outputId = "train_table"),
                     br(),
                     DT::dataTableOutput(outputId = "test_table")
            ),
            
            tabPanel("Model_glmnet",
                     selectizeInput(inputId = "seed2", 
                                    label = "Select set.seed number:", 
                                    choices = seq(1,50,1), 
                                    multiple = TRUE, 
                                    selected = 1),
                     br(),
                     selectizeInput(inputId = "recipe", 
                                    label = "Select a recipe-based processing pipeline", 
                                    choices = c("Only KNN","Standardization follows KNN","KNN follows standardization","Put KNN at last step"), 
                                    multiple = FALSE, 
                                    selected = "Only KNN"),
                     br(),
                     
                     plotOutput(outputId = "optimise_hyperparameters", height = 600),
                     br(),
                     plotOutput(outputId = "predict_performance", height = 600),
                     br(),
                     sliderInput(
                         inputId = "coef",
                         label = "IQR parameter",
                         min = 0,
                         max = 3,
                         value = 1.5
                     ),
                     br(),
                     plotOutput(outputId = "modelBased_outliers", height = 600)
            )
                     

            # tabPanel("Corrgram",
            #          shinycssloaders::withSpinner(
            #          plotOutput(outputId = "Corrgram")
            #          ),
            #          checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
            #          selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
            # )

            # 
            # tabPanel("Mixed Pairs set1",
            #          selectizeInput(inputId = "pairs", label = "Select variable set:", choices = colname_pairs_set, multiple = TRUE, selected = colname_pairs_set[1:11]),
            #          shinycssloaders::withSpinner(
            #              plotOutput(outputId = "MixedPairs")
            #          )
            # ),
            # 
            
            # tabPanel("Mixed Pairs set2",
            #          selectizeInput(inputId = "pairs2", label = "Select variable set:", choices = colname_pairs_set2, multiple = TRUE, selected = colname_pairs_set2[1:10]),
            #          shinycssloaders::withSpinner(
            #              plotOutput(outputId = "MixedPairs2")
            #          )
            # ),
            
            # tabPanel("Mixed Pairs set3",
            #          selectizeInput(inputId = "pairs3", label = "Select variable set:", choices = colname_pairs_set3, multiple = TRUE, selected = colname_pairs_set3[1:9]),
            #          shinycssloaders::withSpinner(
            #              plotOutput(outputId = "MixedPairs3")
            #          )
            # ),
            
            # tabPanel("Mixed Pairs set4",
            #          selectizeInput(inputId = "pairs4", label = "Select variable set:", choices = colname_pairs_set4, multiple = TRUE, selected = colname_pairs_set4[1:13]),
            #          shinycssloaders::withSpinner(
            #              plotOutput(outputId = "MixedPairs4")
            #          )
            # ),
           
            
            
            # 
            # tabPanel("Rising Value",
            #          selectizeInput(inputId = "rising", label = "Select variable set:", choices = choicesC, multiple = TRUE, selected = population_df[c(3,4,5,6,7,8,9,10,11,13,14)]),
            #          plotOutput(outputId = "RisingValue")
            # ),
            
            # tabPanel("Mosaic",
            #          selectizeInput(inputId = "Mosaic", label = "Show variables:", choices = choicesA, multiple = TRUE, selected = population_df[c(1,2,12)],options = list(maxItems = 4)),
            #          plotOutput(outputId = "Mosaic")
            #)
        )
        
    )
    
)

        
            

   
    

