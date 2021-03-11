#' Model Evaluation with Weighted Threshold Optimization App
#'
#' This function launches a Shiny application where the user can interactively
#'  manipulate the threshold used in binary classification and view the
#'  associated metrics, confusion matrix and ROC curve. The app also allows
#'  for optimal threshold calculation according to a weighted version of
#'  Youden's J-statistic.
#' @param actuals Data of factor type with two levels: "yes" for positive and
#'  "no" for negative.
#' @param probabilities Data of numeric type which should represent the
#'  probabilities of realization of the positive category.
#' @keywords threshold, optimum, evaluation, confusion, matrix, roc
#' @details In the calculation of the optimal threshold, a weighted version of
#'  Youden's J-statistic (Youden, 1950) is employed. The optimal cut-off is the
#'  threshold that maximizes the distance to the identity (diagonal) line. The
#'  function maximizes the metric 2 * (w * sensitivity + (1 - w) * specificity),
#'  where w is the "weight" parameter chosen through the second slider. After
#'  setting the desired value on the optimal threshold slider, the user must
#'  press the button "Set" to update the threshold slider above with the optimal
#'  value.
#' @examples
#' mewtoApp(actuals, probabilities)
#' @author Alexandru Monahov, \email{alexandrudezv@@gmail.com}
#' @export
mewtoApp <- function(actuals, probabilities) {
  shinyApp(
    ui <- fluidPage(
      h2(strong("Model Evaluation with Weighted Threshold Optimization"), style = "font-size:15px;"),
      fluidRow(
        column(4,
               wellPanel(
                 sliderInput("threshold", "Threshold:",
                             min = 0, max = 1,
                             value = 0.5)
               ),
               p(strong("Optimal Threshold"), style = "font-size:15px;"),
               wellPanel(
                 sliderInput("weight", "Weight on true positive rate maximization:",
                             min = 0, max = 1,
                             value = 0.5)
               ),
               tableOutput("values4"),
               fluidRow(column(9, tableOutput("values3")),
                        column(2, actionButton("set", "Set")))
        ),
        column(3,
               p(strong(" Confusion Matrix"), style = "font-size:15px;"),
               tableOutput("values2"),
               tableOutput("values")
        ),
        column(5,
               p(strong(" ROC Curve"), style = "font-size:15px;"),
               plotOutput("plot1"),
               column(12, tableOutput("values5"), align="center")
        )
      )
    ),
    server <- function(input, output, session) {
      sliderValues <- reactive({
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        tibble::rownames_to_column(as.data.frame(conf_mat_train$byClass), "Indicator") %>% rename(Value = 2)
      })
      sliderValues2 <- reactive({
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        options(dplyr.summarise.inform = FALSE)
        as.data.frame(conf_mat_train$table) %>%
          group_by(Prediction,Actual.Data) %>%
          summarize(score=mean(Freq)) %>%
          spread(Actual.Data,score) %>%
          ungroup %>%
          mutate(across(is.numeric, as.integer)) %>%
          rename(Pred_Act = 1)
      })
      sliderValues3 <- reactive({
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        roc_v <- suppressMessages(roc(response=d_orig,
                     predictor=d_pred))
        combinations <- as.data.frame((coords(roc = roc_v, x = roc_v$thresholds)))
        combinations <- combinations %>%
          mutate(youden = 2 * (input$weight*sensitivity + (1-input$weight)*specificity))
        results <- data.frame(c("Optimal threshold"),
                              c(combinations$threshold[which.max(combinations$youden)]))
        results <- results %>% rename(Method = 1, Threshold = 2)
      })
      observeEvent(input$set,{
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        roc_v <- suppressMessages(roc(response=d_orig,
                     predictor=d_pred))
        combinations <- as.data.frame((coords(roc = roc_v, x = roc_v$thresholds)))
        combinations <- combinations %>%
          mutate(youden = 2 * (input$weight*sensitivity + (1-input$weight)*specificity))
        results <- data.frame(c("Optimal threshold"),
                              c(combinations$threshold[which.max(combinations$youden)]))
        results <- results %>% rename(Method = 1, Threshold = 2)
        updateSliderInput(session,'threshold',value = combinations$threshold[which.max(combinations$youden)])
      })
      sliderValues4 <- reactive({
        weighttb <- as.data.frame(t(data.frame(c("Specificity (minimize FPR)",1-input$weight,"-","Sensitivity (maximize TPR)",input$weight))))
        weighttb <- weighttb %>%
          mutate(across(is.numeric, as.integer))
      })
      sliderValues5 <- reactive({
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        roc_v <- suppressMessages(roc(response=d_orig,
                     predictor=d_pred))
        roc_auc <- data.frame(c("AUC"),
                              c(roc_v$auc))
      })
      output$values <- renderTable({
        sliderValues()
      })
      output$values2 <- renderTable({
        sliderValues2()
      })
      output$values3 <- renderTable({
        format(sliderValues3(), nsmall = 4)
      }, colnames = FALSE)
      output$values4 <- renderTable({
        sliderValues4()
      }, digits = 0, colnames = FALSE)
      output$values5 <- renderTable({
        sliderValues5()
      }, colnames = FALSE)
      output$plot1 <- renderPlot({
        d_orig <- actuals
        d_pred <- probabilities
        d_pred_bin <- ifelse(d_pred>=input$threshold,"yes","no")
        conf_mat_train<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                        positive="yes",
                                        dnn = c("Prediction", "Actual Data"))
        TPR <- (conf_mat_train$table[1,1])/(conf_mat_train$table[1,1]+conf_mat_train$table[2,1])
        FPR <- 1 - (conf_mat_train$table[1,2])/(conf_mat_train$table[1,2]+conf_mat_train$table[2,2])
        roc_v <- suppressMessages(roc(response=d_orig,
                     predictor=d_pred))
        plot(roc_v, legacy.axes = TRUE, xlab="FPR - False Positives Rate", ylab="TPR - True Positives Rate")
        lines(x=c(TPR,TPR), y=c(0,FPR), col="cornflowerblue",lwd=2)
        lines(x=c(1,TPR), y=c(FPR,FPR), col="cornflowerblue",lwd=2)
        draw.circle(TPR,FPR,0.01,border="cornflowerblue",col="cornflowerblue")
      })
    }
  )
}

#' Optimal threshold calculation using weighted Youden's J-statistic
#'
#' This function calculates the optimal threshold according to a weighted
#' version of Youden's J-statistic.
#' @param actuals Data of factor type with two levels: "yes" for positive and
#'  "no" for negative.
#' @param probabilities Data of numeric type which should represent the
#'  probabilities of realization of the positive category.
#' @param weight The importance attributed to sensitivity, or formulated
#'  differently, to the maximization of the true positives rate.
#' @keywords threshold, optimum, evaluation, youden, weighted
#' @details In the calculation of the optimal threshold, a weighted version of
#'  Youden's J-statistic (Youden, 1950) is employed. The optimal cut-off is the
#'  threshold that maximizes the distance to the identity (diagonal) line. The
#'  function maximizes the metric 2 * (w * sensitivity + (1 - w) * specificity),
#'  where w is the "weight" parameter.
#' @examples
#' mewtoThresh(actuals, probabilities, weight = 0.5) # Original Youden's J-statistic.
#' mewtoThresh(actuals, probabilities, weight = 0.9) # Favor sensitivity greatly over specificity.
#' @author Alexandru Monahov, \email{alexandrudezv@@gmail.com}
#' @export
mewtoThresh <- function(actuals, probabilities, weight=0.5) {
  # Input data
  d_orig <- actuals
  d_pred <- probabilities
  # Generate sensitivity-specificity combinations
  roc_v <- suppressMessages(roc(response=d_orig,
                                predictor=d_pred))
  # Store combinations
  combinations <- as.data.frame((coords(roc = roc_v, x = roc_v$thresholds)))
  # Create evaluation metric (weighted Youden's J statistic)
  combinations <- combinations %>%
    mutate(youden = 2 * (weight*sensitivity + (1-weight)*specificity))
  # Calculate optimal threshold
  threshold <- combinations$threshold[which.max(combinations$youden)]
  # Output
  return(threshold)
}
