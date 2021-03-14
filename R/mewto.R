#' Model Evaluation with Weighted Threshold Optimization App
#'
#' This function launches a Shiny application where the user can interactively
#'  manipulate the threshold used in binary classification and view the
#'  associated metrics, confusion matrix, ROC and PR curves. The app also allows
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
#'  function maximizes the metric (w * sensitivity + (1 - w) * specificity),
#'  where w is the "weight" parameter chosen through the second slider. After
#'  setting the desired value on the optimal threshold slider, the user must
#'  press the button "Set" to update the threshold slider above with the optimal
#'  value. Metrics will automatically be recalculated based on the user's selection.
#' @examples
#' mewtoApp(actuals, probabilities)
#' @author Alexandru Monahov, \email{alexandrudezv@@gmail.com}
#' @export
mewtoApp <- function(actuals, probabilities) {

  rocfun <- function(d_orig, d_pred) {
    roc_v <- suppressMessages(roc(response=d_orig,
                                  predictor=d_pred))
    return(roc_v)
  }

  confmat <- function(d_orig, d_pred, threshold) {
    d_pred_bin <- ifelse(d_pred>=threshold,"yes","no")
    confusionmatrix<-confusionMatrix(as.factor(d_pred_bin),as.factor(d_orig),
                                     positive="yes",
                                     dnn = c("Prediction", "Actual Data"))
    return(confusionmatrix)
  }

  comb <- function(rocfun_output, wght) {
    combinations <- as.data.frame((coords(roc = rocfun_output, x = rocfun_output$thresholds)))
    combinations <- combinations %>%
      mutate(youden = (wght * sensitivity + (1 - wght) * specificity))
    return(combinations)
  }

  prfun <- function(d_orig, d_pred) {
    pr_v<-pr.curve(scores.class0 = d_pred[d_orig=="yes"],# these are the predictions for default=no
                       scores.class1 = d_pred[d_orig=="no"], # these are the predictions for default=yes
                       curve=TRUE)

    return(pr_v)
  }


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
               tags$head(
                 tags$style(type='text/css',
                            ".nav-tabs {font-size: 10px} ")),
               tabsetPanel(tabPanel("ROC Curve",
               plotOutput("plot1"),
               column(12, tableOutput("values5"), align="center")),
               tabPanel("PR Curve", plotOutput("plot_PR"))
               )
        )
      )
    ),

    server <- function(input, output, session) {

      confmatL <- reactive({ confmat(actuals, probabilities, input$threshold) })

      rocfunL <- reactive({ rocfun(actuals, probabilities) })

      prfunL <- reactive({ prfun(actuals, probabilities) })

      combL <- reactive({ comb(rocfunL(), input$weight) })

      sliderValues <- reactive({
        tibble::rownames_to_column(as.data.frame(confmatL()$byClass), "Indicator") %>% rename(Value = 2)
      })

      sliderValues2 <- reactive({
        options(dplyr.summarise.inform = FALSE)
        as.data.frame(confmatL()$table) %>%
          group_by(Prediction,Actual.Data) %>%
          summarise(score=mean(Freq)) %>%
          spread(Actual.Data,score) %>%
          ungroup %>%
          mutate(across(is.numeric, as.integer)) %>%
          rename(Pred_Act = 1)
      })

      sliderValues3 <- reactive({
        results <- data.frame(c("Optimal threshold"),
                              c(combL()$threshold[which.max(combL()$youden)]))
        results <- results %>% rename(Method = 1, Threshold = 2)
      })

      observeEvent(input$set,{
        updateSliderInput(session,'threshold',value = combL()$threshold[which.max(combL()$youden)])
      })

      sliderValues4 <- reactive({
        weighttb <- as.data.frame(t(data.frame(c("Specificity (minimize FPR)",1-input$weight,"-","Sensitivity (maximize TPR)",input$weight))))
        weighttb <- weighttb %>%
          mutate(across(is.numeric, as.integer))
      })

      sliderValues5 <- reactive({
        roc_auc <- data.frame(c("AUC"),
                              c(rocfunL()$auc))
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
        TPR <- (confmatL()$table[1,1])/(confmatL()$table[1,1]+confmatL()$table[2,1])
        FPR <- 1 - (confmatL()$table[1,2])/(confmatL()$table[1,2]+confmatL()$table[2,2])
        plot(rocfunL(), legacy.axes = TRUE, xlab="FPR - False Positives Rate", ylab="TPR - True Positives Rate")
        lines(x=c(TPR,TPR), y=c(0,FPR), col="cornflowerblue",lwd=2)
        lines(x=c(1,TPR), y=c(FPR,FPR), col="cornflowerblue",lwd=2)
        draw.circle(TPR,FPR,0.01,border="cornflowerblue",col="cornflowerblue")
      })

      output$plot_PR <- renderPlot({
        prec <- confmatL()$byClass[5]
        reca <- confmatL()$byClass[6]
        circ <- data.frame(xval = reca, yval=prec)
        scal <- max(1,as.integer(nrow(prfunL()$curve)/3000))
        ggplot(data.frame(prfunL()$curve[seq(1, nrow(prfunL()$curve), scal),]),aes(x=X1,y=X2)) +
          geom_line(size=1) + labs(x="Recall",y="Precision") +
          annotate("text", x = .0, y = .05, label = sprintf("AUC-PR Logit = %0.2f", prfunL()$auc.integral),hjust=0, size=4,
                   color="black", fontface=1) +
          scale_colour_gradient2(low="yellow", mid="green",high="blue") +
          geom_segment(aes(y = 0, x=reca), yend = prec, xend = reca, color = "cornflowerblue")+
          geom_segment(aes(y = prec, x=0), yend = prec, xend = reca, color = "cornflowerblue")+
          geom_point(data=circ, mapping=aes(x = xval, y = yval, size=15), color="cornflowerblue")+
          theme_bw(base_size = 16)+
          theme(legend.position = "None", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      })

      output$plot_confusionmatrix <- renderPlot({
        cfm <- tidy(confmatL()$table)
        plot_confusion_matrix(cfm,
                              target_col = "Actual Data",
                              prediction_col = "Prediction",
                              counts_col = "n",
                              font_counts = font(
                                size = 5,
                                angle = 0,
                                color = "black"
                              ),
                              font_normalized = font(
                                size = 5,
                                angle = 0,
                                color = "black"
                              ),
                              rm_zero_text=FALSE,
                              palette = "Blues"
        )
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
#' @param weight A numeric value corresponding to the importance attributed to sensitivity, or formulated
#'  differently, to the maximization of the true positives rate. This value should be in the interval [0 ; 1].
#' @keywords threshold, optimum, evaluation, youden, weighted
#' @details In the calculation of the optimal threshold, a weighted version of
#'  Youden's J-statistic (Youden, 1950) is employed. The optimal cut-off is the
#'  threshold that maximizes the distance to the identity (diagonal) line. The
#'  function maximizes the metric (w * sensitivity + (1 - w) * specificity),
#'  where w is the "weight" parameter. Metrics will automatically be recalculated
#'  based on the user's selection.
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
    mutate(youden = (weight*sensitivity + (1-weight)*specificity))
  # Calculate optimal threshold
  threshold <- combinations$threshold[which.max(combinations$youden)]
  # Output
  return(threshold)
}
