#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rmarkdown)
library(knitr)
library(pander)

library(openintro)
library(plotrix)

sidebarPanel2 <- function (..., out = NULL, width = 4){
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}


ui <- fluidPage(
  
  # Application title
  titlePanel("Simple linear regression"),
  # h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
  withMathJax(),

  sidebarLayout(
    sidebarPanel2(
      tags$b("Data:"),
      textInput("x", "x", value = "90, 100, 90, 80, 87, 75", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
      textInput("y", "y", value = "950, 1100, 850, 750, 950, 775", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
      checkboxInput("show.resid", "Show residuals", FALSE),
      hr(),
      tags$b("Plot:"),
      checkboxInput("se", "Add confidence interval around the regression line", TRUE),
      textInput("xlab", label = "Axis labels:", value = "x", placeholder = "x label"),
      textInput("ylab", label = NULL, value = "y", placeholder = "y label"),
      hr(),
      # textInput("pred", "Prediction of y when x = ", value = "100, 110, 100, 90, 97, 85", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
      # hr(),
      # radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
      #              inline = TRUE
      # ),
      # checkboxInput("echo", "Show code in report?", FALSE),
      # downloadButton("downloadReport"),
      # br(),
      # hr(),
      helpText(a(href="https://github.com/tig3r66/youreka-textbook-v2/tree/main/assets/interactive_apps", target="_blank", "View the code")),
      helpText("Code adapted from", a(href="https://github.com/AntoineSoetewey/statistics-202", target="_blank", "Antoine Soetewey"), "and", a(href="https://github.com/ShinyEd/intro-stats", "ShinyEd")),
      out = verbatimTextOutput("summary")
    ),

    mainPanel(
      # tags$b("Your data:"),
      # DT::dataTableOutput("tbl"),
      # br(),
      # uiOutput("data"),
      # br(),
      # tags$b("Compute parameters by hand:"),
      # uiOutput("by_hand"),
      # br(),
      # tags$b("Compute parameters in R:"),
      # tags$b("Regression plot:"),
      plotOutput("plot"),
      br(),
      plotOutput("residuals"),
      tags$b("Interpretation:"),
      uiOutput("results"),
      uiOutput("interpretation"),
      br()
    )
  )
)

server <- function(input, output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }

  # Data output
  # output$tbl <- DT::renderDataTable({
  #   y <- extract(input$y)
  #   x <- extract(input$x)
  #   DT::datatable(data.frame(x, y),
  #                 extensions = "Buttons",
  #                 options = list(
  #                   lengthChange = FALSE,
  #                   dom = "Blfrtip",
  #                   buttons = c("copy", "csv", "excel", "pdf", "print")
  #                 )
  #   )
  # })

  output$data <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for x and y"
    } else {
      # withMathJax(
      #   paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
      #   br(),
      #   paste0("\\(\\bar{y} =\\) ", round(mean(y), 3))
      # )
    }
  })
  
  output$by_hand <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      # br(),
      # paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
      # br(),
      # br(),
      # paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
      # br(),
      # br(),
      paste0("\\(y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
    )
  })
  
  output$summary <- renderPrint({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  output$results <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      paste0( 
        paste0("Model: \\( y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
        # ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
      )
    )
  })

  output$interpretation <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), "."),
        br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.")
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), "."),
        br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, "."),
        br(),
        paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), "."),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
        br(),
        paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.")
      )
    } else {
      withMathJax(
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), "."),
        br(),
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0."),
        br(),
        paste0("Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.")
      )
    }
  })

  mydata <- reactive({
    data.frame(x=extract(input$x), y=extract(input$y))
  })
  
  lmResults <- reactive({
    regress.exp <- "y~x"
    lm(regress.exp, data=mydata())
  })

  output$plot <- renderPlot({
    data1 <- mydata()
    x <- data1$x
    y <- data1$y

    # For confidence interval
    xcon <- seq(min(x) - 0.1, max(x) + 0.1, length.out = length(x))
    print(length(xcon))
    predictor <- data.frame(x=xcon)
    yhat <- predict(lmResults())
    yline <- predict(lmResults(), predictor)

    par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar = c(4,4,4,1))

    r.squared = round(summary(lmResults())$r.squared, 4)
    corr.coef = round(sqrt(r.squared), 4)

    plot(data1$x,
         data1$y,
         xlab="x",
         ylab="y",
         main=paste0("Regression Model\n","(R = ", corr.coef,", ", "R-squared = ", r.squared,")"))
    
    newx <- seq(min(data1$x), max(data1$x), length.out=nrow(data1))
    confs <- predict(lmResults(), newdata = data.frame(x=newx),
                     interval = 'confidence')
    preds <- predict(lmResults(), newdata = data.frame(x=newx),
                     interval = 'predict')

    polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
    polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.75), border = NA)

    points(x,y,pch=19, col=COL[1,2])
    lines(xcon, yline, lwd=2, col=COL[1])

    if (input$show.resid) for (j in 1:length(x))
      lines(rep(x[j],2), c(yhat[j],y[j]), col=COL[4])

    legend_pos = ifelse(lmResults()$coefficients[1] < 1, "topleft", "topright")
    legend(legend_pos, inset=.05,
           legend=c("Regression Line", "Confidence Interval", "Prediction Interval"),
           fill=c(COL[1],grey(.75),grey(.95)))
    box()
  })
  
  
  output$residuals <- renderPlot({
    par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
    residuals = summary(lmResults())$residuals
    predicted = predict(lmResults(), newdata = data.frame(x=mydata()$x))
    plot(residuals ~ predicted, 
         main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
         pch=19, col = COL[1,2])
    abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
    qqline(residuals, col = COL[1], lwd = 2)
  }, height=350 )


  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },

    content = function(file) {
      src <- normalizePath("report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
