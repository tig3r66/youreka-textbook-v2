# Code adapted from https://github.com/ShinyEd/intro-stats/tree/master/CLT_prop

# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  # Title ----
  titlePanel("Central Limit Theorem", windowTitle = "CLT"),
  
  sidebarLayout(
    sidebarPanel(width = 4, align="center",
                 wellPanel(
                   # Select distribution ----
                   selectInput("dist", "Population distribution:",
                               c("Normal" = "rnorm",
                                 "Uniform" = "runif",
                                 "Right skewed" = "rlnorm",
                                 "Left skewed" = "rbeta"),
                               selected = "rnorm"),
                   # hr(),
                   
                   # Distribution parameters / features ----
                   uiOutput("mu"),
                   uiOutput("sd"),
                   uiOutput("minmax"),
                   uiOutput("skew"),
                   
                   # Select sample size ----
                   sliderInput("n",
                               "Sample size", 
                               value = 30,
                               min = 2,
                               max = 500),
                   br(),
                   
                   # Number of samples ----
                   sliderInput("k",
                               "Number of samples",
                               value = 200,
                               min = 10,
                               max = 1000),
                 ),
                 
                 # Informational text ---- 
                 helpText(a(href="https://github.com/tig3r66/youreka-textbook-v2/tree/main/assets/interactive_apps", target="_blank", "View the code")),
                 helpText(a(href="https://github.com/ShinyEd/intro-stats", target="_blank", "Code adapted from ShinyEd"))
                 
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # First tab ----
        tabPanel(
          title = "Distributions",
          
          fluidRow(
            column(width = 12,
                   br(),
                   # Population plot ----
                   plotOutput("pop.dist"))
          ),
          fluidRow(
            column(width = 12,
                   br(),
                   # Sampling plot ----
                   plotOutput("sampling.dist"))
          )
        ),
        
        # Second tab ----
        tabPanel(
          title = "Samples",
          # Sample plots ----
          br(),
          plotOutput("sample.dist", height = "600px"),
          #  Number of samples text ----
          div(h3(textOutput("num.samples")), align = "center"),
          br()
        )
        
      )
    )
  )
)



# Define server function -------------------------------------------------------

seed <- as.numeric(Sys.time())
server <- function(input, output, session) {
  
  # Mean slider for Normal distribution ----
  output$mu = renderUI(
    {
      if (input$dist == "rnorm") {
        sliderInput("mu",
                    "Mean",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  # SD slider for Normal distribution ----
  output$sd = renderUI(
    {
      if (input$dist == "rnorm") {
        sliderInput("sd",
                    "Standard deviation",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  # Minmax slider for Uniform distribution ----
  output$minmax = renderUI(
    {
      if (input$dist == "runif") {
        sliderInput("minmax",
                    "Lower and Upper Bounds",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  # Making sure range for uniform distribution != 0 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  
  # skew slider for rlnorm and rbeta ----
  output$skew = renderUI(
    {
      
      if (input$dist == "rlnorm" | input$dist == "rbeta") {
        selectInput(inputId = "skew",
                    label = "Skew:",
                    choices = c("Low skew" = "low",
                                "Medium skew" = "med",
                                "High skew" = "high"),
                    selected = "low")
      }
    })
  
  # generating random samples ----
  rand_draw <- function(dist, n, mu, sd, min, max, skew) {
    
    vals = NULL
    
    if (dist == "rbeta") {
      req(skew)
      if (skew == "low") {
        vals = do.call(dist, list(n=n, shape1=5, shape2=2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
      }
    }
    
    else if (dist == "rnorm") {
      req(mu, sd)
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    }
    
    else if (dist == "rlnorm") {
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
      }
      else if (skew == "med") {
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
      }
      else if (skew == "high") {
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
      }
    }
    
    else if (dist == "runif") {
      req(min, max)
      vals = do.call(dist, list(n=n, min=min, max=max))
    }
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)
  
  # Defining some reactive variables to use later ----
  parent = reactive({
    
    n_sample = 1e5
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2], input$skew))
  })
  
  samples = reactive({
    pop = parent()
    n = input$n
    k = input$k
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  u_min = reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  
  u_max = reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  
  # plot 1 ----
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      rlnorm = "Population distribution: Right skewed",
                      rbeta = "Population distribution: Left skewed",
                      runif = "Population distribution: Uniform")
    
    pop = parent()
    m_pop =  round(mean(pop), 2)
    sd_pop = round(sd(pop), 2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm") {
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#7FA9D7") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom="line", color = "#7FA9D7", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = bquote(atop(mu ~ "=" ~ .(m_pop),
                                     sigma ~ "=" ~ .(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 15) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif") {
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
        
        x_pos = max(pop$samples) - 0.2*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white", fill = "#7FA9D7") +
          stat_density(geom = "line", color = "#7FA9D7", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("mean of x", "=", bquote(.(m_pop)),
                                 "\n", "SD of x", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_light(base_size = 15) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())}
      
    } else if (input$dist == "rlnorm") {
      
      x_pos = max(pop$samples) - 0.2*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#7FA9D7") +
        stat_density(geom = "line", color = "#7FA9D7", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)), 
                               "\n", "SD of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "rbeta") {
      
      x_pos = min(pop$samples) + 0.2*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#7FA9D7") +
        stat_density(geom = "line", color = "#7FA9D7", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("mean of x", "=", bquote(.(m_pop)), 
                               "\n", "SD of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }
  })
  
  
  # plot 2 ----
  output$sample.dist = renderPlot({
    
    y = samples()
    x = samples() %>% as_tibble()
    
    plots = list(rep(NA, 8))
    for(i in 1:8) {
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.2*x_range, 
                     max(y[,i]) - 0.2*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7) +
        labs(title = paste("Sample", i), x = "", y = "") +
        theme_light(base_size = 15) +
        annotate("text", x = x_pos, y = 1.8,
                 label = bquote(atop(bar(x) ~ "=" ~ .(mean),
                                     s ~ "=" ~ .(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  # text for sample plots ----
  output$num.samples = renderText({
    
    k = input$k
    paste0("... continuing to Sample ", k,".")
    
  })
  
  # plot 3 ----
  output$sampling.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "normal population",
                      rlnorm  = "right skewed population",
                      rbeta = "left skewed population",
                      runif = "uniform population")
    
    n = input$n
    k = input$k
    
    pop = parent()
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist = tibble(means = colMeans(samples()))
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot=FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
    
    p = ggplot(data = ndist, aes(x = means, y = ..density..)) +
      geom_histogram(bins = 20, color = "white", fill = "#7FA9D7") +
      stat_density(geom = "line", color = "#7FA9D7", size = 1) +
      labs(title = paste("Sampling Distribution"),
           x = "Sample means",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = bquote(atop("mean of" ~ bar("x") ~ "=" ~ .(m_samp),
                                   "SE of" ~ bar("x") ~ "=" ~ .(sd_samp))),
               color = "black", size = 5) +
      theme_light(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    if (input$dist == "runif") {
      
      if (u_min() == u_max()) {
        " "
      } else {
        p
      }
    } else {
      p
    }
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
  }, height=280 ) 
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
