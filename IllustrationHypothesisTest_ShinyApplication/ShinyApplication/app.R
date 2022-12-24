
library(shiny)
library(scales)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      # tags$hr(),
      # tags$h3("Change the inputs"),
      tags$div(HTML("<span style='margin-top: 25pt; font-size: 18pt'>Change Inputs</span>")),

      tags$hr(),
      
      # Input 1: Null hypothesis ----
      sliderInput(inputId = "t.act",
                  label = withMathJax(
                    'Actual observed test value \\(t^{act}\\)'
                  ),
                  min = as.numeric(-3),
                  max =  as.numeric(3),
                  value = as.numeric(0),
                  # round = FALSE,
                  step = 0.10),
      
      # Input 2: Select type of the test ----
      selectInput("test",
                  label = withMathJax(
                    'Test form: \\(\\left(\\text{w.r.t. } H_{0} \\right) \\)'
                  ),
                  choices = list("Two sided" = "two_sid", "One sided, less or equal" = "one_sid_leq", "One sided, greater or equal" = "one_sid_geq"),
                  selected = "two_sid"),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow(
        
        # Output 1: PDF ----
        column(12,
               # HTML("<hr>"),
               h3("Probability Density Function (PDF)"),
               plotOutput("Plot01", height = 300)),

        # Output 2: CDF ----
        column(12,
               HTML("<hr>"),
               h3("Cumulative Distribution Function (CDF)"),
               plotOutput("Plot02", height = 300)),
        
        # Output 3: p-value ----
        column(12,
               HTML("<hr>"),
               HTML("<span style='font-size: 16pt'>\\(p\\)-value</span><script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>"),
               htmlOutput("Text02"))
        
      )
    )
  )
)

server <- function(input, output) {

  #..................................................
  # Plot 1: PDF ----
  output$Plot01 <- renderPlot({
    
    # test statistic
    t.act <- as.numeric(input$t.act)
    # test    
    test <- input$test
    
    # plot parameters
    par(mfrow=c(1,1),
        mar=c(4,2,5,10))
    #bottom, left, top, and right
    
    # > One-sided less or equal ----
    if (test==c("one_sid_leq")){
      # plot the standard normal density on the interval [-6,6]
      curve(dnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,0.45),
            cex.lab = 1.5,
            col = alpha("black", 0.25))
      lines(seq(0,3,0.01),dnorm(seq(0,3,0.01)),
            lwd = 2,
            col = alpha("black", 1))
      # add x-axis
      axis(3,at = c(-3, 0, 1.64, 2.33, 3), las=2,
           labels = c("", "0", "1.64", "2.33", ""),
           cex.axis = 1.5)
      axis(1,at = c(-3, round(t.act,3), 3),
           cex.axis = 1.5)
      # shade p-value region in right tail
      if (t.act <= 3) {
        polygon(x = c(t.act, seq(t.act, 3, 0.01), 3),
                y = c(0, dnorm(seq(t.act, 3, 0.01)), 0), 
                col = "steelblue")
      }
      # add critical value lines
      abline(v=1.64, lty=2, lwd=2)
      abline(v=2.33, lty=2, lwd=2)
      # add actual test value lines
      abline(v=t.act, col="red", lty=2, lwd=2)
      
    # > One-sided greater or equal ----
    } else if (test==c("one_sid_geq")){
      # plot the standard normal density on the interval [-6,6]
      curve(dnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,0.45),
            cex.lab = 1.5,
            col = alpha("black", 0.25))
      lines(seq(-3,0,0.01),dnorm(seq(-3,0,0.01)),
            lwd = 2,
            col = alpha("black", 1))
      # add x-axis
      axis(3,at = c(-3, -2.33, -1.64, 0, 3), las=2,
           labels = c("", "-2.33", "-1.64", "0", ""),
           cex.axis = 1.5)
      axis(1,at = c(-3, round(t.act,3), 3),
           cex.axis = 1.5)
      # shade p-value region in left tail
      if (t.act > -3) {
        polygon(x = c(-3, seq(-3, t.act, 0.01), t.act),
                y = c(0, dnorm(seq(-3, t.act, 0.01)), 0), 
                col = "steelblue")
      }
      # add critical value lines
      abline(v=-1.64, lty=2, lwd=2)
      abline(v=-2.33, lty=2, lwd=2)
      # add actual test value lines
      abline(v=t.act, col="red", lty=2, lwd=2)
      
    # > Two-sided ----
    } else if (test==c("two_sid") && t.act <=0) { 
      # plot the standard normal density on the interval [-6,6]
      curve(dnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,0.45),
            cex.lab = 1.5,
            col = alpha("black", 0.25))
      lines(seq(-3,0,0.01),dnorm(seq(-3,0,0.01)),
            lwd = 2,
            col = alpha("black", 1))
      # add x-axis top
      axis(3,at = c(-3, -2.58, -1.96, 0), las=2, # left tail axis crit. values (here relevant!)
           labels = c("","-2.58", "-1.96", "0"),
           cex.axis = 1.5)
      axis(3,at = c(0, 1.96, 2.58, 3), las=2, # right tail axis crit. values (here not relevant!)
           labels = c("0", "1.96", "2.58", ""),
           cex.axis = 1.5,
           col.axis = alpha("black",0.25))
      
      # add x-axis bottom
      axis(1,at = c(-3, 0), # left tail (here relevant!)
           labels = c("", ""),
           cex.axis = 1.5)
      axis(1,at = t.act, # left tail actual test statistic (here relevant!)
           labels = format(t.act, nsmall = 2),
           cex.axis = 1.5)
      axis(1,at = c(0, 3), # right tail (here not relevant!) 
           labels = c("", ""),
           cex.axis = 1.5,
           col.axis = alpha("black",0.25))
      axis(1,at = -t.act, # right tail actual test statistic (here not relevant!)
           labels = format(-t.act, nsmall = 2),
           cex.axis = 1.5,
           col.axis = alpha("black", 0.25))
      
      # shade p-value/2 region in left tail
      if (t.act > -3) {
        polygon(x = c(-3, seq(-3, t.act, 0.01), t.act),
                y = c(0, dnorm(seq(-3, t.act, 0.01)),0), 
                col = "steelblue",
                border =  NA)
      }
      # shade p-value/2 region in right tail
      if (-t.act < 3) {
        polygon(x = c(-t.act, seq(-t.act, 3, 0.01), 3),
                y = c(0, dnorm(seq(-t.act, 3, 0.01)), 0), 
                col = alpha("steelblue",0.5),
                border = NA)
      }
      # add critical value lines
      abline(v=-1.96, lty=2, lwd=2)
      abline(v= 1.96, lty=2, lwd=2, col=alpha("black",0.5))
      abline(v=-2.58, lty=2, lwd=2)
      abline(v= 2.58, lty=2, lwd=2, col=alpha("black",0.5))
      # add actual test value lines
      abline(v=-t.act, lty=2, lwd=2, col=alpha("red",0.5))
      abline(v=t.act, col="red", lty=2, lwd=2)
    } else if (test==c("two_sid") && t.act > 0) { # right tail
      # plot the standard normal density on the interval [-6,6]
      curve(dnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,0.45),
            cex.lab = 1.5,
            col = alpha("black", 0.25))
      lines(seq(0,3,0.01),dnorm(seq(0,3,0.01)),
            lwd = 2,
            col = alpha("black", 1))
      # add x-axis top
      axis(3,at = c(-3, -2.58, -1.96, 0), las=2, # left tail axis crit. values (here not relevant!)
           labels = c("","-2.58", "-1.96", "0"),
           cex.axis = 1.5,
           col.axis = alpha("black",0.25))
      axis(3,at = c(0, 1.96, 2.58, 3), las=2, # right tail axis crit. values (here relevant!)
           labels = c("0", "1.96", "2.58", ""),
           cex.axis = 1.5)
      
      # add x-axis bottom
      axis(1,at = c(0, 3), # right tail (here relevant!)
           labels = c("", ""),
           cex.axis = 1.5)
      axis(1,at = t.act, # right tail actual test statistic (here relevant!)
           labels = format(t.act, nsmall = 2),
           cex.axis = 1.5)
      axis(1,at = c(-3, 0), # left tail (here not relevant!)
           labels = c("", ""),
           cex.axis = 1.5,
           col.axis = alpha("black",0.25))
      axis(1,at = -t.act, # left tail actual test statistic (here not relevant!)
           labels = format(-t.act, nsmall = 2),
           cex.axis = 1.5,
           col.axis = alpha("black", 0.25))
      
      # shade p-value/2 region in left tail
      if (-t.act > -3) {
        polygon(x = c(-3, seq(-3, -t.act, 0.01), -t.act),
                y = c(0, dnorm(seq(-3, -t.act, 0.01)),0), 
                col = alpha("steelblue",0.5),
                border =  NA)
      }
      # shade p-value/2 region in right tail
      if (t.act < 3) {
        polygon(x = c(t.act, seq(t.act, 3, 0.01), 3),
                y = c(0, dnorm(seq(t.act, 3, 0.01)), 0), 
                col = "steelblue",
                border = NA)
      }
      # add critical value lines
      abline(v=-1.96, lty=2, lwd=2, col=alpha("black",0.5))
      abline(v= 1.96, lty=2, lwd=2)
      abline(v=-2.58, lty=2, lwd=2, col=alpha("black",0.5))
      abline(v= 2.58, lty=2, lwd=2)
      # add actual test value lines
      abline(v=-t.act, lty=2, lwd=2, col=alpha("red",0.5))
      abline(v=t.act, col="red", lty=2, lwd=2)
    }
    
  }, height = 300)
  
  #..................................................
  # Plot 2: CDF ----
  output$Plot02 <- renderPlot({
    
    # test statistic
    t.act <- input$t.act
    z <- t.act
    phi.z <- pnorm(t.act)
    # test
    test <- input$test
    
    # plot parameters
    par(mfrow=c(1,1),
        mar=c(2,3,4,10))
    #bottom, left, top, and right
    
    # > Two-sided ----
    if (test==c("two_sid")) {
    
      # use always the negative absolute value
      z <- -abs(z)
      phi.z <- pnorm(-abs(z))
      
      # plot standard normal cumulative density on the interval [-6,6]
      curve(pnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,1),
            cex.lab = 1.5,
            col = alpha("black", 0.25))
      lines( seq(-3, 0, 0.01),pnorm( seq(-3, 0, 0.01)))
      axis(1,at = c(-3, 0, 3), las=2,
           labels = c("", "0", ""),
           cex.axis = 1.5)
      axis(2,at = seq(0,1,0.1), las=2,
           labels = as.character(seq(0,1,0.1)),
           cex.axis = 1.5)
      abline(h=phi.z, col="steelblue", lty=2, lwd=2)
      abline(v=z, col="red", lty=2, lwd=2)
      axis(3,at = c(-3, round(z,3), 3), las=1,
           labels = c("", as.character(round(z,3)), ""),
           cex.axis = 1.5, col.axis = "red")
      axis(4,at = c(round(phi.z,3)), las=1,
           labels = bquote(Phi(.(round(z,3)))== .(round(phi.z,3))),
           cex.axis = 1.5, col.axis = "steelblue")
    
    # > One-sided ----
    } else {
      
      # plot standard normal density on the interval [-6,6]
      curve(pnorm(x),
            xlim = c(-3, 3),
            yaxs = "i",
            xlab = "z",
            ylab = "",
            lwd = 2,
            axes = "F",
            ylim = c(0,1),
            cex.lab = 1.5,
            col = "black")
      axis(1,at = c(-3, 0, 3), las=2,
           labels = c("", "0", ""),
           cex.axis = 1.5)
      axis(2,at = seq(0,1,0.1), las=2,
           labels = as.character(seq(0,1,0.1)),
           cex.axis = 1.5)
      abline(h=phi.z, col="steelblue", lty=2, lwd=2)
      abline(v=z, col="red", lty=2, lwd=2)
      axis(3,at = c(-3, round(z,3), 3), las=1,
           labels = c("", as.character(round(z,3)), ""),
           cex.axis = 1.5, col.axis = "red")
      axis(4,at = c(round(phi.z,3)), las=1,
           labels = bquote(Phi(.(round(z,3)))== .(round(phi.z,3))),
           cex.axis = 1.5, col.axis = "steelblue")
      
    }
    
  }, height = 300)
  
  # Interpretation: p-value ----
  output$Text02 <- renderText({ 
    
    if (input$test==c("two_sid")) {
      paste0("<span style='text-decoration: none; font-size: 14pt'> Remember, in the case of a two-sided test we compute: $$p\\text{-value}=\\Phi\\left(-\\left|t^{act}\\right|\\right)=", format(pnorm(-abs(as.numeric(input$t.act))), digits=2, nsmall=2), "$$<span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
    } else if (input$test==c("one_sid_geq")) {
      paste0("<span style='text-decoration: none; font-size: 14pt'> Remember, in the case of a one-sided greater or equal test we compute: $$p\\text{-value}=\\Phi\\left(t^{act}\\right)=", format(pnorm(as.numeric(input$t.act)), digits=2, nsmall=2), "$$<span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
    } else if (input$test==c("one_sid_leq")) {
      paste0("<span style='text-decoration: none; font-size: 14pt'> Remember, in the case of a one-sided less or equal test we compute: $$p\\text{-value}=1-\\Phi\\left(t^{act}\\right)=", format(1-pnorm(as.numeric(input$t.act)), digits=2, nsmall=2), "$$<span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
    }
    

  })
  
}

shinyApp(ui = ui, server = server)
