#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

future_value = function(amount, rate, years){
  result = amount
  for (i in 1:years){
    result = result * (1 + rate)
  }
  return(result)
}

annuity = function(contrib, rate, years){
  temp = 1
  for (i in 1:years){
    temp = temp * (1 + rate)
  }
  return(contrib * (temp - 1) / rate)
}

growing_annuity = function(contrib, rate, growth, years){
  temp1 = 1
  temp2 = 1
  for (i in 1:years){
    temp1 = temp1 * (1 + rate)
    temp2 = temp2 * (1 + growth)
  }
  return(contrib * (temp1 - temp2) / (rate - growth))
}

display_plot = function(amount, contrib, rate, growth, years){
  year = c(0)
  no_contrib = c(amount)
  fixed_contrib = c(amount)
  growing_contrib = c(amount)
  rate_in_percent = 0.01 * rate
  growth_in_percent = 0.01 * growth
  for (i in 1:years){
    base = future_value(amount, rate_in_percent, i)
    fixed = annuity(contrib, rate_in_percent, i)
    growing = growing_annuity(contrib, rate_in_percent, growth_in_percent, i)
    year = c(year, i)
    no_contrib = c(no_contrib, base)
    fixed_contrib = c(fixed_contrib, base + fixed)
    growing_contrib = c(growing_contrib, base + growing)
  }
  result <- data.frame(year = year, 
                       value = c(no_contrib = no_contrib,
                                 fixed_contrib = fixed_contrib,
                                 growing_contrib = growing_contrib),
                       variable = c(rep("no_contrib", length(year)),
                                    rep("fixed_contrib", length(year)),
                                    rep("growing_contrib", length(year)))
                       )
  result$levels <- factor(result$variable, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
  return(result)
}

display_table = function(amount, contrib, rate, growth, years){
  year = c(0)
  no_contrib = c(amount)
  fixed_contrib = c(amount)
  growing_contrib = c(amount)
  rate_in_percent = 0.01 * rate
  growth_in_percent = 0.01 * growth
  for (i in 1:years){
    base = future_value(amount, rate_in_percent, i)
    fixed = annuity(contrib, rate_in_percent, i)
    growing = growing_annuity(contrib, rate_in_percent, growth_in_percent, i)
    year = c(year, i)
    no_contrib = c(no_contrib, base)
    fixed_contrib = c(fixed_contrib, base + fixed)
    growing_contrib = c(growing_contrib, base + growing)
  }
  result <- data.frame(year = year, 
                       value = c(no_contrib = no_contrib,
                                 fixed_contrib = fixed_contrib,
                                 growing_contrib = growing_contrib),
                       variable = c(rep("no_contrib", length(year)),
                                    rep("fixed_contrib", length(year)),
                                    rep("growing_contrib", length(year)))
  )
  result <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
  return(result)
}

ui <- fluidPage(

   titlePanel("Savings Simulation"),
   
   fluidRow(
     column(4,
       sliderInput("amount", "Initial Amount",
                   min = 1, max = 100000,
                   value = 1000, step = 20000,
                   pre = "$", sep = ",")
     ),
     column(4,
            sliderInput("rate", "Return Rate(in %)",
                        min = 0, max = 20,
                        value = 5, step = 2)
     ),
     column(4,
            sliderInput("years", "Years",
                        min = 0, max = 50,
                        value = 10, step = 5)
     )
   ),
   
   fluidRow(
     column(4,
            sliderInput("contrib", "Annual Contribution",
                        min = 0, max = 50000,
                        value = 2000, step = 10000,
                        pre = "$", sep = ",")
     ),
     column(4,
            sliderInput("growth", "Growth Rate(in %)",
                        min = 0, max = 20,
                        value = 2, step = 2)
     ),
     column(4,
            selectInput("facet", "Facet?",
                        choices = c("No", "Yes"))
     )
   ),
   
   mainPanel("Timelines",
     plotOutput("distPlot"),
     "Balances",
     tableOutput("distTable")
   )
)

server <- function(input, output) {
   output$distPlot <- renderPlot(width = 920, {
     result <- display_plot(input$amount, input$contrib, input$rate, input$growth, input$years)
     if (input$facet == "No"){
       ggplot(data = result, aes(x = year, y = value, color = variable)) +
         geom_line() +
         geom_point(cex = 0.5) +
         labs(x = "years", y = "value", title = "Three modes of investing")
     }
     else{
       ggplot(data = result, aes(x = year, y = value, color = variable)) +
         geom_line() +
         geom_area(aes(fill = variable), alpha = 0.2) +
         geom_point(cex = 0.5) +
         labs(x = "years", y = "value", title = "Three modes of investing") +
         facet_wrap(~levels)
     }
   })
   
   output$distTable <- renderTable(width = 920, {
     result <- display_table(input$amount, input$contrib, input$rate, input$growth, input$years)
     head(result, 10)
   })
}

shinyApp(ui = ui, server = server)

