library(tidyverse)
sexintch <- read_excel("data/sexintch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- sexintch %>% 
      gather("Frequency", "Value", "No", "Yes", "Prefer not to say")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("How children came across sexual content online", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("sexintch")
      )


))




output$sexintch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "Thinking about the last year, has any of these things happened?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who have ever seen any images on the internet that are obviously sexual in the past year (16% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
      
})
