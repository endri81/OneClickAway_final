library(tidyverse)
sexexpmeanch <- read_excel("data/sexexpmeanch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- sexexpmeanch %>% 
      gather("Frequency", "Value", "No", "Yes", "Prefer not to say")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Means through which children are exposed to sexual content online in the last year", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("sexexpmeanch")
      )


))




output$sexexpmeanch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Means, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "Did you see the images of this kind on any of the following",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who saw the images of this kind via a mobile phone, computer, tablet or any other online device (12% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
   
})
