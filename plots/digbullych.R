library(tidyverse)
digbullych <- read_excel("data/digbullych.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- digbullych %>% 
      gather("Frequency", "Value", "No", "Yes", "Prefer not to say")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Children bullied in person and via a digital device", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("digbullych")
      )


))




output$digbullych <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Treatment, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "If someone has treated you in this way, how has it happened?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who were mistreated, harassed or made fun of them repeatedly (on purpose to hurt them) (6% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
   
})
