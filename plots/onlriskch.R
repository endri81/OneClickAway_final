library(tidyverse)
onlriskch <- read_excel("data/onlriskch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- onlriskch %>% 
      gather("Frequency", "Value", "No" ,"Yes", "Dont know", "Prefer not to say" )

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Parental perception of online risks for children  ", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("onlriskch")
      )


))




output$onlriskch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Means, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "As far as you are aware, in the past year, has your child seen a website <br /> or an online discussion where people talk about any of these things?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Total target population"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
   
})
