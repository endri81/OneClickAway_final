library(tidyverse)
measurech <- read_excel("data/measurech.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_measurech == "age") {
    data_dig <- measurech %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_measurech == "gender") {
    data_dig <- measurech %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_measurech == "total") {
    data_dig <- measurech %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Parental Technical mediation ", align = "center")),
  
  column(3,
         wellPanel(style = "background: #ffffff", 
                   pickerInput(
                     inputId = "ind_measurech",
                     label = "Select metrics", 
                     choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
                     options = list(
                       style = "btn-primary")
                   )       
         )),
  
  column(3),
  
  column(9,
         
         wellPanel(style = "background: #ffffff", 
                   plotlyOutput("measurech")
         )
  )
  
))




output$measurech <- renderPlotly({
  
  
  if (input$ind_measurech == "age")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~age_value, 
                   y = ~Answer, 
                   type = 'bar', 
                   color = ~age_group,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Rules about how long or when my child is allowed to go online",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (68% of target population)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) 
    
  }         
  else if (input$ind_measurech == "gender")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~gender_value, 
                   y = ~Answer, 
                   type = 'bar', 
                   color = ~gender,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Rules about how long or when my child is allowed to go online",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (68% of target population)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) 
  }
  else  {
    fig <- plot_ly(data = data_digital(),
                   x = ~Total, 
                   y = ~Answer, 
                   type = 'bar', 
                   color = ~Total,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Rules about how long or when my child is allowed to go online",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (68% of target population)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          ))
    
  }
  
  
})
