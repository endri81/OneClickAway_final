library(tidyverse)
cursourcepar <- read_excel("data/cursourcepar.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_cursourcepar == "age") {
    data_dig <- cursourcepar %>% select("Source", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_cursourcepar == "gender") {
    data_dig <- cursourcepar %>% select("Source", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_cursourcepar == "total") {
    data_dig <- cursourcepar %>% select("Source", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Current sources of support for parents", align = "center")),
  
  column(3,
         wellPanel(style = "background: #ffffff", 
                   pickerInput(
                     inputId = "ind_cursourcepar",
                     label = "Select metrics", 
                     choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
                     options = list(
                       style = "btn-primary")
                   )       
         )),
  
  column(3),
  
  column(9,
         
         wellPanel(style = "background: #ffffff", 
                   plotlyOutput("cursourcepar")
         )
  )
  
))




output$cursourcepar <- renderPlotly({
  
  
  if (input$ind_cursourcepar == "age")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~age_value, 
                   y = ~Source, 
                   type = 'bar', 
                   color = ~age_group,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "In general, where do you get information and advice on how to help <br /> and support your child on the internet and keep her/him safe?",
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
    
  }         
  else if (input$ind_cursourcepar == "gender")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~gender_value, 
                   y = ~Source, 
                   type = 'bar', 
                   color = ~gender,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "In general, where do you get information and advice on how to help <br /> and support your child on the internet and keep her/him safe?",
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
  }
  else  {
    fig <- plot_ly(data = data_digital(),
                   x = ~Total, 
                   y = ~Source, 
                   type = 'bar', 
                   color = ~Total,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "In general, where do you get information and advice on how to help <br /> and support your child on the internet and keep her/him safe?",
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
    
  }
  
  
})
