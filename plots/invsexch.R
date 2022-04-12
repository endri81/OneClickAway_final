library(tidyverse)
invsexch <- read_excel("data/invsexch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_invsexch == "age") {
    data_dig <- invsexch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_invsexch == "gender") {
    data_dig <- invsexch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_invsexch == "total") {
    data_dig <- invsexch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Children involved in unwanted sexual experiences through the following persons", align = "center")),
  
  column(3,
         wellPanel(style = "background: #ffffff", 
                   pickerInput(
                     inputId = "ind_invsexch",
                     label = "Select metrics", 
                     choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
                     options = list(
                       style = "btn-primary")
                   )       
         )),
  
  column(3),
  
  column(9,
         
         wellPanel(style = "background: #ffffff", 
                   plotlyOutput("invsexch")
         )
  )
  
))




output$invsexch <- renderPlotly({
  
  
  if (input$ind_invsexch == "age")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~age_value, 
                   y = ~Answer, 
                   type = 'bar', 
                   color = ~age_group,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Who was the person who did this?",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who were asked for sexual information about themselves (3% of target population)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) 
    
  }         
  else if (input$ind_invsexch == "gender")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~gender_value, 
                   y = ~Answer, 
                   type = 'bar', 
                   color = ~gender,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Who was the person who did this?",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who were asked for sexual information about themselves (3% of target population)"),
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
      layout(title= "Who was the person who did this?",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who were asked for sexual information about themselves (3% of target population)"),
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
