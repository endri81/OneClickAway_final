library(tidyverse)
sexplexch <- read_excel("data/sexplexpch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_sexplexch == "age") {
    data_dig <- sexplexch %>% select("Places", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_sexplexch == "gender") {
    data_dig <- sexplexch %>% select("Places", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_sexplexch == "total") {
    data_dig <- sexplexch %>% select("Places", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Places title
  
   titlePanel(h1("Places of children’s exposure to sexual content online", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_sexplexch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("sexplexch")
      )
   )

))




output$sexplexch <- renderPlotly({
  
    
    if (input$ind_sexplexch == "age")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~age_value, 
                     y = ~Places, 
                     type = 'bar', 
                     color = ~age_group,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time you saw images of this kind, where did you see them?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who have been exposed to sexual content in the ways listed (14% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
    }         
    else if (input$ind_sexplexch == "gender")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~gender_value, 
                     y = ~Places, 
                     type = 'bar', 
                     color = ~gender,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time you saw images of this kind, where did you see them?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who have been exposed to sexual content in the ways listed (14% of target population)"),
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
                     y = ~Places, 
                     type = 'bar', 
                     color = ~Total,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time you saw images of this kind, where did you see them?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who have been exposed to sexual content in the ways listed (14% of target population)"),
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
