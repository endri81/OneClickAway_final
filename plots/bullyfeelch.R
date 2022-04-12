library(tidyverse)
bullyfeelch <- read_excel("data/bullyfeelch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_bullyfeelch == "age") {
    data_dig <- bullyfeelch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_bullyfeelch == "gender") {
    data_dig <- bullyfeelch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_bullyfeelch == "total") {
    data_dig <- bullyfeelch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Children's feeling towards being bullied (treated in a hurtful or nasty way)", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_bullyfeelch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("bullyfeelch")
      )
   )

))




output$bullyfeelch <- renderPlotly({
  
    
    if (input$ind_bullyfeelch == "age")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~age_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~age_group,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "Thinking of the last time someone treated you in a hurtful or nasty way, how did you feel?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who were mistreated, harassed or made fun of them repeatedly (on purpose to hurt them) online or via a mobile device (3% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
    }         
    else if (input$ind_bullyfeelch == "gender")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~gender_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~gender,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "Thinking of the last time someone treated you in a hurtful or nasty way, how did you feel?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who were mistreated, harassed or made fun of them repeatedly (on purpose to hurt them) online or via a mobile device (3% of target population)"),
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
        layout(title= "Thinking of the last time someone treated you in a hurtful or nasty way, how did you feel?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: those who were mistreated, harassed or made fun of them repeatedly (on purpose to hurt them) online or via a mobile device (3% of target population)"),
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
