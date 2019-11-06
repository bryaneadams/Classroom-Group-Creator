library(shinydashboard)
library(stringr)
library(tidyverse)

ui <- dashboardPage(
   
  skin = "yellow",
  
  dashboardHeader(title = "Creating Groups for Projects"),
  dashboardSidebar(
    textAreaInput("names", "Paste your names here","", height = 500),
    textInput("hour", "What Hour?"),
    downloadButton("downloadGroups", "Download Groups")
    
  ),
  dashboardBody(
    fluidRow(box(title = "Class Groups",
                 status = "warning",
                 solidHeader = T,
                 width = 12,
                 collapsible = T,
                 collapsed = F,
                 dataTableOutput("dataframe"))))
)


server <- function(input, output) {
  
  group_output = reactive({
   
    cadet_names<-str_match_all(input$names, "[ \\S]{3,}")
    
    names_df = as.data.frame(cadet_names)
    
    names(names_df) = ("names")
    
    class_size = nrow(names_df)
    
    if((class_size %% 2) == 0){
    
    cadet_one = sample(names_df$names, class_size/2)
    
    cadet_two = names_df%>%
      filter(!names %in% cadet_one)
    
    df = bind_cols(as.data.frame(cadet_one), cadet_two)
    
    names(df)  = c("Cadet One", "Cadet Two")
    
    }
    else{
      
      cadet_one = sample(names_df$names, floor(class_size/2))
      
      cadet_two = names_df%>%
        filter(!names %in% cadet_one)%>%
        slice(-1)
      
      cadet_three = names_df%>%
        filter(!names %in% cadet_one)%>%
        filter(!names %in% cadet_two$names)%>%
        add_row(names = rep("",floor(class_size/2)-1))
      
      df = bind_cols(as.data.frame(cadet_one), cadet_two,cadet_three)
      
      names(df)  = c("Cadet One", "Cadet Two", "Cadet Three")
   
    }
    
    df
    
  })
  
  output$dataframe <- renderDataTable({
    
    group_output()
    
  })
  
  output$downloadGroups <- downloadHandler(
    filename = function() {
      paste(input$hour, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(group_output(), file, row.names = FALSE)
    }
  )
  

}


shinyApp(ui = ui, server = server)

