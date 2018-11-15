
library(shiny)
library(DT)
library(ggvis)


movies_data <- data.table::fread("final.csv")

ui <-fluidPage(
        headerPanel("WHAT SHOULD I WATCH TONIGHT?"), 
        sidebarLayout(
                sidebarPanel(
                        selectInput("actor", "Actor", 
                                    choices = c("Any", actor_list), 
                                    selected = 'Bill Murray'),
                        sliderInput(inputId = "length", 
                                    label = "Maximum length", 
                                    min = 0, 
                                    max = 900, 
                                    value = 120, 
                                    step = 30)
                ), 
                mainPanel(
                        textOutput("text"),
                        tabsetPanel(tabPanel("Plot", ggvisOutput("plot")),
                                    tabPanel("Table",
                                             dataTableOutput("table")))
                )))


server <- function(input, output){
        
       movies <- reactive ({ 
                movies <- movies_data %>% 
                filter(runtimeMinutes <= input$length) 
       
                if(input$actor != "Any") {
                movies <- movies %>% 
                        filter(actors == input$actor)} 
                
                movies <- as.data.frame(movies)
                })
        
        output$table <- renderDataTable({
                movies()})
        
        tooltip <- function(data){
                if(is.null(data)) return(NULL)
                paste0("<b>",data$primaryTitle, "</b><br>",
                       data$averageRating, "<br>", 
                       data$awards)
        }
                
        vis <- reactive({
                movies %>% 
                        ggvis(~averageRating, ~numVotes/1000, key := ~primaryTitle) %>%
                        layer_points(fill = ~awards) %>%
                        add_tooltip(tooltip, "hover")
        })  
        
        vis %>% bind_shiny("plot")
      
}


shinyApp(ui = ui, server = server)
