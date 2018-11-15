
library(shiny)

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
                        tabsetPanel(tabPanel("Plot", plotOutput("plot")),
                                    tabPanel("Table",tableOutput("table")))
                )))


server <- function(input, output){
                
                output$table <- renderTable({
                        
                        movies <- movies_data %>% 
                                filter(runtimeMinutes <= input$length) 
                        
                        if(input$actor != "Any") {
                                movies <- movies %>% 
                                        filter(actors == input$actor)}
                                      
                })
                
                output$plot <- renderPlot({
                        
                        movies <- movies_data %>% 
                                filter(runtimeMinutes <= input$length)
                        
                        if(input$actor != "Any") {
                                movies <- movies %>% filter(actors == input$actor)}
                        
                        plot(x = movies$averageRating, 
                             y = movies$numVotes/1000)
                        
                }) 
                
                
                
}


shinyApp(ui = ui, server = server)
