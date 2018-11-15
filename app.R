library(shiny)
library(ggvis)
library(tidyverse)
library(plotly)
library(shinythemes)
library(DT)


movies_data <- data.table::fread("movies.csv")

actor_list <- movies_data %>%
        group_by(actors) %>%
        tally() %>%
        filter(n > 10) %>%
        select(-n)

director_list <- movies_data %>%
        distinct(director, primaryTitle) %>%
        group_by(director) %>%
        tally() %>%
        filter(n > 10) %>%
        select(-n)

genres <- c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
            "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
            "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
            "Short", "Sport", "Thriller", "War", "Western")


ui <-fluidPage(
        headerPanel("WHAT SHOULD I WATCH TONIGHT?"), 
        sidebarLayout(
                sidebarPanel(
                        
                        sliderInput("year","Release Year", value = c(1970, 2018),
                                    min = min(movies_data$startYear, na.rm = T), 
                                    max = max(movies_data$startYear, na.rm = T)),
                        selectInput("actor", "Actor (select 'Any' if you are not picky)", 
                                    choices = c("Any", actor_list), 
                                    selected = 'Bill Murray'),
                        selectInput("director", "Director", 
                                    choices = c("Any", director_list), 
                                    selected = 'Any'),
                        selectInput("genre", "Genres",
                                    choices = genres),
                        checkboxGroupInput("adult", "Adult Content", 
                                           choices = c("Yes", "No"),
                                           selected = "No"), 
                        sliderInput("length", "Maximum length", 0, 931, 120, step = 30),
                        sliderInput("rating", "IMDB Rating", 1, 10, c(7, 10), step = 0.1),
                        downloadButton("downloadData", "Download")
                        
                ), 
                
                
                mainPanel(wellPanel(span("Number of movies selected:", strong(textOutput("text")))), 
                        tabsetPanel(tabPanel("Plot", ggvisOutput("plot")),
                                tabPanel("Table", DT::dataTableOutput("table")) )
                          
                          
)))


server <- function(input, output){
        
        mdata <- reactive({
                
                # data-----------------------------------------               
                movies <- movies_data %>%
                        filter( 
                                startYear >= input$year[1], 
                                startYear <= input$year[2],
                                runtimeMinutes <= input$length,
                                averageRating >= input$rating[1],
                                averageRating <= input$rating[2], 
                                isAdult == input$adult) 
                
                if(input$actor != "Any") {
                        movies <- movies %>% 
                                filter(actors == input$actor)}
                
                if(input$director != "Any") {
                        movies <- movies %>% 
                                filter(director == input$director)}
                
                if (input$genre != "All") {
                        
                        movies <- movies %>% 
                                filter(str_detect(genres, input$genre))}
                
                movies <- as.data.frame(movies)
                
                
        })
        
        ### plot ------------------
        
        tooltip <- function(data){
                if(is.null(data)) return(NULL)
                paste0("<b>",data$primaryTitle, "</b><br>",
                       data$averageRating, "<br>", 
                       data$awards)
        }
        
        vis <- reactive({
                mdata %>% 
                        ggvis(~averageRating, ~numVotes/1000, key := ~primaryTitle) %>%
                        layer_points(size := 100, size.hover := 200, 
                                     fillOpacity := 0.7, fillOpacity.hover := 0.9,
                                     fill = ~awards, key := ~primaryTitle) %>%
                        add_tooltip(tooltip, "hover") %>%
                        scale_nominal("fill", domain = c("Won", "Nominated", "None"),
                                      range = c("red","orange" , "#aaa"))
        })  
        
        vis %>% bind_shiny("plot")
        
        ### text -----------------        
        
        output$text <- renderText({
                nrow(mdata() %>%
                             distinct(tconst))
        })
        
        
        ### table ---------------
        
        output$table <- DT::renderDataTable({
                
                mdata()%>% 
                        arrange(desc(averageRating)) %>%
                        select(-c(tconst, isAdult, numVotes)) %>%
                        rename(Title = primaryTitle,
                               Year = startYear, 
                               Length = runtimeMinutes,
                               Genres = genres, 
                               Rating = averageRating,
                               Director = director, 
                               Oscars = awards
                        ) %>%
                        distinct(Title, Year, Length, 
                                 Genres, Rating, Director, Oscars)
        })
        
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste("data", ".csv", sep = "")
                },
                content = function(file) {
                        write.csv(mdata(), file, row.names = FALSE)
                }
                
        )
        
         observeEvent(input$clicks, {write_csv(mdata(), "data.csv", row.names = FALSE)})
        
        
}
shinyApp(ui = ui, server = server)


