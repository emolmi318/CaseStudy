##########################

# Required Packages
library(shiny)
library(shinythemes) # First time trying this!

# prod_code is referring to the primary product associated with the injury.
prod_codes <- setNames(products$prod_code, 
                       products$title)

# Define UI - ensure each part is in the same order.
## I ended up with the Y axis option on the bottom of the app since I originally placed it last.
ui <- fluidPage(
  theme = shinytheme("united"), # Many options to choose from
  fluidRow(
    column(8, # Option to choose product
           selectInput("code", "Product",
                       choices = setNames(
                         products$prod_code, 
                         products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c(
      "rate", "count"))) # Option to choose rate/count
  ),
  fluidRow( # Will show in table output
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
    
  ),
  fluidRow( # Interactive plot
    column(12, plotOutput("age_sex"))
  ),
  fluidRow( # Randomized "stories"
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
  
)

# Define server logic 
server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(
    prod_code == input$code)) # reactive helps reduce the code complexity
  
## Output for each column of the table - Altered code to make it pretty
  output$diag <- renderTable({
    count_top(selected(), diag)
  }, striped = TRUE, hover = TRUE)
  output$body_part <- renderTable({
    count_top(selected(), diag)
  }, striped = TRUE, hover = TRUE)
  output$location <- renderTable({
    count_top(selected(), diag)
  }, striped = TRUE, hover = TRUE)
  
## Creating object that holds information for plotting
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(
        population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

## Plot Output
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") + 
        theme_minimal(base_size = 14) # Makes a big difference
    }
  }, res = 96)
  
## Object for the story
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  
## Story Output
  output$narrative <- renderText(
    narrative_sample())
}

# Run the application 
shinyApp(ui = ui, server = server)