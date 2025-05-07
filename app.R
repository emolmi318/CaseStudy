##########################

# Download the required packages
library(shiny)
library(vroom)
library(tidyverse)

# Download the required data
dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), 
                paste0("neiss/", name), 
                quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom(
  "neiss/injuries.tsv.gz")
injuries


products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom(
  "neiss/population.tsv")
population

selected <- injuries %>% filter(
  prod_code == 649)
nrow(selected)

selected %>% count(
  location, wt = weight, sort = TRUE)

selected %>% count(
  body_part, wt = weight, sort = TRUE)

selected %>% count(
  diag, wt = weight, sort = TRUE)

summary <- selected %>% 
  count(age, sex, wt = weight)
summary

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(
    population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), 
                         n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(
      fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

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
  numericInput( # Question 3
    "n_rows", "Number of rows to display:", value = 5, min = 1, max = 20),
  fluidRow( # Will show in table output
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
    
  ),
  fluidRow( # Interactive plot
    column(12, plotOutput("age_sex"))
  ),
  fluidRow( # Question 4
    column(2, actionButton("prev_story", label = tagList(icon("arrow-left"),"Previous"))),
    column(2, 
           actionButton("next_story", label = tagList(icon("arrow-right"), "Next")))
           ,
    column(8, textOutput("narrative"))
  ),
  downloadButton("download_summary", "Download Summary") # New Stuff
  
  
)

# Define server logic 
server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(
    prod_code == input$code)) # reactive helps reduce the code complexity
  
## Output for each column of the table - Altered code to make it pretty
  output$diag <- renderTable({
    count_top(selected(), diag, n = input$n_rows) #input$n_rows goes with the numberInput() in the ui
  }, striped = TRUE, hover = TRUE)
  
  output$body_part <- renderTable({
    count_top(selected(), diag, n = input$n_rows)
  }, striped = TRUE, hover = TRUE)
  output$location <- renderTable({
    count_top(selected(), diag, n = input$n_rows)
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
  
## Store the list of narratives and current index
  narratives <- reactive({
    selected() %>% pull(narrative)
  })
  current_index <- reactiveVal(1)

## Question 4 
  observeEvent(input$next_story, {
    if (current_index() < length(narratives())) {
      current_index(current_index() + 1)
    }
  })
  
  observeEvent(input$prev_story, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
  
  output$narrative <- renderText({
    if (length(narratives()) == 0) return("No narratives available.")
    narratives()[current_index()]
  })
  
  
  output$download_summary <- downloadHandler( # New stuff
    filename = function() {
      paste("summary-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summary(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)