library(shiny)
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

ui <- fluidPage(

    titlePanel('Experiment Simulator'),

    sidebarLayout(
        sidebarPanel(
            sliderInput("effect",
                        'Effect size:',
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput('sd',
                        'Population SD:',
                        min = .1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput('n',
                        'No. of participants:',
                        min = 2,
                        max = 200,
                        value = 20,
                        step = 2),
            tableOutput('descriptive')
        ),

        mainPanel(
           plotOutput('plotInitial'),
           plotOutput('plotGrouped'),
           plotOutput('plotFindings')
        )
    )
)

server <- function(input, output) {
  
  d.participants <- reactive(
    tibble(
      Group = c(
        rep(0, input$n/2), rep(1, input$n/2)), 
      Initial_score = rnorm(n = input$n, sd = input$sd),
      Final_score = Initial_score + ifelse(Group == 0, 0, input$effect)
    )
  )

    output$plotInitial <- renderPlot({
      
      d.participants() %>%
        ggplot(
          aes(x = Initial_score)
        ) +
        geom_histogram() +
        scale_x_continuous(limits = c(-3,3))
      
    }
    )
    
    output$plotGrouped <- renderPlot({
      
      d.participants() %>%
        ggplot(
          aes(x = Initial_score)
        ) +
        geom_histogram() +
        scale_x_continuous(limits = c(-3,3)) +
        facet_grid(cols = vars(Group))
      
    }
    )
    
    output$plotFindings <- renderPlot({
      
      d.participants() %>%
        ggplot(
          aes(x = Final_score)
        ) +
        geom_histogram() +
        scale_x_continuous(limits = c(-3,3)) +
        facet_grid(cols = vars(Group))
      
    }
    )
    
    output$descriptive <- renderTable({
      
      d.participants() %>%
        group_by(Group) %>%
        summarise(
          n = n(),
          M = mean(Final_score),
          SD = sd(Final_score)
        ) %>% 
        mutate(
          across(
            .cols = c(M, SD),
            .fns = ~ round(., digits = 2)
          )
        )
      
    })
}

shinyApp(ui = ui, server = server)
