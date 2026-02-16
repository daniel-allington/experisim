library(shiny)
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

report.apa.pval <- function(p) {
  if(p >= .001) {
    return(
      paste0(
        '<i>p</i> = ',
        p %>%
          format.pval(
            digits = 3,
            nsmall = 3,
            eps = .001
          )
        )
      )
  }
  return('<i>p</i> < .001')
}

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
                        value = 2,
                        step = 2),
            actionButton('collect', 'Run experiment'),
            tableOutput('descriptive'),
            uiOutput('t.test')
        ),

        mainPanel(
           plotOutput('plotInitial', width = '400px', height = '200px'),
           plotOutput('plotGrouped', width = '400px', height = '200px'),
           plotOutput('plotFindings', width = '400px', height = '200px')
        )
    )
)

server <- function(input, output) {
  
  d.participants <- eventReactive(
    input$collect,
    {
    tibble(
      Group = c(
        rep('Treatment', input$n/2), rep('Control', input$n/2)), 
      Initial_score = rnorm(n = input$n, sd = input$sd),
      Final_score = Initial_score + ifelse(Group == 'Control', 0, input$effect)
    )
    }
  )
  
  ymax <- reactiveVal()
  
  results <- reactive(
    d.participants() %>%
      t.test(Final_score ~ Group == 'Control', data = .)
  )

    output$plotInitial <- renderPlot({
      
      initial.plot <- d.participants() %>%
        ggplot(
          aes(x = Initial_score)
        ) +
        geom_histogram() +
        scale_x_continuous(name = NULL, limits = c(-3,3)) +
        scale_y_continuous(
          name = NULL, 
          labels = scales::label_number(accuracy = 1)) +
        ggtitle('Intrinsic variation')
      
      ymax(ggplot_build(initial.plot)$data[[1]]$count %>% max)
      
      initial.plot
      
    }, width = 400, height = 200
    )
    
    output$plotGrouped <- renderPlot({
      
      d.participants() %>%
        ggplot(
          aes(x = Initial_score)
        ) +
        geom_histogram() +
        scale_x_continuous(name = NULL, limits = c(-3,3)) +
        scale_y_continuous(
          name = NULL, 
          limits = c(0, ymax()),
          labels = scales::label_number(accuracy = 1)) +
        facet_grid(cols = vars(Group)) +
        ggtitle('Intrinsic variation, grouped')
  
    }, width = 400, height = 200
    )
    
    output$plotFindings <- renderPlot({
      
      d.participants() %>%
        ggplot(
          aes(x = Final_score)
        ) +
        geom_histogram() +
        scale_x_continuous(name = NULL, limits = c(-3,3)) +
        scale_y_continuous(
          name = NULL, 
          limits = c(0, ymax()),
          labels = scales::label_number(accuracy = 1)) +
        facet_grid(cols = vars(Group)) +
        ggtitle('Intrinsic variation plus effect of treatment')
      
    }, width = 400, height = 200
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
          Group = Group %>% as.character,
          across(
            .cols = c(M, SD),
            .fns = ~ round(., digits = 2)
          )
        )
      
    })
    
    output$t.test <- renderUI({
      
      if(nrow(d.participants()) < 3) {return('')}
      
      HTML(
        paste0(
          '<i>t</i>(', results()$parameter %>% round(digits = 2),
          ') = ', results()$statistic %>% round(digits = 2),
          ', ', results()$p.value %>% report.apa.pval,
          '<br>95% CI[', results()$conf.int[1] %>% round(digits = 2),
          ', ', results()$conf.int[2] %>% round(digits = 2),
          ']'
        )
      )
      
    })
}

shinyApp(ui = ui, server = server)
