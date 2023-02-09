
library(shiny)
library(tidyverse)
library(janitor)
library(stopwords)
library(joeyr)

full_df <- read_csv("joey_darla.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  select(speaker_id = name, everything(),
         -matches("lobanov"), -b1, -b2, -b3,-t, -beg, -end, -matches("word_trans"), -matches("_word"), -n_formants) %>%
  rename_with(ucfirst, matches("f\\d")) %>%
  rename(phoneme = vowel) %>%
  
  # Reshape 
  select(-F1, -F2, -F3) %>%
  pivot_longer(cols = matches("_percent"), 
               names_to = c(".value", "percent"), 
               names_pattern = "(F\\d)_(\\d\\d)") %>%
  mutate(percent = as.numeric(percent)) %>%
  filter(!is.na(F1), !is.na(F2)) %>%
  
  # OoO1 Allophones
  mutate(phoneme = arpa_to_wells(phoneme)) %>%
  code_allophones(phoneme, .fol_seg = fol_seg, .pre_seg = pre_seg) %>%
  
  # OoO2 Outliers
  group_by(speaker_id, phoneme) %>%
  filter(!find_outliers(F1, F2)) %>%
  ungroup() %>%
  
  # OoO3 Normalization
  group_by(speaker_id, phoneme) %>%
  mutate(across(.cols = c(F1, F2), 
                .fns = c(`z` = scale, `log` = log10),
                .names = "{.col}_{.fn}")) %>%
  # ungroup() %>%
  # norm_logmeans(c(F1_log, F2_log),
  #               .speaker_col = speaker_id,
  #               .vowel_col = phoneme) %>%
  
  # OoO4 Remove other data
  print()
  
vowels <- c("FLEECE", "KIT", "FACE", "DRESS", "TRAP", "LOT", "THOUGHT", "STRUT", "GOAT", "FOOT", "GOOSE", "PRICE", "MOUTH", "CHOICE", "NURSE")
stopwords <- stopwords(source = "marimo")
allwords <- sort(unique(full_df$word))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("LING 580R Sociolinguistic Fieldwork: Acoustic Analysis"),
  
  # sidebarLayout(
  
  # sidebarPanel(),
  
  # mainPanel(
  
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "Main vowel plot",
      
      
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          title = "Data",
          fileInput("raw_data", "Choose a DARLA-generated csv file",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
        ),
        
        tabPanel(
          title = "Vowels",
        ),
        
        tabPanel("Words",
                 fluidRow(
                   column(3,
                          radioButtons("include_exclude",
                                       label = h3(""),
                                       choices = c("Exclude these words"   = "hide",
                                                   "Only show these words" = "show"),
                                       width = "100%"),
                          # actionButton("stopwords_btn",
                          #              label = "Default stop words",
                          #              width = "100%"),
                          # actionButton("clear_words_btn",
                          #              label="Clear list",
                          #              width = "100%")
                   ),
                   column(9,
                          fluidRow(
                            column(12,
                                   selectInput("wordlist",
                                               label = h3(""),
                                               choices  = allwords,
                                               selected = stopwords,
                                               multiple  = TRUE,
                                               selectize = TRUE,
                                               width = "100%"
                                   )
                            )
                          )
                   )
                 )
        ),
        
        
        tabPanel(
          title = "Plot",
          
          fluidRow(
            column(12,
                   column(3,
                          checkboxInput(inputId = "show_points",
                                        label   = h3("Points"),
                                        value   = 1),
                          sliderInput(inputId = "points_alpha",
                                      label = "Opacity",
                                      min = 0,
                                      max = 1,
                                      value = 1,
                                      width="100%"),
                          sliderInput(inputId = "points_size",
                                      label = "Size",
                                      min = 0.01,
                                      max = 10,
                                      value = 0.25,
                                      round = 1,
                                      width="100%")
                   ),
                   column(3,
                          checkboxInput(inputId = "show_ellipses",
                                        label   = h3("Ellipses"),
                                        value   = T),
                          sliderInput(inputId = "ellipses_alpha",
                                      label = "Opacity",
                                      min = 0,
                                      max = 1,
                                      value = 1,
                                      width="100%"),
                          sliderInput(inputId = "ellipses_size",
                                      label = "Size",
                                      min = 1,
                                      max = 100,
                                      value = 67,
                                      post = "%",
                                      width="100%")
                   ),
                   column(3,
                          checkboxInput(inputId = "show_means",
                                        label   = h3("Means"),
                                        value   = TRUE),
                          sliderInput(inputId = "means_alpha",
                                      label = "Opacity",
                                      min = 0,
                                      max = 1,
                                      value = 1,
                                      width="100%"),
                          sliderInput(inputId = "means_size",
                                      label = "Size",
                                      min = 2,
                                      max = 20,
                                      value = 10,
                                      width="100%")
                   ),
                   column(3,
                          checkboxInput(inputId = "show_words",
                                        label   = h3("Words"),
                                        value   = FALSE),
                          sliderInput(inputId = "words_alpha",
                                      label = "Opacity",
                                      min = 0,
                                      max = 1,
                                      value = 1,
                                      width="100%"),
                          sliderInput(inputId = "words_size",
                                      label = "Size",
                                      min = 0.01,
                                      max = 10,
                                      value = 3,
                                      width="100%")
                   )
            )
          ),
        ),
        
        
        tabPanel(
          title = "Customization"
        ),
        
        tabPanel(
          title = "Download"
        )
      ),
    plotOutput("midpoints_plot", width = "800px", height = "600px")
    ),
    
    # tabPanel(
    #   title = "Allophones",
    #   sidebarLayout(
    #     sidebarPanel(
    #       
    #     ),
    #     mainPanel(
    #     )
    #   )
    # ),
    # tabPanel(
    #   title = "Trajectories",
    #   sidebarLayout(
    #     sidebarPanel(
    #       
    #     ),
    #     mainPanel(
    #       
    #     )
    #   )
    # ),
    # tabPanel(
    #   title = "Other",
    #   sidebarLayout(
    #     sidebarPanel(
    #       
    #     ),
    #     mainPanel(
    #       
    #     )
    #   )
    # ),
    # )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # full_df <- reactive({
  #   raw_data %>%
  #     
  # })
  
  
  output$midpoints_plot <- renderPlot({
    midpoint_df <- full_df %>%
      filter(percent == 50)
    
    # Basic elements
    p <- ggplot(midpoint_df, aes(F2, F1, color = phoneme))
    
    # Optional elements
    if (input$show_points) {
      p <- p +
        geom_point(size = input$points_size, alpha = input$points_alpha)
    }
    if (input$show_ellipses) {
      p <- p +
        stat_ellipse(level = input$ellipses_size/100, alpha = input$ellipses_alpha)
    }
    if (input$show_means) {
      midpoint_labels <- midpoint_df %>%
        group_by(phoneme) %>%
        summarize(across(matches("F\\d"), mean), .groups = "drop_last")
      p <- p +
        geom_text(data = midpoint_labels, aes(label = phoneme), size = input$means_size, alpha = input$means_alpha)
    }
    
    # Final elements
    p <- p +
      scale_x_reverse() +
      scale_y_reverse() +
      theme_minimal()
    
    p
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
