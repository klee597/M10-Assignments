library(shiny)
library(tidyverse)


# Load your cleaned trust dataset
trust_clean <- read_csv("MEin3D_TrustSurvey_CLEAN.csv")

# Lookup table for trust item labels
trust_labels <- tibble(
  item = c("trust_1","trust_2","trust_3","trust_4","trust_5",
           "trust_6","trust_7","trust_8","trust_9","trust_10"),
  question = c(
    "I trust this brand.",
    "This email feels genuine.",
    "This email shows that the brand understands my needs.",
    "The information in this email feels accurate and reliable.",
    "I feel comfortable with how this brand uses my data in this email.",
    "This email is transparent about why I am receiving it.",
    "This email makes me feel valued as a customer.",
    "I believe this brand uses my information responsibly.",
    "This email increases my confidence in this brand.",
    "I would be willing to receive more emails like this from the brand."
  )
)

# Reshape data
trust_items_long <- trust_clean %>%
  select(condition, starts_with("trust_")) %>%
  pivot_longer(cols = starts_with("trust_"), names_to = "item", values_to = "score") %>%
  left_join(trust_labels, by = "item")

# UI
ui <- fluidPage(
  titlePanel("Interactive Trust Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("condition", "Select Condition:",
                  choices = c("ai", "control"), selected = "ai", multiple = TRUE),

      selectInput("trust_item", "Select Trust Items:",
                  choices = trust_labels$question,
                  selected = trust_labels$question[1:3],
                  multiple = TRUE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Average Scores",
                 plotOutput("avgPlot", height = "500px")
        ),
        tabPanel("Distribution Explorer",
                 plotOutput("distPlot", height = "500px")
        ),
        tabPanel("Summary Table",
                 tableOutput("summaryTable")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {

  filtered_data <- reactive({
    trust_items_long %>%
      filter(condition %in% input$condition,
             question %in% input$trust_item)
  })

  # Tab 1: Average Scores
  output$avgPlot <- renderPlot({
    filtered_data() %>%
      group_by(question, condition) %>%
      summarize(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = question, y = mean_score, fill = condition)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      coord_flip() +
      labs(title = "Average Trust Scores",
           x = "Trust Question",
           y = "Average Score (1–5)") +
      theme_minimal(base_size = 14)
  })

  # Tab 2: Distribution Explorer
  output$distPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = score, fill = condition)) +
      geom_density(alpha = 0.4) +
      labs(title = "Distribution of Trust Scores",
           x = "Score",
           y = "Density") +
      theme_minimal(base_size = 14)
  })

  # Tab 3: Summary Table
  output$summaryTable <- renderTable({
    filtered_data() %>%
      group_by(question, condition) %>%
      summarize(
        mean = round(mean(score, na.rm = TRUE), 2),
        sd = round(sd(score, na.rm = TRUE), 2),
        n = n(),
        .groups = "drop"
      )
  })
}

shinyApp(ui = ui, server = server)
