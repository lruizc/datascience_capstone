library(shiny)

model <- readRDS("ngrams.rds")

predict_next <- function(text, n = 3) {
  words <- tolower(trimws(text))
  words <- gsub("[^a-z ]", " ", words)
  words <- strsplit(trimws(gsub("\\s+", " ", words)), " ")[[1]]
  words <- words[nchar(words) > 0]

  if (length(words) == 0) return(model$top_words[1:n])

  last <- words[length(words)]

  if (length(words) >= 2) {
    prefix <- paste(words[length(words) - 1], last)
    res <- model$trigrams[[prefix]]
    if (!is.null(res)) return(head(res, n))
  }

  res <- model$bigrams[[last]]
  if (!is.null(res)) return(head(res, n))

  model$top_words[1:n]
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', sans-serif; background-color: #f0f2f5; }
    .container-fluid { max-width: 800px; margin: auto; padding-top: 30px; }
    h2 { color: #2c3e50; }
    .well { background: #ffffff; border: none; box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-radius: 10px; }
    .btn-primary { background-color: #2980b9; border-color: #2980b9; }
    .btn-primary:hover { background-color: #1f6391; }
    .chip {
      display: inline-block; background: #2980b9; color: #fff;
      padding: 10px 22px; border-radius: 25px; margin: 6px 4px;
      font-size: 18px; font-weight: 500; letter-spacing: 0.3px;
    }
    .result-box { background: #fff; padding: 24px; border-radius: 10px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.08); min-height: 80px; margin-top: 10px; }
    p.hint { color: #888; font-size: 13px; }
  "))),
  titlePanel("Next Word Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Enter a phrase:", value = "", placeholder = "e.g. I would like to"),
      actionButton("go", "Predict", class = "btn-primary"),
      br(), br(),
      p(class = "hint", "Type any phrase in English and click Predict.")
    ),
    mainPanel(
      div(class = "result-box",
        h4("Top suggestions:"),
        uiOutput("result")
      )
    )
  )
)

server <- function(input, output) {
  preds <- eventReactive(input$go, {
    req(nchar(trimws(input$phrase)) > 0)
    predict_next(input$phrase)
  })

  output$result <- renderUI({
    p <- preds()
    if (is.null(p)) return(p("Enter a phrase and click Predict."))
    tags$div(lapply(p, function(w) tags$span(class = "chip", w)))
  })
}

shinyApp(ui, server)
