library(shiny)
library(tibble)
library(dplyr)

gen_pdmx <- function(labels = sample(setdiff(LETTERS, c("F", "I", "N", "O")), 4, FALSE)) {
  repeat {
    mx <- matrix(sample(1:9, 16, TRUE) * sample(c(-.1, .1), 16, TRUE),
                 nrow = 4, ncol = 4)
    diag(mx) <- 1L
    mx[2:4, 1] <- mx[1, 2:4]
    mx[3:4, 2] <- mx[2, 3:4]
    mx[4, 3] <- mx[3, 4]
    if (!any(eigen(mx)$value < 0)) break;
  }
  orig <- mx
  sds <- sample(1:9)
  for (i in 1:4) {
    for (j in 1:4) {
      mx[i, j] <- mx[i, j] * sds[i] * sds[j]
    }
  }
  
  dimnames(mx) <- list(c(sort(labels)),
                       c(sort(labels)))
  mx
}


server <- function(input, output, session) {
  cmx <- reactive({
    input$new
    gen_pdmx() %>% as_tibble()
  })

  rv <- reactiveValues(cov = "", cor = "", sdv = "")

  v <- reactive({
    input$new
    list(cov = sample(1:4, 2, FALSE) %>% sort(),
         cor = sample(1:4, 2, FALSE) %>% sort(),
         sdv = sample(1:4, 1),
         is_sd = sample(c(TRUE, FALSE), 1))

  })

  output$cov_q <- renderText({
    paste0("What is the covariance between ",
           names(cmx())[v()$cov[1]], " and ",
           names(cmx())[v()$cov[2]], "?")
  })

  output$cor_q <- renderText({
    paste0("What is the correlation between ",
           names(cmx())[v()$cor[1]], " and ",
           names(cmx())[v()$cor[2]], "?")
  })

  output$sdv_q <- renderText({
    paste0("What is the ", 
           if (v()$is_sd) "standard deviation " else "variance ",
           "of ", names(cmx())[v()$sdv[1]], "?")
  })
  
  observeEvent(input$new, {
    rv$sdv <- ""
    rv$cor <- ""
    rv$cov <- ""
    updateTextInput(session, inputId = "cov",
                    value = "")
    updateTextInput(session, inputId = "cor",
                    value = "")
    updateTextInput(session, inputId = "sdv",
                    value = "")
  })

  observeEvent(input$check, {
    if (input$cov != "") {
      result <- cmx()[v()$cov[1], v()$cov[2]] %>% pull()
      if (near(as.numeric(input$cov), result)) {
        rv$cov <- "✔"
      } else {
        rv$cov <- "❌"
      }
    }

    if (input$cor != "") {
      var_value <- cmx()[v()$cor[1], v()$cor[2]] %>% pull()
      sd1 <- cmx()[v()$cor[1], v()$cor[1]] %>% pull() %>% sqrt()
      sd2 <- cmx()[v()$cor[2], v()$cor[2]] %>% pull() %>% sqrt()
      result <- var_value / (sd1 * sd2)
      if (near(as.numeric(input$cor), result)) {
        rv$cor <- "✔"
      } else {
        rv$cor <- "❌"
      }
    }
    
    if (input$sdv != "") {
      var_value <- cmx()[v()$sdv, v()$sdv] %>% pull()
      result <- if (v()$is_sd) sqrt(var_value) else var_value
      rv$sdv <- if (near(as.numeric(input$sdv), result)) {
                  "✔"
                } else {
                  "❌"
                }
    }
  })

  output$cor_check <- renderText({
    rv$cor
  })

  output$cov_check <- renderText({
    rv$cov
  })
 
  output$sdv_check <- renderText({
    rv$sdv
  })
  
  output$cvmx <- renderTable({
    bind_cols(tibble(` ` = c(names(cmx()))),
              cmx())
  })

}

ui <- fluidPage(

  fluidRow(
    tags$head(
           tags$style(type = "text/css",
                      "label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}") 
         ),
    column(4, style='background-color:#d0d0d0;min-width: 300px;',
           h4("Consider the variance-covariance matrix below."),
           br(),
           tableOutput("cvmx")
           )
  ),
  
  fluidRow(
    column(4, style='background-color:#ffffd0;min-width: 300px;',
           p("Enter your answers to two decimal places."),
           tags$table(
                  tags$tr(width = "100%",
                          tags$td(width = "75%", div(style = "font-size:16px;",
                                                     textOutput("cov_q"))),
                          tags$td(width = "15%", textInput(inputId = "cov", label = NULL)),
                          tags$td(width = "10%", uiOutput("cov_check"))
                          ),
                  tags$tr(width = "100%",
                          tags$td(width = "75%", tags$div(style = "font-size:16px;",
                                                          textOutput("cor_q"))),
                          tags$td(width = "15%", textInput(inputId = "cor", label = NULL)),
                          tags$td(width = "10%", uiOutput("cor_check"))
                          ),
                  tags$tr(width = "100%",
                          tags$td(width = "75%", tags$div(style = "font-size:16px;",
                                                          textOutput("sdv_q"))),
                          tags$td(width = "15%", textInput(inputId = "sdv", label = NULL)),
                          tags$td(width = "10%", textOutput("sdv_check"))
                          )
                ),
           )
  ),

  fluidRow(
    column(2, style='background-color:#ffffd0;min-width: 150px;',
           actionButton("check", "check my answers")),
    column(2, style='background-color:#ffffd0;min-width: 150px;',
           actionButton("new", "new problem")),
  )
)

shinyApp(ui = ui, server = server)
