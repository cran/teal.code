## -----------------------------------------------------------------------------
library(teal.code)

# can be created without any code/environment
empty_qenv <- new_qenv()
print(empty_qenv)

# or can be created with objects inside the environment
my_qenv <- new_qenv(env = list2env(list(x = 5)), code = "x <- 5")
print(my_qenv)

## -----------------------------------------------------------------------------
library(magrittr)

q2 <- eval_code(my_qenv, "y <- x * 2") %>% eval_code("z <- y * 2")

# my_qenv still contains only x
print(my_qenv)

# q2 contains x, y and z
print(q2)

## -----------------------------------------------------------------------------
print(q2[["y"]])

cat(paste(get_code(q2), collapse = "\n"))

## -----------------------------------------------------------------------------
common_q <- eval_code(new_qenv(), quote(x <- 1))

x_q <- eval_code(common_q, quote(y <- 5))
y_q <- eval_code(common_q, quote(z <- 5))

join_q <- join(x_q, y_q)

print(join_q)

## -----------------------------------------------------------------------------
q_message <- eval_code(new_qenv(), quote(message("this is a message")))
q_message@messages

q_warning <- eval_code(new_qenv(), quote(warning("and this is a warning")))
q_warning@warnings

## -----------------------------------------------------------------------------
q_message@warnings
q_warning@messages

## -----------------------------------------------------------------------------
library(shiny)
library(magrittr)
# create an initial qenv with the data in
data_q <- new_qenv() %>% eval_code("iris_data <- iris")

ui <- fluidPage(
  radioButtons(
    "option", "Choose a column to plot:",
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "error_option")
  ),
  verbatimTextOutput("rcode"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  # create a qenv containing the reproducible output
  output_q <- reactive({
    req(input$option)
    eval_code(
      data_q,
      bquote(p <- hist(iris_data[, .(input$option)]))
    )
  })

  # display output
  output$plot <- renderPlot(output_q()[["p"]])
  # display code
  output$rcode <- renderText(get_code(output_q()))
}

if (interactive()) {
  shinyApp(ui, server)
}

