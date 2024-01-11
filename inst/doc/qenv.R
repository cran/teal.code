## -----------------------------------------------------------------------------
library(teal.code)

# create a new qenv object
empty_qenv <- qenv()
print(empty_qenv)

## -----------------------------------------------------------------------------
library(magrittr)

# evaluate code in qenv
my_qenv <- eval_code(empty_qenv, "x <- 2")
print(my_qenv)
get_env(my_qenv)


q1 <- eval_code(my_qenv, "y <- x * 2") %>% eval_code("z <- y * 2")

# my_qenv still contains only x
print(my_qenv)
ls(get_env(my_qenv))

# q1 contains x, y and z
print(q1)
ls(get_env(q1))

## -----------------------------------------------------------------------------
q2 <- within(my_qenv, y <- x * 2) %>% within(z <- y * 2)
print(q2)

## -----------------------------------------------------------------------------
print(q2[["y"]])

cat(get_code(q2))

## -----------------------------------------------------------------------------
q <- qenv()
q <- eval_code(q, quote(i <- subset(iris, Species == "setosa")))
q <- eval_code(q, substitute(
  ii <- subset(iris, Species == species),
  env = list(species = "versicolor")
))
input_value <- "virginica"
q <- eval_code(q, substitute(
  iii <- subset(iris, Species == species),
  env = list(species = input_value)
))

summary(q[["i"]]$Species)
summary(q[["ii"]]$Species)
summary(q[["iii"]]$Species)

## -----------------------------------------------------------------------------
qq <- qenv()
qq <- within(qq, i <- subset(iris, Species == "setosa"))
qq <- within(qq, ii <- subset(iris, Species == species), species = "versicolor")
input_value <- "virginica"
qq <- within(qq, iii <- subset(iris, Species == species), species = input_value)

summary(qq[["i"]]$Species)
summary(qq[["ii"]]$Species)
summary(qq[["iii"]]$Species)

## -----------------------------------------------------------------------------
common_q <- eval_code(qenv(), quote(x <- 1))

x_q <- eval_code(common_q, quote(y <- 5))
y_q <- eval_code(common_q, quote(z <- 5))

join_q <- join(x_q, y_q)

print(join_q)
ls(get_env(join_q))

## -----------------------------------------------------------------------------
q_message <- eval_code(qenv(), quote(message("this is a message")))
q_message@messages

q_warning <- eval_code(qenv(), quote(warning("and this is a warning")))
q_warning@warnings

## -----------------------------------------------------------------------------
q_message@warnings
q_warning@messages

## -----------------------------------------------------------------------------
library(shiny)
library(magrittr)
# create an initial qenv with the data in
data_q <- qenv() %>% eval_code("iris_data <- iris")

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

