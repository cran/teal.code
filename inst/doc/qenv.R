## -----------------------------------------------------------------------------
library(teal.code)

# create a new qenv object
empty_qenv <- qenv()
print(empty_qenv)

## -----------------------------------------------------------------------------
# evaluate code in qenv
my_qenv <- eval_code(empty_qenv, "x <- 2")
print(my_qenv)

q1 <- eval_code(my_qenv, "y <- x * 2")
q1 <- eval_code(q1, "z <- y * 2")

# my_qenv still contains only x
print(my_qenv)
names(my_qenv)

# q1 contains x, y and z
print(q1)
names(q1)

## -----------------------------------------------------------------------------
q2 <- within(my_qenv, y <- x * 2)
q2 <- within(q2, z <- y * 2)
q2 <- within(q2, plot(z))
print(q2)

## -----------------------------------------------------------------------------
print(q2[["y"]])

print(get_outputs(q2)[[1]])

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

join_q <- c(x_q, y_q)

print(join_q)
names(join_q)

## -----------------------------------------------------------------------------
q_message <- eval_code(qenv(), quote(message("this is a message")))
get_messages(q_message)

q_warning <- eval_code(qenv(), quote(warning("and this is a warning")))
get_warnings(q_warning)

## ----eval=requireNamespace("shiny")-------------------------------------------
library(shiny)
# create an initial qenv with the data in
data_q <- qenv()
data_q <- eval_code(data_q, "iris_data <- iris")

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
    within(
      data_q,
      p <- hist(iris_data[, .(input$option)])
    )
  })

  # display plot output
  output$plot <- renderPlot(output_q()[["p"]])
  # display code
  output$rcode <- renderText(get_code(output_q()))
}

if (interactive()) {
  shinyApp(ui, server)
}

## ----get_code-----------------------------------------------------------------
q_reproducible <- qenv()
q_reproducible <- within(q_reproducible, {
  a <- 2
  b <- 5
  c <- a + b
})
cat(get_code(q_reproducible))
cat(get_code(q_reproducible, names = "a"))
cat(get_code(q_reproducible, names = "c"))

## ----get_code_linked----------------------------------------------------------
q_linked <- qenv()
q_linked <- eval_code(q_reproducible, "
  set.seed(2) # @linksto a
  a <- runif(1)
")
cat(get_code(q_linked))
cat(get_code(q_linked, names = "a"))

