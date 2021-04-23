library("shiny")

shinyUI(fluidPage(

  # titlePanel('Bayesian inference for normal mean (known variance)'),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("n_trials", label = "Trials:", value = 10, min = 0, step = 1L),
      numericInput("y_successes", label = "Successes:", value = 5, min = 0, step = 1L),
      hr(),
      numericInput("prior_a", label = "Prior a:", value = 1, min = 1,step = 0.1),
      numericInput("prior_b", label = "Prior b:", value = 1, min = 1, step = 0.1)
    ),
    mainPanel(
      width = 9,
      plotOutput("dist_plot")
    )
  )

))
