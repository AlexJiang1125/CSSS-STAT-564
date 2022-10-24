library("shiny")
library("ggsci")

shinyServer(function(input, output) {

  output$dist_plot = renderPlot({

    n_trials      = input$"n_trials"
    y_successes   = input$"y_successes"
    prior_a       = input$"prior_a"
    prior_b       = input$"prior_b"

    if (y_successes > n_trials) return(NULL)

    #n = length(y_data)
    #data_mu = mean(y_data)
    post_a = prior_a + y_successes
    post_b = prior_b + n_trials - y_successes

    set.seed(42)
    y = seq(0,1,length.out = 500)  # to center plot on posterior
    y_prior = dbeta(y, prior_a, prior_b)
    y_lik   = dbeta(y, y_successes + 1,  n_trials - y_successes + 1)
    y_post  = dbeta(y, post_a,  post_b)
    y_max = max(c(y_prior, y_lik, y_post))

    pal = rev(pal_lancet("lanonc")(3))

    plot(y, y_prior, type = "l", col = pal[1],
         lty = 2, xlim = c(min(y), max(y)), ylim = c(0, y_max),
         ylab = "density", lwd = 2)
    lines(y, y_lik,  type = "l", col = pal[2], lwd = 2)
    lines(y, y_post, type = "l", col = pal[3], lwd = 2)
    #abline(v = y_data, col = pal[2], lty = 3, lwd = 2)

    legend("topright", col = c(pal, pal[2], pal[2]),
           lty = c(2, 1, 1), cex = 1.5, lwd = 2, bty = "n",
           legend = c("Prior", "Likelihood", "Posterior"))

  })

})
