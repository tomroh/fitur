fit_distributions <- function() {
  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      fillRow(
        fillCol(flex = c(NA, NA, 1),
          selectInput('distributions', 'Distributions',
                      choices = c('Weibull', 'Lognormal')),
          numericInput('data', label = 'Data', value = 1),
          plotOutput('ppPlot', height = '100%')
        ),
        fillCol(flex = c(NA, 1, 1),
          numericInput('nbins', 'No. of Bins', value = 30,
                       min = 5, max = 100, step = 1),
          plotOutput('densityPlot', height = '100%'),
          plotOutput('qqPlot', height = '100%')
          )
      )
    )
  )

  server <- function(input, output, session) {
    require(fitur)
    x <- rweibull(100, 1)
    fits <- reactive({
      dists <- c('gamma', 'lnorm', 'weibull')
      fits <- lapply(dists, fit_univariate, x = x)
    })
    # Render the plot
    output$densityPlot <- renderPlot({
      plot_density(x, fits(), input$nbins) +
        theme_bw()
    })

    output$ppPlot <- renderPlot({
      plot_pp(x, fits()) +
        theme_bw()
    })

    output$qqPlot <- renderPlot({
      plot_qq(x, fits()) +
        theme_bw()
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      stopApp("Done")
    })
  }

  runGadget(ui, server,
            #viewer = dialogViewer('Fit Univariate Distributions', 800, 800)
            viewer = paneViewer()
            )
}
