#' Fit Univariate Distributions Addin
#'
#' Interactively submit a numeric vector and choose what distributions that
#' you want to run fit diagnostics. Click done to have the desired distribution
#' code put into your cursor position.
#'
#'
#'
#' @export
fit_dist_addin <- function() {
  numericObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),
                                                  function(xChar) {
    is.numeric(get(xChar, envir = .GlobalEnv))}
  )]
  discreteDists <- c('geom', 'nbinom', 'pois', 'dunif')
  continuousDists <- c(Exponential = 'exp', Cauchy = 'cauchy',
                       Gamma = 'gamma', LogNormal = 'lnorm',
                       Normal = 'norm', Uniform = 'unif',
                       Weibull = 'weibull', LogLogistic = 'llogis',
                       Logistic = 'logis', InverseWeibull = 'invweibull',
                       InverseGamma = 'invgamma')
  ui <- miniPage(
    miniContentPanel(
      fillCol(flex = c(NA, NA, 1),
              gadgetTitleBar("Fit Distributions"),
      fillRow(flex = c(NA, NA, NA, NA),
        selectizeInput('distributions', 'Distributions',
                    choices = continuousDists,
                    selected = 'norm',
                    multiple = TRUE),
        selectInput('data', label = 'Data',
                    choices = numericObjects),
        numericInput('nbins', 'No. of Bins', value = 30,
                     min = 5, max = 100, step = 1)
      ),
      fillRow(
        fillCol(flex = c(1, 1),
          DTOutput('gofTable', height = '100%'),
          plotOutput('ppPlot', height = '100%')
        ),
        fillCol(flex = c(1, 1),
          plotOutput('densityPlot', height = '100%'),
          plotOutput('qqPlot', height = '100%')
          )
      )
    )
  )
  )

  server <- function(input, output, session) {
    require(fitur)
    require(miniUI)
    x <- reactive({
      get(input$data, envir = .GlobalEnv)
    })
    fits <- reactive({
      x <- x()
      dists <- input$distributions
      fits <- lapply(dists, fit_univariate, x = x)
    })
    output$gofTable <- renderDT({
      gof <- gof_tests(fits(), x())
      datatable(setNames(gof, gsub('_', ' ', names(gof))),
                options = list(searching = FALSE,
                               lengthMenu = -1,
                               lengthChange = FALSE,
                               paging = FALSE)
                ) %>% formatRound(columns = 2:7)

    })
    output$densityPlot <- renderPlot({
      plot_density(x(), fits(), input$nbins) +
        theme_bw()
    })

    output$ppPlot <- renderPlot({
      plot_pp(x(), fits()) +
        theme_bw()
    })

    output$qqPlot <- renderPlot({
      plot_qq(x(), fits()) +
        theme_bw()
    })
    observeEvent(input$done, {
      stopApp("Done")
    })
  }

  runGadget(ui, server,
            #viewer = dialogViewer('Fit Univariate Distributions', 800, 800)
            viewer = paneViewer()
            )
}
