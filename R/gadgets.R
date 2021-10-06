#' Fit Univariate Distributions Addin
#'
#' Interactively submit a numeric vector and choose what distributions that
#' you want to run fit diagnostics. Click done to have the desired distribution
#' code put into your cursor position.
#'
#' @import miniUI
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
  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      shiny::fillCol(flex = c(NA, NA, 1),
              miniUI::gadgetTitleBar("Fit Distributions", right = NULL),
              shiny::fillRow(flex = c(NA, NA, NA, NA),
        shiny::selectizeInput('distributions', 'Distributions',
                    choices = continuousDists,
                    selected = 'norm',
                    multiple = TRUE),
        shiny::selectInput('data', label = 'Data',
                    choices = numericObjects),
        shiny::numericInput('nbins', 'No. of Bins', value = 30,
                     min = 5, max = 100, step = 1)
      ),
      shiny::fillRow(
        shiny::fillCol(flex = c(1, 1),
          DT::DTOutput('gofTable', height = '100%'),
          shiny::plotOutput('ppPlot', height = '100%')
        ),
        shiny::fillCol(flex = c(1, 1),
          shiny::plotOutput('densityPlot', height = '100%'),
          shiny::plotOutput('qqPlot', height = '100%')
          )
      )
    )
  )
  )

  server <- function(input, output, session) {
    x <- shiny::reactive({
      get(input$data, envir = .GlobalEnv)
    })
    fits <- shiny::reactive({
      x <- x()
      dists <- input$distributions
      fits <- lapply(dists, fitur::fit_univariate, x = x)
    })
    output$gofTable <- DT::renderDT({
      gof <- fitur::gof_tests(fits(), x())
      DT::formatRound(
        DT::datatable(stats::setNames(gof, gsub('_', ' ', names(gof))),
                options = list(searching = FALSE,
                               lengthMenu = -1,
                               lengthChange = FALSE,
                               paging = FALSE)
                ),
        columns = 2:7)

    })
    output$densityPlot <- shiny::renderPlot({
      fitur::plot_density(x(), fits(), input$nbins) +
        ggplot2::labs(title = 'Density Plot') +
        ggplot2::theme_bw()
    })

    output$ppPlot <- shiny::renderPlot({
      fitur::plot_pp(x(), fits()) +
        ggplot2::labs(title = 'PP-Plot') +
        ggplot2::theme_bw()
    })

    output$qqPlot <- shiny::renderPlot({
      fitur::plot_qq(x(), fits()) +
        ggplot2::labs(title = 'QQ-Plot') +
        ggplot2::theme_bw()
    })
    # shiny::observeEvent(input$done, {
    #   shiny::stopApp("Done")
    # })
  }

  shiny::runGadget(ui, server,
            viewer = shiny::paneViewer()
            )
}
