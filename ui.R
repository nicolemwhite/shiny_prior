library(shiny)
library(shinyjs)
library(shinyBS)


renderInputs <- function(suffix){
  wellPanel(id=paste0('prior_setup_',suffix),h5(strong(paste("Define inputs for Prior",suffix))),{
            fluidRow(
              #Distribution family
              column(10, 
                     selectInput(inputId = paste0("dist_",suffix),
                                 label = 'Distribution',
                                 choices = c("Beta" = paste0("beta_",suffix),
                                             "Gamma" = paste0("gamma_",suffix),
                                             "Normal" = paste0("norm_",suffix),
                                             "Uniform" = paste0("unif_",suffix),
                                             "Triangular" = paste0("tri_",suffix),
                                             "Poisson" = paste0("pois_",suffix),
                                             'Log-normal' = paste0("lnorm_",suffix)),
                                 selected = paste0("norm_",suffix)),
                     uiOutput(paste0("ui_dist_",suffix))
                     #actionButton(inputId = paste0("go_",suffix),label=paste("Run simulation for Prior",suffix))
                     
              ),
              # #form of evidence based on distribution selected
               column(6,
                      uiOutput(paste0("ui_evidence_",suffix))
               )
            )
  },
  helpText(paste0('Select the distribution and form of evidence that best describe prior ',
                  suffix,' . Enter values for all inputs based on options selected.'))
  )
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  # Application title
  titlePanel(h3("ShinyPrior: A simulation tool for visualising prior distributions based on published evidence")),
  
  # Panels
  fluidRow(
    column(4,wellPanel(
      
      # Samples
      fluidRow(
        #comparison of distributions
        column(6,
               radioButtons(inputId= 'compare',
                            label = 'Number of prior distributions',
                            choices=c('Single distribution'=1,
                                      'Compare two distributions' = 2),
                            selected=1)
               
        ),        
        column(6,
               numericInput(inputId = 'samples',
                            label = 'Number of simulations',
                            value = 10000, min = 1000, max = 1000000,step=1000)
        )
        
      ),
      helpText('Options are to estimate and visualise one prior (`Single distribution`) 
               or compare two priors based on different distributions and/or forms of evidence 
               (`Compare two distributions`). ShinyPrior will simulate the same number of values 
               from each prior distribution.')
      
    ),
    #Prior 1 options
    renderInputs(1),
    #Prior 2 options
    #shinyjs::hidden(renderInputs(2))
    conditionalPanel(condition = "input.compare==2",renderInputs(2)),
    wellPanel(h5(strong('Run simulation')),
      fluidRow(
      column(6,actionButton(inputId = "go",label="Simulate from prior distribution(s)"))
      ),

      fluidRow(
      column(6,downloadButton("downloadData", "Download simulation to .csv"))
      ),
      fluidRow(
        column(6, actionButton("resetAll", "Start over"))
      ))
  )
  ,



# Show a plot of the generated distribution, display summary statistics
column(6,
       plotOutput("hist"),
       renderUI("param_tabs"),
       tabsetPanel(
         tabPanel('Overview',
                  p("This application is designed to estimate and visualise prior distributions for use in simulation-based modelling. 
                    Options focus on using common forms of evidence reported in published articles to estimate unknown distribution parameters."),
                  h5(strong("References and Contacts")),
                  p("A short tutorial on using ShinyPrior will be available soon.
                    Questions about ShinyPrior including bug reports and suggested improvements can be sent to Nicole White (nm.white@qut.edu.au) or Robin Blythe (robin.blythe@qut.edu.au)")
                  ),
         tabPanel("Parameter estimates",
                  tableOutput("param_est")
         ),
         tabPanel("Five-number summary",
                  tableOutput("stats")
         )

       )
))
)
)

