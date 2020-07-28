
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
             
      ),
      # #form of evidence based on distribution selected
      column(6,
             uiOutput(paste0("ui_evidence_",suffix))
      ),
      #custom label for histogram
      column(10,textInput(inputId = paste0("prior_label_",suffix),label = 'Label',value=paste0('Prior_',suffix)))
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
  titlePanel(("ShinyPrior: A tool for building prior distributions based on published evidence")),
  
  # Panels
  sidebarLayout(
    sidebarPanel(
      wellPanel(
          # Samples
          fluidRow(
            #comparison of distributions
            column(6,
                   radioButtons(inputId= 'compare',
                                label = 'Number of prior distributions',
                                choices=c('Single distribution'=1,
                                          'Compare two distributions' = 2),
                                selected=1)),        
            column(6,numericInput(inputId = 'samples',
                                label = 'Number of simulations',
                                value = 10000, min = 1000, max = 1000000,step=1000))
            
          ),
          helpText('Options are to estimate and visualise one prior (`Single distribution`) 
               or compare two priors based on different distributions and/or forms of evidence 
               (`Compare two distributions`). ShinyPrior will simulate the same number of values 
               from each prior distribution.')
        ),
        #Prior 1 options
        renderInputs(1),
        #Prior 2 options
        conditionalPanel(condition = "input.compare==2",renderInputs(2)),
        wellPanel(h5(strong('Run ShinyPrior')),
                  fluidRow(column(6,actionButton(inputId = "go",label="Simulate from prior distribution(s)"))),
                  fluidRow(column(6, actionButton("resetAll", "Start over")))
                  ),
        
        wellPanel(h5(strong('Customise output')),
                  h5(strong('Show summary statistics')),
                  fluidRow(
                    column(6,checkboxInput('perc_theory','Theoretical',value=T)),
                    column(6,checkboxInput('perc_simulated','Simulated',value=T))
                  ),
                  h5(strong('Visualisation')),
                  fluidRow(
                    column(6,selectInput('colourscheme',h6(strong('Choose colour scheme')),
                                         choices = names(colourschemes),
                                         selected = 'Colour')),
                    column(6, selectInput("theme", label = h6(strong("Select plot theme")), 
                                          choices = names(themes),
                                          selected = 'Minimal')),
                    column(6,textInput('xlabtext',label='x-axis label',
                                       value = 'Sampled value')),
                    column(6,textInput('ylabtext',label='y-axis label',
                                       value = 'Density')),                
                    column(12,radioButtons('legend',h6(strong('Display legend?')),
                                           choices = names(legend_positions),
                                           selected = "Yes",inline = T))
                  ),
                  
                  fluidRow(
                    column(8,sliderInput('nbins',h6(strong('Number of histogram bins')),
                                         min = 1,max=50,value=25)),    
                    column(8,sliderInput('opacity',h6(strong('Opacity')),
                                         min=0,max=1,value=0.25))
                  )
                  
                  
        ),
        wellPanel(h5(strong('Save output')),
                  h5(strong('Simulated values')),
                  fluidRow(
                    column(12,downloadButton("downloadData", "Download as .csv"))
                  ),
                  h5(strong('Visualisation')),
                  fluidRow(
                    column(3,selectInput("fformat", "Format",
                                         c("png" = "png",
                                           "tiff" = "tiff",
                                           "jpeg" = "jpeg"), 'png')),
                    
                    column(3,selectInput(inputId = "fres",
                                          label = "Resolution",
                                          c("300dpi"=300,
                                            "600dpi"=600),
                                          selected = "300dpi")),
                    column(3,numericInput(inputId = "fheight",
                                          label = "Height (cm)",
                                          min = 8,
                                          max = 22,
                                          step = 1,
                                          value = 10)),
                    
                    column(3,numericInput(inputId = "fwidth",
                                          label = "Width (cm)",
                                          min = 8,
                                          max = 22,
                                          step = 1,
                                          value = 15))
                  ),
                  fluidRow(column(6,downloadButton("downloadFigure", "Download Figure")))
                  
        )

      ), #end of sidebarPanel
      
      mainPanel(
        style="position:fixed;margin-left:32vw;",
        
        # Show a plot of the generated distribution, display summary statistics
        column(10,
               plotOutput("hist"),
               renderUI("param_tabs"),
               tabsetPanel(
                 tabPanel('Overview',
                          p("This application is designed to estimate and visualise prior distributions for use in simulation-based modelling. 
                    Options focus on using common forms of evidence reported in published articles to estimate unknown distribution parameters."),
                          h5(strong("Contacts")),
                          p("Questions about ShinyPrior and suggestions for improvements can be sent to Nicole White (nm.white@qut.edu.au) or Robin Blythe (robin.blythe@qut.edu.au)")
                 ),
                 tabPanel("Parameter estimates",
                          tableOutput("param_est")
                 ),
                 tabPanel("Summary statistics",
                          conditionalPanel(condition = "input.perc_theory",
                                           h5(strong('Expected under chosen distribution (theoretical)')),
                                           tableOutput("stats_theory")),
                          conditionalPanel(condition = "input.perc_simulated",
                                           h5(strong('Estimated from simulation output')),
                                           textOutput("n_draws"),
                                           tableOutput("stats_simulated"))
             )
                 
               )
        )
      )
    )
  )
)


