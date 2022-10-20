shinyUI(fluidPage(
  
  
  
  useShinyjs(),
  # Application title,
  titlePanel(("ShinyPrior: A tool for estimating probability distributions using published evidence")),
  #inlineCSS(".control-label {font-weight: 500}"),
  
  # Panels
  sidebarLayout(
    sidebarPanel(
      id = "side-panel",
      
      #Distribution name
      wellPanel(h5(strong('Step 1. Choose a distribution')),
                h5("Select an appropriate distribution for your outcome of interest below.
                         Enter a short name for your distribution in the 'Description' box. This name will be used in all application outputs"),
                #Distribution family
                fluidRow(column(6, 
                                selectInput(inputId = "dist_family",
                                            label = 'Distribution family',
                                            choices = c("Beta" = paste0("beta"),
                                                        "Gamma" = paste0("gamma"),
                                                        "Normal" = paste0("norm"),
                                                        "Uniform" = paste0("unif"),
                                                        'Log-normal' = paste0("lnorm"),
                                                        "Weibull" = paste0("weib")),
                                            selected = paste0("norm")))),
                
                fluidRow(column(6,textInput('dist_label',label='Description',placeholder = 'hello_world')))
      ), #end of wellPanel
      
      
      #Define distribution inputs
      renderInputs(),
      
      
      #Run application
      wellPanel(h5(strong('Step 3: Run Application')),
                h5("Click on 'Estimate distribution' to generate a tabular summary and density plot. Descriptions aleady stored in the Table will be overwritten."),
                fluidRow(
                  column(4,actionButton(inputId = "go",label="Estimate distribution"),

                )),
                column(12,style='padding-top:15px;'),
                textOutput("input_error"),
                column(12,style='padding-top:15px;'),
                textOutput("no_soln")
      ),
      
      wellPanel(h5(strong('Step 4: Customise output')),
                column(12,style='padding-top:5px;'),
                h5(strong('Summary table')),
                h5("Select which summary statistic(s) to include in the Table. When done, click 'Download Table' to download the customised table as a Word document."),
                column(10,checkboxGroupInput(inputId = 'summary_stats',label=NULL,
                                             choices=c('Form of evidence','Distribution','Mean (SD)','Mean (95% CI)','Median (Q1 to Q3)'),
                                             selected = c('Form of evidence','Distribution','Mean (SD)','Mean (95% CI)','Median (Q1 to Q3)'),inline=F)),
                
                column(12,checkboxGroupInput(inputId = 'table_order',label='Arrange rows by:',choices = c('Description','Distribution family'),selected=NULL,inline=TRUE)),
                fluidRow(column(6,textInput(inputId = 'tab_fname',label='Table name',value='table')),
                  column(6,downloadButton("downloadTable", "Download table"),style = 'margin-top:25px')),
                fluidRow(column(12,style='padding-bottom:20px;')),
                
                h5(strong('Visualisation')),
                h5("Customise the appearance of the density plot here. Multiple distributions can be added and removed in the 'Select distribution(s)' box below. When done, click 'Download Figure' to download the customised visualation in the selected format."),
                fluidRow(column(10,selectInput('select_output_plot',label='Select distribution(s) to plot',choices=NULL,multiple=TRUE,selectize=T))),

                fluidRow(
                  column(6,selectInput('colourscheme',label='Choose colour scheme',choices = names(colourschemes),selected = 'Greyscale')),
                  column(6, selectInput("theme", label = "Select plot theme", choices = names(themes),selected = 'Minimal'))
                ),
                fluidRow(column(6,textInput('xlabtext',label='x-axis label',
                                            value = 'Value')),
                         column(6,textInput('ylabtext',label='y-axis label',
                                            value = 'Density'))),
                fluidRow(column(6,radioButtons(inputId='legend','Display legend?',choices=names(legend_positions),selected = "Yes",inline=TRUE),style = 'margin-top:10px'),
                         column(6,uiOutput("legend_title"))),
                fluidRow(
                  column(3,selectInput("fformat", "Format",c("png" = "png","tiff" = "tiff","jpeg" = "jpeg"), 'png')),
                  column(3,selectInput(inputId = "fres",label = "Resolution (dpi)",c("300"=300,"600"=600),selected = "300")),
                  column(3,numericInput(inputId = "fheight",label = "Height (cm)",min = 8,max = 22,step = 1,value = 10)),
                  column(3,numericInput(inputId = "fwidth",label = "Width (cm)",min = 8,max = 22,step = 1,value = 15))),
                fluidRow(column(6,textInput(inputId = 'fig_fname',label='Figure name',value='figure')),
                         column(6,downloadButton("downloadFigure", "Download Figure"),style = 'margin-top:25px')),
                fluidRow(column(12,style='padding-bottom:20px;')),
                column(12,style='padding-top:5px;'),
                h5(strong('Remove results from saved output')),
                h5("Any distributions estimated in Step 3 can be permanently deleted here. Select all relevant results in the box below and click 'Remove'"),
                fluidRow(column(6,selectInput(inputId = "select_output",label='Select distribution(s)',choices=NULL,multiple=TRUE,selectize=T)),
                         column(4,actionButton(inputId = "remove_result",label="Remove",style = 'margin-top:25px')))
      )
    ), #end of sidebarPanel
    
    mainPanel(
      style="position:fixed;margin-left:32vw;",
      
      
      # Show a plot of the generated distribution and display summary tables for current and stored distribution
      fluidRow(column(10,align="center",plotOutput("hist"),style = 'margin-left:75px')),
      tabsetPanel(
        id = 'output-tabs',
        tabPanel("Table",column(10,style='padding-bottom:20px;'),column(12, align="center",tableOutput("param_est"))),
        tabPanel('About ShinyPrior',
                 p("Summary here with link to preprint/vignette"),
                 h5(strong("Contacts")),
                 p("Questions about ShinyPrior and suggestions for improvements can be sent to Nicole White (nm.white@qut.edu.au)"),

                 actionButton(inputId='ab1', label="Learn More", 
                              icon = icon("th"), 
                              onclick ="window.open('https://www.aushsi.org.au/', '_blank')")
        )
      )
    )
  )
)
)

