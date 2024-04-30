#header
header <- dashboardHeader(title = "ShinyPrior: A tool for estimating probability distributions using published evidence",titleWidth = 990,disable=F)

#sidebar
sidebar <- dashboardSidebar(
  width=600,
  sidebarMenu(
    id = 'sidebar',
    menuItem("Home", tabName = "home", icon=icon("home"),selected = T),
    menuItem("Define distribution inputs", tabName = "setup", icon = icon("list"),startExpanded = T),
    menuItem("Customisation", tabName = "customise", icon = icon("wrench"),startExpanded = F,
             menuSubItem(text="Visualisation",tabName = 'visualisation',icon=icon('bar-chart')),
             menuSubItem(text="Summary table",tabName = 'summary_table',icon=icon('table')),
             menuSubItem(text="Remove results from saved output",tabName = 'remove_selected',icon=icon('trash'))),
    menuItem("Github", icon = icon("github"), href = "https://github.com/nicolemwhite/ShinyPrior"),

    
    div(id = 'sidebar_setup',
        conditionalPanel("input.sidebar == 'setup'",
                         fluidRow(column(6,
                                         selectInput(inputId = "dist_family",
                                                     label = 'Distribution family',
                                                     choices = c("Beta" = paste0("beta"),
                                                                 "Gamma" = paste0("gamma"),
                                                                 "Normal" = paste0("norm"),
                                                                 "Uniform" = paste0("unif"),
                                                                 'Log-normal' = paste0("lnorm"),
                                                                 "Weibull" = paste0("weib")),
                                                     selected = paste0("norm"))),
                                  column(6,textInput('dist_label',label='Description',placeholder = 'hello_world'))),
                         
                         #step 2
                         fluidRow(column(6,uiOutput(paste0("ui_dist"))),
                                  column(6,uiOutput(paste0("ui_evidence"))),
                                  column(6,numericInput(inputId = "table_dps",label = "Decimal places in output",min = 0,max=5,step = 1,value = 1))),
                         
                         #step 3
                         fluidRow(column(4,actionButton(inputId = "go",label="Estimate distribution",icon=icon('calculator'),style='background-color:	#f9f9f9;font-family: Arial;font-weight: bold'))),
                         column(12,style='padding-top:15px;'),
                         column(10,textOutput("input_error"),style='margin-left:15px')#,
                         
        ),
        conditionalPanel("input.sidebar == 'visualisation'||input.sidebar == 'summary_table'",
        column(10,selectInput('select_output_plot',label='Select distribution(s)',choices=NULL,multiple=TRUE,selectize=T))
        ),
        
        
        conditionalPanel("input.sidebar == 'visualisation'",
                         fluidRow(style='margin-left:0px',
                                  column(6,selectInput('colourscheme',label='Choose colour scheme',choices = names(colourschemes),selected = 'Greyscale')),
                                  column(6, selectInput("theme", label = "Select plot theme", choices = names(themes),selected = 'Minimal'))),
                         fluidRow(style='margin-left:0px',column(6,textInput('xlabtext',label='x-axis label',value = 'Value')),
                                  column(6,textInput('ylabtext',label='y-axis label',value = 'Density'))),
                         fluidRow(style='margin-left:0px',column(6,radioButtons(inputId='legend','Display legend',choices=names(legend_positions),selected = "Yes",inline=TRUE),style = 'margin-top:10px'),column(6,uiOutput("legend_title"))),
                         fluidRow(style='margin-left:0px',
                                  column(3,selectInput("fformat", "Format",c("png" = "png","tiff" = "tiff","jpeg" = "jpeg"), 'png')),
                                  column(3,selectInput(inputId = "fres",label = "Resolution",c("300 dpi"=300,"600 dpi"=600),selected = "300")),
                                  column(3,numericInput(inputId = "fheight",label = "Height (cm)",min = 8,max = 22,step = 1,value = 10)),
                                  column(3,numericInput(inputId = "fwidth",label = "Width (cm)",min = 8,max = 22,step = 1,value = 15))),
                         fluidRow(style='margin-left:0px',column(10,textInput(inputId = 'fig_fname',label='Figure name',value='shinyprior_figure'))),
                         fluidRow(column(6,downloadButton("downloadFigure", "Download Figure",style = 'margin-left:30px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')))
        ),
        conditionalPanel("input.sidebar == 'summary_table'",
                         column(10,checkboxGroupInput(inputId = 'summary_stats',label="Select summary statistic(s)",choices=c('Form of evidence','Distribution','Mean (SD)','Mean (95% Interval)','Median (Q1 to Q3)'),
                                                      selected = c('Form of evidence','Distribution','Mean (SD)','Mean (95% Interval)','Median (Q1 to Q3)'),inline=F)),
                         
                         column(12,checkboxGroupInput(inputId = 'table_order',label='Arrange rows by:',choices = c('Description','Distribution family'),selected=NULL,inline=TRUE)),
                         fluidRow(column(10,textInput(inputId = 'tab_fname',label='Table name',value='shinyprior_table'),style='margin-left:15px')),
                         fluidRow(column(6,downloadButton("downloadTable", "Download table",style='margin-left:30px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')))
        ),
        
        conditionalPanel("input.sidebar == 'remove_selected'",
                         fluidRow(column(6,selectInput(inputId = "select_output",label='Select distribution(s)',choices=NULL,multiple=TRUE,selectize=T),style='margin-left:15px')),
                         fluidRow(column(6,actionButton(inputId = "remove_result",label="Remove selected distribution(s)",icon=icon("trash"),style='margin-left:30px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')))
        ))
  ))


body <- dashboardBody(
  tabItems(
    tabItem(tabName='home',
            #strong("About",style='font-family: "Arial";font-size:20px'),
            br(),
            p('ShinyPrior is a web-based application for estimating probability distributions from summary statistics. Results are presented as visualisations and summary tables for use in publications.',style='font-family: "Arial";font-size:16px'),
            p('To use the application, start by selecting a distribution family and form of evidence from the "Define distribution inputs" menu. Users will then be prompted to enter required data based on the options selected. Application outputs in the main window can be customised via the "Customisation" menu.',style='font-family: "Arial";font-size:16px'),
            p('Full details can be found in the ',a(href="https://osf.io/preprints/zf62e/","ShinyPrior vignette."),'Questions about ShinyPrior and suggestions for future updates can be sent to',a(href="https://www.aushsi.org.au/about-us/team/nicole-white/", "Nicole White"),style='font-family: "Arial";font-size:16px'),
            br(),
            strong("Citation",style='font-family: "Arial";font-size:18px'),
            br(),
            p('If you use ShinyPrior in your work, please cite:',style='font-family: "Arial";font-size:16px'),
            p('White, NM., Blythe, R. ShinyPior: A tool for estimating probability distributions from published evidence. OSF Preprints. 16 February 2023. DOI:10.31219/osf.io/zf62e',style='font-family: "Arial";font-size:16px'),
            br(),
            strong("Contributors",style='font-family: "Arial";font-size:18px'),
            br(),
            p('Nicole White (Conceptualization, Methodology, Software, Writing - Original Draft)',style='font-family: "Arial";font-size:16px'),
            p('Robin Blythe (Conceptualization, Validation, Writing - Review & Editing)',style='font-family: "Arial";font-size:16px'),
            br(),
            strong("Acknowledgement",style='font-family: "Arial";font-size:18px'),
            br(),
            p('We thank', a(href="https://www.aushsi.org.au/about-us/team/hannah-carter/","Hannah Carter"),' and ',a(href="https://www.aushsi.org.au/about-us/team/adrian-barnett/","Adrian Barnett"),'for their feedback on early versions of the application, and', a(href="https://www.qut.edu.au/about/our-people/academic-profiles/dn.borg","David Borg"),'for feedback on the accompanying vignette',style='font-family: "Arial";font-size:16px')
    ),
    
    tabItem(tabName = 'setup',
            h4("Visualisation",style='font-weight: bold;font-family: "Arial";color: #000000'),
            fluidRow(box(title=NULL,status='primary',width=12,column(12,align="center",plotOutput("hist",width = "auto",height = "400px")))),
            h4("Summary table",style='font-weight: bold;font-family: "Arial";color: #000000'),
            fluidRow(box(title=NULL,status='primary',width=12,column(12, align="center",tableOutput("param_est"))))
    )



  ),
  
  #style elements
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #003D58;
                                color: #EAE23B;
                                font-family: "Arial";
                                font-weight: bold;
                                font-size: 24px;
                                margin-left: 0px;


                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #003D58;


                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #009FE3;
                                font-family: "Arial";
                                font-size: 16px;
                                font-weight:bold;

                                
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #009FE3;
                                color: #EAE23B;
                                
                                }
            
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color:#009FE3;
                                color: #003D58;
                                }


                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;

                                }

                                /*other text*/
                                .shiny-output-error-validation {color: #EAE23B;font-weight: bold;}
                                

                                
                                '))),
  tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
  
)

ui <- dashboardPage(header, sidebar, body)