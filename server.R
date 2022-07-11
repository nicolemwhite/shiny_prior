shinyServer(function(input, output,session) {
  
  #renderUI 
  dist_info <- reactive({define_dist_options(input[['dist_family']],input[['dist_label']])})  
  evidence_info <- reactive({define_evidence_options(input[['parms_in']])}) 
  
  output$ui_dist <- renderUI({renderDistIn(dist_info())})
  output$ui_evidence <-renderUI({renderParmsIn(evidence_info())})
  
  estimate_info = reactive({
    sample_labels = evidence_info()$sample_inputs
    sapply(seq(sample_labels),function(z) input[[paste0(sample_labels[z])]])
  })
  
  
  #estimate parameters based on data provided and chosen distribution
  parameter_estimates = function(){#reactive({
    #estimate distribution based on evidence inputted
    estimate_dist_parameters(dist_name=dist_info()$dist_name,evidence_type= evidence_info()$evidence_type,sample_dat=estimate_info())
    #}) #end of reactive function
  }
  #theoretical moments based on parameters estimates
  dist_summary = function(){
    validate(check_all_inputs(dist_info(),evidence_info()$evidence_type,estimate_info()))
    
    
    #table
    dat = data.frame("Description" = input[['dist_label']],
                     "Form of evidence" = nice_names_evidence(input[['parms_in']]),
                     "Distribution" = nice_names(input[['dist_family']],parameter_estimates()[1:2]),
                     "Mean (95% uncertainty interval)" = paste0(round(parameter_estimates()['Mean'],2),' (',round(parameter_estimates()['p2.5'],2),' to ',round(parameter_estimates()['p97.5'],2),')'),
                     "Median (Q1 to Q3)" = paste0(round(parameter_estimates()['p50'],2),' (',round(parameter_estimates()['p25'],2),' to ',round(parameter_estimates()['p75'],2),')'),check.names=F)
    
    #figure
    plot_colours = colourschemes[[input$colourscheme]]
    plot_theme = themes[[input$theme]]
    #legend_posn = legend_positions[[input$legend]]
    
    return(list(output_name = input[['dist_label']],output_family = input[['dist_family']],param_est = parameter_estimates()[1:2],table_output = dat)) 
  }
  
  #any_missing = eventReactive(input$go,({validate(check_missing_inputs(estimate_info()))}))
  bad_dist_params = eventReactive(input$go,({validate(check_all_inputs(dist_info(),evidence_info()$evidence_type,estimate_info()))}))
  
  
  
  #Outputs
  generate_outputs = eventReactive(input$go,({dist_summary()}))
  
  
  #Saved output
  results <- reactiveValues(output_name = NULL,output_family = NULL,param_est = NULL,table_output = NULL)

  
  plot_colours <- reactive({
  custom_palette <- col_factor(palette = colourschemes[[input$colourscheme]], domain = seq_along(input$select_output_plot),reverse=T,alpha=T)
  out <- custom_palette(1:length(input$select_output_plot))
  names(out) <- input$select_output_plot
  out
  })
  
  create_density_plot <- function(){
    #if(input$colourscheme == "Greyscale") {color_selected = "Greys"}
    #if(input$colourscheme == "Viridis") {color_selected = "Viridis"}
    
    plot_theme = themes[[input$theme]]
    show_legend = legend_positions[[input$legend]]
    legend_title = input$custom_legend_title
    include_vars <-  intersect(input$select_output_plot,results$output_name)
    n_var <- length(include_vars)
    
    if(length(include_vars)>0){
      #plot_colours <- custom_palette()
      #names(plot_colours) <- include_vars
      x_limits <- lapply(include_vars, function(i){calc_xlim(results[['output_family']][[i]],results[['param_est']][[i]])})
      x_min <- min(unlist(x_limits))
      x_max <- max(unlist(x_limits))
      
     ggplot(data = data.frame(x=c(x_min,x_max)),aes(x)) + 
        lapply(include_vars, function(i){
          calc_dens(results[['output_family']][[i]],results[['param_est']][[i]],results[['output_name']][[i]])
        })+
        scale_colour_manual(values=plot_colours(),name=legend_title)+
        xlab(input$xlabtext)+ylab(input$ylabtext)+
        plot_theme+theme(text=element_text(size=14),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
                         legend.position = show_legend) #legend.title = element_text(legend_title)
    }
  }
  
  create_table <- function(){
    ftab <- bind_rows(results[['table_output']])
    if(nrow(ftab)>0){
      if(!is.null(input$summary_stats)) ftab <- ftab[,c("Description",input$summary_stats)]
      else ftab <- data.frame(Description = ftab[,1]) 
    }
    ftab
  }
  
  observeEvent(input$go,if(input$dist_label!=''){
    add_results <- generate_outputs()
    for(x in names(results)){results[[x]][[input$dist_label]] <<- add_results[[x]]}
  })
  
  observeEvent(input$remove_result,{
    current_names <- unlist(results$output_name)
    to_remove <- input$select_output
    keep_results <- results
    for(x in names(results)){results[[x]] <<- keep_results[[x]][!(current_names %in% to_remove)]}

  })
  
  observe({updateSelectInput(session,inputId = "select_output",choices=results[['output_name']])})
  observe({updateSelectInput(session,inputId = "select_output_plot",choices=results[['output_name']],selected=isolate(input$dist_label))})

  output$legend_title <- renderUI({
    if (input$legend == 'No') return(NULL) else {
      textInput(inputId ='custom_legend_title',label='Legend title',value='Description')
    }
  })

  output$input_error <- renderText({bad_dist_params()})
  output$hist = renderPlot({create_density_plot()})
  output$param_est = renderTable({create_table()})
  
  #download outputs
  #table - to do
  fn_download_tab <- function(){
    ftab <- create_table() %>% flextable() %>% fontsize(size-10,part=c("header","body")) %>% FitFlextableToPage()
    save_as_docx(ftab,path=fn_downloadname_tab())
  }
  # create filename
  fn_downloadname_tab <- reactive({
    fname = isolate(input$tab_fname)
    filename <- paste0(fname,".docx",sep="")
    return(filename)
  })
  
  # download handler
  output$downloadTable<- downloadHandler(
    filename = fn_downloadname_tab,
    content = function(file) {
      fn_download_tab()
      file.copy(fn_downloadname_tab(), file, overwrite=T)
    }
  )  
  ##
  
  
  #figure
  fn_download_fig <- function()
  {
    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    
    # open file dependent on format    
    if(input$fformat=="png") png(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="cm")
    if(input$fformat=="tiff") tiff(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="cm", compression="lzw")
    if(input$fformat=="jpeg") jpeg(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="cm", quality=100)
    
    g = create_density_plot()
    print(g)
    dev.off()
  }
  # create filename
  fn_downloadname_fig <- reactive({
    
    fname = isolate(input$fig_fname)
    if(input$fformat=="png") filename <- paste0(fname,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(fname,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(fname,".jpg",sep="")
    return(filename)
  })
  
  # download handler
  output$downloadFigure <- downloadHandler(
    filename = fn_downloadname_fig,
    content = function(file) {
      fn_download_fig()
      file.copy(fn_downloadname_fig(), file, overwrite=T)
    }
  )  
  
  
})
