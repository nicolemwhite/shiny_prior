shinyServer(function(input, output,session) {

  #renderUI 
  #Prior 1
  dist_info_1 <- reactive(define_dist_options(input[['dist_1']]))  #list with dist_name, suffix
  evidence_info_1 <- reactive({define_evidence_options(input[['parms_in_1']])}) #list with evidence_type, labels, suffix
  
  
  output$ui_dist_1 <- renderUI({
    renderDistIn(dist_info_1())
  })
  output$ui_evidence_1 <-renderUI({
    renderParmsIn(evidence_info_1(),dist_info_1())
  })
  
  
  #Prior 2
  dist_info_2 <- reactive(define_dist_options(input[['dist_2']]))  #list with dist_name, suffix
  evidence_info_2 <- reactive({define_evidence_options(input[['parms_in_2']])}) #list with evidence_type, labels, suffix
  
  output$ui_dist_2 <- renderUI({
    renderDistIn(dist_info_2())
  })
  output$ui_evidence_2 <-renderUI({
    renderParmsIn(evidence_info_2(),dist_info_2())
  })
  
  observeEvent(input[['compare']]==2,{
    shinyjs::toggle(id='prior_setup_2')
  })
  

  #combine into dist_info, evidence_info
  dist_info <- reactive({
    list(dist_info_1(),dist_info_2())
  })
  evidence_info <- reactive({
    list(evidence_info_1(),evidence_info_2())
  })
  
  estimate_info = reactive({lapply(seq(n_prior()),function(x){
    sample_labels = evidence_info()[[x]]$sample_inputs
    sapply(seq(sample_labels),function(z) input[[paste0(sample_labels[z])]])
    }) 
  })
  
  #define number of priors as reactive function
  n_prior = reactive({as.numeric(input[['compare']])})
  
  
  #estimate parameters
  parameter_estimates = reactive({lapply(seq(n_prior()),function(x){
     validate(
       check_missing_inputs(estimate_info()[[x]])
     )
    validate(
      check_dist_params(dist_info()[[x]]$dist_name,evidence_info()[[x]]$evidence_type,estimate_info()[[x]],x)
    )
    estimate_dist_parameters(dist_name=dist_info()[[x]]$dist_name,
                             evidence_type= evidence_info()[[x]]$evidence_type,
                             sample_dat=estimate_info()[[x]])})
  })

  #theoretical moments based on parameters estimates
  dist_summary = function(){
    n_prior = as.numeric(input[['compare']])
    dat_list = lapply(seq(n_prior),function(x){
      data.frame(Prior=x,
                 Distribution = nice_names(input[[paste0('dist_',x)]]),
                 Evidence = simulated_values()$evidence_type[[x]],
                 Mean = parameter_estimates()[[x]]['Mean'],
                 SD = parameter_estimates()[[x]]['Standard deviation'],
                 p2.5 = parameter_estimates()[[x]]['p2.5'],
                 p25 = parameter_estimates()[[x]]['p25'],                 
                 p50 = parameter_estimates()[[x]]['p50'],
                 p75 = parameter_estimates()[[x]]['p75'],
                 p97.5 = parameter_estimates()[[x]]['p97.5'])
    })
    dat = do.call(rbind.data.frame,dat_list)
    dat$Distribution =  gsub('distribution','',dat$Distribution)
    
    return(dat) 
  }
  
  #main simulation functions for output
  get_draws = function(){
    out = reactiveValues()
    n_prior = as.numeric(input[['compare']])
    dat = sapply(seq(n_prior),function(x) sim_values(input[[paste0('dist_',x)]],input[['samples']],parameter_estimates()[[x]]))
    dat = as.data.frame(dat)
    colnames(dat) = paste0('Prior_',seq(n_prior))
    dat = dat %>% rowid_to_column(.,var='simulation')
    out$dat = gather(dat,variable,value,-simulation)
    
    out$dist_names = lapply(seq(n_prior),function(x) nice_names(input[[paste0('dist_',x)]]))
    out$evidence_type = lapply(seq(n_prior),function(x) nice_names_evidence(input[[paste0('parms_in_',x)]]))
    
    return(out)
  }
  
  tabluate_param_estimates = function(){
    n_prior = as.numeric(input[['compare']])
    dat_list = lapply(seq(n_prior),function(x){
      data.frame(Prior=x,
                 Distribution = nice_names(input[[paste0('dist_',x)]]),
                 Evidence = simulated_values()$evidence_type[[x]],
                 Parameter = names(parameter_estimates()[[x]][1:2]),
                 Estimate=(parameter_estimates()[[x]][1:2])
      ) 
    })
    
    dat = do.call(rbind.data.frame,dat_list)
    return(dat) 
  }

  print_summary_tab = function(){
    dat = simulated_values()$dat
    dist_names = simulated_values()$dist_names
    dist_names = gsub('distribution','',dist_names)
    evidence_type = simulated_values()$evidence_type
    tab_dat = dat %>% group_by(variable) %>% summarise(Mean = mean(value,na.rm=T),
                                                       SD = sd(value,na.rm=T),
                                                       `p2.5` = quantile(value,0.025,na.rm=T),
                                                       `p25` = quantile(value,0.25,na.rm=T),
                                                       `p50` = quantile(value,0.5,na.rm=T),
                                                       `p75` = quantile(value,0.75,na.rm=T),
                                                       `p97.5` = quantile(value,0.975,na.rm=T)) %>%
      mutate(variable = gsub('.*_','',variable)) %>% rename('Prior'=variable) %>%
      add_column(.,'Distribution'=dist_names,Evidence=evidence_type) %>% 
      select(Prior,Distribution,Evidence,everything())
    
    return(data.frame(tab_dat))
    
  }
  
  #eventReactive
  simulated_values = eventReactive(input$go,({
    get_draws()
  })
  )
  parms_est = eventReactive(input$go,({
    tabluate_param_estimates()
  })
  )
  
  parms_est_theory = eventReactive(input$go,({
    dist_summary()
  })
  )

  display_summary_table =eventReactive(input$go,{(
    print_summary_tab()
  )})
  


  build_histograms = reactive({
    #aushsi.colours<-hist_colours()
    plot_colours = colourschemes[[input$colourscheme]]
    plot_theme = themes[[input$theme]]
    legend_posn = legend_positions[[input$legend]]
      
    plot_dat = simulated_values()$dat
    dist_names = simulated_values()$dist_names
    if(sum(dist_names=='Poisson distribution')==0){
      g = ggplot(plot_dat,aes(x=value,colour=variable))+
        geom_histogram(aes(y=..density..,fill=variable),bins=input$nbins,alpha=input$opacity,position='identity')+
        scale_fill_manual(values=c(plot_colours[2],plot_colours[1]))+
        scale_colour_manual(values=c(plot_colours[2],plot_colours[1]))+
        xlab(input$xlabtext)+ylab(input$ylabtext)+
        plot_theme+
        theme(text=element_text(size=14),
              axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),              
              legend.title = element_blank(),
              legend.position = legend_posn,
              legend.direction = 'horizontal') +
        scale_x_continuous(n.breaks=10)
      
    }
    #special case: poisson distribution
    if(sum(dist_names=='Poisson distribution')>0){
      g = ggplot(plot_dat,aes(x=value,colour=variable))+
        geom_histogram(aes(y=..density..,fill=variable),bins = input$nbins,alpha=input$opacity,position='identity')+
        scale_fill_manual(values=c(plot_colours[2],plot_colours[1]))+
        scale_colour_manual(values=c(plot_colours[2],plot_colours[1]))+
        xlab(input$xlabtext)+ylab(input$ylabtext)+
        plot_theme+
        theme(text=element_text(size=14),
              axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),              
              legend.title = element_blank(),
              legend.position = input$legend_position,
              legend.direction = 'horizontal') +
        expand_limits(x=c(floor(min(plot_dat$value)),ceiling(max(plot_dat$value)))) +
        scale_x_continuous()
    }
    g
  })
  
  #Outputs

  output$n_draws = renderText({
    paste('Number of simulations per prior:',input$samples)
  })
  #plot histogram
  output$hist = renderPlot({
    build_histograms()
  })  
  
  output$param_est = renderTable({
    parms_est()
  })  


  #print summary statistics
  output$stats_simulated <-renderTable(spacing='xs',{
    display_summary_table()
  })
  
  output$stats_theory = renderTable({
    parms_est_theory()
  })

  
  observeEvent(input$resetAll, {
    updateRadioButtons(session, "compare", selected=1)
    updateNumericInput(session,'samples',value=10000)
    updateTextInput(session,'xlabtext',value='Sampled value')
    updateTextInput(session,'ylabtext',value='Density')
    updateRadioButtons(session,"legend",selected="Yes")
    updateSelectInput(session,"theme",selected = 'Minimal')
    updateSelectInput(session,"colourscheme",selected = 'Colour')
    shinyjs::reset("prior_setup_1")
    shinyjs::reset("prior_setup_2")
  })
  
  
  #download csv file with simulations
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prior_simulations.csv", sep = "")
    },
    content = function(file) {
      dat = simulated_values()$dat 
      dat = spread(dat,variable,value)
      write.csv(dat, file, row.names = FALSE)
    }
  )

  fn_download <- function()
  {
    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    if(input$fformat=="pdf") fheight <- round(fheight*0.3937,2)
    if(input$fformat=="pdf") fwidth <- round(fwidth*0.3937,2)
    
    # open file dependent on format    
    if(input$fformat=="png") png(fn_downloadname(), height=fheight, width=fwidth, res=fres, units="cm")
    if(input$fformat=="tiff") tiff(fn_downloadname(), height=fheight, width=fwidth, res=fres, units="cm", compression="lzw")
    if(input$fformat=="jpeg") jpeg(fn_downloadname(), height=fheight, width=fwidth, res=fres, units="cm", quality=100)
    if(input$fformat=="pdf") pdf(fn_downloadname(), height=fheight, width=fwidth)
    
    g = build_histograms()
    print(g)
    dev.off()
  }
  # create filename
  fn_downloadname <- reactive({
    
    fname = 'histogram'
    if(input$fformat=="png") filename <- paste0(fname,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(fname,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(fname,".jpg",sep="")
    if(input$fformat=="pdf") filename <- paste0(fname,".pdf",sep="")
    return(filename)
  })
  
  # download handler
  output$downloadFigure <- downloadHandler(
    filename = fn_downloadname,
    content = function(file) {
      fn_download()
      file.copy(fn_downloadname(), file, overwrite=T)
    }
  )  
 
  
})
