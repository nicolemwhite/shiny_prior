library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(triangle)

#DISTRIBUTIONS
#simulate values based on distribution and parameter estimates
estimate_normal = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = sample_values[1]
    sigma_est = sample_values[2]
  }
  if(evidence_type=='ci'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    mu_est = (cilow_est*qnorm(1-p1)-cihigh_est*qnorm(p1))/(qnorm(1-p1)-qnorm(p1))
    sigma_est = (cihigh_est-cilow_est)/(qnorm(1-p1)-qnorm(p1))
  }

  return(c(Mean=mu_est,`Standard deviation`=sigma_est))
}
#gamma distribution: mean_se only
estimate_gamma = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
  mean_est = sample_values[1]
  sigma_est = sample_values[2]
  shape_est = (mean_est/sigma_est)^2
  scale_est = (sigma_est^2)/mean_est
  }
  if(evidence_type=='ci'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    values = get.gamma.par(p=c(p1,p2),q=c(cilow_est,cihigh_est),show.output=F)
    names(values) = NULL
    shape_est = values[1]
    scale_est = 1/values[2]
  }
  
  return(c(Shape=shape_est,Scale=scale_est))
}
#beta distribution
estimate_beta = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = sample_values[1]
    sigma_est = sample_values[2]
    aplusb = (mu_est*(1-mu_est)/(sigma_est^2))-1
    a_est = aplusb*mu_est
    b_est = aplusb*(1-mu_est)
  }
  if(evidence_type=='r_n'){
    a_est = sample_values[1]
    b_est = sample_values[2] - a_est
  }
  if(evidence_type=='ci'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    values = get.beta.par(p=c(p1,p2),q=c(cilow_est,cihigh_est),show.output=F)
    names(values) = NULL
    a_est = values[1]
    b_est = values[2]
  }
  out = c(a_est,b_est)
  return(c(`Shape (alpha)`=a_est,`Shape (beta)` = b_est))
}  

#uniform: min/max only
estimate_uniform = function(sample_values){
  a_est = sample_values[1]
  b_est = sample_values[2]
  return(c(Minimum=a_est,Maximum=b_est))
}

#triangular: mode with min/max only
estimate_triangular = function(sample_values){
  c_est = sample_values[1]
  a_est = sample_values[2]
  b_est = sample_values[3]
  return(c(Mode = c_est,Minimum=a_est,Maximum=b_est))
}

#poisson: mean only
estimate_poisson = function(sample_values){
  mu_est = sample_values[1]
  return(Mean = mu_est)
}
#Lognormal
estimate_lognormal = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = log(sample_values[1])
    sigma_est = sample_values[2]
  }
  if(evidence_type=='ci'){ 
    cilow_est = log(sample_values[1])
    cihigh_est = log(sample_values[2])
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    mu_est = (cilow_est*qnorm(1-p1)-cihigh_est*qnorm(p1))/(qnorm(1-p1)-qnorm(p1))
    sigma_est = (cihigh_est-cilow_est)/(qnorm(1-p1)-qnorm(p1))
  }
  return(c(Mean=mu_est,`Standard deviation`=sigma_est))
}

sim_values = function(dist_name,n_samples,param_estimates){
  if(is.null(dist_name)||is.null(n_samples))
    return()
  d = gsub('_.*','',dist_name)
  
  switch(d,
         'norm' = rnorm(n_samples,param_estimates[1],param_estimates[2]),
         'gamma' = rgamma(n_samples,shape = param_estimates[1],scale = param_estimates[2]),
         'beta' = rbeta(n_samples,param_estimates[1],param_estimates[2]),
         'unif' = runif(n_samples,param_estimates[1],param_estimates[2]),
         'tri' = rtriangle(n_samples,param_estimates[2],param_estimates[3],param_estimates[1]),
         'pois' = rpois(n_samples,param_estimates[1]),
         'lnorm' = rlnorm(n_samples,param_estimates[1],param_estimates[2])
  )
}



#renderDistn: take information from applying define_dist_info and set up ui option conditional on chosen distributino
renderDistIn <- function(dist_info){
  suffix = dist_info$suffix #gsub('.*_','',dist_info)
  dist_name = dist_info$dist_name #gsub('_.*','',dist_info)
  
  switch(dist_name,
         "norm" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                              choices = c('Mean with uncertainty'=paste0('mean_se_',suffix),
                                          'Confidence interval'=paste0('ci_',suffix)),
                              selected=paste0('mean_se_',suffix)),
         "gamma" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                               choices = c('Mean with uncertainty'=paste0('mean_se_',suffix),
                                           'Confidence interval'=paste0('ci_',suffix)),
                               selected=paste0('mean_se_',suffix)),
         "beta" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                              choices = c('Mean with uncertainty'=paste0('mean_se_',suffix),
                                          'Confidence interval'=paste0('ci_',suffix),
                                          'Number of events, sample size' = paste0('r_n_',suffix)),
                              selected=paste0('r_n_',suffix)),
         "unif" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                              choices = c('Min/Max'=paste0('min_max_',suffix)),
                              selected=paste0('min_max_',suffix)),
         "tri" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                             choices = c('Most likely value with Min/Max'=paste0('mode_min_max_',suffix)),
                             selected=paste0('mode_min_max_',suffix)),
         'pois' = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                              choices = c('Mean'=paste0('mean_',suffix)),
                              selected=paste0('mean_',suffix)),
         "lnorm" = selectInput(inputId = paste0('parms_in_',suffix),label='Form of evidence',
                              choices = c('Mean with uncertainty'=paste0('mean_se_',suffix),
                                          'Confidence interval'=paste0('ci_',suffix)),
                              selected=paste0('mean_se_',suffix))         
  )    
}

#renderParmsIn: take info from define_evidence_info and setup numeric inputs to enter published evidence
#later: add default values depedning on distirbuiton
renderParmsIn <- function(evidence_in,dist_obj){
  parms = evidence_in$evidence_parms
  suffix = evidence_in$suffix
  labels = evidence_in$labels

  parms_n = length(parms)
  div(
    lapply(seq(parms_n),function(v){
      numericInput(inputId=paste('sample',parms[v],suffix,sep='_'),
                   label=paste(labels[v]),
                   value=NA)}
    )
  )
}

#global functions
define_dist_options <-function(dist_obj){
  if (is.null(dist_obj))
    return()
  distn = reactiveValues()
  distn$dist_name = gsub('_.*','',dist_obj)
  distn$suffix = gsub('.*_','',dist_obj) 
  return(distn)
}

define_evidence_options <- function(input_obj){
  if(is.null(input_obj))
    return()
  state = reactiveValues()
  state$evidence_type_suffix = as.character(input_obj)
  d = unlist(strsplit(state$evidence_type_suffix,'_'))
  state$parms = d[-length(d)]
  state$evidence_type = paste(state$parms,collapse='_')
  state$suffix = d[length(d)]
  state$evidence_parms = if(state$evidence_type != 'ci'){state$parms} else{c('lower_ci','upper_ci','ci_level')}
  state$labels = switch(
    state$evidence_type,
    'mean_se' =  c('Mean','Uncertainty'),
    'ci' = c('Lower interval value','Upper interval value','Confidence level (%)'),
    'r_n' = c('Number of events','Sample size'),
    'min_max' = c('Minimum','Maximum'),
    'mode_min_max' = c('Most likely value','Minimum', 'Maximum'),
    'mean' = c('Mean rate')
  )
  #getLabels(state$evidence_type)
  state$sample_inputs = paste('sample',state$evidence_parms,state$suffix,sep='_')
  return(state)
}

nice_names <- function(dist_obj){
  x = gsub('.*_','',dist_obj)
  dist_name = gsub('_.*','',dist_obj)
  switch(dist_name,
         'norm' = paste0('Normal distribution'),
         'gamma' = paste0('Gamma distribution'),
         'beta' = paste0('Beta distribution'),
         'unif' = paste0('Uniform distribution'),
         'tri' = paste0('Triangular distribution'),
         'pois' = paste0('Poisson distribution'),
         'lnorm' = paste0('Log-normal distribution')
  )
}

nice_names_evidence <- function(evidence_obj){
  d = unlist(strsplit(evidence_obj,'_'))
  parms = d[-length(d)]
  evidence_type = paste(parms,collapse='_')
  switch(evidence_type,
         'mean_se' = paste0('Mean with uncertainty'),
         'ci' = paste0('Confidence interval'),
         'r_n' = paste0('Number of events, sample size'),
         'min_max' = paste0('Min/Max'),
         'mode_min_max' = paste0('Most likely value with Min/Max'),
         'mean' = paste0('Mean')
  )
}



# Define server logic 
shinyServer(function(input, output,session) {


  #define number of priors as reactive function
  n_prior = reactive({as.numeric(input[['compare']])})
  
  #renderUI conditional on number of priors
  #key inputs are distribution type (in dist_info) and form of published evidence (in evidence_info)  
   dist_info = reactive({lapply(seq(n_prior()),function(x) define_dist_options(input[[paste0('dist_',x)]]))})
   evidence_info = reactive({lapply(seq(n_prior()),function(x) define_evidence_options(input[[paste0('parms_in_',x)]]))})

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

  #take user defined values and estimate distribution-specific parameters
  get_sample_values = function(){
    out = lapply(seq(n_prior()), function(x){ 
      sample_labels = evidence_info()[[x]]$sample_inputs #change evidence_into to get_evidence()? ok for now
      sapply(seq(sample_labels), function(z) input[[paste0(sample_labels[z])]])
    })
    return(out)
  }

  estimate_dist_parameters = function(){
    lapply(seq(n_prior()),function(x){
      evidence_type = evidence_info()[[x]]$evidence_type #need to change to evidence_info_1,evidence_info_2. see next comment
      dist_name = dist_info()[[x]]$dist_name
      sample_dat = estimate_info()[[x]]
      switch(dist_name,
             'norm' = estimate_normal(evidence_type,sample_dat),
             'gamma' = estimate_gamma(evidence_type,sample_dat),
             'beta' = estimate_beta(evidence_type,sample_dat),
             'unif' = estimate_uniform(sample_dat),
             'tri' = estimate_triangular(sample_dat),
             'pois' = estimate_poisson(sample_dat),
             'lnorm' = estimate_lognormal(evidence_type,sample_dat)
      )
    })
  }

  #could change this to lapply instead of inside function
  estimate_info = reactive({get_sample_values()})
  parameter_estimates = reactive({estimate_dist_parameters()})
  
  
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
                 Parameter = names(parameter_estimates()[[x]]),Estimate=(parameter_estimates()[[x]])) 
    } )

    dat = do.call(rbind.data.frame,dat_list)
    return(dat) 
  }

  
  print_summary_tab = function(){
    dat = simulated_values()$dat
    dist_names = simulated_values()$dist_names
    dist_names = gsub('distribution','',dist_names)
    evidence_type = simulated_values()$evidence_type
    tab_dat = dat %>% group_by(variable) %>% summarise(Minimum=min(value,na.rm=F),
                                                       Q1=quantile(value,0.25,na.rm=F),
                                                       Median= quantile(value,0.5,na.rm=F),
                                                       Q3=quantile(value,0.75,na.rm=F),
                                                       Maximum=max(value,na.rm=F)) %>%
      mutate(variable = gsub('.*_','',variable)) %>% rename('Prior'=variable) %>%
      add_column(.,'Distribution'=dist_names,Evidence=evidence_type) %>% 
        select(Prior,Distribution,Evidence,everything())
    
    return(data.frame(tab_dat))
    
  }
  
  simulated_values = eventReactive(input$go,({
    get_draws()
    })
  )
  parms_est = eventReactive(input$go,({
    tabluate_param_estimates()
  })
  )
  
  display_summary_table =eventReactive(input$go,{(
    print_summary_tab()
  )})
  
  build_histograms = reactive({
    aushsi.colours<-c("#00AEEF",'#AA4371')
    plot_dat = simulated_values()$dat
    ggplot(plot_dat,aes(x=value,colour=variable))+
      geom_histogram(aes(y=..density..,fill=variable),bins=40,alpha=.25,position='identity')+
      scale_fill_manual(values=c(aushsi.colours[2],aushsi.colours[1]))+
      scale_colour_manual(values=c(aushsi.colours[2],aushsi.colours[1]))+
      geom_line(aes(y = ..density..),size=.75, stat = 'density')+
      #scale_colour_manual(values=c(aushsi.colours[1],aushsi.colours[2]))+
      xlab('Sampled value')+ylab('')+
      theme(text=element_text(size=14),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x = element_text(size=14),
            legend.title = element_blank(),
            legend.position = 'top',
            legend.direction = 'horizontal')+
      expand_limits(x=c(floor(min(plot_dat$value)),ceiling(max(plot_dat$value)))) +
      scale_x_continuous(breaks=round(seq(floor(min(plot_dat$value)),ceiling(max(plot_dat$value)),length.out=10),2))
  })
  
  #Outputs
  #plot histogram
  output$hist = renderPlot({
    build_histograms()
  })  
  
  output$param_est = renderTable({
    parms_est()
  })  
  
  observeEvent(input$compare==2,{
    shinyjs::toggle("prior_2")
  })
  
  observeEvent(input$compare==2,{
    shinyjs::toggle("param_est_2")
  })

  #print summary statistics
  output$stats <-renderTable({
    display_summary_table()
  })
  
  #download csv file with simulations
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prior_simulations",".csv", sep = "")
    },
    content = function(file) {
      dat = simulated_values() 
      dat = spread(dat,variable,value)
      write.csv(dat, file, row.names = FALSE)
    }
  )


})
