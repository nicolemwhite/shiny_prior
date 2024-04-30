library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyhelper)
library(tidyverse)
library(flextable)
library(officer)
library(RColorBrewer)
library(knitr)

#DISTRIBUTIONS
#simulate values based on distribution and parameter estimates
estimate_normal = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = sample_values[1]
    sigma_est = sample_values[2]
  }
  if(evidence_type=='perc'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    mu_est = (cilow_est*qnorm(1-p1)-cihigh_est*qnorm(p1))/(qnorm(1-p1)-qnorm(p1))
    sigma_est = (cihigh_est-cilow_est)/(qnorm(1-p1)-qnorm(p1))
  }
  return(c(mu_est,sigma_est))
}

check_normal_params <- function(evidence_type,sample_vals){
  if (evidence_type=='mean_se' & sample_vals[2]<=0){paste("Check distribution inputs (Normal distribution): Uncertainty estimate must be greater than 0")}
  else if (evidence_type=='perc' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Normal distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='perc' & (sample_vals[3]<=0|sample_vals[3]>=100)){paste("Check distribution inputs (Normal distribution): Define percentile coverage as a % value between 0 and 100")}
  else{NULL}
}

summary_stats_normal = function(dist_label,evidence_type,param_est,dps){
  mu_est = param_est[1];sigma_est = param_est[2]
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("norm",param_est,dps),
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(param_est[1],dps),' (',
                                                   roundz(qnorm(0.025,mu_est,sigma_est),dps),' to ',
                                                   roundz(qnorm(0.975,mu_est,sigma_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qnorm(0.5,mu_est,sigma_est),dps),' (',
                                                 roundz(qnorm(0.25,mu_est,sigma_est),dps),' to ',
                                                 roundz(qnorm(0.75,mu_est,sigma_est),dps),')'),check.names=F)
  return(out)
}



#gamma distribution
estimate_gamma = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mean_est = sample_values[1]
    sigma_est = sample_values[2]
    shape_est = (mean_est/sigma_est)^2
    scale_est = (sigma_est^2)/mean_est 
  }
  if(evidence_type=='perc'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    f <- function(a){(qgamma(p2,shape=exp(a),scale=1)/qgamma(p1,shape=exp(a),scale=1)) - cihigh_est/cilow_est}
    
    out = uniroot(f,interval=c(-1,1),extendInt = "yes",check.conv=T)
    shape_est = exp(out$root)
    scale_est = 0.5*(cilow_est/qgamma(p1,shape=shape_est,scale=1) + cihigh_est/qgamma(p2,shape=shape_est,scale=1))
  }
  
  return(c(shape_est,scale_est))
}

summary_stats_gamma = function(dist_label,evidence_type,param_est,dps){
  shape_est=param_est[1];scale_est=param_est[2]
  mu_est = shape_est*scale_est
  sigma_est = sqrt(shape_est)*scale_est
  
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("gamma",param_est,dps), #input[['dist_family']];parameter_es
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(mu_est,dps),' (',
                                                   roundz(qgamma(0.025,shape=shape_est,scale=scale_est),dps),' to ',
                                                   roundz(qgamma(0.975,shape=shape_est,scale=scale_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qgamma(0.5,shape=shape_est,scale=scale_est),dps),' (',
                                                 roundz(qgamma(0.25,shape=shape_est,scale=scale_est),dps),' to ',
                                                 roundz(qgamma(0.75,shape=shape_est,scale=scale_est),dps),')'),check.names=F)
  return(out)
}


check_gamma_params <- function(evidence_type,sample_vals){
  if (evidence_type=='mean_se' & (sample_vals[1]<=0|sample_vals[2]<=0)){paste("Check distribution inputs (Gamma distribution): Mean and uncertainty estimates must be greater than 0")}
  else if (evidence_type=='perc' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Gamma distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='perc' & sample_vals[1]<=0|sample_vals[2]<=0){paste("Check distribution inputs (Gamma distribution): Lower and upper interval values must be greater than 0")}
  else if (evidence_type=='perc' & (sample_vals[3]<=0|sample_vals[3]>=100)){paste("Check distribution input (Gamma distribution): Define percentile coverage as a % value between 0 and 100")}
  else{NULL}
}



#weibull distribution
estimate_weibull = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = sample_values[1]
    sigma_est = sample_values[2]
    f = function(a) (((sigma_est/mu_est)^2) - (gamma(1+2/exp(a))/((gamma(1+1/exp(a)))^2)) + 1)
    out <- uniroot(f,interval=c(-1,1),extendInt = "yes") 
    shape_est <- exp(out$root)
    scale_est <- mu_est/gamma(1+1/shape_est)
  }
  if(evidence_type=='perc'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    shape_est = (log(-log(1-p2))-log(-log(1-p1)))/(log(cihigh_est)-log(cilow_est))
    #take average value for scale_est to account for roundzing errors in inputs
    scale_est = 0.5*(cilow_est/((-log(1-p1))^(1/shape_est)) + cihigh_est/((-log(1-p2))^(1/shape_est)))
  }
  return(c(shape_est,scale_est))
  
}

summary_stats_weibull = function(dist_label,evidence_type,param_est,dps){
  shape_est = param_est[1];scale_est = param_est[2]
  mu_est = scale_est*gamma(1+1/shape_est)
  
  sigma_est = sqrt((scale_est^2)*(gamma(1+2/shape_est)-(gamma(1+1/shape_est))^2))
  
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("weib",param_est,dps), #input[['dist_family']];parameter_es
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(mu_est,dps),' (',
                                                   roundz(qweibull(0.025,shape=shape_est,scale=scale_est),dps),' to ',
                                                   roundz(qweibull(0.975,shape=shape_est,scale=scale_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qweibull(0.5,shape=shape_est,scale=scale_est),dps),' (',
                                                 roundz(qweibull(0.25,shape=shape_est,scale=scale_est),dps),' to ',
                                                 roundz(qweibull(0.75,shape=shape_est,scale=scale_est),dps),')'),check.names=F)
  return(out)
}


check_weibull_params <- function(evidence_type,sample_vals){
  if (evidence_type=='mean_se' & (sample_vals[1]<=0|sample_vals[2]<=0)){paste("Check distribution inputs (Weibull distribution): Mean and uncertainty estimates must be greater than 0")}
  else if (evidence_type=='perc' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Weibull distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='perc' & sample_vals[1]<=0|sample_vals[2]<=0){paste("Check distribution inputs (Weibull distribution): Lower and upper interval values must be greater than 0")}
  else if (evidence_type=='perc' & (sample_vals[3]<=0|sample_vals[3]>=100)){paste("Check distribution input (Weibull distribution): Define percentile coverage as a % value between 0 and 100")}
  else{NULL}
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
  if(evidence_type=='perc'){
    cilow_est = sample_values[1]
    cihigh_est = sample_values[2]
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    f <- function(a, x, p) {
      logit.prob <-  log(p/(1-p))
      fit<-pbeta(x,shape1=exp(a)[1],shape2=exp(a)[2])
      logit.fit <- log(fit/(1-fit))
      return(sum((logit.fit-logit.prob)^2))
    }
    out = try(optim(par=exp(c(0.1,0.1)),f,x=c(cilow_est,cihigh_est),p=c(p1,p2),method='BFGS'),silent=T)
    a_est = exp(out$par)[1]
    b_est = exp(out$par)[2]
  }
  
  
  return(c(a_est,b_est))
  
}  


summary_stats_beta = function(dist_label,evidence_type,param_est,dps){
  a_est = param_est[1];b_est = param_est[2]
  mu_est = a_est/(a_est+b_est)
  sigma_est = sqrt((a_est*b_est)/((a_est+b_est+1)*(a_est+b_est)^2))
  
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("beta",param_est,dps), #input[['dist_family']];parameter_es
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(mu_est,dps),' (',
                                                   roundz(qbeta(0.025,shape1=a_est,shape2=b_est),dps),' to ',
                                                   roundz(qbeta(0.975,shape1=a_est,shape2=b_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qbeta(0.5,shape1=a_est,shape2=b_est),dps),' (',
                                                 roundz(qbeta(0.25,shape1=a_est,shape2=b_est),dps),' to ',
                                                 roundz(qbeta(0.75,shape1=a_est,shape2=b_est),dps),')'),check.names=F)
  return(out)
}



#testing needed
check_beta_params <- function(evidence_type,sample_vals){
  if (evidence_type=='mean_se' & (any(sample_vals[1:2]<=0)|any(sample_vals[1:2]>=1))){paste("Check distribution inputs (Beta distribution): Mean and uncertainty estimates must be between 0 and 1")}
  else if (evidence_type=='perc' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Beta distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='perc' & (any(sample_vals[1:2]<=0)|any(sample_vals[1:2]>=1))){paste("Check distribution inputs (Beta distribution): Lower and upper interval values must be between 0 and 1")}
  else if (evidence_type=='perc' & (sample_vals[3]<=0|sample_vals[3]>=100)){paste("Check distribution inputs (Beta distribution): Define percentile coverage as a % value between 0 and 100")}
  else if (evidence_type=='r_n' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Beta distribution): Number of events must be less than sample size")}
  else{NULL}
}

#uniform: min/max only
estimate_uniform = function(sample_values){
  a_est = sample_values[1]
  b_est = sample_values[2]
  return(c(a_est,b_est))
}


summary_stats_uniform = function(dist_label,evidence_type,param_est,dps){
  a_est = param_est[1];b_est = param_est[2]
  mu_est = 0.5*(a_est+b_est)
  sigma_est = sqrt((1/12)*(b_est-a_est)^2)
  
  
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("unif",param_est,dps), #input[['dist_family']];parameter_es
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(mu_est,dps),' (',
                                                   roundz(qunif(0.025,min=a_est,max=b_est),dps),' to ',
                                                   roundz(qunif(0.975,min=a_est,max=b_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qunif(0.5,min=a_est,max=b_est),dps),' (',
                                                 roundz(qunif(0.25,min=a_est,max=b_est),dps),' to ',
                                                 roundz(qunif(0.75,min=a_est,max=b_est),dps),')'),check.names=F)
  return(out)
}

check_uniform_params <- function(sample_vals){
  if (sample_vals[1]>sample_vals[2]){paste("Check distibution inputs (Uniform distribution): Minimum value must be less than the Maximum value")}
  else{NULL}
}



#Lognormal
estimate_lognormal = function(evidence_type,sample_values){
  if(evidence_type=='mean_se'){
    mu_est = log(sample_values[1]^2/sqrt(sample_values[2]^2+sample_values[1]^2))
    sigma_est = sqrt(log(1+(sample_values[2]^2)/(sample_values[1]^2)))
  }
  if(evidence_type=='perc'){ 
    cilow_est = log(sample_values[1])
    cihigh_est = log(sample_values[2])
    cilevel_est = sample_values[3]/100
    p1 = (1-cilevel_est)/2
    p2 = 1-p1
    mu_est = (cilow_est*qnorm(1-p1)-cihigh_est*qnorm(p1))/(qnorm(1-p1)-qnorm(p1))
    sigma_est = (cihigh_est-cilow_est)/(qnorm(1-p1)-qnorm(p1))
  }
  return(c(mu_est,sigma_est))
  
}


summary_stats_lognormal = function(dist_label,evidence_type,param_est,dps){
  log_mu_est = param_est[1];log_sigma_est = param_est[2]
  
  mu_est = exp(param_est[1]+0.5*param_est[2]^2)
  sigma_est = sqrt((exp(param_est[2]^2)-1)*exp(2*param_est[1]+param_est[2]^2))
  
  
  out <- data.frame("Description"=dist_label, #input[['dist_label']]
                    "Form of evidence"= nice_names_evidence(evidence_type), #input[['parms_in]]
                    "Distribution" = nice_names("lnorm",param_est,dps),
                    "Mean (SD)" = paste0(roundz(mu_est,dps),' (',roundz(sigma_est,dps),')'), 
                    "Mean (95% Interval)" = paste0(roundz(mu_est,dps),' (',
                                                   roundz(qlnorm(0.025,log_mu_est,log_sigma_est),dps),' to ',
                                                   roundz(qlnorm(0.975,log_mu_est,log_sigma_est),dps),')'), 
                    
                    "Median (Q1 to Q3)" = paste0(roundz(qlnorm(0.5,log_mu_est,log_sigma_est),dps),' (',
                                                 roundz(qlnorm(0.25,log_mu_est,log_sigma_est),dps),' to ',
                                                 roundz(qlnorm(0.75,log_mu_est,log_sigma_est),dps),')'),check.names=F)
  return(out)
}


check_lognormal_params <- function(evidence_type,sample_vals){
  if (evidence_type=='mean_se' & (sample_vals[1]<=0|sample_vals[2]<=0)){paste("Check distribution inputs (Lognormal distribution): Mean and uncertainty estimates must be greater than 0")}
  else if (evidence_type=='perc' & sample_vals[1]>=sample_vals[2]){paste("Check distribution inputs (Lognormal distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='perc' & sample_vals[1]<=0|sample_vals[2]<=0){paste("Check distribution inputs (Lognormal distribution): Lower and upper interval values must be greater than 0")}
  else if (evidence_type=='perc' & (sample_vals[3]<=0|sample_vals[3]>=100)){paste("Check distribution inputs (Lognormal distribution): Define percentile coverage as a % value between 0 and 100")}
  else{NULL}
}

check_missing_inputs <-function(sample_vals){
  if(anyNA(sample_vals)==T){"At least one input is empty. Please specify all values based on the form of evidence selected before clicking 'Estimate distribution'"}
  else{NULL}
}

check_dist_params <- function(dist_name,evidence_type,sample_vals){
  switch(dist_name,
         'norm'= check_normal_params(evidence_type,sample_vals),
         'gamma'= check_gamma_params(evidence_type,sample_vals),
         'beta'= check_beta_params(evidence_type,sample_vals),
         'unif'=check_uniform_params(sample_vals),
         'lnorm' = check_lognormal_params(evidence_type,sample_vals),
         'weib' = check_weibull_params(evidence_type,sample_vals)
  )
}


check_all_inputs <- function(dist_obj,evidence_type,sample_vals){
  
  dist_label = dist_obj$dist_label
  dist_name = dist_obj$dist_name
  
  if(dist_label==''){"Empty description. Please provide a name for your distrbution in Step 1. Names already stored in the final output table will be overwritten."}
  else if(dist_label!='' & anyNA(sample_vals)==T){"At least one input is empty. Please enter all required values based on the form of evidence selected before clicking 'Estimate distribution'"}
  else if (dist_label!='' & anyNA(sample_vals)==F){
    switch(dist_name,
           'norm'= check_normal_params(evidence_type,sample_vals),
           'gamma'= check_gamma_params(evidence_type,sample_vals),
           'beta'= check_beta_params(evidence_type,sample_vals),
           'unif'=check_uniform_params(sample_vals),
           'lnorm' = check_lognormal_params(evidence_type,sample_vals),
           'weib' = check_weibull_params(evidence_type,sample_vals))}
  
  
}


estimate_dist_parameters = function(dist_name,evidence_type,sample_dat){
  switch(dist_name,
         'norm' = estimate_normal(evidence_type,sample_dat),
         'gamma' = estimate_gamma(evidence_type,sample_dat),
         'beta' = estimate_beta(evidence_type,sample_dat),
         'unif' = estimate_uniform(sample_dat),
         'lnorm' = estimate_lognormal(evidence_type,sample_dat),
         'weib' = estimate_weibull(evidence_type,sample_dat)
  )
}

check_parameter_estimates<-function(dist_name,param_est){
  if(dist_name %in% c('norm','lnorm') & param_est[2]<=0){"Solution not defined. Check all inputs or consider using a different distribution family"}
  else if (dist_name %in% c('gamma','beta','weib') & any(param_est<=0)==T){"Solution not defined. Check all inputs or consider using a different distribution family"}
  else if (anyNA(param_est)|any(is.null(param_est))){"Solution not defined. Check all inputs or consider using a different distribution family"}
  else{NULL}
}




renderInputs <- function(){
  wellPanel(id='dist_inputs',h5(strong(paste("Step 2: Define distribution inputs"))),
            h5(paste0('Select the form of evidence available to estimate the distribution in Step 1. 
                            Enter values for all inputs based on the form of evidence selected.')),
            #form of evidence based on distribution selected
            fluidRow(column(6,uiOutput(paste0("ui_dist")))),
            fluidRow(column(6,
                            uiOutput(paste0("ui_evidence")))))
}

#renderParmsIn: take info from define_evidence_info and setup numeric inputs to enter published evidence
#later: add default values depedning on distirbuiton
renderParmsIn <- function(evidence_in){
  parms = evidence_in$evidence_parms
  labels = evidence_in$labels
  
  parms_n = length(parms)
  div(
    lapply(seq(parms_n),function(v){
      numericInput(inputId=paste('sample',parms[v],sep='_'),
                   label=paste(labels[v]),
                   value=NA)}
    )
  )
}


#renderDistn: take information from applying define_dist_info and set up ui option conditional on chosen distributino
renderDistIn <- function(dist_obj){
  dist_name <-dist_obj$dist_name
  switch(dist_name,
         "norm" = selectInput(inputId = 'parms_in',label='Form of evidence',
                              choices = c('Mean with uncertainty'='mean_se','Percentiles'='perc'),
                              selected='mean_se'),
         
         "gamma" = selectInput(inputId = 'parms_in',label='Form of evidence',
                               choices = c('Mean with uncertainty'='mean_se','Percentiles'='perc'),
                               selected='mean_se'),
         
         "beta" = selectInput(inputId = 'parms_in',label='Form of evidence',
                              choices = c('Mean with uncertainty'='mean_se','Percentiles'='perc','Number of events, sample size' = 'r_n'),
                              selected='mean_se'),
         
         "unif" = selectInput(inputId = 'parms_in',label='Form of evidence',choices = c('Min/Max'='min_max'),selected='min_max'),
         
         "lnorm" = selectInput(inputId = 'parms_in',label='Form of evidence',
                               choices = c('Mean with uncertainty'='mean_se','Percentiles'='perc'),
                               selected='mean_se'),
         "weib" = selectInput(inputId = 'parms_in',label='Form of evidence',
                              choices = c('Mean with uncertainty'='mean_se','Percentiles'='perc'),
                              selected='mean_se')
  )
}



#global functions

define_dist_options <-function(dist_obj_name,dist_obj_label){
  if (is.null(dist_obj_name))
    return()
  distn = reactiveValues()
  distn$dist_name = dist_obj_name
  distn$dist_label = dist_obj_label 
  return(distn)
}

define_evidence_options <- function(input_obj){
  if(is.null(input_obj))
    return()
  state = reactiveValues()
  state$evidence_type = as.character(input_obj)
  d = unlist(strsplit(state$evidence_type,'_'))
  state$parms = d
  state$evidence_type = paste(state$parms,collapse='_')
  state$evidence_parms = if(state$evidence_type != 'perc'){state$parms} else{c('lower_perc','upper_perc','perc_level')}
  state$labels = switch(
    state$evidence_type,
    'mean_se' =  c('Mean','Uncertainty'),
    'perc' = c('Lower interval value','Upper interval value','Percentile coverage (%)'),
    'r_n' = c('Number of events','Sample size'),
    'min_max' = c('Minimum','Maximum'),
    'mode_min_max' = c('Most likely value','Minimum', 'Maximum')  )
  state$sample_inputs = paste('sample',state$evidence_parms,sep='_')
  return(state)
}

nice_names <- function(dist_obj_name,param_est,dps){
  switch(dist_obj_name,
         'norm' = paste0('Normal(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')'),
         'gamma' = paste0('Gamma(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')'),
         'beta' = paste0('Beta(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')'),
         'unif' = paste0('Uniform(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')'),
         'lnorm' = paste0('Log-normal(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')'),
         'weib' = paste0('Weibull(',roundz(param_est[1],dps),',',roundz(param_est[2],dps),')')
  )
}

nice_names_evidence <- function(evidence_obj){
  d = unlist(strsplit(evidence_obj,'_'))
  parms = d
  evidence_type = paste(parms,collapse='_')
  switch(evidence_type,
         'mean_se' = paste0('Mean with uncertainty'),
         'perc' = paste0('Percentiles'),
         'r_n' = paste0('Number of events, sample size'),
         'min_max' = paste0('Min/Max'),
         'mode_min_max' = paste0('Most likely value with Min/Max'),
         'mean' = paste0('Mean')
  )
}



colourschemes <- list('Greyscale' = c("#000000","#737373","#BDBDBD","#D9D9D9","#F0F0F0"),#"Greys",
                      'Accent' = c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#666666"),
                      'Dark2'= brewer.pal(n=8,name="Dark2"),
                      'Paired'=brewer.pal(n=8,name="Paired"),
                      "Set1" = brewer.pal(n=8,name="Set1"),
                      "Set2"=brewer.pal(n=8,name="Set2"),
                      "Set3"=brewer.pal(n=8,name="Set3"),
                      'Colourblind-1'=  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      'Colourblind-2'=  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
)


themes <- list("Minimal" = theme_minimal(),
               "Light" = theme_light(),
               "Black/White" = theme_bw(),
               "Classic" = theme_classic(),
               "Gray"=theme_gray())
legend_positions <- list("Yes" = 'right',"No" = 'none')

removeReactiveValuesIndex <- function(rv, ind) { .subset2(rv, "impl")$.values$remove(ind) }

calc_dens <- function(dist_family,parm_est,dist_name){
  switch(dist_family,
         'norm' = geom_function(fun = dnorm,args=list(mean=parm_est[1],sd=parm_est[2]),size=1.25,aes(colour=dist_name)),
         'gamma' = geom_function(fun = dgamma,args=list(shape=parm_est[1],scale=parm_est[2]),size=1.25,aes(colour=dist_name)),
         'beta' = geom_function(fun = dbeta,args=list(shape1=parm_est[1],shape2=parm_est[2]),size=1.25,aes(colour=dist_name)),
         'unif' = geom_function(fun = dunif,args=list(min=parm_est[1],max=parm_est[2]),size=1.25,aes(colour=dist_name)),
         'lnorm' = geom_function(fun = dlnorm,args=list(meanlog=parm_est[1],sdlog=parm_est[2]),size=1.25,aes(colour=dist_name)),
         'weib' = geom_function(fun = dweibull,args=list(shape=parm_est[1],scale=parm_est[2]),size=1.25,aes(colour=dist_name)))
}


calc_xlim <- function(dist_family,parm_est){
  switch(dist_family,
         'norm' = qnorm(p=c(0.0001,0.9999),mean=parm_est[1],sd=parm_est[2]),
         'gamma' = qgamma(p=c(0.0001,0.9999),shape=parm_est[1],scale=parm_est[2]),
         'beta' = qbeta(p=c(0.0001,0.9999),shape1=parm_est[1],shape2=parm_est[2]),
         'unif' = qunif(p=c(0.0001,0.9999),min=parm_est[1],max=parm_est[2]),
         'lnorm' = qlnorm(p=c(0.0001,0.9999),meanlog=parm_est[1],sdlog=parm_est[2]),
         'weib' = qweibull(p=c(0.0001,0.9999),shape=parm_est[1],scale=parm_est[2]))
}

roundz<-function(x, digits){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}


sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

FitFlextableToPage <- function(ft, pgwidth = 6){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}
# twitter_url <- "https://twitter.com/intent/tweet?text=ShinyPrior%20-%20estimating%20distributions%20using%20published%20evidence&url=https://aushsi.shinyapps.io/ShinyPrior"
# linkedin_url <- "http://www.linkedin.com/shareArticle?mini=true&url=https://aushsi.shinyapps.io/ShinyPrior"