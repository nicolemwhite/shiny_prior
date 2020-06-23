library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(triangle)
library(rriskDistributions)

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

check_normal_params <- function(evidence_type,sample_vals,prior_number){
  if (evidence_type=='mean_se' & sample_vals[2]<=0){paste("Check inputs defined for Prior",prior_number,"(Normal distribution): Uncertainty estimate must be greater than 0")}
  else if (evidence_type=='ci' & sample_vals[1]>=sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Normal distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='ci' & (sample_vals[3]<0|sample_vals[3]>100)){paste("Check inputs defined for Prior",prior_number,"(Normal distribution): Define confidence level as a % value between 0 and 100")}
  else{NULL}
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

check_gamma_params <- function(evidence_type,sample_vals,prior_number){
  if (evidence_type=='mean_se' & (sample_vals[1]<=0|sample_vals[2]<=0)){paste("Check inputs defined for Prior",prior_number,"(Gamma distribution): Mean and uncertainty estimates must be greater than 0")}
  else if (evidence_type=='ci' & sample_vals[1]>=sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Gamma distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='ci' & sample_vals[1]<=0|sample_vals[2]<=0){paste("Check inputs defined for Prior",prior_number,"(Gamma distribution): Lower and upper interval values must be greater than 0")}
  else if (evidence_type=='ci' & (sample_vals[3]<0|sample_vals[3]>100)){paste("Check inputs defined for Prior",prior_number,"(Gamma distribution): Define confidence level as a % value between 0 and 100")}
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

check_beta_params <- function(evidence_type,sample_vals,prior_number){
  if (evidence_type=='mean_se' & (!sample_vals[1] %in% seq(0,1)|!sample_vals[2] %in% seq(0,1))){paste("Check inputs defined for Prior",prior_number,"(Beta distribution): Mean and uncertainty estimates must be between 0 and 1")}
  else if (evidence_type=='ci' & sample_vals[1]>=sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Beta distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='ci' & (!sample_vals[1] %in% seq(0,1)|!sample_vals[2] %in% seq(0,1))){paste("Check inputs defined for Prior",prior_number,"(Beta distribution): Lower and upper interval values must be between 0 and 1")}
  else if (evidence_type=='ci' & (sample_vals[3]<0|sample_vals[3]>100)){paste("Check inputs defined for Prior",prior_number,"(Beta distribution): Define confidence level as a % value between 0 and 100")}
  else if (evidence_type=='r_n' & sample_vals[1]>sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Beta distribution): Number of events must be less than sample size")}
  else{NULL}
}

#uniform: min/max only
estimate_uniform = function(sample_values){
  a_est = sample_values[1]
  b_est = sample_values[2]
  return(c(Minimum=a_est,Maximum=b_est))
}

check_uniform_params <- function(sample_vals,prior_number){
  if (sample_vals[1]>sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Uniform distribution): Minimum value must be lower than Maximum value")}
  else{NULL}
}


#triangular: mode with min/max only
estimate_triangular = function(sample_values){
  c_est = sample_values[1]
  a_est = sample_values[2]
  b_est = sample_values[3]
  return(c(Mode = c_est,Minimum=a_est,Maximum=b_est))
}

check_triangular_params <- function(sample_vals,prior_number){
  if (sample_vals[2]>sample_vals[1]){paste("Check inputs defined for Prior",prior_number,"(Triangular distribution): Minimum value must be less than most likely value")}
  else if (sample_vals[3]<sample_vals[1]){paste("Check inputs defined for Prior",prior_number,"(Triangular distribution): Maximum value must be greater than most likely value")}
  else if (sample_vals[3]<sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Triangular distribution): Maximum value must be greater than minimum value")}
  else{NULL}
}


#poisson: mean only
estimate_poisson = function(sample_values){
  mu_est = sample_values[1]
  return(c(Mean = mu_est))
}

check_poisson_params <- function(sample_vals,prior_number){
  if (sample_vals[1]<=0){paste("Check inputs defined for Prior",prior_number,"(Poisson distribution): Mean value must be greater than 0")}
  else{NULL}
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

check_lognormal_params <- function(evidence_type,sample_vals,prior_number){
  if (evidence_type=='mean_se' & (sample_vals[1]<=0|sample_vals[2]<=0)){paste("Check inputs defined for Prior",prior_number,"(Lognormal distribution): Mean and uncertainty estimates must be greater than 0")}
  else if (evidence_type=='ci' & sample_vals[1]>=sample_vals[2]){paste("Check inputs defined for Prior",prior_number,"(Lognormal distribution): Lower interval value must be less than upper interval value")}
  else if (evidence_type=='ci' & sample_vals[1]<=0|sample_vals[2]<=0){paste("Check inputs defined for Prior",prior_number,"(Lognormal distribution): Lower and upper interval values must be greater than 0")}
  else if (evidence_type=='ci' & (sample_vals[3]<0|sample_vals[3]>100)){paste("Check inputs defined for Prior",prior_number,"(Lognormal distribution): Define confidence level as a % value between 0 and 100")}
  else{NULL}
}

check_missing_inputs <-function(sample_vals){
  if(anyNA(sample_vals)==T){"At least one input is empty. Please specify value(s) or select 'Single prior distribution' option"}
  else{NULL}
}

check_dist_params <- function(dist_name,evidence_type,sample_vals,prior_number){
  switch(dist_name,
         'norm'= check_normal_params(evidence_type,sample_vals,prior_number),
         'gamma'= check_gamma_params(evidence_type,sample_vals,prior_number),
         'beta'= check_beta_params(evidence_type,sample_vals,prior_number),
         'unif'=check_uniform_params(sample_vals,prior_number),
         'tri' = check_triangular_params(sample_vals,prior_number),
         'pois' = check_poisson_params(sample_vals,prior_number),
         'lnorm' = check_lognormal_params(evidence_type,sample_vals,prior_number)
  )
}

estimate_dist_parameters = function(dist_name,evidence_type,sample_dat){
  switch(dist_name,
         'norm' = estimate_normal(evidence_type,sample_dat),
         'gamma' = estimate_gamma(evidence_type,sample_dat),
         'beta' = estimate_beta(evidence_type,sample_dat),
         'unif' = estimate_uniform(sample_dat),
         'tri' = estimate_triangular(sample_dat),
         'pois' = estimate_poisson(sample_dat),
         'lnorm' = estimate_lognormal(evidence_type,sample_dat)
  )
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

