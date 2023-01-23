---
title: 'ShinyPrior: A tool for estimated probability distributions from published
  evidence'
author: "Nicole White"
date: "2023-01-23"
output: html_document

bibliography: reflist.bib  

#mathjax: "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
---

<style type="text/css">
body{
font-size: 12pt;
font-family: "Arial";
.basic-styling td,
.basic-styling th {
border: 1px solid #999;
padding: 0.5rem;

}
</style>







# **Overview**


***ShinyPrior*** is a web-based application for estimating probability distributions from summary statistics. The application was originally designed to help researchers characterise uncertainty in model inputs, as part of health economic evaluation [@drummond2015methods]. The application was developed using the <tt>shiny</tt> [@shinypkg] and <tt>shinydashboard</tt> [@shinydashboardpkg] packages available in R [@Rcite].

The purpose of this vignette is to outline the main steps involved in using ***ShinyPrior***. The process is described by two main steps, which have been divided into separate menus displayed on the left-hand side of the application:


**1. Define distribution inputs**: The user specifies the type of distribution and form of evidence available to estimate distribution parameters

**2. Customisation**: Provides options to summarise one or more estimated distributions, to produce publication ready figures and tables.

Troubleshooting tips are given in Section 3.

&nbsp;   

## **1. Define distribution inputs**


#### **Distribution family**

Available distributions are defined by two unknown parameters, and were selected based on their common use in health economic evaluations to characterise uncertainty in continuous model inputs. Table 1 provides further details about each distribution family. Assumed parmeterisations align with the R help documentation. Users wishing to use application outputs in other software are advised to check software-specific parameterisations to ensure consistency across platforms. A conversion table from R to STATA and TreeAge is provided under the **Resources** menu.

&nbsp;   

<!--html_preserve--><template id="37b80307-66b5-4c4e-8c05-17e298f533f6"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}
</style><div class="tabwid"><style>.cl-4c632b34{}.cl-4c5120ba{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c5120bb{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c59ba7c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4c59e2b8{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c59e2b9{width:2.5in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c59e2ba{width:3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c59e2bb{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c59e2bc{width:2.5in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c59e2bd{width:3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c43e4ea{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-4c632b34'><caption><span class="cl-4c43e4ea">Table 1: Available distributions and forms of evidence</span></caption><thead><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120ba">Distribution</span></p></td><td class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120ba">Parameterisation</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120ba">Form(s) of evidence supported</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120ba">Estimation method</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td  rowspan="3"class="cl-4c59e2bb"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Normal</span></p></td><td  rowspan="3"class="cl-4c59e2bc"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">N(μ,σ)</span><br><span class="cl-4c5120bb">Mean: -∞ &lt; μ &lt; ∞</span><br><span class="cl-4c5120bb">Standard deviation, σ &gt; 0</span></p></td><td class="cl-4c59e2bd"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2bd"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">toadd</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  rowspan="3"class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Gamma</span></p></td><td  rowspan="3"class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">G(α,β)</span><br><span class="cl-4c5120bb">Shape: α &gt; 0</span><br><span class="cl-4c5120bb">Scale: β &gt; 0</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Numerical optimisation</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">toadd</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  rowspan="3"class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">log-Normal</span></p></td><td  rowspan="3"class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">LN(μ,σ)</span><br><span class="cl-4c5120bb">Mean: μ &gt; 0</span><br><span class="cl-4c5120bb">Standard deviation, σ &gt; 0</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">toadd</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  rowspan="3"class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Weibull</span></p></td><td  rowspan="3"class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Weib(λ,k)</span><br><span class="cl-4c5120bb">Shape: λ &gt; 0</span><br><span class="cl-4c5120bb">Scale: k &gt; 0</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Numerical optimisation</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">toadd</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  rowspan="4"class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Beta</span></p></td><td  rowspan="4"class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Beta(α,β)</span><br><br><span class="cl-4c5120bb">Shape: α &gt; 0</span><br><span class="cl-4c5120bb">Scale: β &gt; 0</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">toadd</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Number of events, sample size</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  rowspan="4"class="cl-4c59e2b8"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Uniform</span></p></td><td  rowspan="4"class="cl-4c59e2b9"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">U(a,b)</span><br><span class="cl-4c5120bb">Minimum: a</span><br><span class="cl-4c5120bb">Maximum: b</span><br><span class="cl-4c5120bb">-∞ &lt; a &lt; b &lt; ∞</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Mean with uncertainty</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Confidence interval</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Median with interquartile range</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Minumum, Maximum</span></p></td><td class="cl-4c59e2ba"><p class="cl-4c59ba7c"><span class="cl-4c5120bb">Closed-form</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="66aaa7a8-cf06-46ca-8e23-c9e3e138898c"></div>
<script>
var dest = document.getElementById("66aaa7a8-cf06-46ca-8e23-c9e3e138898c");
var template = document.getElementById("37b80307-66b5-4c4e-8c05-17e298f533f6");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>
<!--/html_preserve-->
&nbsp;   

#### **Description**

ShinyPrior was designed to summarise multiple distributions within the same session. This functionality allows users to compare different distributions, as well as create figures and tables for use in research outputs. Figure and table options are discussed further in Section 2. To support this functionality, users are required to supply a text label in the _Description_ box for each unique distribution. Supplied descriptions are not subject to a character limit, however we recommend a short label of up to 20 characters. Entered labels are used to index all application outputs, and are displayed in both the Visualisation and Summary table panes. An error message will appear if a user selects _Estimate distribution_ before supplying a description. If a new distribution is specified under an existing description, the previous result matching that description will be overwritten.

#### **Form of evidence**

Distribution parameters are estimated using the form of evidence selected by the user. Forms of evidence currently available reflect commonly used summary statistics for describing unbounded and bounded continuous variables. All distributions include the options "Mean with uncertainty", "Confidence interval" and "Median with interquartile range". Available options will update based on the distribution selected from the _Distribution family_ menu. Further guidance on each option is provided below:

* Mean with uncertainty: Uncertainty is defined on the standard deviation scale and must be greater than 0. Users should therefore ensure that any appropriate transformations are applied to summary statistics before using this option.

* Confidence interval: Lower and upper interval values correspond to the lower and upper limits of the confidence interval. The confidence level must be provided as a percentage between 0 and 100%. The confidence level is used to determine the distribution percentiles corresponding to the lower and upper interval values. For example, a 95% confidence level assumes that the lower and upper interval values represent the $2.5^{th}$ and $97.5^{th}$ percentiles, respectively.

* Median with interquartile range: All three values must entered, and be within the defined range of the distribution. 

* Number of events, sample size: For the Beta distribution only. The number of events must be greater than 0 and less than the defined sample size.

* Minimum and Maximum: For the Uniform distribution only. 

Required inputs are dynamically updated based on the option selected from the _Form of evidence_ drop-down menu. An error message will appear if incompatible values are entered or if one or more inputs are missing. Errors messages include guidance on feasible values, as appropriate. An example error message is shown in Figure 1.

&nbsp;   
Figure 1: Example error message generated after supplying incorrect inputs. In this case, the Mean value is missing
 

![](Figure1.png)

&nbsp;   


#### **Estimate distribution**

Supplied inputs are used to estimate distribution parameters based on closed-form solutions, when available, or by numerical optimisation. Numerical optimisation methods are pre-specified based on the number of parameters without a closed form solution. Numerical optimisation of a single unknown parameter is conducted using one-dimensional root finding, conditional on the closed-form estimate of the other parameter. In the case of two unknown parameters, a quasi-Newton routine is implemented using the general optimisation function <tt>optim()</tt>, imported from the <tt>stats</tt> package. Optimisation aims to minimise the sum of squares between expected and observed values.

Once estimated, results for the current distribution are displayed in the Visualisation and Summary table windows (Figure 2). Previous results can be added via the **Customisation** menu.

&nbsp; 

Figure 2: Example application display for a single distribution. Outputs are generated after specfying all inputs under "Define distribution inputs" and pressing the button "Estimate distribution"
   


![](Figure2.png)

&nbsp;   


## **2. Customisation**

ShinyPrior gives several options for customising the appearance of density plots and summary table information. Results can be customised for a single distribution result, or collectively for multiple results. For all outputs, the distribution(s) of interest are specified in the _Select distribution(s)_ box. Please note that both Visualisation and Summary table windows will update based on names specified in the _Select distribution(s)_ box.

Outputs can be exported at any time into selected file formats for future use. All results are saved by the application for use in figures and tables until they are deleted by the user (see **Remove results from saved output**).

### **Visualisation**

Density plots are generated using the <tt>ggplot2</tt> package. Options to customise plot output include colour palette (_Choose colour scheme_), plot theme (_Select plot theme_), axis/legend labels (_x-axis label_, _y-axis label_, _Legend title_) and legend display (_Display legend_). See Table 2 for a description of plot elements.

&nbsp;   

<!--html_preserve--><template id="2797e0a9-f285-4025-90af-3ed6acc01ef2"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}
</style><div class="tabwid"><style>.cl-4c94515a{}.cl-4c7dcb88{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c7dcb89{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c86ed76{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4c871422{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c871423{width:2in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c871424{width:7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c871425{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c871426{width:2in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c871427{width:7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c743546{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-4c94515a'><caption><span class="cl-4c743546">Table 2: Visualisation options</span></caption><thead><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb88">Plot element</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb88">Format</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb88">Options</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-4c871425"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Colour scheme</span></p></td><td class="cl-4c871426"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Drop-down selection</span></p></td><td class="cl-4c871427"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Greyscale (default), Accent, Dark2, Paired, Set1, Set2, Set3, Colourblind-1, Colourblind-2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Plot theme</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Drop-down selection</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Minmal (default), Light, Black/White, Classic, Gray</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">x-axis label</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Free-text</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">y-axis label</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Free-text</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Display legend</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Radio button</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Yes (default), No</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Legend title</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Free text</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Format</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Drop-down selection</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">.png (default), .tiff, .jpeg</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Resolution</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Drop-down selection</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">300dpi (default), 600dpi</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Height (cm)</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Numeric</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Width (cm)</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Numeric</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4c871422"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Figure name</span></p></td><td class="cl-4c871423"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">Free text</span></p></td><td class="cl-4c871424"><p class="cl-4c86ed76"><span class="cl-4c7dcb89">--</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="f3c60bdf-34fb-4eec-bafa-09f3888f756c"></div>
<script>
var dest = document.getElementById("f3c60bdf-34fb-4eec-bafa-09f3888f756c");
var template = document.getElementById("2797e0a9-f285-4025-90af-3ed6acc01ef2");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>
<!--/html_preserve-->

&nbsp;   

Nine pre-defined colour palettes are available for use (@rcolorbrewerpkg, @rcookbook). All colour palettes include a maximum of eight colours each. The greyscale scheme includes a maximum of five colours. Hexidecimal values by colour palette are given in Table 3. 

&nbsp;   

<!--html_preserve--><template id="d1f97ccc-5899-4949-b523-7149f2575ca8"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}
</style><div class="tabwid"><style>.cl-4ccb42a0{}.cl-4cb94db6{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4cb94db7{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4cc026ae{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4cc026af{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4cc04da0{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cc04da1{width:1in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cc04da2{width:7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cc04da3{width:1.25in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cc04da4{width:1in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cc04da5{width:7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4cb02c0e{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-4ccb42a0'><caption><span class="cl-4cb02c0e">Table 3: Colour scheme properties</span></caption><thead><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db6">Colour scheme</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db6">Max. colours</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db6">Hexidecimal values</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da3"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Greyscale</span></p></td><td class="cl-4cc04da4"><p class="cl-4cc026af"><span class="cl-4cb94db7">5</span></p></td><td class="cl-4cc04da5"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#000000, #737373, #BDBDBD, #D9D9D9, #F0F0F0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Accent</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#7FC97F, #BEAED4, #FDC086, #FFFF99, #386CB0, #F0027F, #BF5B17, #666666</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Dark2</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#1B9E77, #D95F02, #7570B3, #E7298A, #66A61E, #E6AB02, #A6761D, #666666</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Paired</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#A6CEE3, #1F78B4, #B2DF8A, #33A02C, #FB9A99, #E31A1C, #FDBF6F, #FF7F00</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Set1</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#E41A1C, #377EB8, #4DAF4A, #984EA3, #FF7F00, #FFFF33, #A65628, #F781BF</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Set2</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#66C2A5, #FC8D62, #8DA0CB, #E78AC3, #A6D854, #FFD92F, #E5C494, #B3B3B3</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Set3</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#8DD3C7, #FFFFB3, #BEBADA, #FB8072, #80B1D3, #FDB462, #B3DE69, #FCCDE5</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Colourblind-1</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#000000, #E69F00, #56B4E9, #009E73, #F0E442, #0072B2, #D55E00, #CC79A7</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4cc04da0"><p class="cl-4cc026ae"><span class="cl-4cb94db7">Colourblind-2</span></p></td><td class="cl-4cc04da1"><p class="cl-4cc026af"><span class="cl-4cb94db7">8</span></p></td><td class="cl-4cc04da2"><p class="cl-4cc026ae"><span class="cl-4cb94db7">#999999, #E69F00, #56B4E9, #009E73, #F0E442, #0072B2, #D55E00, #CC79A7</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="23430b6c-48e4-43ed-86fc-c712d654be7c"></div>
<script>
var dest = document.getElementById("23430b6c-48e4-43ed-86fc-c712d654be7c");
var template = document.getElementById("d1f97ccc-5899-4949-b523-7149f2575ca8");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>
<!--/html_preserve-->

&nbsp;   

When multiple distributions are displayed, legend colours and label ordering will match the order of results as entered in the _Select distributions(s)_ box.

The following ggplot2 themes are supported: <tt>theme_minimal()</tt> (default), <tt>theme_light()</tt>, <tt>theme_bw()</tt>, <tt>theme_classic()</tt>, and <tt>theme_gray()</tt>

Axis labels and legend title can be updated in the corresponding free-text boxes. Users may also decide to include or exclude the figure legend, by selecting the appropriate option under _Display legend?_. If "Yes" is selected, the legend will appear on the right-hand side of the figure.

Users can further specify the saved figure size (_Height (cm)_, _Width (cm)_), figure resolution (_Resolution_), and file format (_Format_). Plots can be saved at 300 or 600 dots per inch (dpi), as a .png, .tiff or .jpeg file. Custom filenames are allowed and can be specified in the _Figure name_ box.


### **Summary table**

Distribution summary statistics are presented in tabular form, generated using the <tt>flextable</tt> package [@flextablepkg]. Customisation options include the inclusion/exclusion of columns, and row ordering by _Description_ or _Distribution family_.

All table columns will appear in the Summary table window by default. Available columns are: Description, Form of evidence, Distribution, Mean (SD), Mean (95% Interval), and Median (Q1 to Q3). The Description column cannot be removed within the application. Otherwise, any number of columns may be included or excluded from the final table via the checkboxes under the **Summary table** sub-menu. Similar to figures, a custom filename may be entered in the _Table name_ box. In the current version of ShinyPrior, tables can only be exported as Word documents (.docx).

### **Remove results from saved output**

users can permanently delete estimated distributions at any time. Results to be deleted are specified in the _Select distribution(s)_ box, found in the **Remove results from saved output** sub-menu. Once entered, confirm the selection by clicking _Remove selected distribution(s)_.

&nbsp;   

## **Citation information**

If you use ShinyPrior in your work, please cite:

TODO - OSF doi

&nbsp;   

## **Acknowledgements**

ShinyPrior was developed to support other researchers create reproducible outputs for use in publications. We thank colleagues at the [Australian Centre for Health Services Innovation (AusHSI)](https://www.aushsi.org.au/) for their time spent testing and providing valuable feedback on the application. We particularly wish to thank [Hannah Carter](https://www.aushsi.org.au/about-us/team/hannah-carter/) and [Adrian Barnett](https://www.aushsi.org.au/about-us/team/adrian-barnett/) for their contributions that improved earlier versions of the application.

&nbsp;   

## **References**


