## Escouto (2000) experiment. Finding optimal designs and evaluating the one that was used in the experiment.

## Three three-level factors, n=26 runs, p=10 -- full quadratic polynomial model
## https://www.jstatsoft.org/article/view/v099i01


## Search for DP- and LP-optimal designs
require("devtools")

devtools::install_github("vkstats/MOODE")
library(MOODE)

GF_exp <- mood(K = 3, Levels = 3, Nruns = 26, 
               criterion.choice = "MSE.P",
               kappa = list(kappa.DP = 1.0, kappa.LoF = 0.0, kappa.mse = 0.0),
               control = list(Nstarts = 50, tau2 = 1.0, Biter = 100),
               model_terms = list(primary.model = "second_order", 
                                  potential.model = "third_order_terms")
               )


GF_DP <- Search(GF_exp, "ptex") 

criteria.values.mse(GF_DP, mood.obj = GF_exp)

################### Compound optimality ##################

GF_exp <- mood(K = 3, Levels = 3, Nruns = 26, 
               criterion.choice = "MSE.P",
               kappa = list(kappa.DP = 0.5, kappa.LoF = 0.25, kappa.mse = 0.25),
               control = list(Nstarts = 50, tau2 = 1.0, Biter = 100),
               model_terms = list(primary.model = "second_order", 
                                  potential.model = "third_order_terms")
)

set.seed(10082024)

GF_comp <- Search(GF_exp,
                algorithm = "ptex", 
                update.info = TRUE)

GF_comp$time

GF_comp$criteria.values
which.min(GF_comp$path)

## Just some code to calculate efficiency values

if (GF_exp$tau2 == 0.2){    # optimum values, tau2 = 0.2
  optim.values <- c(0.245547275,	0.352200558,	0.119203882,
                    0.759993512,	1.401613566,	0.161096939)
} 
if (GF_exp$tau2 == 1.0){    # optimum values, tau2 = 1.0
  optim.values <- c(0.245547275, 0.7762591, 0.1414382,
                    0.759993512, 3.06828, 0.24579)
} 

 
eff_values <- optim.values/unlist(criteria.values.mse(GF_comp, GF_exp)
                                 [c("DP","LoFDP", "mseD", "LP","LoFLP", "mseL")])


#######################################################
##### Larger example: 5 factors, 3 levels each, 40 runs
#######################################################

GF_exp <- mood(K = 5, Levels = 3, Nruns = 40, 
               criterion.choice = "MSE.P", 
               kappa = list(kappa.DP = 1./2,   
                            kappa.LoF = 1./4, 
                            kappa.mse = 1./4),
               control = list(Nstarts = 50, 
                              tau2 = 0.1, 
                              Biter = 100),
               model_terms = list(primary.model = "second_order", 
                                  potential.model = 
                                    "third_order_terms")
)



GF_comp <- Search(GF_exp,
                  algorithm = "ptex",
                  update.info = TRUE)

GF_comp$time

which.min(GF_comp$path)
