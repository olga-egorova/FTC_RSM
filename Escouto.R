## Escouto (2000) experiment. 

## Three three-level factors, n=26 runs, p=10 -- full quadratic polynomial model
## https://www.jstatsoft.org/article/view/v099i01



install.packages("gt")
install.packages("rintrojs")

require(rintrojs)
require(gt)

library(skpr)
setwd("~/Conferences/FTC2024/R")

set.seed(18102037)
candidate.set <- expand.grid(X1 = c(-1, 0, 1), 
                             X2 = c(-1, 0, 1),
                             X3 = c(-1, 0, 1))

# Point − exchange optimal design search
D_design<- gen_design(candidateset = candidate.set,
                      model = ~ X1 + X2 + X3 + I(X1^2) + I(X2^2) +  
                        I(X3^2) + X1:X2 + X1:X3 + X2:X3, 
                      optimality = "D", 
                      repeats = 200,
                      trials = 26)

I_design <- gen_design(candidateset = candidate.set,
                       model = ~ X1 + X2 + X3 + I(X1^2) + I(X2^2) +  
                                 I(X3^2) + X1:X2 + X1:X3 + X2:X3, 
                       optimality = "I", 
                       repeats = 200,
                       trials = 26)
I_design
# Evaluating Design:
eval_design(design = E_design, 
            model = ~ X1 + X2 + X3 + I(X1^2) + I(X2^2) + I(X3^2) + X1:X2 + X1:X3 +X2:X3, 
            alpha = 0.05,   
            detailedoutput = TRUE)



skprGUI()


### Plotting the designs

library(rgl)
library(dplyr)
library(car)

D_plot <- data.frame(D_design %>% group_by_all() %>% count)
X1 <- D_plot$X1; X2 <- D_plot$X2; X3 <- D_plot$X3

D_plot <- data.frame(I_design %>% group_by_all() %>% count)
X1 <- D_plot$X1; X2 <- D_plot$X2; X3 <- D_plot$X3

scatter3d(x = X1, y = X2, z = X3,
          radius = D_plot$n,
          point.col = "forestgreen",
          surface=FALSE, axis.ticks = TRUE, 
          id=list(method = "mahal", n = length(D_plot$n), labels = D_plot$n),
          axis.col = c("black", "black", "black"))



