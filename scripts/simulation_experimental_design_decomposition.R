# install.packages("simstudy")
library(simstudy)

# c(6, 4, 6, 6)

def <- defData(varname = "treat", formula = "0.25;0.25;0.25;0.25", dist = "categorical")
def <- defData(def, varname = "growth", dist = "normal", 
               formula = "5 * treat", variance = 50)



dat <- genData(30, def)

dat$treat <- factor(dat$treat, labels = c("C", "N", "P", "NP"))

dat$poolN <- ifelse(grepl(x = dat$treat, pattern = "N"), "+N", "-N")
dat$poolP <- ifelse(grepl(x = dat$treat, pattern = "P"), "+P", "-P")

dat$growth[dat$treat == "NP"] <- mean(dat$growth[dat$treat == "C"]) * 2 + rnorm(n = sum(dat$treat == "NP"), sd = 3)


par(mfrow = c(1, 2))

boxplot(growth ~ treat, data = dat)

summary(lm(growth ~ treat, data = dat))
# table(dat$treat)

summary(lm(growth ~ poolN * poolP, data = dat))

summary(lm(growth ~ poolN + poolP, data = dat))

boxplot(growth ~ poolN * poolP, data = dat)



aggregate(growth ~ treat, dat, mean)
aggregate(growth ~ poolN, dat, mean)
aggregate(growth ~ poolP, dat, mean)
aggregate(growth ~ poolN + poolP, dat, mean)






sim_fun <- function(n = 30, effect = 5, var = 50){
  
  def <- defData(varname = "treat", formula = "0.25;0.25;0.25;0.25", dist = "categorical")
  def <- defData(def, varname = "growth", dist = "normal", 
                 formula = paste0(effect ," * treat"), variance = var)

  
  dat <- genData(n, def)
  
  dat$treat <- factor(dat$treat, labels = c("C", "N", "P", "NP"))
  
  dat$poolN <- ifelse(grepl(x = dat$treat, pattern = "N"), "+N", "-N")
  dat$poolP <- ifelse(grepl(x = dat$treat, pattern = "P"), "+P", "-P")
  
  # dat$growth[dat$treat == "NP"] <- mean(dat$growth[dat$treat == "C"]) * 2 + rnorm(n = sum(dat$treat == "NP"), sd = 3)
  
  mod <- summary(lm(growth ~ treat, data = dat))
  # table(dat$treat)
  modc <- summary(lm(growth ~ poolN + poolP, data = dat))
  modn <- summary(lm(growth ~ poolP, data = dat))
  modp <- summary(lm(growth ~ poolN, data = dat))
  
  # mod_int <- summary(lm(growth ~ poolN * poolP, data = dat))
  
df <- data.frame(pN = mod$coefficients[2, 4], pP = mod$coefficients[3, 4], pNc = modc$coefficients[2, 4], pPc = modc$coefficients[3, 4], pNn = modn$coefficients[2, 4], pPp = modp$coefficients[2, 4])
  
return(df)
}



rep_sim <- replicate(1000, try(sim_fun(n = 30, effect = 5, var = 50), silent = TRUE), simplify = FALSE)

rep_sim <- rep_sim[sapply(rep_sim, class) == "data.frame"]

rep_sim <- do.call(rbind, rep_sim)

# compare N pvalues
sum(rep_sim$pN < 0.05) / nrow(rep_sim)
sum(rep_sim$pNc < 0.05) / nrow(rep_sim)
sum(rep_sim$pNp < 0.05) / nrow(rep_sim)

# compare P pvalues
sum(rep_sim$pP < 0.05) / nrow(rep_sim)
sum(rep_sim$pPc < 0.05) / nrow(rep_sim)
sum(rep_sim$pPp < 0.05) / nrow(rep_sim)
