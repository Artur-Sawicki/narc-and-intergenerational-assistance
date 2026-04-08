library(lm.beta)
library(broom)

extract_model <- function(model, wave, outcome) {
  
  b <- lm.beta(model)
  s <- summary(model)
  pvals <- coef(s)[, "Pr(>|t|)"]
  
  data.frame(
    wave = wave,
    outcome = outcome,
    term = names(coef(b)),
    beta = round(as.numeric(coef(b)), 2),
    p = round(pvals, 3),
    sig = case_when(pvals < .001 ~ "***",pvals < .01  ~ "**",pvals < .05  ~ "*",TRUE ~ ""),
    r2 = round(s$r.squared, 3),
    N = nobs(model)
  )
}