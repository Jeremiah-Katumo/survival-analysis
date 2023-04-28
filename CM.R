# Compute Cox model
cxmod <- coxph(Surv(time, status) ~ performance, data = dat)

# Show model coefficient
coef(cxmod)



# Cox model
cxmod <- coxph(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75)))
rownames(newdat) <- letters[1:6]

# Compute survival curves
cxsf <- survfit(cxmod, data = GBSG2, newdata = newdat, conf.type = "none")

# Look at first 6 rows of cxsf$surv and time points
head(cxsf$surv)
head(cxsf$time)




# Compute data.frame needed for plotting
surv_cxmod0 <- surv_summary(cxsf)

# Get a character vector of patient letters (patient IDs)
pid <- as.character(surv_cxmod0$strata)

# Multiple of the rows in newdat so that it fits with surv_cxmod0
m_newdat <- newdat[pid, ]

# Add patient info to data.frame
surv_cxmod <- cbind(surv_cxmod0, m_newdat)

# Plot
ggsurvplot_df(surv_cxmod, linetype = "horTh", color = "tsize",
              legend.title = NULL, censor = FALSE)




# Compute Cox model and survival curves
cxmod <- coxph(Surv(time, status) ~ performance, data = lung)
new_lung <- data.frame(performance = c(60, 70, 80, 90))
cxsf <- survfit(cxmod, data = lung, newdata = new_lung, conf.type = "none")

# Use the summary of cxsf to take a vector of patient IDs
surv_cxmod0 <- surv_summary(cxsf)
pid <- as.character(surv_cxmod0$strata)

# Duplicate rows in newdat to fit with surv_cxmod0 and add them in
m_newdat <- new_lung[pid, , drop = FALSE]
surv_cxmod <- cbind(surv_cxmod0, m_newdat)

# Plot
ggsurvplot_df(surv_cxmod, color = "performance", 
              legend.title = NULL, censor = FALSE)
