#This code creates univariate sensitivity plots ("tornado plots") and performs probabilistic sensitivity analysis

#First, we source the helper functions for the analysis
source("R/functions.R")

#Next, we parameterize the sensitivity analyses.  

#We starts with probabilities.
#From the mean and alpha, we derive beta and then calculate confidence intervals
PSA_parameters<-list()
PSA_parameters$probabilities<-tribble(
  ~parameter, ~mean, ~shape1,
  "p_usage", expected_value$p_usage, 10,
  "p_donor_cryptococcus",expected_value$p_donor_cryptococcus, 10, 
  "p_transmission",expected_value$p_transmission,  10,
  "p_spont_cryptococcus",expected_value$p_spont_cryptococcus, 10, 
  "p_sensitivity",expected_value$p_sensitivity, 10,
  "p_specificity",expected_value$p_specificity, 10,
  "p_cancelled",expected_value$p_cancelled, 10,
  "p_prophrate",expected_value$p_prophrate, 10,
  "p_prophefficacy",expected_value$p_prophefficacy, 10)%>%
  mutate(shape2=shape1 * (1 - mean) / mean)%>%
  rowwise() %>%
  mutate(mu=mean(rbeta(1e6, shape1, shape2)), lb_95=qbeta(0.025, shape1, shape2), ub_95=qbeta(0.975, shape1, shape2))
#For costs and QALYS, we start from the mean and shape parameter, and we derive scale and then calculate confidence intervals
PSA_parameters$costs<-tribble(
  ~parameter, ~mean, ~shape,
  "cost_test",expected_value$cost_test,4,
  "cost_disease",expected_value$cost_disease,4,
  "cost_fluconazole",expected_value$cost_fluconazole,4,
  "cost_cancellation",expected_value$cost_cancellation,NA,
  "cost_nocryptococcus",expected_value$cost_nocryptococcus,NA,
  "cost_nonacceptance",expected_value$cost_nonacceptance,NA
  )%>%
  mutate(scale=mean/shape)%>%
  rowwise() %>%
  mutate(mu=mean(rgamma(1e6, shape=shape, scale=scale)), 
         lb_95=qgamma(0.025, shape=shape, scale=scale), 
         ub_95=qgamma(0.975, shape=shape, scale=scale))%>%
  mutate(lb_95=ifelse(is.na(lb_95), mean, lb_95),
         ub_95=ifelse(is.na(ub_95), mean, ub_95)
         )

PSA_parameters$qalys<-tribble(
  ~parameter, ~mean, ~shape,
"q_nocryptococcus",expected_value$q_nocryptococcus,4,
"q_noacceptance",expected_value$q_noacceptance,NA,
"q_loss_cryptococcus",expected_value$q_loss_cryptococcus,4)%>%
  mutate(scale=mean/shape)%>%
  rowwise() %>%
  mutate(mu=mean(rgamma(1e6, shape=shape, scale=scale)), 
         lb_95=qgamma(0.025, shape=shape, scale=scale), 
         ub_95=qgamma(0.975, shape=shape, scale=scale))%>%
  mutate(lb_95=ifelse(is.na(lb_95), mean, lb_95),
         ub_95=ifelse(is.na(ub_95), mean, ub_95)
  )

#Loop to save parameter distributions for probabilities
for (i in 1:nrow(PSA_parameters$probabilities))
{
  temp_plot<-ggplot(data.frame(x = c(0, 1)), aes(x)) +
    stat_function(fun = dbeta, args = list(shape1 = PSA_parameters$probabilities[[i,3]], shape2 = PSA_parameters$probabilities[[i,4]]))+
    labs(
      title = glue("Prior distribution for **{PSA_parameters$probabilities[[i,1]]}**"),
      x = "Probability",
      y = "Density",
    )+
    theme_classic()+
    theme(plot.title = element_markdown())
  ggsave(
    glue("figures/PSA_prior_{PSA_parameters$probabilities[[i,1]]}.png"),
    plot = temp_plot,
  )
}

#Loop to save parameter distributions for costs
for (i in 1:nrow(PSA_parameters$costs))
{
  temp_plot<-ggplot(data.frame(x = c(PSA_parameters$costs[[i,6]]-1, 1+PSA_parameters$costs[[i,7]])), aes(x)) +
    stat_function(fun = dgamma, args = list(shape = PSA_parameters$costs[[i,3]], scale = PSA_parameters$costs[[i,4]]))+
    labs(
      title = glue("Prior distribution for **{PSA_parameters$costs[[i,1]]}**"),
      x = "Cost",
      y = "Probability density",
    )+
    theme_classic()+
    theme(plot.title = element_markdown())
  ggsave(
    glue("figures/PSA_prior_{PSA_parameters$costs[[i,1]]}.png"),
    plot = temp_plot,
  )
}

#Loop to save parameter distributions for QALYs
for (i in 1:nrow(PSA_parameters$qalys))
{
  temp_plot<-ggplot(data.frame(x = c(PSA_parameters$qalys[[i,6]]-1, 1+PSA_parameters$qalys[[i,7]])), aes(x)) +
    stat_function(fun = dgamma, args = list(shape = PSA_parameters$qalys[[i,3]], scale = PSA_parameters$qalys[[i,4]]))+
    labs(
      title = glue("Prior distribution for **{PSA_parameters$qalys[[i,1]]}**"),
      x = "Cost",
      y = "Probability density",
    )+
    theme_classic()+
    theme(plot.title = element_markdown())
  ggsave(
    glue("figures/PSA_prior_{PSA_parameters$qalys[[i,1]]}.png"),
    plot = temp_plot,
  )
}


#Tables
PSA_parameters$probabilities%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape1 = "α",
    shape2 = "β",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )

PSA_parameters$costs%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape = "k",
    scale = "θ",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )

PSA_parameters$qalys%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape = "k",
    scale = "θ",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )


#Now, we simulate 10,000 draws from the above.  First, we create the dataset with the simulated values

#Define number of simulations
nsim<-10000

#Extract values from above
PSA_simulation<-list()
PSA_simulation$probabilities <- PSA_parameters$probabilities %>%
  transmute(
    draws = purrr::map2(shape1, shape2, ~ rbeta(nsim, .x, .y))
  ) %>%
  pull(draws) %>%
  rlang::set_names(PSA_parameters$probabilities$parameter) %>%
  as_tibble()
PSA_simulation$costs<-PSA_parameters$costs %>%
  transmute(
    draws = purrr::map2(shape, scale, ~ rgamma(nsim, shape=.x, scale=.y))
  ) %>%
  pull(draws) %>%
  rlang::set_names(PSA_parameters$costs$parameter) %>%
  as_tibble()%>%
  mutate(cost_cancellation=0,
         cost_nocryptococcus=-10,
         cost_nonacceptance=0)
PSA_simulation$qalys<-PSA_parameters$qalys %>%
  transmute(
    draws = purrr::map2(shape, scale, ~ rgamma(nsim, shape=.x, scale=.y))
  ) %>%
  pull(draws) %>%
  rlang::set_names(PSA_parameters$qalys$parameter) %>%
  as_tibble()%>%
  mutate(q_noacceptance=0)

#Combine into a single tibble
PSA_simulation<-bind_cols(PSA_simulation)%>%
  
  #Add derived quantities
  mutate(
  p_nonusage=1-p_usage,
  p_donor_nocryptococcus=1-p_donor_cryptococcus,
  p_nontransmission=1-p_transmission,
  p_nospont_cryptococcus=1-p_spont_cryptococcus,
  p_falsenegative=1-p_sensitivity,
  p_falsepositive=1-p_specificity,
  p_nocancelled=1-p_cancelled,
  p_noprophrate=1-p_prophrate,
  p_noprophefficacy=1-p_prophefficacy,
  p_breakthrough_donorpos=(1-p_prophefficacy)*p_transmission,
  p_nobreakthrough_donorpos=1-p_breakthrough_donorpos,
  p_breakthrough_donorneg=(1-p_prophefficacy)*p_spont_cryptococcus,
  p_nobreakthrough_donorneg=1-p_breakthrough_donorneg,
  q_cryptococcus=q_nocryptococcus-q_loss_cryptococcus)%>%
  
  #Calculate costs and QALYs
  rowwise() %>%
  mutate(results=list(calculate_cost_QALY_QC(p_usage=p_usage, 
                                        p_donor_cryptococcus=p_donor_cryptococcus, 
                                        p_transmission=p_transmission,
                                        p_spont_cryptococcus=p_spont_cryptococcus,
                                        p_sensitivity=p_sensitivity,
                                        p_specificity=p_specificity,
                                        p_cancelled=p_cancelled,
                                        p_prophrate=p_prophrate,
                                        p_prophefficacy=p_prophefficacy,
                                        cost_test=cost_test,
                                        cost_disease=cost_disease,
                                        cost_fluconazole=cost_fluconazole,
                                        cost_cancellation=cost_cancellation,
                                        cost_nocryptococcus=cost_nocryptococcus,
                                        cost_nonacceptance=cost_nonacceptance,
                                        q_nocryptococcus=q_nocryptococcus,
                                        q_noacceptance=q_noacceptance,
                                        q_loss_cryptococcus=q_loss_cryptococcus
  )))%>%
  ungroup()

wtp <- 100000  # willingness-to-pay

PSA_simulation_unnested<-PSA_simulation%>%
  unnest(results)%>%
  mutate(cost_change=total_expected_cost_s-total_expected_cost_ns,
         qaly_change=total_expected_qaly_s-total_expected_qaly_ns)%>%
  mutate(icer=cost_change/qaly_change)%>%
  mutate(
    nmb = wtp * qaly_change - cost_change
  )


PSA_simulation_unnested%>%
  ggplot()+
  geom_point(mapping = aes(x=qaly_change, y=cost_change), alpha=0.1)+
  theme_classic()