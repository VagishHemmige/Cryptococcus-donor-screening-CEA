#This code creates univariate sensitivity plots ("tornado plots") and performs probabilistic sensitivity analysis

#First, we source the helper functions for the analysis
source("R/functions.R")

#Next, we parameterize the sensitivity analyses.  

#We start with probabilities.  we derive the mean and a shape parameter from the setup file 
#From the mean and alpha, we derive beta and then calculate confidence intervals
PSA_parameters<-list()
PSA_parameters$probabilities<-tribble(
  ~parameter, ~mean, ~shape1,
  "p_usage", expected_value$p_usage, shape_parameter$p_usage,
  "p_donor_cryptococcus",expected_value$p_donor_cryptococcus, shape_parameter$p_donor_cryptococcus, 
  "p_transmission",expected_value$p_transmission,  shape_parameter$p_transmission,
  "p_spont_cryptococcus",expected_value$p_spont_cryptococcus, shape_parameter$p_spont_cryptococcus, 
  "p_sensitivity",expected_value$p_sensitivity, shape_parameter$p_sensitivity,
  "p_specificity",expected_value$p_specificity, shape_parameter$p_specificity,
  "p_cancelled",expected_value$p_cancelled, shape_parameter$p_cancelled,
  "p_prophrate",expected_value$p_prophrate, shape_parameter$p_prophrate,
  "p_prophefficacy",expected_value$p_prophefficacy, shape_parameter$p_prophefficacy)%>%
  mutate(shape2=shape1 * (1 - mean) / mean)%>%
  rowwise() %>%
  mutate(mu=mean(rbeta(1e6, shape1, shape2)), lb_95=qbeta(0.025, shape1, shape2), ub_95=qbeta(0.975, shape1, shape2))
#For costs and QALYS, we start from the mean and shape parameter, and we derive scale and then calculate confidence intervals
PSA_parameters$costs<-tribble(
  ~parameter, ~mean, ~shape,
  "cost_test",expected_value$cost_test,shape_parameter$cost_test,
  "cost_disease",expected_value$cost_disease,shape_parameter$cost_disease,
  "cost_fluconazole",expected_value$cost_fluconazole,shape_parameter$cost_fluconazole,
  "cost_cancellation",expected_value$cost_cancellation,shape_parameter$cost_cancellation,
  "cost_nocryptococcus",expected_value$cost_nocryptococcus,shape_parameter$cost_nocryptococcus,
  "cost_nonacceptance",expected_value$cost_nonacceptance,shape_parameter$cost_nonacceptance
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
"q_nocryptococcus",expected_value$q_nocryptococcus,shape_parameter$q_nocryptococcus,
"q_noacceptance",expected_value$q_noacceptance,shape_parameter$q_noacceptance,
"q_loss_cryptococcus",expected_value$q_loss_cryptococcus,shape_parameter$q_loss_cryptococcus)%>%
  mutate(scale=mean/shape)%>%
  rowwise() %>%
  mutate(mu=mean(rgamma(1e6, shape=shape, scale=scale)), 
         lb_95=qgamma(0.025, shape=shape, scale=scale), 
         ub_95=qgamma(0.975, shape=shape, scale=scale))%>%
  mutate(lb_95=ifelse(is.na(lb_95), mean, lb_95),
         ub_95=ifelse(is.na(ub_95), mean, ub_95)
  )

#Loop to save plots parameter distributions for probabilities
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

#Create and save summary tables of the parameters
PSA_tables<-list()
PSA_tables$probabilities<-PSA_parameters$probabilities%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape1 = "α",
    shape2 = "β",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )
PSA_tables$probabilities
PSA_tables$probabilities%>%
  gtsave("figures/PSA_tables_probabilities.png")
PSA_tables$probabilities%>%
  gtsave("tables/PSA_tables_probabilities.docx")

PSA_tables$costs<-PSA_parameters$costs%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape = "k",
    scale = "θ",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )
PSA_tables$costs
PSA_tables$costs%>%
  gtsave("figures/PSA_tables_costs.png")
PSA_tables$costs%>%
  gtsave("tables/PSA_tables_costs.docx")

PSA_tables$qalys<-PSA_parameters$qalys%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    mean = "Mean",
    shape = "k",
    scale = "θ",
    lb_95 = "Lower bound, 95% CI",
    ub_95 = "Upper bound, 95% CI"
  )
PSA_tables$qalys
PSA_tables$qalys%>%
  gtsave("figures/PSA_tables_qalys.png")
PSA_tables$qalys%>%
  gtsave("tables/PSA_tables_qalys.docx")

#One-way sensivity analysis (tornado diagrams)
tornado_parameters<-list()

#Add the results of low, mean, and high values of the CI to the dataframes
tornado_parameters$probabilities<-PSA_parameters$probabilities%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(lb_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_low=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_low=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_low=wtp*(qaly_difference_low)-cost_difference_low
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(mean), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_mean=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_mean=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_mean=wtp*(qaly_difference_mean)-cost_difference_mean
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(ub_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_high=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_high=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_high=wtp*(qaly_difference_high)-cost_difference_high
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  mutate(range_cost=abs(cost_difference_high-cost_difference_low),
         range_qaly=abs(qaly_difference_high-qaly_difference_low))

#Add the results of low, mean, and high values of the CI to the dataframes for cost
tornado_parameters$costs<-PSA_parameters$costs%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(lb_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_low=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_low=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_low=wtp*(qaly_difference_low)-cost_difference_low
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(mean), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_mean=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_mean=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_low=wtp*(qaly_difference_low)-cost_difference_low
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(ub_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_high=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_high=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_high=wtp*(qaly_difference_high)-cost_difference_high
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  mutate(range_cost=abs(cost_difference_high-cost_difference_low),
         range_qaly=abs(qaly_difference_high-qaly_difference_low))

#Add the results of low, mean, and high values of the CI to the dataframes for qaly
tornado_parameters$qalys<-PSA_parameters$qalys%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(lb_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_low=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_low=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_low=wtp*(qaly_difference_low)-cost_difference_low
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(mean), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_mean=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_mean=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_mean=wtp*(qaly_difference_mean)-cost_difference_mean
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  rowwise() %>%
  mutate(
    result = do.call(
      calculate_cost_QALY_QC,
      setNames(list(ub_95), parameter)
    )
  ) %>%
  unnest(cols = c(result))%>%
  ungroup()%>%
  mutate(
    cost_difference_high=total_expected_cost_s-total_expected_cost_ns,
    qaly_difference_high=total_expected_qaly_s-total_expected_qaly_ns,
    nmb_difference_high=wtp*(qaly_difference_high)-cost_difference_high
  )%>%
  select(-total_expected_cost_ns, -total_expected_cost_s, -total_expected_qaly_ns,-total_expected_qaly_s)%>%
  mutate(range_cost=abs(cost_difference_high-cost_difference_low),
         range_qaly=abs(qaly_difference_high-qaly_difference_low),
         range_nmb=abs(nmb_difference_high-nmb_difference_low))

tornado_parameters_final<-bind_rows(tornado_parameters)%>%
  select(parameter, contains("difference"), contains("range"))

#Tornado plot of costs for probabilities
tornado_cost_df<-tornado_parameters_final%>%
  arrange(range_cost)%>%
  mutate(rownum=row_number())
tornado_cost_df%>%
  ggplot()+
  geom_rect(mapping=aes(xmin=cost_difference_low,
                        xmax=cost_difference_mean,
                        ymin=rownum-0.4,
                        ymax=rownum+0.4,
                        fill="Lower bound of CI"))+
  geom_rect(mapping=aes(xmin=cost_difference_mean,
                        xmax=cost_difference_high,
                        ymin=rownum-0.4,
                        ymax=rownum+0.4,
                        fill="Upper bound of CI"))+
  geom_vline(xintercept = first(tornado_cost_df$cost_difference_mean))+
  scale_y_continuous(
    breaks = tornado_cost_df$rownum,
    labels = tornado_cost_df$parameter
  )+
  labs(
    x="Cost difference",
    y="Parameter",
    title="Tornado plot of cost",
  )+scale_fill_manual(
    name = "Parameter value",
    values = c(
      "Lower bound of CI" = "steelblue",
      "Upper bound of CI" = "firebrick"
    )
  )+ 
  theme(legend.position = "bottom")
ggsave("figures/tornado_cost.svg")

#Tornado plot of QALYs
tornado_qaly_df<-tornado_parameters_final%>%
  arrange(range_qaly)%>%
  mutate(rownum=row_number())
tornado_qaly_df%>%
  ggplot()+
  geom_rect(mapping=aes(xmin=qaly_difference_low,
                        xmax=qaly_difference_mean,
                        ymin=rownum-0.4,
                        ymax=rownum+0.4,
                        fill="Lower bound of CI"))+
  geom_rect(mapping=aes(xmin=qaly_difference_mean,
                        xmax=qaly_difference_high,
                        ymin=rownum-0.4,
                        ymax=rownum+0.4,
                        fill="Upper bound of CI"))+
  geom_vline(xintercept = first(tornado_qaly_df$qaly_difference_mean))+
  scale_y_continuous(
    breaks = tornado_qaly_df$rownum,
    labels = tornado_qaly_df$parameter
  )+
  labs(
    x="QALY difference",
    y="Parameter",
    title="Tornado plot of QALYs",
  )+scale_fill_manual(
    name = "Parameter value",
    values = c(
      "Lower bound of CI" = "steelblue",
      "Upper bound of CI" = "firebrick"
    )
  )+ 
  theme(legend.position = "bottom")
ggsave("figures/tornado_qaly.svg")

#Probabilistic sensitivity analysis
#Now, we simulate 10,000 draws from the above.  First, we create the dataset with the simulated values

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

PSA_simulation_unnested<-PSA_simulation%>%
  unnest(results)%>%
  mutate(cost_change=total_expected_cost_s-total_expected_cost_ns,
         qaly_change=total_expected_qaly_s-total_expected_qaly_ns)%>%
  mutate(icer=cost_change/qaly_change)%>%
  mutate(
    nmb = wtp * qaly_change - cost_change
  )

#Calculate the 95% ellipse
PSA_min_df <- PSA_simulation_unnested %>%
  select(qaly_change, cost_change)

# Create mu and sigma vectors for ellipse
ellipse_mu <- colMeans(PSA_min_df)
ellipse_Sigma <- cov(PSA_min_df)
# Generate ellipse points
ellipse_df <- as.data.frame(
  ellipse(
    ellipse_Sigma,
    centre = ellipse_mu,
    level = 0.95,
    npoints = 200
  )
)
colnames(ellipse_df) <- c("qaly_change", "cost_change")


#Create PSA plot
PSA_plot<-PSA_simulation_unnested%>%
  ggplot()+
  geom_point(mapping = aes(x=qaly_change, y=cost_change), alpha=0.1)+
  geom_path(
    data = ellipse_df,
    color = "red",
    mapping = aes(x=qaly_change, y=cost_change),
    linewidth = 1
  )+
  geom_hline(yintercept = 0, linewidth = 0.4, color = "black")+
  geom_vline(xintercept = 0, linewidth = 0.4, color = "black")+
  coord_cartesian(xlim = c(-1.5, 1.5),
                  ylim = c(-300, 300))+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    title = "Probabilistic sensitivity analysis",
    x="QALY change",
    y="Cost change"
  )
ggsave("figures/PSA_plot.svg")
PSA_plot