#Functions for CEA

#
calculate_cost_QALY_QC<-function(p_usage, 
                                 p_donor_cryptococcus, 
                                 p_transmission, 
                                 p_spont_cryptococcus, 
                                 p_sensitivity,
                                 p_specificity, 
                                 p_cancelled, 
                                 p_prophrate, 
                                 p_prophefficacy,
                                 cost_test,
                                 cost_disease,
                                 cost_fluconazole,
                                 cost_cancellation,
                                 cost_nocryptococcus,
                                 cost_nonacceptance,
                                 q_nocryptococcus,
                                 q_noacceptance,
                                 q_loss_cryptococcus
)
{
  
  #Define parameters
  p_nonusage<-1-p_usage
  p_donor_nocryptococcus<-1-p_donor_cryptococcus
  p_nontransmission<-1-p_transmission
  p_nospont_cryptococcus<-1-p_spont_cryptococcus
  p_falsenegative<-1-p_sensitivity
  p_falsepositive<-1-p_specificity
  p_nocancelled<-1-p_cancelled
  p_noprophrate<-1-p_prophrate
  p_noprophefficacy<-1-p_prophefficacy
  p_breakthrough_donorpos<-(1-p_prophefficacy)*p_transmission
  p_nobreakthrough_donorpos<-1-p_breakthrough_donorpos
  p_breakthrough_donorneg<-(1-p_prophefficacy)*p_spont_cryptococcus
  p_nobreakthrough_donorneg<-1-p_breakthrough_donorneg
  q_cryptococcus<-q_nocryptococcus-q_loss_cryptococcus
  
  #Define tibble of outcomes
  path_table<-tribble(
    ~strategy, ~acceptance, ~donor_dz_status, ~donor_test_result, ~cancellation, ~proph, ~outcome, ~probability, ~cost_total, ~qaly_total,
    "No Screening","Accept","Positive",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_transmission,cost_disease,q_cryptococcus,
    "No Screening","Accept","Positive",NA,NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_nontransmission,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_spont_cryptococcus,cost_disease,q_cryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_nospont_cryptococcus,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance,q_noacceptance,
    
    
    "Screening","Accept","Positive","CrAg+","Cancelled",NA,"Cancelled",p_usage*p_donor_cryptococcus*p_sensitivity*p_cancelled,cost_cancellation+cost_test,q_noacceptance,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","Fluconazole","Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_prophrate*p_breakthrough_donorpos,cost_test+cost_disease+cost_fluconazole,q_cryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","Fluconazole","No cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_prophrate*p_nobreakthrough_donorpos,cost_test+cost_nocryptococcus+cost_fluconazole,q_nocryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","None","Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_noprophrate*p_transmission,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","None","No cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_noprophrate*p_nontransmission,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Accept","Positive","CrAg-",NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_falsenegative*p_transmission,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Positive","CrAg-",NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_falsenegative*p_nontransmission,cost_test+cost_nocryptococcus,q_nocryptococcus,
    
    "Screening","Accept","Negative","CrAg+","Cancelled",NA,"Cancelled",p_usage*p_donor_nocryptococcus*p_falsepositive*p_cancelled,cost_cancellation+cost_test,q_noacceptance,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","Fluconazole","Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_prophrate*p_breakthrough_donorneg,cost_test+cost_disease+cost_fluconazole,q_cryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","Fluconazole","No cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_prophrate*p_nobreakthrough_donorneg,cost_test+cost_nocryptococcus+cost_fluconazole,q_nocryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","None","Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_noprophrate*p_spont_cryptococcus,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","None","No cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_noprophrate*p_nospont_cryptococcus,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Accept","Negative","CrAg-",NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_specificity*p_spont_cryptococcus,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Negative","CrAg-",NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_specificity*p_nospont_cryptococcus,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance+cost_test,q_noacceptance,
    
  )%>%
    mutate(cost_expected=probability*cost_total,
           qaly_expected=probability*qaly_total)
  
  #Summary object ofr
  summary_tibble<-path_table%>%
    group_by(strategy)%>%
    summarize(total_expected_cost=sum(cost_expected), total_expected_qaly=sum(qaly_expected))%>%
    mutate(
      strategy = recode(
        strategy,
        "No Screening" = "ns",
        "Screening"    = "s"
      )
    ) %>%
    tidyr::pivot_wider(
      names_from  = strategy,
      values_from = c(
        total_expected_cost,
        total_expected_qaly
      ),
      names_sep = "_"
    )
  
  #Final return
  return(summary_tibble)
}





