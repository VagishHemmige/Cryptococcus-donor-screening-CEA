# ==============================================================================
# Organ Donation Cryptococcus Infection Screening Decision Tree
# ==============================================================================
# This script replicates the TreeAge decision tree analysis comparing:
#   1. No CrAg screening
#   2. CrAg screening with conditional prophylaxis
#
# Terminal outcomes (4 branches):
#   - No infection, no prophylaxis: $0
#   - Prophylaxis only: $6,660
#   - Infection: $110,945
#   - Infection + prophylaxis: $117,605
# ==============================================================================


# Load packages ----------------------------------------------------------------
library(heemod)
library(dplyr)
library(tidyverse)

# Parameters -------------------------------------------------------------------
param <- define_parameters(
  # Epidemiology
  p_donor_crypto = 0.001,
  p_no_crypto = 1 - p_donor_crypto,
  
  # Organ use
  p_used = 0.43,
  p_not_used = 0.57,
  
  # CrAg test performance
  sens = 0.90,
  spec = 0.98,
  p_fp = 1-spec,
  p_fn = 1-sens,
  
  # Donation rules
  p_donation_blocked = 0.80,
  p_prophylaxis = 0.10,
  
  # Costs
  cost_screen = 1404,
  cost_crypto = 110945,
  cost_prophylaxis = 6660,
)

# Terminal states -------------------------------------------------------------------

no_event <- define_state(
  cost = 0
)

crypto <- define_state(
  cost = cost_crypto
)

crypto_with_proph <- define_state(
  cost = cost_crypto_plus_proph
)

proph_only <- define_state(
  cost = cost_prophylaxis
)

# Define No Screening strategy ------------------------------------------------------

no_screen <- define_strategy(
  transition = define_transition(
    
    # Donor organs not used
    C_not_used = p_not_used,
    
    # Donor organs used
    C_used = p_used
  ),
  
  C_not_used = define_transition(
    no_event = 1
  ),
  
  C_used = define_transition(
    crypto = p_donor_crypto,
    no_event = p_no_crypto
  )
)

# Define CrAg Screening strategy ----------------------------------------------------

screen <- define_strategy(
  
  cost = cost_screen,
  
  transition = define_transition(
    C_not_used = p_not_used,
    C_used = p_used
  ),
  
  C_not_used = define_transition(
    no_event = 1
  ),
  
  C_used = define_transition(
    
    # True positive
    TP = p_donor_crypto * sens,
    
    # False negative
    FN = p_donor_crypto * p_fn,
    
    # False positive
    FP = p_no_crypto * p_fp,
    
    # True negative
    TN = p_no_crypto * spec
  ),
  
  TP = define_transition(
    blocked = p_donation_blocked,
    prophylaxis = p_prophylaxis,
    crypto = 1 - p_donation_blocked - p_prophylaxis
  ),
  
  FN = define_transition(
    crypto = 1
  ),
  
  FP = define_transition(
    blocked = p_donation_blocked,
    prophylaxis = p_prophylaxis,
    no_event = 1 - p_donation_blocked - p_prophylaxis
  ),
  
  TN = define_transition(
    no_event = 1
  ),
  
  blocked = define_transition(
    no_event = 1
  ),
  
  prophylaxis = define_transition(
    proph_only = 1
  )
)

# Run model--------- ----------------------------------------------------------------

res <- run_model(
  screen = screen,
  no_screen = no_screen,
  parameters = param
)

summary(res)

