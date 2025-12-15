# ==============================================================================
# Organ Donation Screening Decision Tree with PSA
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ==============================================================================
# 1. DEFINE PARAMETER DISTRIBUTIONS
# ==============================================================================

# Number of PSA iterations
n_sims <- 10000

set.seed(123)  # For reproducibility

# Generate parameter distributions
psa_params <- data.frame(
  
  # Costs (gamma distributions - can't be negative)
  cost_infection = rgamma(n_sims, shape = 100, scale = 1109.45),  # Mean = 110,945
  cost_screening = rgamma(n_sims, shape = 100, scale = 14.04),    # Mean = 1,404
  cost_prophylaxis = rgamma(n_sims, shape = 50, scale = 133.2),   # Mean = 6,660
  
  # Probabilities (beta distributions - bounded 0-1)
  p_donor_infection = rbeta(n_sims, shape1 = 1, shape2 = 999),    # Mean = 0.001
  p_organs_not_used = rbeta(n_sims, shape1 = 43, shape2 = 57),    # Mean = 0.43
  
  # Test characteristics
  sensitivity = rbeta(n_sims, shape1 = 90, shape2 = 10),          # Mean = 0.90
  specificity = rbeta(n_sims, shape1 = 98, shape2 = 2),           # Mean = 0.98
  
  # Donation decisions after test
  p_donation_blocked_pos = rbeta(n_sims, shape1 = 80, shape2 = 20),  # Mean = 0.80
  p_donation_blocked_neg = rbeta(n_sims, shape1 = 10, shape2 = 90),  # Mean = 0.10
  
  # Prophylaxis effectiveness (if applicable)
  p_infection_with_proph = rbeta(n_sims, shape1 = 2, shape2 = 98)    # Mean = 0.02
)

# ==============================================================================
# 2. DECISION TREE FUNCTION
# ==============================================================================

calculate_tree <- function(params) {
  
  # Extract parameters
  cost_inf <- params$cost_infection
  cost_scr <- params$cost_screening
  cost_pro <- params$cost_prophylaxis
  
  p_inf <- params$p_donor_infection
  p_not_used <- params$p_organs_not_used
  
  sens <- params$sensitivity
  spec <- params$specificity
  
  p_block_pos <- params$p_donation_blocked_pos
  p_block_neg <- params$p_donation_blocked_neg
  
  p_inf_proph <- params$p_infection_with_proph
  
  # ============================================================================
  # STRATEGY 1: NO SCREENING
  # ============================================================================
  
  # Branch 1: Organs not used
  no_screen_branch1 <- p_not_used * 0
  
  # Branch 2: Organs used
  p_used <- 1 - p_not_used
  no_screen_branch2 <- p_used * (
    p_inf * cost_inf +           # Donor has infection
      (1 - p_inf) * 0              # Donor no infection
  )
  
  no_screen_cost <- no_screen_branch1 + no_screen_branch2
  
  # ============================================================================
  # STRATEGY 2: SCREENING
  # ============================================================================
  
  screen_base_cost <- cost_scr
  
  # Branch A: Donor HAS infection
  # A1: Test positive (true positive)
  branchA1_prob <- p_inf * sens
  branchA1_cost <- (
    p_block_pos * 0 +                                    # Donation blocked
      (1 - p_block_pos) * (cost_pro + p_inf_proph * cost_inf)  # Prophylaxis given
  )
  
  # A2: Test negative (false negative)
  branchA2_prob <- p_inf * (1 - sens)
  branchA2_cost <- (
    p_block_neg * 0 +            # Donation blocked
      (1 - p_block_neg) * cost_inf # Donation occurs, infection develops
  )
  
  # Branch B: Donor NO infection
  # B1: Test positive (false positive)
  branchB1_prob <- (1 - p_inf) * (1 - spec)
  branchB1_cost <- (
    p_block_pos * 0 +                       # Donation blocked
      (1 - p_block_pos) * cost_pro            # Prophylaxis unnecessarily
  )
  
  # B2: Test negative (true negative)
  branchB2_prob <- (1 - p_inf) * spec
  branchB2_cost <- (
    p_block_neg * 0 +            # Donation blocked (rare)
      (1 - p_block_neg) * 0        # Donation occurs, no issues
  )
  
  screen_cost <- screen_base_cost + 
    (branchA1_prob * branchA1_cost) +
    (branchA2_prob * branchA2_cost) +
    (branchB1_prob * branchB1_cost) +
    (branchB2_prob * branchB2_cost)
  
  # Return both strategies
  return(c(
    no_screen = no_screen_cost,
    screen = screen_cost,
    incremental = screen_cost - no_screen_cost
  ))
}

# ==============================================================================
# 3. RUN PSA
# ==============================================================================

cat("Running Probabilistic Sensitivity Analysis...\n")
cat(paste0("Simulations: ", n_sims, "\n\n"))

# Run decision tree for each parameter set
psa_results <- data.frame(
  sim = 1:n_sims,
  no_screen_cost = numeric(n_sims),
  screen_cost = numeric(n_sims),
  incremental_cost = numeric(n_sims)
)

for (i in 1:n_sims) {
  result <- calculate_tree(psa_params[i, ])
  psa_results$no_screen_cost[i] <- result["no_screen"]
  psa_results$screen_cost[i] <- result["screen"]
  psa_results$incremental_cost[i] <- result["incremental"]
}

# ==============================================================================
# 4. RESULTS SUMMARY
# ==============================================================================

cat("====================================\n")
cat("PSA RESULTS SUMMARY\n")
cat("====================================\n\n")

cat("No Screening Strategy:\n")
cat(sprintf("  Mean Cost: $%.2f\n", mean(psa_results$no_screen_cost)))
cat(sprintf("  95%% CI: $%.2f - $%.2f\n\n", 
            quantile(psa_results$no_screen_cost, 0.025),
            quantile(psa_results$no_screen_cost, 0.975)))

cat("Screening Strategy:\n")
cat(sprintf("  Mean Cost: $%.2f\n", mean(psa_results$screen_cost)))
cat(sprintf("  95%% CI: $%.2f - $%.2f\n\n", 
            quantile(psa_results$screen_cost, 0.025),
            quantile(psa_results$screen_cost, 0.975)))

cat("Incremental Cost (Screen vs No Screen):\n")
cat(sprintf("  Mean: $%.2f\n", mean(psa_results$incremental_cost)))
cat(sprintf("  95%% CI: $%.2f - $%.2f\n", 
            quantile(psa_results$incremental_cost, 0.025),
            quantile(psa_results$incremental_cost, 0.975)))

pct_screening_cheaper <- mean(psa_results$incremental_cost < 0) * 100
cat(sprintf("\nScreening cheaper in %.1f%% of simulations\n", pct_screening_cheaper))

# ==============================================================================
# 5. VISUALIZATIONS
# ==============================================================================

# Cost-effectiveness plane
p1 <- ggplot(psa_results, aes(x = no_screen_cost, y = screen_cost)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Cost-Effectiveness Plane",
    subtitle = "Points below line = screening cheaper",
    x = "No Screening Cost ($)",
    y = "Screening Cost ($)"
  ) +
  theme_minimal()

print(p1)

# Incremental cost distribution
p2 <- ggplot(psa_results, aes(x = incremental_cost)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(psa_results$incremental_cost), 
             color = "darkblue", linewidth = 1) +
  labs(
    title = "Distribution of Incremental Cost",
    subtitle = paste0("Screening cheaper in ", round(pct_screening_cheaper, 1), "% of simulations"),
    x = "Incremental Cost (Screen - No Screen) ($)",
    y = "Count"
  ) +
  theme_minimal()

print(p2)

# Cost distribution comparison
p3 <- psa_results %>%
  select(sim, no_screen_cost, screen_cost) %>%
  pivot_longer(cols = c(no_screen_cost, screen_cost), 
               names_to = "strategy", 
               values_to = "cost") %>%
  ggplot(aes(x = cost, fill = strategy)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("no_screen_cost" = "coral", "screen_cost" = "steelblue"),
    labels = c("No Screening", "Screening")
  ) +
  labs(
    title = "Cost Distribution by Strategy",
    x = "Expected Cost ($)",
    y = "Density",
    fill = "Strategy"
  ) +
  theme_minimal()

print(p3)

# ==============================================================================
# 6. EXPORT RESULTS
# ==============================================================================

# Save results
write.csv(psa_results, "output/psa_results.csv", row.names = FALSE)

# Save summary statistics
summary_stats <- data.frame(
  Strategy = c("No Screening", "Screening", "Incremental"),
  Mean = c(mean(psa_results$no_screen_cost), 
           mean(psa_results$screen_cost),
           mean(psa_results$incremental_cost)),
  SD = c(sd(psa_results$no_screen_cost),
         sd(psa_results$screen_cost),
         sd(psa_results$incremental_cost)),
  CI_lower = c(quantile(psa_results$no_screen_cost, 0.025),
               quantile(psa_results$screen_cost, 0.025),
               quantile(psa_results$incremental_cost, 0.025)),
  CI_upper = c(quantile(psa_results$no_screen_cost, 0.975),
               quantile(psa_results$screen_cost, 0.975),
               quantile(psa_results$incremental_cost, 0.975))
)

write.csv(summary_stats, "output/psa_summary.csv", row.names = FALSE)

cat("\n✅ Results saved to output/ folder\n")