setwd("C:\\Users\\PASINDU VIHANGA\\Desktop\\SLIIT\\Year 02\\Semester 01\\P&S\\Week 12\\Lab 10-20251017")
# Suppose researcher recorded the following snack purchases:
snack_counts <- c(120, 95, 85, 100)   # A, B, C, D
names(snack_counts) <- c("A", "B", "C", "D")

# 3. State hypotheses
# H0 - Customers choose all snack types equally (equal probabilities)
# H1 - Customers do NOT choose all snack types equally

# 4. Perform Chi-Square Goodness-of-Fit Test
chisq_result <- chisq.test(snack_counts)

# 5. Display test result
print(chisq_result)

# 6. Interpretation
# If p-value < 0.05 → Reject H0 (snacks not chosen equally)
# If p-value ≥ 0.05 → Fail to reject H0 (snacks chosen equally)
