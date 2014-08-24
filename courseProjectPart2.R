################################################################################################
# Attempt 2 for part 2 
################################################################################################
## 3. Use Confidence Intervals and Hypothesis Tests to compare tooth growth by supplement and dose

### a) Confidence Intervals

#__Comparing by Supp__

dt[, list(meanLength=mean(len), sdLength=sd(len)), by = supp]
#    supp meanLength sdLength
# 1:   VC   16.96333 8.266029
# 2:   OJ   20.66333 6.605561

###############
# Individually
###############
n_VC = 30
X_VC = 16.96333
s_VC = 8.266029
degfree = n-1      

# What is 95% CI for the Mean length using VC?

# X_bar +/- tn-1 * s / sqrt(n)
round(X_VC + c(-1,1) * qt(0.975, degfree) * s_VC / sqrt(n_VC))
# CI = 13, 20

n_OJ = 30
X_OJ = 20.66333
s_OJ = 6.605561
degfree = n-1      

# What is 95% CI for the Mean length using OJ?

# X_bar +/- tn-1 * s / sqrt(n)
round(X_VC + c(-1,1) * qt(0.975, degfree) * s_VC / sqrt(n_VC))
# CI = 18, 23


###############
# Paired
###############
# Y
n_OJ = 30
X_OJ = 20.66333
s_OJ = 6.605561

# X
n_VC = 30
X_VC = 16.96333
s_VC = 8.266029

alpha = 0.05

degfree = n_OJ + n_VC - 2

Sp = sqrt( ( (n_VC - 1) * s_VC^2 + (n_OJ - 1) * s_OJ^2 ) /  degfree )

# t for 95%
round(X_OJ - X_VC + c(-1,1) * qt(1 - alpha/2, degfree) * Sp * sqrt(1/n_VC + 1/n_OJ), 2)

# CI = -0.17,  7.57
# Most of the time the OJ out-performs the VC with 95% confidence




#__Comparing by Dose__

dt[, list(meanLength=mean(len), sdLength=sd(len)), by = dose]
#    dose meanLength sdLength
# 1:  0.5     10.605 4.499763
# 2:  1.0     19.735 4.415436
# 3:  2.0     26.100 3.774150

# X
n_05 = 20
X_05 = 10.605
s_05 = 4.499763

# Y
n_10 = 20
X_10 = 19.735
s_10 = 4.415436

# Z
n_20 = 20
X_20 = 26.100
s_20 = 3.774150

# 95% CI
alpha = 0.05

## Compare 0.5 vs 1.0 
degfree = n_10 + n_05 - 2

Sp = sqrt( ( (n_05 - 1) * s_05^2 + (n_10 - 1) * s_10^2 ) /  degfree )

# t for 95%
round(X_10 - X_05 + c(-1,1) * qt(1 - alpha/2, degfree) * Sp * sqrt(1/n_05 + 1/n_10), 2)

# CI = 6.28 11.98
# Dose of 1.0 always out performs does of 0.5 in stimulating tooth growth with 95% confidence

## Compare 0.5 vs 2.0 
degfree = n_20 + n_05 - 2

Sp = sqrt( ( (n_05 - 1) * s_05^2 + (n_20 - 1) * s_20^2 ) /  degfree )

# t for 95%
round(X_20 - X_05 + c(-1,1) * qt(1 - alpha/2, degfree) * Sp * sqrt(1/n_05 + 1/n_20), 2)

# CI = 12.84 18.15
# Dose of 2.0 always out performs does of 0.5 in stimulating tooth growth with 95% confidence

## Compare 1.0 vs 2.0 
degfree = n_20 + n_10 - 2

Sp = sqrt( ( (n_10 - 1) * s_10^2 + (n_20 - 1) * s_20^2 ) /  degfree )

# t for 95%
round(X_20 - X_10 + c(-1,1) * qt(1 - alpha/2, degfree) * Sp * sqrt(1/n_10 + 1/n_20), 2)

# CI = 3.74 8.99
# Dose of 2.0 always out performs does of 1.0 in stimulating tooth growth with 95% confidence





### b) Hypothesis Test
#__Comparing by Supp__




#__Comparing by Dose__



