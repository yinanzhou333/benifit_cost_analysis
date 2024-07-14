# ALY6050 Module2 Benefit-Cost Analysis
# Yinan Zhou 06/02/2024

# Part1
# (i)
set.seed(123)
K<-10000
cost1<-vector()
cost2<-vector()
benifit1<-vector()
benifit2<-vector()
ratio1 <-vector()
ratio2 <-vector()

getRandomVariate <-function(a,c,b){
  Fc <- (c-a)/(b-a)
  U <- runif(1)
  x <- 0
  if (U<Fc){
    x<-a+sqrt(U*(b-a)*(c-a))
  } 
  else{
    x<-b-sqrt((1-U)*(b-a)*(b-c))
  }
  return (x)
}
#x<-getRandomVariate (a,c,b)

for (j in 1:K){
  #Dam1
  B1 <- getRandomVariate(1.1, 2, 2.8)
  B2 <- getRandomVariate(8, 12, 14.9)
  B3 <- getRandomVariate(1.4, 1.4, 2.2)
  B4 <- getRandomVariate(6.5, 9.8, 14.6)
  B5 <- getRandomVariate(1.7, 2.4, 3.6)
  B6 <- getRandomVariate(0, 1.6, 2.4)
  C1 <- getRandomVariate(13.2, 14.2, 19.1)
  C2 <- getRandomVariate(3.5, 4.9, 7.4)
  benifit1[j] <- B1 + B2 + B3 + B4 + B5 + B6
  cost1[j] <- C1 + C2
  ratio1[j] <- benifit1[j]/cost1[j]
  
  ##############################################################################
  #Dam2
  B1_d2 <- getRandomVariate(2.1, 3, 4.8)
  B2_d2 <- getRandomVariate(8.7, 12.2, 13.6)
  B3_d2 <- getRandomVariate(2.3, 3, 3)
  B4_d2 <- getRandomVariate(5.9, 8.7, 15)
  B5_d2 <- getRandomVariate(0, 3.4, 3.4)
  B6_d2 <- getRandomVariate(0, 1.2, 1.8)
  C1_d2 <- getRandomVariate(12.8, 15.8, 20.1)
  C2_d2 <- getRandomVariate(3.8, 5.7, 8)
  benifit2[j] <- B1_d2 + B2_d2 + B3_d2 + B4_d2 + B5_d2 + B6_d2
  cost2[j] <- C1_d2 + C2_d2
  ratio2[j] <- benifit2[j]/cost2[j]
}

cat("mean of ratio 1 =", mean(ratio1),'\n')
cat("mean of ratio 2 =", mean(ratio2),'\n')
cat("SD of ratio 1 =", sd(ratio1),'\n')
cat("SD of ratio 2 =", sd(ratio2),'\n')

# (ii)
#Dam 1
hist_ratio1 <- hist(ratio1, breaks =20, freq = F) 
lines(density(ratio1), lwd=2, col="blue")

# Create bins and cut the ratio1 data into bins
breaks <- seq(0.95, 2.10, by = 0.05)
ratio1_cut <- cut(ratio1, breaks, right = FALSE, include.lowest = TRUE)
# Create a frequency table
freq_table1 <- table(ratio1_cut)
freq_df1 <- as.data.frame(freq_table1)
colnames(freq_df1) <- c("Ratio1 Range", "Frequency")


#Dam 2
hist_ratio2 <-hist(ratio2, breaks =20, freq = F)
lines(density(ratio2), lwd=2, col="blue")

# Create bins and cut the ratio2 data into bins
breaks <- seq(0.95, 2.10, by = 0.05)
ratio2_cut <- cut(ratio2, breaks, right = FALSE, include.lowest = TRUE)
# Create a frequency table
freq_table2 <- table(ratio2_cut)
freq_df2 <- as.data.frame(freq_table2)
colnames(freq_df2) <- c("Ratio2 Range", "Frequency")

# (iii)
cal_Mean_Var <- function(a,c,b){
  mean = (a+b+c)/3.0
  var = (a^2+b^2+c^2-a*b-a*c-b*c)/18.0
  return (list(mean = mean, var = var))
}

getMean <- function(B,n){
  total_mean<-0
  for (j in 1:n){
    total_mean<-total_mean+B[[j]]$mean
  }
  return (total_mean)
}

getVar <- function(B,n){
  total_var<-0
  for (j in 1:n){
    total_var<-total_var+B[[j]]$var
  }
  return (total_var)
}

#dam1
B_m_v <-list()
B_m_v[[1]] <- cal_Mean_Var(1.1, 2, 2.8)
B_m_v[[2]] <- cal_Mean_Var(8, 12, 14.9)
B_m_v[[3]] <- cal_Mean_Var(1.4, 1.4, 2.2)
B_m_v[[4]] <- cal_Mean_Var(6.5, 9.8, 14.6)
B_m_v[[5]] <- cal_Mean_Var(1.7, 2.4, 3.6)
B_m_v[[6]] <- cal_Mean_Var(0, 1.6, 2.4)
C_m_v <-list()
C_m_v[[1]] <- cal_Mean_Var(13.2, 14.2, 19.1)
C_m_v[[2]] <- cal_Mean_Var(3.5, 4.9, 7.4)

observed_values <- c(mean(benifit1), sd(benifit1), mean(cost1), sd(cost1), mean(ratio1), sd(ratio1))
theoretical_values <- c(getMean(B_m_v,6),sqrt(getVar(B_m_v,6)), getMean(C_m_v,2),sqrt(getVar(C_m_v,2)),NA,NA) 

dam1_df <- data.frame(
  Dam1 = c("Mean of the Total Benefits",
           "SD of the Total Benefits",
           "Mean of the Total Cost",
           "SD of the Total Cost",
           "Mean of the Benefit-cost Ratio",
           "SD of the Benefit-cost Ratio"),
  Observed = observed_values,
  Theoretical = theoretical_values
)

#dam2
B_m_v <-list()
B_m_v[[1]] <- cal_Mean_Var(2.1, 3, 4.8)
B_m_v[[2]] <- cal_Mean_Var(8.7, 12.2, 13.6)
B_m_v[[3]] <- cal_Mean_Var(2.3, 3, 3)
B_m_v[[4]] <- cal_Mean_Var(5.9, 8.7, 15)
B_m_v[[5]] <- cal_Mean_Var(0, 3.4, 3.4)
B_m_v[[6]] <- cal_Mean_Var(0, 1.2, 1.8)
C_m_v <-list()
C_m_v[[1]] <- cal_Mean_Var(12.8, 15.8, 20.1)
C_m_v[[2]] <- cal_Mean_Var(3.8, 5.7, 8)

observed_values <- c(mean(benifit2), sd(benifit2), mean(cost2), sd(cost2), mean(ratio2), sd(ratio2))
theoretical_values <- c(getMean(B_m_v,6),sqrt(getVar(B_m_v,6)), getMean(C_m_v,2),sqrt(getVar(C_m_v,2)),NA,NA) 
dam2_df <- data.frame(
  Dam2 = c("Mean of the Total Benefits",
           "SD of the Total Benefits",
           "Mean of the Total Cost",
           "SD of the Total Cost",
           "Mean of the Benefit-cost Ratio",
           "SD of the Benefit-cost Ratio"),
  Observed = observed_values,
  Theoretical = theoretical_values
)

print(dam1_df)
print(dam2_df)

################################################################################

# Part2
# install.packages("fBasics")
library(fBasics)
skewness(ratio1)
kurtosis(ratio1)
skewness(ratio2)
kurtosis(ratio2)

library(ggplot2)
library(MASS)

fit_normal<-fitdistr(ratio1,"normal")
fit_gamma<-fitdistr(ratio1,"gamma")
fit_lognormal<-fitdistr(ratio1,"log-normal")
fit_normal
fit_gamma
fit_lognormal

df <- data.frame(ratio1)
p <- ggplot(df, aes(x=ratio1)) +
  geom_histogram(aes(y=..density..), bins=100, fill="gray", alpha=0.5) +
  geom_density(aes(y=..density..), fill="gray", alpha=0.5) +
  ggtitle("Distribution Comparison") +
  xlab("ratio1") +
  ylab("Density")

p <- p + stat_function(fun=dnorm, args=list(mean=fit_normal$estimate["mean"], sd=fit_normal$estimate["sd"]), 
                       color="green", size=1.2, linetype="dashed")

p <- p + stat_function(fun=dgamma, args=list(shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"]), 
                       color="red", size=1.2, linetype="dotted")

p <- p + stat_function(fun=dlnorm, args=list(meanlog=fit_lognormal$estimate["meanlog"], sdlog=fit_lognormal$estimate["sdlog"]), 
                       color="blue", size=1.2, linetype="dashed")
p


hist_ratio1 <- hist(ratio1, breaks =180, freq = F) 
observed_counts <- hist_ratio1$counts
bin_edges <- hist_ratio1$breaks

# Calculate expected counts for gamma distribution
expected_gamma <- diff(pgamma(bin_edges, shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])) * length(ratio1)

# Calculate expected counts for normal distribution
expected_normal <- diff(pnorm(bin_edges, mean=fit_normal$estimate["mean"], sd=fit_normal$estimate["sd"])) * length(ratio1)

# Calculate expected counts for log-normal distribution
expected_lognormal <- diff(plnorm(bin_edges, meanlog=fit_lognormal$estimate["meanlog"], sdlog=fit_lognormal$estimate["sdlog"])) * length(ratio1)

# Perform Chi-squared Goodness-of-fit tests
chi_square_gamma <- chisq.test(observed_counts, p=expected_gamma, rescale.p = TRUE)
chi_square_normal <- chisq.test(observed_counts, p=expected_normal, rescale.p = TRUE)
chi_square_lognormal <- chisq.test(observed_counts, p=expected_lognormal, rescale.p = TRUE)

chi_square_gamma
chi_square_normal
chi_square_lognormal

################################################################################

# Part3
# (i)
getRatioP <- function(ratio,alpha){
  P = sum(ratio>alpha)/length(ratio)
  return (P)
}

# sum(ratio1>1.2)/length(ratio1)

P1 <- getRatioP(ratio1,2)
P2 <- getRatioP(ratio1,1.8)
P3 <- getRatioP(ratio1,1.5)
P4 <- getRatioP(ratio1,1.2)
P5 <- getRatioP(ratio1,1)

P6 <- getRatioP(ratio2,2)
P7 <- getRatioP(ratio2,1.8)
P8 <- getRatioP(ratio2,1.5)
P9 <- getRatioP(ratio2,1.2)
P10 <- getRatioP(ratio2,1)

dam1 <-c(min(ratio1), max(ratio1), mean(ratio1), median(ratio1), var(ratio1), sd(ratio1), skewness(ratio1), P1, P2, P3, P4, P5)
dam2 <-c(min(ratio2), max(ratio2), mean(ratio2), median(ratio2), var(ratio2), sd(ratio2), skewness(ratio2), P6, P7, P8, P9, P10)
ratio_comparison <- data.frame(
  Metric = c("Minimum",
             "Maximum",
             "Mean",
             "Median",
             "Variance",
             "Standard Deviation",
             "Skewness",
             "P(alpha>2)",
             "P(alpha>1.8)",
             "P(alpha>1.5)",
             "P(alpha>1.2)",
             "P(alpha>1)"),
  ratio1 = dam1,
  ratio2 = dam2
)
ratio_comparison

# (ii)
#probability of ratio1>ratio2
sum(ratio1>ratio2)/length(ratio1)
