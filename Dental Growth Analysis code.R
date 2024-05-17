
library(ggplot2)
library(lme4)
library(nlme)
library(rlme)
library(performance)
library(predictmeans)
library(lattice)


######################################################################
## 1) Data visualization

#setwd()

# Read the study data text file
growth <- read.table('growth.txt', header=TRUE, sep="\t")

# set males to 0 and females to 1 for clarity sake
growth$SEX <- ifelse(growth$SEX == 1, 0, growth$SEX)
growth$SEX <- ifelse(growth$SEX == 2, 1, growth$SEX)

# have a first look at the data characteristics  
head(growth)
summary(growth)

# look at the interaction between variables
plot(growth)


# check the frequency repartition of the measures
hist(growth$MEASURE, main="Dental Growth histogram", xlab="Dental Growth")

# boxplot of the measures depending on the age
boxplot(MEASURE ~ AGE, data=growth, main="AGE boxplot", xlab="Age", ylab="Mesure")

# Boxplot of the measures depending on the sex
boxplot(MEASURE ~ SEX, data=growth, main="SEX boxplot", xlab="Sex", ylab="Mesure")


# Use ggplot2 to create a more detailled boxplot containing both predictors
growth$AGE_SEX <- interaction(growth$AGE, growth$SEX, sep=" - ")
ggplot(growth, aes(x=AGE_SEX, y=MEASURE, fill=factor(SEX, levels = c(0, 1), labels = c("Male", "Female")))) + 
  geom_boxplot() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Age and Sex", y = "Measurement", fill = "Sex") +
  ggtitle("Boxplots of Measurements by Age and Sex")


#create a  spaghetti plot with all individuals
males_data <- subset(growth, SEX == "0")
females_data <- subset(growth, SEX == "1")

# Calculate the mean evolution for males and females
mean_males <- aggregate(MEASURE ~ AGE, data = males_data, FUN = mean)
mean_females <- aggregate(MEASURE ~ AGE, data = females_data, FUN = mean)

#minor change to color males and females in different colors
growth$SEX <- factor(growth$SEX, levels = c("0", "1"))

# Create the spaghetti plot for males with mean evolution
plot_all <- ggplot(growth, aes(x = AGE, y = MEASURE, group = IDNR, color = SEX)) + 
  geom_line(alpha = 0.2) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) + 
  geom_line(data = mean_males, aes(x = AGE, y = MEASURE), color = "blue", size = 1, inherit.aes = FALSE) + 
  geom_line(data = mean_females, aes(x = AGE, y = MEASURE), color = "red", size = 1, inherit.aes = FALSE) +
  labs(title = "Spaghetti Plot of dental growth", x = "Age", y = "Dental Growth") +
  theme_minimal() +
  theme(legend.position = "right") # Remove legend from individual plots

plot_all


#change the age to make it start at 0 for clarity
growth <- read.table('growth.txt', header=TRUE, sep="\t")
growth$SEX <- ifelse(growth$SEX == 1, 0, growth$SEX)
growth$SEX <- ifelse(growth$SEX == 2, 1, growth$SEX)
growth$AGE<-growth$AGE-8


# Calculate the coefficients of the linear regression for each individual
lin.reg.coef <- by(growth, growth$IDNR, 
                   function(data) coef(lm(MEASURE ~ AGE, data=data)))
lin.reg.coef1 <- unlist(lin.reg.coef)
names(lin.reg.coef1) <- NULL 
lin.reg.coef2 <- matrix(lin.reg.coef1, length(lin.reg.coef1)/2, 2, byrow = TRUE)


# Calculate the R squared value of the linear regression for each individual
lin.reg.r.squared <- by(growth, growth$IDNR, 
                        function(data) summary(lm(MEASURE ~ AGE, data=data))$r.squared )
lin.reg.r.squared1 <- as.vector(unlist(lin.reg.r.squared))


## create individual characteristics histograms

# Set up the layout for the three histograms
par(mfrow=c(1,3))

# Histogram of individual intercepts
range_intercept <- range(lin.reg.coef2[,1])
breaks_intercept <- seq(from = floor(range_intercept[1]), to = ceiling(range_intercept[2]), by = 1)
hist(lin.reg.coef2[,1], breaks = breaks_intercept, xlab="Intercept", col="lightblue", main="Histogram of individual intercepts")

# Histogram of individual slopes
range_slope <- range(lin.reg.coef2[,2])
breaks_slope <- seq(from = floor(range_slope[1]), to = ceiling(range_slope[2]), by = 0.25)
hist(lin.reg.coef2[,2], breaks = breaks_slope, xlab="Slope", col="lightblue", main="Histogram of individual slopes")

# Histogram of individual R squared
hist(lin.reg.r.squared1, xlab="R squared", col="lightblue", main="Histogram of individual R squared")

par(mfrow=c(1,1))

# Calculate and plot the correlation between intercepts and slopes
int.slope.corr <- cor(lin.reg.coef2[,1], lin.reg.coef2[,2])
plot(lin.reg.coef2[,1], lin.reg.coef2[,2], xlab="Intercept", ylab="Slope", main="Intercept versus Slope")

## Plotting individual regression lines per group

# Create a combined matrix 'reg.coef' that includes the intercepts and slopes for each individual,
# along with the 'SEX' group at the baseline 'AGE'
reg.coef <- cbind(lin.reg.coef2, growth$SEX[!duplicated(growth$IDNR)])

# Compute the mean intercept and slope for each SEX
mean.int <- tapply(reg.coef[,1], reg.coef[,3], mean)
mean.slope <- tapply(reg.coef[,2], reg.coef[,3], mean)


par(mfrow=c(1,2))

# Ploting every individual regression line

# Plot for the males
plot(growth$AGE, growth$MEASURE, type="n", xlim=c(0, max(growth$AGE)), ylim=c(min(growth$MEASURE), max(growth$MEASURE)), main="Males", xlab="Age-8 (in years)", ylab="Measure", axes=FALSE)
axis(side=1, at=unique(growth$AGE))
axis(side=2)
box()
# Loop through each individual and plot their regression line
for (i in 1:nrow(reg.coef)) {
  if (reg.coef[i,3] == 0) {
    curve(reg.coef[i,1] + reg.coef[i,2] * x, add=TRUE, col="gray")
  }
}
# Add the average regression line for this group
curve(mean.int[1] + mean.slope[1] * x, add=TRUE, lwd=2)

# Plot for the females
plot(growth$AGE, growth$MEASURE, type="n", xlim=c(0, max(growth$AGE)), ylim=c(min(growth$MEASURE), max(growth$MEASURE)), main="Females", xlab="Age-8 (in years)", ylab="Measure", axes=FALSE)
axis(side=1, at=unique(growth$AGE))
axis(side=2)
box()
# Loop through each individual and plot their regression line
for (i in 1:nrow(reg.coef)) {
  if (reg.coef[i,3] == 1) {
    curve(reg.coef[i,1] + reg.coef[i,2] * x, add=TRUE, col="gray")
  }
}
# Add the average regression line for this group
curve(mean.int[2] + mean.slope[2] * x, add=TRUE, lwd=2)


######################################################################
## 2) Model Hypothesis and Model building  

# 2.1) Hypothesis 1 :
# Fitting a null lm model & a null lmer model with random intercept to check 
#if adding at least one random effect enhance the model 
# MEASURE is the dependent variable
# Only random effect for Intercept because it is expected after the data visualization

model_null_gls <- gls(MEASURE ~ 1, data = growth)
model_null_lme_REML <-   lme(MEASURE ~ 1, random =~1 | IDNR, data = growth, method = "REML")

# individual look at each model
summary(model_null_gls)
summary(model_null_lme_REML)

# Compare the two models between them
anova(model_null_gls, model_null_lme_REML)

#  confirm the anova results by calculating the AIC weights
AIC(model_null_gls, model_null_lme_REML)

######################################################################
## 2.2) Hypothesis 2 : 
# Fitting the null lme model & a model with AGE and SEX as predictors to compare them
# AGE and SEX are considered as fixed effects
# removing the “lme” from the “model_lme_null” for clarity sake because we will only use lme from now on

model_null_ML <-   lme(MEASURE ~ 1, random =~1 | IDNR, data = growth, method = "ML")
model_no_interaction_ML <- lme(MEASURE ~ AGE + SEX, random = ~1|IDNR, data = growth, method = "ML")

summary(model_no_interaction_ML)

anova(model_null_ML,model_no_interaction_ML)
AIC(model_null_ML, model_no_interaction_ML)

######################################################################
## 2.3) Hypothesis 3 :
# Compare the no interaction model with a interaction model to check its effect on fitting the data
model_no_interaction_ML <- lme(MEASURE ~ AGE + SEX, random = ~1|IDNR, data = growth, method = "ML")
model_interaction_ML <- lme(MEASURE ~ AGE *SEX, random = ~1|IDNR, data = growth, method = "ML")

summary(model_interaction_ML)

anova(model_no_interaction_ML, model_interaction_ML)
AIC(model_no_interaction_ML, model_interaction_ML)

######################################################################
## 2.4) Hypothesis 4 :
# Check if adding a random slope to the random error enhance the model
model_interaction_REML <- lme(MEASURE ~ AGE *  SEX, random = ~1|IDNR, data = growth, method = "REML")
model_interaction_rslope_REML <- lme(MEASURE ~ AGE *  SEX, random = ~1+ AGE|IDNR, data = growth, method = "REML")

summary(model_interaction_rslope_REML)

anova(model_interaction_REML, model_interaction_rslope_REML)
AIC(model_interaction_REML, model_interaction_rslope_REML)

######################################################################
## 2.5) Hypothesis 5 :
# Check if the residuals errors have the same variance for men and women or if they are different 
model_interaction_REML <- lme(MEASURE ~ AGE*SEX, random=~ 1 | IDNR, data = growth, method = "REML")
model_interaction_heteroscedastic_REML <- lme(MEASURE~ AGE*SEX, random=~ 1 | IDNR, data = growth,  method = "REML", weights = varIdent(form =~1|SEX))

summary(model_interaction_heteroscedastic_REML)

anova(model_interaction_REML,model_interaction_heteroscedastic_REML)
AIC(model_interaction_REML,model_interaction_heteroscedastic_REML)

######################################################################
## 3) Model assumptions

# Do the assumption checks on residuals
check_model(model_interaction_heteroscedastic_REML)

 # Calculate Cook's distances using CookD function but do not plot them
cooks_values <- CookD(model_interaction_heteroscedastic_REML, plot=FALSE)

# Calculate the threshold for influential points
threshold <- 4 / (nrow(growth) - length(fixed.effects(model_interaction_heteroscedastic_REML)))

# Plot Cook's distances
dev.off()
# Plot Cook's distances with specified y-axis limits
plot(cooks_values, type="h", col="black", main="Cook's Distance", xlab="Index", ylab="Cook's Distance", ylim=c(0, 0.13))
abline(h = threshold, col="red")
text(x = high_cooks_indices, y = cooks_values[high_cooks_indices], labels = high_cooks_indices, pos = 3, col = "blue")


# Find indices of values that exceed the threshold
high_cooks_indices <- which(cooks_values > threshold)

# Annotate points that are above the threshold
text(x = high_cooks_indices, y = cooks_values[high_cooks_indices], labels = high_cooks_indices, pos = 3, col = "blue")

######################################################################
## 4) graphical tools and confidence intervals for interpretation


# Fit the models
model_random.intercept.lmer <- lmer(MEASURE ~ 1 + AGE + SEX + AGE*SEX + (1 | IDNR), data = growth)
model_random.intercept.lme <- lme(MEASURE ~ 1 + AGE + SEX + AGE*SEX, random=~ 1|IDNR, data = growth, method = "REML", weights = varIdent(form =~1|SEX))
### Extracting the re with the sd in lmer. 
## It is not possible to get the conditional variance of the re in lme

dotplot(random.effects(model_random.intercept.lmer, condVar = T))

# Show confidence intervals of our final model
intervals(model_random.intercept.lme)

