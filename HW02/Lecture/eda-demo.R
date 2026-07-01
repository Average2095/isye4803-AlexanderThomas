# ===================================================================
# Day 2 AFTERNOON demo -- EDA & wrangling: profile, plot, missing data,
# outliers, and the traps (mean-imputation, misleading defaults).
# ISyE 4803 Financial Data Analysis | Prof. X. Huo
#
# Run line-by-line. Working dir = THIS folder (loads roe01.txt).
# Theme of the session: missing-data handling and plot choice are
# JUDGMENT, not syntax. Always plot before you model (Anscombe).
# ===================================================================

rm(list = ls())
a <- read.table("roe01.txt", header = TRUE)   # 500 firm-years, 10 columns

## ------------------------------------------------------------------
## 1. Profile a dataset before touching a model
## ------------------------------------------------------------------
dim(a)                       # how big
str(a)                       # types -- catch a factor masquerading as numeric
summary(a)                   # per-column min/median/mean/max + NA count
round(sapply(a, sd), 3)      # spread per column
round(cor(a)[, "ROE"], 3)    # which columns move with ROE?

## ------------------------------------------------------------------
## 2. ALWAYS PLOT FIRST -- Anscombe's quartet (built into R)
##    Four datasets, identical mean/var/cor/regression line, 4 stories.
## ------------------------------------------------------------------
sapply(1:4, function(i) c(
  mean_x = mean(anscombe[[paste0("x", i)]]),
  mean_y = mean(anscombe[[paste0("y", i)]]),
  cor    = cor(anscombe[[paste0("x", i)]], anscombe[[paste0("y", i)]])
))                            # the numbers agree to 2-3 digits...

op <- par(mfrow = c(2, 2), mar = c(4,4,2,1))
for (i in 1:4) {
  xi <- anscombe[[paste0("x", i)]]; yi <- anscombe[[paste0("y", i)]]
  plot(xi, yi, pch = 19, col = "steelblue",
       xlim = c(2,20), ylim = c(2,14),
       main = paste("Set", i), xlab = "x", ylab = "y")
  abline(lm(yi ~ xi), col = "red")    # ...but the pictures do NOT
}
par(op)

## ------------------------------------------------------------------
## 3. Good plots for the real data
## ------------------------------------------------------------------
hist(a$ROE, breaks = 30, col = "grey80", main = "Distribution of ROE", xlab = "ROE")
plot(a$ROEt, a$ROE, pch = 19, col = rgb(0,0,1,0.3),   # alpha for overplotting
     xlab = "ROE this year (ROEt)", ylab = "ROE next year")
abline(lm(ROE ~ ROEt, data = a), col = "red", lwd = 2)

# A MISLEADING default vs an honest plot: overplotted points hide density.
plot(a$ASSET, a$ROE, main = "default: overplotted")          # looks like a blob
plot(a$ASSET, a$ROE, pch = 19, col = rgb(0,0,0,0.15),
     main = "alpha + lowess: the trend appears")
lines(lowess(a$ASSET, a$ROE), col = "red", lwd = 2)

## ------------------------------------------------------------------
## 4. Outliers / influence -- one row can move the line
## ------------------------------------------------------------------
op <- par(mfrow = c(2, 2)); plot(lm(ROE ~ ROEt, data = a), which = 1:4); par(op)
# Cook's distance flags influential rows. Refit without the worst one and
# compare the slope -- this is a MODELING DECISION you must justify, not hide:
infl  <- which.max(cooks.distance(lm(ROE ~ ROEt, data = a)))
cat("most influential row:", infl, "\n")
slope_all  <- coef(lm(ROE ~ ROEt, data = a))[2]
slope_drop <- coef(lm(ROE ~ ROEt, data = a[-infl, ]))[2]
cat(sprintf("slope with all rows: %.4f | dropping row %d: %.4f\n",
            slope_all, infl, slope_drop))

## ------------------------------------------------------------------
## 5. THE TRAP: silent mean-imputation shrinks standard errors
## ------------------------------------------------------------------
set.seed(4803)                              # reproducible (Day-1 habit!)
b <- a
miss <- sample(nrow(b), 150)                # pretend 30% of ROEt is missing
b$ROEt[miss] <- NA

# (i) honest: complete-case analysis -- uses the 350 real rows
fit_cc  <- lm(ROE ~ ROEt, data = b)         # lm() drops NA rows by default
# (ii) the silent fix an AI loves: replace NA with the column mean, then fit
b_imp <- b
b_imp$ROEt[is.na(b_imp$ROEt)] <- mean(b_imp$ROEt, na.rm = TRUE)
fit_imp <- lm(ROE ~ ROEt, data = b_imp)

se <- function(f) summary(f)$coefficients["ROEt", "Std. Error"]
cat(sprintf("SE(slope)  complete-case: %.4f   mean-imputed: %.4f\n",
            se(fit_cc), se(fit_imp)))
# The imputed SE is SMALLER even though we added NO new information:
# 150 points stacked on the mean fake-inflate n and pull the slope toward 0.
# "It ran and the CI got tighter" is exactly the wrong conclusion.

## ------------------------------------------------------------------
## 6. Wrap
## ------------------------------------------------------------------
# Profiling + the right plot caught what a tidy summary statistic hid.
# Imputation and plot choice are decisions with consequences -- read what
# the AI wrote, plot before modeling, and state how you handled NA/outliers.

