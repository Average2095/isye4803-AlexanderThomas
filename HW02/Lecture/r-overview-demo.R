# ===================================================================
# Day 2 MORNING demo -- R Overview: structures, factors, reading lm()
# ISyE 4803 Financial Data Analysis | Prof. X. Huo
#
# Run line-by-line in RStudio (Ctrl/Cmd-Enter) so the class sees each
# result. Set the working directory to THIS folder first so the data
# file loads:
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))   # RStudio
# or just open this file's folder as the project. Data: roe01.txt
# (500 firm-years; columns ROEt ATO PM LEV GROWTH PB ARR INV ASSET ROE).
# ===================================================================

rm(list = ls())            # clear the workspace -- reproducible from the top

## ------------------------------------------------------------------
## 1. Everything is an object; you INSPECT objects, not re-derive them
## ------------------------------------------------------------------
x <- c(0.012, -0.004, 0.008, 0.005)   # assign with <-
x                                      # auto-print
class(x); length(x)                    # ask the object what it is

## ------------------------------------------------------------------
## 2. The five core data structures (with finance flavor)
## ------------------------------------------------------------------

# (a) VECTOR -- ordered, ONE type; vectorized math + logical subsetting
returns <- c(0.012, -0.004, 0.008, 0.005, -0.011)
mean(returns)                 # vectorized -- no loop
returns[returns > 0]          # logical subset: only the up days
returns * 100                 # elementwise

# (b) MATRIX -- 2-D, ONE type; this is your linear-algebra object
X <- matrix(c(1,1,1, 0.30,0.67,-0.05), ncol = 2)   # design matrix [1 | ROEt]
X
t(X) %*% X                    # X'X -- the thing OLS inverts

# (c) DATA.FRAME -- a table; columns may DIFFER in type. Everyday dataset.
df <- data.frame(
  firm   = c("A","B","C"),    # character
  up_day = c(TRUE, FALSE, TRUE),
  ret    = c(0.012, -0.004, 0.008)
)
df
str(df)                       # <- str() is the first thing to run on ANY dataset

# (d) LIST -- a container of anything; a fitted model is a list underneath
box <- list(name = "stockA", n = 5L, rets = returns)
box$rets                      # access by name with $
box[["n"]]

# (e) FACTOR -- a CATEGORICAL variable: integer codes + a labels table
sector <- factor(c("Tech","Bank","Tech","Energy","Bank"))
sector
levels(sector)                # the labels
as.integer(sector)            # the hidden integer CODES (1,2,3...) -- remember this!

## ------------------------------------------------------------------
## 3. THE FACTOR TRAP  (the bug the AI reproduces constantly)
## ------------------------------------------------------------------
# Numbers read from a messy file: one stray "n/a" turns the whole column to text,
# and stringsAsFactors makes it a factor. Then as.numeric() lies to you.
raw <- factor(c("0.012","-0.004","0.008","n/a","0.005"))   # what read.csv can hand you
as.numeric(raw)               # WRONG: returns the level codes 1,2,3,4,5 -- not the values!
mean(as.numeric(raw))         # a confident, meaningless number

# the FIX: text -> number goes through as.character() first; bad entries become NA
fixed <- as.numeric(as.character(raw))
fixed
mean(fixed, na.rm = TRUE)     # the real average of the valid entries
# Diagnose with class()/str() whenever a "numeric" column misbehaves.

## ------------------------------------------------------------------
## 4. Reading summary(lm()) on real data
## ------------------------------------------------------------------
a <- read.table("roe01.txt", header = TRUE)   # 500 firm-years
str(a)                                          # all numeric -- good
round(head(a, 5), 3)

fit <- lm(ROE ~ ROEt, data = a)   # next year's ROE on this year's ROE
summary(fit)                      # estimate | std.error | t value | Pr(>|t|), plus R^2, F
# A fit is just a list:
class(fit)
coef(fit)                         # pull the two numbers we need

## ------------------------------------------------------------------
## 5. Map coefficients -> fitted equation -> predict one value BY HAND
## ------------------------------------------------------------------
b0 <- coef(fit)[1]; b1 <- coef(fit)[2]
cat(sprintf("Fitted model:  ROE_hat = %.4f + %.4f * ROEt\n", b0, b1))

# Predict ROE for a firm with ROEt = 1.0, by hand:
roet_new <- 1.0
by_hand <- b0 + b1 * roet_new
cat(sprintf("By hand at ROEt=%.1f:  %.4f + %.4f*%.1f = %.4f\n",
            roet_new, b0, b1, roet_new, by_hand))

# Confirm against predict():
from_R <- predict(fit, newdata = data.frame(ROEt = roet_new))
cat(sprintf("predict() says: %.4f   (match: %s)\n",
            from_R, isTRUE(all.equal(unname(by_hand), unname(from_R)))))

## ------------------------------------------------------------------
## 6. Bridge to the "break it" demo
## ------------------------------------------------------------------
# Now open ../../homeworks/flawed_r_overview.R -- the AI's "average return of
# stock A" reproduces exactly the factor trap from section 3. Have the class
# find it before you reveal the fix.
