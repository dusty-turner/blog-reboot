# MA376 Lesson 23: Fixed, random and mixed effects
#
# Run this top to bottom with the lesson page or the board sheet beside you. Each section is one part of the lesson:
#   1  the AFT data: 36 companies, 4 to 120 cadets each               (The AFT Data)
#   2  Model 1: no company effect, lm(score ~ 1)                       (Model 1)
#   3  Model 2: fixed company effect, lm(score ~ company)              (Model 2)
#   4  Model 3: random company effect, lmer(score ~ 1 + (1 | company)) (Model 3)
#   5  shrinkage: Model 2's ranking against Model 3's                  (Shrinkage)
#   6  side by side                                                    (Side by Side)
#   7  the ICC                                                         (The ICC)
#   8  tell the commander                                              (Tell the Commander)
#   9  fixed or random?                                                (Fixed or Random?)
#  10  practice: radon in Minnesota, 85 counties                      (Practice)
# kable() (from the knitr package) prints each result as a small labeled table. lmer() is in the lme4 package.
# If R says there's no package called knitr or lme4, run install.packages(c("knitr", "lme4")) once.
library(knitr)
library(lme4)

# The data file. This reads it straight from the course page. If you saved it to a data folder
# next to this script, change this line to:  where <- "data/"
where <- "https://dustysturner.com/westpoint/MA376-AY27-1/data/"

# ============================================================================
#  1. THE AFT DATA
#  Fall Army Fitness Test results reported so far. One row per cadet.
#  company: A1 to I4.  score: AFT total, 0 to 500.
#  Some companies have tested most cadets, some only a handful.
#  The commander's question: which company is the most fit?
# ============================================================================
aft <- read.csv(paste0(where, "L23_aft_companies.csv"))
head(aft)
nrow(aft)                                        # cadets
table(aft$company)                               # cadets tested per company

# One row per company: how many cadets, and their average score.
co <- data.frame(n   = as.vector(table(aft$company)),
                 avg = as.vector(tapply(aft$score, aft$company, mean)),
                 row.names = names(table(aft$company)))
co <- co[order(co$avg), ]
plot(seq_len(nrow(co)), co$avg, pch = 19, cex = 0.6 + 2.2 * sqrt(co$n / max(co$n)),
     col = ifelse(co$n <= 6, "#c25708", "#1f5fa8"), xaxt = "n", las = 1,
     xlab = "", ylab = "company average AFT score", main = "Dot size = cadets tested")
axis(1, at = seq_len(nrow(co)), labels = rownames(co), las = 2, cex.axis = 0.7)
abline(h = mean(aft$score), col = "grey", lty = 2)
kable(head(co[order(-co$avg), ], 5), digits = 1, caption = "Top five company averages")

# ============================================================================
#  2. MODEL 1: NO COMPANY EFFECT
#  y_ij = mu + e_ij. One overall average. Every company gets the same prediction.
# ============================================================================
fit1 <- lm(score ~ 1, data = aft)
summary(fit1)
predict(fit1, data.frame(company = c("D3", "H2", "J5")))   # J5 is a new company

# ============================================================================
#  3. MODEL 2: FIXED COMPANY EFFECT
#  y_ij = b0 + b_j + e_ij. Lesson 13: ANOVA is regression with indicators.
#  b0 is company A1's average (the baseline). b_j is how far company j sits from A1.
# ============================================================================
fit2 <- lm(score ~ company, data = aft)
summary(fit2)
predict(fit2, data.frame(company = c("D3", "H2")))
co[c("D3", "H2"), ]                              # each prediction is just the company's own average
try(predict(fit2, data.frame(company = "J5")))   # J5 is a new company: no b_J5, so no prediction

# ============================================================================
#  4. MODEL 3: RANDOM COMPANY EFFECT
#  y_ij = mu + u_j + e_ij,  u_j ~ N(0, sigma_u^2),  e_ij ~ N(0, sigma^2)
#  lmer: linear mixed-effects regression. (1 | company) is an intercept for each company,
#  drawn from one normal distribution.
# ============================================================================
fit3 <- lmer(score ~ 1 + (1 | company), data = aft)
summary(fit3)

mu   <- fixef(fit3)[["(Intercept)"]]            # Fixed effects, (Intercept)
vc   <- as.data.frame(VarCorr(fit3))
s2_u <- vc$vcov[vc$grp == "company"]             # Random effects, company:  how much companies really differ
s2   <- vc$vcov[vc$grp == "Residual"]            # Random effects, Residual: how much cadets vary inside a company
kable(data.frame(mu, s2_u, s2), digits = 1, caption = "From summary(fit3)")

# The Model 3 formula:
#   y-hat = mu-hat + u-hat_j = mu-hat + w_j (ybar_j - mu-hat)
#   the overall average plus a share w_j of the company's gap, w_j between 0 (Model 1) and 1 (Model 2)
#   w_j    = n_j / (n_j + lambda)                    more cadets, more trust
#          = sigma_u^2 / (sigma_u^2 + sigma^2 / n_j)  the same w: real spread over real spread plus luck
#   lambda = sigma^2 / sigma_u^2                     big when cadets are noisy or companies barely differ
lambda <- s2 / s2_u
lambda                                           # n_j = lambda gives w_j = 1/2

co$w     <- co$n / (co$n + lambda)
co$u_hat <- co$w * (co$avg - mu)
co$y_hat <- mu + co$u_hat
kable(co[c("D3", "H2"), ], digits = 2, caption = "The Model 3 formula, filled in")
# A new company, J5: no cadets, so no gap to trust. u-hat = 0 and y-hat = mu-hat.

# Why shrink? Luck in a small company's average against how much companies really differ.
sqrt(s2 / co["D3", "n"])                         # D3: points of luck in a 4-cadet average
sqrt(s2_u)                                       # how much companies really differ
s2_u / (s2_u + s2 / co["D3", "n"])               # the same w as n / (n + lambda)

# R does the same arithmetic. ranef() holds the u-hats.
fixef(fit3)                                      # mu-hat
ranef(fit3)$company[c("D3", "H2"), , drop = FALSE]   # u-hat = y-hat minus mu-hat
predict(fit3, data.frame(company = c("D3", "H2", "J5")), allow.new.levels = TRUE)   # J5 is new: allow.new.levels sets u-hat = 0 (Model 2 errors)

# ============================================================================
#  5. SHRINKAGE
#  Small companies get pulled toward the overall average. Large companies barely move.
# ============================================================================
top_raw   <- co[order(-co$avg), ][1:5, c("n", "avg")]
top_model <- co[order(-co$y_hat), ][1:5, c("n", "y_hat")]
kable(data.frame(rank = 1:5,
                 model2 = paste0(rownames(top_raw), " (", top_raw$n, ")"), estimate2 = top_raw$avg,
                 model3 = paste0(rownames(top_model), " (", top_model$n, ")"), estimate3 = top_model$y_hat),
      digits = 1, caption = "Top five: Model 2 against Model 3, company (cadets)")

small <- co$n <= 6
plot(NA, xlim = c(0.8, 2.2), ylim = range(co$avg), xaxt = "n", las = 1,
     xlab = "", ylab = "company AFT estimate", main = "Shrinkage")
axis(1, at = 1:2, labels = c("Model 2: fixed", "Model 3: random"))
abline(h = mu, col = "grey", lty = 2)
segments(1, co$avg, 2, co$y_hat, col = ifelse(small, "#c25708", "#1f5fa8"))
points(rep(1, nrow(co)), co$avg,   pch = 19, col = ifelse(small, "#c25708", "#1f5fa8"))
points(rep(2, nrow(co)), co$y_hat, pch = 19, col = ifelse(small, "#c25708", "#1f5fa8"))

# ============================================================================
#  6. SIDE BY SIDE
# ============================================================================
kable(data.frame(model = c("Model 1: no company effect", "Model 2: fixed company effect", "Model 3: random company effect"),
                 D3 = c(coef(fit1)[[1]], co["D3", "avg"], co["D3", "y_hat"]),
                 H2 = c(coef(fit1)[[1]], co["H2", "avg"], co["H2", "y_hat"]),
                 J5 = c(coef(fit1)[[1]], NA, mu)),
      digits = 1, caption = "Predicted AFT score for a cadet in each company")

# ============================================================================
#  7. THE ICC: HOW MUCH DOES COMPANY MATTER?
#  Intraclass correlation coefficient: the share of the total variance that comes from the groups.
#  It is also the correlation between two cadets in the same company.
# ============================================================================
ICC <- s2_u / (s2_u + s2)
ICC
(1 - ICC) / ICC                                  # the ICC sets the pull: same lambda as Section 4

# ============================================================================
#  8. TELL THE COMMANDER
#  How much does company matter, and which company is the most fit?
# ============================================================================
best <- rownames(co)[which.max(co$y_hat)]
kable(co[c(best, "D3"), ], digits = 1, caption = "Model 3's top company against Model 2's")

# ============================================================================
#  9. FIXED OR RANDOM?
#  Fixed: these levels are the only ones you care about (is THIS level different?).
#  Random: the levels are a sample of many (how much do levels vary?).
#  Pick fixed for few levels, every one well sampled. Pick random for many levels, or some thinly
#  sampled, even when you want to rank them.
#  Mixed model: some effects fixed, some random. Model 3 is one: mu fixed, company random.
# ============================================================================

# ============================================================================
#  10. PRACTICE: RADON IN MINNESOTA
#  Radon is a radioactive gas that seeps into basements. One row per home.
#  county: 85 Minnesota counties.  log_radon: radon on the log scale.
#  Source: Gelman and Hill (2007). Answers are on the lesson page, under Practice.
#  The state's question: which county has the worst radon?
# ============================================================================
#  This part uses the tidyverse. If R says there's no package called tidyverse, run
#  install.packages("tidyverse") once.
library(tidyverse)
radon <- read_csv(paste0(where, "L23_radon_counties.csv"), show_col_types = FALSE)
radon
radon |> count(county)                           # homes tested per county

# P1. Each county's average log_radon and number of homes. Highest? Lowest? How many homes?
#     Hint: group_by(county) |> summarise(n = n(), avg = mean(log_radon)), then arrange().

# P2. Fit Model 2, lm(log_radon ~ county). Which county does it call the worst?

# P3. Fit Model 3, lmer(log_radon ~ 1 + (1 | county)). Report mu-hat, sigma_u^2, sigma^2 and lambda.

# P4. w for Lac Qui Parle and St. Louis, both forms.

# P5. Predict a home in Lac Qui Parle, in St. Louis, and in a county never tested. By hand, then predict().

# P6. The ICC. What share of the radon variation is county to county?

# P7. Top five counties under Model 2 and Model 3. Who drops out, and why?

# P8. Tell the state in two or three sentences.

# P9. "Is Lac Qui Parle worse than Blue Earth?" Fixed or random? Why?
