# MA376 Lesson 24: Mixed-effects logistic regression
#
# Run this top to bottom with the board sheet beside you. Each section is one part of the sheet:
#   1  the IOCT data: pass or fail, 36 companies, 4 to 120 cadets each      (Which Company Is Best?)
#   2  fixed company effect: glm(pass ~ company)                             (questions 1 and 3)
#   3  random company effect: glmer(pass ~ 1 + (1 | company))                (questions 2 and 3)
#   4  how much each company shrinks: the blend and lambda                   (questions 4 to 7)
#   5  rifle qualification: fixed AND random effects in one model            (questions 8 to 14)
# kable() (from the knitr package) prints each result as a small labeled table. glmer() is in the lme4 package.
# If R says there's no package called knitr or lme4, run install.packages(c("knitr", "lme4")) once.
library(knitr)
library(lme4)

# The data files. This reads them straight from the course page. If you saved them to a data folder
# next to this script, change this line to:  where <- "data/"
where <- "https://dustysturner.com/westpoint/MA376-AY27-1/data/"

# ============================================================================
#  1. THE IOCT DATA
#  Indoor Obstacle Course Test, same 36 companies and company sizes as Lesson 23. One row per cadet.
#  company: A1 to I4.  pass: 1 = passed, 0 = failed.
#  The commander's question: which company is best at the IOCT?
# ============================================================================
ioct <- read.csv(paste0(where, "L24_ioct_companies.csv"))
head(ioct)
mean(ioct$pass)                                  # share of all cadets who passed

# One row per company: cadets tested, cadets passed, pass rate.
co <- data.frame(n    = as.vector(table(ioct$company)),
                 k    = as.vector(tapply(ioct$pass, ioct$company, sum)),
                 row.names = names(table(ioct$company)))
co$rate <- co$k / co$n
kable(head(co[order(-co$rate, -co$n), ], 6), digits = 3, caption = "Top six raw pass rates")

# ============================================================================
#  2. FIXED COMPANY EFFECT
#  log(p / (1 - p)) = b0 + b_j. Lesson 9's logistic regression with one dummy per company.
#  b0 is company A1's log-odds (the baseline). b_j is how far company j sits from A1.
# ============================================================================
fitF <- glm(pass ~ company, family = binomial, data = ioct)
round(summary(fitF)$coefficients[c("(Intercept)", "companyA2", "companyG4", "companyH3"), ], 3)
# H3 and A2 passed everyone. Look at their estimates and standard errors (question 1).
predict(fitF, data.frame(company = c("G4", "H3", "A2")), type = "response")

# ============================================================================
#  3. RANDOM COMPANY EFFECT
#  log(p / (1 - p)) = mu + u_j, u_j ~ N(0, sigma_u^2). Lesson 23's random intercept on the log-odds scale.
#  glmer() is lmer() plus a family, the way glm() is lm() plus a family.
# ============================================================================
fitR <- glmer(pass ~ 1 + (1 | company), family = binomial, data = ioct)
summary(fitR)                                    # no Residual row (question 2)
mu  <- fixef(fitR)[[1]]                          # log-odds for an average company
s2u <- as.numeric(VarCorr(fitR)$company)         # sigma_u^2: how much companies differ
ranef(fitR)$company[c("G4", "H3", "A2"), , drop = FALSE]   # each u-hat, in log-odds
predict(fitR, data.frame(company = c("G4", "H3", "A2")), type = "response")

# Raw rate against the model, every company. Small companies move the most (question 3).
co$model <- predict(fitR, data.frame(company = rownames(co)), type = "response")
kable(head(co[order(-co$model), ], 5), digits = 3, caption = "Top five companies by glmer")

# ============================================================================
#  4. HOW MUCH DOES EACH COMPANY SHRINK?
#  The blend: p-hat_j = p-bar + w_j (k_j / n_j - p-bar), w_j = n_j / (n_j + lambda),
#  lambda = noise / signal = p-bar(1 - p-bar) / (sigma_u^2 [p-bar(1 - p-bar)]^2).
#  It lands within 1.7 points of glmer for every company.
# ============================================================================
pbar   <- plogis(mu)                             # turn mu-hat's log-odds into a pass rate
noise  <- pbar * (1 - pbar)                      # one cadet's pass/fail variance
signal <- s2u * noise^2                          # how much companies differ, on the pass-rate scale
lam    <- noise / signal
c(pbar = pbar, lambda = lam)

co$w     <- co$n / (co$n + lam)
co$blend <- pbar + co$w * (co$rate - pbar)
kable(co[c("H3", "A2", "G4", "B2"), ], digits = 3, caption = "Blend against glmer")

# Question 5: two new companies, 9 of 10 and 90 of 100. Fill in n and k and run.
n <- 10; k <- 9
pbar + n / (n + lam) * (k / n - pbar)

# ============================================================================
#  5. RIFLE QUALIFICATION
#  One row per round fired. 48 shooters; most fired 40 rounds, 6 fired 10 before lightning closed the range.
#  shooter: S01 to S48.  position: prone supported, prone unsupported, kneeling.
#  distance: target distance, 50 to 300 m.  hit: 1 = hit, 0 = miss.
#  The OIC's questions: how much do distance and position matter, and who are the best shooters?
# ============================================================================
rifle <- read.csv(paste0(where, "L24_rifle_qual.csv"))
rifle$position <- factor(rifle$position, levels = c("prone supported", "prone unsupported", "kneeling"))
rifle$dist100  <- rifle$distance / 100           # distance in hundreds of meters
head(rifle)
table(table(rifle$shooter))                      # rounds fired: how many shooters fired each count

# Question 8: distance and position fixed, shooter random. A mixed model.
fit_rifle <- glmer(hit ~ dist100 + position + (1 | shooter), family = binomial, data = rifle)
summary(fit_rifle)
exp(fixef(fit_rifle))                            # odds ratios (question 9)

r_s2u <- as.numeric(VarCorr(fit_rifle)$shooter)
r_s2u / (r_s2u + pi^2 / 3)                       # latent ICC (question 10)

# Each shooter's u-hat beside their record (questions 11 and 12).
rs <- data.frame(u_hat  = ranef(fit_rifle)$shooter[, 1],
                 rounds = as.vector(table(rifle$shooter)[rownames(ranef(fit_rifle)$shooter)]),
                 hits   = as.vector(tapply(rifle$hit, rifle$shooter, sum)[rownames(ranef(fit_rifle)$shooter)]),
                 avg_distance = as.vector(tapply(rifle$distance, rifle$shooter, mean)[rownames(ranef(fit_rifle)$shooter)]),
                 row.names = rownames(ranef(fit_rifle)$shooter))
kable(head(rs[order(-rs$u_hat), ], 5), digits = 3, caption = "Top five shooters by u-hat")

# Question 13: ignore the shooter and compare.
fit_glm <- glm(hit ~ dist100 + position, family = binomial, data = rifle)
round(cbind(glm = coef(fit_glm), glmer = fixef(fit_rifle)), 3)

# Predict a hit at 300 m kneeling: a new (average) shooter, and S22.
new <- data.frame(shooter = c("new shooter", "S22"), dist100 = 3,
                  position = factor("kneeling", levels = levels(rifle$position)))
predict(fit_rifle, new, type = "response", allow.new.levels = TRUE)
