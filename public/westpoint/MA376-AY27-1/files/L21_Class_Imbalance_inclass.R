# MA376 Lesson 21: Class imbalance and prevalence
#
# Run this top to bottom with the lesson page or the board sheet beside you. Each section is one part of the lesson:
#   0  the scoring function from Lesson 20
#   1  the clinic model on a test set                      (What We Did)
#   2  the same clinic when no-shows are rare              (What If the Data Looked Like This?)
#   3  three fixes: threshold, class weights, undersample  (Three Fixes)
#   4  precision at other prevalences                      (Precision Moves with Prevalence)
# kable() (from the knitr package) prints each result as a small labeled table. If R says there's no
# package called knitr, run install.packages("knitr") once.
library(knitr)

# The two data files. This reads them straight from the course page. If you saved them to a data folder
# next to this script, change this line to:  where <- "data/"
where <- "https://dustysturner.com/westpoint/MA376-AY27-1/data/"

# ============================================================================
#  0. THE SCORING FUNCTION  (from Lesson 20)
#  Every metric comes from the four cells of a confusion matrix. The positive class is a no-show.
#    tp = no-show, flagged       fn = no-show, missed
#    fp = kept, flagged anyway   tn = kept, left alone
# ============================================================================
score <- function(tp, fp, fn, tn) {
  sens <- tp / (tp + fn)                         # of the real no-shows, the share you caught
  prec <- tp / (tp + fp)                         # of the ones you flagged, the share that were real
  c(flagged     = tp + fp,
    caught      = tp,
    false_alarm = fp,
    accuracy    = (tp + tn) / (tp + fp + fn + tn),
    sensitivity = sens,
    specificity = tn / (tn + fp),                # of the kept appointments, the share you left alone
    precision   = prec,
    F1          = 2 * tp / (2 * tp + fp + fn))
}
# Give it the truth and a TRUE/FALSE flag for every appointment and it counts the four cells for you.
judge <- function(truth, flag) {
  score(sum(flag & truth == 1), sum(flag & truth == 0), sum(!flag & truth == 1), sum(!flag & truth == 0))
}

# ============================================================================
#  1. THE CLINIC MODEL  (What We Did)
#  4,000 appointments simulated from the Lesson 9 Air Force Academy clinic data.
#  lead_days is how far ahead the appointment was booked. no_show is 1 if the patient missed it.
#  The set column splits the appointments once: fit on "train", judge on "test".
# ============================================================================
clinic <- read.csv(paste0(where, "L21_clinic.csv"))
kable(head(clinic), caption = "The first six appointments")
train <- clinic[clinic$set == "train", ]
test  <- clinic[clinic$set == "test", ]
kable(data.frame(appointments = c(nrow(train), nrow(test)), no_shows = c(sum(train$no_show), sum(test$no_show)),
                 share = c(mean(train$no_show), mean(test$no_show)), row.names = c("train", "test")),
      digits = 3, caption = "How common is a no-show?")

# Look before you fit. Lead time for each outcome, then the no-show rate inside ten-day bands.
boxplot(lead_days ~ factor(no_show, 0:1, c("kept", "no-show")), data = train, horizontal = TRUE, las = 1,
        col = c("#dce9f6", "#f6dcc6"), xlab = "lead days", ylab = "", main = "Lead time by outcome")
band <- cut(train$lead_days, seq(0, 90, by = 10), right = FALSE)
barplot(tapply(train$no_show, band, mean), col = "#c25708", las = 2, ylim = c(0, 1),
        ylab = "share that were no-shows", main = "No-show rate by lead time")

fit <- glm(no_show ~ lead_days, family = binomial, data = train)
summary(fit)

p_test <- predict(fit, test, type = "response")    # each test appointment's predicted probability of a no-show
flag   <- p_test >= 0.5                            # TRUE = the model flags this appointment
kable(table(model  = factor(flag, c(TRUE, FALSE), c("predicted no-show", "predicted kept")),
            actual = factor(test$no_show, 1:0, c("no-show", "kept"))),
      caption = "Confusion matrix at 0.5, test set (the truth across the top)")
before <- judge(test$no_show, flag)
kable(t(before), digits = 3, caption = "The clinic model at 0.5, test set")
cat("The null classifier (call every appointment the larger class) scores",
    round(max(mean(test$no_show), 1 - mean(test$no_show)), 3), "\n")

# All at once. yardstick builds the same table and the same five numbers. (install.packages("yardstick") once.)
# library(yardstick)
# results <- data.frame(truth = factor(ifelse(test$no_show == 1, "no-show", "kept"), levels = c("no-show", "kept")),
#                       pred  = factor(ifelse(flag, "no-show", "kept"), levels = c("no-show", "kept")))
# conf_mat(results, truth = truth, estimate = pred)
# metric_set(accuracy, sensitivity, specificity, precision, f_meas)(results, truth = truth, estimate = pred)

# ============================================================================
#  2. WHAT IF THE DATA LOOKED LIKE THIS?
#  The same clinic, but only one appointment in ten is a no-show. Same model, same threshold.
# ============================================================================
rare  <- read.csv(paste0(where, "L21_clinic_rare.csv"))
train <- rare[rare$set == "train", ]
test  <- rare[rare$set == "test", ]
kable(data.frame(appointments = c(nrow(train), nrow(test)), no_shows = c(sum(train$no_show), sum(test$no_show)),
                 share = c(mean(train$no_show), mean(test$no_show)), row.names = c("train", "test")),
      digits = 3, caption = "Now how common is a no-show?")

plain  <- glm(no_show ~ lead_days, family = binomial, data = train)
p_test <- predict(plain, test, type = "response")
flag   <- p_test >= 0.5
kable(table(model  = factor(flag, c(TRUE, FALSE), c("predicted no-show", "predicted kept")),
            actual = factor(test$no_show, 1:0, c("no-show", "kept"))),
      caption = "Confusion matrix at 0.5, rare test set")
at_half <- judge(test$no_show, flag)
kable(rbind("Before (half are no-shows)" = before, "Now (one in ten)" = at_half), digits = 3,
      caption = "Recalculate. Which numbers are out of whack?")
cat("The null classifier (call every appointment kept) scores", round(1 - mean(test$no_show), 3), "\n")

# Every model here turns into a cutoff in lead days: flag an appointment booked this far out or more.
cutoff <- function(model, t = 0.5) unname((qlogis(t) - coef(model)[1]) / coef(model)[2])
cat("Before, the model flagged at", round(cutoff(fit), 1), "days. Now it flags at", round(cutoff(plain), 1), "days.\n")

# ============================================================================
#  3. THREE FIXES
#  Each one is fit on the training set and judged on the same test set.
# ============================================================================

# Fix 1: move the threshold. Tune it on F1 by five-fold cross-validation, the loop from Lesson 20.
ts <- seq(0.05, 0.50, by = 0.025)                     # the thresholds to try
set.seed(11)
fold <- sample(rep(1:5, length.out = nrow(train)))    # each training appointment gets a fold number 1 to 5
p_cv <- numeric(nrow(train))
for (k in 1:5) {
  f <- glm(no_show ~ lead_days, family = binomial, data = train[fold != k, ])   # fit on four folds
  p_cv[fold == k] <- predict(f, train[fold == k, ], type = "response")          # predict the fifth
}
f1 <- sapply(ts, function(t) judge(train$no_show, p_cv >= t)["F1"])
best <- ts[which.max(f1)]
plot(ts, f1, type = "b", pch = 19, las = 1, bty = "l", xlab = "threshold",
     ylab = "cross-validated F1, training set", main = paste("Best threshold:", best))
abline(v = best, lty = 2)
fix1 <- judge(test$no_show, p_test >= best)

# Fix 2: class weights. Each training no-show counts as n0 / n1 appointments, so the two classes weigh the same.
w <- ifelse(train$no_show == 1, sum(train$no_show == 0) / sum(train$no_show == 1), 1)
weighted <- glm(no_show ~ lead_days, family = binomial, data = train, weights = w)
# R warns about "non-integer #successes" because the weights aren't whole numbers. We mean it.
fix2 <- judge(test$no_show, predict(weighted, test, type = "response") >= 0.5)

# Fix 3: undersample. Keep every training no-show and an equal number of kept appointments, drawn at random.
set.seed(376)
keep  <- c(which(train$no_show == 1), sample(which(train$no_show == 0), sum(train$no_show == 1)))
under <- glm(no_show ~ lead_days, family = binomial, data = train[keep, ])
fix3 <- judge(test$no_show, predict(under, test, type = "response") >= 0.5)

side <- rbind("Plain at 0.5" = at_half, "Fix 1: tuned threshold" = fix1,
              "Fix 2: class weights" = fix2, "Fix 3: undersample" = fix3)
side <- cbind(cutoff_days = c(cutoff(plain), cutoff(plain, best), cutoff(weighted), cutoff(under)), side)
kable(side, digits = 3, caption = paste("Side by side, the same", nrow(test), "test appointments"))
# Which fix would the clinic pick? What does a missed no-show cost them, and what does a false alarm cost?

# The four models as curves. The fixes slide the curve (or the threshold) back toward where it was.
days <- seq(0, 85, by = 0.5)
crv  <- function(model) predict(model, data.frame(lead_days = days), type = "response")
matplot(days, cbind(crv(plain), crv(weighted), crv(under), crv(fit)), type = "l", lty = c(1, 1, 1, 3),
        lwd = c(3, 3, 3, 2), col = c("#b3261e", "#1f5fa8", "#c25708", "#1f2937"), las = 1, bty = "l",
        xlab = "lead days", ylab = "predicted probability of a no-show")
abline(h = c(0.5, best), lty = 2, col = "gray50")
legend("topleft", c("plain, rare data", "class weights", "undersampled", "the original data"),
       lty = c(1, 1, 1, 3), lwd = c(3, 3, 3, 2), col = c("#b3261e", "#1f5fa8", "#c25708", "#1f2937"), bty = "n")

# ============================================================================
#  4. PRECISION MOVES WITH PREVALENCE
#  Sensitivity is a share of the no-shows and specificity is a share of the kept appointments, so
#  neither depends on how common no-shows are. Precision mixes the two groups, so it does.
# ============================================================================
precision_at <- function(prev, sens, spec) sens * prev / (sens * prev + (1 - spec) * (1 - prev))
prevs <- c(0.50, 0.25, 0.10, 0.05, 0.026)
tab <- sapply(rownames(side), function(m) precision_at(prevs, side[m, "sensitivity"], side[m, "specificity"]))
rownames(tab) <- paste0(100 * prevs, "% no-shows")
kable(tab, digits = 3, caption = "Precision of each model at each prevalence")

p <- seq(0.005, 0.60, by = 0.005)
matplot(100 * p, sapply(rownames(side), function(m) precision_at(p, side[m, "sensitivity"], side[m, "specificity"])),
        type = "l", lty = 1, lwd = 3, col = c("#1f2937", "#1f5fa8", "#c25708", "#b3261e"), las = 1, bty = "l",
        xlab = "prevalence (%)", ylab = "precision", ylim = c(0, 1))
legend("bottomright", rownames(side), lty = 1, lwd = 3, col = c("#1f2937", "#1f5fa8", "#c25708", "#b3261e"), bty = "n")
# Memo 4's leads enlist 2.6% of the time. Read that row before you report a precision.
