# MA376 Lesson 26: Model competition starter code
#
# Sections 1 to 5 run top to bottom. They build model type 1 (logistic regression, cut at 0.5),
# score it on leads it never saw, and write the upload file. Sections 6 to 10 are your start
# point for model types 2 to 6: fill in the blanks.
#   1  the leads: 12,000 worked leads (outcome known), 3,900 incoming (no outcome)
#   2  split the worked leads: train on 70%, check on 30%
#   3  model type 1: logistic regression, cut at 0.5
#   4  score it like the board: F1, accuracy, sensitivity, specificity, precision, calls, caught
#   5  refit on all 12,000 and write the upload file
#   6 to 10  model types 2 to 6, a start point: put your predictors where you see ___
# Model types: 1 logistic (cut 0.5), 2 logistic your cut (Fix 1), 3 class weights (Fix 2),
#              4 undersampled (Fix 3), 5 mixed-effects logistic, 6 random forest
# Install anything you don't have, then load it.
pkgs <- c("tidyverse", "yardstick", "lme4", "randomForest")
install.packages(setdiff(pkgs, rownames(installed.packages())))

library(tidyverse)
library(yardstick)
library(lme4)
library(randomForest)

# Both Memo 4 files, downloaded from Canvas into the same folder as this script.
# In RStudio: Session > Set Working Directory > To Source File Location.

# ============================================================================
#  1. THE LEADS
#  enlisted: 1 = signed a contract, 0 = did not. Only the worked leads have it.
# ============================================================================
# Words become factors so every model (the forest too) can use them.
leads    <- read_csv("ma376_leads_m4.csv", show_col_types = FALSE) |>
  mutate(across(where(is.character), factor), enlisted = factor(enlisted))
incoming <- read_csv("ma376_leads_score.csv", show_col_types = FALSE) |>
  mutate(across(where(is.character), factor))

head(leads) |> print(width = Inf)
leads |> count(enlisted) |> mutate(share = n / sum(n))

# ============================================================================
#  2. SPLIT THE WORKED LEADS
#  The board only scores incoming leads, and you get 5 uploads per model type a day.
#  Check your models here first.
# ============================================================================
set.seed(376)
leads <- leads |> mutate(set = if_else(runif(n()) < 0.7, "train", "test"))
train <- leads |> filter(set == "train") |> select(-set)
test  <- leads |> filter(set == "test")  |> select(-set)

# ============================================================================
#  3. MODEL TYPE 1: LOGISTIC REGRESSION, CUT AT 0.5
#  station is left out: 48 towns would be 47 dummies. Lesson 24 has a better idea.
# ============================================================================
fit_logit <- glm(enlisted ~ market_type + propensity + lead_source + education + age +
                   distance_miles + social_media,
                 family = binomial, data = train)
summary(fit_logit)

results_logit <- test |>
  mutate(prob = predict(fit_logit, test, type = "response"),
         call = factor(if_else(prob > 0.5, 1, 0), levels = c(0, 1)))

results_logit |> summarise(max_prob = max(prob), calls = sum(call == "1"))

# ============================================================================
#  4. SCORE IT LIKE THE BOARD
#  F1 is the score. The rest are columns you can sort the board by.
# ============================================================================
conf_mat(results_logit, truth = enlisted, estimate = call)

lead_metrics <- metric_set(f_meas, accuracy, sensitivity, specificity, precision)

results_logit |>
  lead_metrics(truth = enlisted, estimate = call, event_level = "second")
# F1 is NA when you call nobody: precision is 0/0. The board scores it 0.

results_logit |>
  summarise(calls = sum(call == "1"),                        # the recruiters can make 1,230 a week
            caught = sum(call == "1" & enlisted == "1"))

# ============================================================================
#  5. REFIT ON ALL 12,000 AND WRITE THE UPLOAD FILE
#  Three columns: lead_id, probability, call (1 = a recruiter calls this lead).
# ============================================================================
fit_final <- glm(enlisted ~ market_type + propensity + lead_source + education + age +
                   distance_miles + social_media,
                 family = binomial, data = leads)

upload <- incoming |>
  mutate(probability = predict(fit_final, incoming, type = "response"),
         call = if_else(probability > 0.5, 1, 0)) |>
  select(lead_id, probability, call)

head(upload)
nrow(upload)   # must be 3900
write_csv(upload, "team_XX_type1.csv")   # put your team code in place of XX; upload as type 1
# Upload at https://dustydell.tail280d22.ts.net:8443/ma376

# Each type below runs start to finish, the same five steps as type 1: fit on train, predict test,
# score it, refit on all 12,000, predict the incoming leads and write the upload.
# Fill in the blanks, run it, upload it, then make it better.
# ============================================================================
#  6. TYPE 2: LOGISTIC, YOUR CUT (FIX 1)
# ============================================================================
# Pick your predictors. Tune the cut on F1 by five-fold cross-validation on train, as in Lesson 21.
# Never tune on test.
cuts <- seq(0.01, 0.10, by = 0.005)                  # the cuts to try
set.seed(11)
fold <- sample(rep(1:5, length.out = nrow(train)))   # each training lead gets a fold
p_cv <- numeric(nrow(train))
for (k in 1:5) {
  f <- glm(enlisted ~ ___,
           family = binomial, data = train[fold != k, ])                  # fit on four folds
  p_cv[fold == k] <- predict(f, train[fold == k, ], type = "response")    # predict the fifth
}

f1 <- sapply(cuts, function(t) {
  call <- p_cv > t
  2 * sum(call & train$enlisted == "1") / (sum(call) + sum(train$enlisted == "1"))
})
best_cut <- cuts[which.max(f1)]
best_cut

fit_cut <- glm(enlisted ~ ___,
               family = binomial, data = train)

results_cut <- test |>
  mutate(prob = predict(fit_cut, test, type = "response"),
         call = factor(if_else(prob > best_cut, 1, 0), levels = c(0, 1)))

results_cut |> conf_mat(truth = enlisted, estimate = call)
results_cut |> lead_metrics(truth = enlisted, estimate = call, event_level = "second")

fit_cut_final <- glm(enlisted ~ ___,
                     family = binomial, data = leads)

upload <- incoming |>
  mutate(probability = predict(fit_cut_final, incoming, type = "response"),
         call = if_else(probability > best_cut, 1, 0)) |>
  select(lead_id, probability, call)

sum(upload$call)
write_csv(upload, "team_XX_type2.csv")

# The cut is a choice. Best F1 is one reason. 1,230 calls is another.
# ============================================================================
#  7. TYPE 3: LOGISTIC, CLASS WEIGHTS (FIX 2)
# ============================================================================
# Each training enlistee counts as n0 / n1 leads, as in Lesson 21. Cut at 0.5.
w <- ifelse(train$enlisted == "1", sum(train$enlisted == "0") / sum(train$enlisted == "1"), 1)
fit_weighted <- glm(enlisted ~ ___,
                    family = binomial, data = train, weights = w)
# R warns about "non-integer #successes" because the weights aren't whole numbers. We mean it.

results_weighted <- test |>
  mutate(prob = predict(fit_weighted, test, type = "response"),
         call = factor(if_else(prob > 0.5, 1, 0), levels = c(0, 1)))

results_weighted |> conf_mat(truth = enlisted, estimate = call)
results_weighted |> lead_metrics(truth = enlisted, estimate = call, event_level = "second")

w_all <- ifelse(leads$enlisted == "1", sum(leads$enlisted == "0") / sum(leads$enlisted == "1"), 1)
fit_weighted_final <- glm(enlisted ~ ___,
                          family = binomial, data = leads, weights = w_all)

upload <- incoming |>
  mutate(probability = predict(fit_weighted_final, incoming, type = "response"),
         call = if_else(probability > 0.5, 1, 0)) |>
  select(lead_id, probability, call)

sum(upload$call)
write_csv(upload, "team_XX_type3.csv")

# How many does it call at 0.5? Compare to 1,230.
# ============================================================================
#  8. TYPE 4: LOGISTIC, UNDERSAMPLED (FIX 3)
# ============================================================================
# In train, keep every enlistee and the same number of leads that didn't enlist, drawn at random, as
# in Lesson 21. Cut at 0.5.
set.seed(376)
keep <- c(which(train$enlisted == "1"), sample(which(train$enlisted == "0"), sum(train$enlisted == "1")))
fit_under <- glm(enlisted ~ ___,
                 family = binomial, data = train[keep, ])

results_under <- test |>
  mutate(prob = predict(fit_under, test, type = "response"),
         call = factor(if_else(prob > 0.5, 1, 0), levels = c(0, 1)))

results_under |> conf_mat(truth = enlisted, estimate = call)
results_under |> lead_metrics(truth = enlisted, estimate = call, event_level = "second")

set.seed(376)
keep_all <- c(which(leads$enlisted == "1"), sample(which(leads$enlisted == "0"), sum(leads$enlisted == "1")))
fit_under_final <- glm(enlisted ~ ___,
                       family = binomial, data = leads[keep_all, ])

upload <- incoming |>
  mutate(probability = predict(fit_under_final, incoming, type = "response"),
         call = if_else(probability > 0.5, 1, 0)) |>
  select(lead_id, probability, call)

sum(upload$call)
write_csv(upload, "team_XX_type4.csv")

# ============================================================================
#  9. TYPE 5: MIXED-EFFECTS LOGISTIC (LESSON 24)
# ============================================================================
# Your predictors plus a random intercept for each station, fit with glmer as in Lesson 24. A 0.5 cut
# calls nobody again, so use type 2's cut.
fit_glmer <- glmer(enlisted ~ ___ + (1 | station),
                   family = binomial, data = train)
summary(fit_glmer)

results_glmer <- test |>
  mutate(prob = predict(fit_glmer, test, type = "response", allow.new.levels = TRUE),
         call = factor(if_else(prob > best_cut, 1, 0), levels = c(0, 1)))

results_glmer |> conf_mat(truth = enlisted, estimate = call)
results_glmer |> lead_metrics(truth = enlisted, estimate = call, event_level = "second")

fit_glmer_final <- glmer(enlisted ~ ___ + (1 | station),
                         family = binomial, data = leads)

upload <- incoming |>
  mutate(probability = predict(fit_glmer_final, incoming, type = "response", allow.new.levels = TRUE),
         call = if_else(probability > best_cut, 1, 0)) |>
  select(lead_id, probability, call)

sum(upload$call)
write_csv(upload, "team_XX_type5.csv")

# Look at the station variance. What is it telling you?
# Its own cut: run type 2's loop with this glmer in place of glm. Five glmer fits take a minute or
# two.
# If glmer warns about converging or rescaling, that's the size of propensity and distance_miles.
# Try scale().
# allow.new.levels = TRUE: a station with no worked leads gets the average station.
# ============================================================================
#  10. TYPE 6: RANDOM FOREST (LESSON 25)
# ============================================================================
# Fit the forest as in Lesson 25. A majority vote calls nobody here, so cut the share of trees that
# vote "enlisted." Tune the cut on the out-of-bag votes, never on test.
set.seed(376)
fit_rf <- randomForest(enlisted ~ ___,
                       data = train,
                       ntree = ___,       # how many trees
                       mtry = ___,        # predictors each split may look at
                       nodesize = ___)    # smallest box a tree can make
fit_rf

p_oob <- fit_rf$votes[, "1"]   # each training lead, voted on only by trees that never saw it

f1_rf <- sapply(cuts, function(t) {
  call <- p_oob > t
  2 * sum(call & train$enlisted == "1") / (sum(call) + sum(train$enlisted == "1"))
})
best_rf <- cuts[which.max(f1_rf)]
best_rf

results_rf <- test |>
  mutate(prob = predict(fit_rf, test, type = "prob")[, "1"],
         call = factor(if_else(prob > best_rf, 1, 0), levels = c(0, 1)))

results_rf |> conf_mat(truth = enlisted, estimate = call)
results_rf |> lead_metrics(truth = enlisted, estimate = call, event_level = "second")

set.seed(376)
fit_rf_final <- randomForest(enlisted ~ ___,
                             data = leads, ntree = ___, mtry = ___, nodesize = ___)

upload <- incoming |>
  mutate(probability = predict(fit_rf_final, incoming, type = "prob")[, "1"],
         call = if_else(probability > best_rf, 1, 0)) |>
  select(lead_id, probability, call)

sum(upload$call)
write_csv(upload, "team_XX_type6.csv")

# Tune ntree, mtry, nodesize with Lesson 25's grid. Check the winner on test before you upload:
# with this few enlistees, the best OOB score can be luck.
