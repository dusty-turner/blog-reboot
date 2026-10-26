# MA376 Lesson 25: Trees and random forest
#
# Run this top to bottom with the board sheet beside you. Each section is one part of the sheet:
#   1  the IOCT data: pass or fail, one row per cadet, train (last year) and test (this year)
#   2  what we know: logistic regression                                    (question 1)
#   3  one split, and where it comes from: Gini impurity at every cut       (questions 2 and 3)
#   4  grow the tree: two splits, three splits, no limits, the limited tree (questions 4 to 6)
#   5  so what's the downside? two new train/test splits                    (question 7)
#   6  random forest, and tuning it                                         (questions 8 to 10)
#   7  so what do I make of it? variable importance                         (questions 11 and 12)
# If R says there's no package called something, run once:
#   install.packages(c("tidyverse", "yardstick", "rpart", "rpart.plot", "randomForest"))
library(tidyverse)
library(yardstick)
library(rpart)
library(rpart.plot)
library(randomForest)

# The data file. This reads it straight from the course page. If you saved it to a data folder
# next to this script, change this line to:  where <- "data/"
where <- "https://dustysturner.com/westpoint/MA376-AY27-1/data/"

# ============================================================================
#  1. THE IOCT DATA
#  One row per cadet. pass: 1 = passed the IOCT, 0 = failed.
#  set: train (last year, 2,100 cadets) or test (this year, 900 cadets).
#  Goal: predict which of this year's cadets will pass.
# ============================================================================
ioct <- read_csv(paste0(where, "L25_ioct_cadets.csv"), show_col_types = FALSE) |>
  mutate(pass = factor(pass), class_year = factor(class_year))
head(ioct)

train <- ioct |> filter(set == "train") |> select(-cadet, -set)
test  <- ioct |> filter(set == "test")  |> select(-cadet, -set)
train |> count(pass)

# Lesson 20's metrics. event_level = "second": 1 is the second level of pass, the one we predict.
ioct_metrics <- metric_set(accuracy, sensitivity, precision, f_meas)

# ============================================================================
#  2. WHAT WE KNOW: LOGISTIC REGRESSION
#  Fit on train, score on test. The scoreboard grows one row per model.
# ============================================================================
fit_logit <- glm(pass ~ ., family = binomial, data = train)

results_logit <- test |>
  mutate(prob = predict(fit_logit, test, type = "response"),
         pred = factor(if_else(prob > 0.5, 1, 0), levels = c(0, 1)))
results_logit |> conf_mat(truth = pass, estimate = pred)

scoreboard <- results_logit |>
  ioct_metrics(truth = pass, estimate = pred, event_level = "second") |>
  mutate(model = "logistic")
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

coef(fit_logit) |> round(3)                      # one slope per predictor (question 1)

# ============================================================================
#  3. ONE SPLIT
#  Each box: its prediction (1 = pass, 0 = fail), the share who passed, the share of cadets in the box.
#  Gini impurity of a box where a share p passed: G = 2p(1 - p). 0 is pure, 0.5 is half and half.
#  The tree tries every cut of every predictor and keeps the one with the lowest G after the split.
# ============================================================================
fit_stump <- rpart(pass ~ ., data = train, method = "class", maxdepth = 1)   # one split
rpart.plot(fit_stump, extra = 106)

results_stump <- test |> mutate(pred = predict(fit_stump, test, type = "class"))
scoreboard <- scoreboard |>
  bind_rows(results_stump |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "one split"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# G after splitting at a cut: the two boxes' G, weighted by how many cadets land in each.
g_after <- function(d, v, cut) {
  left <- d[[v]] < cut
  if (sum(left) < 7 || sum(!left) < 7) return(NA)   # rpart keeps at least 7 cadets on each side
  gini <- function(p) 2 * p * (1 - p)
  (sum(left) * gini(mean(d$pass[left] == "1")) + sum(!left) * gini(mean(d$pass[!left] == "1"))) / nrow(d)
}

# Every cut of every numeric predictor, and each predictor's best cut (questions 2 and 3).
g_curves <- function(d) {
  d |>
    select(height_in, weight_lb, pullups, run_min, plank_sec) |>
    pivot_longer(everything(), names_to = "predictor", values_to = "x") |>
    distinct() |> arrange(predictor, x) |> group_by(predictor) |>
    mutate(cut = (x + lead(x)) / 2) |> filter(!is.na(cut)) |> ungroup() |>
    mutate(G = map2_dbl(predictor, cut, \(v, c) g_after(d, v, c))) |>
    filter(!is.na(G))
}
cuts <- g_curves(train)
cuts |> group_by(predictor) |> slice_min(G, n = 1, with_ties = FALSE) |> arrange(G)

ggplot(cuts, aes(cut, G)) +
  geom_line() +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(x = "cut point", y = "Gini impurity after the split, G")

# ============================================================================
#  4. GROW THE TREE
#  The second split: the same search, inside each box (question 4).
# ============================================================================
bind_rows(`6 or fewer pull-ups` = g_curves(filter(train, pullups <= 6)),
          `7 or more pull-ups`  = g_curves(filter(train, pullups >= 7)), .id = "box") |>
  group_by(box, predictor) |> slice_min(G, n = 1, with_ties = FALSE) |> arrange(box, G)

fit_two <- rpart(pass ~ ., data = train, method = "class", maxdepth = 2)
rpart.plot(fit_two, extra = 106)
results_two <- test |> mutate(pred = predict(fit_two, test, type = "class"))
scoreboard <- scoreboard |>
  bind_rows(results_two |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "two splits"))

fit_three <- rpart(pass ~ ., data = train, method = "class", maxdepth = 3)
rpart.plot(fit_three, extra = 106)
results_three <- test |> mutate(pred = predict(fit_three, test, type = "class"))
scoreboard <- scoreboard |>
  bind_rows(results_three |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "three splits"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# Let it run: no limits (question 5).
fit_big <- rpart(pass ~ ., data = train, method = "class", cp = 0, minsplit = 2)
sum(fit_big$frame$var != "<leaf>")               # number of splits
train |> mutate(pred = predict(fit_big, train, type = "class")) |> accuracy(truth = pass, estimate = pred)
results_big <- test |> mutate(pred = predict(fit_big, test, type = "class"))
scoreboard <- scoreboard |>
  bind_rows(results_big |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "no limits"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# Limiting the tree: rpart's defaults, cp = 0.01, minsplit = 20, maxdepth = 30 (question 6).
fit_tree <- rpart(pass ~ ., data = train, method = "class")
rpart.plot(fit_tree, extra = 106)
results_tree <- test |> mutate(pred = predict(fit_tree, test, type = "class"))
scoreboard <- scoreboard |>
  bind_rows(results_tree |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "limited tree"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# ============================================================================
#  5. SO WHAT'S THE DOWNSIDE?
#  Same cadets, two new random 70/30 splits. Refit everything (question 7).
# ============================================================================
make_split <- function(seed) {
  set.seed(seed)
  new_train <- ioct |> slice_sample(prop = 0.7)
  new_test  <- ioct |> anti_join(new_train, by = "cadet")
  list(train = new_train |> select(-cadet, -set), test = new_test |> select(-cadet, -set))
}
split_a <- make_split(2)
split_b <- make_split(4)

par(mfrow = c(1, 2))
rpart(pass ~ ., data = split_a$train, method = "class") |> rpart.plot(extra = 106, main = "Split A")
rpart(pass ~ ., data = split_b$train, method = "class") |> rpart.plot(extra = 106, main = "Split B")
par(mfrow = c(1, 1))

trees <- list(`one split`    = rpart.control(maxdepth = 1),
              `two splits`   = rpart.control(maxdepth = 2),
              `three splits` = rpart.control(maxdepth = 3),
              `no limits`    = rpart.control(cp = 0, minsplit = 2),
              `limited tree` = rpart.control())
splits <- list(original = list(train = train, test = test), `split A` = split_a, `split B` = split_b)

score_split <- function(m, s) {
  tr <- splits[[s]]$train; te <- splits[[s]]$test
  pred <- if (m == "logistic") {
    fit <- glm(pass ~ ., family = binomial, data = tr)
    factor(if_else(predict(fit, te, type = "response") > 0.5, 1, 0), levels = c(0, 1))
  } else {
    predict(rpart(pass ~ ., data = tr, method = "class", control = trees[[m]]), te, type = "class")
  }
  f_meas_vec(te$pass, pred, event_level = "second")
}
expand_grid(model = c("logistic", names(trees)), split = names(splits)) |>
  mutate(f_meas = map2_dbl(model, split, score_split)) |>
  pivot_wider(names_from = split, values_from = f_meas)

# ============================================================================
#  6. RANDOM FOREST
#  500 trees. Each grows on a bootstrap sample of the cadets, sees only mtry predictors at each split,
#  and grows with no limits. Every tree votes; the majority wins (questions 8 and 9).
# ============================================================================
set.seed(376)
fit_rf <- randomForest(pass ~ ., data = train,
                       ntree = 500,       # how many trees
                       mtry = 2,          # predictors each split may look at
                       nodesize = 1)      # smallest box a tree can make
fit_rf                                           # OOB error; this confusion matrix has truth on the rows

results_rf <- test |> mutate(pred = predict(fit_rf, test))
scoreboard <- scoreboard |>
  bind_rows(results_rf |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "forest"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# Tune all three knobs on OOB F1. Never on the test set. About 30 seconds (question 10).
rf_grid <- expand_grid(mtry = 1:6, ntree = c(100, 300, 500), nodesize = c(1, 5, 20)) |>
  mutate(oob_f1 = pmap_dbl(list(mtry, ntree, nodesize), \(m, n, k) {
    set.seed(376)
    rf <- randomForest(pass ~ ., data = train, mtry = m, ntree = n, nodesize = k)
    f_meas_vec(train$pass, rf$predicted, event_level = "second")   # rf$predicted: the OOB predictions
  }))
rf_grid |> arrange(desc(oob_f1))

best <- rf_grid |> slice_max(oob_f1, n = 1)
set.seed(376)
fit_best <- randomForest(pass ~ ., data = train,
                         ntree = best$ntree, mtry = best$mtry, nodesize = best$nodesize,
                         importance = TRUE)
results_best <- test |> mutate(pred = predict(fit_best, test))
scoreboard <- scoreboard |>
  bind_rows(results_best |> ioct_metrics(truth = pass, estimate = pred, event_level = "second") |> mutate(model = "tuned forest"))
scoreboard |> select(model, .metric, .estimate) |> pivot_wider(names_from = .metric, values_from = .estimate)

# ============================================================================
#  7. SO WHAT DO I MAKE OF IT?
#  Variable importance: scramble one predictor among the OOB cadets; how much worse does the forest do?
#  (questions 11 and 12)
# ============================================================================
importance(fit_best, type = 1) |>                # type = 1: the scramble test, mean decrease in accuracy
  as_tibble(rownames = "predictor") |>
  arrange(desc(MeanDecreaseAccuracy))
varImpPlot(fit_best, type = 1, main = "")
