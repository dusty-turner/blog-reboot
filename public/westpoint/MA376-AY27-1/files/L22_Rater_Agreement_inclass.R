# MA376 Lesson 22: Rater agreement and the intraclass correlation coefficient (ICC)
#
# Run this top to bottom with the lesson page or the board sheet beside you. Each section is one part of the lesson:
#   1  the push-up graders: 40 reps, 5 graders, no truth column     (The ACFT Graders)
#   2  two graders: percent agreement, chance agreement, kappa      (Two Graders)
#   3  a drawback of kappa: it depends on prevalence                (A Drawback of Kappa)
#   4  PABAK                                                         (PABAK)
#   5  Grader D                                                      (Grader D)
#   6  all ten pairs                                                 (All Ten Pairs)
#   7  the ICC from a two-way ANOVA with graders as blocks          (The ICC)
#   8  which grader is the problem                                   (Which Grader Is the Problem)
#   9  your turn: is it a sandwich?                                  (Your Turn)
# kable() (from the knitr package) prints each result as a small labeled table. If R says there's no
# package called knitr, run install.packages("knitr") once.
library(knitr)

# The data file. This reads it straight from the course page. If you saved it to a data folder
# next to this script, change this line to:  where <- "data/"
where <- "https://dustysturner.com/westpoint/MA376-AY27-1/data/"

# ============================================================================
#  1. THE ACFT GRADERS
#  40 hand-release push-up reps on video. 5 graders watch every rep.
#  pushup is the push-up's number on the video. A to E: 1 if that grader called a no rep, 0 if the rep counts.
#  There is no truth column. Nobody knows which reps were really bad.
# ============================================================================
graders <- read.csv(paste0(where, "L22_acft_graders.csv"))
kable(head(graders), caption = "The first six push-ups")
kable(t(colSums(graders[, -1])), caption = "No reps called by each grader (out of 40)")

# ============================================================================
#  2. TWO GRADERS: B AND E
#  Same table as a confusion matrix, but the truth column is now another grader.
# ============================================================================
B <- graders$B
E <- graders$E
kable(table(E = factor(E, 1:0, c("no rep", "rep")), B = factor(B, 1:0, c("no rep", "rep"))),
      caption = "Grader B across the top, Grader E down the side")

# Percent agreement: how often did they match?
p_o <- mean(B == E)

# Chance agreement: how often would they match by chance?
# Take away the video and keep each grader's habit. Blind guesses are independent, so by the
# multiplication rule from MA206, P(B and E) = P(B) P(E).
P_B <- mean(B)                                   # P(Grader B calls no rep)
P_E <- mean(E)                                   # P(Grader E calls no rep)
both_no  <- P_B * P_E                            # P(B and E)
both_rep <- (1 - P_B) * (1 - P_E)                # P(B^c and E^c)
p_e <- both_no + both_rep                        # they agree on the diagonal

# Kappa: how much they agreed once you remove chance.
kappa <- (p_o - p_e) / (1 - p_e)
kable(data.frame(P_B, P_E, both_no, both_rep, p_o, p_e, kappa), digits = 4,
      caption = "Grader B and Grader E")

# Landis and Koch (1977): 0 to 0.20 slight, 0.21 to 0.40 fair, 0.41 to 0.60 moderate,
# 0.61 to 0.80 substantial, above 0.80 almost perfect.
landis <- function(k) as.character(cut(k, c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, Inf),
                                       c("worse than chance", "slight", "fair", "moderate", "substantial", "almost perfect")))
landis(kappa)

# ============================================================================
#  3. A DRAWBACK OF KAPPA: IT DEPENDS ON PREVALENCE
#  Hold percent agreement at B and E's 0.875. Let both graders call no rep on the same share of reps.
# ============================================================================
prevalence <- seq(0.10, 0.90, by = 0.10)
p_e_at  <- prevalence^2 + (1 - prevalence)^2
kappa_at <- (p_o - p_e_at) / (1 - p_e_at)
kable(data.frame(prevalence, p_o, p_e = p_e_at, kappa = kappa_at, rating = landis(kappa_at)), digits = 3,
      caption = "Same agreement, different prevalence")

curve_prev <- seq(0.07, 0.93, by = 0.01)
curve_pe <- curve_prev^2 + (1 - curve_prev)^2
plot(100 * curve_prev, (p_o - curve_pe) / (1 - curve_pe), type = "l", lwd = 3, col = "#1f5fa8", las = 1,
     ylim = c(-0.1, 1), xlab = "share of reps each grader calls a no rep (%)", ylab = "kappa",
     main = "Agreement held at 0.875")
abline(h = 0, col = "grey")

# ============================================================================
#  4. PABAK
#  If both graders call no rep half the time, p_e = (0.5)(0.5) + (0.5)(0.5) = 0.5.
#  Put that into kappa: (p_o - 0.5) / (1 - 0.5) = 2 p_o - 1.
# ============================================================================
pabak <- 2 * p_o - 1
kable(data.frame(kappa, pabak), digits = 3, caption = "Grader B and Grader E")
# Of the reps either grader called a no rep, how many did they both call?
c(either = sum(B == 1 | E == 1), both = sum(B == 1 & E == 1))

# ============================================================================
#  5. GRADER D
# ============================================================================
colSums(graders[, -1])                           # D never calls a no rep
# One function for any pair, so the rest of the script does not repeat the arithmetic.
agree <- function(x, y) {
  p_o <- mean(x == y)
  p_e <- mean(x) * mean(y) + (1 - mean(x)) * (1 - mean(y))
  c(p_o = p_o, p_e = p_e, kappa = (p_o - p_e) / (1 - p_e), pabak = 2 * p_o - 1)
}
kable(t(agree(graders$A, graders$D)), digits = 3, caption = "Grader A and Grader D")
# D is the null classifier from Lesson 21. Kappa catches D. PABAK does not.

# ============================================================================
#  6. ALL TEN PAIRS
#  Kappa for every pair. Each row is one grader's report card.
# ============================================================================
g <- c("A", "B", "C", "D", "E")
kgrid <- matrix(NA, 5, 5, dimnames = list(g, g))
for (i in g) for (j in g) if (i != j) kgrid[i, j] <- agree(graders[[i]], graders[[j]])["kappa"]
kable(kgrid, digits = 2, caption = "Kappa for every pair")
# Each grader's average kappa with the others, leaving D out of the average
sapply(g, function(i) mean(kgrid[i, setdiff(c("A", "B", "C", "E"), i)]))

# ============================================================================
#  7. THE ICC: TWO-WAY ANOVA, GRADERS AS BLOCKS
#  The ICC is the share of the variation in a call that comes from the push-up.
#  Long format: one row per call, 40 push-ups x 5 graders = 200 rows.
# ============================================================================
long <- data.frame(pushup = factor(rep(graders$pushup, 5)),
                   grader = factor(rep(g, each = 40)),
                   no_rep = c(as.matrix(graders[, -1])))
head(long)
long[long$pushup == 30, ]                        # one push-up, all five graders

fit <- lm(no_rep ~ pushup + grader, data = long)
anova(fit)

ms   <- anova(fit)$`Mean Sq`                     # pushup, grader, Residuals
MS_R <- ms[1]; MS_C <- ms[2]; MS_E <- ms[3]
k <- 5; n <- 40
var_pushup <- (MS_R - MS_E) / k                  # real differences in form
var_grader <- (MS_C - MS_E) / n                  # one grader harsher or softer
var_error  <- MS_E                               # a grader and a push-up that do not line up
ICC <- var_pushup / (var_pushup + var_grader + var_error)
kable(data.frame(var_pushup, var_grader, var_error, ICC), digits = 3, caption = "Where the variance is")
# Koo and Li: below 0.50 poor, 0.50 to 0.75 moderate, 0.75 to 0.90 good, above 0.90 excellent.

# All at once. irr computes the same ICC. (install.packages("irr") once.)
# library(irr)
# icc(graders[, -1], model = "twoway", type = "agreement", unit = "single")

# ============================================================================
#  8. WHICH GRADER IS THE PROBLEM
#  Refit without Grader D. Look at the grader row of the ANOVA and the ICC.
# ============================================================================
noD  <- droplevels(long[long$grader != "D", ])
fit4 <- lm(no_rep ~ pushup + grader, data = noD)
anova(fit4)
ms4 <- anova(fit4)$`Mean Sq`
ICC4 <- ((ms4[1] - ms4[3]) / 4) / ((ms4[1] - ms4[3]) / 4 + (ms4[2] - ms4[3]) / 40 + ms4[3])
kable(data.frame(graders = c("all five", "without D"),
                 grader_p_value = c(anova(fit)$`Pr(>F)`[2], anova(fit4)$`Pr(>F)`[2]),
                 ICC = c(ICC, ICC4)), digits = 3, caption = "Which grader is the problem")

# ============================================================================
#  9. YOUR TURN: IS IT A SANDWICH?
#  Type your calls and your neighbor's: 1 = sandwich, 0 = not. Same order as the page.
#  Hot dog, Taco, Burrito, Pop-Tart, Ice cream sandwich, Sub, Wrap, Gyro, Oreo, Sloppy joe, Quesadilla, Open-faced turkey
# ============================================================================
you      <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
neighbor <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
agree(you, neighbor)
# Kappa is NaN when you both called every item the same way: chance explains all of it.
