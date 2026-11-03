# MA376 Lesson 28: PCA, the model
#
# Run this top to bottom with the board sheet beside you. Each section is one part of the sheet:
#   1  the data: crime in 50 states, no y                               (questions 1 and 2)
#   2  how can we rank the states by crime? two tries                   (questions 3 and 4)
#   3  fit PCA                                                          (questions 5 to 9)
#   4  how R finds the loadings: the eigen system
#   5  back to the ranking                                              (question 10)
#   6  four columns, two numbers: the biplot                            (questions 11 and 12)
# If R says there's no package called something, run once:
#   install.packages(c("tidyverse", "ggrepel", "ggvfields", "maps"))
library(tidyverse)
library(ggrepel)
library(ggvfields)

# ============================================================================
#  1. THE DATA: CRIME IN 50 STATES
#  USArrests ships with R. 1973 arrests per 100,000 residents for Murder, Assault
#  and Rape, plus UrbanPop: the percent of residents living in cities.
#  No y. Four x's and a state name.
# ============================================================================
arrests <- as_tibble(USArrests, rownames = "state")
arrests

arrests |>
  pivot_longer(-state, names_to = "column") |>
  group_by(column) |>
  summarise(min = min(value), mean = mean(value), max = max(value), sd = sd(value))

# Each column on its own
arrests |>
  pivot_longer(-state, names_to = "column") |>
  ggplot(aes(value)) +
  geom_histogram(bins = 12, fill = "steelblue", color = "white") +
  facet_wrap(~ column, scales = "free_x") +
  theme_minimal()

# All four on one axis. What do you notice?
arrests |>
  pivot_longer(-state, names_to = "column") |>
  ggplot(aes(value, column)) +
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  labs(x = "Value", y = NULL) +
  theme_minimal()

# On a map (the lower 48: no Alaska or Hawaii)
states <- map_data("state")
arrests |>
  mutate(region = str_to_lower(state)) |>
  pivot_longer(c(Murder, Assault, UrbanPop, Rape), names_to = "column") |>
  group_by(column) |>
  mutate(rank = percent_rank(value)) |>   # 0 = lowest state, 1 = highest
  ungroup() |>
  right_join(states, by = "region", relationship = "many-to-many") |>
  filter(!is.na(column)) |>
  ggplot(aes(long, lat, group = group, fill = rank)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  coord_quickmap() +
  facet_wrap(~ column) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Rank",
                       breaks = c(0, 1), labels = c("lowest", "highest")) +
  theme_void()

# Two columns at a time
long <- arrests |> pivot_longer(-state, names_to = "column", values_to = "value")
long |>
  inner_join(long, by = "state", suffix = c("_x", "_y"), relationship = "many-to-many") |>
  filter(column_x != column_y) |>
  ggplot(aes(value_x, value_y)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  facet_grid(column_y ~ column_x, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()

arrests |> select(-state) |> cor() |> round(2)                    # questions 1 and 2

# Who is at the top of each column?
arrests |>
  pivot_longer(-state, names_to = "column") |>
  group_by(column) |>
  slice_max(value, n = 3) |>
  summarise(top_three = paste(state, collapse = ", "))

# ============================================================================
#  2. HOW CAN WE RANK THE STATES BY CRIME?
#  We want one crime number per state. How do we combine the columns?
# ============================================================================
# Try 1: add them up                                                (question 3)
added <- arrests |> mutate(crime = Murder + Assault + Rape)
added |> slice_max(crime, n = 5)
cor(added$crime, added$Assault)

# Try 2: scale, then average                                        (question 4)
averaged <- arrests |>
  mutate(across(c(Murder, Assault, Rape), \(x) (x - mean(x)) / sd(x)),
         crime = (Murder + Assault + Rape) / 3)
averaged |> slice_max(crime, n = 5)

# ============================================================================
#  3. FIT PCA
#  scale. = TRUE scales each column first. R picks the sign of each component;
#  it made more crime negative, so the two middle lines flip PC1. Nothing else changes.
#  R names the new columns PC1 to PC4. PC1 is Z1: each state's score.
# ============================================================================
pca <- prcomp(arrests |> select(-state), scale. = TRUE)
pca$rotation[, 1] <- -pca$rotation[, 1]   # flip PC1 so more crime is higher
pca$x[, 1] <- -pca$x[, 1]
scores <- arrests |> bind_cols(as_tibble(pca$x))

scores |> slice_max(PC1, n = 6)                                     # most crime
scores |> slice_min(PC1, n = 6)                                     # least crime

pca$rotation                                    # the loadings (questions 5 and 6)
summary(pca)                                    # proportion of variance (questions 7 to 9)

# ============================================================================
#  4. HOW R FINDS THE LOADINGS
#  R does not search. It uses the eigen system of the correlation matrix.
# ============================================================================
e <- eigen(cor(arrests |> select(-state)))
dimnames(e$vectors) <- dimnames(pca$rotation)   # label the rows and columns

e$values                                        # eigenvalues
pca$sdev^2                                      # the variances of the Z's: same numbers

e$vectors                                       # eigenvectors
pca$rotation                                    # the loadings: same numbers, some signs flipped

# ============================================================================
#  5. BACK TO THE RANKING                                           (question 10)
# ============================================================================
scores |>
  mutate(equal_crimes = averaged$crime) |>
  slice_max(PC1, n = 5) |>
  select(state, PC1, equal_crimes)

cor(scores$PC1, averaged$crime)

# ============================================================================
#  6. FOUR COLUMNS, TWO NUMBERS: THE BIPLOT                         (questions 11 and 12)
#  Dots: each state's Z1 and Z2 scores. Arrows: each column's loadings (stretched 3x).
# ============================================================================
arrows <- as_tibble(pca$rotation, rownames = "variable") |> mutate(PC1 = 3 * PC1, PC2 = 3 * PC2)

ggplot(scores, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_vline(xintercept = 0, color = "gray80") +
  geom_point(color = "steelblue") +
  geom_vector(data = arrows, aes(x = 0, y = 0, fx = PC1, fy = PC2, color = I("firebrick")),
              center = FALSE, normalize = FALSE, inherit.aes = FALSE, show.legend = FALSE) +
  geom_text_repel(data = bind_rows(scores |> mutate(label = state, type = "state"),
                                   arrows |> mutate(label = variable, type = "column",
                                                    PC1 = 1.15 * PC1, PC2 = 1.15 * PC2)),
                  aes(label = label, color = type, fontface = if_else(type == "column", "bold", "plain"),
                      size = type), max.overlaps = 50, show.legend = FALSE) +
  scale_color_manual(values = c(state = "gray30", column = "firebrick")) +
  scale_size_manual(values = c(state = 3, column = 4.5)) +
  labs(x = "Z1 (right = more crime)", y = "Z2 (up = more urban)") +
  theme_minimal()

# The same picture in base R, the ISLR lab:
# biplot(pca, scale = 0)
