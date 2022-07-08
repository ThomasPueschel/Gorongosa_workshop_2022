# linear models

library(agridat)
library(ggplot2)

#We will use the ilri.sheep dataset from the agridat package, to answer the question:
# Is the weight of lambs at weaning a function of their age at weaning?,
# with the hypothesis that lambs that are weaned later are also heavier.

sheep <- agridat::ilri.sheep   # load the data

library(dplyr)
sheep <- filter(sheep, ewegen == "R")   # there are confounding variables in this dataset that we don't want to take into account. We'll only consider lambs that come from mothers belonging to the breed "R".

head(sheep)  # overview of the data; we'll focus on weanwt (wean weight) and weanage

sheep.m1 <- lm(weanwt ~ weanage, data = sheep)   # run the model
summary(sheep.m1)                                # study the output

#What if the sex of the lamb also influences weight gain? Let’s run a new model to test this
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)
(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
    geom_point(aes(colour = sex)) +                                # scatter plot, coloured by sex
    labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
    stat_smooth(method = "lm", aes(fill = sex, colour = sex)) +    # adding regression lines for each sex
    scale_colour_manual(values = c("#FFC125", "#36648B")) +
    scale_fill_manual(values = c("#FFC125", "#36648B")) )

# Practicing generalised linear models

#The data represent population trends for European Shags on the Isle of May
shag <- read.csv("data/shagLPI.csv", header = TRUE)

shag$year <- as.numeric(shag$year)  # transform year from character into numeric variable

# Making a histogram to assess data distribution
(shag.hist <- ggplot(shag, aes(pop)) + geom_histogram() )

# our pop variable represents count abundance data, i.e. integer values (whole European Shags!) so a Poisson distribution is appropriate here.
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)

# From the summary of our model we can see that European Shag abundance varies significantly based on the predictor year.
# Let’s visualise how European Shag abundance has changed through the years:

(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    labs(x = " ", y = "European Shag abundance"))

# A model with a binomial distribution
# We can examine if damage to Scot’s pine by weevils (a binary, TRUE/FALSE variable) varies based on the block in which the trees are located. 
# You can imagine that different blocks represent different Scot’s pine populations, and perhaps some of them will be particularly vulnerable to weevils? Because of the binary nature of the response variable (true or false), a binomial model is appropriate here.

Weevil_damage <- read.csv("data/Weevil_damage.csv")

# Making block a factor (a categorical variable)
Weevil_damage$block <- as.factor(Weevil_damage$block)

# Running the model
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)
