## -----------------------------------------------------------------------------
library(elo)

## -----------------------------------------------------------------------------
e.winpct <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                       subset = points.Home != points.Visitor) # to get rid of ties for now
summary(e.winpct)
rank.teams(e.winpct)
predict(e.winpct, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))

tournament$neutral <- replace(rep(0, nrow(tournament)), 30:35, 1)
summary(elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral) + group(week),
                   data = tournament, subset = points.Home != points.Visitor))

## -----------------------------------------------------------------------------
e.winpct <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                       subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(e.winpct)
predict(e.winpct, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

## -----------------------------------------------------------------------------
results <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                   subset = points.Home != points.Visitor) # to get rid of ties for now
summary(results)
rank.teams(results)
predict(results, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))

summary(elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral) + group(week),
                data = tournament, subset = points.Home != points.Visitor))

## -----------------------------------------------------------------------------
results <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                   subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(results)
predict(results, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

## -----------------------------------------------------------------------------
mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                      subset = points.Home != points.Visitor, k = 0.7)
summary(mc)
rank.teams(mc)
predict(mc, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))
summary(elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral),
                        data = tournament, subset = points.Home != points.Visitor, k = 0.7))

## -----------------------------------------------------------------------------
mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                      subset = points.Home != points.Visitor, k = 0.7, running = TRUE, skip = 5)
summary(mc)
predict(mc, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

## ----eval=FALSE---------------------------------------------------------------
#  elo.markovchain(floor(wins.home) ~ team.home + team.visitor + k(ifelse(x > 0, rH(x), 1 - rH(x))))

## ----eval=FALSE---------------------------------------------------------------
#  elo.markovchain(ifelse(home.points - visitor.points > h, 1, 0) ~ team.home + team.visitor + k(pmax(rH(x), 1 - rH(x))))

## -----------------------------------------------------------------------------
co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                 subset = points.Home != points.Visitor)
summary(co)
rank.teams(co)
predict(co, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))
summary(elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral),
                   data = tournament, subset = points.Home != points.Visitor))

## -----------------------------------------------------------------------------
co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                      subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(co)
predict(co, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

## -----------------------------------------------------------------------------
summary(elo.glm(mov(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                family = "gaussian"))

