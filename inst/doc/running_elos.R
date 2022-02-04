## -----------------------------------------------------------------------------
library(elo)

## -----------------------------------------------------------------------------
data(tournament)
str(tournament)

## -----------------------------------------------------------------------------
tournament$wins.A <- tournament$points.Home > tournament$points.Visitor
elo.run(wins.A ~ team.Home + team.Visitor, data = tournament, k = 20)
# on the fly
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament, k = 20)

## -----------------------------------------------------------------------------
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
        k(20*log(abs(points.Home - points.Visitor) + 1)), data = tournament)

## -----------------------------------------------------------------------------
k1 <- 20*log(abs(tournament$points.Home - tournament$points.Visitor) + 1)
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + k(k1, k1/2), data = tournament)

## -----------------------------------------------------------------------------
elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
        data = tournament, k = 20)

## -----------------------------------------------------------------------------
tournament$elo.Visitor <- 1500
elo.run(score(points.Home, points.Visitor) ~ team.Home + elo.Visitor,
        data = tournament, k = 20)

## -----------------------------------------------------------------------------
tournament$elo.Visitor <- 1500
elo.run(score(points.Home, points.Visitor) ~ team.Home + elo.Visitor +
        regress(half, 1500, 0.2),
        data = tournament, k = 20)

## -----------------------------------------------------------------------------
er <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
                group(week),
              data = tournament, k = 20)
as.matrix(er)

## -----------------------------------------------------------------------------
d <- data.frame(
  team1 = c("Part 2", "Part 2", "Part 1"),
  team2 = c("Part 1", "Part 3", "Part 3"),
  won = 1
)
d

## -----------------------------------------------------------------------------
d$group <- 1
final.elos(elo.run(won ~ team1 + team2 + group(group), data = d, k = 20))

## -----------------------------------------------------------------------------
d2 <- data.frame(
  first = "Part 2",
  second = "Part 1",
  third = "Part 3"
)
final.elos(elo.run.multiteam(~ multiteam(first, second, third), k = 20, data = d2))

## -----------------------------------------------------------------------------
data("tournament.multiteam")
str(tournament.multiteam)
erm <- elo.run.multiteam(~ multiteam(Place_1, Place_2, Place_3, Place_4),
                         data = tournament.multiteam, k = 20)
final.elos(erm)

## -----------------------------------------------------------------------------
e <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
             data = tournament, k = 20)
summary(e)
rank.teams(e)

## -----------------------------------------------------------------------------
head(as.matrix(e))

## -----------------------------------------------------------------------------
str(as.data.frame(e))

## -----------------------------------------------------------------------------
final.elos(e)

## -----------------------------------------------------------------------------
results <- elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
                   data = tournament, k = 20)
newdat <- data.frame(
  team.Home = "Athletic Armadillos",
  team.Visitor = "Blundering Baboons"
)
predict(results, newdata = newdat)

## -----------------------------------------------------------------------------
custom_update <- function(wins.A, elo.A, elo.B, k, adjust.A, adjust.B, ...)
{
  k*(wins.A - elo.prob(elo.A, elo.B, adjust.B = adjust.B,
                       adjust.A = ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))
}
custom_prob <- function(elo.A, elo.B, adjust.A, adjust.B)
{
  1/(1 + 10^(((elo.B + adjust.B) - (elo.A + ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))/400.0))
}
er2 <- elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
               data = tournament, k = 20, prob.fun = custom_prob, update.fun = custom_update)
final.elos(er2)

## -----------------------------------------------------------------------------
er3 <- elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
               data = tournament, k = 20)
final.elos(er3)

