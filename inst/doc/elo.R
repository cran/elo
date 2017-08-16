## ------------------------------------------------------------------------
library(elo)

## ------------------------------------------------------------------------
elo.A <- c(1500, 1500)
elo.B <- c(1500, 1600)
elo.prob(elo.A, elo.B)

## ------------------------------------------------------------------------
wins.A <- c(1, 0)
elo.update(elo.A, elo.B, wins.A, k = 20)

## ------------------------------------------------------------------------
elo.calc(elo.A, elo.B, wins.A, k = 20)

## ------------------------------------------------------------------------
data(tournament)
str(tournament)

## ------------------------------------------------------------------------
tournament$wins.A <- tournament$points.Home > tournament$points.Visitor
elo.run(wins.A ~ team.Home + team.Visitor, data = tournament, k = 20)
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament, k = 20)

## ------------------------------------------------------------------------
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
        k(20*log(abs(points.Home - points.Visitor) + 1)), data = tournament)

## ------------------------------------------------------------------------
elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
        data = tournament, k = 20)

## ------------------------------------------------------------------------
tournament$elo.Visitor <- 1500
elo.run(score(points.Home, points.Visitor) ~ team.Home + elo.Visitor,
        data = tournament, k = 20)

## ------------------------------------------------------------------------
e <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
             data = tournament, k = 20)
head(as.matrix(e))

## ------------------------------------------------------------------------
str(as.data.frame(e))

## ------------------------------------------------------------------------
last(e)

