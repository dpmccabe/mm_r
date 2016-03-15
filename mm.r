library(plyr)

est_elo = function() {
  elo = c(2097,2075,2052,2078,1972,2045,2014,1956,1938,2033,1915,1973,1953,1910,1876,1938,1933,1867,1837,1788,1887,1893,1871,1904,1846,1916,1896,1832,1872,1794,1815,1914,1798,1788,1772,1787,1814,1824,1751,1824,1756,1792,1777,1690,1768,1733,1734,1735,1730,1722,1740,1759,1667,1663,1610,1737,1708,1613,1635,1638,1553,1623,1544,1392,1477,1488,1420,1417)
  rating = c(94.5,93.9,92.5,91.8,90,91.3,90.7,89.3,88.7,88,86.8,87.7,89,87.3,87.4,87.4,87.1,86.5,85.5,84.7,86.6,86.6,86.5,85.9,85.6,86,84.8,84.4,85.3,83.7,84.2,84.5,83.1,82.4,82.7,82.3,81.6,82.5,80.2,81,81.5,80.2,81.3,79.9,79.6,81.4,78.9,78.6,78.5,77.7,77.6,78.2,76.2,77.1,76.6,78,76.6,75.7,75,75,74.2,73.3,71.4,68,68.8,68.6,66.9,66.7)
  elo_rating = data.frame(elo, rating)
  summary(lm(elo ~ rating, elo_rating))
}

load_elod = function() {
  elod0 = read.csv("/Users/devin/Sites/mm/fivethirtyeight_ncaa_forecasts.csv", header = T)
  elod0 = subset(elod0, select = c("team_name", "team_rating", "team_region", "team_seed"))
  colnames(elod0) = c("team", "rating", "region", "seed")

  elod0[elod0$seed == "16a" | elod0$seed == "16b",]$seed = "16"
  elod0[elod0$seed == "11a" | elod0$seed == "11b",]$seed = "11"
  elod0$seed = as.integer(as.character(elod0$seed))

  elod0$region = factor(elod0$region, levels = c("South", "West", "East", "Midwest"))

  elod0$elo = -191.9026 + 24.2262 * elod0$rating
  elod0 = subset(elod0, select = c("team", "region", "seed", "elo"))

  elod = ddply(elod0, .(region, seed), function(r) {
    if (nrow(r) == 1) {
      return(r)
    } else {
      return(r[which.max(r$elo),])
    }
  })

  return(elod)
}

load_games_template = function(elod) {
  games_template = data.frame(round_name = c(rep(64, 2 * 4 * 8), rep(32, 2 * 4 * 4), rep(16, 2 * 4 * 2), rep(8, 2 * 4 * 1), rep(4, 4), rep(2, 2)), region = c(c(rep("South", 16), rep("West", 16), rep("East", 16), rep("Midwest", 16)), c(rep("South", 8), rep("West", 8), rep("East", 8), rep("Midwest", 8)), c(rep("South", 4), rep("West", 4), rep("East", 4), rep("Midwest", 4)), c(rep("South", 2), rep("West", 2), rep("East", 2), rep("Midwest", 2)), rep("South-West", 2), rep("East-Midwest", 2), rep("South-West-East-Midwest", 2)), team = "", seed = NA, elo = NA, played = F, winprob = NA, winner = F)

  games_template$team = factor(NA, levels = levels(elod$team))

  elodsorted = ddply(elod, .(region), function(r) {
    r[c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)[r$seed],]
  })

  game_points = 10 * 2**(6 - log(games_template[seq(from = 1, to = nrow(games_template), by = 2), "round_name"], 2))

  games_template[1:64, c("team", "seed", "elo")] = elodsorted[, c("team", "seed", "elo")]

  return(games_template)
}

sim_tourney = function(games_template) {
  games = games_template

  while(any(games$played == F)) {
    round_i = which(games$played == F & !is.na(games$team))

    winners_probs = adply(seq(from = round_i[1], to = max(round_i) - 1, by = 2), 1, function(i) {
      teams = games[c(i, i + 1),]

      elo_diff = teams[1, "elo"] - teams[2, "elo"]
      team1_win_prob = 1 / (10^(-elo_diff / 400) + 1)
      team1_win = runif(1) <= team1_win_prob

      return(data.frame(winprob = c(team1_win_prob, 1 - team1_win_prob), winner = c(team1_win, !team1_win)))
    })

    games[round_i, "played"] = T
    games[round_i, c("winprob", "winner")] = winners_probs[, c("winprob", "winner")]

    next_round_i = which(games$round_name == games[round_i[1], "round_name"] / 2)
    games[next_round_i, c("team", "seed", "elo")] = subset(games[round_i,], winner == T, select = c("team", "seed", "elo"))
  }

  return(games)
}

entry_score = function(entry, result) {
  matches = subset(entry, winner == T, select = "team") == subset(result, winner == T, select = "team")
  return(sum(game_points[matches]))
}

result_set = function(n, games_template) {
  return(alply(1:n, 1, function(i) {
    result = sim_tourney(games_template)
    return(list(result = result, sim_prob = prod(result[result$winner, "winprob"])))
  }))
}

elod = load_elod()
games_template = load_games_template(elod)

# entry = sim_tourney(games_template)
# entry_score(entry, result)
