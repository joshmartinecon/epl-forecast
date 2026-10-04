
library(rvest)
library(stringr)
library(jsonlite)
library(purrr)
library(png)
library(MASS)
library(dplyr)
library(stringi)

### important that my working director is set correctly
# setwd("C:/Users/jmart/Dropbox/github/epl-forecast")
# setwd("~/Desktop/Dropbox/github/epl-forecast")

## clear data environment if wanted
# rm(list = ls())

##### Get fixture urls, player ratings and match data #####

leagues <- data.frame(
  league  = c("mls", "nwsl", "epl", "la-liga", "bundesliga", "serie-a", "ligue-1"),
  id      = c("130", "9134", "47", "87", "54", "55", "53"),
  slug    = c("mls", "nwsl", "premier-league", "laliga", "bundesliga", "serie", "ligue-1"),
  n_top   = c(18, 8, 4, 4, 4, 4, 4),   # "top N" line for p_top (UCL / playoff spots)
  n_releg = c(0, 0, 3, 3, 3, 3, 3),    # 0 = no relegation; column dropped from output
  tz = c("America/New_York", "America/New_York", "Europe/London",
         "Europe/Madrid", "Europe/Berlin", "Europe/Rome", "Europe/Paris")
)

local_date <- function(tm, tz = "America/New_York") {
  t <- as.POSIXct(substr(tm, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  as.Date(format(t, tz = tz))
}

get_fixtures <- function(id, slug, page = 0) {
  pp <- paste0("https://www.fotmob.com/leagues/", id, "/fixtures/", slug, "?group=by-date&page=", page) |>
    read_html() |>
    html_element("script#__NEXT_DATA__") |> html_text() |>
    fromJSON(simplifyVector = FALSE) |> pluck("props", "pageProps")
  
  # pull a name out of a home/away node, trying the usual keys
  team_name <- function(node) {
    if (!is.list(node)) return(NA_character_)
    for (k in c("longName", "name", "shortName")) {
      v <- node[[k]]
      if (!is.null(v) && nzchar(v)) return(as.character(v))
    }
    NA_character_
  }
  
  out <- list()
  walk_tree <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    nm <- names(x)
    if (!is.null(nm) && "id" %in% nm && "status" %in% nm) {
      tm <- x[["status"]][["utcTime"]]
      if (!is.null(tm)) {
        out[[length(out) + 1]] <<- data.frame(
          match_id = as.character(x[["id"]]),
          date     = local_date(tm),
          home     = team_name(x[["home"]]),
          away     = team_name(x[["away"]]),
          finished = isTRUE(x[["status"]][["finished"]]),
          kickoff  = substr(tm, 1, 19)
        )
      }
    }
    lapply(x, walk_tree)
    invisible(NULL)
  }
  walk_tree(pp)
  if (!length(out)) stop("no fixtures found for league ", id, " -- check the id/slug")
  
  dplyr::bind_rows(out) |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::filter(!is.na(home), !is.na(away)) |>
    dplyr::mutate(url = paste0("https://www.fotmob.com/match/", match_id)) |>
    dplyr::select(match_id, date, home, away, url, finished, kickoff) |>
    dplyr::arrange(date)
}

team_rating <- function(pp, team_id) {
  ps <- pp$content$playerStats
  if (is.null(ps) || !length(ps)) return(NA_real_)
  
  vals <- lapply(ps, function(p) {
    if (!identical(p$teamId, team_id) || !length(p$stats)) return(NULL)
    s   <- p$stats[[1]]$stats
    rat <- s[["FotMob rating"]]$stat$value
    min <- s[["Minutes played"]]$stat$value
    if (is.null(rat) || is.null(min)) return(NULL)      # unrated cameo: dropped
    c(rating = as.numeric(rat), minutes = as.numeric(min))
  })
  vals <- do.call(rbind, vals)
  if (is.null(vals) || nrow(vals) == 0) return(NA_real_)
  
  sum(vals[, "rating"] * vals[, "minutes"]) / sum(vals[, "minutes"])
}

get_match <- function(url) {
  pp <- read_html(url) |>
    html_element("script#__NEXT_DATA__") |>
    html_text() |>
    fromJSON(simplifyVector = FALSE) |>
    pluck("props", "pageProps")
  
  # team stats are grouped; header rows have NULL values, so filter them out
  stat <- function(key) {
    hits <- pp$content$stats$Periods$All$stats |>
      map("stats") |> flatten() |>
      keep(~ identical(.x$key, key) && !is.null(.x$stats[[1]]))
    if (!length(hits)) return(c(NA_real_, NA_real_))
    as.numeric(unlist(hits[[1]]$stats))
  }
  
  xg  <- stat("expected_goals")
  mom <- map_dbl(pp$content$momentum$main$data, "value")
  
  data.frame(
    match_id   = pp$general$matchId,
    home       = pp$general$homeTeam$name,
    away       = pp$general$awayTeam$name,
    date       = local_date(pp$header$status$utcTime),
    home_goals = pp$header$teams[[1]]$score,
    away_goals = pp$header$teams[[2]]$score,
    home_xg    = xg[1],
    away_xg    = xg[2],
    home_xt    = if (length(mom)) sum(mom[mom > 0]) else NA_real_,
    away_xt    = if (length(mom)) -sum(mom[mom < 0]) else NA_real_,
    # home_rtg   = pp$content$lineup$homeTeam$rating,
    # away_rtg   = pp$content$lineup$awayTeam$rating,
    home_rtg  = team_rating(pp, pp$general$homeTeam$id),
    away_rtg  = team_rating(pp, pp$general$awayTeam$id)
  )
}

## clean up file names (moved up; transliterate + drop the women's-team suffix)
slugify <- function(s) {
  s |> stri_trans_general("Latin-ASCII") |>
    gsub(" \\(W\\)", "", x = _) |>
    tolower() |>
    gsub("&", "and", x = _) |>
    gsub("[^a-z0-9]+", "-", x = _) |>
    gsub("^-|-$", "", x = _)
}

run_league <- function(lg, force = FALSE) {

## per-league paths (epl keeps figures/epl_stats.png and data/epl_table.csv)
f_match <- file.path("data", paste0(lg$league, "_match_data.csv"))
f_table <- file.path("data", paste0(lg$league, "_table.csv"))
f_next  <- file.path("data", paste0(lg$league, "_next_match_predictions.csv"))
f_fig   <- file.path("figures", paste0(lg$league, "_stats.png"))
logo_dir <- file.path("logos", lg$league)
dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

##### Webscrape #####
### get fixtures
fx <- get_fixtures(lg$id, lg$slug)
fx$home <- stri_trans_general(fx$home, "Latin-ASCII")
fx$away <- stri_trans_general(fx$away, "Latin-ASCII")
fx$home <- gsub(" \\(W\\)", "", fx$home)
fx$away <- gsub(" \\(W\\)", "", fx$away)

### When was the code last updated?
x <- if (file.exists(f_match)) {
  read.csv(f_match, stringsAsFactors = FALSE,
           colClasses = c(match_id = "character")) |>
    transform(date = as.Date(date))
} else {
  data.frame()
}
todo <- fx[fx$finished & !(fx$match_id %in% x$match_id), ]

### gather new matches
if (nrow(todo) > 0) {
  z <- purrr::map(todo$url, ~ {
    Sys.sleep(5)
    tryCatch(get_match(.x), error = function(e) {
      message("failed: ", .x, " -- ", conditionMessage(e)); NULL })
  })
  df <- dplyr::bind_rows(x, dplyr::bind_rows(z))
  write.csv(df, f_match, row.names = FALSE)
} else {
  message("no new matches")
  if (!force && file.exists(f_table) && file.exists(f_next)) {
    message(lg$league, ": outputs up to date, skipping")
    return(invisible(NULL))
  }
  df <- x
}

## normalize names like the fixtures (FotMob mixes accented/unaccented spellings)
clean_nm <- function(v) gsub(" \\(W\\)", "", stri_trans_general(v, "Latin-ASCII"))
df$home <- clean_nm(df$home)
df$away <- clean_nm(df$away)

##### Predicted Goals & Massey Ratings #####
lm1 <- lm(I(home_goals - away_goals) ~ I(home_xg - away_xg) + I((home_xt - away_xt)/100) + I(home_rtg - away_rtg), data = df)
df$predicted_goals <- predict(lm1, newdata = df)
df$goals <- df$home_goals - df$away_goals

# yay linear algebra
vars <- c("home_xg", "away_xg", "home_xt", "away_xt", "home_rtg", "away_rtg")
m <- df[df$match_id %in% fx$match_id & complete.cases(df[, vars]), ]   # this season; all inputs present
teams <- sort(unique(c(m$home, m$away)))
X <- matrix(0, nrow(m), length(teams), dimnames = list(NULL, teams))
X[cbind(seq_len(nrow(m)), match(m$home, teams))] <-  1
X[cbind(seq_len(nrow(m)), match(m$away, teams))] <- -1
rate <- function(y, lambda = 3) {
  D <- cbind(hfa = 1, X)
  P <- diag(c(0, rep(lambda, length(teams))))
  b <- solve(crossprod(D) + P, crossprod(D, y))
  r <- as.vector(b)[-1]; names(r) <- teams
  list(hfa = as.vector(b)[1], rating = r - mean(r))
}

y_target <- function(d, type = c("wdl", "cap", "gd"), cap = 3) {
  gd <- d$home_goals - d$away_goals
  switch(match.arg(type),
         wdl = sign(gd),
         cap = pmax(pmin(gd, cap), -cap),
         gd  = gd)
}

## Massey ratings
# r_goals <- rate(m$home_goals - m$away_goals)
r_goals <- rate(y_target(m))   
r_xg    <- rate(m$home_xg    - m$away_xg)
r_xt    <- rate((m$home_xt   - m$away_xt) / 100)
r_rtg   <- rate(m$home_rtg   - m$away_rtg)
tm <- data.frame(team = names(r_goals$rating), r_goals = as.numeric(r_goals$rating))
tm$r_xg <- as.numeric(r_xg$rating[tm$team])
tm$r_xt <- as.numeric(r_xt$rating[tm$team])
tm$r_rtg <- as.numeric(r_rtg$rating[tm$team])

## expected goals
fit <- lm(r_goals ~ r_xg + r_xt + r_rtg, data = tm)
tm$exp_goals <- fitted(fit)
tm$luck      <- resid(fit)

##### Figure #####

## find files
files <- list.files(logo_dir, pattern = "\\.png$")
fslug <- sub("-logo-footylogos\\.png$", "", files)
tm$file <- vapply(slugify(tm$team), function(s) {
  hit <- which(fslug == s)
  if (!length(hit)) hit <- grep(s, fslug, fixed = TRUE)       # liverpool -> liverpool-fc
  if (length(hit) > 1) hit <- hit[which.min(nchar(fslug[hit]))]  # barcelona -> fc-barcelona
  if (!length(hit)) hit <- which(vapply(fslug, grepl, logical(1), x = s, fixed = TRUE))
  if (length(hit) == 1) files[hit] else NA_character_
}, character(1))
imgs <- lapply(tm$file, function(f) if (is.na(f)) NULL else readPNG(file.path(logo_dir, f)))
if (any(is.na(tm$file))) message("no logo for: ", paste(tm$team[is.na(tm$file)], collapse = ", "))

## create image
png(f_fig, width = 2000, height = 1600, res = 300)
par(mar = c(4.5, 4.5, 1, 1))
plot(tm$exp_goals, tm$r_goals, type = "n",
     xlab = "Expected Goal Difference", 
     ylab = "Goal Difference (Massey Rating)",
     cex.lab = 1.25, cex.axis = 1.25)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)
abline(0, 1, lty = 3, lwd = 2, col = "darkgrey")
sz  <- 0.28                                   # logo width in inches
usr <- par("usr"); pin <- par("pin")
for (i in seq_len(nrow(tm))) {
  if (is.null(imgs[[i]])) {                              # no logo: fall back to a label
    text(tm$exp_goals[i], tm$r_goals[i], abbreviate(tm$team[i], 4), cex = 0.6)
    next
  }
  ar <- dim(imgs[[i]])[1] / dim(imgs[[i]])[2]           # height / width
  w  <- sz * diff(usr[1:2]) / pin[1]
  h  <- sz * ar * diff(usr[3:4]) / pin[2]
  rasterImage(imgs[[i]],
              tm$exp_goals[i] - w/2, tm$r_goals[i] - h/2,
              tm$exp_goals[i] + w/2, tm$r_goals[i] + h/2,
              interpolate = TRUE)
}
legend("topleft", legend = "Above (Below) = (Un)lucky",
       lty = 3, lwd = 2, col = "darkgrey", bty = "n")
dev.off()

##### Standard Errors #####
### simplify
tm <- tm[order(-tm$exp_goals),]
x <- data.frame(
  team = tm$team,
  massey_rating = round(tm$r_goals, 2),
  expected_rating = round(tm$exp_goals, 2),
  luck = round(tm$luck, 2)
)

## bootstrap standard errors
boot_ratings <- function(m, B = 1000, lambda = 3) {
  n <- nrow(m)
  P <- diag(c(0, rep(lambda, length(teams))))     # hfa unpenalised
  fit_r <- function(D, yv) {
    b <- solve(crossprod(D) + P, crossprod(D, yv))
    r <- as.vector(b)[-1]
    r - mean(r)
  }
  out <- matrix(NA_real_, B, length(teams), dimnames = list(NULL, teams))
  for (b in seq_len(B)) {
    mb <- m[sample(n, n, replace = TRUE), ]
    Xb <- matrix(0, nrow(mb), length(teams), dimnames = list(NULL, teams))
    Xb[cbind(seq_len(nrow(mb)), match(mb$home, teams))] <-  1
    Xb[cbind(seq_len(nrow(mb)), match(mb$away, teams))] <- -1
    D  <- cbind(hfa = 1, Xb)
    # rg <- fit_r(D, mb$home_goals - mb$away_goals)
    rg <- fit_r(D, y_target(mb))
    rx <- fit_r(D, mb$home_xg    - mb$away_xg)
    rt <- fit_r(D, (mb$home_xt   - mb$away_xt) / 100)
    rr <- fit_r(D, mb$home_rtg - mb$away_rtg)
    # out[b, ] <- fitted(lm(rg ~ rx + rt + rr))
    out[b, ] <- fitted(lm(rg ~ rx + rr))
  }
  out
}
bs <- boot_ratings(m)

## out-of-sample predict values, for calibrating the ordered logit only
loo_predict <- function(m, lambda = 3) {
  D0 <- cbind(hfa = 1, X)
  P  <- diag(c(0, rep(lambda, length(teams))))
  vapply(seq_len(nrow(m)), function(i) {
    Di <- D0[-i, , drop = FALSE]
    A  <- solve(crossprod(Di) + P)
    f  <- function(yv) {
      b <- A %*% crossprod(Di, yv)
      r <- as.vector(b)[-1]; names(r) <- teams
      r - mean(r)
    }
    # rg <- f((m$home_goals - m$away_goals)[-i])
    rg <- f(y_target(m)[-i])    
    rx <- f((m$home_xg    - m$away_xg)[-i])
    rt <- f(((m$home_xt   - m$away_xt) / 100)[-i])
    rr <- f((m$home_rtg   - m$away_rtg)[-i])
    eg <- fitted(lm(rg ~ rx + rt + rr)); names(eg) <- teams
    unname(eg[m$home[i]] - eg[m$away[i]])
  }, numeric(1))
}
m$predict_loo <- loo_predict(m)

# x$se <- apply(bs, 2, sd)[x$team]
# x$lb <- apply(bs, 2, quantile, 0.025)[x$team]
# x$ub <- apply(bs, 2, quantile, 0.975)[x$team]

##### forecast prep #####
## make names consistent
# fixture-page names -> match-page names, matched on shared match ids
xw <- unique(rbind(
  data.frame(abbrev = fx$home, team = df$home[match(fx$match_id, df$match_id)]),
  data.frame(abbrev = fx$away, team = df$away[match(fx$match_id, df$match_id)])))
xw <- xw[!is.na(xw$team), ]
fixnm <- function(v) ifelse(v %in% xw$abbrev, xw$team[match(v, xw$abbrev)], v)

## simplify
y <- fx[, c("match_id","date","home","away", "kickoff")]
y$home <- fixnm(y$home)
y$away <- fixnm(y$away)

# define wins and losses
y$goals   <- df$goals[match(y$match_id, df$match_id)]
y$predict <- x$expected_rating[match(y$home, x$team)] - x$expected_rating[match(y$away, x$team)]
y$outcome <- factor(ifelse(y$goals > 0, "win", ifelse(y$goals == 0, "draw", "lose")),
                    levels = c("lose","draw","win"), ordered = TRUE)
future <- y[is.na(y$goals) & y$home %in% x$team & y$away %in% x$team, ]   # drops TBD / playoff slots

# estimate probability of winning | on rating
train <- y[!is.na(y$goals), ]
train$predict <- m$predict_loo[match(train$match_id, m$match_id)]
train <- train[!is.na(train$predict), ]
mod <- polr(outcome ~ predict, data = train, Hess = TRUE)

## points already banked
pl  <- y[!is.na(y$goals), ]
stk <- rbind(data.frame(team = pl$home, g =  pl$goals),
             data.frame(team = pl$away, g = -pl$goals))
stk$pts <- ifelse(stk$g > 0, 3, ifelse(stk$g == 0, 1, 0))
pts_now <- tapply(stk$pts, stk$team, sum)[x$team]
pts_now[is.na(pts_now)] <- 0

##### simulation #####
B   <- 10000
res <- matrix(NA_integer_, B, nrow(x), dimnames = list(NULL, x$team))
pts <- matrix(NA_real_,    B, nrow(x), dimnames = list(NULL, x$team))
set.seed(1)
for (i in seq_len(B)) {
  r <- bs[sample(nrow(bs), 1), ]
  p <- predict(mod, newdata = data.frame(
    predict = r[future$home] - r[future$away]), type = "probs")
  o  <- apply(p, 1, function(pr) sample.int(3, 1, prob = pr))
  add <- tapply(c(c(0,1,3)[o], c(3,1,0)[o]),
                c(future$home, future$away), sum)[x$team]
  add[is.na(add)] <- 0
  tot      <- pts_now + add
  pts[i, ] <- tot
  res[i, ] <- rank(-tot, ties.method = "random")
}

##### output #####
out <- data.frame(
  team     = x$team,
  pts_now  = as.numeric(pts_now),
  exp_pts  = colMeans(pts),
  exp_rank = colMeans(res),
  p_title  = colMeans(res == 1),
  p_top    = colMeans(res <= lg$n_top),
  p_releg  = colMeans(res > nrow(x) - lg$n_releg)
)
rownames(out) <- NULL
out <- out[order(out$exp_rank),]
out <- cbind(out, x[match(out$team, x$team), c(2:4)])
names(out)[8:9] <- c("massey_rtg", "exp_rtg")
out <- out[order(-out$exp_pts, out$exp_rank, -out$p_title),]
out$exp_pts <- round(out$exp_pts)
out$exp_rank <- round(out$exp_rank, 1)
for(i in 5:7){
  out[,i] <- round(100 * out[,i])
}
# out

nxt <- future
nxt <- cbind(nxt[,2:4], round(100 * predict(mod, newdata = nxt, type = "probs")))
nxt <- nxt[,c(1:3, 6, 5, 4)]
nxt$kickoff_utc <- future$kickoff
n1 <- aggregate(date ~ home, nxt, min)
n2 <- aggregate(date ~ away, nxt, min)
names(n1)[1] <- names(n2)[1] <- "team"
n <- rbind(n1, n2)
n <- aggregate(date ~ team, n, min)
nxt <- nxt[nxt$date <= max(n$date),]
# nxt

##### save #####
names(out)[names(out) == "p_top"] <- paste0("p_top", lg$n_top)   # epl stays p_top4
if (lg$n_releg == 0) out$p_releg <- NULL
write.csv(out, f_table, row.names = F)
write.csv(nxt, f_next, row.names = F)
invisible(list(table = out, next_matches = nxt))
}

##### Run each league #####
for (i in seq_len(nrow(leagues))) {
  lg <- leagues[i, ]
  message("---- ", lg$league, " ----")
  tryCatch(run_league(lg),
           error = function(e) message(lg$league, " failed: ", conditionMessage(e)))
}
