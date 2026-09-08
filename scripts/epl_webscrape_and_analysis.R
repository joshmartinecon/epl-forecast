
library(rvest)
library(stringr)
library(jsonlite)
library(purrr)
library(png)
library(MASS)
library(dplyr)

## clear data environment if wanted
# rm(list = ls())

##### Get fixture urls and then match data #####
get_fixtures <- function(page = 0) {
  pp <- paste0("https://www.fotmob.com/leagues/47/fixtures/premier-league?group=by-date&page=", page) |>
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
          date     = as.Date(substr(tm, 1, 10)),
          home     = team_name(x[["home"]]),
          away     = team_name(x[["away"]]),
          finished = isTRUE(x[["status"]][["finished"]])
        )
      }
    }
    lapply(x, walk_tree)
    invisible(NULL)
  }
  walk_tree(pp)
  
  dplyr::bind_rows(out) |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::filter(!is.na(home), !is.na(away)) |>
    dplyr::mutate(url = paste0("https://www.fotmob.com/match/", match_id)) |>
    dplyr::select(match_id, date, home, away, url, finished) |>
    dplyr::arrange(date)
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
    date       = as.Date(substr(pp$header$status$utcTime, 1, 10)),
    home_goals = pp$header$teams[[1]]$score,
    away_goals = pp$header$teams[[2]]$score,
    home_xg    = xg[1],
    away_xg    = xg[2],
    home_xt    = if (length(mom)) sum(mom[mom > 0]) else NA_real_,
    away_xt    = if (length(mom)) -sum(mom[mom < 0]) else NA_real_
  )
}

##### Webscrape #####

### get fixtures
fx <- get_fixtures(0)

### When was the code last updated?
x <- read.csv("data/match_data.csv")
todo <- fx[fx$finished & !(fx$match_id %in% x$match_id), ]

### gather new matches
if (nrow(todo) > 0) {
  z  <- purrr::map(todo$url, purrr::possibly(~ { Sys.sleep(5); get_match(.x) }, NULL))
  df <- dplyr::bind_rows(x, dplyr::bind_rows(z))
  write.csv(df, "data/match_data.csv", row.names = FALSE)
} else {
  message("no new matches")
  df <- x
}

##### Predicted Goals & Massey Ratings #####
lm1 <- lm(I(home_goals - away_goals) ~ I(home_xg - away_xg) + I((home_xt - away_xt)/100), data = df)
df$predicted_goals <- predict(lm1, newdata = df)
df$goals <- df$home_goals - df$away_goals

# yay linear algebra
m <- df[!is.na(df$home_xg), ]
teams <- sort(unique(c(m$home, m$away)))
X <- matrix(0, nrow(m), length(teams), dimnames = list(NULL, teams))
X[cbind(seq_len(nrow(m)), match(m$home, teams))] <-  1
X[cbind(seq_len(nrow(m)), match(m$away, teams))] <- -1
rate <- function(y) {
  D <- cbind(hfa = 1, X[, -ncol(X), drop = FALSE])
  fit <- lm(y ~ 0 + D)
  b <- coef(fit)
  r <- c(b[-1], 0); names(r) <- teams
  list(hfa = unname(b[1]), rating = r - mean(r), fit = fit, V = vcov(fit))
}

## massey ratings
r_goals <- rate(m$home_goals - m$away_goals)
r_xg    <- rate(m$home_xg    - m$away_xg)
r_xt    <- rate((m$home_xt   - m$away_xt) / 100)
tm <- data.frame(team = names(r_goals$rating), r_goals = as.numeric(r_goals$rating))
tm$r_xg <- as.numeric(r_xg$rating[tm$team])
tm$r_xt <- as.numeric(r_xt$rating[tm$team])

## expected goals
fit <- lm(r_goals ~ r_xg + r_xt, data = tm)
tm$exp_goals <- fitted(fit)
tm$luck      <- resid(fit)

##### Figure #####

## clean up file names
slugify <- function(s) {
  s |> tolower() |>
    gsub("&", "and", x = _) |>
    gsub("[^a-z0-9]+", "-", x = _) |>
    gsub("^-|-$", "", x = _)
}

## find files
files <- list.files("logos", pattern = "\\.png$")
fslug <- sub("-logo-footylogos\\.png$", "", files)
tm$file <- vapply(slugify(tm$team), function(s) {
  hit <- which(fslug == s)
  if (!length(hit)) hit <- grep(s, fslug, fixed = TRUE)       # liverpool -> liverpool-fc
  if (!length(hit)) hit <- which(vapply(fslug, grepl, logical(1), x = s, fixed = TRUE))
  if (length(hit) == 1) files[hit] else NA_character_
}, character(1))
imgs <- lapply(tm$file, function(f) readPNG(file.path("logos", f)))

## create image
png("figures/epl_stats.png", width = 2000, height = 1600, res = 300)
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
    rg <- fit_r(D, mb$home_goals - mb$away_goals)
    rx <- fit_r(D, mb$home_xg    - mb$away_xg)
    rt <- fit_r(D, (mb$home_xt   - mb$away_xt) / 100)
    out[b, ] <- fitted(lm(rg ~ rx + rt))
  }
  out
}
bs <- boot_ratings(m)
x$se <- apply(bs, 2, sd)[x$team]
x$lb <- apply(bs, 2, quantile, 0.025)[x$team]
x$ub <- apply(bs, 2, quantile, 0.975)[x$team]

##### forecast prep #####
## make names consistent
xw <- data.frame(
  abbrev = c("Hull","Ipswich","Nottm Forest","Brighton","Man City","Newcastle",
             "Bournemouth","Coventry","Tottenham","Leeds","Man United"),
  team   = c("Hull City","Ipswich Town","Nottingham Forest","Brighton & Hove Albion",
             "Manchester City","Newcastle United","AFC Bournemouth","Coventry City",
             "Tottenham Hotspur","Leeds United","Manchester United"))
fixnm <- function(v) ifelse(v %in% xw$abbrev, xw$team[match(v, xw$abbrev)], v)

## simplify
y <- fx[, c("match_id","date","home","away")]
y$home <- fixnm(y$home)
y$away <- fixnm(y$away)

# define wins and losses
y$goals   <- df$goals[match(y$match_id, df$match_id)]
y$predict <- x$expected_rating[match(y$home, x$team)] - x$expected_rating[match(y$away, x$team)]
y$outcome <- factor(ifelse(y$goals > 0, "win", ifelse(y$goals == 0, "draw", "lose")),
                    levels = c("lose","draw","win"), ordered = TRUE)
future <- y[is.na(y$goals), ]

# estimate probability of winning | on rating
m      <- polr(outcome ~ predict, data = y[!is.na(y$goals), ], Hess = TRUE)

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
  p <- predict(m, newdata = data.frame(
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
  p_top4   = colMeans(res <= 4),
  p_releg  = colMeans(res >= 18)
)
rownames(out) <- NULL
out <- out[order(out$exp_rank),]
out <- cbind(out, x[match(out$team, x$team), c(2:4)])
names(out)[8:9] <- c("massey_rtg", "exp_rtg")
out <- out[order(-out$exp_pts),]
out$exp_pts <- round(out$exp_pts)
out$exp_rank <- round(out$exp_rank, 1)
for(i in 5:7){
  out[,i] <- round(100 * out[,i])
}
# out

nxt <- future[future$date == min(future$date),]
nxt <- cbind(nxt[,2:4], round(100 * predict(m, newdata = nxt, type = "probs")))
nxt <- nxt[,c(1:3, 6, 5, 4)]
# nxt

##### save #####
write.csv(out, "data/epl_table.csv", row.names = F)
write.csv(nxt, "data/next_match_predictions.csv", row.names = F)
