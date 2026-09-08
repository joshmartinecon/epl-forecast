# EPL Ratings and Season Forecast

Team strength ratings and end-of-season simulations for the English Premier League, built from match-level data scraped from FotMob

The core idea that if you know the margin in every match, you can solve for the underlying team strengths with a system of linear equations (a.k.a the Massey rating)
What this repo adds is a second rating built on underlying performance rather than realized goals, and the gap between the two is a measure of how much a team has over- or under-performed its process

---

## What it does

1. **Scrapes** every Premier League fixture and finished match from FotMob
2. **Rates** all 20 teams three ways — on goal difference, on expected goals
   (xG), and on a possession-value index derived from FotMob's momentum graph
3. **Decomposes** goal-based strength into the part explained by underlying
   performance and the part that isn't (call it luck, finishing, or model
   error)
4. **Simulates** the remaining fixtures 10,000 times to produce a distribution
   over final points and league position

---

## Method

### Ratings

- Stack the played matches into a design matrix `X` where each row is a match, coded `+1` for the home team and `-1` for the away team 
- Regressing the home margin on `X` plus a constant recovers each team's rating and a home-advantage term simultaneously
  - So ratings are already adjusted for strength of schedule, which matters a lot early in a season when fixture lists are unbalanced
- The design matrix is rank deficient by one (every row sums to zero), so one team is dropped for identification and ratings are re-centred to mean zero afterwards
- Ratings are in goals per match: the predicted neutral-site margin for team *i* against team *j* is `r[i] - r[j]`
  - The same machinery is applied three times, to goal difference, xG difference, and momentum difference

### Expected rating and "luck"

- A team-level regression of the goals-based rating on the xG and momentum ratings gives:
  - **Expected rating** — fitted values
    - Team strength as implied by underlying performance
  - **Luck** — residuals
    - Realized strength in excess of what the underlying numbers support
    - "Luck" is a convenient label but an imprecise one as the residual bundles genuine variance with finishing quality, goalkeeping, and xG model error
    - Example: A team with an elite finisher will sit above the line year after year
    
### Uncertainty

- Standard errors come from a bootstrap that resamples matches and reruns the entire pipeline (ratings and the team-level regression)
  - The bootstrap uses ridge with `lambda` as a tuning parameter which will be chosen by held-out predictive error on match margins once enough matchweeks have accumulated

### Match outcomes

- Three outcomes with a natural order (away win < draw < home win), so I use an ordered logistic regression on the rating difference
  - The two cutpoints define the width of the draw band on the latent scale, which answers directly how close two teams have to be for a draw to become likely
- Home advantage enters implicitly: the model is fit on home-oriented outcomes, so the cutpoints absorb it

### Simulation

- For each of 10,000 replicates: 
  - draw one bootstrap row (a full, internally consistent set of 20 ratings)
  - compute outcome probabilities for every remaining fixture
  - sample an actual result from each
  - add the points to what has already been banked
  - and rank the table

- Ratings are drawn as whole rows, not independently per team, which preserves the correlation induced by mean-centring
  - Otherwise you would simulate seasons in which every club is simultaneously above average
  
- And each match is realized, not scored at its expected value
  - A team with a 50% win probability wins or loses, it does not collect 1.5 points

---

## Outputs

`epl_table.csv` — one row per team:

| column | meaning |
| --- | --- |
| `pts_now` | points banked to date |
| `exp_pts` | mean simulated final points |
| `exp_rank` | mean simulated final position |
| `p_title` | share of simulations finishing 1st |
| `p_top4` | share finishing top four |
| `p_releg` | share finishing 18th or below |
| `massey_rtg` | rating from realized goal difference |
| `exp_rtg` | rating from underlying performance |
| `luck` | difference between the two |

`next_match_predictions.csv` — win/draw/loss probabilities for the next matchday

`epl_stats.png` — expected rating against realized rating, plotted with club crests
  - The dotted 45° line is the break-even: above it a team has out-performed its underlying numbers, below it the reverse

---

### Scraping notes

- FotMob is a Next.js application
  - The fixture and match data are not in the rendered DOM but in a JSON payload inside a `<script id="__NEXT_DATA__">` tag, so no headless browser is needed
  - Parse the JSON and read it directly

- The scrape is deliberately throttled as FotMob's terms restrict bulk extraction

---

## Limitations

- **Small samples early.** 
  - With three or four matches per team, ratings are barely identified
  - Wide intervals are a feature, not a bug
- **The momentum index is not Excpected Threat (xT).**
  - FotMob exposes a single signed value per minute amd summing the positive and negative parts to get team totals is a construction, not a figure FotMob publishes
  - Thus far, contrary to prior beliefs, it is *negatively* associated with goal margins
- **No squad information.**
  - Injuries, transfers, and fixture congestion are currently invisible to the model
  - A team's rating carries forward unchanged regardless of who is available
