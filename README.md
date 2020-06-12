ST 558 - Project 1
================
Christine Marie Heubusch
June 12, 2020

  - [Understanding JSON Data](#understanding-json-data)
      - [What is JSON data?](#what-is-json-data)
      - [Packages Used for Reading JSON
        Data](#packages-used-for-reading-json-data)
  - [Contacting the NHL API](#contacting-the-nhl-api)
      - [Creating a Function for Accessing Franchise
        Data](#creating-a-function-for-accessing-franchise-data)
      - [Creating a Function for Accessing Franchise Team
        Totals](#creating-a-function-for-accessing-franchise-team-totals)
      - [Creating a Function for Accessing Season
        Records](#creating-a-function-for-accessing-season-records)
      - [Creating a Function for Accessing Goalie
        Records](#creating-a-function-for-accessing-goalie-records)
      - [Creating a Function for Accessing Skater
        Records](#creating-a-function-for-accessing-skater-records)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Is a Team With More Recorded Shutouts More Likely to Still be
        an Active
        Franchise?](#is-a-team-with-more-recorded-shutouts-more-likely-to-still-be-an-active-franchise)
      - [Are Teams more Likely to Win at Home than
        Away?](#are-teams-more-likely-to-win-at-home-than-away)
      - [Do Aggressive Teams Tend to Win
        More?](#do-aggressive-teams-tend-to-win-more)
      - [Do Newer Teams Tend to be More
        Aggressive?](#do-newer-teams-tend-to-be-more-aggressive)
      - [How are Weak Goalies Affecting Teams’
        Performances?](#how-are-weak-goalies-affecting-teams-performances)

# Understanding JSON Data

## What is JSON data?

[JavaScript Object Notation (JSON)](https://www.json.org/json-en.html)
is a format of storing data that is “lightweight” (that is, tends to be
a small file size) and integrates well with most programming languages,
though it is technically a subset of JavaScript. These properties make
it a favorite format for application programming interfaces (APIs) and
is often used to [“serialize and transfer data over a network
connection”](https://www.w3resource.com/JSON/introduction.php). In
JSON, [data can be stored in a variety of different
ways](https://www.youtube.com/watch?v=iiADhChRriM&list=TLPQMTAwNjIwMjBzt1zaeUU28g&index=2),
including strings, numbers, booleans (with the true/false operators),
arrays, and null. While objects are wrapped with a left and right curly
brace ({}), arrays are enclosed in brackets (\[\]). Objects contain
[key-value pairs](https://www.w3schools.com/js/js_json_objects.asp),
with the key being a string, a value being one of the aformed JSON data
types, and a colon separating the two; each key-value pair is separated
with a comma. Values may be nested in a JSON data object, which makes it
both convenient and easy to read.

## Packages Used for Reading JSON Data

Three R packages can be utilized for working with JSON data:

  - `rjson`  
  - `RJSONIO`  
  - `jsonlite`

All three packages offer options for converting to/from JSON. `RJSONIO`
was originally introduced as a [faster
alternative](https://www.rdocumentation.org/packages/RJSONIO/versions/1.3-1.4)
to `rjson`, given concerns about the latter’s slower speeds, though
there are some indications that `rjson` may now actually be faster in
certain circumstances. I have selected `jsonlite`, which debuted in 2014
and originally [started as a branch of
`RJSONIO`](https://cran.r-project.org/web/packages/jsonlite/index.html#:~:text=In%20addition%20to%20converting%20JSON,data%20in%20systems%20and%20applications.),
as my package of choice. Documentation on the package is extensive, as
evidenced by the package’s thorough
[vignette](https://cran.r-project.org/web/packages/jsonlite/jsonlite.pdf);
and a wide variety of useful functions and options are within.
Interestingly, only `RJSONIO` and `jsonlite` [give control over vector
simplification](https://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html),
an important consideration when parsing JSON data.

# Contacting the NHL API

When I first tried converting the data from JSON, *“data.”* appeared at
the beginning of each column name; ostensibly, this points to the object
within which the array of data was stored in JSON. However, thinking
this a bit cumbersome for usage in later code, per Subhankar’s
recommendation, I played around with using
[`str_remove`](https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_remove)
to remove the string, and found it to be useful for keeping the column
names short and more enjoyable to work with.

## Creating a Function for Accessing Franchise Data

``` r
api_function_franchise <- function(x) {
  base_url <- "https://records.nhl.com/site/api" 
  x <- "franchise"
  full_url <- paste0(base_url, "/", x) 
  franchise_get <- GET(full_url) 
  franchise_txt <- content(franchise_get, "text") 
  franchise_json <- fromJSON(franchise_txt, flatten=TRUE, simplifyDataFrame=TRUE)
  franchise_df <- as.data.frame(franchise_json)
  colnames(franchise_df) <- str_remove(colnames(franchise_df), "data.") 
  return(franchise_df)
}
franchise_data <- api_function_franchise(x)
#view(franchise_data)
head(franchise_data)
```

    ##   id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName teamPlaceName
    ## 1  1      19171918           NA                8      Canadiens      Montréal
    ## 2  2      19171918     19171918               41      Wanderers      Montreal
    ## 3  3      19171918     19341935               45         Eagles     St. Louis
    ## 4  4      19191920     19241925               37         Tigers      Hamilton
    ## 5  5      19171918           NA               10    Maple Leafs       Toronto
    ## 6  6      19241925           NA                6         Bruins        Boston
    ##   total
    ## 1    38
    ## 2    38
    ## 3    38
    ## 4    38
    ## 5    38
    ## 6    38

## Creating a Function for Accessing Franchise Team Totals

``` r
api_function_totals <- function(x) {
  base_url <- "https://records.nhl.com/site/api" 
  x <- "franchise-team-totals"
  full_url <- paste0(base_url, "/", x) 
  team_totals_get <- GET(full_url) 
  team_totals_txt <- content(team_totals_get, "text") 
  team_totals_json <- fromJSON(team_totals_txt, flatten=TRUE)
  team_totals_df <- as.data.frame(team_totals_json)  
  colnames(team_totals_df) <- str_remove(colnames(team_totals_df), "data.") 
  return(team_totals_df)
}
team_totals_data <- api_function_totals(x)
#view(team_totals_data)
head(team_totals_data)
```

    ##   id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ## 1  1               1      19821983          23          2        2937
    ## 2  2               1      19821983          23          3         257
    ## 3  3               1      19721973          22          2        3732
    ## 4  4               1      19721973          22          3         272
    ## 5  5               1      19261927          10          2        6504
    ## 6  6               1      19261927          10          3         515
    ##   goalsAgainst goalsFor homeLosses homeOvertimeLosses homeTies homeWins
    ## 1         8708     8647        507                 82       96      783
    ## 2          634      697         53                  0       NA       74
    ## 3        11779    11889        674                 81      170      942
    ## 4          806      869         46                  1       NA       84
    ## 5        19863    19864       1132                 73      448     1600
    ## 6         1436     1400        103                  0        1      137
    ##   lastSeasonId losses overtimeLosses penaltyMinutes pointPctg points roadLosses
    ## 1           NA   1181            162          44397    0.5330   3131        674
    ## 2           NA    120              0           4266    0.0039      2         67
    ## 3           NA   1570            159          57422    0.5115   3818        896
    ## 4           NA    124              0           5356    0.0147      8         78
    ## 5           NA   2693            147          85564    0.5125   6667       1561
    ## 6           NA    263              0           8132    0.0000      0        160
    ##   roadOvertimeLosses roadTies roadWins shootoutLosses shootoutWins shutouts
    ## 1                 80      123      592             79           78      193
    ## 2                  0       NA       63              0            0       25
    ## 3                 78      177      714             67           82      167
    ## 4                  0       NA       64              0            0        9
    ## 5                 74      360     1256             66           78      403
    ## 6                  0        7      107              0            0       44
    ##   teamId           teamName ties triCode wins total
    ## 1      1  New Jersey Devils  219     NJD 1375   104
    ## 2      1  New Jersey Devils   NA     NJD  137   104
    ## 3      2 New York Islanders  347     NYI 1656   104
    ## 4      2 New York Islanders   NA     NYI  148   104
    ## 5      3   New York Rangers  808     NYR 2856   104
    ## 6      3   New York Rangers    8     NYR  244   104

## Creating a Function for Accessing Season Records

``` r
api_function_season <- function(ID) {
  base_url <- "https://records.nhl.com/site/api" 
  full_url <- paste0(base_url, "/", "franchise-season-records?cayenneExp=franchiseId=", ID) 
  season_get <- GET(full_url) 
  season_txt <- content(season_get, "text") 
  season_json <- fromJSON(season_txt, flatten=TRUE)
  season_df <- as.data.frame(season_json)
  colnames(season_df) <- str_remove(colnames(season_df), "data.") 
  return(season_df)
}
seasonCaps <- api_function_season(24) #Testing the function with Washington Capitals data, since I'm originally from the DC area!
#view(seasonCaps)
```

## Creating a Function for Accessing Goalie Records

``` r
api_function_goalie <- function(ID) {
  base_url <- "https://records.nhl.com/site/api" 
  full_url <- paste0(base_url, "/", "franchise-season-records?cayenneExp=franchiseId=", ID) 
  goalie_get <- GET(full_url) 
  goalie_txt <- content(goalie_get, "text") 
  goalie_json <- fromJSON(goalie_txt, flatten=TRUE)
  goalie_df <- as.data.frame(goalie_json)
  colnames(goalie_df) <- str_remove(colnames(goalie_df), "data.")
  return(goalie_df)
}
goalieCanes<- api_function_goalie(26) #Testing the function with Carolina Hurricanes data, to honor my new home. 
#view(goalieCanes)
```

## Creating a Function for Accessing Skater Records

``` r
api_function_skater <- function(ID) {
  base_url <- "https://records.nhl.com/site/api" 
  full_url <- paste0(base_url, "/", "franchise-season-records?cayenneExp=franchiseId=", ID) 
  skater_get <- GET(full_url) 
  skater_txt <- content(skater_get, "text") 
  skater_json <- fromJSON(skater_txt, flatten=TRUE)
  skater_df <- as.data.frame(skater_json)
  colnames(skater_df) <- str_remove(colnames(skater_df), "data.")
  return(skater_df)
}
skaterPenguins <- api_function_skater(17) #Testing the function - I'll look at some Penguins data too! 
#view(skaterPenguins)
```

# Exploratory Data Analysis

## Is a Team With More Recorded Shutouts More Likely to Still be an Active Franchise?

In a [shutout game](https://en.wikipedia.org/wiki/Shutout), a hockey
team’s defense manages to prevent the other team from scoring any points
throughout the entirety of the game - surely, a sign of the franchise’s
strong performance\! To begin, I decided to created a categorical
variable called **shutoutRange**, usuing the `mutate` function to create
ranges for total number of shutouts and `ifelse` to assign each value to
the appropriate level of the new variable. I then created a contingency
table to get a quick glimpse of how many active (and inactive)
franchises fell within each category. Not too surprisingly, none of the
teams that had a history of over a 100 shutouts were inactive. To
visualize this data, I created a barplot, filling the bars with color
based upon whether or not the franchise is active.

``` r
team_totals_data <- mutate(team_totals_data, shutoutRange = 
                             ifelse(shutouts %in% 0:50, "0-50",
                                ifelse(shutouts %in% 51:100, "51-100",
                                ifelse(shutouts %in% 101:150, "101-150", 
                                ifelse(shutouts >150, "151+", "")))))
team_totals_data$shutoutRange <- factor(team_totals_data$shutoutRange, levels=c("0-50", "51-100", "101-150","151+")) #Changed these values to factors so that they could be easily ordered for the barplot below. 
```

``` r
shutoutTable <- table(team_totals_data$activeFranchise, team_totals_data$shutoutRange)
kable(shutoutTable, caption="Table of Active or Non-Active Franchise vs. Total Number of Shutouts")
```

|   | 0-50 | 51-100 | 101-150 | 151+ |
| - | ---: | -----: | ------: | ---: |
| 0 |   15 |      3 |       0 |    0 |
| 1 |   54 |      4 |      12 |   16 |

Table of Active or Non-Active Franchise vs. Total Number of Shutouts

``` r
shutoutGraph<- ggplot(data=team_totals_data, aes(x=shutoutRange))
shutoutGraph + geom_bar(aes(fill=as.factor(activeFranchise))) + 
  labs(x="Total Number of Shutouts", title="Bar Plot of Number of Shutouts") + 
  scale_fill_discrete(name="Active Franchise?", labels=c("No","Yes")) + 
  coord_flip()
```

![](README_files/figure-gfm/Creating%20Bar%20Plot%20of%20Number%20of%20Shutouts,%20with%20Active%20Franchise-1.png)<!-- -->

## Are Teams more Likely to Win at Home than Away?

We’ve heard of home-“field” advantage - let’s see if the same can be
said of the rink. I began by creating another two variables:

  - **roadWinLossRatio**, AKA total number of wins on the road divided
    by total number of losses on the road.  
  - **homeWinLossRatio**, AKA total number of losses at home divided by
    total number of wins on the road.

*For this purposes of this variable, I did not include ties.*

``` r
team_totals_data <- mutate(team_totals_data, roadWinLossRatio = roadWins / roadLosses)
team_totals_data <- mutate(team_totals_data, homeWinLossRatio = homeWins / homeLosses)

team_totals_data %>%
  select(teamName, roadWins, roadLosses, roadWinLossRatio, homeWins, homeLosses, homeWinLossRatio) %>%
  arrange(teamName) %>%
  head()
```

    ##            teamName roadWins roadLosses roadWinLossRatio homeWins homeLosses
    ## 1     Anaheim Ducks       38         39        0.9743590       51         34
    ## 2     Anaheim Ducks      422        463        0.9114471      551        341
    ## 3   Arizona Coyotes       86        128        0.6718750      104        108
    ## 4    Atlanta Flames      107        156        0.6858974      161        104
    ## 5    Atlanta Flames        0          9        0.0000000        2          6
    ## 6 Atlanta Thrashers      159        233        0.6824034      183        204
    ##   homeWinLossRatio
    ## 1        1.5000000
    ## 2        1.6158358
    ## 3        0.9629630
    ## 4        1.5480769
    ## 5        0.3333333
    ## 6        0.8970588

``` r
roadWLR <- summary(team_totals_data$roadWinLossRatio)
roadWLR
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.4384  0.7130  0.6497  0.8788  1.2340

``` r
homeWLR <- summary(team_totals_data$homeWinLossRatio)
homeWLR
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.9069  1.3024  1.2541  1.5573  3.0000       1

Looking at the mean and median Win-Loss ratios for on the road (or
“away”) and home games, we see that both the home mean and median
ratios are nearly double that of away games - perhaps indicating that a
team is more likely to win when it’s playing on its own ice. We can then
look at boxplots of these ratios, as a visual representation of these
summary statistics. The distributions appear slightly skewed, with the
Road Win-Loss ratio being slightly skewed left, and the Home Win-Loss
Ratio appearing skewed right, likely due to its outlier. Looking at the
data, we can identify this outlier as the Toronto Arenas franchise (with
a game type ID of 2); however, we also note that they only played a
total of 20 games at home, winning 15 - giving them their extraordinary
Win-Loss Ratio of 3.0.

``` r
penalty_plot <- ggplot(team_totals_data, aes(x=roadWinLossRatio,y=""))
penalty_plot + 
  geom_boxplot(fill="#cc0000") +
  ggtitle("Boxplot of Ratio of Road Wins to Road Losses") +
  labs(y="")
```

![](README_files/figure-gfm/Creating%20Boxplot%20of%20roadWinToLossRatio-1.png)<!-- -->

``` r
penalty_plot <- ggplot(team_totals_data, aes(x=homeWinLossRatio,y=""))
penalty_plot + 
  geom_boxplot(fill="#cc0000") +
  ggtitle("Boxplot of Ratio of Home Wins to Home Losses") +
  labs(y="")
```

![](README_files/figure-gfm/Creating%20Boxplot%20of%20home%20WinLossRatio-1.png)<!-- -->

``` r
#team_totals_data %>% select(teamName, homeWinLossRatio) %>% view()
#We can see that the outlier, with a homeWinLossRatio of 3 is the Toronto Arenas. 
```

``` r
roadWinsLossesHist <- ggplot(team_totals_data, aes(x=roadWinLossRatio))
roadWinsLossesHist +
  geom_histogram(aes(y=..density..), binwidth=0.25) + 
  geom_density(kernal="gaussian")
```

    ## Warning: Ignoring unknown parameters: kernal

![](README_files/figure-gfm/Creating%20Histogram%20of%20Road%20Wins%20to%20Road%20Losses%20Ratio-1.png)<!-- -->

``` r
  ggtitle("Histogram of Road Wins to Road Losses Ratio")
```

    ## $title
    ## [1] "Histogram of Road Wins to Road Losses Ratio"
    ## 
    ## attr(,"class")
    ## [1] "labels"

``` r
homeWinsLossesHist <- ggplot(team_totals_data, aes(x=homeWinLossRatio))
homeWinsLossesHist +
  geom_histogram(aes(y=..density..), binwidth=0.25) + 
  geom_density(kernal="gaussian") + 
  ggtitle("Histogram of Home Wins to Home Losses Ratio")
```

![](README_files/figure-gfm/Creating%20Histogram%20of%20Home%20Wins%20to%20Home%20Losses%20Ratio-1.png)<!-- -->

## Do Aggressive Teams Tend to Win More?

Do teams that spend more time in the penalty box tend to win more? To
help explore this question, I created another new variable called
**penaltyGamesRatio** - the total number of minutes spent in the penalty
box, divided by the total number of games played. This variable is
intended to display the average amount of time per game a team’s players
spend in penalty - perhaps a larger number is indicative of a more
“aggressive”, edgy playing style.

``` r
team_totals_data <- mutate(team_totals_data, penaltyGamesRatio = penaltyMinutes / gamesPlayed)

team_totals_data %>%
  arrange(desc(penaltyGamesRatio)) %>%
  select(teamName, penaltyGamesRatio) %>%
  head()
```

    ##               teamName penaltyGamesRatio
    ## 1     Quebec Nordiques          30.42500
    ## 2    Atlanta Thrashers          28.75000
    ## 3       Atlanta Flames          27.11765
    ## 4     Hartford Whalers          25.97959
    ## 5       Toronto Arenas          25.14286
    ## 6 Winnipeg Jets (1979)          24.93548

I then calculated an **overall** Win-Loss ratio, by dividing total
number of wins by total number of losses. My hope here was to create a
standardized measure, controlling for the amount of time that a team had
been in operation, so that new teams and old teams could be compared
equitably. Upon printing the first six rows of the data, we see that the
Vegas Golden Knights - a team that just began in 2017 - has the highest
win-to-loss ratio.

``` r
team_totals_data <- mutate(team_totals_data, winLossRatio = wins / losses)
team_totals_data %>%
  select (teamName, winLossRatio) %>%
  arrange(desc(winLossRatio)) %>%
  head()
```

    ##               teamName winLossRatio
    ## 1 Vegas Golden Knights     1.662500
    ## 2      Edmonton Oilers     1.514286
    ## 3   Montréal Canadiens     1.512056
    ## 4         Dallas Stars     1.475661
    ## 5 Vegas Golden Knights     1.454545
    ## 6  Philadelphia Flyers     1.437369

I then created a categorical variable called **MoreWinsOrLosses**, using
`ifelse`. If a team won more than it lost (that is, its winLossRatio \>
1), then the value of MoreWinsOrLosses was designated as “More Wins”.
Otherwise, it was assigned a value of “More Losses”. *(For the purposes
of this project, I designated the scenario of an equal number of wins
and losses as “More Losses”.)*

``` r
team_totals_data <- mutate(team_totals_data, moreWinsOrLosses = ifelse(winLossRatio > 1, "More Wins", "More Losses"))
team_totals_data %>%
  select(teamName, winLossRatio, moreWinsOrLosses) %>%
  arrange(teamName) %>%
  head(10)
```

    ##              teamName winLossRatio moreWinsOrLosses
    ## 1       Anaheim Ducks    1.2191781        More Wins
    ## 2       Anaheim Ducks    1.2101990        More Wins
    ## 3     Arizona Coyotes    0.8050847      More Losses
    ## 4      Atlanta Flames    1.0307692        More Wins
    ## 5      Atlanta Flames    0.1333333      More Losses
    ## 6   Atlanta Thrashers    0.7826087      More Losses
    ## 7   Atlanta Thrashers    0.0000000      More Losses
    ## 8       Boston Bruins    1.3439464        More Wins
    ## 9       Boston Bruins    0.9907407      More Losses
    ## 10 Brooklyn Americans    0.5517241      More Losses

I also created two contingency tables using the new variable. Winning
teams appear to be both more likely to still be an active franchise, as
well as more likely to achieve shutout games.

``` r
shutoutTable <- table(team_totals_data$moreWinsOrLosses, team_totals_data$activeFranchise)
kable(shutoutTable, caption="Table of More Wins or Losses vs. Active or Non-Active Franchise")
```

|             |  0 |  1 |
| ----------- | -: | -: |
| More Losses | 15 | 44 |
| More Wins   |  3 | 42 |

Table of More Wins or Losses vs. Active or Non-Active Franchise

``` r
kable(table(team_totals_data$moreWinsOrLosses, team_totals_data$shutoutRange), caption="Contingency Table of More Wins or Losses vs. Shutout Range")
```

|             | 0-50 | 51-100 | 101-150 | 151+ |
| ----------- | ---: | -----: | ------: | ---: |
| More Losses |   53 |      2 |       2 |    2 |
| More Wins   |   16 |      5 |      10 |   14 |

Contingency Table of More Wins or Losses vs. Shutout Range

Looking at the resulting boxplot *(below)*, we can see that the median
amount of time spent in the penalty box by a “winning” team is greater
than that of a “losing” team. Yet interestingly, the winning team range
is much, much narrower than that of a losing team, and two losing teams
prove to be outliers, with one team spending an average of *over 30
minutes a game* in the penalty box.

``` r
penalty_plot <- ggplot(team_totals_data, aes(x=penaltyGamesRatio,y=moreWinsOrLosses))
penalty_plot + 
  geom_boxplot(fill="#cc0000") +
  ggtitle("Boxplot of Average Time Spent in Penalty Box") +
  labs (x="Ratio of Total Minutes in Penalty Box to Total Number of Games Played", y="'Winning' or 'Losing' Team?")
```

![](README_files/figure-gfm/Creating%20Boxplot%20of%20Average%20Time%20Spent%20in%20Penalty%20Box-1.png)<!-- -->

Looking at the scatterplot, there does not appear to be a straight-line
relationship between the amount of time that a team spends in the
penalty box and its Win-Loss ratio. Looking at the distribution of the
points, it seems that there may be a “sweet spot” - perhaps indicating
that making certain illegal or more aggressive plays that could be
risky, but rewarding.

``` r
ratiosScatter <- ggplot(team_totals_data, aes(x=penaltyGamesRatio, y=winLossRatio))
ratiosScatter +
  geom_point() +
  geom_smooth() + 
  ggtitle("Scatterplot of Average Time Spent in Penalty Box vs. Win-Loss Ratio") + 
  labs(x="Average Time Spent in Penalty Box", y="Win-Loss Ratio")
```

![](README_files/figure-gfm/Creating%20Scatterplot%20of%20the%20Penalty-Games%20Ratio%20and%20Win-Loss%20Ratio-1.png)<!-- -->

## Do Newer Teams Tend to be More Aggressive?

For this brief analysis, I filtered the dataset twice: once for years
after or equal to 2000 (season stored as 20002001), then for years
before 2000. I then took summary statistics for each of the ratios of
Penalty to Games. In this case, we see that the median and mean for
teams before 2000 is actually higher than that of teams that began in or
after 2000, perhaps indicating that newer teams are actually less likely
to be in the penalty box.

``` r
post2000Penalties<- team_totals_data %>% 
  filter(firstSeasonId >= 20002000) %>% 
  select(teamName, penaltyGamesRatio, firstSeasonId) %>% 
  arrange(desc(penaltyGamesRatio))
summary(post2000Penalties$penaltyGamesRatio)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   7.272   8.894  10.581  10.339  11.010  13.222

``` r
pre2000Penalties <- team_totals_data %>% 
  filter(firstSeasonId < 20002000) %>% 
  select(teamName, penaltyGamesRatio, firstSeasonId) %>% 
  arrange(desc(penaltyGamesRatio))
summary(pre2000Penalties$penaltyGamesRatio)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    4.50   11.91   14.20   14.83   17.11   30.43

## How are Weak Goalies Affecting Teams’ Performances?

The [Goals Against
Average](https://en.wikipedia.org/wiki/Goals_against_average) indicates
“the number of goals a goaltender allows per 60 minutes of playing
time” - that is, per one game. The column goalsAgainst in the dataset is
a cumulative measure, indicating the total number of Goals Against for
the team since its inception. We can average this as a “per game” value
by dividing the goalsAgainst by the total number of games played. After
creating that new variable, I then plotted this value against the total
Win-Loss ratio. Of course, there appears to be a weak negative
correlation between the two values, with r= -0.421: the larger the
number of goals that a team’s goalies have let in, the lower their
win-loss ratio. I set the colors of the values on the scatterplot by
whether or not a team is still an active franchise - indeed, the greater
the number of goals are let in, the lower the number of wins… and
seemingly, the less likely they are to still be an active franchise. The
active franchises appear to have a much lower Goals Against Ratio, and a
much higher Win-Loss ratio.

``` r
team_totals_data$activeFranchise <- as.logical(team_totals_data$activeFranchise)
team_totals_data <- mutate(team_totals_data, goalsAgainstRatio = goalsAgainst / gamesPlayed)
correlation <- cor(team_totals_data$goalsAgainstRatio, team_totals_data$winLossRatio)
correlation
```

    ## [1] -0.4209071

``` r
goalsAgainstLossesPlot <- ggplot(team_totals_data, aes(x=goalsAgainstRatio, y=winLossRatio, color=activeFranchise))
goalsAgainstLossesPlot + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle("Scatterplot of Goals Against vs. Win-Loss Ratio") +
  labs(x="Goals Against Ratio", y="Win-Loss Ratio") +
  scale_color_discrete(name="Active?", labels=c("No","Yes"))
```

![](README_files/figure-gfm/Creating%20Scatterplot%20of%20Goals%20Against%20vs.%20Losses-1.png)<!-- -->
