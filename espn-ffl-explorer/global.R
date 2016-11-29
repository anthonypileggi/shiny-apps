require(rvest)
require(magrittr)
require(ggplot2)
require(plotly)

## Load data from an espn fantasy football league -- current season only
getLeagueData <- function (leagueID=431416, max_weeks=NULL) {
  
  mysite <- xml2::read_html(paste0("http://games.espn.go.com/ffl/scoreboard",
                                   "?leagueId=", leagueID, 
                                   "&scoringPeriodId=1"))
  
  ## number of weeks
  nweeks <- mysite %>% html_nodes("b+ b , .bodyCopy a") %>% html_text() %>% as.numeric() %>% max()
  if (!is.null(max_weeks))  nweeks <- ifelse(nweeks>max_weeks, max_weeks, nweeks)
  
  
  for (i in 1:nweeks) {
    
    cat("Week",i,"out of",nweeks,"\n")
    
    ## read webpage
    mysite <- xml2::read_html(paste0("http://games.espn.go.com/ffl/scoreboard",
                                      "?leagueId=", leagueID, 
                                      "&scoringPeriodId=", i))
    
    ## grab teams & scores
    teams <- mysite %>% html_nodes(".name a") %>% html_text()
    n <- length(teams)
    scores <- mysite %>% html_nodes(".score") %>% html_text() %>% as.numeric()
    
    ## organize data
    tmp <- list(results = data.frame(team=teams,
                                     week=i,
                                     score=scores),
                matchups = data.frame(week=i,
                                     team1=factor(teams[seq(1,n,by=2)],levels=teams),
                                     team2=factor(teams[seq(2,n,by=2)],levels=teams),
                                     score1=scores[seq(1,n,by=2)],
                                     score2=scores[seq(2,n,by=2)]) %>%
                              dplyr::mutate(winner=ifelse(score1>score2,
                                                          as.character(team1), as.character(team2)),
                                            winner=factor(winner,levels=teams),
                                            loser=ifelse(score1<score2,
                                                         as.character(team1), as.character(team2)),
                                            loser=factor(loser,levels=teams),
                                            completed=ifelse(score1==0 & score2==0, 0, 1)))
    if (i==1) {
      out <- tmp
      
    } else {
      out$results <- rbind(out$results, tmp$results)
      out$matchups <- rbind(out$matchups, tmp$matchups)
    }
  }
  return(out)
}


## get current week (based on totals) -- TODO: fix edge-cases (or sync w/ calendar?)
getCurrentWeek <- function (x) {
  
  totals <- x$results %>% dplyr::group_by(week) %>% 
                          dplyr::summarize(score=sum(score))
  next_week <- which(totals$score==0)[1]
  if (is.na(next_week)) {
    cur_week <- max(x$matchups$week)
  } else if (totals$score[next_week-1] > 0 & totals$score[next_week-1] < 1300) {
    cur_week <- next_week-1
  } else {
    cur_week <- next_week
  }
  return(cur_week)
}


## Win-loss record
makeRecord <- function (x, cur_week) {
  
  dplyr::left_join(x %>% dplyr::filter(week < cur_week) %>%
                         dplyr::group_by(Team=winner) %>% 
                         dplyr::summarize(Wins=n()),
                   x %>% dplyr::filter(week < cur_week) %>%
                         dplyr::group_by(Team=loser) %>% 
                         dplyr::summarize(Losses=n()),
                   by="Team") %>%
    dplyr::mutate(Wins = ifelse(is.na(Wins), 0, Wins),
                  Losses = ifelse(is.na(Losses), 0, Losses)) %>%
    dplyr::arrange(desc(Wins))
  
}


## Full league standings
makeLeagueStandings <- function (x, cur_week=NULL) {
  
  if (is.null(cur_week))  cur_week <- getCurrentWeek(x)
  
  makeRecord(x$matchups, cur_week) %>%
    dplyr::left_join(x$results %>% dplyr::filter(week < cur_week) %>%
                                   dplyr::group_by(Team=team) %>% 
                                   dplyr::summarize(Score=round(sum(score))),
                     by="Team") %>%
    dplyr::arrange(desc(Wins))
}


## Power Rankings ---  input: getLeagueData()$results
makePowerRankings <- function (x, cur_week=NULL) {

  if (is.null(cur_week))  cur_week <- getCurrentWeek(x)
  
  teams <- unique(x$results$team)
  n <- length(teams)
  id <- combn(n,2)
  for (i in 1:cur_week) {
    x1 <- x$results %>% dplyr::filter(week==i)
    id1 <- id[1,]
    id2 <- id[2,]
    tmp <- data.frame(week=i,
                       team1=x1$team[id1],
                       team2=x1$team[id2],
                       score1=x1$score[id1],
                       score2=x1$score[id2]) %>%
              dplyr::mutate(winner=ifelse(score1>score2,
                                          as.character(team1), as.character(team2)),
                            winner=factor(winner,levels=teams),
                            loser=ifelse(score1<score2,
                                            as.character(team1), as.character(team2)),
                            loser=factor(loser,levels=teams),
                            completed=ifelse(score1==0 & score2==0, 0, 1))
    if (i==1) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
  }
  
  return(makeRecord(out, cur_week))
}
