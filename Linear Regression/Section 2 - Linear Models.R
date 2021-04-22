
# 2.1 - Introduction to Linear Models -------------------------------------

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


# 2.2 - Least Squares Estimates ----------------------------------------
    # compute RSS for any pair of beta0 and beta1 in Galton's data
    library(tidyverse)
    library(HistData)
    data("GaltonFamilies")
    set.seed(1983)
    galton_heights <- GaltonFamilies %>%
      filter(gender == "male") %>%
      group_by(family) %>%
      sample_n(1) %>%
      ungroup() %>%
      select(father, childHeight) %>%
      rename(son = childHeight)
    rss <- function(beta0, beta1, data){
      resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
      return(sum(resid^2))
    }
    
    # plot RSS as a function of beta1 when beta0=25
    beta1 = seq(0, 1, len=nrow(galton_heights))
    results <- data.frame(beta1 = beta1,
                          rss = sapply(beta1, rss, beta0 = 25))
    results %>% ggplot(aes(beta1, rss)) + geom_line() + 
      geom_line(aes(beta1, rss))
    
    # fit regression line to predict son's height from father's height
    fit <- lm(son ~ father, data = galton_heights)
    fit
    
    # summary statistics
    summary(fit)
    
    # Monte Carlo simulation
    B <- 1000
    N <- 50
    lse <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>% 
        lm(son ~ father, data = .) %>% 
        .$coef 
    })
    lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
    
    # Plot the distribution of beta_0 and beta_1
    library(gridExtra)
    p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
    p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
    grid.arrange(p1, p2, ncol = 2)
    
    # summary statistics
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% 
      summary %>%
      .$coef
    
    lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
    
    lse %>% summarize(cor(beta_0, beta_1))
    
    B <- 1000
    N <- 50
    lse <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>%
        mutate(father = father - mean(father)) %>%
        lm(son ~ father, data = .) %>% .$coef 
    })
    
    cor(lse[1,], lse[2,]) 
    
    # plot predictions and confidence intervals
    galton_heights %>% ggplot(aes(father, son)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    # predict Y directly
    fit <- galton_heights %>% lm(son ~ father, data = .) 
    Y_hat <- predict(fit, se.fit = TRUE)
    names(Y_hat)
    
    # plot best fit line
    galton_heights %>%
      mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
      ggplot(aes(father, Y_hat))+
      geom_line()

    

  # Assignment 2.2 ----------------------------------------------------------
    
    ## Question 1 
    
    beta1 = seq(0, 1, len=nrow(galton_heights))
    results <- data.frame(beta1 = beta1,
                          rss = sapply(beta1, rss, beta0 = 36))
    results %>% ggplot(aes(beta1, rss)) + geom_line() + 
      geom_line(aes(beta1, rss), col=2)

    x <- which.min(results$rss)
    
    results$beta1[x]
    
    ## Question 3
    
    library(tidyverse)
    library(HistData)
    library(Lahman)
    data("GaltonFamilies")
    
    ## Version 1 - it was missing the mutate statement to account for the /G 
    
    teams_q3 <- Teams %>% filter(yearID %in% 1961:2001) %>% 
      mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G)
    # fit regression line to predict runs by walks and homeruns
    fit <- lm(R_per_game ~ BB_per_game + HR_per_game, data = teams_q3)
    fit
    
    # summary statistics
    summary(fit)
    
    ## Version 2  
    
    Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
    Teams_small %>% 
      mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
      do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))
    
    ## Question 4 
    
    B <- 1000
    N <- 100
    lse <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>% 
        lm(son ~ father, data = .) %>% .$coef 
    })
    
    lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
    
    ## Part 2 
    
    library(tidyverse)
    library(HistData)
    library(Lahman)
    
    set.seed(1989) #if you are using R 3.5 or earlier
    ## set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
    data("GaltonFamilies")
    options(digits = 3)    # report 3 significant digits
    
    female_heights <- GaltonFamilies %>%     
      filter(gender == "female") %>%     
      group_by(family) %>%     
      sample_n(1) %>%     
      ungroup() %>%     
      select(mother, childHeight) %>%     
      rename(daughter = childHeight)
    
    model <- lm(mother ~ daughter, data = female_heights)
    model 
    
    
    
    new <- data.frame( daughter = 65.5)
    
    prediction <- predict(model,new,interval = "prediction") 
    
    ## Baseball 
    
    library(tidyverse)
    library(HistData)
    library(Lahman)
    
    bat_02 <- Batting %>% filter(yearID == 2002) %>%
      mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
      filter(pa >= 100) %>%
      select(playerID, singles, bb)
    
    bat_99 <- Batting %>% filter(yearID %in% 1999:2001) %>%
      mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
      filter(pa >= 100) %>%
      select(playerID, singles, bb) %>% 
      group_by(playerID) %>% 
      summarise(mean_singles = mean(singles) , mean_bb = mean(bb))
    
    ms <- bat_99 %>% filter(mean_singles > 0.2) 
    
    nrow(ms)
    
    mb <- bat_99 %>% filter(mean_bb > 0.2) 
    
    nrow(mb)
    
    x <- inner_join(bat_02,bat_99)
    
    head(x)
    
    cor(x$singles,x$mean_singles)
    
    cor(x$bb,x$mean_bb)
    
    x %>% ggplot(aes(singles, mean_singles)) + 
      geom_point()
    
    x %>% ggplot(aes(bb, mean_bb)) + 
      geom_point()
    
    lm_s_02 <- lm( singles ~ mean_singles , data = x)
    
    lm_b_02 <- lm( bb ~ mean_bb , data = x)
    
    
    
    

    
    
    
    

# 2.3 - Tibbles, do and broom ---------------------------------------------
    library(tidyverse)
    library(Lahman)
    
    # stratify by HR
    dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(HR = round(HR/G, 1), 
             BB = BB/G,
             R = R/G) %>%
      select(HR, BB, R) %>%
      filter(HR >= 0.4 & HR<=1.2)
    
    # calculate slope of regression lines to predict runs by BB in different HR strata
    dat %>%  
      group_by(HR) %>%
      summarize(slope = cor(BB,R)*sd(R)/sd(BB))
    
    # use lm to get estimated slopes - lm does not work with grouped tibbles
    dat %>%  
      group_by(HR) %>%
      lm(R ~ BB, data = .) %>%
      .$coef
    
    # inspect a grouped tibble
    dat %>% group_by(HR) %>% head()
    dat %>% group_by(HR) %>% class()
    
    # inspect data frame and tibble
    Teams
    as_tibble(Teams)
    # Note that the function was formerly called as.tibble()
    
    # subsetting a data frame sometimes generates vectors
    class(Teams[,20])
    
    # subsetting a tibble always generates tibbles
    class(as_tibble(Teams[,20]))
    
    # pulling a vector out of a tibble
    class(as_tibble(Teams)$HR)
    
    # access a non-existing column in a data frame or a tibble
    Teams$hr
    as_tibble(Teams)$HR
    
    # create a tibble with complex objects
    tibble(id = c(1, 2, 3), func = c(mean, median, sd))
    
    # use do to fit a regression line to each HR stratum
    dat %>%  
      group_by(HR) %>%
      do(fit = lm(R ~ BB, data = .))
    
    # using do without a column name gives an error
    dat %>%
      group_by(HR) %>%
      do(lm(R ~ BB, data = .))
    
    # define a function to extract slope from lm
    get_slope <- function(data){
      fit <- lm(R ~ BB, data = data)
      data.frame(slope = fit$coefficients[2], 
                 se = summary(fit)$coefficient[2,2])
    }
    
    # return the desired data frame
    dat %>%  
      group_by(HR) %>%
      do(get_slope(.))
    
    # not the desired output: a column containing data frames
    dat %>%  
      group_by(HR) %>%
      do(slope = get_slope(.))
    
    # data frames with multiple rows will be concatenated appropriately
    get_lse <- function(data){
      fit <- lm(R ~ BB, data = data)
      data.frame(term = names(fit$coefficients),
                 estimate = fit$coefficients, 
                 se = summary(fit)$coefficient[,2])
    }
    
    dat %>%  
      group_by(HR) %>%
      do(get_lse(.))
    
    # use tidy to return lm estimates and related information as a data frame
    library(broom)
    fit <- lm(R ~ BB, data = dat)
    tidy(fit)
    
    # add confidence intervals with tidy
    tidy(fit, conf.int = TRUE)
    
    # pipeline with lm, do, tidy
    dat %>%  
      group_by(HR) %>%
      do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
      filter(term == "BB") %>%
      select(HR, estimate, conf.low, conf.high)
    
    # make ggplots
    dat %>%  
      group_by(HR) %>%
      do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
      filter(term == "BB") %>%
      select(HR, estimate, conf.low, conf.high) %>%
      ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_errorbar() +
      geom_point()
    
    # inspect with glance
    glance(fit)

  # Assignment 2.3  ---------------------------------------------------------

    library(tidyverse)
    library(HistData)
    library(broom)
    data("GaltonFamilies")
    set.seed(1) # if you are using R 3.5 or earlier
    ## set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
    galton <- GaltonFamilies %>%
      group_by(family, gender) %>%
      sample_n(1) %>%
      ungroup() %>% 
      gather(parent, parentHeight, father:mother) %>%
      mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
      unite(pair, c("parent", "child"))
    
    head(galton)
    
    galton %>% group_by(pair) %>%
      summarize(n = n())
    
    galton %>% 
      group_by(pair) %>%
      do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
      filter(term == "parentHeight")
    
    galton %>%
      group_by(pair) %>%
      summarize(cor = cor(parentHeight, childHeight)) 
    
    
    
    
    
# 2.4 - Regression and baseball -------------------------------------------

    library(tidyverse)
    library(HistData)
    library(broom)
    library(Lahman)
    
    # linear regression with two variables
    fit <- Teams %>% 
      filter(yearID %in% 1961:2001) %>% 
      mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
      lm(R ~ BB + HR, data = .)
    tidy(fit, conf.int = TRUE)
    
    # regression with BB, singles, doubles, triples, HR
    fit <- Teams %>% 
      filter(yearID %in% 1961:2001) %>% 
      mutate(BB = BB / G, 
             singles = (H - X2B - X3B - HR) / G, 
             doubles = X2B / G, 
             triples = X3B / G, 
             HR = HR / G,
             R = R / G) %>%  
      lm(R ~ BB + singles + doubles + triples + HR, data = .)
    coefs <- tidy(fit, conf.int = TRUE)
    coefs
    
    # predict number of runs for each team in 2002 and plot
    Teams %>% 
      filter(yearID %in% 2002) %>% 
      mutate(BB = BB/G, 
             singles = (H-X2B-X3B-HR)/G, 
             doubles = X2B/G, 
             triples =X3B/G, 
             HR=HR/G,
             R=R/G)  %>% 
      mutate(R_hat = predict(fit, newdata = .)) %>%
      ggplot(aes(R_hat, R, label = teamID)) + 
      geom_point() +
      geom_text(nudge_x=0.1, cex = 2) + 
      geom_abline()
    
    # average number of team plate appearances per game
    pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
      group_by(teamID) %>%
      summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
      pull(pa_per_game) %>% 
      mean
    
    # compute per-plate-appearance rates for players available in 2002 using previous data
    players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
      group_by(playerID) %>%
      mutate(PA = BB + AB) %>%
      summarize(G = sum(PA)/pa_per_game,
                BB = sum(BB)/G,
                singles = sum(H-X2B-X3B-HR)/G,
                doubles = sum(X2B)/G, 
                triples = sum(X3B)/G, 
                HR = sum(HR)/G,
                AVG = sum(H)/sum(AB),
                PA = sum(PA)) %>%
      filter(PA >= 300) %>%
      select(-G) %>%
      mutate(R_hat = predict(fit, newdata = .))
    
    # plot player-specific predicted runs
    qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))
    
    # add 2002 salary of each player
    players <- Salaries %>% 
      filter(yearID == 2002) %>%
      select(playerID, salary) %>%
      right_join(players, by="playerID")
    
    # add defensive position
    position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
    tmp_tab <- Appearances %>% 
      filter(yearID == 2002) %>% 
      group_by(playerID) %>%
      summarize_at(position_names, sum) %>%
      ungroup()  
    pos <- tmp_tab %>%
      select(position_names) %>%
      apply(., 1, which.max) 
    players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
      mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
      filter(POS != "P") %>%
      right_join(players, by="playerID") %>%
      filter(!is.na(POS)  & !is.na(salary))
    
    # add players' first and last names
    players <- Master %>%
      select(playerID, nameFirst, nameLast, debut) %>%
      mutate(debut = as.Date(debut)) %>%
      right_join(players, by="playerID")
    
    # top 10 players
    players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
      arrange(desc(R_hat)) %>% 
      top_n(10) 
    
    # players with a higher metric have higher salaries
    players %>% ggplot(aes(salary, R_hat, color = POS)) + 
      geom_point() +
      scale_x_log10()
    
    # remake plot without players that debuted after 1998
    library(lubridate)
    players %>% filter(year(debut) < 1998) %>%
      ggplot(aes(salary, R_hat, color = POS)) + 
      geom_point() +
      scale_x_log10()
    
    ## Regression Fallacy 
    
    library(Lahman)
    playerInfo <- Fielding %>%
      group_by(playerID) %>%
      arrange(desc(G)) %>%
      slice(1) %>%
      ungroup %>%
      left_join(Master, by="playerID") %>%
      select(playerID, nameFirst, nameLast, POS)
    
    ROY <- AwardsPlayers %>%
      filter(awardID == "Rookie of the Year") %>%
      left_join(playerInfo, by="playerID") %>%
      rename(rookie_year = yearID) %>%
      right_join(Batting, by="playerID") %>%
      mutate(AVG = H/AB) %>%
      filter(POS != "P")
    
    ROY <- ROY %>%
      filter(yearID == rookie_year | yearID == rookie_year+1) %>%
      group_by(playerID) %>%
      mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
      filter(n() == 2) %>%
      ungroup %>%
      select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
    
    ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
    ROY
    
    mean(ROY$sophomore - ROY$rookie <= 0)
    
    two_years <- Batting %>%
      filter(yearID %in% 2013:2014) %>%
      group_by(playerID, yearID) %>%
      filter(sum(AB) >= 130) %>%
      summarize(AVG = sum(H)/sum(AB)) %>%
      ungroup %>%
      spread(yearID, AVG) %>%
      filter(!is.na(`2013`) & !is.na(`2014`)) %>%
      left_join(playerInfo, by="playerID") %>%
      filter(POS!="P") %>%
      select(-POS) %>%
      arrange(desc(`2013`)) %>%
      select(nameFirst, nameLast, `2013`, `2014`)
    two_years
    
    arrange(two_years, `2013`)
    
    qplot(`2013`, `2014`, data = two_years)
    
    summarize(two_years, cor(`2013`,`2014`))
    
    library(dslabs)
    falling_object <- rfalling_object()
    
    falling_object %>%
      ggplot(aes(time, observed_distance)) +
      geom_point() +
      ylab("Distance in meters") +
      xlab("Time in seconds")
    
    fit <- falling_object %>%
      mutate(time_sq = time^2) %>%
      lm(observed_distance~time+time_sq, data=.)
    
    tidy(fit)
    
    augment(fit) %>%
      ggplot() +
      geom_point(aes(time, observed_distance)) +
      geom_line(aes(time, .fitted), col = "blue")
    
    tidy(fit, conf.int = TRUE)
    

  # Linear Programming ------------------------------------------------------

    library(reshape2)
    library(lpSolve)
    
    players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
    constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
    npos <- nrow(constraint_matrix)
    constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
    constraint_dir <- c(rep("==", npos), "<=")
    constraint_limit <- c(rep(1, npos), 50*10^6)
    lp_solution <- lp("max", players$R_hat,
                      constraint_matrix, constraint_dir, constraint_limit,
                      all.int = TRUE) 
    
    our_team <- players %>%
      filter(lp_solution$solution == 1) %>%
      arrange(desc(R_hat))
    our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)
    
    my_scale <- function(x) (x - median(x))/mad(x)
    players %>% mutate(BB = my_scale(BB), 
                       singles = my_scale(singles),
                       doubles = my_scale(doubles),
                       triples = my_scale(triples),
                       HR = my_scale(HR),
                       AVG = my_scale(AVG),
                       R_hat = my_scale(R_hat)) %>%
      filter(playerID %in% our_team$playerID) %>%
      select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
      arrange(desc(R_hat))
    
  # Assignment 2.4 ----------------------------------------------------------

    fit <- Teams %>% 
      filter(yearID == 1971 ) %>% 
      mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
      lm(R ~ BB + HR, data = .)
    tidy(fit, conf.int = TRUE)
    
    # Repeat the above exercise to find the effects of BB and HR on runs (R)
    #  for every year from 1961 to 2018 using do and the broom package.
    fit <- Teams %>% filter(yearID %in% 1961:2018) %>%
      group_by(yearID) %>%
      do(tidy(lm(R ~ BB + HR, data = .)))
    # Make a scatterplot of the estimate for the effect of BB on runs over time
    #  and add a trend line with confidence intervals.
    fit %>% filter(term == 'BB') %>% 
      select(yearID, estimate) %>%
      ggplot(aes(yearID, estimate)) + 
      geom_line() + geom_smooth(method="lm")
    
    # Fit a linear model on the results from Question 10 to determine the effect
    #  of year on the impact of BB.
    fit<- Teams %>% filter(yearID %in% 1961:2018) %>% 
      group_by(yearID) %>% 
      do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))
    
    fit2 <- fit %>% filter(term == "BB")
    fit3 <- lm(estimate ~ yearID, data= fit2)
    
    glance(fit3)
    
# Final Assignment Section 2 ----------------------------------------------

    library(tidyverse)
    library(broom)
    library(Lahman)
    Teams_small <- Teams %>% 
      filter(yearID %in% 1961:2001) %>% 
      mutate(avg_attendance = attendance/G)
    
    fit <- Teams_small %>% 
      mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
      lm(avg_attendance ~ HR , data = .)
    tidy(fit, conf.int = TRUE)
    
    fit <- Teams_small %>%  
      lm(avg_attendance ~ W , data = .)
    tidy(fit, conf.int = TRUE)
    
    fit <- Teams_small %>%  
      lm(avg_attendance ~ yearID , data = .)
    tidy(fit, conf.int = TRUE)
    
    # What is the correlation coefficient for wins and runs per game?
    corr <- Teams_small %>% mutate(rpg = R/G)
    cor(x = corr$W, y = corr$rpg)
    
    # What is the correlation coefficient for wins and home runs per game?
    corr <- Teams_small %>% mutate(hrpg = HR/G)
    cor(x = corr$W, y = corr$hrpg)
    
    
    ###################### QUESTION 3 a/b/c #####################
    # Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest
    #  integer. Keep only strata 5 through 10, which have 20 or more data points.
    strat_teams <- Teams_small %>%
      mutate(strat = round(W/10)) %>%
      group_by(strat) %>%
      filter(strat %in% 5:10)
    
    # How many observations are in the 8 win strata?
    strat_teams %>% filter(strat == 8) %>% count()
    
    
    # Calculate the slope of the regression line predicting average attendance given
    #  runs per game for each of the win strata. Which win stratum has the largest
    #  regression line slope?
    strat_teams %>%
      mutate(rpg = R/G) %>%
      do(tidy(lm(avg_attendance ~ rpg, data = .), conf.int = TRUE)) %>%
      filter(term == "rpg")
  
    
    # Calculate the slope of the regression line predicting average attendance given
    #  HR per game for each of the win strata. Which win stratum has the largest
    #  regression line slope?
    
    strat_teams %>%
      mutate(hrpg = HR/G) %>%
      do(tidy(lm(avg_attendance ~ hrpg, data = .), conf.int = TRUE)) %>%
      filter(term == "hrpg")
    
    ###################### QUESTION 4 a #####################
    # Fit a multivariate regression determining the effects of runs per game,
    #  home runs per game, wins, and year on average attendance. Use the original
    #  Teams_small wins column, not the win strata from question 3.
    
    # build model
    fit <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) %>%
      lm(avg_attendance ~ rpg + hrpg + W + yearID, data = .)
    
    # first prediction
    guess <- data.frame(
      rpg = 5,
      hrpg = 1.2,
      W = 80,
      yearID = 2002
    )
    predict(fit, guess)
    
    # second prediction
    guess <- data.frame(
      rpg = 5,
      hrpg = 1.2,
      W = 80,
      yearID = 1960
    )
    predict(fit, guess)
    
    
    ###################### QUESTION 5 a#####################
    # Use your model from Question 4 to predict average attendance for teams in 2002
    #  in the original Teams data frame.
    
    pred_teams <- Teams %>% filter(yearID == 2002) %>%
      mutate(rpg = R/G, hrpg = HR/G) %>%
      mutate(pred_attendance = predict(fit, data.frame(rpg = rpg, hrpg = hrpg, W = W, yearID = yearID)))
    
    cor(pred_teams$attendance, pred_teams$pred_attendance)
    
    
    