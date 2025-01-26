games = read.csv('/Users/ferasoo/Downloads/games.csv')
#to_remove = c('id', 'increment_code', 'white_id', 'black_id', 'moves', 'opening_eco' )
to_remove = c('id', 'increment_code', 'white_id', 'black_id', 'moves', 'opening_eco', 'opening_name' , 'opening_ply')

#drop unused columns
games = games[ , !(names(games) %in% to_remove)]


#games$game_length = as_datetime(games$last_move_at/1000) - as_datetime(games$created_at/1000)


games$game_length = games$last_move_at/1000 - games$created_at/1000
games = games[9300:nrow(games),]
row.names(games) <- NULL
nrow(games)
View(games)

#convert UNIX timestamp to date

#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("DescTools")
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(DescTools)
library(corrplot)
library("BSDA")
library(randtests)


games$avg_rating = (games$white_rating + games$black_rating) / 2
games$game_length = games$last_move_at/1000 - games$created_at/1000
games = games[9300:nrow(games),]

games$rated[games$rated == "False" | games$rated == "FALSE"] = FALSE
games$rated[games$rated == "True" | games$rated == "TRUE"] = TRUE

head(games,2)
View(games)

SAMPLE_SIZE = 150
alpha = 0.05


# systematic_sampling <- function(data, n) {
#   
#   total_obs <- nrow(data)  # Total number of observations
#   
#   sampling_interval <- floor(total_obs / n)  # Calculate the sampling interval
#   
#   start_point <- sample(1:sampling_interval, 1)  # Generate a random starting point
#   
#   sample_indices <- seq(start_point, total_obs, by = sampling_interval)  # Perform systematic sampling
#   sample_data <- data[sample_indices, ]  # Select the samples
#   
#   return(sample_data)
# }

systematic_sampling <- function(data, n) {
  
  total_obs <- nrow(data)  # Total number of observations
  
  sampling_interval <- ceiling(total_obs / n)  # Calculate the sampling interval
  
  start_point <- sample(1:sampling_interval, 1)  # Generate a random starting point
  
  sample_indices <- seq(start_point, start_point + sampling_interval*(n-1), by = sampling_interval)  # Perform systematic sampling
  sample_data <- data[sample_indices, ]  # Select the samples
  
  return(sample_data)
}

total_obs


nrow(gamesSample2)


set.seed(2004)
gamesSample = systematic_sampling(games, SAMPLE_SIZE)

View(gamesSample)
#sample plots

hist(gamesSample$avg_rating[gamesSample$rated==TRUE], main='rated games', xlab = 'avg_rating', ylab = 'count', col='red')
hist(gamesSample$avg_rating[gamesSample$rated==FALSE], main='casual games', xlab = 'avg_rating', ylab = 'count', col='blue')

hist(gamesSample$turns, col='red', main='number of turns', xlab='turns', ylab = 'count')



clyinders3 = table(gamesSample$rated)
barplot(clyinders3, col='blue',xlab='rated', ylab = 'count')

clyinders4 = table(gamesSample$winner)
barplot(clyinders4, col='blue',xlab='winner', ylab = 'count')

clyinders5 = table(gamesSample$victory_status)
barplot(clyinders5, col='blue', xlab='victory_status', ylab = 'count')

boxplot(gamesSample$avg_rating, col='red', xlab='avg_rating')
boxplot(gamesSample$turns, col='red', xlab='turns')
boxplot(gamesSample$game_length, col='red', xlab='game_length(s)', ylim=c(0,3500))

hist(games$game_length[games$game_length < 5000], col='purple', xlab='avg_rating')



#population plots

hist(games$avg_rating[games$rated==TRUE], main='rated games', xlab = 'avg_rating', ylab = 'count', col='red')
hist(games$avg_rating[games$rated==FALSE], main='casual games', xlab = 'avg_rating', ylab = 'count', col='blue')

clyinders2 = table(games$turns)
barplot(clyinders2, col='red',xlab='turns', ylab = 'count')
#hist(games$turns, col='red', main='number of turns', xlab='turns', ylab = 'count')




clyinders3 = table(games$rated)
barplot(clyinders3, col='blue',xlab='rated', ylab = 'count')

clyinders4 = table(games$winner)
barplot(clyinders4, col='blue',xlab='winner', ylab = 'count')

clyinders5 = table(games$victory_status)
barplot(clyinders5, col='blue', xlab='victory_status', ylab = 'count')

boxplot(games$avg_rating, col='red', xlab='avg_rating')
boxplot(games$turns, col='red', xlab='turns')
boxplot(games$game_length, col='red', xlab='game_length(s)', ylim=c(0,3500))

hist(games$game_length[games$game_length < 5000], col='purple', xlab='avg_rating')



#opening<-filter(summarise(group_by(games,opening_name), count=length(opening_name)),count>200)
#ggplot(opening,aes(x=opening_name,y=count))+geom_col()+coord_flip()+theme_classic()

#ggplot(games,aes(x=toupper(rated)))+geom_bar()+xlab(label = "Rated_Games")+ylab(label = "Frequency")+theme_classic()


# calculate_match_time <- function(increment_code, turns) {
#   increment <- strsplit(increment_code, split = "\\+")[[1]]
#   main_time <- as.numeric(increment[1])
#   increment_time <- as.numeric(increment[2])
#   
#   # Convert main_time and increment_time to desired units (e.g., seconds)
#   main_time <- main_time * 60 * 2
#   increment_time <- increment_time
#   
#   # Calculate the total time spent on increments
#   total_increment_time <- increment_time * turns
#   
#   # Calculate the total match time
#   match_time <- main_time + total_increment_time
#   
#   return(match_time / 60)
# }


# Apply the calculate_match_time function to each row in the data frame
# games$max_time <- mapply(calculate_match_time, games$increment_code, games$turns)
# 
# mean(games$max_time)


#Pearson’s Coefficient (PC) of Skewness

qqnorm(gamesSample$avg_rating,col='purple', main='avg_rating normal probability plot')
3*(mean(gamesSample$avg_rating) - median(gamesSample$avg_rating)) / sd(gamesSample$avg_rating)

qqnorm(gamesSample$turns,col='purple', main='turns normal probability plot')
3*(mean(gamesSample$turns) - median(gamesSample$turns)) / sd(gamesSample$turns)

qqnorm(gamesSample$opening_ply,col='purple', main='opening_ply normal probability plot')
3*(mean(gamesSample$opening_ply) - median(gamesSample$opening_ply)) / sd(gamesSample$opening_ply)


# Point estimation

pop_sd = function(data){
  return(sqrt(var(data) * ((length(data) - 1) / length(data))))
}

cat("mean population game_length:", mean(games$game_length), ", mean sample game_length:",mean(gamesSample$game_length))
cat("std population game_length:", pop_sd(games$game_length), ", std sample game_length:",sd(gamesSample$game_length))

cat("mean population opening_ply:", mean(games$opening_ply), ", mean sample opening_ply:",mean(gamesSample$opening_ply))
cat("std population opening_ply:", pop_sd(games$opening_ply), ", std sample opening_ply:",sd(gamesSample$opening_ply))

cat("mean population avg_rating:", mean(games$avg_rating), ", mean sample avg_rating:",mean(gamesSample$avg_rating))
cat("std population avg_rating:", pop_sd(games$avg_rating), ", std sample avg_rating:",sd(gamesSample$avg_rating))

cat("mean population turns:", mean(games$turns), ", mean sample turns:",mean(gamesSample$turns))
cat("std population turns:", pop_sd(games$turns), ", std sample turns:",sd(gamesSample$turns))

# turns confidence interval
turns_sample_mean = mean(gamesSample$turns)
turns_sample_sd = sd(gamesSample$turns)
turns_sample_se = turns_sample_sd / sqrt(SAMPLE_SIZE)

t_score = qt(p=1-alpha/2, df=SAMPLE_SIZE-1)
margin_error <- t_score * turns_sample_se
lower_bound <- turns_sample_mean - margin_error
upper_bound <- turns_sample_mean + margin_error
cat('cl=',1-alpha,lower_bound,'< turns mean < ', upper_bound)

# avg_rating
avg_rating_sample_mean = mean(gamesSample$avg_rating)
avg_rating_sample_sd = sd(gamesSample$avg_rating)
avg_rating_sample_se = turns_sample_sd / sqrt(SAMPLE_SIZE)

t_score = qt(p=1-alpha/2, df=SAMPLE_SIZE-1,)
margin_error <- t_score * avg_rating_sample_se
lower_bound <- avg_rating_sample_mean - margin_error
upper_bound <- avg_rating_sample_mean + margin_error
cat('cl=',1-alpha,lower_bound,'< avg_rating< ', upper_bound)
mean(games$avg_rating)

# Confidence Intervals for Proportions
p_hat = table(gamesSample['rated'])[[1]] / SAMPLE_SIZE
q_hat = 1 - p_hat
SE = sqrt(p_hat*q_hat/SAMPLE_SIZE)
moe = qnorm(1-alpha/2) * SE
cat('cl=',1-alpha,p_hat - moe,' < proportion of rated games < ',p_hat + moe)

# Confidence Intervals for Variances and Standard Deviations
df = SAMPLE_SIZE - 1
chi_right = qchisq(1-alpha/2,df)
chi_left = qchisq(alpha/2,df)
cat('cl=',1-alpha,sqrt(df/chi_right) * sd(games$white_rating),' < sigma < ',sqrt(df/chi_left) * sd(games$white_rating))



# Hypotheses tests

# H_0: un-rated and rated player have the same numbers of turns
# H_a: un-rated and rated player have different numbers of turns
rated_false = systematic_sampling(games[games$rated == FALSE,], SAMPLE_SIZE)
rated_true= systematic_sampling(games[games$rated == TRUE,], SAMPLE_SIZE)
t.test(rated_false$turns, rated_true$turns)

t.test(gamesSample$turns, mu = 80, alternative = 'less')

mate_sample = systematic_sampling(games[games$victory_status == 'mate',], SAMPLE_SIZE)
resign_sample = systematic_sampling(games[games$victory_status == "resign",], SAMPLE_SIZE)
t.test(mate_sample$turns, resign_sample$turns)


white_turns = systematic_sampling(games[games$winner == 'white',], SAMPLE_SIZE)
black_turns = systematic_sampling(games[games$winner == "black",], SAMPLE_SIZE)
var.test(white_turns$turns, black_turns$turns)

ss1 = systematic_sampling(games[games$victory_status == 'mate',], SAMPLE_SIZE)
ss2 = systematic_sampling(games[games$victory_status == "resign",], SAMPLE_SIZE)
var.test(ss1$turns, ss2$turns)

table(gamesSample$winner)
black_win_t = table(gamesSample$winner)[[1]]
white_win_t = table(gamesSample$winner)[[3]]

white_wins = sum(gamesSample$winner == "white")
black_wins = sum(gamesSample$winner == "black")
prop.test(x = white_wins, n = (white_wins+black_wins), p = 0.5)










cot_v <- unlist(lapply(games, is.numeric), use.names = FALSE)  
corrplot(cor(games[,cot_v]),method = "circle", type = "upper", order = "AOE")


#first model
cor(gamesSample$white_rating,gamesSample$black_rating)

model = lm(gamesSample$white_rating~gamesSample$black_rating)
cooksd <- cooks.distance(model)  
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/SAMPLE_SIZE, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/SAMPLE_SIZE, names(cooksd),""), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > (4/SAMPLE_SIZE))])
new_gamesSample <- gamesSample[-influential, ]

cor(new_gamesSample$white_rating,new_gamesSample$black_rating)

a <- new_gamesSample$white_rating
x <- new_gamesSample$black_rating
new_model = lm(a~x)
predict(new_model, data.frame(x=c(1200, 2000, 1500)))

plot(new_gamesSample$white_rating, new_gamesSample$black_rating, main = "Linear Regression", xlab = "x", ylab = "y")
abline(new_model, col = "red")

plot(new_model, which = 1)  # Residuals vs. Fitted values plot



# second model
plot(gamesSample$turns, gamesSample$game_length, main = "Before", xlab = "x", ylab = "y")

plot(new_gamesSample$game_length,new_gamesSample$turns, main = "Before", ylab = "turns", xlab = "game_length")
cor(gamesSample$turns, gamesSample$game_length)

plot(log(new_gamesSample$game_length),log(new_gamesSample$turns), main = "After", ylab = "log(turns)", xlab = "log(game_legth)")
cor(log(gamesSample$turns),log(gamesSample$game_length))

a <- log(new_gamesSample$turns)
x <- log(new_gamesSample$game_length)
model2 = lm(a~x)
exp(predict(model2, data.frame(x=c(log(1500), log(3000), log(5000)))))

plot(log(new_gamesSample$game_length),log(new_gamesSample$turns), main = "Linear Regression", ylab = "log(turns)", xlab = "log(game_legth)")
abline(model2, col = "red")


# Add the regression line
#abline(model, col = "red")


# Godness-of-fit
#observed
table(gamesSample$winner)
b = table(gamesSample$winner)[[1]]
d = table(gamesSample$winner)[[2]]
w = table(gamesSample$winner)[[3]]
o = c(w,b,d)
#expected
p = c(0.2885,0.18,0.5315)
result = chisq.test(o,p=p)
result
result$expected
result$observed

# Create a first line
plot(result$expected, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "winner", ylab = "count", ylim = c(10,80), xaxt = "n",)
axis(1,                         # Define x-axis manually
     at = 1:3,
     labels = c('white', 'black', 'draw'))
# Add a second line
lines(result$observed, pch = 18, col = "blue", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c("expected", "observed"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


lineplt
# independence test
table(gamesSample$rated, gamesSample$victory_status)
chisq.test(table(gamesSample$rated, gamesSample$victory_status))

# homogeneity test
sample1 = systematic_sampling(games[games$rated == FALSE, ], SAMPLE_SIZE)
sample2 = systematic_sampling(games[games$rated == TRUE, ], SAMPLE_SIZE)
sample3 = rbind(sample1, sample2)
table(sample3$rated, sample3$winner)
chisq.test(table(sample3$rated, sample3$winner))

table(games$rated, games$victory_status)
chisq.test(table(games$rated, games$victory_status))

#ANOVA

# one-way
gamesSample$victory_status = ordered(gamesSample$victory_status, levels = c("outoftime", "resign", "mate", "draw"))
anova_t1 = aov(turns~victory_status, data=gamesSample)
summary(anova_t1)

#ScheffeTest 
#TukeyHSD(anova_t1)

#perform Scheffe's test
ScheffeTest(anova_t1)


#two-way
anova_t2 = aov(turns~winner*rated, data=gamesSample)
summary(anova_t2)



wilcox.test(turns~rated, data = gamesSample)
wilcox.test(gamesSample$turns, gamesSample$avg_rating)
wilcox.test(sample1$turns, sample2$turns)

plot(gamesSample$turns, gamesSample$avg_rating)
View(gamesSample)

# non-parametric test
#hist(games$opening_ply, main='rated games', xlab = 'turns', col='red')
#median(games$opening_ply)

s2 = systematic_sampling(games, 20)
SIGN.test(s2$opening_ply ,md=8, alternative = 'less')

wilcox.test(s2$opening_ply[s2$rated == TRUE],s2$opening_ply[s2$rated == FALSE])

cor.test(s2$opening_ply, s2$game_length , method = "spearman", exact = FALSE)

runs_test_sample = games$opening_ply[1:20] %% 2
runs_test_sample
runs.test(runs_test_sample)



View(gamesSample)

mean()