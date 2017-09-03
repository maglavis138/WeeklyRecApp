

library(MASS)
library(fitdistrplus)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

plot_histogram <- function(data){
  
  hist(data, # histogram
       col = "peachpuff", # column color
       border = "black", 
       prob = TRUE, # show densities instead of frequencies
       # xlim = c(36,38.5),
       # ylim = c(0,3),
       xlab = "Variable",
       main = "Articles")
  lines(density(data), # density plot
        lwd = 2, # thickness of line
        col = "chocolate3")
  
  abline(v = mean(data),
         col = "royalblue",
         lwd = 2)
  
  abline(v = median(data),
         col = "red",
         lwd = 2)
  
  legend(x = "topright", # location of legend within plot area
         c("Density plot", "Mean", "Median"),
         col = c("chocolate3", "royalblue", "red"),
         lwd = c(2, 2, 2))
}



article_data <- DataArticles[which(DataArticles$date >= "2017-07-01" & DataArticles$date < "2017-08-01" & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks
video_data <- DataVideos[which(DataVideos$date >= "2017-05-01" & DataVideos$date < "2017-08-01" & DataVideos$video_meme == 0),]$post_video_views
video_meme_data <- DataVideos[which(DataVideos$date >= "2017-05-01" & DataVideos$date < "2017-08-01" &  DataVideos$video_meme == 1),]$post_video_views

article_data <- remove_outliers(DataArticles[which(DataArticles$date >= "2016-01-01" & DataArticles$repost == 0),]$post_reach)[!is.na(remove_outliers(DataArticles[which(DataArticles$date >= "2016-01-01" & DataArticles$repost == 0),]$post_reach))]

video_data <- remove_outliers(DataVideos[which(DataVideos$date >= "2016-01-01" & DataVideos$repost == 0 & DataVideos$video_meme == 0),]$post_reach)[!is.na(remove_outliers(DataVideos[which(DataVideos$date >= "2016-01-01" & DataVideos$repost == 0 & DataVideos$video_meme == 0),]$post_reach))]

video_meme_data <- remove_outliers(DataVideos[which(DataVideos$date >= "2016-01-01" & DataVideos$repost == 0 & DataVideos$video_meme == 1),]$post_reach)[!is.na(remove_outliers(DataVideos[which(DataVideos$date >= "2016-01-01" & DataVideos$repost == 0 & DataVideos$video_meme == 1),]$post_reach))]

photo_data <- remove_outliers(DataPhotos[which(DataPhotos$date >= "2016-01-01" & DataPhotos$repost == 0),]$post_reach)[!is.na(remove_outliers(DataPhotos[which(DataPhotos$date >= "2016-01-01" & DataPhotos$repost == 0),]$post_reach))]


plotdist(article_data, histo = TRUE, demp = TRUE)

fln_articles <- fitdist(article_data, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("lognormal")
denscomp(list(fln_articles), legendtext = plot.legend)
qqcomp(list(fln_articles), legendtext = plot.legend)
cdfcomp(list(fln_articles), legendtext = plot.legend)
ppcomp(list(fln_articles), legendtext = plot.legend)


plotdist(log(article_data), histo = TRUE, demp = TRUE)

n_articles <- fitdist(log(article_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_articles), legendtext = plot.legend)
qqcomp(list(n_articles), legendtext = plot.legend)
cdfcomp(list(n_articles), legendtext = plot.legend)
ppcomp(list(n_articles), legendtext = plot.legend)

exp(qnorm(0.05, mean = n_articles$estimate[1], sd = n_articles$estimate[2]))
exp(qnorm(0.95, mean = n_articles$estimate[1], sd = n_articles$estimate[2]))

1 - pnorm(log(median(article_data)), mean = n_articles$estimate[1], sd = n_articles$estimate[2])

set.seed(0)

for(i in 1:10000){
  monte_carlo_articles[i] <- sum(exp(rnorm(155, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
}

plotdist((monte_carlo_articles), histo = TRUE, demp = TRUE)
sd(monte_carlo_articles)
  
plotdist(log(monte_carlo_articles), histo = TRUE, demp = TRUE)

n_monte_carlo_articles <- fitdist(log(monte_carlo_articles), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_monte_carlo_articles), legendtext = plot.legend)
qqcomp(list(n_monte_carlo_articles), legendtext = plot.legend)
cdfcomp(list(n_monte_carlo_articles), legendtext = plot.legend)
ppcomp(list(n_monte_carlo_articles), legendtext = plot.legend)

exp(qnorm(0.05, mean = n_monte_carlo_articles$estimate[1], sd = n_monte_carlo_articles$estimate[2]))
exp(qnorm(0.95, mean = n_monte_carlo_articles$estimate[1], sd = n_monte_carlo_articles$estimate[2]))



plotdist(video_data, histo = TRUE, demp = TRUE)

fln_videos <- fitdist(video_data, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("lognormal")
denscomp(list(fln_videos), legendtext = plot.legend)
qqcomp(list(fln_videos), legendtext = plot.legend)
cdfcomp(list(fln_videos), legendtext = plot.legend)
ppcomp(list(fln_videos), legendtext = plot.legend)


plotdist(log(video_data), histo = TRUE, demp = TRUE)

n_videos <- fitdist(log(video_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_videos), legendtext = plot.legend)
qqcomp(list(n_videos), legendtext = plot.legend)
cdfcomp(list(n_videos), legendtext = plot.legend)
ppcomp(list(n_videos), legendtext = plot.legend)

exp(qnorm(0.05, mean = n_videos$estimate[1], sd = n_videos$estimate[2]))
exp(qnorm(0.95, mean = n_videos$estimate[1], sd = n_videos$estimate[2]))

sum(exp(rnorm(62, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
1 - pnorm(log(mean(video_data)), mean = n_videos$estimate[1], sd = n_videos$estimate[2])



plotdist(video_meme_data, histo = TRUE, demp = TRUE)

fln_video_memes <- fitdist(video_meme_data, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("lognormal")
denscomp(list(fln_video_memes), legendtext = plot.legend)
qqcomp(list(fln_video_memes), legendtext = plot.legend)
cdfcomp(list(fln_video_memes), legendtext = plot.legend)
ppcomp(list(fln_video_memes), legendtext = plot.legend)


plotdist(log(video_meme_data), histo = TRUE, demp = TRUE)

n_video_memes <- fitdist(log(video_meme_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_video_memes), legendtext = plot.legend)
qqcomp(list(n_video_memes), legendtext = plot.legend)
cdfcomp(list(n_video_memes), legendtext = plot.legend)
ppcomp(list(n_video_memes), legendtext = plot.legend)

exp(qnorm(0.05, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2]))
exp(qnorm(0.95, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2]))


sum(exp(rnorm(62, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
1 - pnorm(log(mean(video_meme_data)), mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])
pnorm(log(40574.16), mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])



plotdist(photo_data, histo = TRUE, demp = TRUE)

fln_photos <- fitdist(photo_data, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("lognormal")
denscomp(list(fln_photos), legendtext = plot.legend)
qqcomp(list(fln_photos), legendtext = plot.legend)
cdfcomp(list(fln_photos), legendtext = plot.legend)
ppcomp(list(fln_photos), legendtext = plot.legend)



gofstat(fln_videos)

fln_articles$estimate[1]



video_meme_data_aug <- DataVideos[which(DataVideos$date >= "2016-01-01" & DataVideos$date < "2017-01-01" &  DataVideos$video_meme == 1),]$post_video_views

hist(video_meme_data_aug, prob=TRUE, breaks = 200, xlim = c(0, 5000000))
curve(dlnorm(x, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2]), add=TRUE)


## MODEL ------------------------------------------------------------------------------------------------------------------

library(MASS)
library(fitdistrplus)


# 1. Data ----------------------------------


date_range <- c("2017-05-01", "2017-08-01")

article_data <- DataArticles[which(DataArticles$date >= date_range[1] & DataArticles$date < date_range[2] & DataArticles$repost == 0, DataArticles$post_source_type == "native"),]$link_clicks

article_repost_data <- DataArticles[which(DataArticles$date >= date_range[1] & DataArticles$date < date_range[2] & DataArticles$repost == 1, DataArticles$post_source_type == "native"),]$link_clicks

video_data <- DataVideos[which(DataVideos$date >= date_range[1] & DataVideos$date < date_range[2] & DataVideos$video_meme == 0 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views

video_repost_data <- DataVideos[which(DataVideos$date >= date_range[1] & DataVideos$date < date_range[2] & DataVideos$video_meme == 0 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views

video_meme_data <- DataVideos[which(DataVideos$date >= date_range[1] & DataVideos$date < date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 0, DataVideos$post_source_type == "native"),]$post_video_views

video_meme_repost_data <- DataVideos[which(DataVideos$date >= date_range[1] & DataVideos$date < date_range[2] & DataVideos$video_meme == 1 & DataVideos$repost == 1, DataVideos$post_source_type == "native"),]$post_video_views

meme_data <- DataPhotos[which(DataPhotos$date >= date_range[1] & DataPhotos$date < date_range[2] & DataPhotos$repost == 0, DataPhotos$post_source_type == "native"),]$post_reach

meme_repost_data <- DataPhotos[which(DataPhotos$date >= date_range[1] & DataPhotos$date < date_range[2] & DataPhotos$repost == 1, DataPhotos$post_source_type == "native"),]$post_reach


# 2. Dist. Estimation ----------------------------------


n_articles <- fitdist(log(article_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_articles), legendtext = plot.legend)
qqcomp(list(n_articles), legendtext = plot.legend)
cdfcomp(list(n_articles), legendtext = plot.legend)
ppcomp(list(n_articles), legendtext = plot.legend)

n_videos <- fitdist(log(video_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_videos), legendtext = plot.legend)
qqcomp(list(n_videos), legendtext = plot.legend)
cdfcomp(list(n_videos), legendtext = plot.legend)
ppcomp(list(n_videos), legendtext = plot.legend)

n_video_memes <- fitdist(log(video_meme_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_video_memes), legendtext = plot.legend)
qqcomp(list(n_video_memes), legendtext = plot.legend)
cdfcomp(list(n_video_memes), legendtext = plot.legend)
ppcomp(list(n_video_memes), legendtext = plot.legend)

n_memes <- fitdist(log(meme_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_memes), legendtext = plot.legend)
qqcomp(list(n_memes), legendtext = plot.legend)
cdfcomp(list(n_memes), legendtext = plot.legend)
ppcomp(list(n_memes), legendtext = plot.legend)


n_articles_repo <- fitdist(log(article_repost_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_articles_repo), legendtext = plot.legend)
qqcomp(list(n_articles_repo), legendtext = plot.legend)
cdfcomp(list(n_articles_repo), legendtext = plot.legend)
ppcomp(list(n_articles_repo), legendtext = plot.legend)

n_videos_repo <- fitdist(log(video_repost_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_videos_repo), legendtext = plot.legend)
qqcomp(list(n_videos_repo), legendtext = plot.legend)
cdfcomp(list(n_videos_repo), legendtext = plot.legend)
ppcomp(list(n_videos_repo), legendtext = plot.legend)

n_video_memes_repo <- fitdist(log(video_meme_repost_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_video_memes_repo), legendtext = plot.legend)
qqcomp(list(n_video_memes_repo), legendtext = plot.legend)
cdfcomp(list(n_video_memes_repo), legendtext = plot.legend)
ppcomp(list(n_video_memes_repo), legendtext = plot.legend)

n_memes_repo <- fitdist(log(meme_repost_data), "norm")
par(mfrow = c(2, 2))
plot.legend <- c("normal")
denscomp(list(n_memes_repo), legendtext = plot.legend)
qqcomp(list(n_memes_repo), legendtext = plot.legend)
cdfcomp(list(n_memes_repo), legendtext = plot.legend)
ppcomp(list(n_memes_repo), legendtext = plot.legend)



# 3. Conf. Intervals -------------------------------------------

alpha <- 0.05

exp(qnorm(alpha/2, mean = n_articles$estimate[1], sd = n_articles$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_articles$estimate[1], sd = n_articles$estimate[2]))

exp(qnorm(alpha/2, mean = n_articles_repo$estimate[1], sd = n_articles_repo$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_articles_repo$estimate[1], sd = n_articles_repo$estimate[2]))


exp(qnorm(alpha/2, mean = n_videos$estimate[1], sd = n_videos$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_videos$estimate[1], sd = n_videos$estimate[2]))

exp(qnorm(alpha/2, mean = n_videos_repo$estimate[1], sd = n_videos_repo$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_videos_repo$estimate[1], sd = n_videos_repo$estimate[2]))


exp(qnorm(alpha/2, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2]))

exp(qnorm(alpha/2, mean = n_video_memes_repo$estimate[1], sd = n_video_memes_repo$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_video_memes_repo$estimate[1], sd = n_video_memes_repo$estimate[2]))


exp(qnorm(alpha/2, mean = n_memes$estimate[1], sd = n_memes$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_memes$estimate[1], sd = n_memes$estimate[2]))

exp(qnorm(alpha/2, mean = n_memes_repo$estimate[1], sd = n_memes_repo$estimate[2]))
exp(qnorm(1 - alpha/2, mean = n_memes_repo$estimate[1], sd = n_memes_repo$estimate[2]))

# 4. Monte Carlo Sim. ------------------------------------------

set.seed(0)

monte_carlo_articles <- NA
monte_carlo_videos <- NA
monte_carlo_video_memes <- NA
monte_carlo_memes <- NA

monte_carlo_articles_repo <- NA
monte_carlo_videos_repo <- NA
monte_carlo_video_memes_repo <- NA
monte_carlo_memes_repo <- NA

for(i in 1:100000){
  monte_carlo_articles[i] <- sum(exp(rnorm(155, mean = n_articles$estimate[1], sd = n_articles$estimate[2])))
  monte_carlo_videos[i] <- sum(exp(rnorm(62, mean = n_videos$estimate[1], sd = n_videos$estimate[2])))
  monte_carlo_video_memes[i] <- sum(exp(rnorm(62, mean = n_video_memes$estimate[1], sd = n_video_memes$estimate[2])))
  monte_carlo_memes[i] <- sum(exp(rnorm(186, mean = n_memes$estimate[1], sd = n_memes$estimate[2])))
  
  monte_carlo_articles_repo[i] <- sum(exp(rnorm(30, mean = n_articles_repo$estimate[1], sd = n_articles_repo$estimate[2])))
  monte_carlo_videos_repo[i] <- sum(exp(rnorm(15, mean = n_videos_repo$estimate[1], sd = n_videos_repo$estimate[2])))
  monte_carlo_video_memes_repo[i] <- sum(exp(rnorm(15, mean = n_video_memes_repo$estimate[1], sd = n_video_memes_repo$estimate[2])))
  monte_carlo_memes_repo[i] <- sum(exp(rnorm(60, mean = n_memes_repo$estimate[1], sd = n_memes_repo$estimate[2])))
}

monte_carlo_page <- monte_carlo_articles + monte_carlo_videos + monte_carlo_video_memes + monte_carlo_memes + monte_carlo_articles_repo + monte_carlo_videos_repo + monte_carlo_video_memes_repo + monte_carlo_memes_repo

plotdist(monte_carlo_page, histo = TRUE, demp = TRUE)

plotdist(monte_carlo_articles, histo = TRUE, demp = TRUE)
plotdist(monte_carlo_videos, histo = TRUE, demp = TRUE)
plotdist(monte_carlo_video_memes, histo = TRUE, demp = TRUE)
plotdist(monte_carlo_memes, histo = TRUE, demp = TRUE)

plot_histogram(monte_carlo_articles)
plot_histogram(monte_carlo_videos)
plot_histogram(monte_carlo_video_memes)
plot_histogram(monte_carlo_memes)
