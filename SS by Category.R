# LIBRARIES ========================================================================================================================

library(ggplot2)
library(plyr)
library(dplyr)


Data <- read.csv("data/Facebook Insights.csv", header = TRUE, stringsAsFactors = FALSE)

Data <- Data[!duplicated(Data),]
Data$date = strptime(Data$date, "%d/%m/%Y")
Data$date <- as.Date(Data$date)
Data$sharetext[Data$sharetext == ""] <- "We are mitÃº"
Data$headline[Data$headline == ""] <- "We are mitÃº"
Data$interaction_rate <- (Data$total_comments+Data$total_likes + Data$total_shares)/Data$post_reach
Data$ctr <- Data$link_clicks/Data$post_reach

LinkData <- read.csv("data/LinkData.csv", header = TRUE)
VideoData <- read.csv("data/VideoData.csv", header = TRUE)

AuthorsWeek <- read.csv("data/authors-export-week.csv", header = TRUE)
AuthorsMonth <- read.csv("data/authors-export-month.csv", header = TRUE)

DataArticles <- Data[Data$post_type == "link",]
DataArticles <- merge(DataArticles[,], LinkData[,c("status_id", "mitu_link", "category","reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])

DataVideos <- Data[Data$post_type == "video",]
DataVideos <- merge(DataVideos[,], VideoData[,c("status_id","video_meme", "series", "category", "format")])

DataPhotos <- Data[Data$post_type == "photo",]

DataArticlesCommentsSentiment <- read.csv("data/wearemitu comment_summary 2015-11-01 - 2016-07-31 link.csv", header = TRUE, stringsAsFactors = FALSE)

DataArticlesHeadlineSentiment <- read.csv("data/Sentiment_short_identities.csv", header = TRUE, stringsAsFactors = FALSE)

ArticleDataWithSS <- merge(DataArticles, DataArticlesCommentsSentiment[, c("post_id", "ss_comments")], by.x = "status_id", by.y = "post_id")

ArticleDataWithSS <- merge(ArticleDataWithSS, DataArticlesHeadlineSentiment[, c("status_id", "ss_headlines")], by.x = "status_id", by.y = "status_id")

DataPareto <- read.csv("data/Pareto_SS.csv", header = TRUE)
DataParetoInter <- read.csv("data/Pareto_SS_Inter.csv", header = TRUE)

ArticleDataParetoWithSS <- merge(DataArticles, DataPareto[, c("PostID", "ss_headlines", "ss_comments")], by.x = "status_id", by.y = "PostID")

ArticleDataParetoWithSSInter <- merge(DataArticles, DataParetoInter[, c("PostID", "ss_headlines", "ss_comments")], by.x = "status_id", by.y = "PostID")


Avg_Categories <- ddply(ArticleDataWithSS[which(ArticleDataWithSS$ss_headlines != 0),], "category", summarize, avg_engagement = mean(interaction_rate), sd_engagement = sd(interaction_rate), avg_ctr = mean(ctr), sd_ctr = sd(ctr), avg_log_reach = mean(log(post_reach)), sd_log_reach = sd(log(post_reach)), avg_ss_comments = mean(ss_comments), avg_ss_headlines = mean(ss_headlines))

Avg_Categories_Pareto <- ddply(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], "category", summarize, n = length(post_reach), avg_engagement = mean(interaction_rate), sd_engagement = sd(interaction_rate), avg_ctr = mean(ctr), sd_ctr = sd(ctr), avg_log_reach = mean(log(post_reach)), sd_log_reach = sd(log(post_reach)), avg_ss_comments = mean(ss_comments), avg_ss_headlines = mean(ss_headlines))

Avg_Categories_Pareto_Inter <- ddply(ArticleDataParetoWithSSInter[which(ArticleDataParetoWithSSInter$ss_headlines != 0),], "category", summarize, n = length(post_reach), avg_engagement = mean(interaction_rate), sd_engagement = sd(interaction_rate), avg_ctr = mean(ctr), sd_ctr = sd(ctr), avg_log_reach = mean(log(post_reach)), sd_log_reach = sd(log(post_reach)), avg_ss_comments = mean(ss_comments), avg_ss_headlines = mean(ss_headlines))


p1<-ggplot(Avg_Categories, aes(x=avg_ss_headlines, y=avg_ss_comments, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + ggtitle("All Articles") + geom_text(check_overlap = TRUE, hjust = -0.2)

p2<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_headlines, y=avg_ss_comments, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + ggtitle("Pareto LC Articles") + geom_text(check_overlap = TRUE, hjust = -0.2)

p3<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_headlines, y=avg_ss_comments, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + ggtitle("Pareto Eng. Articles") + geom_text(check_overlap = TRUE, hjust = -0.2)

grid.arrange(p1,p2,p3, ncol = 3, nrow = 1)




# CTR Vs. Engagement

p<-ggplot(Avg_Categories, aes(x=avg_ctr, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories, aes(x=avg_ss_comments, y=avg_ss_headlines, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)


p<-ggplot(Avg_Categories, aes(x=avg_log_reach, y=avg_ss_comments, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories, aes(x=avg_log_reach, y=avg_ss_headlines, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ctr, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_comments, y=avg_ss_headlines, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)


p<-ggplot(Avg_Categories_Pareto, aes(x=avg_log_reach, y=avg_ss_comments, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories_Pareto, aes(x=avg_log_reach, y=avg_ss_headlines, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ctr, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_comments, y=avg_ss_headlines, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)


p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_log_reach, y=avg_ss_comments, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_log_reach, y=avg_ss_headlines, colour = category, label = category)) + geom_point(size= 3) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)


# CTR Vs. SS Comments

p<-ggplot(Avg_Categories, aes(x=avg_ss_comments, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_comments, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_comments, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

# Engagement Vs. SS Comments

p<-ggplot(Avg_Categories, aes(x=avg_ss_comments, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_comments, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_comments, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

# CTR Vs. SS Headlines

p<-ggplot(Avg_Categories, aes(x=avg_ss_headlines, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_headlines, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_headlines, y=avg_ctr, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

# Engagement Vs. SS Headlines

p<-ggplot(Avg_Categories, aes(x=avg_ss_headlines, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_headlines, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto_Inter, aes(x=avg_ss_headlines, y=avg_engagement, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)




p<-ggplot(Avg_Categories, aes(x=avg_ss_comments, y=avg_log_reach, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)



p<-ggplot(Avg_Categories_Pareto, aes(x=avg_ss_comments, y=avg_log_reach, colour = category, label = category)) + geom_point(aes(size = avg_log_reach)) + geom_text(check_overlap = TRUE, hjust = -0.2)

print(p)

# SS Comments, SS Headlines Correlation

corr_comm_head <- ddply(ArticleDataWithSS, "category", summarize, corr_comm_head = cor(ss_comments, ss_headlines), corr_comm_ctr = cor(ss_comments, ctr), corr_head_ctr = cor(ss_headlines, ctr), corr_comm_ir = cor(ss_comments, interaction_rate), corr_head_ir = cor(ss_headlines, interaction_rate))



corr_comm_head_pareto <- ddply(ArticleDataParetoWithSS, "category", summarize, corr_comm_head = cor(ss_comments, ss_headlines), corr_comm_ctr = cor(ss_comments, ctr), corr_head_ctr = cor(ss_headlines, ctr), corr_comm_ir = cor(ss_comments, interaction_rate), corr_head_ir = cor(ss_headlines, interaction_rate))



corr_comm_head_pareto_inter <- ddply(ArticleDataParetoWithSSInter, "category", summarize, corr_comm_head = cor(ss_comments, ss_headlines), corr_comm_ctr = cor(ss_comments, ctr), corr_head_ctr = cor(ss_headlines, ctr), corr_comm_ir = cor(ss_comments, interaction_rate), corr_head_ir = cor(ss_headlines, interaction_rate))

# SS Comments Vs. SS Headlines (CTR & Engagement)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "News & Issues", "Citizenship & Politics") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataParetoWithSSInter[which(ArticleDataParetoWithSSInter$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)


# SS Comments Vs. SS Headlines (CTR & Engagement)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


# p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments < 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 
# 
# print(p)
# 
# p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments > 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 
# 
# print(p)





p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "Family & Friends", "Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)


# SS Comments Vs. SS Headlines (CTR & Engagement)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size =3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size =3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)




# p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments < 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 
# 
# print(p)
# 
# p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments > 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 
# 
# print(p)

p1<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach))) + geom_point(size = 1) + geom_smooth(method = "lm", se = FALSE) + ggtitle("All Articles") 

p2<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach))) + geom_point(size = 1) + geom_smooth(method = "lm", se = FALSE) + ggtitle("Pareto LC Articles") 

p3<-ggplot(ArticleDataParetoWithSSInter[which(ArticleDataParetoWithSSInter$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach))) + geom_point(size = 1) + geom_smooth(method = "lm", se = FALSE) + ggtitle("Pareto Eng. Articles") 

grid.arrange(p1,p2,p3, ncol = 3, nrow = 1)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach))) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach))) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach))) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach))) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)






p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Beauty & Fashion", "Celebrity & Gossip", "TV & Movies") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)


# SS Comments Vs. SS Headlines (CTR & Engagement)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Animals", "Sports", "Music") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

# SS Comments Vs. SS Headlines (CTR & Engagement)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments < 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0 & ArticleDataWithSS$ss_comments > 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)





p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)





p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(aes(size = interaction_rate)) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(aes(size = ctr)) + geom_smooth(method = "lm", se = FALSE) 

print(p)


p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y= log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Latino Culture", "Identities") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) 

print(p)








p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Food & Beverages", "TV & Movies") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Food & Beverages", "TV & Movies")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "TV & Movies", "Family & Friends") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Food & Beverages", "TV & Movies", "Family & Friends")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ss_headlines, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("News & Issues", "Family & Friends", "Music", "Identities", "Animals") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("News & Issues", "Family & Friends", "Music", "Identities", "Animals")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Family & Friends", "Music", "Celebrity & Gossip") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Family & Friends", "Music", "Celebrity & Gossip")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Family & Friends", "Sports", "Animals") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Family & Friends", "Sports", "Animals")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Beauty & Fashion") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Beauty & Fashion")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("News & Issues", "Beauty & Fashion") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("News & Issues", "Beauty & Fashion")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Beauty & Fashion", "Animals") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Beauty & Fashion", "Animals")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "Food & Beverages", "Music", "Animals") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Sensationalist", "Food & Beverages", "Music", "Animals")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sensationalist", "Food & Beverages", "Family & Friends", "Beauty & Fashion", "TV & Movies", "Music", "Animals") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Sensationalist", "Food & Beverages", "Family & Friends", "Beauty & Fashion", "TV & Movies", "Music", "Animals")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=ctr, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Citizenship & Politics", "Sensationalist", "News & Issues", "Beauty & Fashion", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Citizenship & Politics", "Sensationalist", "News & Issues", "Beauty & Fashion", "Music")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Sensationalist", "Beauty & Fashion", "Music", "Food & Beverages", "Family & Friends") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Sensationalist", "Beauty & Fashion", "Music", "Food & Beverages", "Family & Friends")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 
print(p)



p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Sex & Relationships", "Food & Beverages", "Beauty & Fashion", "Celebrity & Gossip", "Music") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Sex & Relationships", "Food & Beverages", "Beauty & Fashion", "Celebrity & Gossip", "Music")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Celebrity & Gossip", "Beauty & Fashion", "TV & Movies", "Music", "Sports", "Food & Beverages", "Family & Friends", "Identities", "Latino Culture") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Citizenship & Politics", "Celebrity & Gossip", "Beauty & Fashion", "TV & Movies", "Music", "Sports", "Food & Beverages", "Family & Friends", "Identities", "Latino Culture")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_comments, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Sex & Relationships", "Music", "Family & Friends", "Beauty & Fashion", "TV & Movies", "Citizenship & Politics") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataWithSS[which(!(ArticleDataWithSS$category %in% c("Sensationalist", "News & Issues", "Sex & Relationships", "Music", "Family & Friends", "Beauty & Fashion", "TV & Movies", "Citizenship & Politics")) & ArticleDataWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sex & Relationships", "Music", "TV & Movies", "Family & Friends") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(!(ArticleDataParetoWithSS$category %in% c("Sex & Relationships", "Music", "TV & Movies", "Family & Friends")) & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ss_headlines, y=log(post_reach), colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)




p<-ggplot(ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sex & Relationships", "Food & Beverages", "Family & Friends", "Sensationalist") & ArticleDataWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)

p<-ggplot(ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sex & Relationships", "Food & Beverages", "Family & Friends", "Sensationalist") & ArticleDataParetoWithSS$ss_headlines != 0),], aes(x=ctr, y=interaction_rate, colour = category)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE) 

print(p)


sex_relat <- ArticleDataWithSS[which(ArticleDataWithSS$category %in% c("Sex & Relationships") & ArticleDataWithSS$ss_headlines != 0),]

sex_relat_pareto <- ArticleDataParetoWithSS[which(ArticleDataParetoWithSS$category %in% c("Sex & Relationships") & ArticleDataParetoWithSS$ss_headlines != 0),]






