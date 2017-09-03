# LIBRARIES ========================================================================================================================

library(ggplot2)
library(reshape)
library(scales)
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(ggthemes)
library(RColorBrewer)
library(highcharter)
library(quantmod)
library(dtplyr)
library(data.table)
library(readr)
library(lubridate)
# library(jsonlite)

# DATA LOAD ========================================================================================================================

load("data/PostData.Rda")
load("data/DataArticles.Rda")
load("data/DataVideos.Rda")
load("data/DataPhotos.Rda")

load("data/PostDataBH.Rda")
load("data/DataArticlesBH.Rda")
load("data/DataVideosBH.Rda")
load("data/DataPhotosBH.Rda")

load("data/PostDataFC.Rda")
load("data/DataArticlesFC.Rda")
load("data/DataVideosFC.Rda")
load("data/DataPhotosFC.Rda")

# write.csv(DataArticles, "WAMDataArticles.csv", row.names = FALSE)
# write.csv(DataVideos, "WAMDataVideos.csv", row.names = FALSE)
# write.csv(DataPhotos, "WAMDataPhotos.csv", row.names = FALSE)

# write.csv(DataArticlesBH, "BHDataArticles.csv", row.names = FALSE)
# write.csv(DataVideosBH, "BHDataVideos.csv", row.names = FALSE)
# write.csv(DataPhotosBH, "BHDataPhotos.csv", row.names = FALSE)
# 
# write.csv(DataArticlesFC, "FCDataArticles.csv", row.names = FALSE)
# write.csv(DataVideosFC, "FCDataVideos.csv", row.names = FALSE)
# write.csv(DataPhotosFC, "FCDataPhotos.csv", row.names = FALSE)

DataGoals <- read.csv("data/WAM KPIs 2017.csv", header = TRUE, stringsAsFactors = FALSE)
DataGoals$date <- strptime(DataGoals$date, "%d/%m/%Y")

DataGoalsBH <- read.csv("data/BH KPIs 2017.csv", header = TRUE, stringsAsFactors = FALSE)
DataGoalsBH$date <- strptime(DataGoalsBH$date, "%d/%m/%Y")


# SQL --------------------------------------------------------------------------------------------------------------------

library(RMySQL)
mydb = dbConnect(MySQL(), host = "104.198.210.36", user = "root", password = "tacozombies54992", db = "analytics")

rs <- dbSendQuery(mydb, "select * from PAGE_DATA where from_name = 'We are mitu'")
WamPageData <- fetch(rs, -1)

rs <- dbSendQuery(mydb, "select * from PAGE_DATA where from_name = 'Bad Hombres'")
BHPageData <- fetch(rs, -1)

rs <- dbSendQuery(mydb, "select * from PAGE_DATA where from_name = 'Fierce by mitu'")
FCPageData <- fetch(rs, -1)

dbClearResult(rs)
dbDisconnect(mydb)

WamPageData$date <- as.Date(WamPageData$date)
BHPageData$date <- as.Date(BHPageData$date)
FCPageData$date <- as.Date(FCPageData$date)


# LinkData <- read.csv("data/LinkData.csv", header = TRUE, stringsAsFactors = FALSE)
# VideoData <- read.csv("data/VideoData.csv", header = TRUE, stringsAsFactors = FALSE)
# PhotoData <- read.csv("data/PhotoData.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# 
# library(RMySQL)
# mydb = dbConnect(MySQL(), host="104.198.210.36", user = "root", password = "tacozombies54992", db = "analytics")
# # rs <- dbSendQuery(mydb, 'set character set "utf8"')
# rs <- dbSendQuery(mydb, "select * from FB_INSIGHTS")
# Data <- fetch(rs, -1)
# dbClearResult(rs)
# dbDisconnect(mydb)
# 
# 
# Data <- Data[!duplicated(Data),]
# # Data$date = strptime(Data$date, "%d/%m/%Y")
# Data$date <- as.Date(Data$date)
# 
# Data$created_time <- as.POSIXct(strptime(Data$created_time, "%Y-%m-%d %H:%M:%S"))
# Data[Data$sharetext == "",]$sharetext <- as.character(Data[Data$sharetext == "",]$status_id)
# Data[Data$headline == "",]$headline <- as.character(Data[Data$headline == "",]$status_id)
# # Encoding(Data$sharetext) <- "latin1"
# # Encoding(Data$headline) <- "latin1"
# Data$total_interactions <- Data$total_comments+Data$total_likes + Data$total_shares
# Data$interaction_rate <- (Data$total_comments+Data$total_likes + Data$total_shares)/Data$post_reach
# Data$ctr <- Data$link_clicks/Data$post_reach
# Data$views_rate <- Data$post_video_views/Data$post_reach
# Data$viral_fan_rate <- Data$post_reach_viral_unique/Data$post_reach_fan_unique
# Data$share_rate <- Data$total_shares/(Data$total_comments+Data$total_likes + Data$total_shares)
# Data$post_image <- paste("<img src ='", Data$full_picture,"'",'title=""', 'alt="" border="0" height="100" width="100">')
# Data[, grep("s0", colnames(Data)):grep("s40", colnames(Data))] <- lapply(Data[, grep("s0", colnames(Data)):grep("s40", colnames(Data))], as.numeric)
# 
# 
# DataArticles <- Data[Data$post_type == "link",]
# DataArticles <- merge(DataArticles[,], LinkData[,c("status_id", "mitu_link", "category","reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataArticles <- ddply(DataArticles, "mitu_link", transform, average_ctr = mean(ctr), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_link_clicks = mean(link_clicks))
# # DataArticles$created_time <- as.POSIXct(strptime(DataArticles$created_time, "%Y-%m-%d %H:%M:%s"))
# 
# DataVideos <- Data[Data$post_type == "video",]
# DataVideos <- merge(DataVideos[,], VideoData[,c("status_id", "video_repost_sharetext", "video_meme", "series", "category", "format", "reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataVideos <- ddply(DataVideos, "video_repost_sharetext", transform, average_views_rate = mean(views_rate), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_video_views = mean(post_video_views), average_viral_fan_rate = mean(viral_fan_rate))
# # DataVideos$created_time <- as.POSIXct(strptime(DataVideos$created_time, "%d/%m/%Y %H:%M"), tz = "GMT")
# 
# DataPhotos <- Data[Data$post_type == "photo",]
# DataPhotos <- merge(DataPhotos[,],PhotoData[,c("status_id", "image_text_py", "reposted", "original", "repost", "repost_order", "times_repost", "days_bet_repost")])
# DataPhotos <- ddply(DataPhotos, "image_text_py", transform, average_share_rate = mean(share_rate), average_interaction_rate = mean(interaction_rate), average_post_reach = mean(post_reach), average_viral_fan_rate = mean(viral_fan_rate))
# # DataPhotos$created_time <- as.POSIXct(strptime(DataPhotos$created_time, "%d/%m/%Y %H:%M"), tz = "GMT")


# We Are Mitú Data -------------------------------------------------------------------------------------------------------

DataArticles$general_rank_link_clicks <- rank(-DataArticles$link_clicks, ties.method="max")
DataArticles$general_rank_reach <- rank(-DataArticles$post_reach, ties.method="max")
DataArticles$general_rank_interactions <- rank(-DataArticles$total_interactions, ties.method="max")
DataArticles$article_click_rank <- ifelse(DataArticles$link_clicks <= 5000, "< 5K", ifelse(DataArticles$link_clicks <= 10000, "5K - 10K", ifelse(DataArticles$link_clicks <= 25000, "10K - 25K", "> 25K")))


DataVideos$general_rank_video_views <- ave(DataVideos$post_video_views, DataVideos$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideos$general_rank_reach <- ave(DataVideos$post_reach, DataVideos$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideos$general_rank_interactions <- ave(DataVideos$total_interactions, DataVideos$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideos$video_views_rank <- ifelse(DataVideos$post_video_views <= 500000, "< 500K", ifelse(DataVideos$post_video_views <= 1500000, "500K - 1.5M", ifelse(DataVideos$post_video_views <= 5000000, "1.5M - 5M", "> 5M")))

DataVideos$viral_video_views <- DataVideos$post_video_views*DataVideos$viral_rate

DataVideos$fan_video_views <- DataVideos$post_video_views - DataVideos$fan_rate

DataPhotos$general_rank_reach <- rank(-DataPhotos$post_reach, ties.method="max")
DataPhotos$general_rank_interactions <- rank(-DataPhotos$total_interactions, ties.method="max")  

# Bad Hombres Data -------------------------------------------------------------------------------------------------------

DataArticlesBH$general_rank_link_clicks <- rank(-DataArticlesBH$link_clicks, ties.method="max")
DataArticlesBH$general_rank_reach <- rank(-DataArticlesBH$post_reach, ties.method="max")
DataArticlesBH$general_rank_interactions <- rank(-DataArticlesBH$total_interactions, ties.method="max")

DataVideosBH$general_rank_video_views <- ave(DataVideosBH$post_video_views, DataVideosBH$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideosBH$general_rank_reach <- ave(DataVideosBH$post_reach, DataVideosBH$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideosBH$general_rank_interactions <- ave(DataVideosBH$total_interactions, DataVideosBH$video_meme, FUN = function(x) rank(-x, ties.method = "max"))

DataVideosBH$viral_video_views <- DataVideosBH$post_video_views*DataVideosBH$viral_rate

DataVideosBH$fan_video_views <- DataVideosBH$post_video_views - DataVideosBH$fan_rate

DataPhotosBH$general_rank_reach <- rank(-DataPhotosBH$post_reach, ties.method="max")
DataPhotosBH$general_rank_interactions <- rank(-DataPhotosBH$total_interactions, ties.method="max")  

# Fierce Data ------------------------------------------------------------------------------------------------------------

DataArticlesFC$general_rank_link_clicks <- rank(-DataArticlesFC$link_clicks, ties.method="max")
DataArticlesFC$general_rank_reach <- rank(-DataArticlesFC$post_reach, ties.method="max")
DataArticlesFC$general_rank_interactions <- rank(-DataArticlesFC$total_interactions, ties.method="max")

DataVideosFC$general_rank_video_views <- ave(DataVideosFC$post_video_views, DataVideosFC$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideosFC$general_rank_reach <- ave(DataVideosFC$post_reach, DataVideosFC$video_meme, FUN = function(x) rank(-x, ties.method = "max"))
DataVideosFC$general_rank_interactions <- ave(DataVideosFC$total_interactions, DataVideosFC$video_meme, FUN = function(x) rank(-x, ties.method = "max"))

DataPhotosFC$general_rank_reach <- rank(-DataPhotosFC$post_reach, ties.method="max")
DataPhotosFC$general_rank_interactions <- rank(-DataPhotosFC$total_interactions, ties.method="max")  

# Authors Data -----------------------------------------------------------------------------------------------------------

AuthorsWeek <- read.csv("data/authors-export-week.csv", header = TRUE)
AuthorsMonth <- read.csv("data/authors-export-month.csv", header = TRUE)

sprint_benchs <- c(11250,2000000,1500000)


# UI ===============================================================================================================================


ui <- dashboardPage(skin = "blue",
                    
                    # Dashboard Header ---------------------------------------------------------------------------------------------
                    
                    dashboardHeader(title = "Weekly Recapp"),
                    
                    # Dashboard Sidebar --------------------------------------------------------------------------------------------                   
                    dashboardSidebar(
                      
                      tags$head(
                        tags$style(HTML("
                                        .sidebar { height: 180vh; }
                                        " )
                        )
                      ),
                      
                      sidebarMenu(
                        
                        menuItem("Date Range", dateRangeInput('dateRange1',label = "", start = range(Data$date)[2]-6, end = range(Data$date)[2], min = "2015-10-01", max = range(Data$date)[2]), icon = icon("fa fa-calendar"), br()),
                        br(),
                        
                        menuItem("Overview", tabName = "overview", icon = icon("fa fa-eye")),
                        # menuItem("KPI's", tabName = "kpis", icon = icon("fa fa-line-chart")),
                        menuItem("Top Posts", tabName = "topposts", icon = icon("fa fa-chevron-up")),
                        menuItem("Bottom Posts", tabName = "bottomposts", icon = icon("fa fa-chevron-down")),
                        # menuItem("Editor Leaderboard", tabName = "authors", icon = icon("fa fa-pencil")),
                        menuItem("Content Categories", tabName = "categories", icon = icon("fa fa-tags")),
                        menuItem("Reposts", tabName = "reposts", icon = icon("fa fa-cubes"))
                        # menuItem("Sprint", tabName = "sprint", icon = icon("fa fa-trophy"))
                        
                      )
                    ),
                    
                    # Dashboard Body -----------------------------------------------------------------------------------------------                    
                    dashboardBody(fluidRow(
                      
                      tabItems(
                        
                        # 1. Overview ---------------------------------------------------------------------------------------------
                        
                        tabItem(tabName = "overview", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 # 1.1. Overview - We Are Mitú -----------------------------------------------------------------------------------
                                 
                                 tabPanel("We Are Mitú",
                                          
                                          box(title = "Page Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              column(3,selectizeInput(inputId = "reach_chart_selectize", label = "Chart Type:", choices = c("Reach", "Reach by Gender", "Reach by Age"), selected = "Reach")),
                                              column(3, selectizeInput(inputId = "avg_total_overview_page_reach", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(3, selectizeInput(inputId = "chart_time_overview_page_reach", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, actionButton(inputId = "plot_overview_page_reach", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotOverviewPageReach", height = 350))),
                                          
                                          tabsetPanel(
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "chart_type_overview1", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "chart_time_overview1", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "chart_stack_overview1", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "plot_article_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(9, selectizeInput(inputId = "article_select_categories_overview", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         column(3,selectizeInput(inputId = "chart_link_clicks_group_overview2", label = "Link Clicks:", choices = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), selected = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview111", height = 350))),
                                                     
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "chart_time_article_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")), 
                                                         column(2, selectizeInput(inputId = "avg_total_article_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "variable_display_overview", label = "Variable Displayed:", choices = c("link clicks", "interactions"), selected = "link clicks")),
                                                         column(2, selectizeInput(inputId = "rate_display_overview", label = "Rate Displayed:", choices = c("ctr", "interaction rate"), selected = "ctr")),
                                                         column(4, actionButton(inputId = "plot_article_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(8, selectizeInput(inputId = "article_select_categories_performance_overview", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         column(2,selectizeInput(inputId = "chart_link_clicks_group_overview", label = "Link Clicks:", choices = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), selected = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), multiple = TRUE)),
                                                         column(2, selectizeInput(inputId = "article_overview_repost", label = "Reposts:", choices = c(0,1), selected = c(0,1), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview11", height = 500)))
                                                     
                                            ),
                                            
                                            tabPanel("Videos", 
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "chart_type_overview2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "chart_time_overview2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "chart_stack_overview2", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "plot_video_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(9, selectizeInput(inputId = "video_select_categories_overview", label = "Categories:", choices = as.character(unique(DataVideos[which(DataVideos$video_meme == 0),]$category)[order(unique(DataVideos[which(DataVideos$video_meme == 0),]$category))]), selected = as.character(unique(DataVideos[which(DataVideos$video_meme == 0),]$category)), multiple = TRUE)),
                                                         column(3,selectizeInput(inputId = "chart_video_views_group_overview2", label = "Video Views:", choices = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), selected = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview121", height = 350))),
                                                     
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "chart_time_video_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "avg_total_video_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "video_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                                         column(2, selectizeInput(inputId = "video_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                                         column(4, actionButton(inputId = "plot_video_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(8, selectizeInput(inputId = "video_select_categories_performance_overview", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         column(2,selectizeInput(inputId = "chart_video_views_group_overview", label = "Video Views:", choices = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), selected = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), multiple = TRUE)),
                                                         column(2, selectizeInput(inputId = "video_overview_repost", label = "Reposts:", choices = c(0,1), selected = c(0,1), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview12", height = 500)))
                                                     
                                            ),
                                            
                                            tabPanel("Video Memes", 
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "chart_type_overview3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "chart_time_overview3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "chart_stack_overview3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "plot_videomeme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(9, selectizeInput(inputId = "video_meme_select_categories_overview", label = "Categories:", choices = as.character(unique(DataVideos[which(DataVideos$video_meme == 1),]$category)[order(unique(DataVideos[which(DataVideos$video_meme == 1),]$category))]), selected = as.character(unique(DataVideos[which(DataVideos$video_meme == 1),]$category)), multiple = TRUE)),
                                                         column(3,selectizeInput(inputId = "chart_video_views_meme_group_overview2", label = "Video Views:", choices = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), selected = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview131", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "chart_time_videomeme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "avg_total_videomeme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "videomeme_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                                         column(2, selectizeInput(inputId = "videomeme_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                                         column(4, actionButton(inputId = "plot_videomeme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(8, selectizeInput(inputId = "video_meme_select_categories_performance_overview", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         column(2,selectizeInput(inputId = "chart_video_views_meme_group_overview", label = "Video Views:", choices = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), selected = c("< 500K", "500K - 1.5M", "1.5M - 5M", "> 5M"), multiple = TRUE)),
                                                         column(2, selectizeInput(inputId = "video_meme_overview_repost", label = "Reposts:", choices = c(0,1), selected = c(0,1), multiple = TRUE)),
                                                         column(12, highchartOutput("PlotOverview13", height = 500)))
                                                     
                                            ),
                                            
                                            tabPanel("Memes", 
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "chart_type_overview4", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "chart_time_overview4", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "chart_stack_overview4", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "plot_meme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("PlotOverview141", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "chart_time_meme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "avg_total_meme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "meme_variable_display_overview", label = "Variable Displayed:", choices = c("fan reach", "viral reach"), selected = "fan reach")),
                                                         column(2, selectizeInput(inputId = "meme_rate_display_overview", label = "Rate Displayed:", choices = c("interaction rate", "share rate"), selected = "views rate")),
                                                         column(4, actionButton(inputId = "plot_meme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("PlotOverview14", height = 500)))
                                            )
                                            
                                          )),
                                 
                                 # 1.2. Overview - Bad Hombres -----------------------------------------------------------------------
                                 
                                 tabPanel("Bad Hombres",
                                          
                                          # box(title = "Page Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_overview_page_reach", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_avg_total_overview_page_reach", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_overview_page_reach", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                          #     column(3, actionButton(inputId = "bh_plot_overview_page_reach", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotOverviewPageReach", height = 350))),
                                          # 
                                          # box(title = "Page Reach by Gender", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_overview_page_reach_demo", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_avg_total_overview_page_reach_demo", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_overview_page_reach_demo", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                          #     column(3, actionButton(inputId = "bh_plot_overview_page_reach_demo", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotOverviewPageReachDemo", height = 350))),
                                          # 
                                          # box(title = "Page Reach by Age", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_overview_page_reach_demo_age", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_avg_total_overview_page_reach_demo_age", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_overview_page_reach_demo_age", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                          #     column(3, actionButton(inputId = "bh_plot_overview_page_reach_demo_age", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotOverviewPageReachDemoAge", height = 350))),
                                          
                                          tabsetPanel(
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "bh_chart_type_overview1", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "bh_chart_time_overview1", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "bh_chart_stack_overview1", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "bh_plot_article_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview111", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "bh_chart_time_article_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")), 
                                                         column(2, selectizeInput(inputId = "bh_avg_total_article_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "bh_variable_display_overview", label = "Variable Displayed:", choices = c("link clicks", "interactions"), selected = "link clicks")),
                                                         column(2, selectizeInput(inputId = "bh_rate_display_overview", label = "Rate Displayed:", choices = c("ctr", "interaction rate"), selected = "ctr")),
                                                         column(3, actionButton(inputId = "bh_plot_article_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview11", height = 500)))
                                                     
                                            ),
                                            
                                            tabPanel("Videos", 
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "bh_chart_type_overview2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "bh_chart_time_overview2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "bh_chart_stack_overview2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "bh_plot_video_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview121", height = 350))),
                                                     
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "bh_chart_time_video_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "bh_avg_total_video_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "bh_video_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                                         column(2, selectizeInput(inputId = "bh_video_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                                         column(3, actionButton(inputId = "bh_plot_video_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview12", height = 500)))
                                                     
                                            ),
                                            
                                            # tabPanel("Video Memes", 
                                            #          
                                            #          box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                            #              column(3,selectizeInput(inputId = "bh_chart_type_overview3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                            #              column(3, selectizeInput(inputId = "bh_chart_time_overview3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                            #              column(3, selectizeInput(inputId = "bh_chart_stack_overview3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                            #              column(3, actionButton(inputId = "bh_plot_videomeme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                            #              column(12, highchartOutput("BHPlotOverview131", height = 350))),
                                            #          
                                            #          box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                            #              column(2, selectizeInput(inputId = "bh_chart_time_videomeme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                            #              column(2, selectizeInput(inputId = "bh_avg_total_videomeme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                            #              column(2, selectizeInput(inputId = "bh_videomeme_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                            #              column(2, selectizeInput(inputId = "bh_videomeme_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                            #              column(3, actionButton(inputId = "bh_plot_videomeme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                            #              column(12, highchartOutput("BHPlotOverview13", height = 500)))
                                            #          
                                            # )
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "bh_chart_type_overview4", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "bh_chart_time_overview4", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "bh_chart_stack_overview4", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "bh_plot_meme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview141", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "bh_chart_time_meme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "bh_avg_total_meme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "bh_meme_variable_display_overview", label = "Variable Displayed:", choices = c("fan reach", "viral reach"), selected = "fan reach")),
                                                         column(2, selectizeInput(inputId = "bh_meme_rate_display_overview", label = "Rate Displayed:", choices = c("interaction rate", "share rate"), selected = "views rate")),
                                                         column(3, actionButton(inputId = "bh_plot_meme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("BHPlotOverview14", height = 500)))
                                            )
                                          )
                                 ),
                                 
                                 # 1.3. Overview - Fierce -----------------------------------------------------------------------
                                 
                                 tabPanel("Fierce",
                                          
                                          tabsetPanel(
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "fc_chart_type_overview1", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "fc_chart_time_overview1", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "fc_chart_stack_overview1", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "fc_plot_article_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview111", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "fc_chart_time_article_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")), 
                                                         column(2, selectizeInput(inputId = "fc_avg_total_article_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "fc_variable_display_overview", label = "Variable Displayed:", choices = c("link clicks", "interactions"), selected = "link clicks")),
                                                         column(2, selectizeInput(inputId = "fc_rate_display_overview", label = "Rate Displayed:", choices = c("ctr", "interaction rate"), selected = "ctr")),
                                                         column(3, actionButton(inputId = "fc_plot_article_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview11", height = 500)))
                                                     
                                            ),
                                            
                                            tabPanel("Videos", 
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "fc_chart_type_overview2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "fc_chart_time_overview2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "fc_chart_stack_overview2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "fc_plot_video_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview121", height = 350))),
                                                     
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "fc_chart_time_video_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "fc_avg_total_video_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "fc_video_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                                         column(2, selectizeInput(inputId = "fc_video_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                                         column(3, actionButton(inputId = "fc_plot_video_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview12", height = 500)))
                                                     
                                            ),
                                            
                                            # tabPanel("Video Memes", 
                                            #          
                                            #          box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                            #              column(3,selectizeInput(inputId = "fc_chart_type_overview3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                            #              column(3, selectizeInput(inputId = "fc_chart_time_overview3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                            #              column(3, selectizeInput(inputId = "fc_chart_stack_overview3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                            #              column(3, actionButton(inputId = "fc_plot_videomeme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                            #              column(12, highchartOutput("FCPlotOverview131", height = 350))),
                                            #          
                                            #          box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                            #              column(2, selectizeInput(inputId = "fc_chart_time_videomeme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                            #              column(2, selectizeInput(inputId = "fc_avg_total_videomeme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                            #              column(2, selectizeInput(inputId = "fc_videomeme_variable_display_overview", label = "Variable Displayed:", choices = c("video views", "interactions"), selected = "video views")),
                                            #              column(2, selectizeInput(inputId = "fc_videomeme_rate_display_overview", label = "Rate Displayed:", choices = c("views rate", "interaction rate"), selected = "views rate")),
                                            #              column(3, actionButton(inputId = "fc_plot_videomeme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                            #              column(12, highchartOutput("FCPlotOverview13", height = 500)))
                                            #          
                                            # )
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(3,selectizeInput(inputId = "fc_chart_type_overview4", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(3, selectizeInput(inputId = "fc_chart_time_overview4", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(3, selectizeInput(inputId = "fc_chart_stack_overview4", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         column(3, actionButton(inputId = "fc_plot_meme_overview1", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview141", height = 350))),
                                                     
                                                     box(title = "Content Performance", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(2, selectizeInput(inputId = "fc_chart_time_meme_overview", label = "Chart Timeline:", choices = c("hour","day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "fc_avg_total_meme_overview", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "fc_meme_variable_display_overview", label = "Variable Displayed:", choices = c("fan reach", "viral reach"), selected = "fan reach")),
                                                         column(2, selectizeInput(inputId = "fc_meme_rate_display_overview", label = "Rate Displayed:", choices = c("interaction rate", "share rate"), selected = "views rate")),
                                                         column(3, actionButton(inputId = "fc_plot_meme_overview", label = "Plot", width = "100%", style = "height:60px")),
                                                         column(12, highchartOutput("FCPlotOverview14", height = 500)))
                                            )
                                          )
                                 )
                                 
                                 , width = 12))
                        ),
                        
                        # 2. KPIs ---------------------------------------------------------------------------------------------
                        
                        tabItem(tabName = "kpis", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 # 2.1. KPIs - We Are Mitú -----------------------------------------------------------------------------------
                                 
                                 tabPanel("We Are Mitú",
                                          
                                          tabsetPanel(
                                            
                                            tabPanel("Content Output",
                                                     
                                                     # box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                     #     column(3,selectizeInput(inputId = "chart_type_kpis", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                                     #     column(3, selectizeInput(inputId = "chart_time_kpis", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                                     #     column(3, selectizeInput(inputId = "chart_stack_kpis", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                     #     column(3, actionButton(inputId = "plot_kpis", label = "Plot", width = "100%", style = "height:60px")),
                                                     #     column(12, highchartOutput("PlotKpis"))),
                                                     # 
                                                     #                                             box(title = "Content Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                     #                                                 column(3,selectizeInput(inputId = "chart_type_kpis1", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                                     #                                                 column(3, selectizeInput(inputId = "chart_time_kpis1", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                                     #                                                 column(3, selectizeInput(inputId = "chart_stack_kpis1", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                     #                                                 column(3, actionButton(inputId = "plot_kpis1", label = "Plot", width = "100%", style = "height:60px")),
                                                     #                                                 column(12, highchartOutput("PlotKpis1"))),
                                                     # 
                                                     # box(title = "Content Interactions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                     #     column(3,selectizeInput(inputId = "chart_type_kpis2", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                                     #     column(3, selectizeInput(inputId = "chart_time_kpis2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                                     #     column(3, selectizeInput(inputId = "chart_stack_kpis2", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                     #     column(3, actionButton(inputId = "plot_kpis2", label = "Plot", width = "100%", style = "height:60px")),
                                                     #     column(12, highchartOutput("PlotKpis2"))),
                                                     # 
                                                     # box(title = "Content Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                     #     column(3,selectizeInput(inputId = "chart_type_kpis3", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                                     #     column(3, selectizeInput(inputId = "chart_time_kpis3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                                     #     column(3, selectizeInput(inputId = "chart_stack_kpis3", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                     #     column(3, actionButton(inputId = "plot_kpis3", label = "Plot", width = "100%", style = "height:60px")),
                                                     #     column(12, highchartOutput("PlotKpis3"))),
                                                     
                                                     box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_content_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                        
                                                         column(12, highchartOutput("PlotKpisContentOutputMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisContentOutputTable")),
                                                     
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisContentOutputTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                      
                                                         column(12, highchartOutput("PlotKpisContentOutput"))
                                                     ),
                                                     
                                                     box(title = "Article Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_article_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisArticleContentOutputMonth")),
                                                         column(12, DT::dataTableOutput("PlotKpisArticleContentOutputTable")),
                                                         
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisArticleContentOutputTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_article_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisArticleContentOutput"))
                                                         
                                                     ),
                                                     
                                                     box(title = "Video Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_video_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisVideoContentOutputMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisVideoContentOutputTable")),
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisVideoContentOutputTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_video_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisVideoContentOutput"))
                                                         
                                                     ),
                                                     
                                                     box(title = "Meme Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         
                                                         column(12, selectizeInput(inputId = "kpis_year_meme_output", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisMemeContentOutputMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisMemeContentOutputTable")),
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisMemeContentOutputTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_meme_content_output", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisMemeContentOutput"))
                                                     )
                                                     ),
                                                     
                                            
                                            tabPanel("Followers",
                                                     
                                                     box(title = "Total Followers", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_total_followers", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         column(12, highchartOutput("PlotKpisTotalFollowers")),
                                                         column(12, DT::dataTableOutput("PlotKpisTotalFollowersTable"))
                                                         
                                                     ),
                                                     
                                                     box(title = "New Followers", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_new_followers", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisNewFollowersMonth")),
                                                         column(12, DT::dataTableOutput("PlotKpisNewFollowersTable")),
                                                        
                                                         column(12, br()),
                                                         
                                                         # column(12, highchartOutput("PlotKpisNewFollowersTitle", height = 30)),
                                                         column(12, selectizeInput(inputId = "kpis_month_new_followers", label = "Month:", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(12, highchartOutput("PlotKpisNewFollowers"))
                                                         
                                                     )
                                                     
                                            ),
                                            
                                            tabPanel("Content Views",
                                                                        
                                                     box(title = "Total Content Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_content_views", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisContentViewsMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisContentViewsTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(12, 
                                                                # column(8, highchartOutput("PlotKpisContentViewsTitle", height = 30)),
                                                                column(12, selectizeInput(inputId = "kpis_month_content_views", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisContentViews"))
                                                         
                                                     ),
                                                     
                                                     box(title = "Articles", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_articles", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisArticlesMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisArticlesTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(4, selectizeInput(inputId = "kpis_month_articles", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(4, selectizeInput(inputId = "articles_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "articles_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(12, highchartOutput("PlotKpisArticles"))),
                                                     
                                                     box(title = "All Videos", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_all_videos", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisAllVideosMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisAllVideosTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(4, selectizeInput(inputId = "kpis_month_all_videos", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(4, selectizeInput(inputId = "all_videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "all_videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(12, highchartOutput("PlotKpisAllVideos"))),
                                                     
                                                     
                                                     box(title = "Videos", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_videos", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisVideosMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisVideosTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(4, selectizeInput(inputId = "kpis_month_videos", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(4, selectizeInput(inputId = "videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(12, highchartOutput("PlotKpisVideos"))),
                                                     
                                                     box(title = "Video Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_video_memes", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisVideoMemesMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisVideoMemesTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(4, selectizeInput(inputId = "kpis_month_video_memes", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(4, selectizeInput(inputId = "video_memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(12, highchartOutput("PlotKpisVideoMemes"))),
                                                     
                                                     box(title = "Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_memes", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisMemesMonth")),
                                                         
                                                         column(12, DT::dataTableOutput("PlotKpisMemesTable")),
                                                         
                                                         column(12, br()),
                                                         
                                                         column(4, selectizeInput(inputId = "kpis_month_memes", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                                         column(4, selectizeInput(inputId = "memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions"), selected = "Content Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(12, highchartOutput("PlotKpisMemes")))
                                            ),
                                            
                                            tabPanel("Reach & Engagement",
                                            
                                                     box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_reach", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisReachMonth")),
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisReachTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_reach", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisReach"))
                                                     ),
                                                     
                                                     box(title = "Engagement", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         column(12, selectizeInput(inputId = "kpis_year_engagement", label = "Year: ", choices = as.character(seq(as.numeric(format(range(Data$date)[1], "%Y")),as.numeric(format(range(Data$date)[2], "%Y")), by = 1)), selected = format(Sys.Date(), "%Y"))),
                                                         
                                                         column(12, highchartOutput("PlotKpisEngagementMonth")),
                                                         column(12, 
                                                                column(8, highchartOutput("PlotKpisEngagementTitle", height = 30)),
                                                                column(4, selectizeInput(inputId = "kpis_month_engagement", label = "Month: ", choices = as.character(format(seq(range(Data$date)[1], range(Data$date)[2], by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                                         ),
                                                         column(12, highchartOutput("PlotKpisEngagement"))
                                                         
                                                     )
                                                     
                                            ))
                                 ),
                                 
                                 # 2.2. KPIs - Bad Hombres -----------------------------------------------------------------------------------
                                 
                                 tabPanel("Bad Hombres",
                                          
                                          # box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_kpis", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_kpis", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_stack_kpis", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                          #     column(3, actionButton(inputId = "bh_plot_kpis", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotKpis"))),
                                          # 
                                          #                                             box(title = "Content Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                          #                                                 column(3,selectizeInput(inputId = "bh_chart_type_kpis1", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                          #                                                 column(3, selectizeInput(inputId = "bh_chart_time_kpis1", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                          #                                                 column(3, selectizeInput(inputId = "bh_chart_stack_kpis1", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                          #                                                 column(3, actionButton(inputId = "bh_plot_kpis1", label = "Plot", width = "100%", style = "height:60px")),
                                          #                                                 column(12, highchartOutput("BHPlotKpis1"))),
                                          # 
                                          # box(title = "Content Interactions", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_kpis2", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_kpis2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_stack_kpis2", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                          #     column(3, actionButton(inputId = "bh_plot_kpis2", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotKpis2"))),
                                          # 
                                          # box(title = "Content Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                          #     column(3,selectizeInput(inputId = "bh_chart_type_kpis3", label = "Chart Type:", choices = c("area", "column"), selected = "column")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_time_kpis3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "month")),
                                          #     column(3, selectizeInput(inputId = "bh_chart_stack_kpis3", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                          #     column(3, actionButton(inputId = "bh_plot_kpis3", label = "Plot", width = "100%", style = "height:60px")),
                                          #     column(12, highchartOutput("BHPlotKpis3"))),
                                          
                                          box(title = "Content Output", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              column(6, highchartOutput("BHPlotKpisContentOutputMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisContentOutputTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_content_output", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisContentOutput")),
                                              
                                              column(6, highchartOutput("BHPlotKpisArticleContentOutputMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisArticleContentOutputTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_article_content_output", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisArticleContentOutput")),
                                              
                                              column(6, highchartOutput("BHPlotKpisVideoContentOutputMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisVideoContentOutputTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_video_content_output", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisVideoContentOutput")),
                                              
                                              column(6, highchartOutput("BHPlotKpisMemeContentOutputMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisMemeContentOutputTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_meme_content_output", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisMemeContentOutput"))
                                          ),
                                          
                                          box(title = "Followers", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              column(12, highchartOutput("BHPlotKpisTotalFollowers")),
                                              column(6, highchartOutput("BHPlotKpisNewFollowersMonth")),
                                              column(4, highchartOutput("BHPlotKpisNewFollowersTitle", height = 30)),
                                              column(2, selectizeInput(inputId = "bh_kpis_month_new_followers", label = "Month:", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(6, highchartOutput("BHPlotKpisNewFollowers"))
                                              
                                          ),
                                          
                                          # box(title = "New Followers - Month 2016", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                                          #     column(12, highchartOutput("BHPlotKpisNewFollowersMonth16", height = 478))),
                                          # 
                                          # box(title = "Reach - Month 2016", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                          #     column(12, highchartOutput("BHPlotKpisReachMonth16", height = 478))),
                                          
                                          box(title = "Content Views - Reach - Engagement", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              column(6, highchartOutput("BHPlotKpisContentViewsMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisContentViewsTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_content_views", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisContentViews")),
                                              
                                              column(6, highchartOutput("BHPlotKpisReachMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisReachTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_reach", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisReach")),
                                              
                                              column(6, highchartOutput("BHPlotKpisEngagementMonth")),
                                              column(6, 
                                                     column(8, highchartOutput("BHPlotKpisEngagementTitle", height = 30)),
                                                     column(4, selectizeInput(inputId = "bh_kpis_month_engagement", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y")))
                                              ),
                                              column(6, highchartOutput("BHPlotKpisEngagement"))
                                              
                                          ),
                                          
                                          box(title = "Articles", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                              column(4, selectizeInput(inputId = "bh_kpis_month_articles", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(4, selectizeInput(inputId = "bh_articles_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                              
                                              column(4, selectizeInput(inputId = "bh_articles_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                              
                                              column(12, highchartOutput("BHPlotKpisArticles"))),
                                          
                                          box(title = "All Videos", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                              column(4, selectizeInput(inputId = "bh_kpis_month_all_videos", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(4, selectizeInput(inputId = "bh_all_videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                              
                                              column(4, selectizeInput(inputId = "bh_all_videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                              
                                              column(12, highchartOutput("BHPlotKpisAllVideos"))),
                                          
                                          
                                          box(title = "Videos", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                              column(4, selectizeInput(inputId = "bh_kpis_month_videos", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(4, selectizeInput(inputId = "bh_videos_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                              
                                              column(4, selectizeInput(inputId = "bh_videos_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                              
                                              column(12, highchartOutput("BHPlotKpisVideos"))),
                                          
                                          box(title = "Video Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                              column(4, selectizeInput(inputId = "bh_kpis_month_video_memes", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(4, selectizeInput(inputId = "bh_video_memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions", "Reach"), selected = "Content Views")),
                                              
                                              column(4, selectizeInput(inputId = "bh_video_memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                              
                                              column(12, highchartOutput("BHPlotKpisVideoMemes"))),
                                          
                                          box(title = "Memes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                              column(4, selectizeInput(inputId = "bh_kpis_month_memes", label = "Month: ", choices = as.character(format(seq(as.Date(paste(format(range(DataBH$date)[1], "%b %Y"), "01"), "%b %Y %d"), as.Date(paste(format(range(DataBH$date)[2], "%b %Y"), "01"), "%b %Y %d"), by = "month"), "%b %Y")), selected = format(Sys.Date(), "%b %Y"))),
                                              column(4, selectizeInput(inputId = "bh_memes_kpi_select_variable", label = "Show: ", choices = c("Content Views", "Interactions"), selected = "Content Views")),
                                              
                                              column(4, selectizeInput(inputId = "bh_memes_kpi_select_repost", label = "Posts: ", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                              
                                              column(12, highchartOutput("BHPlotKpisMemes")))
                                          
                                 )
                                 , width = 12))
                        ),
                      
                      
                        # 3. Top Posts -------------------------------------------------------------------------------------------------------
                        
                        tabItem(tabName = "topposts", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 # 3.1. Top Posts - We Are Mitú -----------------------------------------------------------------------------------
                                 
                                 tabPanel("We Are Mitú",
                                          
                                          tabsetPanel(
                                            
                                            # 3.1.1. Articles -------------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Top 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "article_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "article_select_sort_by_top", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "article_select_plot_variable_top", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "article_select_categories_top", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         
                                                         # column(12, uiOutput("article_select_categories_top")),
                                                         
                                                         column(4, actionButton(inputId = "article_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         # tagList(shiny::icon("fa fa-chevron-left", "fa-2x"), " Previous 10")
                                                         
                                                         column(4, highchartOutput("Plot1_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "article_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot1", height = 450))),
                                                     
                                                     box(title = "Top 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot2")),
                                                     
                                                     box(title = "Top 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("Plot21")),
                                                         column(5,highchartOutput("Plot22"))),
                                                     
                                                     box(title = "Top 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars1", strong("Variables"), names(DataArticles),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table1"))
                                                     
                                            ),
                                            
                                            # 3.1.2. Videos ---------------------------------------------------------------------------------
                                            
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Top 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "video_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "video_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "video_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot5_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "video_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot5", height = 450))),
                                                     
                                                     box(title = "Top 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("Plot53")),
                                                         column(5, highchartOutput("Plot54"))),
                                                     
                                                     box(title = "Top 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot52")),
                                                     
                                                     box(title = "Top 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars2", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table3"))
                                                     
                                            ),
                                            
                                            # 3.1.3. Video Memes ----------------------------------------------------------------------------
                                            
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Top 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "video_meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_meme_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "video_meme_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "video_meme_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot5b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "video_meme_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot5b", height = 450))),
                                                     
                                                     box(title = "Top 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("Plot53b")),
                                                         column(5, highchartOutput("Plot54b"))),
                                                     
                                                     box(title = "Top 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot52b")),
                                                     
                                                     box(title = "Top 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars2b", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table3b"))
                                                     
                                            ),
                                            
                                            # 3.1.4. Memes ----------------------------------------------------------------------------------
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Top 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "meme_select_sort_by_top", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "meme_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot7_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "meme_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot7", height = 450))),
                                                     
                                                     box(title = "Top 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("Plot71")),
                                                         column(5,highchartOutput("Plot72"))),
                                                     
                                                     box(title = "Top 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars3", strong("Variables"), names(DataPhotos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table5"))
                                                     
                                            ))),
                                 
                                 # 3.2. Top Posts - Bad Hombres ---------------------------------------------------------------------------
                                 
                                 tabPanel("Bad Hombres",
                                          
                                          tabsetPanel(
                                            
                                            # 3.2.1. Articles ------------------------------------------------------------------------
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Top 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_article_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_article_select_sort_by_top", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_article_select_plot_variable_top", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_article_select_categories_top", label = "Categories:", choices = as.character(unique(DataArticlesBH$category)[order(unique(DataArticlesBH$category))]), selected = as.character(unique(DataArticlesBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_article_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot1_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_article_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         column(12, highchartOutput("BH_Plot1", height = 450))),
                                                     
                                                     box(title = "Top 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot2")),
                                                     
                                                     box(title = "Top 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("BH_Plot21")),
                                                         column(5,highchartOutput("BH_Plot22"))),
                                                     
                                                     box(title = "Top 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars1", strong("Variables"), names(DataArticlesBH),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table1"))
                                            ),
                                            
                                            # 3.2.2. Videos ------------------------------------------------------------------------
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Top 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_video_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_video_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideosBH$category)[order(unique(DataVideosBH$category))]), selected = as.character(unique(DataVideosBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot5_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot5", height = 450))),
                                                     
                                                     box(title = "Top 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("BH_Plot53")),
                                                         column(5, highchartOutput("BH_Plot54"))),
                                                     
                                                     box(title = "Top 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot52")),
                                                     
                                                     box(title = "Top 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars2", strong("Variables"), names(DataVideosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table3"))
                                            ),
                                            
                                            # 3.2.3. Video Memes ------------------------------------------------------------------------
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Top 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_video_meme_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideosBH$category)[order(unique(DataVideosBH$category))]), selected = as.character(unique(DataVideosBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_meme_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot5b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_meme_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot5b", height = 450))),
                                                     
                                                     box(title = "Top 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("BH_Plot53b")),
                                                         column(5, highchartOutput("BH_Plot54b"))),
                                                     
                                                     box(title = "Top 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot52b")),
                                                     
                                                     box(title = "Top 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars2b", strong("Variables"), names(DataVideosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table3b"))
                                            ),
                                            
                                            # 3.2.4. Memes ------------------------------------------------------------------------
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Top 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_meme_select_sort_by_top", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "bh_meme_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot7_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_meme_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot7", height = 450))),
                                                     
                                                     box(title = "Top 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("BH_Plot71")),
                                                         column(5,highchartOutput("BH_Plot72"))),
                                                     
                                                     box(title = "Top 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars3", strong("Variables"), names(DataPhotosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table5"))
                                            ))),
                                 
                                 # 3.3. Top Posts - Fierce ---------------------------------------------------------------------------
                                 
                                 tabPanel("Fierce",
                                          
                                          tabsetPanel(
                                            
                                            # 3.3.1. Articles ------------------------------------------------------------------------
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Top 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_article_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_article_select_sort_by_top", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_article_select_plot_variable_top", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_article_select_categories_top", label = "Categories:", choices = as.character(unique(DataArticlesFC$category)[order(unique(DataArticlesFC$category))]), selected = as.character(unique(DataArticlesFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_article_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot1_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_article_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         column(12, highchartOutput("FC_Plot1", height = 450))),
                                                     
                                                     box(title = "Top 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot2")),
                                                     
                                                     box(title = "Top 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("FC_Plot21")),
                                                         column(5,highchartOutput("FC_Plot22"))),
                                                     
                                                     box(title = "Top 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars1", strong("Variables"), names(DataArticlesFC),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table1"))
                                            ),
                                            
                                            # 3.3.2. Videos ------------------------------------------------------------------------
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Top 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_video_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_video_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideosFC$category)[order(unique(DataVideosFC$category))]), selected = as.character(unique(DataVideosFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot5_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot5", height = 450))),
                                                     
                                                     box(title = "Top 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("FC_Plot53")),
                                                         column(5, highchartOutput("FC_Plot54"))),
                                                     
                                                     box(title = "Top 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot52")),
                                                     
                                                     box(title = "Top 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars2", strong("Variables"), names(DataVideosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table3"))
                                            ),
                                            
                                            # 3.3.3. Video Memes ------------------------------------------------------------------------
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Top 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_sort_by_top", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_video_meme_select_categories_top", label = "Categories:", choices = as.character(unique(DataVideosFC$category)[order(unique(DataVideosFC$category))]), selected = as.character(unique(DataVideosFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_meme_prev_ten_top", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot5b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_meme_next_ten_top", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot5b", height = 450))),
                                                     
                                                     box(title = "Top 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("FC_Plot53b")),
                                                         column(5, highchartOutput("FC_Plot54b"))),
                                                     
                                                     box(title = "Top 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot52b")),
                                                     
                                                     box(title = "Top 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars2b", strong("Variables"), names(DataVideosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table3b"))
                                            ),
                                            
                                            # 3.3.4. Memes ------------------------------------------------------------------------
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Top 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_meme_select_original_repost_top", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_meme_select_sort_by_top", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_meme_select_plot_variable_top", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "fc_meme_prev_ten_top", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot7_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_meme_next_ten_top", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot7", height = 450))),
                                                     
                                                     box(title = "Top 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("FC_Plot71")),
                                                         column(5,highchartOutput("FC_Plot72"))),
                                                     
                                                     box(title = "Top 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars3", strong("Variables"), names(DataPhotosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table5"))
                                            )))
                                 
                                 , width = 12))
                          
                        ),
                        

                        # 4. Bottom Posts -----------------------------------------------------------------------------------------
                        
                        tabItem(tabName = "bottomposts", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 # 4.1. Bottom Posts - We Are Mitú ----------------------------------------------------------------
                                 
                                 tabPanel("We Are Mitú",
                                          
                                          tabsetPanel(
                                            
                                            # 4.1.1. Articles ----------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Bottom 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, 
                                                         column(4, selectizeInput(inputId = "article_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "article_select_sort_by_bottom", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "article_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "article_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "article_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot3_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "article_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot3", height = 450))),
                                                     
                                                     box(title = "Bottom 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot4")),
                                                     
                                                     box(title = "Bottom 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("Plot41")),
                                                         column(5,highchartOutput("Plot42"))),
                                                     
                                                     box(title = "Bottom 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars4", strong("Variables"), names(DataArticles),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table2"))
                                                     
                                            ),
                                            
                                            # 4.1.2. Videos ------------------------------------------------------------------------------
                                            
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Bottom 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "video_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "video_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "video_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot6_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "video_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot6", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("Plot63")),
                                                         column(5, highchartOutput("Plot64"))),
                                                     
                                                     box(title = "Bottom 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot62")),
                                                     
                                                     box(title = "Bottom 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars5", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table4"))
                                                     
                                            ),
                                            
                                            # 4.1.3. Video Memes -------------------------------------------------------------------------
                                            
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Bottom 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "video_meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_meme_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "video_meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "video_meme_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "video_meme_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot6b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "video_meme_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot6b", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("Plot63b")),
                                                         column(5, highchartOutput("Plot64b"))),
                                                     
                                                     box(title = "Bottom 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("Plot62b")),
                                                     
                                                     box(title = "Bottom 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars5b", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table4b"))
                                                     
                                            ),
                                            
                                            # 4.1.4. Memes -------------------------------------------------------------------------------
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Bottom 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "meme_select_sort_by_bottom", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "meme_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-left")
                                                         
                                                         column(4, highchartOutput("Plot8_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "meme_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         # , icon = icon("fa fa-chevron-right")
                                                         column(12, highchartOutput("Plot8", height = 450))),
                                                     
                                                     box(title = "Bottom 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("Plot81")),
                                                         column(5,highchartOutput("Plot82"))),
                                                     
                                                     box(title = "Bottom 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("show_vars6", strong("Variables"), names(DataPhotos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("Table6"))
                                                     
                                            ))),
                                 
                                 # 4.2. Bottom Posts - Bad Hombres ---------------------------------------------------------------------------
                                 
                                 tabPanel("Bad Hombres",
                                          
                                          tabsetPanel(
                                            
                                            # 4.2.1. Articles ------------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Bottom 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, 
                                                         column(4, selectizeInput(inputId = "bh_article_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_article_select_sort_by_bottom", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_article_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_article_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataArticlesBH$category)[order(unique(DataArticlesBH$category))]), selected = as.character(unique(DataArticlesBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_article_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot3_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_article_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot3", height = 450))),
                                                     
                                                     box(title = "Bottom 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot4")),
                                                     
                                                     box(title = "Bottom 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("BH_Plot41")),
                                                         column(5,highchartOutput("BH_Plot42"))),
                                                     
                                                     box(title = "Bottom 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars4", strong("Variables"), names(DataArticlesBH),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table2"))
                                                     
                                            ),
                                            
                                            # 4.2.2. Videos ------------------------------------------------------------------------------
                                            
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Bottom 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_video_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_video_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideosBH$category)[order(unique(DataVideosBH$category))]), selected = as.character(unique(DataVideosBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot6_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot6", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("BH_Plot63")),
                                                         column(5, highchartOutput("BH_Plot64"))),
                                                     
                                                     box(title = "Bottom 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot62")),
                                                     
                                                     box(title = "Bottom 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars5", strong("Variables"), names(DataVideosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table4"))
                                                     
                                            ),
                                            
                                            # 4.2.3. Video Memes ------------------------------------------------------------------------------
                                            
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Bottom 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_video_meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "bh_video_meme_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideosBH$category)[order(unique(DataVideosBH$category))]), selected = as.character(unique(DataVideosBH$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_meme_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot6b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_video_meme_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot6b", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("BH_Plot63b")),
                                                         column(5, highchartOutput("BH_Plot64b"))),
                                                     
                                                     box(title = "Bottom 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("BH_Plot62b")),
                                                     
                                                     box(title = "Bottom 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars5b", strong("Variables"), names(DataVideosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table4b"))
                                                     
                                            ),
                                            
                                            # 4.2.4. Memes ------------------------------------------------------------------------------
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Bottom 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "bh_meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_meme_select_sort_by_bottom", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "bh_meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "bh_meme_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("BH_Plot8_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "bh_meme_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("BH_Plot8", height = 450))),
                                                     
                                                     box(title = "Bottom 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("BH_Plot81")),
                                                         column(5,highchartOutput("BH_Plot82"))),
                                                     
                                                     box(title = "Bottom 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("bh_show_vars6", strong("Variables"), names(DataPhotosBH),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("BH_Table6"))
                                                     
                                            ))),
                                 
                                 # 4.2. Bottom Posts - Fierce ---------------------------------------------------------------------------
                                 
                                 tabPanel("Fierce",
                                          
                                          tabsetPanel(
                                            
                                            # 4.2.1. Articles ------------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     box(title = "Bottom 10 Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, 
                                                         column(4, selectizeInput(inputId = "fc_article_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_article_select_sort_by_bottom", label = "Sort By:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_article_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Link Clicks", "Interactions", "Reach"), selected = "Link Clicks")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_article_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataArticlesFC$category)[order(unique(DataArticlesFC$category))]), selected = as.character(unique(DataArticlesFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_article_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot3_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_article_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot3", height = 450))),
                                                     
                                                     box(title = "Bottom 10 IR & CTR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot4")),
                                                     
                                                     box(title = "Bottom 10 Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("FC_Plot41")),
                                                         column(5,highchartOutput("FC_Plot42"))),
                                                     
                                                     box(title = "Bottom 10 Articles:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars4", strong("Variables"), names(DataArticlesFC),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table2"))
                                                     
                                            ),
                                            
                                            # 4.2.2. Videos ------------------------------------------------------------------------------
                                            
                                            tabPanel("Videos",
                                                     
                                                     box(title = "Bottom 5 Videos: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_video_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_video_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideosFC$category)[order(unique(DataVideosFC$category))]), selected = as.character(unique(DataVideosFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot6_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot6", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Videos: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("FC_Plot63")),
                                                         column(5, highchartOutput("FC_Plot64"))),
                                                     
                                                     box(title = "Bottom 5 Videos: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot62")),
                                                     
                                                     box(title = "Bottom 5 Videos:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars5", strong("Variables"), names(DataVideosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table4"))
                                                     
                                            ),
                                            
                                            # 4.2.3. Video Memes ------------------------------------------------------------------------------
                                            
                                            tabPanel("Video Memes",
                                                     
                                                     box(title = "Bottom 5 Video Memes: Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_sort_by_bottom", label = "Sort By:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_video_meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Video Views", "Interactions", "Reach"), selected = "Video Views")),
                                                         
                                                         column(12, selectizeInput(inputId = "fc_video_meme_select_categories_bottom", label = "Categories:", choices = as.character(unique(DataVideosFC$category)[order(unique(DataVideosFC$category))]), selected = as.character(unique(DataVideosFC$category)), multiple = TRUE)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_meme_prev_ten_bottom", label = "Previous 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot6b_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_video_meme_next_ten_bottom", label = " Next 5", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot6b", height = 450))),
                                                     
                                                     box(title = "Bottom 5 Video Memes: % Viewed", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         highchartOutput("FC_Plot62b")),
                                                     
                                                     box(title = "Bottom 5 Video Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7, highchartOutput("FC_Plot63b")),
                                                         column(5, highchartOutput("FC_Plot64b"))),
                                                     
                                                     box(title = "Bottom 5 Video Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars5b", strong("Variables"), names(DataVideosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "post_video_views"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table4b"))
                                                     
                                            ),
                                            
                                            # 4.2.4. Memes ------------------------------------------------------------------------------
                                            
                                            tabPanel("Memes",
                                                     
                                                     box(title = "Bottom 10 Memes: Photo Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(4, selectizeInput(inputId = "fc_meme_select_original_repost_bottom", label = "Reposts:", choices = c("All", "Originals", "Reposts"), selected = "All")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_meme_select_sort_by_bottom", label = "Sort By:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, selectizeInput(inputId = "fc_meme_select_plot_variable_bottom", label = "Plot Variable:", choices = c("Interactions", "Reach"), selected = "Reach")),
                                                         
                                                         column(4, actionButton(inputId = "fc_meme_prev_ten_bottom", label = "Previous 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(4, highchartOutput("FC_Plot8_Title", height = 50)),
                                                         
                                                         column(4, actionButton(inputId = "fc_meme_next_ten_bottom", label = " Next 10", width = "100%", style = "height:40px")),
                                                         
                                                         column(12, highchartOutput("FC_Plot8", height = 450))),
                                                     
                                                     box(title = "Bottom 10 Memes: Reaction Rates & Fan Vs. Viral", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         column(7,highchartOutput("FC_Plot81")),
                                                         column(5,highchartOutput("FC_Plot82"))),
                                                     
                                                     box(title = "Bottom 10 Memes:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                         selectizeInput("fc_show_vars6", strong("Variables"), names(DataPhotosFC),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares"), multiple = TRUE ),
                                                         DT::dataTableOutput("FC_Table6"))
                                                     
                                            )))
                               
                                 , width = 12))
                        ),
                        
                        # 5. Authors Performance ----------------------------------------------------------------------------------
                        
                        tabItem(tabName = "authors", fluidRow(
                          
                          box(title = "Last Week's Visitors by Author", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                              highchartOutput("PlotAuthors1")),
                          
                          box(title = "Last Month's Visitors by Author", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                              highchartOutput("PlotAuthors2")),
                          
                          valueBox("Parse.ly", "click for More Info" , icon = icon("fa fa-eye"), color = "light-blue" , width = 12, href = "https://dash.parsely.com/wearemitu.com/authors/?minutes=today&metric=visitors")
                          
                        )),
                        
                        # 6. Content Categories -----------------------------------------------------------------------------------
                        
                        tabItem(tabName = "categories", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 # 6.1. Content Categories - We Are Mitú --------------------------------------------------------------
                                 
                                 tabPanel("We Are Mitú",
                                          
                                          tabsetPanel(
                                            
                                            # 6.1.1. Articles ------------------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     #  6.1.1.1. Top 5 Days ------------------------------------------------------------------------
                                                     
                                                     box(title = "Top 5 Days by Category", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategories4")),
                                                     
                                                     #  6.1.1.2. Category Pie --------------------------------------------------------------------
                                                     
                                                     box(title = "Category Distribution", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategories1")),
                                                     
                                                     #  6.1.1.3. Avg. Reach & Link Clicks --------------------------------------------------------
                                                     
                                                     box(title = "Avg. Reach & Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategories2")),
                                                     
                                                     #  6.1.1.4. Avg. CTR & IR -------------------------------------------------------------------
                                                     
                                                     box(title = "Avg. CTR & IR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategories3")),
                                                     
                                                     #  6.1.1.5. Overview ------------------------------------------------------------------------
                                                     
                                                     box(title = "Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4,actionButton(inputId = "plot_categories", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesOverview1", height = 450))),
                                                     
                                                     
                                                     box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons2", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4,actionButton(inputId = "plot_categories2", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesOverview2", height = 450)))
                                                     
                                            ),
                                            
                                            # 6.1.2. Videos --------------------------------------------------------------------------------------
                                            
                                            tabPanel("Videos",
                                                     
                                                     #  6.1.2.1. Top 5 Days ------------------------------------------------------------------------
                                                     
                                                     box(title = "Top 5 Days by Category", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideo4")),
                                                     
                                                     #  6.1.2.2. Category Pie --------------------------------------------------------------------
                                                     
                                                     box(title = "Category Distribution", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideo1")),
                                                     
                                                     #  6.1.2.3. Avg. Reach & Video Views --------------------------------------------------------
                                                     
                                                     box(title = "Avg. Reach & Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideo2")),
                                                     
                                                     #  6.1.2.4. Avg. IR -------------------------------------------------------------------------
                                                     
                                                     box(title = "Avg. IR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideo3")),
                                                     
                                                     #  6.1.2.5. Overview ------------------------------------------------------------------------
                                                     
                                                     box(title = "Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons_video", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type_video", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total_video", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time_video", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack_video", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4, actionButton(inputId = "plot_categories_video", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesVideoOverview1", height = 450))),
                                                     
                                                     
                                                     box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons_video2", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type_video2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total_video2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time_video2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack_video2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4, actionButton(inputId = "plot_categories_video2", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesVideoOverview2", height = 450)))
                                                     
                                            ),
                                            
                                            # 6.1.3. Video Memes ---------------------------------------------------------------------------------
                                            
                                            tabPanel("Video Memes",
                                                     
                                                     #  6.1.3.1. Top 5 Days ------------------------------------------------------------------------
                                                     
                                                     box(title = "Top 5 Days by Category", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideoMeme4")),
                                                     
                                                     #  6.1.3.2. Category Pie --------------------------------------------------------------------
                                                     
                                                     box(title = "Category Distribution", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideoMeme1")),
                                                     
                                                     #  6.1.3.3. Avg. Reach & Video Views --------------------------------------------------------
                                                     
                                                     box(title = "Avg. Reach & Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideoMeme2")),
                                                     
                                                     #  6.1.3.4. Avg. IR -------------------------------------------------------------------------
                                                     
                                                     box(title = "Avg. IR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("PlotCategoriesVideoMeme3")),
                                                     
                                                     #  6.1.3.5. Overview ------------------------------------------------------------------------
                                                     
                                                     box(title = "Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons_video_meme", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type_video_meme", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total_video_meme", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time_video_meme", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack_video_meme", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4, actionButton(inputId = "plot_categories_video_meme", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesVideoMemeOverview1", height = 450))),
                                                     
                                                     
                                                     box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "category_buttons_video_meme2", label = "Categories:", choices = as.character(unique(DataVideos$category)[order(unique(DataVideos$category))]), selected = as.character(unique(DataVideos$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "chart_type_video_meme2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "chart_avg_total_video_meme2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "chart_time_video_meme2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "chart_stack_video_meme2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4, actionButton(inputId = "plot_categories_video_meme2", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("PlotCategoriesVideoMemeOverview2", height = 450)))
                                                     
                                            )
                                            
                                          )),
                                 
                                 # 6.2. Content Categories - Bad Hombres -------------------------------------------------------------------------
                                 
                                 tabPanel("Bad Hombres (WIP*)",
                                          
                                          tabsetPanel(
                                            
                                            # 6.2.1. Articles -----------------------------------------------------------------------------------
                                            
                                            tabPanel("Articles",
                                                     
                                                     #  6.2.1.1. Top 5 Days ------------------------------------------------------------------------
                                                     
                                                     box(title = "Top 5 Days by Category", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("BHPlotCategories4")),
                                                     
                                                     #  6.2.1.2. Category Pie --------------------------------------------------------------------
                                                     
                                                     box(title = "Category Distribution", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("BHPlotCategories1")),
                                                     
                                                     #  6.2.1.3. Avg. Reach & Link Clicks --------------------------------------------------------
                                                     
                                                     box(title = "Avg. Reach & Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("BHPlotCategories2")),
                                                     
                                                     #  6.2.1.4. Avg. CTR & IR -------------------------------------------------------------------
                                                     
                                                     box(title = "Avg. CTR & IR", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
                                                         highchartOutput("BHPlotCategories3")),
                                                     
                                                     #  6.2.1.5. Overview ------------------------------------------------------------------------
                                                     
                                                     box(title = "Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "bh_category_buttons", label = "Categories:", choices = as.character(unique(DataArticlesBH$category)[order(unique(DataArticlesBH$category))]), selected = as.character(unique(DataArticlesBH$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "bh_chart_type", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "bh_chart_avg_total", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "bh_chart_time", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "bh_chart_stack", label = "Stacked:", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4,actionButton(inputId = "bh_plot_categories", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("BHPlotCategoriesOverview1", height = 450))),
                                                     
                                                     
                                                     box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12,
                                                         
                                                         column(4, selectizeInput(inputId = "bh_category_buttons2", label = "Categories:", choices = as.character(unique(DataArticlesBH$category)[order(unique(DataArticlesBH$category))]), selected = as.character(unique(DataArticlesBH$category)), multiple = TRUE)),
                                                         
                                                         column(2,selectizeInput(inputId = "bh_chart_type2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "column")),
                                                         column(2, selectizeInput(inputId = "bh_chart_avg_total2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                                         column(2, selectizeInput(inputId = "bh_chart_time2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                                         column(2, selectizeInput(inputId = "bh_chart_stack2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                                         
                                                         column(width = 8, offset = 4,actionButton(inputId = "bh_plot_categories2", label = "Plot", width = "100%")),
                                                         
                                                         column(12, highchartOutput("BHPlotCategoriesOverview2", height = 450)))
                                                     
                                            )
                                            
                                          )
                                 )
                                 
                                 , width = 12))),
                        
                        #  7. Reposts ----------------------------------------------------------------------------------------------
              
                        tabItem(tabName = "reposts", fluidRow(
                          
                          tabBox(title = "",
                                 
                                 #  7.1 Articles -----------------------------------------------------------------------------------
                                 
                                 tabPanel("Articles",
                                          
                                          box(title = "Number of Posts", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(3,selectizeInput(inputId = "chart_type_repost_articles", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(3, selectizeInput(inputId = "chart_time_repost_articles", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_articles", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_articles", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewArticles1", height = 450))),
                                          
                                          box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_articles2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_articles2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_articles2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_articles2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_articles2", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewArticles2", height = 450))),
                                          
                                          box(title = "Link Clicks", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_articles3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_articles3", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_articles3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_articles3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_articles3", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewArticles3", height = 450))),
                                          
                                          box(title = "Reposts:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              selectizeInput("show_vars_repost_articles1", strong("Variables"), names(DataArticles),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                              DT::dataTableOutput("TableRepostsArticles1")),
                                          
                                          conditionalPanel(
                                            condition = "input.TableRepostsArticles1_rows_selected != 0",
                                            box( title = "Breakdown", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                 column(1,htmlOutput("picture_article")),
                                                 column(11,highchartOutput("TitleRepostSelectedArticles1", height = 200)),
                                                 column(4,highchartOutput("PlotRepostSelectedArticles1")),
                                                 column(4,highchartOutput("PlotRepostSelectedArticles2")),
                                                 column(4,highchartOutput("PlotRepostSelectedArticles3")),
                                                 column(12,
                                                        selectizeInput("show_vars_repost_selected_articles1", strong("Variables"), names(DataArticles),selected = c("headline","date","post_reach","interaction_rate","link_clicks","ctr", "times_repost"), multiple = TRUE ),
                                                        DT::dataTableOutput("TableRepostSelectedArticles1")
                                                 )
                                                 
                                            ))
                                 ),
                                 
                                 #  7.2 Videos -------------------------------------------------------------------------------------
                                 
                                 tabPanel("Videos",
                                          
                                          box(title = "Number of Posts", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(3,selectizeInput(inputId = "chart_type_repost_videos", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(3, selectizeInput(inputId = "chart_time_repost_videos", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_videos", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_videos", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideos1", height = 450))),
                                          
                                          box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_videos2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_videos2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_videos2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_videos2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_videos2", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideos2", height = 450))),
                                          
                                          box(title = "Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_videos3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_videos3", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_videos3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_videos3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_videos3", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideos3", height = 450))),
                                          
                                          box(title = "Reposts:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              selectizeInput("show_vars_repost_videos1", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","post_video_views", "interaction_rate", "times_repost"), multiple = TRUE ),
                                              DT::dataTableOutput("TableRepostsVideos1")),
                                          
                                          conditionalPanel(
                                            condition = "input.TableRepostsVideos1_rows_selected != 0",
                                            box( title = "Breakdown", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                 column(1,htmlOutput("picture_video")),
                                                 column(11,highchartOutput("TitleRepostSelectedVideos1", height = 200)),
                                                 column(4,highchartOutput("PlotRepostSelectedVideos1")),
                                                 column(4,highchartOutput("PlotRepostSelectedVideos2")),
                                                 column(4,highchartOutput("PlotRepostSelectedVideos3")),
                                                 column(12,
                                                        selectizeInput("show_vars_repost_selected_videos1", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","post_video_views", "interaction_rate", "times_repost"), multiple = TRUE ),
                                                        DT::dataTableOutput("TableRepostSelectedVideos1")
                                                 )
                                                 
                                            ))
                                 ),
                                 
                                 #  7.3 Video Memes -------------------------------------------------------------------------------------
                                 
                                 tabPanel("Video Memes",
                                          
                                          box(title = "Number of Posts", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(3,selectizeInput(inputId = "chart_type_repost_video_memes", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(3, selectizeInput(inputId = "chart_time_repost_video_memes", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_video_memes", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_video_memes", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideoMemes1", height = 450))),
                                          
                                          box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_video_memes2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_video_memes2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_video_memes2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_video_memes2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_video_memes2", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideoMemes2", height = 450))),
                                          
                                          box(title = "Video Views", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_video_memes3", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_video_memes3", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_video_memes3", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_video_memes3", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_video_memes3", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewVideoMemes3", height = 450))),
                                          
                                          box(title = "Reposts:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              selectizeInput("show_vars_repost_video_memes1", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","post_video_views", "interaction_rate", "times_repost"), multiple = TRUE ),
                                              DT::dataTableOutput("TableRepostsVideoMemes1")),
                                          
                                          conditionalPanel(
                                            condition = "input.TableRepostsVideoMemes1_rows_selected != 0",
                                            box( title = "Breakdown", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                 column(1,htmlOutput("picture_video_meme")),
                                                 column(11,highchartOutput("TitleRepostSelectedVideoMemes1", height = 200)),
                                                 column(4,highchartOutput("PlotRepostSelectedVideoMemes1")),
                                                 column(4,highchartOutput("PlotRepostSelectedVideoMemes2")),
                                                 column(4,highchartOutput("PlotRepostSelectedVideoMemes3")),
                                                 column(12,
                                                        selectizeInput("show_vars_repost_selected_video_memes1", strong("Variables"), names(DataVideos),selected = c("sharetext","date","post_reach","post_video_views", "interaction_rate", "times_repost"), multiple = TRUE ),
                                                        DT::dataTableOutput("TableRepostSelectedVideoMemes1")
                                                 )
                                                 
                                            ))
                                          
                                 ),
                                 
                                 #  7.4 Memes -------------------------------------------------------------------------------------------
                                 
                                 tabPanel("Memes",
                                          
                                          box(title = "Number of Posts", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(3,selectizeInput(inputId = "chart_type_repost_memes", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(3, selectizeInput(inputId = "chart_time_repost_memes", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_memes", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_memes", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewMemes1", height = 450))),
                                          
                                          box(title = "Reach", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              
                                              column(2,selectizeInput(inputId = "chart_type_repost_memes2", label = "Chart Type:", choices = c("line", "spline", "area", "column"), selected = "area")),
                                              column(2, selectizeInput(inputId = "chart_avg_total_repost_memes2", label = "Charting Method:", choices = c("average", "sum"), selected = "average")),
                                              column(2, selectizeInput(inputId = "chart_time_repost_memes2", label = "Chart Timeline:", choices = c("day", "week", "month"), selected = "week")),
                                              column(3, selectizeInput(inputId = "chart_stack_repost_memes2", label = "Stacked (Area & Column Only):", choices = c("none","normal", "percent"), selected = "normal")),
                                              column(3, actionButton(inputId = "plot_repost_memes2", label = "Plot", width = "100%", style = "height:60px")),
                                              column(12, highchartOutput("PlotRepostsOverviewMemes2", height = 450))),
                                          
                                          box(title = "Reposts:", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                              selectizeInput("show_vars_repost_memes1", strong("Variables"), names(DataPhotos),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "times_repost"), multiple = TRUE ),
                                              DT::dataTableOutput("TableRepostsMemes1")),
                                          
                                          conditionalPanel(
                                            condition = "input.TableRepostsMemes1_rows_selected != 0",
                                            box( title = "Breakdown", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                 column(1,htmlOutput("picture_meme")),
                                                 column(11,highchartOutput("TitleRepostSelectedMemes1", height = 200)),
                                                 column(4,highchartOutput("PlotRepostSelectedMemes1")),
                                                 column(4,highchartOutput("PlotRepostSelectedMemes2")),
                                                 column(12,
                                                        selectizeInput("show_vars_repost_selected_memes1", strong("Variables"), names(DataArticles),selected = c("sharetext","date","post_reach","total_comments","total_likes","total_shares", "times_repost"), multiple = TRUE ),
                                                        DT::dataTableOutput("TableRepostSelectedMemes1")
                                                 )
                                            ))
                                 )
                                 
                                 , width = 12))),
                        
                        #  8. Sprint -----------------------------------------------------------------------------------------------
                        
                        tabItem(tabName = "sprint",
                                
                                box(title = "Pageviews per Post", solidHeader = FALSE, collapsible = FALSE, width = 12,
                                    
                                    infoBox("Goal", formatC(sprint_benchs[1], format = "f", digits = 0, big.mark = ","), icon = icon("fa fa-crosshairs"),width = 6, fill = TRUE, color = "light-blue"),
                                    infoBoxOutput("infobox1", width = 6)),
                                
                                box(title = "Photoviews per Post", solidHeader = FALSE, collapsible = FALSE, width = 12,
                                    
                                    infoBox("Goal", formatC(sprint_benchs[2], format = "f", digits = 0, big.mark = ","), icon = icon("fa fa-crosshairs"),width = 6, fill = TRUE, color = "light-blue"),
                                    
                                    infoBoxOutput("infobox2", width = 6)),                               
                                
                                box(title = "Videoviews per Post", solidHeader = FALSE, collapsible = FALSE, width = 12,
                                    
                                    infoBox("Goal", formatC(sprint_benchs[3], format = "f", digits = 0, big.mark = ","), icon = icon("fa fa-crosshairs"),width = 6, fill = TRUE, color = "light-blue"),
                                    
                                    infoBoxOutput("infobox3", width = 6))
                                
                        )
                      )
                    ))
)





# SERVER  ==========================================================================================================================

server <- function(input, output, session) {
  
  # 1. Reactive Data --------------------------------------------------------------------------------------------------------------------
  
  # 1.1 Reactive Data - We Are Mitú ---------------------------------------------------------------------------------------------------------------
  
  number_article_top <- reactiveValues(n = 0)
  number_article_bottom <- reactiveValues(n = 0)
  number_video_top <- reactiveValues(n = 0)
  number_video_bottom <- reactiveValues(n = 0)
  number_video_meme_top <- reactiveValues(n = 0)
  number_video_meme_bottom <- reactiveValues(n = 0)
  number_meme_top <- reactiveValues(n = 0)
  number_meme_bottom <- reactiveValues(n = 0)
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$article_select_original_repost_top
    input$article_select_categories_top
    input$article_select_sort_by_top
  }, {
    number_article_top$n <- 0
  })
  
  observeEvent({
    input$article_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$article_select_original_repost_top
    input$article_select_categories_top
    input$article_select_sort_by_top
    }, {
      
      DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
      
      if(input$article_select_original_repost_top == "Originals"){
        DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      }
      
      else if (input$article_select_original_repost_top == "Reposts"){
        DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
      }
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_top),]
      
      number_article_top$n <- min(number_article_top$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$article_prev_ten_top, {
    number_article_top$n <- max(number_article_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$article_select_original_repost_bottom
    input$article_select_categories_bottom
    input$article_select_sort_by_bottom
  }, {
    number_article_bottom$n <- 0
  })
  
  observeEvent({
    input$article_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$article_select_original_repost_bottom
    input$article_select_categories_bottom
    input$article_select_sort_by_bottom
  }, {
    DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    if(input$article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_bottom),]
    
    number_article_bottom$n <- min(number_article_bottom$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$article_prev_ten_bottom, {
    number_article_bottom$n <- max(number_article_bottom$n - 10,10)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_select_original_repost_top
    input$video_select_categories_top
    input$video_select_sort_by_top
  }, {
    number_video_top$n <- 0
  })
  
  observeEvent({
    input$video_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_select_original_repost_top
    input$video_select_categories_top
    input$video_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    if(input$video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }

    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_top),]
    
    number_video_top$n <- min(number_video_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$video_prev_ten_top, {
    number_video_top$n <- max(number_video_top$n - 5,5)
  })

  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_select_original_repost_bottom
    input$video_select_categories_bottom
    input$video_select_sort_by_bottom
  }, {
    number_video_bottom$n <- 0
  })
  
  observeEvent({
    input$video_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_select_original_repost_bottom
    input$video_select_categories_bottom
    input$video_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    if(input$video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_bottom),]
    
    number_video_bottom$n <- min(number_video_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$video_prev_ten_bottom, {
    number_video_bottom$n <- max(number_video_bottom$n - 5,5)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_meme_select_original_repost_top
    input$video_meme_select_categories_top
    input$video_meme_select_sort_by_top
  }, {
    number_video_meme_top$n <- 0
  })
  
  observeEvent({
    input$video_meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_meme_select_original_repost_top
    input$video_meme_select_categories_top
    input$video_meme_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    if(input$video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_top),]
    
    number_video_meme_top$n <- min(number_video_meme_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$video_meme_prev_ten_top, {
    number_video_meme_top$n <- max(number_video_meme_top$n - 5,5)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_meme_select_original_repost_bottom
    input$video_meme_select_categories_bottom
    input$video_meme_select_sort_by_bottom
  }, {
    number_video_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$video_meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$video_meme_select_original_repost_bottom
    input$video_meme_select_categories_bottom
    input$video_meme_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    if(input$video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_bottom),]
    
    number_video_meme_bottom$n <- min(number_video_meme_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$video_meme_prev_ten_bottom, {
    number_video_meme_bottom$n <- max(number_video_meme_bottom$n - 5,5)
  })
  

  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$meme_select_original_repost_top
    input$meme_select_sort_by_top
  }, {
    number_meme_top$n <- 0
  })
  
  observeEvent({
    input$meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$meme_select_original_repost_top
    input$meme_select_sort_by_top
  }, {
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    if(input$meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    number_meme_top$n <- min(number_meme_top$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$meme_prev_ten_top, {
    number_meme_top$n <- max(number_meme_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$meme_select_original_repost_bottom
    input$meme_select_sort_by_bottom
  }, {
    number_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$meme_select_original_repost_bottom
    input$meme_select_sort_by_bottom
  }, {
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    if(input$meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    number_meme_bottom$n <- min(number_meme_bottom$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$meme_prev_ten_bottom, {
    number_meme_bottom$n <- max(number_meme_bottom$n - 10,10)
  })
  
  
  WeekLinksTop <- reactive({
    
    DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    ifelse(input$article_select_sort_by_top == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = TRUE),], ifelse(input$article_select_sort_by_top == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = TRUE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = TRUE),]))
    
    if(input$article_select_original_repost_top == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$article_select_original_repost_top == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_top),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((number_article_top$n-9), 1):min(nrow(DateRangeArticles), number_article_top$n),]
    DateRangeArticles
    
  })
  
  WeekLinksBottom <- reactive({
    
    DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    ifelse(input$article_select_sort_by_bottom == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = FALSE),], ifelse(input$article_select_sort_by_bottom == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = FALSE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = FALSE),]))
    
    if(input$article_select_original_repost_bottom == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$article_select_original_repost_bottom == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_bottom),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((number_article_bottom$n-9), 1):min(nrow(DateRangeArticles), number_article_bottom$n),]
    DateRangeArticles
    
  })
  
  WeekVideosTop <- reactive({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    ifelse(input$video_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$video_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$video_select_original_repost_top == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$video_select_original_repost_top == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((number_video_top$n-4), 1):min(nrow(DateRangeVideos), number_video_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideosBottom <- reactive({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    ifelse(input$video_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$video_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$video_select_original_repost_bottom == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$video_select_original_repost_bottom == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((number_video_bottom$n-4), 1):min(nrow(DateRangeVideos), number_video_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesTop <- reactive({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    ifelse(input$video_meme_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$video_meme_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((number_video_meme_top$n-4), 1):min(nrow(DateRangeVideos), number_video_meme_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesBottom <- reactive({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    ifelse(input$video_meme_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$video_meme_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((number_video_meme_bottom$n-4), 1):min(nrow(DateRangeVideos), number_video_meme_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekPhotoTop <- reactive({
    
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),]
    
    ifelse(input$meme_select_sort_by_top == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = TRUE),])
    
    if(input$meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((number_meme_top$n-9), 1):min(nrow(DateRangeMemes), number_meme_top$n),]
    DateRangeMemes
    
  })
  
  WeekPhotoBottom <- reactive({
    
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    ifelse(input$meme_select_sort_by_bottom == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = FALSE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = FALSE),])
    
    if(input$meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((number_meme_bottom$n-9), 1):min(nrow(DateRangeMemes), number_meme_bottom$n),]
    DateRangeMemes
    
  })
  
  
  # 1.2. Reactive Data - Bad Hombres ---------------------------------------------------------------------------------------------------------------
  
  bh_number_article_top <- reactiveValues(n = 0)
  bh_number_article_bottom <- reactiveValues(n = 0)
  bh_number_video_top <- reactiveValues(n = 0)
  bh_number_video_bottom <- reactiveValues(n = 0)
  bh_number_video_meme_top <- reactiveValues(n = 0)
  bh_number_video_meme_bottom <- reactiveValues(n = 0)
  bh_number_meme_top <- reactiveValues(n = 0)
  bh_number_meme_bottom <- reactiveValues(n = 0)

  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_article_select_original_repost_top
    input$bh_article_select_categories_top
    input$bh_article_select_sort_by_top
  }, {
    bh_number_article_top$n <- 0
  })
  
  observeEvent({
    input$bh_article_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_article_select_original_repost_top
    input$bh_article_select_categories_top
    input$bh_article_select_sort_by_top
  }, {
    
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    if(input$bh_article_select_original_repost_top == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$bh_article_select_original_repost_top == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_top),]
    
    bh_number_article_top$n <- min(bh_number_article_top$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$bh_article_prev_ten_top, {
    bh_number_article_top$n <- max(bh_number_article_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_article_select_original_repost_bottom
    input$bh_article_select_categories_bottom
    input$bh_article_select_sort_by_bottom
  }, {
    bh_number_article_bottom$n <- 0
  })
  
  observeEvent({
    input$bh_article_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_article_select_original_repost_bottom
    input$bh_article_select_categories_bottom
    input$bh_article_select_sort_by_bottom
  }, {
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    if(input$bh_article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$bh_article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_bottom),]
    
    bh_number_article_bottom$n <- min(bh_number_article_bottom$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$bh_article_prev_ten_bottom, {
    bh_number_article_bottom$n <- max(bh_number_article_bottom$n - 10,10)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_select_original_repost_top
    input$bh_video_select_categories_top
    input$bh_video_select_sort_by_top
  }, {
    bh_number_video_top$n <- 0
  })
  
  observeEvent({
    input$bh_video_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_select_original_repost_top
    input$bh_video_select_categories_top
    input$bh_video_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    if(input$bh_video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_top),]
    
    bh_number_video_top$n <- min(bh_number_video_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$bh_video_prev_ten_top, {
    bh_number_video_top$n <- max(bh_number_video_top$n - 5,5)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_select_original_repost_bottom
    input$bh_video_select_categories_bottom
    input$bh_video_select_sort_by_bottom
  }, {
    bh_number_video_bottom$n <- 0
  })
  
  observeEvent({
    input$bh_video_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_select_original_repost_bottom
    input$bh_video_select_categories_bottom
    input$bh_video_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    if(input$bh_video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_bottom),]
    
    bh_number_video_bottom$n <- min(bh_number_video_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$bh_video_prev_ten_bottom, {
    bh_number_video_bottom$n <- max(bh_number_video_bottom$n - 5,5)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_meme_select_original_repost_top
    input$bh_video_meme_select_categories_top
    input$bh_video_meme_select_sort_by_top
  }, {
    bh_number_video_meme_top$n <- 0
  })
  
  observeEvent({
    input$bh_video_meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_meme_select_original_repost_top
    input$bh_video_meme_select_categories_top
    input$bh_video_meme_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    if(input$bh_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_top),]
    
    bh_number_video_meme_top$n <- min(bh_number_video_meme_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$bh_video_meme_prev_ten_top, {
    bh_number_video_meme_top$n <- max(bh_number_video_meme_top$n - 5,5)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_meme_select_original_repost_bottom
    input$bh_video_meme_select_categories_bottom
    input$bh_video_meme_select_sort_by_bottom
  }, {
    bh_number_video_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$bh_video_meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_video_meme_select_original_repost_bottom
    input$bh_video_meme_select_categories_bottom
    input$bh_video_meme_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    if(input$bh_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_bottom),]
    
    bh_number_video_meme_bottom$n <- min(bh_number_video_meme_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$bh_video_meme_prev_ten_bottom, {
    bh_number_video_meme_bottom$n <- max(bh_number_video_meme_bottom$n - 5,5)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_meme_select_original_repost_top
    input$bh_meme_select_sort_by_top
  }, {
    bh_number_meme_top$n <- 0
  })
  
  observeEvent({
    input$bh_meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_meme_select_original_repost_top
    input$bh_meme_select_sort_by_top
  }, {
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    
    if(input$bh_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    bh_number_meme_top$n <- min(bh_number_meme_top$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$bh_meme_prev_ten_top, {
    bh_number_meme_top$n <- max(bh_number_meme_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_meme_select_original_repost_bottom
    input$bh_meme_select_sort_by_bottom
  }, {
    bh_number_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$bh_meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$bh_meme_select_original_repost_bottom
    input$bh_meme_select_sort_by_bottom
  }, {
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    
    if(input$bh_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    bh_number_meme_bottom$n <- min(bh_number_meme_bottom$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$bh_meme_prev_ten_bottom, {
    bh_number_meme_bottom$n <- max(bh_number_meme_bottom$n - 10,10)
  })
  
  
  WeekLinksTopBH <- reactive({
    
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    ifelse(input$bh_article_select_sort_by_top == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = TRUE),], ifelse(input$bh_article_select_sort_by_top == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = TRUE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = TRUE),]))
    
    if(input$bh_article_select_original_repost_top == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$bh_article_select_original_repost_top == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_top),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((bh_number_article_top$n-9), 1):min(nrow(DateRangeArticles), bh_number_article_top$n),]
    DateRangeArticles
    
  })
  
  WeekLinksBottomBH <- reactive({
    
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    ifelse(input$bh_article_select_sort_by_bottom == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = FALSE),], ifelse(input$bh_article_select_sort_by_bottom == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = FALSE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = FALSE),]))
    
    if(input$bh_article_select_original_repost_bottom == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$bh_article_select_original_repost_bottom == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_bottom),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((bh_number_article_bottom$n-9), 1):min(nrow(DateRangeArticles), bh_number_article_bottom$n),]
    DateRangeArticles
    
  })
  
  WeekVideosTopBH <- reactive({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    ifelse(input$bh_video_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$bh_video_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$bh_video_select_original_repost_top == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$bh_video_select_original_repost_top == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((bh_number_video_top$n-4), 1):min(nrow(DateRangeVideos), bh_number_video_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideosBottomBH <- reactive({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    ifelse(input$bh_video_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$bh_video_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$bh_video_select_original_repost_bottom == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$bh_video_select_original_repost_bottom == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((bh_number_video_bottom$n-4), 1):min(nrow(DateRangeVideos), bh_number_video_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesTopBH <- reactive({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    ifelse(input$bh_video_meme_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$bh_video_meme_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$bh_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((bh_number_video_meme_top$n-4), 1):min(nrow(DateRangeVideos), bh_number_video_meme_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesBottomBH <- reactive({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    ifelse(input$bh_video_meme_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$bh_video_meme_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$bh_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((bh_number_video_meme_bottom$n-4), 1):min(nrow(DateRangeVideos), bh_number_video_meme_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekPhotoTopBH <- reactive({
    
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),]
    
    ifelse(input$bh_meme_select_sort_by_top == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = TRUE),])
    
    if(input$bh_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((bh_number_meme_top$n-9), 1):min(nrow(DateRangeMemes), bh_number_meme_top$n),]
    DateRangeMemes
    
  })
  
  WeekPhotoBottomBH <- reactive({
    
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    
    ifelse(input$bh_meme_select_sort_by_bottom == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = FALSE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = FALSE),])
    
    if(input$bh_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((bh_number_meme_bottom$n-9), 1):min(nrow(DateRangeMemes), bh_number_meme_bottom$n),]
    DateRangeMemes
    
  })

  # 1.3. Reactive Data - Fierce ---------------------------------------------------------------------------------------------------------------
  
  fc_number_article_top <- reactiveValues(n = 0)
  fc_number_article_bottom <- reactiveValues(n = 0)
  fc_number_video_top <- reactiveValues(n = 0)
  fc_number_video_bottom <- reactiveValues(n = 0)
  fc_number_video_meme_top <- reactiveValues(n = 0)
  fc_number_video_meme_bottom <- reactiveValues(n = 0)
  fc_number_meme_top <- reactiveValues(n = 0)
  fc_number_meme_bottom <- reactiveValues(n = 0)
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_article_select_original_repost_top
    input$fc_article_select_categories_top
    input$fc_article_select_sort_by_top
  }, {
    fc_number_article_top$n <- 0
  })
  
  observeEvent({
    input$fc_article_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_article_select_original_repost_top
    input$fc_article_select_categories_top
    input$fc_article_select_sort_by_top
  }, {
    
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    if(input$fc_article_select_original_repost_top == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$fc_article_select_original_repost_top == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_top),]
    
    fc_number_article_top$n <- min(fc_number_article_top$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$fc_article_prev_ten_top, {
    fc_number_article_top$n <- max(fc_number_article_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_article_select_original_repost_bottom
    input$fc_article_select_categories_bottom
    input$fc_article_select_sort_by_bottom
  }, {
    fc_number_article_bottom$n <- 0
  })
  
  observeEvent({
    input$fc_article_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_article_select_original_repost_bottom
    input$fc_article_select_categories_bottom
    input$fc_article_select_sort_by_bottom
  }, {
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    if(input$fc_article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$fc_article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_bottom),]
    
    fc_number_article_bottom$n <- min(fc_number_article_bottom$n + 10, nrow(DateRangeArticles))
  })
  
  observeEvent(input$fc_article_prev_ten_bottom, {
    fc_number_article_bottom$n <- max(fc_number_article_bottom$n - 10,10)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_select_original_repost_top
    input$fc_video_select_categories_top
    input$fc_video_select_sort_by_top
  }, {
    fc_number_video_top$n <- 0
  })
  
  observeEvent({
    input$fc_video_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_select_original_repost_top
    input$fc_video_select_categories_top
    input$fc_video_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    if(input$fc_video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_top),]
    
    fc_number_video_top$n <- min(fc_number_video_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$fc_video_prev_ten_top, {
    fc_number_video_top$n <- max(fc_number_video_top$n - 5,5)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_select_original_repost_bottom
    input$fc_video_select_categories_bottom
    input$fc_video_select_sort_by_bottom
  }, {
    fc_number_video_bottom$n <- 0
  })
  
  observeEvent({
    input$fc_video_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_select_original_repost_bottom
    input$fc_video_select_categories_bottom
    input$fc_video_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    if(input$fc_video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_bottom),]
    
    fc_number_video_bottom$n <- min(fc_number_video_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$fc_video_prev_ten_bottom, {
    fc_number_video_bottom$n <- max(fc_number_video_bottom$n - 5,5)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_meme_select_original_repost_top
    input$fc_video_meme_select_categories_top
    input$fc_video_meme_select_sort_by_top
  }, {
    fc_number_video_meme_top$n <- 0
  })
  
  observeEvent({
    input$fc_video_meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_meme_select_original_repost_top
    input$fc_video_meme_select_categories_top
    input$fc_video_meme_select_sort_by_top
  }, {
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    if(input$fc_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_top),]
    
    fc_number_video_meme_top$n <- min(fc_number_video_meme_top$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$fc_video_meme_prev_ten_top, {
    fc_number_video_meme_top$n <- max(fc_number_video_meme_top$n - 5,5)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_meme_select_original_repost_bottom
    input$fc_video_meme_select_categories_bottom
    input$fc_video_meme_select_sort_by_bottom
  }, {
    fc_number_video_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$fc_video_meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_video_meme_select_original_repost_bottom
    input$fc_video_meme_select_categories_bottom
    input$fc_video_meme_select_sort_by_bottom
  }, {
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    if(input$fc_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_bottom),]
    
    fc_number_video_meme_bottom$n <- min(fc_number_video_meme_bottom$n + 5, nrow(DateRangeVideos))
  })
  
  observeEvent(input$fc_video_meme_prev_ten_bottom, {
    fc_number_video_meme_bottom$n <- max(fc_number_video_meme_bottom$n - 5,5)
  })
  
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_meme_select_original_repost_top
    input$fc_meme_select_sort_by_top
  }, {
    fc_number_meme_top$n <- 0
  })
  
  observeEvent({
    input$fc_meme_next_ten_top
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_meme_select_original_repost_top
    input$fc_meme_select_sort_by_top
  }, {
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    
    if(input$fc_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    fc_number_meme_top$n <- min(fc_number_meme_top$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$fc_meme_prev_ten_top, {
    fc_number_meme_top$n <- max(fc_number_meme_top$n - 10,10)
  })
  
  observeEvent({
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_meme_select_original_repost_bottom
    input$fc_meme_select_sort_by_bottom
  }, {
    fc_number_meme_bottom$n <- 0
  })
  
  observeEvent({
    input$fc_meme_next_ten_bottom
    input$dateRange1[1]
    input$dateRange1[2]
    input$fc_meme_select_original_repost_bottom
    input$fc_meme_select_sort_by_bottom
  }, {
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    
    if(input$fc_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    fc_number_meme_bottom$n <- min(fc_number_meme_bottom$n + 10, nrow(DateRangeMemes))
  })
  
  observeEvent(input$fc_meme_prev_ten_bottom, {
    fc_number_meme_bottom$n <- max(fc_number_meme_bottom$n - 10,10)
  })
  
  
  WeekLinksTopFC <- reactive({
    
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    ifelse(input$fc_article_select_sort_by_top == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = TRUE),], ifelse(input$fc_article_select_sort_by_top == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = TRUE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = TRUE),]))
    
    if(input$fc_article_select_original_repost_top == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$fc_article_select_original_repost_top == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_top),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((fc_number_article_top$n-9), 1):min(nrow(DateRangeArticles), fc_number_article_top$n),]
    DateRangeArticles
    
  })
  
  WeekLinksBottomFC <- reactive({
    
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    ifelse(input$fc_article_select_sort_by_bottom == "Link Clicks", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$link_clicks, decreasing = FALSE),], ifelse(input$fc_article_select_sort_by_bottom == "Reach", DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$post_reach, decreasing = FALSE),], DateRangeArticles <- DateRangeArticles[order(DateRangeArticles$total_interactions, decreasing = FALSE),]))
    
    if(input$fc_article_select_original_repost_bottom == "Originals"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
      
    }
    
    else if (input$fc_article_select_original_repost_bottom == "Reposts"){
      
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_bottom),]
    
    DateRangeArticles$rank_link_clicks <- rank(-DateRangeArticles$link_clicks, ties.method="max")
    DateRangeArticles$rank_reach <- rank(-DateRangeArticles$post_reach, ties.method="max")
    DateRangeArticles$rank_interactions <- rank(-DateRangeArticles$total_interactions, ties.method="max")
    
    DateRangeArticles <- DateRangeArticles[max((fc_number_article_bottom$n-9), 1):min(nrow(DateRangeArticles), fc_number_article_bottom$n),]
    DateRangeArticles
    
  })
  
  WeekVideosTopFC <- reactive({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    ifelse(input$fc_video_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$fc_video_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$fc_video_select_original_repost_top == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$fc_video_select_original_repost_top == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((fc_number_video_top$n-4), 1):min(nrow(DateRangeVideos), fc_number_video_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideosBottomFC <- reactive({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    ifelse(input$fc_video_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$fc_video_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$fc_video_select_original_repost_bottom == "Originals"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
      
    }
    
    else if (input$fc_video_select_original_repost_bottom == "Reposts"){
      
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((fc_number_video_bottom$n-4), 1):min(nrow(DateRangeVideos), fc_number_video_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesTopFC <- reactive({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    ifelse(input$fc_video_meme_select_sort_by_top == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = TRUE),], ifelse(input$fc_video_meme_select_sort_by_top == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = TRUE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = TRUE),]))
    
    if(input$fc_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_top),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((fc_number_video_meme_top$n-4), 1):min(nrow(DateRangeVideos), fc_number_video_meme_top$n),]
    DateRangeVideos
    
  })
  
  WeekVideoMemesBottomFC <- reactive({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    ifelse(input$fc_video_meme_select_sort_by_bottom == "Video Views", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_video_views, decreasing = FALSE),], ifelse(input$fc_video_meme_select_sort_by_bottom == "Reach", DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$post_reach, decreasing = FALSE),], DateRangeVideos <- DateRangeVideos[order(DateRangeVideos$total_interactions, decreasing = FALSE),]))
    
    if(input$fc_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_bottom),]
    
    DateRangeVideos$rank_video_views <- rank(-DateRangeVideos$post_video_views, ties.method="max")
    DateRangeVideos$rank_reach <- rank(-DateRangeVideos$post_reach, ties.method="max")
    DateRangeVideos$rank_interactions <- rank(-DateRangeVideos$total_interactions, ties.method="max")
    
    DateRangeVideos <- DateRangeVideos[max((fc_number_video_meme_bottom$n-4), 1):min(nrow(DateRangeVideos), fc_number_video_meme_bottom$n),]
    DateRangeVideos
    
  })
  
  WeekPhotoTopFC <- reactive({
    
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),]
    
    ifelse(input$fc_meme_select_sort_by_top == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = TRUE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = TRUE),])
    
    if(input$fc_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((fc_number_meme_top$n-9), 1):min(nrow(DateRangeMemes), fc_number_meme_top$n),]
    DateRangeMemes
    
  })
  
  WeekPhotoBottomFC <- reactive({
    
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    
    ifelse(input$fc_meme_select_sort_by_bottom == "Reach", DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$post_reach, decreasing = FALSE),], DateRangeMemes <- DateRangeMemes[order(DateRangeMemes$total_interactions, decreasing = FALSE),])
    
    if(input$fc_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    DateRangeMemes$rank_reach <- rank(-DateRangeMemes$post_reach, ties.method="max")
    DateRangeMemes$rank_interactions <- rank(-DateRangeMemes$total_interactions, ties.method="max")
    
    DateRangeMemes <- DateRangeMemes[max((fc_number_meme_bottom$n-9), 1):min(nrow(DateRangeMemes), fc_number_meme_bottom$n),]
    DateRangeMemes
    
  })
  
  
  #----------------------------------------------------------------------------------------------------------------------

  
  # 2. Overview --------------------------------------------------------------------------------------------------------------------
  
  # 2.1. Overview - We Are Mitú -------------------------------------------------------------------------------------------------------------------
  
  output$PlotOverview11 <- renderHighchart({
    
    input$plot_article_overview
    isolate({   
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_select_categories_performance_overview & DataArticles$article_click_rank %in% input$chart_link_clicks_group_overview & DataArticles$repost %in% input$article_overview_repost),]
      
      AvgDailyArticleReach <- as.xts(DataArticles$post_reach, order.by = DataArticles$created_time)
      AvgDailyArticleLC <- as.xts(DataArticles$link_clicks, order.by = DataArticles$created_time)
      
      if(input$variable_display_overview == "link clicks"){
        AvgDailyArticleVariable <- as.xts(DataArticles$link_clicks, order.by = DataArticles$created_time)
      } else {
        AvgDailyArticleVariable <- as.xts(DataArticles$total_comments+DataArticles$total_likes+DataArticles$total_shares, order.by = DataArticles$created_time)
      }
      
      if(input$rate_display_overview == "ctr"){
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticles$ctr*100, format = "f", digits = 2)), order.by = DataArticles$created_time)
      } else {
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticles$interaction_rate*100, format = "f", digits = 2)), order.by = DataArticles$created_time)
      }
      
      sufix <- ifelse(input$avg_total_article_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$variable_display_overview == "link clicks", "Link Clicks", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$avg_total_article_overview == "average", ifelse(input$rate_display_overview == "ctr", "Avg. CTR", "Avg. IR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))) %>% 
        
        hc_add_series(AvgDailyArticleReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyArticleVariable, id = "Daily LC", yAxis = 1, name = ifelse(input$variable_display_overview == "link clicks", "Link Clicks", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyArticleRate, id = "Daily CTR", yAxis = 2, name = ifelse(input$rate_display_overview == "ctr", "CTR", "IR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>% 
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_article_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$avg_total_article_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$chart_time_article_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$avg_total_article_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$PlotOverview111 <- renderHighchart({
    
    input$plot_article_overview1
    isolate({ 
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_select_categories_overview & DataArticles$article_click_rank %in% input$chart_link_clicks_group_overview2),]
      
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$repost == 1 & DataArticles$post_source_type == "native"),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$repost == 1  & DataArticles$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1  & DataArticles$post_source_type == "native"),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$original == 1  & DataArticles$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      shares <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$post_source_type == "shared"),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$post_source_type == "shared"),]))), by = "created_time", all = TRUE)
      shares <- as.xts(shares[,"num"], order.by = shares[,"created_time"])
      
      
      colores<- c('#D55200', '#2580B9', '#C80D4A')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_overview1) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_overview1) %>%
        hc_add_series(shares, name = "Shares", type = input$chart_type_overview1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_overview1 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_overview1 == "area" || input$chart_type_overview1 == "column"){if(input$chart_stack_overview1 != "none"){input$chart_stack_overview1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_overview1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })

  output$PlotOverview12 <- renderHighchart({
    
    input$plot_video_overview
    isolate({  
      
      VideoData <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$video_views_rank %in% input$chart_video_views_group_overview & DataVideos$category %in% input$video_select_categories_performance_overview & DataVideos$repost %in% input$video_overview_repost),]
      
      AvgDailyVideoReach <- as.xts(VideoData$post_reach, order.by = VideoData$created_time)
      
      if(input$video_variable_display_overview == "video views"){
        AvgDailyVideoVariable <- as.xts(VideoData$post_video_views, order.by = VideoData$created_time)
      } else {
        AvgDailyVideoVariable <- as.xts(VideoData$total_comments+VideoData$total_likes+VideoData$total_shares , order.by = VideoData$created_time)
      }
      
      if(input$video_rate_display_overview == "interaction rate"){
        AvgDailyVideoRate <- as.xts(as.numeric(formatC(VideoData$interaction_rate*100, format = "f", digits = 2)), order.by = VideoData$created_time)
      } else {
        AvgDailyVideoRate <- as.xts(as.numeric(formatC((VideoData$post_video_views/VideoData$post_reach)*100, format = "f", digits = 2)), order.by = VideoData$created_time)
      }
  
      sufix <- ifelse(input$avg_total_video_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$video_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$avg_total_video_overview == "average", ifelse(input$video_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        
        hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$video_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$video_rate_display_overview == "interaction rate", "IR", "VR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_video_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$avg_total_video_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$chart_time_video_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$avg_total_video_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    
    })
  })
  
  output$PlotOverview121 <- renderHighchart({
    
    input$plot_video_overview1
    isolate({ 
      
      VideoData <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$video_views_rank %in% input$chart_video_views_group_overview2 & DataVideos$category %in% input$video_select_categories_overview),]
      
      dates <- data.frame(created_time = VideoData$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = VideoData[which(VideoData$repost == 1  & VideoData$post_source_type == "native"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$repost == 1 & VideoData$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = VideoData[which(VideoData$original == 1 & VideoData$post_source_type == "native"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$original == 1 & VideoData$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      shares <- merge(dates, data.frame(created_time = VideoData[which(VideoData$post_source_type == "shared"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$post_source_type == "shared"),]))), by = "created_time", all = TRUE)
      shares <- as.xts(shares[,"num"], order.by = shares[,"created_time"])
      
      colores<- c('#D55200', '#2580B9', '#C80D4A')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_overview2) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_overview2) %>%
        hc_add_series(shares, name = "Shares", type = input$chart_type_overview2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_overview2 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_overview2 == "area" || input$chart_type_overview2 == "column"){if(input$chart_stack_overview2 != "none"){input$chart_stack_overview2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_overview2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotOverview13 <- renderHighchart({

    input$plot_videomeme_overview
    isolate({  
      
      DataVideos <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$video_views_rank %in% input$chart_video_views_meme_group_overview & DataVideos$category %in% input$video_meme_select_categories_performance_overview & DataVideos$repost %in% input$video_meme_overview_repost),]
      
    AvgDailyVideoReach <- as.xts(DataVideos[which(DataVideos$video_meme == 1),]$post_reach, order.by = DataVideos[which(DataVideos$video_meme == 1),]$created_time)
   
    if(input$videomeme_variable_display_overview == "video views"){
      AvgDailyVideoVariable <- as.xts(DataVideos[which(DataVideos$video_meme == 1),]$post_video_views, order.by = DataVideos[which(DataVideos$video_meme == 1),]$created_time)
    } else {
      AvgDailyVideoVariable <- as.xts(DataVideos[which(DataVideos$video_meme == 1),]$total_comments+DataVideos[which(DataVideos$video_meme == 1),]$total_likes+DataVideos[which(DataVideos$video_meme == 1),]$total_shares , order.by = DataVideos[which(DataVideos$video_meme == 1),]$created_time)
    }
    
    if(input$videomeme_rate_display_overview == "interaction rate"){
      AvgDailyVideoRate <- as.xts(as.numeric(formatC(DataVideos[which(DataVideos$video_meme == 1),]$interaction_rate*100, format = "f", digits = 2)), order.by = DataVideos[which(DataVideos$video_meme == 1),]$created_time)
    } else {
      AvgDailyVideoRate <- as.xts(as.numeric(formatC((DataVideos[which(DataVideos$video_meme == 1),]$post_video_views/DataVideos[which(DataVideos$video_meme == 1),]$post_reach)*100, format = "f", digits = 2)), order.by = DataVideos[which(DataVideos$video_meme == 1),]$created_time)
    }
    
    sufix <- ifelse(input$avg_total_videomeme_overview == "average", "Avg.", "Total")
    
    hc <-highchart(type = "stock") %>%
      hc_yAxis_multiples(
        list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
        list(offset = 30, title = list(text = paste(sufix, ifelse(input$videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
        list(offset = 30, title = list(text = ifelse(input$avg_total_videomeme_overview == "average", ifelse(input$videomeme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
      ) %>%
      hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
      hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
      hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$videomeme_rate_display_overview == "interaction rate", "Interaction Rate", "Views Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
      
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input$chart_time_videomeme_overview == "month", 5, 4)) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$avg_total_videomeme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$chart_time_videomeme_overview, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input$avg_total_videomeme_overview == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
    })
  })
  
  output$PlotOverview131 <- renderHighchart({
    
    input$plot_videomeme_overview1
    isolate({ 
      
      VideoData <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$video_views_rank %in% input$chart_video_views_meme_group_overview2 & DataVideos$category %in% input$video_meme_select_categories_overview),]
      
      dates <- data.frame(created_time = VideoData$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = VideoData[which(VideoData$repost == 1 & VideoData$post_source_type == "native"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$repost == 1 & VideoData$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = VideoData[which(VideoData$original == 1 & VideoData$post_source_type == "native"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$original == 1 & VideoData$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      shares <- merge(dates, data.frame(created_time = VideoData[which(VideoData$post_source_type == "shared"),"created_time"], num = rep(1,nrow(VideoData[which(VideoData$post_source_type == "shared"),]))), by = "created_time", all = TRUE)
      shares <- as.xts(shares[,"num"], order.by = shares[,"created_time"])
       
      colores<- c('#D55200', '#2580B9', '#C80D4A')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_overview3) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_overview3) %>%
        hc_add_series(shares, name = "Shares", type = input$chart_type_overview3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_overview3 == "area" || input$chart_type_overview3 == "column"){if(input$chart_stack_overview2 != "none"){input$chart_stack_overview3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_overview3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotOverview14 <- renderHighchart({
    
    input$plot_meme_overview
    isolate({  
    
    AvgDailyPhotoReach <- as.xts(DataPhotos$post_reach, order.by = DataPhotos$created_time)
    AvgDailyPhotoFanReach <- as.xts(DataPhotos$post_reach_fan_unique, order.by = DataPhotos$created_time)
    AvgDailyPhotoIR <- as.xts(as.numeric(formatC(DataPhotos$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotos$created_time)
    
    if(input$meme_variable_display_overview == "fan reach"){
      AvgDailyPhotoVariable <- as.xts(DataPhotos$post_reach_fan_unique, order.by = DataPhotos$created_time)
    } else {
      AvgDailyPhotoVariable <- as.xts(DataPhotos$post_reach_viral_unique, order.by = DataPhotos$created_time)
    }
    
    if(input$meme_rate_display_overview == "interaction rate"){
      AvgDailyPhotoRate <- as.xts(as.numeric(formatC(DataPhotos$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotos$created_time)
    } else {
      AvgDailyPhotoRate <- as.xts(as.numeric(formatC((DataPhotos$total_shares/(DataPhotos$total_comments+DataPhotos$total_likes+DataPhotos$total_shares))*100, format = "f", digits = 2)), order.by = DataPhotos$created_time)
    }
    
    sufix <- ifelse(input$avg_total_meme_overview == "average", "Avg.", "Total")
    
    hc <-highchart(type = "stock") %>%
      hc_yAxis_multiples(
        list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
        list(offset = 30, title = list(text = paste(sufix, ifelse(input$meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), sep = " ")), height = "35%", top = "47.5%"),
        list(offset = 30, title = list(text = ifelse(input$avg_total_videomeme_overview == "average", ifelse(input$meme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. Share Rate"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
      ) %>%

      hc_add_series(AvgDailyPhotoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
      hc_add_series(AvgDailyPhotoVariable, id = "Daily Fan Reach", yAxis = 1, name = ifelse(input$meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), color = "#F46D43", type = "area") %>%
      hc_add_series(AvgDailyPhotoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$meme_rate_display_overview == "interaction rate", "Interaction Rate", "Share Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input$chart_time_meme_overview == "month", 5, 4)) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$avg_total_meme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$chart_time_meme_overview, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input$avg_total_meme_overview == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
    })
  })
  
  output$PlotOverview141 <- renderHighchart({
    
    input$plot_meme_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataPhotos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$repost == 1 & DataPhotos$post_source_type == "native"),"created_time"], num = rep(1,nrow(DataPhotos[which(DataPhotos$repost == 1 & DataPhotos$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1 & DataPhotos$post_source_type == "native"),"created_time"], num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 1 & DataPhotos$post_source_type == "native"),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      shares <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$post_source_type == "shared"),"created_time"], num = rep(1,nrow(DataPhotos[which(DataPhotos$post_source_type == "shared"),]))), by = "created_time", all = TRUE)
      shares <- as.xts(shares[,"num"], order.by = shares[,"created_time"])
      
      colores<- c('#D55200', '#2580B9', '#C80D4A')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_overview4) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_overview4) %>%
        hc_add_series(shares, name = "Shares", type = input$chart_type_overview4) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_overview4 == "area" || input$chart_type_overview4 == "column"){if(input$chart_stack_overview2 != "none"){input$chart_stack_overview4}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_overview4, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  
  Plot_Page_Reach_Function <- function (viral_page_reach, organic_page_reach, page_reach, page_reach_us, input_chart_time, input_avg_total){
    
    hc <-highchart(type = "stock") %>%
      # hc_colors(colores) %>%
      hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
      # hc_add_series(viral_page_reach, name = "Viral Reach", type = "column") %>%
      # hc_add_series(organic_page_reach, name = "Organic Reach", type = "column") %>%
      hc_add_series(page_reach, name = "Reach", type = "column", stacking = NULL) %>%
      # hc_add_series(page_reach_us, name = "US Reach", type = "line", stacking = NULL) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input_avg_total, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input_avg_total == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Page_Reach_US_Function <- function (viral_page_reach, organic_page_reach, page_reach, page_reach_us, input_chart_time, input_avg_total){
    
    hc <-highchart(type = "stock") %>%
      # hc_colors(colores) %>%
      hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
      # hc_add_series(viral_page_reach, name = "Viral Reach", type = "column") %>%
      # hc_add_series(organic_page_reach, name = "Organic Reach", type = "column") %>%
      # hc_add_series(page_reach, name = "Reach", type = "column", stacking = NULL) %>%
      hc_add_series(page_reach_us, name = "US Reach", type = "column", stacking = NULL) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input_avg_total, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input_avg_total == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Page_Reach_Gender_Function <- function (page_reach_fem, page_reach_male, input_chart_time, input_avg_total){
    
    hc <-highchart(type = "stock") %>%
      # hc_colors(colores) %>%
      hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
      hc_add_series(page_reach_fem, name = "Female", type = "column") %>%
      hc_add_series(page_reach_male, name = "Male", type = "column") %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input_avg_total, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input_avg_total == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Page_Reach_Age_Function <- function (page_reach_13_17, page_reach_18_24, page_reach_25_34, page_reach_35_44, page_reach_45_54, page_reach_55_64, page_reach_65, input_chart_time, input_avg_total){
    
    hc <-highchart(type = "stock") %>%
      # hc_colors(colores) %>%
      hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
      hc_add_series(page_reach_13_17, name = "13 - 17", type = "column") %>%
      hc_add_series(page_reach_18_24, name = "18 - 24", type = "column") %>%
      hc_add_series(page_reach_25_34, name = "25 - 34", type = "column") %>%
      hc_add_series(page_reach_35_44, name = "35 - 44", type = "column") %>%
      hc_add_series(page_reach_45_54, name = "45 - 54", type = "column") %>%
      hc_add_series(page_reach_55_64, name = "55 - 64", type = "column") %>%
      hc_add_series(page_reach_65, name = "65+", type = "column") %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input_avg_total, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(valueDecimals = ifelse(input_avg_total == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  output$PlotOverviewPageReach <- renderHighchart({
    
    input$plot_overview_page_reach
    isolate({ 
      
      input_chart_time <- input$chart_time_overview_page_reach
      input_avg_total <- input$avg_total_overview_page_reach
      
      dates <- data.frame(created_time = WamPageData$date)

      page_reach <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach), by = "created_time", all = TRUE)
      page_reach_us <- merge(dates, data.frame(created_time = WamPageData$date, reach_us = WamPageData$page_reach_US), by = "created_time", all = TRUE)
      page_reach_non_us <- merge(dates, data.frame(created_time = WamPageData$date, reach_non_us = WamPageData$page_reach - WamPageData$page_reach_US), by = "created_time", all = TRUE)
      page_reach_viral <- merge(dates, data.frame(created_time = WamPageData$date, viral_reach = WamPageData$page_reach_viral), by = "created_time", all = TRUE)
      page_reach_organic <- merge(dates, data.frame(created_time = WamPageData$date, organic_reach = WamPageData$page_reach_organic), by = "created_time", all = TRUE)
      
      page_reach <- as.xts(page_reach[,"reach"], order.by = page_reach[,"created_time"])
      page_reach_us <- as.xts(page_reach_us[,"reach_us"], order.by = page_reach_us[,"created_time"])
      viral_page_reach <- as.xts(page_reach_viral[,"viral_reach"], order.by = page_reach_viral[,"created_time"])
      organic_page_reach <- as.xts(page_reach_organic[,"organic_reach"], order.by = page_reach_organic[,"created_time"])
      
      
      page_reach_fem <- merge(dates, data.frame(created_time = WamPageData$date, reach_fem = WamPageData$page_reach_F_13_17 + WamPageData$page_reach_F_18_24 + WamPageData$page_reach_F_25_34 + WamPageData$page_reach_F_35_44 + WamPageData$page_reach_F_45_54 + WamPageData$page_reach_F_55_64 + WamPageData$page_reach_F_65), by = "created_time", all = TRUE)
      page_reach_male <- merge(dates, data.frame(created_time = WamPageData$date, reach_male = WamPageData$page_reach_M_13_17 + WamPageData$page_reach_M_18_24 + WamPageData$page_reach_M_25_34 + WamPageData$page_reach_M_35_44 + WamPageData$page_reach_M_45_54 + WamPageData$page_reach_M_55_64 + WamPageData$page_reach_M_65), by = "created_time", all = TRUE)
      
      page_reach_fem <- as.xts(page_reach_fem[,"reach_fem"], order.by = page_reach_fem[,"created_time"])
      page_reach_male <- as.xts(page_reach_male[,"reach_male"], order.by = page_reach_male[,"created_time"])
      
      
      page_reach_13_17 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_13_17 + WamPageData$page_reach_M_13_17), by = "created_time", all = TRUE)
      page_reach_18_24 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_18_24 + WamPageData$page_reach_M_18_24), by = "created_time", all = TRUE)
      page_reach_25_34 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_25_34 + WamPageData$page_reach_M_25_34), by = "created_time", all = TRUE)
      page_reach_35_44 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_35_44 + WamPageData$page_reach_M_35_44), by = "created_time", all = TRUE)
      page_reach_45_54 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_45_54 + WamPageData$page_reach_M_45_54), by = "created_time", all = TRUE)
      page_reach_55_64 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_55_64 + WamPageData$page_reach_M_55_64), by = "created_time", all = TRUE)
      page_reach_65 <- merge(dates, data.frame(created_time = WamPageData$date, reach = WamPageData$page_reach_F_65 + WamPageData$page_reach_M_65), by = "created_time", all = TRUE)
      
      page_reach_13_17 <- as.xts(page_reach_13_17[,"reach"], order.by = page_reach_13_17[,"created_time"])
      page_reach_18_24 <- as.xts(page_reach_18_24[,"reach"], order.by = page_reach_18_24[,"created_time"])
      page_reach_25_34 <- as.xts(page_reach_25_34[,"reach"], order.by = page_reach_25_34[,"created_time"])
      page_reach_35_44 <- as.xts(page_reach_35_44[,"reach"], order.by = page_reach_35_44[,"created_time"])
      page_reach_45_54 <- as.xts(page_reach_45_54[,"reach"], order.by = page_reach_45_54[,"created_time"])
      page_reach_55_64 <- as.xts(page_reach_55_64[,"reach"], order.by = page_reach_55_64[,"created_time"])
      page_reach_65 <- as.xts(page_reach_65[,"reach"], order.by = page_reach_65[,"created_time"])
      
      
      if(input$reach_chart_selectize == "Reach"){
        
        Plot_Page_Reach_Function(viral_page_reach, organic_page_reach, page_reach, page_reach_us, input_chart_time, input_avg_total)
      }
      else if(input$reach_chart_selectize == "Reach by Gender") {
        Plot_Page_Reach_Gender_Function(page_reach_fem, page_reach_male, input_chart_time, input_avg_total)
        
      }
      else {
        Plot_Page_Reach_Age_Function(page_reach_13_17, page_reach_18_24, page_reach_25_34, page_reach_35_44, page_reach_45_54, page_reach_55_64, page_reach_65, input_chart_time, input_avg_total)
        
      }
      
     
    })
  })
  
  
  # 2.2. Overview - Bad Hombres -------------------------------------------------------------------------------------------------------------------
  
  output$BHPlotOverview11 <- renderHighchart({
    
    input$bh_plot_article_overview
    isolate({   
      
      AvgDailyArticleReach <- as.xts(DataArticlesBH$post_reach, order.by = DataArticlesBH$created_time)
      AvgDailyArticleLC <- as.xts(DataArticlesBH$link_clicks, order.by = DataArticlesBH$created_time)
      
      if(input$bh_variable_display_overview == "link clicks"){
        AvgDailyArticleVariable <- as.xts(DataArticlesBH$link_clicks, order.by = DataArticlesBH$created_time)
      } else {
        AvgDailyArticleVariable <- as.xts(DataArticlesBH$total_comments+DataArticlesBH$total_likes+DataArticles$total_shares, order.by = DataArticlesBH$created_time)
      }
      
      if(input$bh_rate_display_overview == "ctr"){
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticlesBH$ctr*100, format = "f", digits = 2)), order.by = DataArticlesBH$created_time)
      } else {
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticlesBH$interaction_rate*100, format = "f", digits = 2)), order.by = DataArticlesBH$created_time)
      }
      
      sufix <- ifelse(input$bh_avg_total_article_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$bh_variable_display_overview == "link clicks", "Link Clicks", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$bh_avg_total_article_overview == "average", ifelse(input$bh_rate_display_overview == "ctr", "Avg. CTR", "Avg. IR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))) %>% 
        
        hc_add_series(AvgDailyArticleReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyArticleVariable, id = "Daily LC", yAxis = 1, name = ifelse(input$bh_variable_display_overview == "link clicks", "Link Clicks", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyArticleRate, id = "Daily CTR", yAxis = 2, name = ifelse(input$bh_rate_display_overview == "ctr", "CTR", "IR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>% 
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_article_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$bh_avg_total_article_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_article_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_article_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$BHPlotOverview111 <- renderHighchart({
    
    input$bh_plot_article_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataArticlesBH$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$repost == 1),"created_time"], num = rep(1,nrow(DataArticlesBH[which(DataArticlesBH$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 1),"created_time"], num = rep(1,nrow(DataArticlesBH[which(DataArticlesBH$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$bh_chart_type_overview1) %>%
        hc_add_series(reposts, name = "Reposts", type = input$bh_chart_type_overview1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview1 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_overview1 == "area" || input$bh_chart_type_overview1 == "column"){if(input$bh_chart_stack_overview1 != "none"){input$bh_chart_stack_overview1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_overview1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotOverview12 <- renderHighchart({
    
    input$bh_plot_video_overview
    isolate({  
      
      AvgDailyVideoReach <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 0),]$post_reach, order.by = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      
      if(input$bh_video_variable_display_overview == "video views"){
        AvgDailyVideoVariable <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 0),]$post_video_views, order.by = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      } else {
        AvgDailyVideoVariable <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 0),]$total_comments+DataVideosBH[which(DataVideosBH$video_meme == 0),]$total_likes+DataVideosBH[which(DataVideosBH$video_meme == 0),]$total_shares , order.by = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      }
      
      if(input$bh_video_rate_display_overview == "interaction rate"){
        AvgDailyVideoRate <- as.xts(as.numeric(formatC(DataVideosBH[which(DataVideosBH$video_meme == 0),]$interaction_rate*100, format = "f", digits = 2)), order.by = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      } else {
        AvgDailyVideoRate <- as.xts(as.numeric(formatC((DataVideosBH[which(DataVideosBH$video_meme == 0),]$post_video_views/DataVideosBH[which(DataVideosBH$video_meme == 0),]$post_reach)*100, format = "f", digits = 2)), order.by = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      }
      
      sufix <- ifelse(input$bh_avg_total_video_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$bh_video_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$bh_avg_total_video_overview == "average", ifelse(input$bh_video_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        
        hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$bh_video_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$bh_video_rate_display_overview == "interaction rate", "IR", "VR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_video_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$bh_avg_total_video_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_video_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_video_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$BHPlotOverview121 <- renderHighchart({
    
    input$bh_plot_video_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0),]$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$repost == 1),"created_time"], num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),"created_time"], num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$bh_chart_type_overview2) %>%
        hc_add_series(reposts, name = "Reposts", type = input$bh_chart_type_overview2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview2 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_overview2 == "area" || input$bh_chart_type_overview2 == "column"){if(input$bh_chart_stack_overview2 != "none"){input$bh_chart_stack_overview2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_overview2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotOverview13 <- renderHighchart({
    
    input$bh_plot_videomeme_overview
    isolate({  
      
      AvgDailyVideoReach <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 1),]$post_reach, order.by = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      
      if(input$bh_videomeme_variable_display_overview == "video views"){
        AvgDailyVideoVariable <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 1),]$post_video_views, order.by = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      } else {
        AvgDailyVideoVariable <- as.xts(DataVideosBH[which(DataVideosBH$video_meme == 1),]$total_comments+DataVideosBH[which(DataVideosBH$video_meme == 1),]$total_likes+DataVideosBH[which(DataVideosBH$video_meme == 1),]$total_shares , order.by = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      }
      
      if(input$bh_videomeme_rate_display_overview == "interaction rate"){
        AvgDailyVideoRate <- as.xts(as.numeric(formatC(DataVideosBH[which(DataVideosBH$video_meme == 1),]$interaction_rate*100, format = "f", digits = 2)), order.by = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      } else {
        AvgDailyVideoRate <- as.xts(as.numeric(formatC((DataVideosBH[which(DataVideosBH$video_meme == 1),]$post_video_views/DataVideosBH[which(DataVideosBH$video_meme == 1),]$post_reach)*100, format = "f", digits = 2)), order.by = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      }
      
      sufix <- ifelse(input$bh_avg_total_videomeme_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$bh_videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$bh_avg_total_videomeme_overview == "average", ifelse(input$bh_videomeme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$bh_videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$bh_videomeme_rate_display_overview == "interaction rate", "Interaction Rate", "Views Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_videomeme_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$bh_avg_total_videomeme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_videomeme_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_videomeme_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$BHPlotOverview131 <- renderHighchart({
    
    input$bh_plot_videomeme_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1),]$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$repost == 1),"created_time"], num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),"created_time"], num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$bh_chart_type_overview3) %>%
        hc_add_series(reposts, name = "Reposts", type = input$bh_chart_type_overview3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_overview3 == "area" || input$bh_chart_type_overview3 == "column"){if(input$bh_chart_stack_overview2 != "none"){input$bh_chart_stack_overview3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_overview3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotOverview14 <- renderHighchart({
    
    input$bh_plot_meme_overview
    isolate({  
      
      AvgDailyPhotoReach <- as.xts(DataPhotosBH$post_reach, order.by = DataPhotosBH$created_time)
      AvgDailyPhotoFanReach <- as.xts(DataPhotosBH$post_reach_fan_unique, order.by = DataPhotosBH$created_time)
      AvgDailyPhotoIR <- as.xts(as.numeric(formatC(DataPhotosBH$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotosBH$created_time)
      
      if(input$bh_meme_variable_display_overview == "fan reach"){
        AvgDailyPhotoVariable <- as.xts(DataPhotosBH$post_reach_fan_unique, order.by = DataPhotosBH$created_time)
      } else {
        AvgDailyPhotoVariable <- as.xts(DataPhotosBH$post_reach_viral_unique, order.by = DataPhotosBH$created_time)
      }
      
      if(input$bh_meme_rate_display_overview == "interaction rate"){
        AvgDailyPhotoRate <- as.xts(as.numeric(formatC(DataPhotosBH$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotosBH$created_time)
      } else {
        AvgDailyPhotoRate <- as.xts(as.numeric(formatC((DataPhotosBH$total_shares/(DataPhotosBH$total_comments+DataPhotosBH$total_likes+DataPhotosBH$total_shares))*100, format = "f", digits = 2)), order.by = DataPhotosBH$created_time)
      }
      
      sufix <- ifelse(input$bh_avg_total_meme_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$bh_meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$bh_avg_total_videomeme_overview == "average", ifelse(input$bh_meme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. Share Rate"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        
        hc_add_series(AvgDailyPhotoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyPhotoVariable, id = "Daily Fan Reach", yAxis = 1, name = ifelse(input$bh_meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyPhotoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$bh_meme_rate_display_overview == "interaction rate", "Interaction Rate", "Share Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_meme_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$bh_avg_total_meme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_meme_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_meme_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$BHPlotOverview141 <- renderHighchart({
    
    input$bh_plot_meme_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataPhotosBH$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$repost == 1),"created_time"], num = rep(1,nrow(DataPhotosBH[which(DataPhotosBH$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 1),"created_time"], num = rep(1,nrow(DataPhotosBH[which(DataPhotosBH$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$bh_chart_type_overview4) %>%
        hc_add_series(reposts, name = "Reposts", type = input$bh_chart_type_overview4) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_overview4 == "area" || input$bh_chart_type_overview4 == "column"){if(input$bh_chart_stack_overview2 != "none"){input$bh_chart_stack_overview4}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_overview4, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  
  output$BHPlotOverviewPageReach <- renderHighchart({
    
    input$bh_plot_overview_page_reach
    isolate({ 
      
      dates <- data.frame(created_time = BHPageData$date)
      
      page_reach <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach), by = "created_time", all = TRUE)
      
      page_reach_us <- merge(dates, data.frame(created_time = BHPageData$date, reach_us = BHPageData$page_reach_US), by = "created_time", all = TRUE)
      
      page_reach_viral <- merge(dates, data.frame(created_time = BHPageData$date, viral_reach = BHPageData$page_reach_viral), by = "created_time", all = TRUE)
      
      page_reach_organic <- merge(dates, data.frame(created_time = BHPageData$date, organic_reach = BHPageData$page_reach_organic), by = "created_time", all = TRUE)
      
      page_reach <- as.xts(page_reach[,"reach"], order.by = page_reach[,"created_time"])
      
      page_reach_us <- as.xts(page_reach_us[,"reach_us"], order.by = page_reach_us[,"created_time"])
      
      viral_page_reach <- as.xts(page_reach_viral[,"viral_reach"], order.by = page_reach_viral[,"created_time"])
      
      organic_page_reach <- as.xts(page_reach_organic[,"organic_reach"], order.by = page_reach_organic[,"created_time"])
      
      # input$bh_chart_type_overview_page_reach
      
      hc <-highchart(type = "stock") %>%
        # hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(viral_page_reach, name = "Viral Reach", type = "column") %>%
        hc_add_series(organic_page_reach, name = "Organic Reach", type = "column") %>%
        hc_add_series(page_reach, name = "Reach", type = "line", stacking = NULL) %>%
        hc_add_series(page_reach_us, name = "US Reach", type = "line", stacking = NULL) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview_page_reach == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input$bh_avg_total_overview_page_reach, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_overview_page_reach, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_overview_page_reach == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotOverviewPageReachDemo <- renderHighchart({
    
    input$bh_plot_overview_page_reach_demo
    isolate({ 
      
      dates <- data.frame(created_time = BHPageData$date)
      
      page_reach_fem <- merge(dates, data.frame(created_time = BHPageData$date, reach_fem = BHPageData$page_reach_F_13_17 + BHPageData$page_reach_F_18_24 + BHPageData$page_reach_F_25_34 + BHPageData$page_reach_F_35_44 + BHPageData$page_reach_F_45_54 + BHPageData$page_reach_F_55_64 + BHPageData$page_reach_F_65), by = "created_time", all = TRUE)
      
      page_reach_male <- merge(dates, data.frame(created_time = BHPageData$date, reach_male = BHPageData$page_reach_M_13_17 + BHPageData$page_reach_M_18_24 + BHPageData$page_reach_M_25_34 + BHPageData$page_reach_M_35_44 + BHPageData$page_reach_M_45_54 + BHPageData$page_reach_M_55_64 + BHPageData$page_reach_M_65), by = "created_time", all = TRUE)
      
      page_reach_fem <- as.xts(page_reach_fem[,"reach_fem"], order.by = page_reach_fem[,"created_time"])
      
      page_reach_male <- as.xts(page_reach_male[,"reach_male"], order.by = page_reach_male[,"created_time"])
      
      hc <-highchart(type = "stock") %>%
        # hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(page_reach_fem, name = "Female", type = "column") %>%
        hc_add_series(page_reach_male, name = "Male", type = "column") %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview_page_reach_demo == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input$bh_avg_total_overview_page_reach_demo, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_overview_page_reach_demo, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_overview_page_reach_demo == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotOverviewPageReachDemoAge <- renderHighchart({
    
    input$bh_plot_overview_page_reach_demo_age
    isolate({ 
      
      dates <- data.frame(created_time = BHPageData$date)
      
      page_reach_13_17 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_13_17 + BHPageData$page_reach_M_13_17), by = "created_time", all = TRUE)
      
      page_reach_18_24 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_18_24 + BHPageData$page_reach_M_18_24), by = "created_time", all = TRUE)
      
      page_reach_25_34 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_25_34 + BHPageData$page_reach_M_25_34), by = "created_time", all = TRUE)
      
      page_reach_35_44 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_35_44 + BHPageData$page_reach_M_35_44), by = "created_time", all = TRUE)
      
      page_reach_45_54 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_45_54 + BHPageData$page_reach_M_45_54), by = "created_time", all = TRUE)
      
      page_reach_55_64 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_55_64 + BHPageData$page_reach_M_55_64), by = "created_time", all = TRUE)
      
      page_reach_65 <- merge(dates, data.frame(created_time = BHPageData$date, reach = BHPageData$page_reach_F_65 + BHPageData$page_reach_M_65), by = "created_time", all = TRUE)
      
      page_reach_13_17 <- as.xts(page_reach_13_17[,"reach"], order.by = page_reach_13_17[,"created_time"])
      page_reach_18_24 <- as.xts(page_reach_18_24[,"reach"], order.by = page_reach_18_24[,"created_time"])
      page_reach_25_34 <- as.xts(page_reach_25_34[,"reach"], order.by = page_reach_25_34[,"created_time"])
      page_reach_35_44 <- as.xts(page_reach_35_44[,"reach"], order.by = page_reach_35_44[,"created_time"])
      page_reach_45_54 <- as.xts(page_reach_45_54[,"reach"], order.by = page_reach_45_54[,"created_time"])
      page_reach_55_64 <- as.xts(page_reach_55_64[,"reach"], order.by = page_reach_55_64[,"created_time"])
      page_reach_65 <- as.xts(page_reach_65[,"reach"], order.by = page_reach_65[,"created_time"])
      
      hc <-highchart(type = "stock") %>%
        # hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(page_reach_13_17, name = "13 - 17", type = "column") %>%
        hc_add_series(page_reach_18_24, name = "18 - 24", type = "column") %>%
        hc_add_series(page_reach_25_34, name = "25 - 34", type = "column") %>%
        hc_add_series(page_reach_35_44, name = "35 - 44", type = "column") %>%
        hc_add_series(page_reach_45_54, name = "45 - 54", type = "column") %>%
        hc_add_series(page_reach_55_64, name = "55 - 64", type = "column") %>%
        hc_add_series(page_reach_65, name = "65+", type = "column") %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_overview_page_reach_demo_age == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black",  stacking = "normal", dataGrouping = list(approximation = input$bh_avg_total_overview_page_reach_demo_age, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$bh_chart_time_overview_page_reach_demo_age, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$bh_avg_total_overview_page_reach_demo_age == "average", 2, 0), pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>') %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  # 2.2. Overview - Fierce -------------------------------------------------------------------------------------------------------------------
  
  output$FCPlotOverview11 <- renderHighchart({
    
    input$fc_plot_article_overview
    isolate({   
      
      AvgDailyArticleReach <- as.xts(DataArticlesFC$post_reach, order.by = DataArticlesFC$created_time)
      AvgDailyArticleLC <- as.xts(DataArticlesFC$link_clicks, order.by = DataArticlesFC$created_time)
      
      if(input$fc_variable_display_overview == "link clicks"){
        AvgDailyArticleVariable <- as.xts(DataArticlesFC$link_clicks, order.by = DataArticlesFC$created_time)
      } else {
        AvgDailyArticleVariable <- as.xts(DataArticlesFC$total_comments+DataArticlesFC$total_likes+DataArticles$total_shares, order.by = DataArticlesFC$created_time)
      }
      
      if(input$fc_rate_display_overview == "ctr"){
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticlesFC$ctr*100, format = "f", digits = 2)), order.by = DataArticlesFC$created_time)
      } else {
        AvgDailyArticleRate <- as.xts(as.numeric(formatC(DataArticlesFC$interaction_rate*100, format = "f", digits = 2)), order.by = DataArticlesFC$created_time)
      }
      
      sufix <- ifelse(input$fc_avg_total_article_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$fc_variable_display_overview == "link clicks", "Link Clicks", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$fc_avg_total_article_overview == "average", ifelse(input$fc_rate_display_overview == "ctr", "Avg. CTR", "Avg. IR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))) %>% 
        
        hc_add_series(AvgDailyArticleReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyArticleVariable, id = "Daily LC", yAxis = 1, name = ifelse(input$fc_variable_display_overview == "link clicks", "Link Clicks", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyArticleRate, id = "Daily CTR", yAxis = 2, name = ifelse(input$fc_rate_display_overview == "ctr", "CTR", "IR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>% 
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_article_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$fc_avg_total_article_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$fc_chart_time_article_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$fc_avg_total_article_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$FCPlotOverview111 <- renderHighchart({
    
    input$fc_plot_article_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataArticlesFC$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataArticlesFC[which(DataArticlesFC$repost == 1),"created_time"], num = rep(1,nrow(DataArticlesFC[which(DataArticlesFC$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataArticlesFC[which(DataArticlesFC$original == 1),"created_time"], num = rep(1,nrow(DataArticlesFC[which(DataArticlesFC$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$fc_chart_type_overview1) %>%
        hc_add_series(reposts, name = "Reposts", type = input$fc_chart_type_overview1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_overview1 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$fc_chart_type_overview1 == "area" || input$fc_chart_type_overview1 == "column"){if(input$fc_chart_stack_overview1 != "none"){input$fc_chart_stack_overview1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$fc_chart_time_overview1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$FCPlotOverview12 <- renderHighchart({
    
    input$fc_plot_video_overview
    isolate({  
      
      AvgDailyVideoReach <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 0),]$post_reach, order.by = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      
      if(input$fc_video_variable_display_overview == "video views"){
        AvgDailyVideoVariable <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 0),]$post_video_views, order.by = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      } else {
        AvgDailyVideoVariable <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 0),]$total_comments+DataVideosFC[which(DataVideosFC$video_meme == 0),]$total_likes+DataVideosFC[which(DataVideosFC$video_meme == 0),]$total_shares , order.by = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      }
      
      if(input$fc_video_rate_display_overview == "interaction rate"){
        AvgDailyVideoRate <- as.xts(as.numeric(formatC(DataVideosFC[which(DataVideosFC$video_meme == 0),]$interaction_rate*100, format = "f", digits = 2)), order.by = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      } else {
        AvgDailyVideoRate <- as.xts(as.numeric(formatC((DataVideosFC[which(DataVideosFC$video_meme == 0),]$post_video_views/DataVideosFC[which(DataVideosFC$video_meme == 0),]$post_reach)*100, format = "f", digits = 2)), order.by = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      }
      
      sufix <- ifelse(input$fc_avg_total_video_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$fc_video_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$fc_avg_total_video_overview == "average", ifelse(input$fc_video_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        
        hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$fc_video_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$fc_video_rate_display_overview == "interaction rate", "IR", "VR"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_video_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$fc_avg_total_video_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$fc_chart_time_video_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$fc_avg_total_video_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$FCPlotOverview121 <- renderHighchart({
    
    input$fc_plot_video_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 0),]$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 0 & DataVideosFC$repost == 1),"created_time"], num = rep(1,nrow(DataVideosFC[which(DataVideosFC$video_meme == 0 & DataVideosFC$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 0 & DataVideosFC$original == 1),"created_time"], num = rep(1,nrow(DataVideosFC[which(DataVideosFC$video_meme == 0 & DataVideosFC$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$fc_chart_type_overview2) %>%
        hc_add_series(reposts, name = "Reposts", type = input$fc_chart_type_overview2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_overview2 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$fc_chart_type_overview2 == "area" || input$fc_chart_type_overview2 == "column"){if(input$fc_chart_stack_overview2 != "none"){input$fc_chart_stack_overview2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$fc_chart_time_overview2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$FCPlotOverview13 <- renderHighchart({
    
    input$fc_plot_videomeme_overview
    isolate({  
      
      AvgDailyVideoReach <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 1),]$post_reach, order.by = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      
      if(input$fc_videomeme_variable_display_overview == "video views"){
        AvgDailyVideoVariable <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 1),]$post_video_views, order.by = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      } else {
        AvgDailyVideoVariable <- as.xts(DataVideosFC[which(DataVideosFC$video_meme == 1),]$total_comments+DataVideosFC[which(DataVideosFC$video_meme == 1),]$total_likes+DataVideosFC[which(DataVideosFC$video_meme == 1),]$total_shares , order.by = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      }
      
      if(input$fc_videomeme_rate_display_overview == "interaction rate"){
        AvgDailyVideoRate <- as.xts(as.numeric(formatC(DataVideosFC[which(DataVideosFC$video_meme == 1),]$interaction_rate*100, format = "f", digits = 2)), order.by = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      } else {
        AvgDailyVideoRate <- as.xts(as.numeric(formatC((DataVideosFC[which(DataVideosFC$video_meme == 1),]$post_video_views/DataVideosFC[which(DataVideosFC$video_meme == 1),]$post_reach)*100, format = "f", digits = 2)), order.by = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      }
      
      sufix <- ifelse(input$fc_avg_total_videomeme_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$fc_videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$fc_avg_total_videomeme_overview == "average", ifelse(input$fc_videomeme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. VR"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        hc_add_series(AvgDailyVideoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyVideoVariable, id = "Daily Video Views", yAxis = 1, name = ifelse(input$fc_videomeme_variable_display_overview == "video views", "Video Views", "Interactions"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyVideoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$fc_videomeme_rate_display_overview == "interaction rate", "Interaction Rate", "Views Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_videomeme_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$fc_avg_total_videomeme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$fc_chart_time_videomeme_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$fc_avg_total_videomeme_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$FCPlotOverview131 <- renderHighchart({
    
    input$fc_plot_videomeme_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 1),]$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 1 & DataVideosFC$repost == 1),"created_time"], num = rep(1,nrow(DataVideosFC[which(DataVideosFC$video_meme == 1 & DataVideosFC$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideosFC[which(DataVideosFC$video_meme == 1 & DataVideosFC$original == 1),"created_time"], num = rep(1,nrow(DataVideosFC[which(DataVideosFC$video_meme == 1 & DataVideosFC$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$fc_chart_type_overview3) %>%
        hc_add_series(reposts, name = "Reposts", type = input$fc_chart_type_overview3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$fc_chart_type_overview3 == "area" || input$fc_chart_type_overview3 == "column"){if(input$fc_chart_stack_overview2 != "none"){input$fc_chart_stack_overview3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$fc_chart_time_overview3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$FCPlotOverview14 <- renderHighchart({
    
    input$fc_plot_meme_overview
    isolate({  
      
      AvgDailyPhotoReach <- as.xts(DataPhotosFC$post_reach, order.by = DataPhotosFC$created_time)
      AvgDailyPhotoFanReach <- as.xts(DataPhotosFC$post_reach_fan_unique, order.by = DataPhotosFC$created_time)
      AvgDailyPhotoIR <- as.xts(as.numeric(formatC(DataPhotosFC$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotosFC$created_time)
      
      if(input$fc_meme_variable_display_overview == "fan reach"){
        AvgDailyPhotoVariable <- as.xts(DataPhotosFC$post_reach_fan_unique, order.by = DataPhotosFC$created_time)
      } else {
        AvgDailyPhotoVariable <- as.xts(DataPhotosFC$post_reach_viral_unique, order.by = DataPhotosFC$created_time)
      }
      
      if(input$fc_meme_rate_display_overview == "interaction rate"){
        AvgDailyPhotoRate <- as.xts(as.numeric(formatC(DataPhotosFC$interaction_rate*100, format = "f", digits = 2)), order.by = DataPhotosFC$created_time)
      } else {
        AvgDailyPhotoRate <- as.xts(as.numeric(formatC((DataPhotosFC$total_shares/(DataPhotosFC$total_comments+DataPhotosFC$total_likes+DataPhotosFC$total_shares))*100, format = "f", digits = 2)), order.by = DataPhotosFC$created_time)
      }
      
      sufix <- ifelse(input$fc_avg_total_meme_overview == "average", "Avg.", "Total")
      
      hc <-highchart(type = "stock") %>%
        hc_yAxis_multiples(
          list(offset = 30, title = list(text = paste(sufix,"Reach", sep = " ")), height = "45%", top = "0%"),
          list(offset = 30, title = list(text = paste(sufix, ifelse(input$fc_meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), sep = " ")), height = "35%", top = "47.5%"),
          list(offset = 30, title = list(text = ifelse(input$fc_avg_total_videomeme_overview == "average", ifelse(input$fc_meme_rate_display_overview == "interaction rate", "Avg. IR", "Avg. Share Rate"), "Ignore!")), height = "15%", top = "85%", labels = list(format = "{value} %"))
        ) %>%
        
        hc_add_series(AvgDailyPhotoReach, id = "Daily Reach", name = "Reach", yAxis = 0, color = "#D73027", type = "area") %>%
        hc_add_series(AvgDailyPhotoVariable, id = "Daily Fan Reach", yAxis = 1, name = ifelse(input$fc_meme_variable_display_overview == "fan reach", "Fan Reach", "Viral Reach"), color = "#F46D43", type = "area") %>%
        hc_add_series(AvgDailyPhotoRate, id = "Daily IR", yAxis = 2, name = ifelse(input$fc_meme_rate_display_overview == "interaction rate", "Interaction Rate", "Share Rate"), type = "column", color = "#2580B9", tooltip = list(valueSuffix = " %")) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_meme_overview == "month", 5, 4)) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = "none", dataGrouping = list(approximation = input$fc_avg_total_meme_overview, enabled = TRUE, forced = TRUE, groupPixelWidth = 1, smoothed = TRUE, units = list(list(input$fc_chart_time_meme_overview, list(1)))))) %>%
        hc_tooltip(valueDecimals = ifelse(input$fc_avg_total_meme_overview == "average", 2, 0)) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$FCPlotOverview141 <- renderHighchart({
    
    input$fc_plot_meme_overview1
    isolate({ 
      
      dates <- data.frame(created_time = DataPhotosFC$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataPhotosFC[which(DataPhotosFC$repost == 1),"created_time"], num = rep(1,nrow(DataPhotosFC[which(DataPhotosFC$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataPhotosFC[which(DataPhotosFC$original == 1),"created_time"], num = rep(1,nrow(DataPhotosFC[which(DataPhotosFC$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$fc_chart_type_overview4) %>%
        hc_add_series(reposts, name = "Reposts", type = input$fc_chart_type_overview4) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$fc_chart_time_overview3 == "day", 0, 2)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$fc_chart_type_overview4 == "area" || input$fc_chart_type_overview4 == "column"){if(input$fc_chart_stack_overview2 != "none"){input$fc_chart_stack_overview4}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$fc_chart_time_overview4, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  #----------------------------------------------------------------------------------------------------------------------
  
  # 3. KPIS --------------------------------------------------------------------------------------------------------------------
  
  # 3.1. KPIS - We Are Mitú ---------------------------------------------------------------------------------------------------------------------------
  
  output$PlotKpis <- renderHighchart({

    input$plot_kpis
    isolate({

      dates <- data.frame(date = Data$date)
      dates$date <- as.POSIXct(dates$date)

      articles_originals <- merge(dates, data.frame(date = DataArticles[which(DataArticles$original == 1),]$date, num = rep(1,nrow(DataArticles[which(DataArticles$original == 1),]))), by = "date", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"date"])

      videos_originals <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]))), by = "date", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"date"])

      video_memes_originals <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]))), by = "date", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"date"])

      memes_originals <- merge(dates, data.frame(date = DataPhotos[which(DataPhotos$original == 1),]$date, num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 1),]))), by = "date", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"date"])


      articles_reposts <- merge(dates, data.frame(date = DataArticles[which(DataArticles$original == 0),]$date, num = rep(1,nrow(DataArticles[which(DataArticles$original == 0),]))), by = "date", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"date"])

      videos_reposts <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]))), by = "date", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"date"])

      video_memes_reposts <- merge(dates, data.frame(date = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$date, num = rep(1,nrow(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]))), by = "date", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"date"])

      memes_reposts <- merge(dates, data.frame(date = DataPhotos[which(DataPhotos$original == 0),]$date, num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 0),]))), by = "date", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"date"])

      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      # colores<- c('#08415C', '#08415C', '#CC2936', '#CC2936', '#EBBAB9', '#EBBAB9','#388697','#388697')

      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Output")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis == "area" || input$chart_type_kpis == "column"){if(input$chart_stack_kpis != "none"){input$chart_stack_kpis}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })

  output$PlotKpis1 <- renderHighchart({

    input$plot_kpis1
    isolate({

      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)

      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$link_clicks), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])

      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])

      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])

      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])


      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$link_clicks), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])

      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])

      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])

      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])


      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')

      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Views")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis1) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis1) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis1 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis1 == "area" || input$chart_type_kpis1 == "column"){if(input$chart_stack_kpis1 != "none"){input$chart_stack_kpis1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))

      hc
    })
  })

  output$PlotKpis2 <- renderHighchart({

    input$plot_kpis2
    isolate({

      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)

      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])

      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])

      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])

      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])


      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])

      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])

      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])

      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])

      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')

      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Interactions")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis2) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis2) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis2 == "area" || input$chart_type_kpis2 == "column"){if(input$chart_stack_kpis2 != "none"){input$chart_stack_kpis2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })

  output$PlotKpis3 <- renderHighchart({

    input$plot_kpis3
    isolate({

      dates <- data.frame(created_time = Data$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)

      articles_originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),]$created_time, num = DataArticles[which(DataArticles$original == 1),]$post_reach), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])

      videos_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])

      video_memes_originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])

      memes_originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),]$created_time, num = DataPhotos[which(DataPhotos$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])


      articles_reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 0),]$created_time, num = DataArticles[which(DataArticles$original == 0),]$post_reach), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])

      videos_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 0 & DataVideos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])

      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$created_time, num = DataVideos[which(DataVideos$video_meme == 1 & DataVideos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])

      memes_reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 0),]$created_time, num = DataPhotos[which(DataPhotos$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])

      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')

      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$chart_type_kpis3) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$chart_type_kpis3) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$chart_type_kpis3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_kpis3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_kpis3 == "area" || input$chart_type_kpis3 == "column"){if(input$chart_stack_kpis3 != "none"){input$chart_stack_kpis3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_kpis3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  
  
  output$PlotKpisContentOutputMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_content_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_output <- cumsum(as.numeric(DataMonth$content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_original_output + DataGoal[x,]$video_repost_output + DataGoal[x,]$video_meme_output + DataGoal[x,]$article_original_output + DataGoal[x,]$article_repost_output + DataGoal[x,]$meme_original_output + DataGoal[x,]$meme_repost_output)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Content Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Output")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Content Output Goal", pointPadding = 0) %>%
      hc_add_series(data = ds, name = "Content Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Content Output", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= "2017-01-01"),], .(date = format(Data[which(Data$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$video_original_output + DataGoals$video_repost_output + DataGoals$video_meme_output + DataGoals$article_original_output + DataGoals$article_repost_output + DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$page_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$page_content_output <- format( DataMonth$page_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Content Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_content_output <- cumsum(DataMonth$content_output)
    
    slope <- lm(cumsum(content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoals$total_content_output <- rowSums(cbind(DataGoals$video_original_output, DataGoals$video_repost_output, DataGoals$video_meme_output, DataGoals$article_original_output, DataGoals$article_repost_output, DataGoals$meme_original_output, DataGoals$meme_repost_output), na.rm = TRUE)
    
    goal <- as.numeric(ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_content_output, 0))

    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Content Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Output")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Content Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Content Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  output$PlotKpisArticleContentOutputMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_article_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),], .(date = format(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_article_content_output <- cumsum(as.numeric(DataMonth$article_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$article_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$article_original_output + DataGoal[x,]$article_repost_output)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Article Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Article Output Goal", pointPadding = 0) %>%
      hc_add_series(data = ds, name = "Article Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Article Output", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisArticleContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= "2017-01-01"),], .(date = format(DataArticles[which(DataArticles$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$article_original_output + DataGoals$article_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$article_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$article_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$article_content_output <- format( DataMonth$article_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisArticleContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Article Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisArticleContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_article_content_output, "01"), "%b %Y %d")
    
    # date_selected <- as.Date(paste("abr. 2017", "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "link"),], "date", summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_article_content_output <- cumsum(DataMonth$article_content_output)
    
    slope <- lm(cumsum(article_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$article_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoals$total_article_content_output <- DataGoals$article_original_output + DataGoals$article_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_article_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Article Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Article Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Article Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  output$PlotKpisVideoContentOutputMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_video_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_content_output <- cumsum(as.numeric(DataMonth$video_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_original_output + DataGoal[x,]$video_repost_output)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Video Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Video Output Goal", pointPadding = 0) %>%
      hc_add_series(data = ds, name = "Video Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Video Output", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideoContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$video_original_output + DataGoals$video_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$video_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$video_content_output <- format( DataMonth$video_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideoContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Video Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideoContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_video_content_output, "01"), "%b %Y %d")
    
    # date_selected <- as.Date(paste("abr. 2017", "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "video"),], "date", summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_content_output <- cumsum(DataMonth$video_content_output)
    
    slope <- lm(cumsum(video_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$video_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoals$total_video_content_output <- DataGoals$video_original_output + DataGoals$video_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_video_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Video Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Video Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Video Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  output$PlotKpisMemeContentOutputMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_meme_output, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),], .(date = format(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_meme_content_output <- cumsum(as.numeric(DataMonth$meme_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$meme_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$meme_original_output + DataGoal[x,]$meme_repost_output)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Meme Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Meme Output Goal", pointPadding = 0) %>%
      hc_add_series(data = ds, name = "Meme Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Meme Output", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisMemeContentOutputTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= "2017-01-01"),], .(date = format(DataPhotos[which(DataPhotos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoals$content_output <- DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    DataMonth <- merge(DataGoals[, c("date", "content_output")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_output-DataMonth$meme_content_output)/DataMonth$content_output
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$meme_content_output)),]
    
    # DataMonth$content_output <- format( DataMonth$content_output, big.mark = ",")
    # DataMonth$meme_content_output <- format( DataMonth$meme_content_output, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisMemeContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Meme Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisMemeContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_meme_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected & Data$post_type == "photo"),], "date", summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_meme_content_output <- cumsum(DataMonth$meme_content_output)
    
    slope <- lm(cumsum(meme_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$meme_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoals$total_meme_content_output <- DataGoals$meme_original_output + DataGoals$meme_repost_output
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$total_meme_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Meme Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Meme Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Meme Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  
  output$PlotKpisTotalFollowers <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_total_followers, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], "date", summarize, page_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, page_followers = max(page_followers), new_followers = sum(new_followers))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)

    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$page_followers))
    })
 
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$total_followers)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Total Followers", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Totla Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Total Followers Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Total Followers", pointPadding = 0.2) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisTotalFollowersTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= "2017-01-01"),], "date", summarize, page_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, page_followers = max(page_followers))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "total_followers")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$total_followers-DataMonth$page_followers)/DataMonth$total_followers
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_followers)),]
    
    DataMonth$total_followers <- format( DataMonth$total_followers, big.mark = ",")
    DataMonth$page_followers <- format( DataMonth$page_followers, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  
  
  output$PlotKpisNewFollowersMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_new_followers, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
   
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, new_followers = sum(page_fan_adds))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_new_followers <- cumsum(as.numeric(DataMonth$new_followers))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$new_followers))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
      
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$new_followers)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
   
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)

    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "New Followers - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "New Followers Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "New Followers", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisNewFollowersTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= "2017-01-01"),], .(date = format(WamPageData[which(WamPageData$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_new_followers = sum(page_fan_adds))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "new_followers")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$new_followers-DataMonth$page_new_followers)/DataMonth$new_followers
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_new_followers)),]
    
    DataMonth$new_followers <- format( DataMonth$new_followers, big.mark = ",")
    DataMonth$page_new_followers <- format( DataMonth$page_new_followers, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisNewFollowers <- renderHighchart({
   
    date_selected <- as.Date(paste(input$kpis_month_new_followers, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    # DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, new_followers = sum(page_fan_adds))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_new_followers <- cumsum(DataMonth$new_followers)
    
    slope <- lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
   
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$new_followers)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$new_followers, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "New Followers - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily New Followers", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  

  output$PlotKpisReachMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_reach, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, reach = sum(page_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_reach <- cumsum(as.numeric(DataMonth$reach))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$reach))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$reach)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Reach - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Reach Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Reach", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisReachTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Reach - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisReach <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_reach, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    # DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, reach = sum(post_reach))
    # 
    DataMonth <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, reach = sum(page_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$reach)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$reach, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Reach - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Reach", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisEngagementMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_engagement, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_interactions <- cumsum(as.numeric(DataMonth$interactions))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$engagement)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, interactions = sum(total_interactions))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Engagement - Monthly", align = "center") %>%
      hc_yAxis(title = list(text = "Engagement - Monthly")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Engagement Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Engagement", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisEngagementTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Engagement - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisEngagement<- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_engagement, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$interactions)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$engagement, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Engagement - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Engagement", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisContentViewsMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_content_views, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),], .(date = format(Data[which(Data$date >= year_selected & Data$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, content_views = sum(post_reach[post_type == "photo"]) + sum(link_clicks[post_type == "link"]))
    
    DataMonthVideo <- ddply(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1) & WamPageData$date %in% Data$date),], .(date = format(WamPageData[which(WamPageData$date >= year_selected & WamPageData$date < year_selected + years(1) & WamPageData$date %in% Data$date),]$date, "%Y-%m")), summarize, content_views_video = sum(page_video_views))
    
    DataMonth$content_views <- DataMonth$content_views + DataMonthVideo$content_views_video
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_views <- cumsum(as.numeric(DataMonth$content_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$content_views)
    })
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == format.Date(max(Data$date), "%m") & format.Date(Data$date, "%Y") == format.Date(max(Data$date), "%Y")),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), video_views = sum(post_video_views[post_type == "video"]), content_views = photo_views + article_views + video_views)
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Content Views - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Content Views Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Content Views", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisContentViewsTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(Data[which(Data$date >= "2017-01-01"),], .(date = format(Data[which(Data$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, page_content_views = sum(post_reach[post_type == "photo"]) + sum(link_clicks[post_type == "link"]))
    
    DataMonthVideo <- ddply(WamPageData[which(WamPageData$date >= "2017-01-01" & WamPageData$date %in% Data$date),], .(date = format(WamPageData[which(WamPageData$date >= "2017-01-01" & WamPageData$date %in% Data$date),]$date, "%Y-%m")), summarize, content_views_video = sum(page_video_views))
    
    DataMonth$page_content_views <- DataMonth$page_content_views + DataMonthVideo$content_views_video
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$content_views-DataMonth$page_content_views)/DataMonth$content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$page_content_views)),]
    
    DataMonth$content_views <- format( DataMonth$content_views, big.mark = ",")
    DataMonth$page_content_views <- format( DataMonth$page_content_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisContentViews <- renderHighchart({
    
    date_selected <- as.Date(paste(input$kpis_month_content_views, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(Data[which(format.Date(Data$date, "%m") == month_selected & format.Date(Data$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), content_views = photo_views + article_views)
    
    DataMonthVideo <- ddply(WamPageData[which(format.Date(WamPageData$date, "%m") == month_selected & format.Date(WamPageData$date, "%Y") == year_selected),], "date", summarize, video_views = sum(page_video_views))
    
    DataMonth$content_views <- DataMonth$content_views + DataMonthVideo[which(DataMonthVideo$date %in% DataMonth$date),]$video_views
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_content_views <- cumsum(DataMonth$content_views)
    
    slope <- lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Content Views - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Content Views", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })

  
  
  output$PlotKpisArticlesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_articles, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),], .(date = format(DataArticles[which(DataArticles$date >= year_selected & DataArticles$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, link_clicks = sum(link_clicks))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_link_clicks <- cumsum(as.numeric(DataMonth$link_clicks))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$link_clicks))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_link_clicks)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
     
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$article_content_views)
    })
     
    DataMonth <- ddply(DataArticles[which(format.Date(DataArticles$date, "%m") == format.Date(max(DataArticles$date), "%m") & format.Date(DataArticles$date, "%Y") == format.Date(max(DataArticles$date), "%Y")),], "date", summarize, link_clicks = sum(link_clicks))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(link_clicks) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)

    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Articles CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Articles CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Articles CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Articles CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisArticlesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataArticles[which(DataArticles$date >= "2017-01-01"),], .(date = format(DataArticles[which(DataArticles$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, link_clicks = sum(link_clicks))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "article_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$article_content_views-DataMonth$link_clicks)/DataMonth$article_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$link_clicks)),]
    
    DataMonth$article_content_views <- format( DataMonth$article_content_views, big.mark = ",")
    DataMonth$link_clicks <- format( DataMonth$link_clicks, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisArticles <- renderHighchart({
    
    if(input$articles_kpi_select_repost == "Originals"){
      
      DataArticles <- DataArticles[which(DataArticles$original == 1),]
      
    }
    
    else if (input$articles_kpi_select_repost == "Reposts"){
      
      DataArticles <- DataArticles[which(DataArticles$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_articles, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataArticles[which(format.Date(DataArticles$date, "%m") == month_selected & format.Date(DataArticles$date, "%Y") == year_selected),], "date", summarize, link_clicks = sum(link_clicks), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_link_clicks <- cumsum(DataMonth$link_clicks)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$articles_kpi_select_variable == "Content Views",lm(cumsum(link_clicks) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$articles_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$articles_kpi_select_variable == "Content Views", DataMonth[x,]$link_clicks, ifelse(input$articles_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$articles_kpi_select_variable == "Content Views", DataMonth[x,]$total_link_clicks, ifelse(input$articles_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$article_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Articles CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$articles_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$articles_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$articles_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisAllVideosMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_all_videos, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))

    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$all_video_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "All Videos CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "All Videos CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "All Videos CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total All Videos CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisAllVideosTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "all_video_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$all_video_content_views-DataMonth$video_views)/DataMonth$all_video_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$all_video_content_views <- format( DataMonth$all_video_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisAllVideos <- renderHighchart({
    
    if(input$all_videos_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$all_videos_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_all_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")

    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))

    DataMonth <- ddply(DataVideos[which(format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$all_videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$all_videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))

    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$all_video_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })

    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "All Videos CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$all_videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$all_videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$all_videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisVideosMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_videos, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Videos CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Videos CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Videos CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Videos CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideosTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "video_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$video_content_views-DataMonth$video_views)/DataMonth$video_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$video_content_views <- format( DataMonth$video_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideos <- renderHighchart({
    
    if(input$videos_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$videos_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 0 & format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
   
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$video_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Videos CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  
  
  output$PlotKpisVideoMemesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_video_memes, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),], .(date = format(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= year_selected & DataVideos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_views <- cumsum(as.numeric(DataMonth$video_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$video_meme_content_views)
    })
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & format.Date(DataVideos$date, "%m") == format.Date(max(DataVideos$date), "%m") & format.Date(DataVideos$date, "%Y") == format.Date(max(DataVideos$date), "%Y")),], "date", summarize, video_views = sum(post_video_views))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Video Memes CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Video Memes CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Video Memes CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Video Memes CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisVideoMemesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= "2017-01-01"),], .(date = format(DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, video_views = sum(post_video_views))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "video_meme_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$video_meme_content_views-DataMonth$video_views)/DataMonth$video_meme_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$video_views)),]
    
    DataMonth$video_meme_content_views <- format( DataMonth$video_meme_content_views, big.mark = ",")
    DataMonth$video_views <- format( DataMonth$video_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisVideoMemes <- renderHighchart({
    
    if(input$video_memes_kpi_select_repost == "Originals"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 1),]
      
    }
    
    else if (input$video_memes_kpi_select_repost == "Reposts"){
      
      DataVideos <- DataVideos[which(DataVideos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_video_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideos[which(DataVideos$video_meme == 1 & format.Date(DataVideos$date, "%m") == month_selected & format.Date(DataVideos$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$video_memes_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$video_memes_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))

    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$video_meme_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })

    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Video Memes CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$video_memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$video_memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$video_memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })

  
  
  output$PlotKpisMemesMonth <- renderHighchart({
    
    year_selected <- as.Date(paste(input$kpis_year_memes, "-01-01", sep = ""))
    
    dates <- data.frame(date = format(DataGoals[which(DataGoals$date >= year_selected & DataGoals$date < year_selected + years(1)),]$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),], .(date = format(DataPhotos[which(DataPhotos$date >= year_selected & DataPhotos$date < year_selected + years(1)),]$date, "%Y-%m")), summarize, photo_views = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_photo_views <- cumsum(as.numeric(DataMonth$photo_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$photo_views))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_photo_views)
    })
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoals[which(DataGoals$date >= format(year_selected, "%Y-%m") & DataGoals$date < format(year_selected + years(1), "%Y-%m")),], by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$meme_content_views)
    })
    
    DataMonth <- ddply(DataPhotos[which(format.Date(DataPhotos$date, "%m") == format.Date(max(DataPhotos$date), "%m") & format.Date(DataPhotos$date, "%Y") == format.Date(max(DataPhotos$date), "%Y")),], "date", summarize, photo_views = sum(post_reach))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(Data$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(Data$date), "%Y-%m-01"))))*(lm(cumsum(photo_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Memes CV - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Memes CV Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Memes CV", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Memes CV", type = "spline", visible = FALSE) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$PlotKpisMemesTable <- DT::renderDataTable({
    
    dates <- data.frame(date = format(DataGoals$date, "%Y-%m"))
    
    DataMonth <- ddply(DataPhotos[which(DataPhotos$date >= "2017-01-01"),], .(date = format(DataPhotos[which(DataPhotos$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, photo_views = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    DataGoals$date <- format(DataGoals$date, "%Y-%m")
    
    DataMonth <- merge(DataGoals[, c("date", "meme_content_views")], DataMonth, by = "date", all = TRUE)
    
    DataMonth$percent <- -(DataMonth$meme_content_views-DataMonth$photo_views)/DataMonth$meme_content_views
    
    DataMonth <- DataMonth[which(!is.na(DataMonth$photo_views)),]
    
    DataMonth$meme_content_views <- format( DataMonth$meme_content_views, big.mark = ",")
    DataMonth$photo_views <- format( DataMonth$photo_views, big.mark = ",")
    DataMonth$percent <- ifelse(DataMonth$percent <= 0, paste0("<span style = 'color:red'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"), paste0("<span style = 'color:green'>",formatC(100*DataMonth$percent, format = "f", digits = 2), "%", "</span>"))
    
    colnames(DataMonth) <- c("Month", "Goal", "Actual", "%")
    
    DataMonth
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:3))), dom = "t"))
  
  output$PlotKpisMemes <- renderHighchart({
    
    if(input$memes_kpi_select_repost == "Originals"){
      
      DataPhotos <- DataPhotos[which(DataPhotos$original == 1),]
      
    }
    
    else if (input$memes_kpi_select_repost == "Reposts"){
      
      DataPhotos <- DataPhotos[which(DataPhotos$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$kpis_month_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))

    DataMonth <- ddply(DataPhotos[which(format.Date(DataPhotos$date, "%m") == month_selected & format.Date(DataPhotos$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach), interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(Data$date)] <- 0
    DataMonth$total_photo_views <- cumsum(DataMonth$photo_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- ifelse(input$memes_kpi_select_variable == "Content Views", lm(cumsum(photo_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])

    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$photo_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_photo_views)
    })
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$memes_kpi_select_variable == "Content Views", DataMonth[x,]$photo_views, DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_photo_views, DataMonth[x,]$total_interactions))
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m") %in% DataGoals$date, DataGoals[which(DataGoals$date == format(date_selected, "%Y-%m")),]$meme_content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })

    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Memes CV - Daily", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc

  })
  
  # 3.2. KPIS - Bad Hombres ---------------------------------------------------------------------------------------------------------------------------
  
  output$BHPlotKpis <- renderHighchart({
    
    input$bh_plot_kpis
    isolate({
      
      dates <- data.frame(date = DataBH$date)
      dates$date <- as.POSIXct(dates$date)
      
      articles_originals <- merge(dates, data.frame(date = DataArticlesBH[which(DataArticlesBH$original == 1),]$date, num = rep(1,nrow(DataArticlesBH[which(DataArticlesBH$original == 1),]))), by = "date", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"date"])
      
      videos_originals <- merge(dates, data.frame(date = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$date, num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]))), by = "date", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"date"])
      
      video_memes_originals <- merge(dates, data.frame(date = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$date, num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]))), by = "date", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"date"])
      
      memes_originals <- merge(dates, data.frame(date = DataPhotosBH[which(DataPhotosBH$original == 1),]$date, num = rep(1,nrow(DataPhotosBH[which(DataPhotosBH$original == 1),]))), by = "date", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"date"])
      
      
      articles_reposts <- merge(dates, data.frame(date = DataArticlesBH[which(DataArticlesBH$original == 0),]$date, num = rep(1,nrow(DataArticlesBH[which(DataArticlesBH$original == 0),]))), by = "date", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"date"])
      
      videos_reposts <- merge(dates, data.frame(date = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$date, num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]))), by = "date", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"date"])
      
      video_memes_reposts <- merge(dates, data.frame(date = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$date, num = rep(1,nrow(DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]))), by = "date", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"date"])
      
      memes_reposts <- merge(dates, data.frame(date = DataPhotosBH[which(DataPhotosBH$original == 0),]$date, num = rep(1,nrow(DataPhotosBH[which(DataPhotosBH$original == 0),]))), by = "date", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"date"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      # colores<- c('#08415C', '#08415C', '#CC2936', '#CC2936', '#EBBAB9', '#EBBAB9','#388697','#388697')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Output")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$bh_chart_type_kpis) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$bh_chart_type_kpis) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$bh_chart_type_kpis) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$bh_chart_type_kpis) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$bh_chart_type_kpis) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$bh_chart_type_kpis) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$bh_chart_type_kpis) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$bh_chart_type_kpis) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_kpis == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_kpis == "area" || input$bh_chart_type_kpis == "column"){if(input$bh_chart_stack_kpis != "none"){input$bh_chart_stack_kpis}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_kpis, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })
  
  output$BHPlotKpis1 <- renderHighchart({
    
    input$bh_plot_kpis1
    isolate({
      
      dates <- data.frame(created_time = DataBH$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 1),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 1),]$link_clicks), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 1),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 0),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 0),]$link_clicks), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$post_video_views), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 0),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Content Views")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$bh_chart_type_kpis1) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$bh_chart_type_kpis1) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_kpis1 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_kpis1 == "area" || input$bh_chart_type_kpis1 == "column"){if(input$bh_chart_stack_kpis1 != "none"){input$bh_chart_stack_kpis1}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_kpis1, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      
      hc
    })
  })
  
  output$BHPlotKpis2 <- renderHighchart({
    
    input$bh_plot_kpis2
    isolate({
      
      dates <- data.frame(created_time = DataBH$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 1),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 1),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 1),]$total_interactions), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 0),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 0),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 0),]$total_interactions), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Interactions")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$bh_chart_type_kpis2) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$bh_chart_type_kpis2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_kpis2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_kpis2 == "area" || input$bh_chart_type_kpis2 == "column"){if(input$bh_chart_stack_kpis2 != "none"){input$bh_chart_stack_kpis2}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_kpis2, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))%>%
        hc_defs(patterns = list(list(id = "custom-pattern", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#08415C', strokeWidth = 2)), list(id = "custom-pattern-1", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#CC2936', strokeWidth = 2)), list(id = "custom-pattern-2", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#EBBAB9', strokeWidth = 2)), list(id = "custom-pattern-3", path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11', stroke = '#388697', strokeWidth = 2))))
      hc
    })
  })
  
  output$BHPlotKpis3 <- renderHighchart({
    
    input$bh_plot_kpis3
    isolate({
      
      dates <- data.frame(created_time = DataBH$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      articles_originals <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 1),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 1),]$post_reach), by = "created_time", all = TRUE)
      articles_originals <- as.xts(articles_originals[,"num"], order.by = articles_originals[,"created_time"])
      
      videos_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 1),]$post_reach), by = "created_time", all = TRUE)
      videos_originals <- as.xts(videos_originals[,"num"], order.by = videos_originals[,"created_time"])
      
      video_memes_originals <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 1),]$post_reach), by = "created_time", all = TRUE)
      video_memes_originals <- as.xts(video_memes_originals[,"num"], order.by = video_memes_originals[,"created_time"])
      
      memes_originals <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 1),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 1),]$post_reach), by = "created_time", all = TRUE)
      memes_originals <- as.xts(memes_originals[,"num"], order.by = memes_originals[,"created_time"])
      
      
      articles_reposts <- merge(dates, data.frame(created_time = DataArticlesBH[which(DataArticlesBH$original == 0),]$created_time, num = DataArticlesBH[which(DataArticlesBH$original == 0),]$post_reach), by = "created_time", all = TRUE)
      articles_reposts <- as.xts(articles_reposts[,"num"], order.by = articles_reposts[,"created_time"])
      
      videos_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 0 & DataVideosBH$original == 0),]$post_reach), by = "created_time", all = TRUE)
      videos_reposts <- as.xts(videos_reposts[,"num"], order.by = videos_reposts[,"created_time"])
      
      video_memes_reposts <- merge(dates, data.frame(created_time = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$created_time, num = DataVideosBH[which(DataVideosBH$video_meme == 1 & DataVideosBH$original == 0),]$post_reach), by = "created_time", all = TRUE)
      video_memes_reposts <- as.xts(video_memes_reposts[,"num"], order.by = video_memes_reposts[,"created_time"])
      
      memes_reposts <- merge(dates, data.frame(created_time = DataPhotosBH[which(DataPhotosBH$original == 0),]$created_time, num = DataPhotosBH[which(DataPhotosBH$original == 0),]$post_reach), by = "created_time", all = TRUE)
      memes_reposts <- as.xts(memes_reposts[,"num"], order.by = memes_reposts[,"created_time"])
      
      colores<- c('#08415C', 'url(#custom-pattern)', '#CC2936', 'url(#custom-pattern-1)', '#EBBAB9', 'url(#custom-pattern-2)','#388697','url(#custom-pattern-3)')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>%
        hc_add_series(articles_originals, name = "Article Originals", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(articles_reposts, name = "Article Reposts", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(videos_originals, name = "Video Originals", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(videos_reposts, name = "Video Reposts", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(video_memes_originals, name = "Video Meme Originals", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(video_memes_reposts, name = "Video Meme Reposts", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(memes_originals, name = "Meme Originals", type = input$bh_chart_type_kpis3) %>%
        hc_add_series(memes_reposts, name = "Meme Reposts", type = input$bh_chart_type_kpis3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$bh_chart_time_kpis3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$bh_chart_type_kpis3 == "area" || input$bh_chart_type_kpis3 == "column"){if(input$bh_chart_stack_kpis3 != "none"){input$bh_chart_stack_kpis3}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$bh_chart_time_kpis3, list(1)))))) %>%
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        # hc_tooltip(pointFormat = "'{series.name}: {point.y} <br>' + 'Total: {point.total}'")%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$BHPlotKpisContentOutputMonth <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_output <- cumsum(as.numeric(DataMonth$content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = sum(DataGoal[x,]$video_original_output, DataGoal[x,]$video_repost_output, DataGoal[x,]$video_meme_output, DataGoal[x,]$article_original_output, DataGoal[x,]$article_repost_output, DataGoal[x,]$meme_original_output, DataGoal[x,]$meme_repost_output, na.rm = TRUE))
    })
    
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Content Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Content Output Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Content Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Content Output", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Content Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_content_output, "01"), "%b %Y %d")
    
    # date_selected <- as.Date(paste("abr. 2017", "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected),], "date", summarize, content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_content_output <- cumsum(DataMonth$content_output)
    
    slope <- lm(cumsum(content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_output)
    })
    
    DataGoalsBH$total_content_output <- rowSums(cbind(DataGoalsBH$video_original_output, DataGoalsBH$video_repost_output, DataGoalsBH$video_meme_output, DataGoalsBH$article_original_output, DataGoalsBH$article_repost_output, DataGoalsBH$meme_original_output, DataGoalsBH$meme_repost_output), na.rm = TRUE)
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$total_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Content Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Output")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Content Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Content Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisArticleContentOutputMonth <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "link"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "link"),]$date, "%Y-%m")), summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_article_content_output <- cumsum(as.numeric(DataMonth$article_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$article_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = sum(DataGoal[x,]$article_original_output, DataGoal[x,]$article_repost_output, na.rm = TRUE))
    })
    
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Article Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Article Output Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Article Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Article Output", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisArticleContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Article Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisArticleContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_article_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected & DataBH$post_type == "link"),], "date", summarize, article_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_article_content_output <- NA
    DataMonth[!is.na(DataMonth$article_content_output),]$total_article_content_output <- cumsum(DataMonth[!is.na(DataMonth$article_content_output),]$article_content_output)
    
    slope <- lm(cumsum(article_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$article_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_article_content_output)
    })
    
    DataGoalsBH$total_article_content_output <- rowSums(cbind(DataGoalsBH$article_original_output, DataGoalsBH$article_repost_output), na.rm = TRUE)
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$total_article_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))

    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Article Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Article Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Article Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisVideoContentOutputMonth <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "video"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "video"),]$date, "%Y-%m")), summarize, video_content_output = ifelse(is.na(length(post_reach)), 0, length(post_reach)))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_video_content_output <- NA
    DataMonth[!is.na(DataMonth$video_content_output),]$total_video_content_output <- cumsum(as.numeric(DataMonth[!is.na(DataMonth$video_content_output),]$video_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$video_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = sum(DataGoal[x,]$video_original_output, DataGoal[x,]$video_repost_output, na.rm = TRUE))
    })
    
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Video Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Video Output Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Video Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Video Output", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisVideoContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Video Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisVideoContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_video_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected & DataBH$post_type == "video"),], "date", summarize, video_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_video_content_output <- cumsum(DataMonth$video_content_output)
    
    slope <- lm(cumsum(video_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$video_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_video_content_output)
    })
    
    DataGoalsBH$total_video_content_output <- rowSums(cbind(DataGoalsBH$video_original_output, DataGoalsBH$video_repost_output), na.rm = TRUE)
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$total_video_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Video Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Video Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Video Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisMemeContentOutputMonth <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "photo"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01" & DataBH$post_type == "photo"),]$date, "%Y-%m")), summarize, meme_content_output = ifelse(is.na(length(post_reach)), 0, length(post_reach)))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_meme_content_output <- cumsum(as.numeric(DataMonth$meme_content_output))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$meme_content_output))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = sum(DataGoal[x,]$meme_original_output, DataGoal[x,]$meme_repost_output, na.rm = TRUE))
    })
    
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Meme Output - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Meme Output Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Meme Output", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Meme Output", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisMemeContentOutputTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Meme Output - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisMemeContentOutput <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_meme_content_output, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected & DataBH$post_type == "photo"),], "date", summarize, meme_content_output = length(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_meme_content_output <- cumsum(DataMonth$meme_content_output)
    
    slope <- lm(cumsum(meme_content_output) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$meme_content_output)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_meme_content_output)
    })
    
    DataGoalsBH$total_meme_content_output <- rowSums(cbind(DataGoalsBH$meme_original_output, DataGoalsBH$meme_repost_output), na.rm = TRUE)
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$total_meme_content_output, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Meme Output - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Number of Posts")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Meme Output", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Meme Output", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisTotalFollowers <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], "date", summarize, page_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, page_followers = max(page_followers), new_followers = sum(new_followers))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$page_followers))
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$total_followers)
    })
    
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Total Followers", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Totla Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Total Followers Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Total Followers", pointPadding = 0.2) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisNewFollowersMonth16 <- renderHighchart({
    
    dates <- data.frame(date = format(seq(min(DataBH[which(DataBH$date >= "2016-01-01"),]$date), max(DataBH[which(DataBH$date < "2017-01-01"),]$date), by = "month"), "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2016-01-01" & DataBH$date < "2017-01-01"),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, total_followers = sum(total_followers), new_followers = sum(new_followers))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_new_followers <- cumsum(as.numeric(DataMonth$new_followers))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$new_followers))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    # DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    # 
    # DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    # 
    # ds_goal <- lapply(1:nrow(DataGoal), function(x){
    #   list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$new_followers)
    # })
    # 
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      # hc_add_series(data = ds_goal, name = "New Followers Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "New Followers", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisNewFollowersMonth <- renderHighchart({
    
    # dates <- data.frame(date = format(seq(min(DataBH[which(DataBH$date >= "2017-01-01"),]$date), max(DataBH[which(DataBH$date >= "2017-01-01"),]$date), by = "month"), "%Y-%m"))
    # 
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- ddply(DataMonth, .(date = format(DataMonth$date, "%Y-%m")), summarize, total_followers = sum(total_followers), new_followers = sum(new_followers))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_new_followers <- cumsum(as.numeric(DataMonth$new_followers))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$new_followers))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$new_followers)
    })
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "New Followers - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "New Followers Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "New Followers", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisNewFollowersTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "New Followers - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisNewFollowers <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_new_followers, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected),], "date", summarize, total_followers = mean(page_total_likes), new_followers = mean(page_new_likes))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_new_followers <- cumsum(DataMonth$new_followers)
    
    slope <- lm(cumsum(new_followers) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$new_followers)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_new_followers)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$new_followers, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "New Followers")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily New Followers", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total New Followers", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisReachMonth16 <- renderHighchart({
    
    dates <- data.frame(date = format(seq(min(DataBH[which(DataBH$date >= "2016-01-01"),]$date), max(DataBH[which(DataBH$date < "2017-01-01"),]$date), by = "month"), "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2016-01-01" & DataBH$date < "2017-01-01"),], .(date = format(DataBH[which(DataBH$date >= "2016-01-01" & DataBH$date < "2017-01-01"),]$date, "%Y-%m")), summarize, reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_reach <- cumsum(as.numeric(DataMonth$reach))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$reach))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    # DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    # 
    # DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    # 
    # ds_goal <- lapply(1:nrow(DataGoal), function(x){
    #   list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$reach)
    # })
    # 
    # DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    # 
    # DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    # 
    # DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    # 
    # ds_forecast <- lapply(1:nrow(DataForecast), function(x){
    #   list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    # })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      # hc_add_series(data = ds_goal, name = "Reach Goal", pointPadding = 0) %>%
      # hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Reach", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisReachMonth <- renderHighchart({
    
    # dates <- data.frame(date = format(seq(min(DataBH[which(DataBH$date >= "2017-01-01"),]$date), max(DataBH[which(DataBH$date >= "2017-01-01"),]$date), by = "month"), "%Y-%m"))
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_reach <- cumsum(as.numeric(DataMonth$reach))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$reach))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$reach)
    })
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, reach = sum(post_reach))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Reach - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Reach Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Reach", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisReachTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Reach - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisReach <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_reach, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected),], "date", summarize, reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$reach)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_reach)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$reach, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Reach - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Reach")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Reach", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Reach", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisEngagementMonth <- renderHighchart({
    
    # dates <- data.frame(date = format(seq(min(DataBH[which(DataBH$date >= "2017-01-01"),]$date), max(DataBH[which(DataBH$date >= "2017-01-01"),]$date), by = "month"), "%Y-%m"))
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_interactions <- cumsum(as.numeric(DataMonth$interactions))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = round(DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$engagement)
    })
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, interactions = sum(total_interactions))
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Engagement - Monthly", align = "center") %>%
      hc_yAxis(title = list(text = "Engagement - Monthly")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Engagement Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Engagement", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisEngagementTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Engagement - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisEngagement<- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_engagement, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected),], "date", summarize, interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$interactions)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_interactions)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$engagement, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_yAxis(title = list(text = "Engagement - Daily")) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Engagement")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Engagement", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Engagement", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisContentViewsMonth <- renderHighchart({
    
    dates <- data.frame(date = format(DataGoalsBH$date, "%Y-%m"))
    
    DataMonth <- ddply(DataBH[which(DataBH$date >= "2017-01-01"),], .(date = format(DataBH[which(DataBH$date >= "2017-01-01"),]$date, "%Y-%m")), summarize, content_views = sum(post_reach[post_type == "photo"]) + sum(link_clicks[post_type == "link"]) + sum(post_video_views[post_type == "video"]))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth$total_content_views <- cumsum(as.numeric(DataMonth$content_views))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    DataGoalsBH$date <- format(DataGoalsBH$date, "%Y-%m")
    
    DataGoal <- merge(dates, DataGoalsBH, by = "date", all = TRUE)
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$content_views)
    })
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == format.Date(max(DataBH$date), "%m") & format.Date(DataBH$date, "%Y") == format.Date(max(DataBH$date), "%Y")),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), video_views = sum(post_video_views[post_type == "video"]), content_views = photo_views + article_views + video_views)
    
    DataForecast <- data.frame(date = format(DataMonth[1,]$date, "%Y-%m"), forecast = round((as.numeric(as.Date(format(max(DataBH$date), "%Y-%m-01")) %m+% months(1) - as.Date(format(max(DataBH$date), "%Y-%m-01"))))*(lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])))
    
    DataForecast <- merge(dates, DataForecast, by = "date", all = TRUE)
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Content Views - Monthly", align = "center") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds_goal, name = "Content Views Goal", pointPadding = 0) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", color = "#c91910", pointPadding = 0.2) %>%
      hc_add_series(data = ds, name = "Content Views", pointPadding = 0.2) %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline") %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(grouping = FALSE, shadow = FALSE, borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisContentViewsTitle <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 50) %>%
      hc_title(text = "Content Views - Daily", align = "center") %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisContentViews <- renderHighchart({
    
    date_selected <- as.Date(paste(input$bh_kpis_month_content_views, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataBH[which(format.Date(DataBH$date, "%m") == month_selected & format.Date(DataBH$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach[post_type == "photo"]), article_views = sum(link_clicks[post_type == "link"]), video_views = sum(post_video_views[post_type == "video"]), content_views = photo_views + article_views + video_views)
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_content_views <- cumsum(DataMonth$content_views)
    
    slope <- lm(cumsum(content_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$content_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_content_views)
    })
    
    goal <- ifelse(format(date_selected, "%Y-%m-%d") %in% DataGoalsBH$date, DataGoalsBH[which(DataGoalsBH$date == format(date_selected, "%Y-%m-%d")),]$content_views, 0)
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Content Views")) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = "Daily Content Views", type = "column") %>%
      hc_add_series(data = ds_total, name = "Total Content Views", type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  output$BHPlotKpisArticles <- renderHighchart({
    
    if(input$bh_articles_kpi_select_repost == "Originals"){
      
      DataArticlesBH <- DataArticlesBH[which(DataArticlesBH$original == 1),]
      
    }
    
    else if (input$bh_articles_kpi_select_repost == "Reposts"){
      
      DataArticlesBH <- DataArticlesBH[which(DataArticlesBH$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$bh_kpis_month_articles, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataArticlesBH[which(format.Date(DataArticlesBH$date, "%m") == month_selected & format.Date(DataArticlesBH$date, "%Y") == year_selected),], "date", summarize, link_clicks = sum(link_clicks), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_link_clicks <- cumsum(DataMonth$link_clicks)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$bh_articles_kpi_select_variable == "Content Views",lm(cumsum(link_clicks) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$bh_articles_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_articles_kpi_select_variable == "Content Views", DataMonth[x,]$link_clicks, ifelse(input$bh_articles_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_articles_kpi_select_variable == "Content Views", DataMonth[x,]$total_link_clicks, ifelse(input$bh_articles_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- 2000
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$bh_articles_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$bh_articles_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$bh_articles_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisAllVideos <- renderHighchart({
    
    if(input$bh_all_videos_kpi_select_repost == "Originals"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 1),]
      
    }
    
    else if (input$bh_all_videos_kpi_select_repost == "Reposts"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$bh_kpis_month_all_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideosBH[which(format.Date(DataVideosBH$date, "%m") == month_selected & format.Date(DataVideosBH$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$bh_all_videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$bh_all_videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$bh_all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_all_videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$bh_all_videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- 5000000
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$bh_all_videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$bh_all_videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$bh_all_videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisVideos <- renderHighchart({
    
    if(input$bh_videos_kpi_select_repost == "Originals"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 1),]
      
    }
    
    else if (input$bh_videos_kpi_select_repost == "Reposts"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$bh_kpis_month_videos, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideosBH[which(DataVideosBH$video_meme == 0 & format.Date(DataVideosBH$date, "%m") == month_selected & format.Date(DataVideosBH$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$bh_videos_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$bh_videos_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_videos_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$bh_videos_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_videos_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$bh_videos_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- 2500000
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$bh_videos_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$bh_videos_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$bh_videos_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisVideoMemes <- renderHighchart({
    
    if(input$bh_video_memes_kpi_select_repost == "Originals"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 1),]
      
    }
    
    else if (input$bh_video_memes_kpi_select_repost == "Reposts"){
      
      DataVideosBH <- DataVideosBH[which(DataVideosBH$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$bh_kpis_month_video_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataVideosBH[which(DataVideosBH$video_meme == 1 & format.Date(DataVideosBH$date, "%m") == month_selected & format.Date(DataVideosBH$date, "%Y") == year_selected),], "date", summarize, video_views = sum(post_video_views), interactions = sum(total_interactions), reach = sum(post_reach))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_video_views <- cumsum(DataMonth$video_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    DataMonth$total_reach <- cumsum(DataMonth$reach)
    
    slope <- ifelse(input$bh_video_memes_kpi_select_variable == "Content Views",lm(cumsum(video_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], ifelse(input$bh_video_memes_kpi_select_variable == "Interactions", lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(reach) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2]))
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$video_views, ifelse(input$bh_video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$interactions, DataMonth[x,]$reach)))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_video_memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_video_views, ifelse(input$bh_video_memes_kpi_select_variable == "Interactions", DataMonth[x,]$total_interactions, DataMonth[x,]$total_reach)))
    })
    
    goal <- 2500000
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$bh_video_memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$bh_video_memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$bh_video_memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BHPlotKpisMemes <- renderHighchart({
    
    if(input$bh_memes_kpi_select_repost == "Originals"){
      
      DataPhotosBH <- DataPhotosBH[which(DataPhotosBH$original == 1),]
      
    }
    
    else if (input$bh_memes_kpi_select_repost == "Reposts"){
      
      DataPhotosBH <- DataPhotosBH[which(DataPhotosBH$original == 0),]
    }
    
    date_selected <- as.Date(paste(input$bh_kpis_month_memes, "01"), "%b %Y %d")
    
    month_selected <- format(date_selected, "%m")
    year_selected <- format(date_selected,"%Y")
    
    dates <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"))
    
    DataMonth <- ddply(DataPhotosBH[which(format.Date(DataPhotosBH$date, "%m") == month_selected & format.Date(DataPhotosBH$date, "%Y") == year_selected),], "date", summarize, photo_views = sum(post_reach), interactions = sum(total_interactions))
    
    DataMonth <- merge(dates, DataMonth, by = "date", all = TRUE)
    DataMonth[is.na(DataMonth) & DataMonth$date <= max(DataBH$date)] <- 0
    DataMonth$total_photo_views <- cumsum(DataMonth$photo_views)
    DataMonth$total_interactions <- cumsum(DataMonth$interactions)
    
    slope <- ifelse(input$bh_memes_kpi_select_variable == "Content Views", lm(cumsum(photo_views) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2], lm(cumsum(interactions) ~ seq(1:nrow(DataMonth)), data = DataMonth)$coeff[2])
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$photo_views)
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = DataMonth[x,]$total_photo_views)
    })
    
    ds <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_memes_kpi_select_variable == "Content Views", DataMonth[x,]$photo_views, DataMonth[x,]$interactions))
    })
    
    ds_total <- lapply(1:nrow(DataMonth), function(x){
      list(name = format(DataMonth[x,]$date, format = "%b - %d"), y = ifelse(input$bh_memes_kpi_select_variable == "Content Views", DataMonth[x,]$total_photo_views, DataMonth[x,]$total_interactions))
    })
    
    goal <- 800000
    
    DataGoal <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), goal = (seq(goal/nrow(dates), goal, by = goal/nrow(dates))))
    
    ds_goal <- lapply(1:nrow(DataGoal), function(x){
      list(name = format(DataGoal[x,]$date, format = "%b - %d"), y = DataGoal[x,]$goal)
    })
    
    DataForecast <- data.frame(date = seq(date_selected, date_selected %m+% months(1) - 1, by = "day"), forecast = (cumsum(rep(slope, nrow(dates)))))
    
    ds_forecast <- lapply(1:nrow(DataForecast), function(x){
      list(name = format(DataForecast[x,]$date, format = "%b - %d"), y = DataForecast[x,]$forecast)
    })
    
    hc <-highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = input$bh_memes_kpi_select_variable)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_add_series(data = ds, name = paste("Daily", input$bh_memes_kpi_select_variable, sep = " "), type = "column") %>%
      hc_add_series(data = ds_total, name = paste("Total", input$bh_memes_kpi_select_variable, sep = " "), type = "spline") %>%
      hc_add_series(data = ds_goal, name = "Goal", type = "spline", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_add_series(data = ds_forecast, name = "Forecast", type = "spline", color = "#c91910", tooltip = list(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b><br/>')) %>%
      hc_plotOptions(spline = list(marker = list(enabled = FALSE)), column = list(borderColor = "black")) %>%
      hc_tooltip(shared = TRUE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  #----------------------------------------------------------------------------------------------------------------------
  
  Plot_Top_Bottom_Function <- function (ds, input_categories, input_plot_var, fntltp) {
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), bar = list(borderColor = "black")) %>% 
      hc_xAxis(categories = input_categories) %>% 
      hc_yAxis(title = list(text = input_plot_var))%>% 
      hc_add_series(data = ds)%>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_tooltip(useHTML = TRUE, formatter = fntltp, pointFormat = '{point.y:,.0f}')%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
    
  }
  
  Plot_Article_Rates_Function <- function (ds_ctr, ds_ir, input_categories){
    
    hc <- highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), bar = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = input_categories) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %")) %>% 
      hc_add_series(data = ds_ctr, name = "CTR") %>% 
      hc_add_series(data = ds_ir, name = "IR") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(valueSuffix = " %") %>%
      hc_add_theme(hc_theme_smpl()) 
    hc
    
  }
  
  Plot_Article_Reaction_Rates_Function <- function (ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories){
    
    hc <- highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), bar = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = input_categories) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %"), max = 100) %>% 
      hc_add_series(data = ds_feed_likes_rate, name = "Likes") %>% 
      hc_add_series(data = ds_love, name = "Love") %>%
      hc_add_series(data = ds_wow, name = "Wow") %>%
      hc_add_series(data = ds_haha, name = "Haha") %>%
      hc_add_series(data = ds_sad, name = "Sad") %>%
      hc_add_series(data = ds_angry, name = "Angry") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(valueSuffix = " %") %>%
      hc_add_theme(hc_theme_smpl()) 
    hc
    
  }
  
  Plot_Fan_Viral_Function <- function (ds_fan, ds_viral, input_categories){
    
    hc <- highchart() %>%
      hc_chart(type = "bar") %>% 
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), bar = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = input_categories, lineWidth = 0, minorGridLineWidth = 0, lineColor = "transparent", labels = list(enabled = FALSE), minorTickLength = 0, tickLength = 0) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %"), max = 100) %>% 
      hc_add_series(data = ds_fan, name = "Fan") %>% 
      hc_add_series(data = ds_viral, name = "Viral") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(valueSuffix = " %") %>%
      hc_add_theme(hc_theme_smpl()) 
    hc
    
  }
  
  Plot_Video_Drop_Off_Function <- function (ds){
    
    hc <- highchart() %>% 
      hc_chart(type = "spline") %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>% 
      hc_add_series_list(ds) %>% 
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "right", verticalAlign = "top", layout = "vertical", floating = TRUE, y = 1) %>%  
      hc_xAxis(title = list(text = "Seconds Viewed")) %>%
      hc_tooltip(shared = TRUE)
    hc
    
  }
  
  # 4. Top & Bottom Post --------------------------------------------------------------------------------------------------------------------
  
  # 4.1. Top & Bottom Posts - We Are Mitú ---------------------------------------------------------------------------------------------------------------------------
  
  # 4.1.1. Articles  ------------------------------------------------------------------------------------------------------
  
  # 4.1.1.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$Table1 = DT::renderDataTable({
    
    WeekLinksTop <- WeekLinksTop()
    
    WeekLinksTop$post_reach <- format( WeekLinksTop$post_reach, big.mark = ",")
    WeekLinksTop$link_clicks <- format( WeekLinksTop$link_clicks, big.mark = ",")
    WeekLinksTop$headline <- paste0("<a href='", WeekLinksTop$permalink,"' target='_blank'>", WeekLinksTop$headline,"</a>")
    WeekLinksTop$ctr <- paste0(formatC(100 * WeekLinksTop$ctr, format = "f", digits = 2), "%")
    WeekLinksTop$interaction_rate <- paste0(formatC(100 * WeekLinksTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksTop[, input$show_vars1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot1_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    if(input$article_select_original_repost_top == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$article_select_original_repost_top == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(number_article_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeArticles), number_article_top$n)), " of ", as.character(nrow(DateRangeArticles)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot1 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTop()
   
    ds <- lapply(1:nrow(WeekLinksTop), function(x){
      list(name = ifelse(WeekLinksTop[x,]$repost == 1, "Repost", "Original"), url = WeekLinksTop[x,]$permalink, color = ifelse(WeekLinksTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksTop[x,]$full_picture, share_text = WeekLinksTop[x,]$sharetext, link_clicks = WeekLinksTop[x,]$link_clicks, rank_link_clicks = WeekLinksTop[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksTop[x,]$general_rank_link_clicks, rank_reach = WeekLinksTop[x,]$rank_reach, general_rank_reach = WeekLinksTop[x,]$general_rank_reach,  rank_interactions = WeekLinksTop[x,]$rank_interactions,  general_rank_interactions = WeekLinksTop[x,]$general_rank_interactions, reach = WeekLinksTop[x,]$post_reach, interactions = WeekLinksTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksTop[x,]$times_repost), 0, WeekLinksTop[x,]$times_repost), post_category = WeekLinksTop[x,]$category, y = ifelse(input$article_select_plot_variable_top == "Link Clicks", WeekLinksTop[x,]$link_clicks, ifelse(input$article_select_plot_variable_top == "Reach", WeekLinksTop[x,]$post_reach, WeekLinksTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    input_plot_var <- input$article_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
  
  })
  
  output$Plot2 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksTop <- WeekLinksTop()

    WeekLinksTop$ctr <- as.numeric(format(WeekLinksTop$ctr*100, digits= 3))
    WeekLinksTop$interaction_rate <- as.numeric(format(WeekLinksTop$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$ctr, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$interaction_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)
    
  })
  
  output$Plot21 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTop()
    
    WeekLinksTop$feed_likes_rate <- as.numeric(format(WeekLinksTop$feed_likes_rate*100, digits= 4))
    WeekLinksTop$love_rate <- as.numeric(format(WeekLinksTop$love_rate*100, digits = 2))
    WeekLinksTop$wow_rate <- as.numeric(format(WeekLinksTop$wow_rate*100, digits = 2))
    WeekLinksTop$haha_rate <- as.numeric(format(WeekLinksTop$haha_rate*100, digits = 2))
    WeekLinksTop$sad_rate <- as.numeric(format(WeekLinksTop$sad_rate*100, digits = 2))
    WeekLinksTop$angry_rate <- as.numeric(format(WeekLinksTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$feed_likes_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$love_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$wow_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$haha_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$sad_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$angry_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot22 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTop()
    
    WeekLinksTop$fan_rate <- as.numeric(format(WeekLinksTop$fan_rate*100, digits = 4))
    WeekLinksTop$viral_rate <- as.numeric(format(WeekLinksTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$fan_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$viral_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.1.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$Table2 = DT::renderDataTable({
    
    WeekLinksBottom <- WeekLinksBottom()
    
    WeekLinksBottom$post_reach <- format(WeekLinksBottom$post_reach, big.mark = ",")
    WeekLinksBottom$link_clicks <- format(WeekLinksBottom$link_clicks, big.mark = ",")
    WeekLinksBottom$headline <- paste0("<a href='",WeekLinksBottom$permalink,"' target='_blank'>",WeekLinksBottom$headline,"</a>")
    WeekLinksBottom$ctr <- paste0(formatC(100 * WeekLinksBottom$ctr, format = "f", digits = 2), "%")
    WeekLinksBottom$interaction_rate <- paste0(formatC(100 * WeekLinksBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksBottom[, input$show_vars4, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot3_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    if(input$article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$article_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeArticles) + 10 - number_article_bottom$n, nrow(DateRangeArticles))), " - ", as.character(max(nrow(DateRangeArticles) - number_article_bottom$n + 1, 1)), " of ", as.character(nrow(DateRangeArticles)))
                           
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot3 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottom()
    
    ds <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(name = ifelse(WeekLinksBottom[x,]$repost == 1, "Repost", "Original"), url = WeekLinksBottom[x,]$permalink, color = ifelse(WeekLinksBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksBottom[x,]$full_picture, share_text = WeekLinksBottom[x,]$sharetext, link_clicks = WeekLinksBottom[x,]$link_clicks, rank_link_clicks = WeekLinksBottom[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksBottom[x,]$general_rank_link_clicks, rank_reach = WeekLinksBottom[x,]$rank_reach, general_rank_reach = WeekLinksBottom[x,]$general_rank_reach,  rank_interactions = WeekLinksBottom[x,]$rank_interactions,  general_rank_interactions = WeekLinksBottom[x,]$general_rank_interactions, reach = WeekLinksBottom[x,]$post_reach, interactions = WeekLinksBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksBottom[x,]$times_repost), 0, WeekLinksBottom[x,]$times_repost), post_category = WeekLinksBottom[x,]$category, y = ifelse(input$article_select_plot_variable_bottom == "Link Clicks", WeekLinksBottom[x,]$link_clicks, ifelse(input$article_select_plot_variable_bottom == "Reach", WeekLinksBottom[x,]$post_reach, WeekLinksBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    input_plot_var <- input$article_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$Plot4 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksBottom <- WeekLinksBottom()
    
    WeekLinksBottom$ctr <- as.numeric(format(WeekLinksBottom$ctr*100, digits= 2))
    WeekLinksBottom$interaction_rate <- as.numeric(format(WeekLinksBottom$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$ctr, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$interaction_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)

  })
  
  output$Plot41 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottom()
    
    WeekLinksBottom$feed_likes_rate <- as.numeric(format(WeekLinksBottom$feed_likes_rate*100, digits= 4))
    WeekLinksBottom$love_rate <- as.numeric(format(WeekLinksBottom$love_rate*100, digits = 2))
    WeekLinksBottom$wow_rate <- as.numeric(format(WeekLinksBottom$wow_rate*100, digits = 2))
    WeekLinksBottom$haha_rate <- as.numeric(format(WeekLinksBottom$haha_rate*100, digits = 2))
    WeekLinksBottom$sad_rate <- as.numeric(format(WeekLinksBottom$sad_rate*100, digits = 2))
    WeekLinksBottom$angry_rate <- as.numeric(format(WeekLinksBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$feed_likes_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$love_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$wow_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$haha_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$sad_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$angry_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot42 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottom()
    
    WeekLinksBottom$fan_rate <- as.numeric(format(WeekLinksBottom$fan_rate*100, digits = 4))
    WeekLinksBottom$viral_rate <- as.numeric(format(WeekLinksBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$fan_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$viral_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.2. Videos --------------------------------------------------------------------------------------------------------
  
  # 4.1.2.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$Table3 = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideosTop()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$show_vars2, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot5_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    if(input$video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(number_video_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), number_video_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot5 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTop()
    
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$video_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$video_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$video_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$Plot52 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTop()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideos)), grep("s0", colnames(DataVideos)):grep("s40", colnames(DataVideos)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
   
  })
  
  output$Plot53 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekVideosTop <- WeekVideosTop()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot54 <- renderHighchart({

    WeekVideosTop <- WeekVideosTop()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.2.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$Table4 = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideosBottom()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$show_vars5, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot6_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    if(input$video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - number_video_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - number_video_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot6 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottom()

    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$video_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$video_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$video_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$Plot62 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottom()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideos)), grep("s0", colnames(DataVideos)):grep("s40", colnames(DataVideos)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$Plot63 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottom()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot64 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottom()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.3. Video Memes ----------------------------------------------------------------------------------------------------
  
  # 4.1.3.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$Table3b = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideoMemesTop()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$show_vars2b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot5b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    if(input$video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(number_video_meme_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), number_video_meme_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot5b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTop()

    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$video_meme_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$video_meme_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$video_meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)

  })
  
  output$Plot52b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTop()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideos)),grep("s0", colnames(DataVideos)):grep("s40", colnames(DataVideos)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df, "sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
   
  })
  
  output$Plot53b <- renderHighchart({

    WeekVideosTop <- WeekVideoMemesTop()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot54b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTop()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.3.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$Table4b = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideoMemesBottom()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$show_vars5b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$Plot6b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    if(input$video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$video_meme_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - number_video_meme_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - number_video_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
  
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot6b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottom()

    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$video_meme_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$video_meme_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$video_meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
 
  })
  
  output$Plot62b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottom()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideos)), grep("s0", colnames(DataVideos)):grep("s40", colnames(DataVideos)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
   
  })
  
  output$Plot63b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottom()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot64b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottom()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.4. Memes ----------------------------------------------------------------------------------------------------------
  
  # 4.1.4.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$Table5 = DT::renderDataTable({
    
    WeekPhotosTop <- WeekPhotoTop()
    
    WeekPhotosTop$post_reach <- format(WeekPhotosTop$post_reach, big.mark = ",")
    WeekPhotosTop$comment<- format(WeekPhotosTop$comment, big.mark = ",")
    WeekPhotosTop$like <- format(WeekPhotosTop$like, big.mark = ",")
    WeekPhotosTop$share <- format(WeekPhotosTop$share, big.mark = ",")
    WeekPhotosTop$sharetext <- paste0("<a href='",WeekPhotosTop$permalink,"' target='_blank'>",WeekPhotosTop$sharetext,"</a>")
    
    WeekPhotosTop$ctr <- paste0(formatC(100 * WeekPhotosTop$ctr, format = "f", digits = 2), "%")
    WeekPhotosTop$interaction_rate <- paste0(formatC(100 * WeekPhotosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosTop[, input$show_vars3, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$Plot7_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    if(input$meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(number_meme_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeMemes), number_meme_top$n)), " of ", as.character(nrow(DateRangeMemes)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot7 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTop()
 
    ds <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(name = ifelse(WeekPhotosTop[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosTop[x,]$permalink, color = ifelse(WeekPhotosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosTop[x,]$full_picture, share_text = WeekPhotosTop[x,]$sharetext, rank_reach = WeekPhotosTop[x,]$rank_reach, general_rank_reach = WeekPhotosTop[x,]$general_rank_reach,  rank_interactions = WeekPhotosTop[x,]$rank_interactions,  general_rank_interactions = WeekPhotosTop[x,]$general_rank_interactions, reach = WeekPhotosTop[x,]$post_reach, interactions = WeekPhotosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosTop[x,]$times_repost), 0, WeekPhotosTop[x,]$times_repost), y = ifelse(input$meme_select_plot_variable_top == "Reach", WeekPhotosTop[x,]$post_reach, WeekPhotosTop[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    input_plot_var <- input$meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)

  })
  
  output$Plot71 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTop()
    
    WeekPhotosTop$feed_likes_rate <- as.numeric(format(WeekPhotosTop$feed_likes_rate*100, digits= 4))
    WeekPhotosTop$love_rate <- as.numeric(format(WeekPhotosTop$love_rate*100, digits = 2))
    WeekPhotosTop$wow_rate <- as.numeric(format(WeekPhotosTop$wow_rate*100, digits = 2))
    WeekPhotosTop$haha_rate <- as.numeric(format(WeekPhotosTop$haha_rate*100, digits = 2))
    WeekPhotosTop$sad_rate <- as.numeric(format(WeekPhotosTop$sad_rate*100, digits = 2))
    WeekPhotosTop$angry_rate <- as.numeric(format(WeekPhotosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$feed_likes_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$love_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$wow_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$haha_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$sad_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$angry_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot72 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTop()
    
    WeekPhotosTop$fan_rate <- as.numeric(format(WeekPhotosTop$fan_rate*100, digits = 4))
    WeekPhotosTop$viral_rate <- as.numeric(format(WeekPhotosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$fan_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$viral_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.1.4.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$Table6 = DT::renderDataTable({
    
    WeekPhotosBottom <- WeekPhotoBottom()
    
    WeekPhotosBottom$post_reach <- format(WeekPhotosBottom$post_reach, big.mark = ",")
    WeekPhotosBottom$comment<- format(WeekPhotosBottom$comment, big.mark = ",")
    WeekPhotosBottom$like <- format(WeekPhotosBottom$like, big.mark = ",")
    WeekPhotosBottom$share <- format(WeekPhotosBottom$share, big.mark = ",")
    WeekPhotosBottom$sharetext <- paste0("<a href='",WeekPhotosBottom$permalink,"' target='_blank'>",WeekPhotosBottom$sharetext,"</a>")
    
    WeekPhotosBottom$ctr <- paste0(formatC(100 * WeekPhotosBottom$ctr, format = "f", digits = 2), "%")
    WeekPhotosBottom$interaction_rate <- paste0(formatC(100 * WeekPhotosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosBottom[, input$show_vars6, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$Plot8_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    if(input$meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeMemes) + 10 - number_meme_bottom$n, nrow(DateRangeMemes))), " - ", as.character(max(nrow(DateRangeMemes) - number_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeMemes))
   
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$Plot8 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottom() 

    ds <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(name = ifelse(WeekPhotosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosBottom[x,]$permalink, color = ifelse(WeekPhotosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosBottom[x,]$full_picture, share_text = WeekPhotosBottom[x,]$sharetext, rank_reach = WeekPhotosBottom[x,]$rank_reach, general_rank_reach = WeekPhotosBottom[x,]$general_rank_reach,  rank_interactions = WeekPhotosBottom[x,]$rank_interactions,  general_rank_interactions = WeekPhotosBottom[x,]$general_rank_interactions, reach = WeekPhotosBottom[x,]$post_reach, interactions = WeekPhotosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosBottom[x,]$times_repost), 0, WeekPhotosBottom[x,]$times_repost), y = ifelse(input$meme_select_plot_variable_bottom == "Reach", WeekPhotosBottom[x,]$post_reach, WeekPhotosBottom[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    input_plot_var <- input$meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
  
  })
  
  output$Plot81 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottom() 
    
    WeekPhotosBottom$feed_likes_rate <- as.numeric(format(WeekPhotosBottom$feed_likes_rate*100, digits= 4))
    WeekPhotosBottom$love_rate <- as.numeric(format(WeekPhotosBottom$love_rate*100, digits = 2))
    WeekPhotosBottom$wow_rate <- as.numeric(format(WeekPhotosBottom$wow_rate*100, digits = 2))
    WeekPhotosBottom$haha_rate <- as.numeric(format(WeekPhotosBottom$haha_rate*100, digits = 2))
    WeekPhotosBottom$sad_rate <- as.numeric(format(WeekPhotosBottom$sad_rate*100, digits = 2))
    WeekPhotosBottom$angry_rate <- as.numeric(format(WeekPhotosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$feed_likes_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$love_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$wow_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$haha_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$sad_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$angry_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$Plot82 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottom() 
    
    WeekPhotosBottom$fan_rate <- as.numeric(format(WeekPhotosBottom$fan_rate*100, digits = 4))
    WeekPhotosBottom$viral_rate <- as.numeric(format(WeekPhotosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$fan_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$viral_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  
  #---------------------------------------------------------------------------------------------------------------------
  
  # 4.2. Top & Bottom Posts - Bad Hombres ---------------------------------------------------------------------------------------------------------------------------
  
  # 4.2.1. Articles  ------------------------------------------------------------------------------------------------------
  
  # 4.2.1.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table1 = DT::renderDataTable({
    
    WeekLinksTop <- WeekLinksTopBH()
    
    WeekLinksTop$post_reach <- format( WeekLinksTop$post_reach, big.mark = ",")
    WeekLinksTop$link_clicks <- format( WeekLinksTop$link_clicks, big.mark = ",")
    WeekLinksTop$headline <- paste0("<a href='", WeekLinksTop$permalink,"' target='_blank'>", WeekLinksTop$headline,"</a>")
    WeekLinksTop$ctr <- paste0(formatC(100 * WeekLinksTop$ctr, format = "f", digits = 2), "%")
    WeekLinksTop$interaction_rate <- paste0(formatC(100 * WeekLinksTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksTop[, input$bh_show_vars1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot1_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    if(input$bh_article_select_original_repost_top == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$bh_article_select_original_repost_top == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(bh_number_article_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeArticles), bh_number_article_top$n)), " of ", as.character(nrow(DateRangeArticles)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot1 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopBH()
    
    ds <- lapply(1:nrow(WeekLinksTop), function(x){
      list(name = ifelse(WeekLinksTop[x,]$repost == 1, "Repost", "Original"), url = WeekLinksTop[x,]$permalink, color = ifelse(WeekLinksTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksTop[x,]$full_picture, share_text = WeekLinksTop[x,]$sharetext, link_clicks = WeekLinksTop[x,]$link_clicks, rank_link_clicks = WeekLinksTop[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksTop[x,]$general_rank_link_clicks, rank_reach = WeekLinksTop[x,]$rank_reach, general_rank_reach = WeekLinksTop[x,]$general_rank_reach,  rank_interactions = WeekLinksTop[x,]$rank_interactions,  general_rank_interactions = WeekLinksTop[x,]$general_rank_interactions, reach = WeekLinksTop[x,]$post_reach, interactions = WeekLinksTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksTop[x,]$times_repost), 0, WeekLinksTop[x,]$times_repost), post_category = WeekLinksTop[x,]$category, y = ifelse(input$bh_article_select_plot_variable_top == "Link Clicks", WeekLinksTop[x,]$link_clicks, ifelse(input$bh_article_select_plot_variable_top == "Reach", WeekLinksTop[x,]$post_reach, WeekLinksTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    input_plot_var <- input$bh_article_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
   
  })
  
  output$BH_Plot2 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksTop <- WeekLinksTopBH()
    
    WeekLinksTop$ctr <- as.numeric(format(WeekLinksTop$ctr*100, digits= 3))
    WeekLinksTop$interaction_rate <- as.numeric(format(WeekLinksTop$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$ctr, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$interaction_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)

  })
  
  output$BH_Plot21 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopBH()
    
    WeekLinksTop$feed_likes_rate <- as.numeric(format(WeekLinksTop$feed_likes_rate*100, digits= 4))
    WeekLinksTop$love_rate <- as.numeric(format(WeekLinksTop$love_rate*100, digits = 2))
    WeekLinksTop$wow_rate <- as.numeric(format(WeekLinksTop$wow_rate*100, digits = 2))
    WeekLinksTop$haha_rate <- as.numeric(format(WeekLinksTop$haha_rate*100, digits = 2))
    WeekLinksTop$sad_rate <- as.numeric(format(WeekLinksTop$sad_rate*100, digits = 2))
    WeekLinksTop$angry_rate <- as.numeric(format(WeekLinksTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$feed_likes_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$love_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$wow_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$haha_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$sad_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$angry_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot22 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopBH()
    
    WeekLinksTop$fan_rate <- as.numeric(format(WeekLinksTop$fan_rate*100, digits = 4))
    WeekLinksTop$viral_rate <- as.numeric(format(WeekLinksTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$fan_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$viral_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.1.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table2 = DT::renderDataTable({
    
    WeekLinksBottom <- WeekLinksBottomBH()
    
    WeekLinksBottom$post_reach <- format(WeekLinksBottom$post_reach, big.mark = ",")
    WeekLinksBottom$link_clicks <- format(WeekLinksBottom$link_clicks, big.mark = ",")
    WeekLinksBottom$headline <- paste0("<a href='",WeekLinksBottom$permalink,"' target='_blank'>",WeekLinksBottom$headline,"</a>")
    WeekLinksBottom$ctr <- paste0(formatC(100 * WeekLinksBottom$ctr, format = "f", digits = 2), "%")
    WeekLinksBottom$interaction_rate <- paste0(formatC(100 * WeekLinksBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksBottom[, input$bh_show_vars4, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot3_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    if(input$bh_article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$bh_article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$bh_article_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeArticles) + 10 - bh_number_article_bottom$n, nrow(DateRangeArticles))), " - ", as.character(max(nrow(DateRangeArticles) - bh_number_article_bottom$n + 1, 1)), " of ", as.character(nrow(DateRangeArticles)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot3 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomBH()
    
    ds <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(name = ifelse(WeekLinksBottom[x,]$repost == 1, "Repost", "Original"), url = WeekLinksBottom[x,]$permalink, color = ifelse(WeekLinksBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksBottom[x,]$full_picture, share_text = WeekLinksBottom[x,]$sharetext, link_clicks = WeekLinksBottom[x,]$link_clicks, rank_link_clicks = WeekLinksBottom[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksBottom[x,]$general_rank_link_clicks, rank_reach = WeekLinksBottom[x,]$rank_reach, general_rank_reach = WeekLinksBottom[x,]$general_rank_reach,  rank_interactions = WeekLinksBottom[x,]$rank_interactions,  general_rank_interactions = WeekLinksBottom[x,]$general_rank_interactions, reach = WeekLinksBottom[x,]$post_reach, interactions = WeekLinksBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksBottom[x,]$times_repost), 0, WeekLinksBottom[x,]$times_repost), post_category = WeekLinksBottom[x,]$category, y = ifelse(input$bh_article_select_plot_variable_bottom == "Link Clicks", WeekLinksBottom[x,]$link_clicks, ifelse(input$bh_article_select_plot_variable_bottom == "Reach", WeekLinksBottom[x,]$post_reach, WeekLinksBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    input_plot_var <- input$bh_article_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot4 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksBottom <- WeekLinksBottomBH()
    
    WeekLinksBottom$ctr <- as.numeric(format(WeekLinksBottom$ctr*100, digits= 2))
    WeekLinksBottom$interaction_rate <- as.numeric(format(WeekLinksBottom$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$ctr, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$interaction_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)
  
  })
  
  output$BH_Plot41 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomBH()
    
    WeekLinksBottom$feed_likes_rate <- as.numeric(format(WeekLinksBottom$feed_likes_rate*100, digits= 4))
    WeekLinksBottom$love_rate <- as.numeric(format(WeekLinksBottom$love_rate*100, digits = 2))
    WeekLinksBottom$wow_rate <- as.numeric(format(WeekLinksBottom$wow_rate*100, digits = 2))
    WeekLinksBottom$haha_rate <- as.numeric(format(WeekLinksBottom$haha_rate*100, digits = 2))
    WeekLinksBottom$sad_rate <- as.numeric(format(WeekLinksBottom$sad_rate*100, digits = 2))
    WeekLinksBottom$angry_rate <- as.numeric(format(WeekLinksBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$feed_likes_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$love_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$wow_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$haha_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$sad_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$angry_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot42 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomBH()
    
    WeekLinksBottom$fan_rate <- as.numeric(format(WeekLinksBottom$fan_rate*100, digits = 4))
    WeekLinksBottom$viral_rate <- as.numeric(format(WeekLinksBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$fan_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$viral_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.2. Videos  --------------------------------------------------------------------------------------------------------
  
  # 4.2.2.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table3 = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideosTopBH()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$bh_show_vars2, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot5_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    if(input$bh_video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(bh_number_video_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), bh_number_video_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot5 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopBH()
    
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$bh_video_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$bh_video_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$bh_video_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
  
  })
  
  output$BH_Plot52 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopBH()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideosBH)), grep("s0", colnames(DataVideosBH)):grep("s40", colnames(DataVideosBH)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$BH_Plot53 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekVideosTop <- WeekVideosTopBH()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot54 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopBH()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.2.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table4 = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideosBottomBH()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$bh_show_vars5, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot6_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 0),]
    
    if(input$bh_video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - bh_number_video_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - bh_number_video_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot6 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomBH()
    
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$bh_video_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$bh_video_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$bh_video_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot62 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomBH()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideosBH)), grep("s0", colnames(DataVideosBH)):grep("s40", colnames(DataVideosBH)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$BH_Plot63 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomBH()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot64 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomBH()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.3. Video Memes ----------------------------------------------------------------------------------------------------
  
  # 4.2.3.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table3b = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideoMemesTopBH()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$bh_show_vars2b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot5b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    if(input$bh_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(bh_number_video_meme_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), bh_number_video_meme_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot5b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopBH()
    
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$bh_video_meme_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$bh_video_meme_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$bh_video_meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot52b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopBH()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideosBH)), grep("s0", colnames(DataVideosBH)):grep("s40", colnames(DataVideosBH)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$BH_Plot53b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopBH()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot54b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopBH()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.3.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table4b = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideoMemesBottomBH()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$bh_show_vars5b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$BH_Plot6b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosBH[which(DataVideosBH$date >= input$dateRange1[1] & DataVideosBH$date <= input$dateRange1[2] & DataVideosBH$video_meme == 1),]
    
    if(input$bh_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$bh_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$bh_video_meme_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - bh_number_video_meme_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - bh_number_video_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot6b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomBH()
    
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$bh_video_meme_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$bh_video_meme_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$bh_video_meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot62b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomBH()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideosBH)), grep("s0", colnames(DataVideosBH)):grep("s40", colnames(DataVideosBH)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$BH_Plot63b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomBH()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot64b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomBH()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.2.4. Memes ----------------------------------------------------------------------------------------------------------
  
  # 4.2.4.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table5 = DT::renderDataTable({
    
    WeekPhotosTop <- WeekPhotoTopBH()
    
    WeekPhotosTop$post_reach <- format(WeekPhotosTop$post_reach, big.mark = ",")
    WeekPhotosTop$comment<- format(WeekPhotosTop$comment, big.mark = ",")
    WeekPhotosTop$like <- format(WeekPhotosTop$like, big.mark = ",")
    WeekPhotosTop$share <- format(WeekPhotosTop$share, big.mark = ",")
    WeekPhotosTop$sharetext <- paste0("<a href='",WeekPhotosTop$permalink,"' target='_blank'>",WeekPhotosTop$sharetext,"</a>")
    
    WeekPhotosTop$ctr <- paste0(formatC(100 * WeekPhotosTop$ctr, format = "f", digits = 2), "%")
    WeekPhotosTop$interaction_rate <- paste0(formatC(100 * WeekPhotosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosTop[, input$bh_show_vars3, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$BH_Plot7_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    
    if(input$bh_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(bh_number_meme_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeMemes), bh_number_meme_top$n)), " of ", as.character(nrow(DateRangeMemes)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot7 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopBH()
   
    ds <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(name = ifelse(WeekPhotosTop[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosTop[x,]$permalink, color = ifelse(WeekPhotosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosTop[x,]$full_picture, share_text = WeekPhotosTop[x,]$sharetext, rank_reach = WeekPhotosTop[x,]$rank_reach, general_rank_reach = WeekPhotosTop[x,]$general_rank_reach,  rank_interactions = WeekPhotosTop[x,]$rank_interactions,  general_rank_interactions = WeekPhotosTop[x,]$general_rank_interactions, reach = WeekPhotosTop[x,]$post_reach, interactions = WeekPhotosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosTop[x,]$times_repost), 0, WeekPhotosTop[x,]$times_repost), y = ifelse(input$bh_meme_select_plot_variable_top == "Reach", WeekPhotosTop[x,]$post_reach, WeekPhotosTop[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    input_plot_var <- input$bh_meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot71 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopBH()
    
    WeekPhotosTop$feed_likes_rate <- as.numeric(format(WeekPhotosTop$feed_likes_rate*100, digits= 4))
    WeekPhotosTop$love_rate <- as.numeric(format(WeekPhotosTop$love_rate*100, digits = 2))
    WeekPhotosTop$wow_rate <- as.numeric(format(WeekPhotosTop$wow_rate*100, digits = 2))
    WeekPhotosTop$haha_rate <- as.numeric(format(WeekPhotosTop$haha_rate*100, digits = 2))
    WeekPhotosTop$sad_rate <- as.numeric(format(WeekPhotosTop$sad_rate*100, digits = 2))
    WeekPhotosTop$angry_rate <- as.numeric(format(WeekPhotosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$feed_likes_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$love_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$wow_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$haha_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$sad_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$angry_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot72 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopBH()
    
    WeekPhotosTop$fan_rate <- as.numeric(format(WeekPhotosTop$fan_rate*100, digits = 4))
    WeekPhotosTop$viral_rate <- as.numeric(format(WeekPhotosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$fan_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$viral_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  
  # 4.2.4.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$BH_Table6 = DT::renderDataTable({
    
    WeekPhotosBottom <- WeekPhotoBottomBH()
    
    WeekPhotosBottom$post_reach <- format(WeekPhotosBottom$post_reach, big.mark = ",")
    WeekPhotosBottom$comment<- format(WeekPhotosBottom$comment, big.mark = ",")
    WeekPhotosBottom$like <- format(WeekPhotosBottom$like, big.mark = ",")
    WeekPhotosBottom$share <- format(WeekPhotosBottom$share, big.mark = ",")
    WeekPhotosBottom$sharetext <- paste0("<a href='",WeekPhotosBottom$permalink,"' target='_blank'>",WeekPhotosBottom$sharetext,"</a>")
    
    WeekPhotosBottom$ctr <- paste0(formatC(100 * WeekPhotosBottom$ctr, format = "f", digits = 2), "%")
    WeekPhotosBottom$interaction_rate <- paste0(formatC(100 * WeekPhotosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosBottom[, input$bh_show_vars6, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$BH_Plot8_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotosBH[which(DataPhotosBH$date >= input$dateRange1[1] & DataPhotosBH$date <= input$dateRange1[2]),]
    
    if(input$bh_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$bh_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeMemes) + 10 - bh_number_meme_bottom$n, nrow(DateRangeMemes))), " - ", as.character(max(nrow(DateRangeMemes) - bh_number_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeMemes))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$BH_Plot8 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomBH() 
    
    ds <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(name = ifelse(WeekPhotosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosBottom[x,]$permalink, color = ifelse(WeekPhotosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosBottom[x,]$full_picture, share_text = WeekPhotosBottom[x,]$sharetext, rank_reach = WeekPhotosBottom[x,]$rank_reach, general_rank_reach = WeekPhotosBottom[x,]$general_rank_reach,  rank_interactions = WeekPhotosBottom[x,]$rank_interactions,  general_rank_interactions = WeekPhotosBottom[x,]$general_rank_interactions, reach = WeekPhotosBottom[x,]$post_reach, interactions = WeekPhotosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosBottom[x,]$times_repost), 0, WeekPhotosBottom[x,]$times_repost), y = ifelse(input$bh_meme_select_plot_variable_bottom == "Reach", WeekPhotosBottom[x,]$post_reach, WeekPhotosBottom[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    input_plot_var <- input$bh_meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$BH_Plot81 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomBH() 
    
    WeekPhotosBottom$feed_likes_rate <- as.numeric(format(WeekPhotosBottom$feed_likes_rate*100, digits= 4))
    WeekPhotosBottom$love_rate <- as.numeric(format(WeekPhotosBottom$love_rate*100, digits = 2))
    WeekPhotosBottom$wow_rate <- as.numeric(format(WeekPhotosBottom$wow_rate*100, digits = 2))
    WeekPhotosBottom$haha_rate <- as.numeric(format(WeekPhotosBottom$haha_rate*100, digits = 2))
    WeekPhotosBottom$sad_rate <- as.numeric(format(WeekPhotosBottom$sad_rate*100, digits = 2))
    WeekPhotosBottom$angry_rate <- as.numeric(format(WeekPhotosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$feed_likes_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$love_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$wow_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$haha_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$sad_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$angry_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$BH_Plot82 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomBH() 
    
    WeekPhotosBottom$fan_rate <- as.numeric(format(WeekPhotosBottom$fan_rate*100, digits = 4))
    WeekPhotosBottom$viral_rate <- as.numeric(format(WeekPhotosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$fan_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$viral_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  #----------------------------------------------------------------------------------------------------------------------
  
  # 4.3. Top & Bottom Posts - Fierce ---------------------------------------------------------------------------------------------------------------------------
  
  # 4.3.1. Articles  ------------------------------------------------------------------------------------------------------
  
  # 4.3.1.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table1 = DT::renderDataTable({
    
    WeekLinksTop <- WeekLinksTopFC()
    
    WeekLinksTop$post_reach <- format( WeekLinksTop$post_reach, big.mark = ",")
    WeekLinksTop$link_clicks <- format( WeekLinksTop$link_clicks, big.mark = ",")
    WeekLinksTop$headline <- paste0("<a href='", WeekLinksTop$permalink,"' target='_blank'>", WeekLinksTop$headline,"</a>")
    WeekLinksTop$ctr <- paste0(formatC(100 * WeekLinksTop$ctr, format = "f", digits = 2), "%")
    WeekLinksTop$interaction_rate <- paste0(formatC(100 * WeekLinksTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksTop[, input$fc_show_vars1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot1_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    if(input$fc_article_select_original_repost_top == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$fc_article_select_original_repost_top == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(fc_number_article_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeArticles), fc_number_article_top$n)), " of ", as.character(nrow(DateRangeArticles)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot1 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopFC()
    
    ds <- lapply(1:nrow(WeekLinksTop), function(x){
      list(name = ifelse(WeekLinksTop[x,]$repost == 1, "Repost", "Original"), url = WeekLinksTop[x,]$permalink, color = ifelse(WeekLinksTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksTop[x,]$full_picture, share_text = WeekLinksTop[x,]$sharetext, link_clicks = WeekLinksTop[x,]$link_clicks, rank_link_clicks = WeekLinksTop[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksTop[x,]$general_rank_link_clicks, rank_reach = WeekLinksTop[x,]$rank_reach, general_rank_reach = WeekLinksTop[x,]$general_rank_reach,  rank_interactions = WeekLinksTop[x,]$rank_interactions,  general_rank_interactions = WeekLinksTop[x,]$general_rank_interactions, reach = WeekLinksTop[x,]$post_reach, interactions = WeekLinksTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksTop[x,]$times_repost), 0, WeekLinksTop[x,]$times_repost), post_category = WeekLinksTop[x,]$category, y = ifelse(input$fc_article_select_plot_variable_top == "Link Clicks", WeekLinksTop[x,]$link_clicks, ifelse(input$fc_article_select_plot_variable_top == "Reach", WeekLinksTop[x,]$post_reach, WeekLinksTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    input_plot_var <- input$fc_article_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$FC_Plot2 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksTop <- WeekLinksTopFC()
    
    WeekLinksTop$ctr <- as.numeric(format(WeekLinksTop$ctr*100, digits= 3))
    WeekLinksTop$interaction_rate <- as.numeric(format(WeekLinksTop$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$ctr, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$interaction_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)

  })
  
  output$FC_Plot21 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopFC()
    
    WeekLinksTop$feed_likes_rate <- as.numeric(format(WeekLinksTop$feed_likes_rate*100, digits= 4))
    WeekLinksTop$love_rate <- as.numeric(format(WeekLinksTop$love_rate*100, digits = 2))
    WeekLinksTop$wow_rate <- as.numeric(format(WeekLinksTop$wow_rate*100, digits = 2))
    WeekLinksTop$haha_rate <- as.numeric(format(WeekLinksTop$haha_rate*100, digits = 2))
    WeekLinksTop$sad_rate <- as.numeric(format(WeekLinksTop$sad_rate*100, digits = 2))
    WeekLinksTop$angry_rate <- as.numeric(format(WeekLinksTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$feed_likes_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$love_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$wow_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$haha_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$sad_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$angry_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot22 <- renderHighchart({
    
    WeekLinksTop <- WeekLinksTopFC()
    
    WeekLinksTop$fan_rate <- as.numeric(format(WeekLinksTop$fan_rate*100, digits = 4))
    WeekLinksTop$viral_rate <- as.numeric(format(WeekLinksTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$fan_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksTop), function(x){
      list(y = WeekLinksTop[x,]$viral_rate, url = WeekLinksTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksTop$headline, WeekLinksTop$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.3.1.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table2 = DT::renderDataTable({
    
    WeekLinksBottom <- WeekLinksBottomFC()
    
    WeekLinksBottom$post_reach <- format(WeekLinksBottom$post_reach, big.mark = ",")
    WeekLinksBottom$link_clicks <- format(WeekLinksBottom$link_clicks, big.mark = ",")
    WeekLinksBottom$headline <- paste0("<a href='",WeekLinksBottom$permalink,"' target='_blank'>",WeekLinksBottom$headline,"</a>")
    WeekLinksBottom$ctr <- paste0(formatC(100 * WeekLinksBottom$ctr, format = "f", digits = 2), "%")
    WeekLinksBottom$interaction_rate <- paste0(formatC(100 * WeekLinksBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksBottom[, input$fc_show_vars4, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot3_Title <- renderHighchart({
    
    DateRangeArticles <- DataArticlesFC[which(DataArticlesFC$date >= input$dateRange1[1] & DataArticlesFC$date <= input$dateRange1[2]),]
    
    if(input$fc_article_select_original_repost_bottom == "Originals"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 1),]
    }
    
    else if (input$fc_article_select_original_repost_bottom == "Reposts"){
      DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$original == 0),]
    }
    
    DateRangeArticles <- DateRangeArticles[which(DateRangeArticles$category %in% input$fc_article_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeArticles) + 10 - fc_number_article_bottom$n, nrow(DateRangeArticles))), " - ", as.character(max(nrow(DateRangeArticles) - fc_number_article_bottom$n + 1, 1)), " of ", as.character(nrow(DateRangeArticles)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot3 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomFC()
    
    ds <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(name = ifelse(WeekLinksBottom[x,]$repost == 1, "Repost", "Original"), url = WeekLinksBottom[x,]$permalink, color = ifelse(WeekLinksBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekLinksBottom[x,]$full_picture, share_text = WeekLinksBottom[x,]$sharetext, link_clicks = WeekLinksBottom[x,]$link_clicks, rank_link_clicks = WeekLinksBottom[x,]$rank_link_clicks, general_rank_link_clicks = WeekLinksBottom[x,]$general_rank_link_clicks, rank_reach = WeekLinksBottom[x,]$rank_reach, general_rank_reach = WeekLinksBottom[x,]$general_rank_reach,  rank_interactions = WeekLinksBottom[x,]$rank_interactions,  general_rank_interactions = WeekLinksBottom[x,]$general_rank_interactions, reach = WeekLinksBottom[x,]$post_reach, interactions = WeekLinksBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekLinksBottom[x,]$times_repost), 0, WeekLinksBottom[x,]$times_repost), post_category = WeekLinksBottom[x,]$category, y = ifelse(input$fc_article_select_plot_variable_bottom == "Link Clicks", WeekLinksBottom[x,]$link_clicks, ifelse(input$fc_article_select_plot_variable_bottom == "Reach", WeekLinksBottom[x,]$post_reach, WeekLinksBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    input_plot_var <- input$fc_article_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Link Clicks : <b>' + this.point.link_clicks + '</b>  | Rank: <b>' + this.point.rank_link_clicks + '</b> | Gen. Rank: <b>' + this.point.general_rank_link_clicks + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$FC_Plot4 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekLinksBottom <- WeekLinksBottomFC()
    
    WeekLinksBottom$ctr <- as.numeric(format(WeekLinksBottom$ctr*100, digits= 2))
    WeekLinksBottom$interaction_rate <- as.numeric(format(WeekLinksBottom$interaction_rate*100, digits = 2))
    
    ds_ctr <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$ctr, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_ir <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$interaction_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Rates_Function(ds_ctr, ds_ir, input_categories)

  })
  
  output$FC_Plot41 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomFC()
    
    WeekLinksBottom$feed_likes_rate <- as.numeric(format(WeekLinksBottom$feed_likes_rate*100, digits= 4))
    WeekLinksBottom$love_rate <- as.numeric(format(WeekLinksBottom$love_rate*100, digits = 2))
    WeekLinksBottom$wow_rate <- as.numeric(format(WeekLinksBottom$wow_rate*100, digits = 2))
    WeekLinksBottom$haha_rate <- as.numeric(format(WeekLinksBottom$haha_rate*100, digits = 2))
    WeekLinksBottom$sad_rate <- as.numeric(format(WeekLinksBottom$sad_rate*100, digits = 2))
    WeekLinksBottom$angry_rate <- as.numeric(format(WeekLinksBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$feed_likes_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$love_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$wow_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$haha_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$sad_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$angry_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot42 <- renderHighchart({
    
    WeekLinksBottom <- WeekLinksBottomFC()
    
    WeekLinksBottom$fan_rate <- as.numeric(format(WeekLinksBottom$fan_rate*100, digits = 4))
    WeekLinksBottom$viral_rate <- as.numeric(format(WeekLinksBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$fan_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekLinksBottom), function(x){
      list(y = WeekLinksBottom[x,]$viral_rate, url = WeekLinksBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekLinksBottom$headline, WeekLinksBottom$link_clicks)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.3.2. Videos  --------------------------------------------------------------------------------------------------------
  
  # 4.3.2.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table3 = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideosTopFC()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$fc_show_vars2, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot5_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    if(input$fc_video_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(fc_number_video_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), fc_number_video_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot5 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopFC()
    
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$fc_video_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$fc_video_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$fc_video_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$FC_Plot52 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopFC()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideosFC)), grep("s0", colnames(DataVideosFC)):grep("s40", colnames(DataVideosFC)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)

  })
  
  
  output$FC_Plot53 <- renderHighchart({
    
    cols=c("steelblue3","skyblue4")
    
    WeekVideosTop <- WeekVideosTopFC()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot54 <- renderHighchart({
    
    WeekVideosTop <- WeekVideosTopFC()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  
  # 4.3.2.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table4 = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideosBottomFC()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$fc_show_vars5, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot6_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 0),]
    
    if(input$fc_video_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - fc_number_video_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - fc_number_video_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot6 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomFC()
    
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$fc_video_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$fc_video_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$fc_video_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
    
  })
  
  output$FC_Plot62 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomFC()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideosFC)), grep("s0", colnames(DataVideosFC)):grep("s40", colnames(DataVideosFC)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)

  })
  
  output$FC_Plot63 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomFC()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot64 <- renderHighchart({
    
    WeekVideosBottom <- WeekVideosBottomFC()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.3.3. Video Memes ----------------------------------------------------------------------------------------------------
  
  # 4.3.3.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table3b = DT::renderDataTable({
    
    WeekVideosTop <- WeekVideoMemesTopFC()
    
    WeekVideosTop$post_video_views <- format(WeekVideosTop$post_video_views, big.mark = ",")
    WeekVideosTop$post_reach <- format(WeekVideosTop$post_reach, big.mark = ",")
    WeekVideosTop$comment<- format(WeekVideosTop$comment, big.mark = ",")
    WeekVideosTop$like <- format(WeekVideosTop$like, big.mark = ",")
    WeekVideosTop$share <- format(WeekVideosTop$share, big.mark = ",")
    WeekVideosTop$sharetext <- paste0("<a href='",WeekVideosTop$permalink,"' target='_blank'>",WeekVideosTop$sharetext,"</a>")
    
    WeekVideosTop$ctr <- paste0(formatC(100 * WeekVideosTop$ctr, format = "f", digits = 2), "%")
    WeekVideosTop$interaction_rate <- paste0(formatC(100 * WeekVideosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosTop[, input$fc_show_vars2b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot5b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    if(input$fc_video_meme_select_original_repost_top == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_top == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_top),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(fc_number_video_meme_top$n-4, 1)), " - ", as.character(min(nrow(DateRangeVideos), fc_number_video_meme_top$n)), " of ", as.character(nrow(DateRangeVideos)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot5b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopFC()
    
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(name = ifelse(WeekVideosTop[x,]$repost == 1, "Repost", "Original"), url = WeekVideosTop[x,]$permalink, color = ifelse(WeekVideosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosTop[x,]$full_picture, share_text = WeekVideosTop[x,]$sharetext, video_views = WeekVideosTop[x,]$post_video_views, rank_video_views = WeekVideosTop[x,]$rank_video_views, general_rank_video_views = WeekVideosTop[x,]$general_rank_video_views, rank_reach = WeekVideosTop[x,]$rank_reach, general_rank_reach = WeekVideosTop[x,]$general_rank_reach,  rank_interactions = WeekVideosTop[x,]$rank_interactions,  general_rank_interactions = WeekVideosTop[x,]$general_rank_interactions, reach = WeekVideosTop[x,]$post_reach, interactions = WeekVideosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosTop[x,]$times_repost), 0, WeekVideosTop[x,]$times_repost), post_category = WeekVideosTop[x,]$category, y = ifelse(input$fc_video_meme_select_plot_variable_top == "Video Views", WeekVideosTop[x,]$post_video_views, ifelse(input$fc_video_meme_select_plot_variable_top == "Reach", WeekVideosTop[x,]$post_reach, WeekVideosTop[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    input_plot_var <- input$fc_video_meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
 
  })
  
  output$FC_Plot52b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopFC()
    
    df <- WeekVideosTop[,c(4, grep("repost_order", colnames(DataVideosFC)), grep("s0", colnames(DataVideosFC)):grep("s40", colnames(DataVideosFC)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosTop), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
   
  })
  
  output$FC_Plot53b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopFC()
    
    WeekVideosTop$feed_likes_rate <- as.numeric(format(WeekVideosTop$feed_likes_rate*100, digits= 4))
    WeekVideosTop$love_rate <- as.numeric(format(WeekVideosTop$love_rate*100, digits = 2))
    WeekVideosTop$wow_rate <- as.numeric(format(WeekVideosTop$wow_rate*100, digits = 2))
    WeekVideosTop$haha_rate <- as.numeric(format(WeekVideosTop$haha_rate*100, digits = 2))
    WeekVideosTop$sad_rate <- as.numeric(format(WeekVideosTop$sad_rate*100, digits = 2))
    WeekVideosTop$angry_rate <- as.numeric(format(WeekVideosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$feed_likes_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$love_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$wow_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$haha_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$sad_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$angry_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot54b <- renderHighchart({
    
    WeekVideosTop <- WeekVideoMemesTopFC()
    
    WeekVideosTop$fan_rate <- as.numeric(format(WeekVideosTop$fan_rate*100, digits = 4))
    WeekVideosTop$viral_rate <- as.numeric(format(WeekVideosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$fan_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosTop), function(x){
      list(y = WeekVideosTop[x,]$viral_rate, url = WeekVideosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosTop$sharetext, WeekVideosTop$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.3.3.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table4b = DT::renderDataTable({
    
    WeekVideosBottom <- WeekVideoMemesBottomFC()
    
    WeekVideosBottom$post_video_views <- format(WeekVideosBottom$post_video_views, big.mark = ",")
    WeekVideosBottom$post_reach <- format(WeekVideosBottom$post_reach, big.mark = ",")
    WeekVideosBottom$comment<- format(WeekVideosBottom$comment, big.mark = ",")
    WeekVideosBottom$like <- format(WeekVideosBottom$like, big.mark = ",")
    WeekVideosBottom$share <- format(WeekVideosBottom$share, big.mark = ",")
    WeekVideosBottom$sharetext <- paste0("<a href='",WeekVideosBottom$permalink,"' target='_blank'>",WeekVideosBottom$sharetext,"</a>")
    
    WeekVideosBottom$ctr <- paste0(formatC(100 * WeekVideosBottom$ctr, format = "f", digits = 2), "%")
    WeekVideosBottom$interaction_rate <- paste0(formatC(100 * WeekVideosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosBottom[, input$fc_show_vars5b, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6))), dom = "ft"))
  
  output$FC_Plot6b_Title <- renderHighchart({
    
    DateRangeVideos <- DataVideosFC[which(DataVideosFC$date >= input$dateRange1[1] & DataVideosFC$date <= input$dateRange1[2] & DataVideosFC$video_meme == 1),]
    
    if(input$fc_video_meme_select_original_repost_bottom == "Originals"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 1),]
    }
    
    else if (input$fc_video_meme_select_original_repost_bottom == "Reposts"){
      DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$original == 0),]
    }
    
    DateRangeVideos <- DateRangeVideos[which(DateRangeVideos$category %in% input$fc_video_meme_select_categories_bottom),]
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeVideos) + 5 - fc_number_video_meme_bottom$n, nrow(DateRangeVideos))), " - ", as.character(max(nrow(DateRangeVideos) - fc_number_video_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeVideos))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot6b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomFC()
    
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(name = ifelse(WeekVideosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekVideosBottom[x,]$permalink, color = ifelse(WeekVideosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekVideosBottom[x,]$full_picture, share_text = WeekVideosBottom[x,]$sharetext, video_views = WeekVideosBottom[x,]$post_video_views, rank_video_views = WeekVideosBottom[x,]$rank_video_views, general_rank_video_views = WeekVideosBottom[x,]$general_rank_video_views, rank_reach = WeekVideosBottom[x,]$rank_reach, general_rank_reach = WeekVideosBottom[x,]$general_rank_reach,  rank_interactions = WeekVideosBottom[x,]$rank_interactions,  general_rank_interactions = WeekVideosBottom[x,]$general_rank_interactions, reach = WeekVideosBottom[x,]$post_reach, interactions = WeekVideosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekVideosBottom[x,]$times_repost), 0, WeekVideosBottom[x,]$times_repost), post_category = WeekVideosBottom[x,]$category, y = ifelse(input$fc_video_meme_select_plot_variable_bottom == "Video Views", WeekVideosBottom[x,]$post_video_views, ifelse(input$fc_video_meme_select_plot_variable_bottom == "Reach", WeekVideosBottom[x,]$post_reach, WeekVideosBottom[x,]$total_interactions)))
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    input_plot_var <- input$fc_video_meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Category: <b>' + this.point.post_category + '</b>' + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Video Views : <b>' + this.point.video_views + '</b>  | Rank: <b>' + this.point.rank_video_views + '</b> | Gen. Rank: <b>' + this.point.general_rank_video_views + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
   
  })
  
  output$FC_Plot62b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomFC()
    
    df <- WeekVideosBottom[,c(4, grep("repost_order", colnames(DataVideosFC)), grep("s0", colnames(DataVideosFC)):grep("s40", colnames(DataVideosFC)))]
    df[duplicated(df$sharetext),]$sharetext <- paste(df[duplicated(df$sharetext),]$sharetext, df[duplicated(df$sharetext),]$repost_order)
    df <- gather(df, "seconds_viewed", "percentage_viewed", 3:43)
    df <- dlply(df,"sharetext", function (x){list(data = x$percentage_viewed, name = unique(x$sharetext))})
    ds <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(data = df[[x]]$data, name = df[[x]]$name)
    })
    
    Plot_Video_Drop_Off_Function(ds)
    
  })
  
  output$FC_Plot63b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomFC()
    
    WeekVideosBottom$feed_likes_rate <- as.numeric(format(WeekVideosBottom$feed_likes_rate*100, digits= 4))
    WeekVideosBottom$love_rate <- as.numeric(format(WeekVideosBottom$love_rate*100, digits = 2))
    WeekVideosBottom$wow_rate <- as.numeric(format(WeekVideosBottom$wow_rate*100, digits = 2))
    WeekVideosBottom$haha_rate <- as.numeric(format(WeekVideosBottom$haha_rate*100, digits = 2))
    WeekVideosBottom$sad_rate <- as.numeric(format(WeekVideosBottom$sad_rate*100, digits = 2))
    WeekVideosBottom$angry_rate <- as.numeric(format(WeekVideosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$feed_likes_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$love_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$wow_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$haha_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$sad_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$angry_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot64b <- renderHighchart({
    
    WeekVideosBottom <- WeekVideoMemesBottomFC()
    
    WeekVideosBottom$fan_rate <- as.numeric(format(WeekVideosBottom$fan_rate*100, digits = 4))
    WeekVideosBottom$viral_rate <- as.numeric(format(WeekVideosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$fan_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekVideosBottom), function(x){
      list(y = WeekVideosBottom[x,]$viral_rate, url = WeekVideosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekVideosBottom$sharetext, WeekVideosBottom$post_video_views)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  # 4.3.4. Memes ----------------------------------------------------------------------------------------------------------
  
  # 4.3.4.1. Top ------------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table5 = DT::renderDataTable({
    
    WeekPhotosTop <- WeekPhotoTopFC()
    
    WeekPhotosTop$post_reach <- format(WeekPhotosTop$post_reach, big.mark = ",")
    WeekPhotosTop$comment<- format(WeekPhotosTop$comment, big.mark = ",")
    WeekPhotosTop$like <- format(WeekPhotosTop$like, big.mark = ",")
    WeekPhotosTop$share <- format(WeekPhotosTop$share, big.mark = ",")
    WeekPhotosTop$sharetext <- paste0("<a href='",WeekPhotosTop$permalink,"' target='_blank'>",WeekPhotosTop$sharetext,"</a>")
    
    WeekPhotosTop$ctr <- paste0(formatC(100 * WeekPhotosTop$ctr, format = "f", digits = 2), "%")
    WeekPhotosTop$interaction_rate <- paste0(formatC(100 * WeekPhotosTop$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosTop[, input$fc_show_vars3, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$FC_Plot7_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    
    if(input$fc_meme_select_original_repost_top == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_top == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    plot_subtitle <- paste(as.character(max(fc_number_meme_top$n-9, 1)), " - ", as.character(min(nrow(DateRangeMemes), fc_number_meme_top$n)), " of ", as.character(nrow(DateRangeMemes)))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot7 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopFC()
    
    ds <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(name = ifelse(WeekPhotosTop[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosTop[x,]$permalink, color = ifelse(WeekPhotosTop[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosTop[x,]$full_picture, share_text = WeekPhotosTop[x,]$sharetext, rank_reach = WeekPhotosTop[x,]$rank_reach, general_rank_reach = WeekPhotosTop[x,]$general_rank_reach,  rank_interactions = WeekPhotosTop[x,]$rank_interactions,  general_rank_interactions = WeekPhotosTop[x,]$general_rank_interactions, reach = WeekPhotosTop[x,]$post_reach, interactions = WeekPhotosTop[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosTop[x,]$times_repost), 0, WeekPhotosTop[x,]$times_repost), y = ifelse(input$fc_meme_select_plot_variable_top == "Reach", WeekPhotosTop[x,]$post_reach, WeekPhotosTop[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    input_plot_var <- input$fc_meme_select_plot_variable_top
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
 
  })
  
  output$FC_Plot71 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopFC()
    
    WeekPhotosTop$feed_likes_rate <- as.numeric(format(WeekPhotosTop$feed_likes_rate*100, digits= 4))
    WeekPhotosTop$love_rate <- as.numeric(format(WeekPhotosTop$love_rate*100, digits = 2))
    WeekPhotosTop$wow_rate <- as.numeric(format(WeekPhotosTop$wow_rate*100, digits = 2))
    WeekPhotosTop$haha_rate <- as.numeric(format(WeekPhotosTop$haha_rate*100, digits = 2))
    WeekPhotosTop$sad_rate <- as.numeric(format(WeekPhotosTop$sad_rate*100, digits = 2))
    WeekPhotosTop$angry_rate <- as.numeric(format(WeekPhotosTop$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$feed_likes_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$love_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$wow_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$haha_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$sad_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$angry_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot72 <- renderHighchart({
    
    WeekPhotosTop <- WeekPhotoTopFC()
    
    WeekPhotosTop$fan_rate <- as.numeric(format(WeekPhotosTop$fan_rate*100, digits = 4))
    WeekPhotosTop$viral_rate <- as.numeric(format(WeekPhotosTop$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$fan_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosTop), function(x){
      list(y = WeekPhotosTop[x,]$viral_rate, url = WeekPhotosTop[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosTop$sharetext, WeekPhotosTop$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  # 4.3.4.2. Bottom ---------------------------------------------------------------------------------------------------------------------
  
  output$FC_Table6 = DT::renderDataTable({
    
    WeekPhotosBottom <- WeekPhotoBottomFC()
    
    WeekPhotosBottom$post_reach <- format(WeekPhotosBottom$post_reach, big.mark = ",")
    WeekPhotosBottom$comment<- format(WeekPhotosBottom$comment, big.mark = ",")
    WeekPhotosBottom$like <- format(WeekPhotosBottom$like, big.mark = ",")
    WeekPhotosBottom$share <- format(WeekPhotosBottom$share, big.mark = ",")
    WeekPhotosBottom$sharetext <- paste0("<a href='",WeekPhotosBottom$permalink,"' target='_blank'>",WeekPhotosBottom$sharetext,"</a>")
    
    WeekPhotosBottom$ctr <- paste0(formatC(100 * WeekPhotosBottom$ctr, format = "f", digits = 2), "%")
    WeekPhotosBottom$interaction_rate <- paste0(formatC(100 * WeekPhotosBottom$interaction_rate, format = "f", digits = 2), "%")
    
    WeekPhotosBottom[, input$fc_show_vars6, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5))), dom = "ft"))
  
  output$FC_Plot8_Title <- renderHighchart({
    
    DateRangeMemes <- DataPhotosFC[which(DataPhotosFC$date >= input$dateRange1[1] & DataPhotosFC$date <= input$dateRange1[2]),]
    
    if(input$fc_meme_select_original_repost_bottom == "Originals"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 1),]
    }
    
    else if (input$fc_meme_select_original_repost_bottom == "Reposts"){
      DateRangeMemes <- DateRangeMemes[which(DateRangeMemes$original == 0),]
    }
    
    plot_title <- paste("From ", as.character(format(as.Date(input$dateRange1[1])), "%b%d"), " to ", as.character(input$dateRange1[2]))
    
    plot_subtitle <- paste(as.character(min(nrow(DateRangeMemes) + 10 - fc_number_meme_bottom$n, nrow(DateRangeMemes))), " - ", as.character(max(nrow(DateRangeMemes) - fc_number_meme_bottom$n + 1, 1)), " of ", nrow(DateRangeMemes))
    
    hc <- highchart() %>% 
      hc_chart(type = "bar", height = 450) %>%
      hc_title(text = plot_title, align = "center") %>%
      hc_subtitle(text = plot_subtitle, align = "center")  %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  output$FC_Plot8 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomFC() 
    
    ds <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(name = ifelse(WeekPhotosBottom[x,]$repost == 1, "Repost", "Original"), url = WeekPhotosBottom[x,]$permalink, color = ifelse(WeekPhotosBottom[x,]$repost == 1, "#2580B9", "#D55200"), photo_url = WeekPhotosBottom[x,]$full_picture, share_text = WeekPhotosBottom[x,]$sharetext, rank_reach = WeekPhotosBottom[x,]$rank_reach, general_rank_reach = WeekPhotosBottom[x,]$general_rank_reach,  rank_interactions = WeekPhotosBottom[x,]$rank_interactions,  general_rank_interactions = WeekPhotosBottom[x,]$general_rank_interactions, reach = WeekPhotosBottom[x,]$post_reach, interactions = WeekPhotosBottom[x,]$total_interactions, times_repo = ifelse(is.na(WeekPhotosBottom[x,]$times_repost), 0, WeekPhotosBottom[x,]$times_repost), y = ifelse(input$fc_meme_select_plot_variable_bottom == "Reach", WeekPhotosBottom[x,]$post_reach, WeekPhotosBottom[x,]$total_interactions))
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    input_plot_var <- input$fc_meme_select_plot_variable_bottom
    
    fntltp <- JS(paste("function(){
                       return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
                       "}"))
    
    Plot_Top_Bottom_Function(ds, input_categories, input_plot_var, fntltp)
  
  })
  
  output$FC_Plot81 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomFC() 
    
    WeekPhotosBottom$feed_likes_rate <- as.numeric(format(WeekPhotosBottom$feed_likes_rate*100, digits= 4))
    WeekPhotosBottom$love_rate <- as.numeric(format(WeekPhotosBottom$love_rate*100, digits = 2))
    WeekPhotosBottom$wow_rate <- as.numeric(format(WeekPhotosBottom$wow_rate*100, digits = 2))
    WeekPhotosBottom$haha_rate <- as.numeric(format(WeekPhotosBottom$haha_rate*100, digits = 2))
    WeekPhotosBottom$sad_rate <- as.numeric(format(WeekPhotosBottom$sad_rate*100, digits = 2))
    WeekPhotosBottom$angry_rate <- as.numeric(format(WeekPhotosBottom$angry_rate*100, digits = 2))
    
    ds_feed_likes_rate <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$feed_likes_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_love <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$love_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_wow <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$wow_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_haha <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$haha_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_sad <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$sad_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_angry <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$angry_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Article_Reaction_Rates_Function(ds_feed_likes_rate, ds_love, ds_wow, ds_haha, ds_sad, ds_angry, input_categories)
    
  })
  
  output$FC_Plot82 <- renderHighchart({
    
    WeekPhotosBottom <- WeekPhotoBottomFC() 
    
    WeekPhotosBottom$fan_rate <- as.numeric(format(WeekPhotosBottom$fan_rate*100, digits = 4))
    WeekPhotosBottom$viral_rate <- as.numeric(format(WeekPhotosBottom$viral_rate*100, digits = 4))
    
    ds_fan <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$fan_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    ds_viral <- lapply(1:nrow(WeekPhotosBottom), function(x){
      list(y = WeekPhotosBottom[x,]$viral_rate, url = WeekPhotosBottom[x,]$permalink)
    })
    
    input_categories <- reorder(WeekPhotosBottom$sharetext, WeekPhotosBottom$post_reach)
    
    Plot_Fan_Viral_Function(ds_fan, ds_viral, input_categories)
    
  })
  
  #----------------------------------------------------------------------------------------------------------------------
  
  
  # 5. Authors Performance ---------------------------------------------------------------------------------------------------------
  
  # 5.1 Authors Performance - We Are Mitú ---------------------------------------------------------------------------------------------------------
  
  # 5.1.1. Week -----------------------------------------------------------------------------------------------------------------------
  
  output$PlotAuthors1 <- renderHighchart({
    
    AuthorsWeek <- AuthorsWeek[1:20,]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = reorder(AuthorsWeek$Author, AuthorsWeek$Visitors)) %>% 
      hc_yAxis(title = list(text = "Visitors"))%>% 
      hc_add_series(data = c(AuthorsWeek$Visitors), name = "Visitors")%>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  # 5.1.2. Month ----------------------------------------------------------------------------------------------------------------------
  
  output$PlotAuthors2 <- renderHighchart({
    
    AuthorsMonth <- AuthorsMonth[1:20,]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = reorder(AuthorsMonth$Author, AuthorsMonth$Visitors)) %>% 
      hc_yAxis(title = list(text = "Visitors"))%>% 
      hc_add_series(data = c(AuthorsMonth$Visitors), name = "Visitors")%>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  #----------------------------------------------------------------------------------------------------------------------
  
  
  # 6. Categories ----------------------------------------------------------------------------------------------------------
  
  # 6.1. Categories - We Are Mitú ---------------------------------------------------------------------------------------------------------
  
  
  # 6.1.1. Articles 
  # 6.1.1.1. Overview --------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesOverview1 <- renderHighchart({
    
    input$plot_categories
    isolate({    
     
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      
      animals <- merge(dates, DataArticles[which(DataArticles$category == "Animals"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      animals <- as.xts(animals[,"link_clicks"], order.by = animals[,"created_time"])
      
      beauty <- merge(dates, DataArticles[which(DataArticles$category == "Beauty & Fashion"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      beauty <- as.xts(beauty[,"link_clicks"], order.by = beauty[,"created_time"])
      
      celeb <- merge(dates, DataArticles[which(DataArticles$category == "Celebrity & Gossip"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      celeb <- as.xts(celeb[,"link_clicks"], order.by = celeb[,"created_time"])

      citizen <- merge(dates, DataArticles[which(DataArticles$category == "Citizenship & Politics"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      citizen <- as.xts(citizen[,"link_clicks"], order.by = citizen[,"created_time"])
      
      family <- merge(dates, DataArticles[which(DataArticles$category == "Family & Friends"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      family <- as.xts(family[,"link_clicks"], order.by = family[,"created_time"])

      food <- merge(dates, DataArticles[which(DataArticles$category == "Food & Beverages"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      food <- as.xts(food[,"link_clicks"], order.by = food[,"created_time"])
      
      identities <- merge(dates, DataArticles[which(DataArticles$category == "Identities"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      identities <- as.xts(identities[,"link_clicks"], order.by = identities[,"created_time"])

      latino <- merge(dates, DataArticles[which(DataArticles$category == "Latino Culture"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      latino <- as.xts(latino[,"link_clicks"], order.by = latino[,"created_time"])
      
      music <- merge(dates, DataArticles[which(DataArticles$category == "Music"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      music <- as.xts(music[,"link_clicks"], order.by = music[,"created_time"])
      
      news <- merge(dates, DataArticles[which(DataArticles$category == "News & Issues"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      news <- as.xts(news[,"link_clicks"], order.by = news[,"created_time"])
      
      sensat <- merge(dates, DataArticles[which(DataArticles$category == "Sensationalist"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      sensat <- as.xts(sensat[,"link_clicks"], order.by = sensat[,"created_time"])
      
      sex <- merge(dates, DataArticles[which(DataArticles$category == "Sex & Relationships"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      sex <- as.xts(sex[,"link_clicks"], order.by = sex[,"created_time"])
      
      sports <- merge(dates, DataArticles[which(DataArticles$category == "Sports"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      sports <- as.xts(sports[,"link_clicks"], order.by = sports[,"created_time"])
      
      tv <- merge(dates, DataArticles[which(DataArticles$category == "TV & Movies"), c("created_time","link_clicks")], by = "created_time", all = TRUE)
      tv <- as.xts(tv[,"link_clicks"], order.by = tv[,"created_time"])
      
      colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Link Clicks")) %>% 
        hc_add_series(animals, name = "Animals", type = input$chart_type, visible = ifelse("Animals" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type, visible = ifelse("Beauty & Fashion" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type, visible = ifelse("Citizenship & Politics" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type, visible = ifelse("Family & Friends" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type, visible = ifelse("Food & Beverages" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type, visible = ifelse("Identities" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type, visible = ifelse("Latino Culture" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type, visible = ifelse("Music" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type, visible = ifelse("News & Issues" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(sensat, name = "Sensationalist", type = input$chart_type, visible = ifelse("Sensationalist" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type, visible = ifelse("Sex & Relationships" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type, visible = ifelse("Sports" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type, visible = ifelse("TV & Movies" %in% input$category_buttons, TRUE, FALSE)) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type == "area" || input$chart_type == "column"){if(input$chart_stack != "none"){input$chart_stack}}, dataGrouping = list(approximation = input$chart_avg_total, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$PlotCategoriesOverview2 <- renderHighchart({
    
    input$plot_categories2
    isolate({    
      AvgDaily <- ddply(DataArticles, c("category", "date"), summarise, AvgDailyLinkClicks = sum(na.omit(post_reach)))
      
      dates <- data.frame(date = unique(DataArticles$date))
      dates$date <- as.Date(dates$date)
      
      animals <- merge(dates, AvgDaily[which(AvgDaily$category == "Animals"),], by = "date", all = TRUE) 
      animals$category <- NULL
      row.names(animals) <- animals$date
      animals$date <- NULL
      animals <- as.xts(animals) 
      
      beauty <- merge(dates, AvgDaily[which(AvgDaily$category == "Beauty & Fashion"),], by = "date", all = TRUE)                  
      beauty$category <- NULL
      row.names(beauty) <- beauty$date
      beauty$date <- NULL              
      beauty <- as.xts(beauty)   
      
      celeb <- merge(dates, AvgDaily[which(AvgDaily$category == "Celebrity & Gossip"),], by = "date", all = TRUE)                  
      celeb$category <- NULL
      row.names(celeb) <- celeb$date
      celeb$date <- NULL               
      celeb <- as.xts(celeb)   
      
      citizen <- merge(dates, AvgDaily[which(AvgDaily$category == "Citizenship & Politics"),], by = "date", all = TRUE)               
      citizen$category <- NULL
      row.names(citizen) <- citizen$date
      citizen$date <- NULL               
      citizen <- as.xts(citizen) 
      
      family <- merge(dates, AvgDaily[which(AvgDaily$category == "Family & Friends"),], by = "date", all = TRUE)                  
      family$category <- NULL
      row.names(family) <- family$date
      family$date <- NULL               
      family <- as.xts(family) 
      
      food <- merge(dates, AvgDaily[which(AvgDaily$category == "Food & Beverages"),], by = "date", all = TRUE)                  
      food$category <- NULL
      row.names(food) <- food$date
      food$date <- NULL             
      food <- as.xts(food) 
      
      identities <- merge(dates, AvgDaily[which(AvgDaily$category == "Identities"),], by = "date", all = TRUE)               
      identities$category <- NULL
      row.names(identities) <- identities$date
      identities$date <- NULL             
      identities <- as.xts(identities) 
      
      latino <- merge(dates, AvgDaily[which(AvgDaily$category == "Latino Culture"),], by = "date", all = TRUE)                  
      latino$category <- NULL
      row.names(latino) <- latino$date
      latino$date <- NULL                
      latino <- as.xts(latino) 
      
      music <- merge(dates, AvgDaily[which(AvgDaily$category == "Music"),], by = "date", all = TRUE)                  
      music$category <- NULL
      row.names(music) <- music$date
      music$date <- NULL                
      music <- as.xts(music) 
      
      news <- merge(dates, AvgDaily[which(AvgDaily$category == "News & Issues"),], by = "date", all = TRUE)                  
      news$category <- NULL
      row.names(news) <- news$date
      news$date <- NULL               
      news <- as.xts(news) 
      
      sensat <- merge(dates, AvgDaily[which(AvgDaily$category == "Sensationalist"),], by = "date", all = TRUE)                  
      sensat$category <- NULL
      row.names(sensat) <- sensat$date
      sensat$date <- NULL                
      sensat <- as.xts(sensat) 
      
      sex <- merge(dates, AvgDaily[which(AvgDaily$category == "Sex & Relationships"),], by = "date", all = TRUE)                  
      sex$category <- NULL
      row.names(sex) <- sex$date
      sex$date <- NULL                
      sex <- as.xts(sex) 
      
      sports <- merge(dates, AvgDaily[which(AvgDaily$category == "Sports"),], by = "date", all = TRUE)                  
      sports$category <- NULL
      row.names(sports) <- sports$date
      sports$date <- NULL               
      sports <- as.xts(sports) 
      
      tv <- merge(dates, AvgDaily[which(AvgDaily$category == "TV & Movies"),], by = "date", all = TRUE)                  
      tv$category <- NULL
      row.names(tv) <- tv$date
      tv$date <- NULL                
      tv <- as.xts(tv) 
      
      colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(animals, name = "Animals", type = input$chart_type2, visible = ifelse("Animals" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type2, visible = ifelse("Beauty & Fashion" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type2, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type2, visible = ifelse("Citizenship & Politics" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type2, visible = ifelse("Family & Friends" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type2, visible = ifelse("Food & Beverages" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type2, visible = ifelse("Identities" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type2, visible = ifelse("Latino Culture" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type2, visible = ifelse("Music" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type2, visible = ifelse("News & Issues" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(sensat, name = "Sensationalist", type = input$chart_type2, visible = ifelse("Sensationalist" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type2, visible = ifelse("Sex & Relationships" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type2, visible = ifelse("Sports" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type2, visible = ifelse("TV & Movies" %in% input$category_buttons2, TRUE, FALSE)) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time2 == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type2 == "area" || input$chart_type2 == "column"){if(input$chart_stack2 != "none"){input$chart_stack2}}, dataGrouping = list(approximation = input$chart_avg_total2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  # 6.1.1.2. Top Days --------------------------------------------------------------------------------------------------------
  
  output$PlotCategories4 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    # WeekLinks <- DataArticles[which(DataArticles$date >= "2017-06-08" & DataArticles$date <= "2017-06-14"),]
    
    Catdf <- ddply(WeekLinks, "date", summarize, AnimalsLC = sum(ifelse(category == "Animals", link_clicks, 0)), BeautyLC = sum(ifelse(category == "Beauty & Fashion", link_clicks, 0)), CelebLC = sum(ifelse(category == "Celebrity & Gossip", link_clicks, 0)), CitiPoliticsLC = sum(ifelse(category == "Citizenship & Politics", link_clicks, 0)), FamilyLC = sum(ifelse(category == "Family & Friends", link_clicks, 0)), FoodLC = sum(ifelse(category == "Food & Beverages", link_clicks, 0)), IdentityLC = sum(ifelse(category == "Identities", link_clicks, 0)), LatinoLC = sum(ifelse(category == "Latino Culture", link_clicks, 0)), MusicLC = sum(ifelse(category == "Music", link_clicks, 0)), NewsLC = sum(ifelse(category == "News & Issues", link_clicks, 0)), OtherLC = sum(ifelse(category == "Other", link_clicks, 0)), SensationLC = sum(ifelse(category == "Sensationalist", link_clicks, 0)), SexLC = sum(ifelse(category == "Sex & Relationships", link_clicks, 0)), SportsLC = sum(ifelse(category == "Sports", link_clicks, 0)), TvLC = sum(ifelse(category == "TV & Movies", link_clicks, 0)), TotalLC = sum(link_clicks))
    
    Catdf <- Catdf[order(Catdf$TotalLC  , decreasing = TRUE),]
    Catdf <- Catdf[1:min(nrow(Catdf),5), -17]
    
    Catdfg <- gather(Catdf, "date", "link_clicks", 2:16)
    names(Catdfg) <- c("date", "category", "y")
    
    Catdfs <- spread(Catdfg, "date", "y")
    Catdfs$category <- as.character(unique(DataArticles[order(DataArticles$category, decreasing = FALSE),]$category))
    Catdfs <- Catdfs[,c("category",as.character(Catdf$date))]
    
    colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4', '#808080', '#1D91C0','#225EA8','#253494','#081D58')
    
    ds <- lapply(1:nrow(Catdfs), function(x){
      
      list(name = as.character(Catdfs[x,]$category), 
           
           data = lapply(2:ncol(Catdfs), function(y){
             
             list(name = as.character(names(Catdfs[y])), y = Catdfs[x,y], drilldown = as.character(names(Catdfs[y])))}
             
           ))
    })
    
    drilldown <- lapply(1:nrow(Catdf), function(x){
      
      list(name = "Link Clicks", id = as.character(Catdf[x,]$date), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]), function(y){
             
             WeekLinksDay <- WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]
             WeekLinksDay <- WeekLinksDay[order(WeekLinksDay$link_clicks, decreasing = TRUE),]
             CatColors <- data.frame(category = as.character(unique(DataArticles[order(DataArticles$category, decreasing = FALSE),]$category)), color = colores)
             
             list(name = WeekLinksDay[y,]$headline, y = WeekLinksDay[y,]$link_clicks, url = WeekLinksDay[y,]$permalink, 
                  color = as.character(CatColors[which(CatColors$category == WeekLinksDay[y,]$category),]$color))}
             
           ))
    })
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black"), series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
      hc_xAxis(type = "category", labels = list(formatter = JS("function () {formatted = this.value.length > 10? this.value.substring(0,10) + '...' : this.value;return formatted;}"))) %>% 
      hc_yAxis(title = list(text = "Link Clicks")) %>% 
      hc_add_series_list(ds) %>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.1.3. Pie -------------------------------------------------------------------------------------------------------------
  
  output$PlotCategories1 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    Catdfm <- as.data.frame(table(WeekLinks$category))
    colnames(Catdfm) <- c("category", "Freq")
    
    colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58')
    
    ds <- lapply(1:nrow(Catdfm), function(x){
      list(name = Catdfm[x,]$category, y = Catdfm[x,]$Freq, drilldown = as.character(Catdfm[x,]$category))
    })
    
    drilldown <- lapply(1:nrow(WeekLinks), function(x){
      
      list(name = "Link Clicks" , id = as.character(Catdfm[x,]$category), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]), function(y){
             
             WeekLinksCat <- WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]
             WeekLinksCat <- WeekLinksCat[order(WeekLinksCat$link_clicks, decreasing = TRUE),]
             
             list(name = WeekLinksCat[y,]$headline, y = WeekLinksCat[y,]$link_clicks, url = WeekLinksCat[y,]$permalink)}
             
           ))
    })
    
    colorCount = length(unique(DataArticles$category))
    
    hc <- highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), pie = list(borderColor = "black", dataLabels = list(formatter = JS("function () {formatted = this.point.name.length > 25? this.point.name.substring(0,25) + '...' : this.point.name;return formatted;}"), style = list(width = "100px"))))%>%
      hc_add_series(data = ds, name = "Number of Articles")%>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  # 6.1.1.4. Reach & Clicks --------------------------------------------------------------------------------------------------
  
  output$PlotCategories2 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgCTR = mean(na.omit(ctr)), AvgIR = mean(na.omit(interaction_rate)), AvgReach = mean(na.omit(post_reach)), AvgLinkClicks = mean(na.omit(link_clicks)))
    
    Catdfm1$AvgCTR <- as.numeric(format(Catdfm1$AvgCTR*100, digits= 3))
    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 3))
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgLinkClicks <- as.numeric(formatC(Catdfm1$AvgLinkClicks, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgLinkClicks, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "") %>% 
      hc_add_series(data = Catdfm1$AvgReach, name = "Avg. Reach") %>% 
      hc_add_series(data = Catdfm1$AvgLinkClicks, name = "Avg. Link Clicks") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
  
  # 6.1.1.5. CTR & IR --------------------------------------------------------------------------------------------------------
  
  output$PlotCategories3 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgCTR = mean(na.omit(ctr)), AvgIR = mean(na.omit(interaction_rate)), AvgReach = mean(na.omit(post_reach)), AvgLinkClicks = mean(na.omit(link_clicks)))
    
    Catdfm1$AvgCTR <- as.numeric(format(Catdfm1$AvgCTR*100, digits= 2))
    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 2))
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgLinkClicks <- as.numeric(formatC(Catdfm1$AvgLinkClicks, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgLinkClicks, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %")) %>% 
      hc_add_series(data = Catdfm1$AvgCTR, name = "Avg. CTR") %>% 
      hc_add_series(data = Catdfm1$AvgIR, name = "Avg. IR") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b><br/>", valueSuffix = "%") %>%
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  
  # 6.1.2. Videos ------------------------------------------------------------------------------------------------------------
  
  # 6.1.2.1. Overview --------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoOverview1 <- renderHighchart({
    
    input$plot_categories_video
    isolate({    
     
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      animals <- merge(dates, DataVideos[which(DataVideos$category == "Animals" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      animals <- as.xts(animals[,"post_video_views"], order.by = animals[,"created_time"])
      
      beauty <- merge(dates, DataVideos[which(DataVideos$category == "Beauty & Fashion" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      beauty <- as.xts(beauty[,"post_video_views"], order.by = beauty[,"created_time"])
      
      celeb <- merge(dates, DataVideos[which(DataVideos$category == "Celebrity & Gossip" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      celeb <- as.xts(celeb[,"post_video_views"], order.by = celeb[,"created_time"])
      
      citizen <- merge(dates, DataVideos[which(DataVideos$category == "Citizenship & Politics" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      citizen <- as.xts(citizen[,"post_video_views"], order.by = citizen[,"created_time"])
      
      diy <- merge(dates, DataVideos[which(DataVideos$category == "DIY" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      diy <- as.xts(diy[,"post_video_views"], order.by = diy[,"created_time"])
      
      family <- merge(dates, DataVideos[which(DataVideos$category == "Family & Friend" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      family <- as.xts(family[,"post_video_views"], order.by = family[,"created_time"])
      
      food <- merge(dates, DataVideos[which(DataVideos$category == "Food & Beverages" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      food <- as.xts(food[,"post_video_views"], order.by = food[,"created_time"])
      
      identities <- merge(dates, DataVideos[which(DataVideos$category == "Identities" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      identities <- as.xts(identities[,"post_video_views"], order.by = identities[,"created_time"])
      
      latino <- merge(dates, DataVideos[which(DataVideos$category == "Latino Culture" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      latino <- as.xts(latino[,"post_video_views"], order.by = latino[,"created_time"])
      
      live <- merge(dates, DataVideos[which(DataVideos$category == "Live" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      live <- as.xts(live[,"post_video_views"], order.by = live[,"created_time"])
      
      music <- merge(dates, DataVideos[which(DataVideos$category == "Music" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      music <- as.xts(music[,"post_video_views"], order.by = music[,"created_time"])
      
      news <- merge(dates, DataVideos[which(DataVideos$category == "News & Issues" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      news <- as.xts(news[,"post_video_views"], order.by = news[,"created_time"])
      
      others <- merge(dates, DataVideos[which(DataVideos$category == "Others" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      others <- as.xts(others[,"post_video_views"], order.by = others[,"created_time"])
      
      sensat <- merge(dates, DataVideos[which(DataVideos$category == "Sensationalist" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sensat <- as.xts(sensat[,"post_video_views"], order.by = sensat[,"created_time"])
      
      sex <- merge(dates, DataVideos[which(DataVideos$category == "Sex & Relationships" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sex <- as.xts(sex[,"post_video_views"], order.by = sex[,"created_time"])
      
      sports <- merge(dates, DataVideos[which(DataVideos$category == "Sports" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sports <- as.xts(sports[,"post_video_views"], order.by = sports[,"created_time"])
      
      tv <- merge(dates, DataVideos[which(DataVideos$category == "TV & Movies" & DataVideos$video_meme == 0), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      tv <- as.xts(tv[,"post_video_views"], order.by = tv[,"created_time"])
      
      colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Video Views")) %>% 
        
        hc_add_series(animals, name = "Animals", type = input$chart_type_video, visible = ifelse("Animals" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type_video, visible = ifelse("Beauty & Fashion" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type_video, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type_video, visible = ifelse("Citizenship & Politics" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(diy, name = "DIY", type = input$chart_type_video, visible = ifelse("DIY" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type_video, visible = ifelse("Family & Friends" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type_video, visible = ifelse("Food & Beverages" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type_video, visible = ifelse("Identities" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type_video, visible = ifelse("Latino Culture" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(live, name = "Live", type = input$chart_type_video, visible = ifelse("Live" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type_video, visible = ifelse("Music" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type_video, visible = ifelse("News & Issues" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(others, name = "Others", type = input$chart_type_video, visible = ifelse("Others" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type_video, visible = ifelse("Sex & Relationships" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type_video, visible = ifelse("Sports" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type_video, visible = ifelse("TV & Movies" %in% input$category_buttons_video, TRUE, FALSE)) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_video == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_video == "area" || input$chart_type_video == "column"){if(input$chart_stack_video != "none"){input$chart_stack_video}}, dataGrouping = list(approximation = input$chart_avg_total_video, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_video, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$PlotCategoriesVideoOverview2 <- renderHighchart({
    
    input$plot_categories_video2
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      animals <- merge(dates, DataVideos[which(DataVideos$category == "Animals" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      animals <- as.xts(animals[,"post_reach"], order.by = animals[,"created_time"])
      
      beauty <- merge(dates, DataVideos[which(DataVideos$category == "Beauty & Fashion" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      beauty <- as.xts(beauty[,"post_reach"], order.by = beauty[,"created_time"])
      
      celeb <- merge(dates, DataVideos[which(DataVideos$category == "Celebrity & Gossip" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      celeb <- as.xts(celeb[,"post_reach"], order.by = celeb[,"created_time"])
      
      citizen <- merge(dates, DataVideos[which(DataVideos$category == "Citizenship & Politics" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      citizen <- as.xts(citizen[,"post_reach"], order.by = citizen[,"created_time"])
      
      diy <- merge(dates, DataVideos[which(DataVideos$category == "DIY" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      diy <- as.xts(diy[,"post_reach"], order.by = diy[,"created_time"])
      
      family <- merge(dates, DataVideos[which(DataVideos$category == "Family & Friend" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      family <- as.xts(family[,"post_reach"], order.by = family[,"created_time"])
      
      food <- merge(dates, DataVideos[which(DataVideos$category == "Food & Beverages" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      food <- as.xts(food[,"post_reach"], order.by = food[,"created_time"])
      
      identities <- merge(dates, DataVideos[which(DataVideos$category == "Identities" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      identities <- as.xts(identities[,"post_reach"], order.by = identities[,"created_time"])
      
      latino <- merge(dates, DataVideos[which(DataVideos$category == "Latino Culture" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      latino <- as.xts(latino[,"post_reach"], order.by = latino[,"created_time"])
      
      live <- merge(dates, DataVideos[which(DataVideos$category == "Live" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      live <- as.xts(live[,"post_reach"], order.by = live[,"created_time"])
      
      music <- merge(dates, DataVideos[which(DataVideos$category == "Music" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      music <- as.xts(music[,"post_reach"], order.by = music[,"created_time"])
      
      news <- merge(dates, DataVideos[which(DataVideos$category == "News & Issues" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      news <- as.xts(news[,"post_reach"], order.by = news[,"created_time"])
      
      others <- merge(dates, DataVideos[which(DataVideos$category == "Others" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      others <- as.xts(others[,"post_reach"], order.by = others[,"created_time"])
      
      sensat <- merge(dates, DataVideos[which(DataVideos$category == "Sensationalist" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sensat <- as.xts(sensat[,"post_reach"], order.by = sensat[,"created_time"])
      
      sex <- merge(dates, DataVideos[which(DataVideos$category == "Sex & Relationships" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sex <- as.xts(sex[,"post_reach"], order.by = sex[,"created_time"])
      
      sports <- merge(dates, DataVideos[which(DataVideos$category == "Sports" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sports <- as.xts(sports[,"post_reach"], order.by = sports[,"created_time"])
      
      tv <- merge(dates, DataVideos[which(DataVideos$category == "TV & Movies" & DataVideos$video_meme == 0), c("created_time","post_reach")], by = "created_time", all = TRUE)
      tv <- as.xts(tv[,"post_reach"], order.by = tv[,"created_time"])
      
      colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        
        hc_add_series(animals, name = "Animals", type = input$chart_type_video2, visible = ifelse("Animals" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type_video2, visible = ifelse("Beauty & Fashion" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type_video2, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type_video2, visible = ifelse("Citizenship & Politics" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(diy, name = "DIY", type = input$chart_type_video2, visible = ifelse("DIY" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type_video2, visible = ifelse("Family & Friends" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type_video2, visible = ifelse("Food & Beverages" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type_video2, visible = ifelse("Identities" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type_video2, visible = ifelse("Latino Culture" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(live, name = "Live", type = input$chart_type_video2, visible = ifelse("Live" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type_video2, visible = ifelse("Music" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type_video2, visible = ifelse("News & Issues" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(others, name = "Others", type = input$chart_type_video2, visible = ifelse("Others" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type_video2, visible = ifelse("Sex & Relationships" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type_video2, visible = ifelse("Sports" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type_video2, visible = ifelse("TV & Movies" %in% input$category_buttons_video2, TRUE, FALSE)) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_video2 == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_video2 == "area" || input$chart_type_video2 == "column"){if(input$chart_stack_video2 != "none"){input$chart_stack_video2}}, dataGrouping = list(approximation = input$chart_avg_total_video2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_video2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  # 6.1.2.2. Top Days --------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideo4 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    
    Catdf <- ddply(WeekLinks, "date", summarize, AnimalsVV = sum(ifelse(category == "Animals", post_video_views, 0)), BeautyVV = sum(ifelse(category == "Beauty & Fashion", post_video_views, 0)), CelebVV = sum(ifelse(category == "Celebrity & Gossip", post_video_views, 0)), CitiPoliticsVV = sum(ifelse(category == "Citizenship & Politics", post_video_views, 0)), DIYVV = sum(ifelse(category == "DIY", post_video_views, 0)), FamilyVV = sum(ifelse(category == "Family & Friends", post_video_views, 0)), FoodVV = sum(ifelse(category == "Food & Beverages", post_video_views, 0)), IdentityVV = sum(ifelse(category == "Identities", post_video_views, 0)), LatinoVV = sum(ifelse(category == "Latino Culture", post_video_views, 0)), LiveVV = sum(ifelse(category == "Live", post_video_views, 0)), MusicVV = sum(ifelse(category == "Music", post_video_views, 0)), NewsVV = sum(ifelse(category == "News & Issues", post_video_views, 0)), SexVV = sum(ifelse(category == "Sex & Relationships", post_video_views, 0)), SportsVV = sum(ifelse(category == "Sports", post_video_views, 0)), TvVV = sum(ifelse(category == "TV & Movies", post_video_views, 0)), OthersVV = sum(ifelse(category == "Others", post_video_views, 0)), TotalVV = sum(post_video_views))
    
    Catdf <- Catdf[order(Catdf$TotalVV  , decreasing = TRUE),]
    Catdf <- Catdf[1:min(nrow(Catdf),5), -18]
    
    Catdfg <- gather(Catdf, "date", "post_video_views", 2:17)
    names(Catdfg) <- c("date", "category", "y")
    
    Catdfs <- spread(Catdfg, "date", "y")
    Catdfs$category <- as.character(unique(DataVideos[order(DataVideos$category, decreasing = FALSE),]$category))
    Catdfs <- Catdfs[,c("category",as.character(Catdf$date))]
    
    colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
    
    ds <- lapply(1:nrow(Catdfs), function(x){
      
      list(name = as.character(Catdfs[x,]$category), 
           
           data = lapply(2:ncol(Catdfs), function(y){
             
             list(name = as.character(names(Catdfs[y])), y = Catdfs[x,y], drilldown = as.character(names(Catdfs[y])))}
             
           ))
    })
    
    
    drilldown <- lapply(1:nrow(Catdf), function(x){
      
      list(name = "Video Views", id = as.character(Catdf[x,]$date), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]), function(y){
             
             WeekLinksDay <- WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]
             WeekLinksDay <- WeekLinksDay[order(WeekLinksDay$post_video_views, decreasing = TRUE),]
             CatColors <- data.frame(category = as.character(unique(DataVideos[order(DataVideos$category, decreasing = FALSE),]$category)), color = colores)
             
             list(name = WeekLinksDay[y,]$sharetext, y = WeekLinksDay[y,]$post_video_views, url = WeekLinksDay[y,]$permalink, 
                  color = as.character(CatColors[which(CatColors$category == WeekLinksDay[y,]$category),]$color))}
             
           ))
    })
    
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black"), series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
      hc_xAxis(type = "category", labels = list(formatter = JS("function () {formatted = this.value.length > 10? this.value.substring(0,10) + '...' : this.value;return formatted;}"))) %>% 
      hc_yAxis(title = list(text = "Video Views")) %>% 
      hc_add_series_list(ds) %>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.2.3. Pie -------------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideo1 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    
    Catdfm <- as.data.frame(table(WeekLinks$category))
    colnames(Catdfm) <- c("category", "Freq")
    
    colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
    
    ds <- lapply(1:nrow(Catdfm), function(x){
      list(name = as.character(Catdfm[x,]$category), y = Catdfm[x,]$Freq, drilldown = as.character(Catdfm[x,]$category))
    })
    
    drilldown <- lapply(1:nrow(Catdfm), function(x){
      
      list(name = "Video Views" , id = as.character(Catdfm[x,]$category), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$category == as.character(Catdfm[x,]$category)),]), function(y){
             
             WeekLinksCat <- WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]
             WeekLinksCat <- WeekLinksCat[order(WeekLinksCat$post_video_views, decreasing = TRUE),]
             
             list(name = WeekLinksCat[y,]$sharetext, y = WeekLinksCat[y,]$post_video_views, url = WeekLinksCat[y,]$permalink)}
             
           ))
    })
    
    
    colorCount = length(unique(DataArticles$category))
    
    hc <- highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), pie = list(borderColor = "black", dataLabels = list(formatter = JS("function () {formatted = this.point.name.length > 25? this.point.name.substring(0,25) + '...' : this.point.name;return formatted;}"), style = list(width = "100px"))))%>%
      
      hc_add_series(data = ds, name = "Number of Videos")%>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.2.4. Reach & Video Views ---------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideo2 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgReach = mean(na.omit(post_reach)), AvgVideoViews = mean(na.omit(post_video_views)))
    
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgVideoViews <- as.numeric(formatC(Catdfm1$AvgVideoViews, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgVideoViews, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "") %>% 
      hc_add_series(data = Catdfm1$AvgReach, name = "Avg. Reach") %>% 
      hc_add_series(data = Catdfm1$AvgVideoViews, name = "Avg. Video Views") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.2.5. IR --------------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideo3 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 0 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgIR = mean(na.omit(interaction_rate)), AvgVideoViews = mean(na.omit(post_video_views)))

    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgVideoViews, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %")) %>% 
      hc_add_series(data = Catdfm1$AvgIR, name = "Avg. IR") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b><br/>", valueSuffix = "%") %>%
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  
  # 6.1.3. Video Memes ------------------------------------------------------------------------------------------------------
  
  # 6.1.3.1. Categories Overview --------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoMemeOverview1 <- renderHighchart({
    
    input$plot_categories_video_meme
    isolate({    
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      animals <- merge(dates, DataVideos[which(DataVideos$category == "Animals" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      animals <- as.xts(animals[,"post_video_views"], order.by = animals[,"created_time"])
      
      beauty <- merge(dates, DataVideos[which(DataVideos$category == "Beauty & Fashion" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      beauty <- as.xts(beauty[,"post_video_views"], order.by = beauty[,"created_time"])
      
      celeb <- merge(dates, DataVideos[which(DataVideos$category == "Celebrity & Gossip" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      celeb <- as.xts(celeb[,"post_video_views"], order.by = celeb[,"created_time"])
      
      citizen <- merge(dates, DataVideos[which(DataVideos$category == "Citizenship & Politics" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      citizen <- as.xts(citizen[,"post_video_views"], order.by = citizen[,"created_time"])
      
      diy <- merge(dates, DataVideos[which(DataVideos$category == "DIY" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      diy <- as.xts(diy[,"post_video_views"], order.by = diy[,"created_time"])
      
      family <- merge(dates, DataVideos[which(DataVideos$category == "Family & Friend" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      family <- as.xts(family[,"post_video_views"], order.by = family[,"created_time"])
      
      food <- merge(dates, DataVideos[which(DataVideos$category == "Food & Beverages" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      food <- as.xts(food[,"post_video_views"], order.by = food[,"created_time"])
      
      identities <- merge(dates, DataVideos[which(DataVideos$category == "Identities" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      identities <- as.xts(identities[,"post_video_views"], order.by = identities[,"created_time"])
      
      latino <- merge(dates, DataVideos[which(DataVideos$category == "Latino Culture" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      latino <- as.xts(latino[,"post_video_views"], order.by = latino[,"created_time"])
      
      live <- merge(dates, DataVideos[which(DataVideos$category == "Live" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      live <- as.xts(live[,"post_video_views"], order.by = live[,"created_time"])
      
      music <- merge(dates, DataVideos[which(DataVideos$category == "Music" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      music <- as.xts(music[,"post_video_views"], order.by = music[,"created_time"])
      
      news <- merge(dates, DataVideos[which(DataVideos$category == "News & Issues" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      news <- as.xts(news[,"post_video_views"], order.by = news[,"created_time"])
      
      others <- merge(dates, DataVideos[which(DataVideos$category == "Others" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      others <- as.xts(others[,"post_video_views"], order.by = others[,"created_time"])
      
      sensat <- merge(dates, DataVideos[which(DataVideos$category == "Sensationalist" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sensat <- as.xts(sensat[,"post_video_views"], order.by = sensat[,"created_time"])
      
      sex <- merge(dates, DataVideos[which(DataVideos$category == "Sex & Relationships" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sex <- as.xts(sex[,"post_video_views"], order.by = sex[,"created_time"])
      
      sports <- merge(dates, DataVideos[which(DataVideos$category == "Sports" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      sports <- as.xts(sports[,"post_video_views"], order.by = sports[,"created_time"])
      
      tv <- merge(dates, DataVideos[which(DataVideos$category == "TV & Movies" & DataVideos$video_meme == 1), c("created_time","post_video_views")], by = "created_time", all = TRUE)
      tv <- as.xts(tv[,"post_video_views"], order.by = tv[,"created_time"])
      
      colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Video Views")) %>% 
        
        hc_add_series(animals, name = "Animals", type = input$chart_type_video_meme, visible = ifelse("Animals" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type_video_meme, visible = ifelse("Beauty & Fashion" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type_video_meme, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type_video_meme, visible = ifelse("Citizenship & Politics" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(diy, name = "DIY", type = input$chart_type_video_meme, visible = ifelse("DIY" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type_video_meme, visible = ifelse("Family & Friends" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type_video_meme, visible = ifelse("Food & Beverages" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type_video_meme, visible = ifelse("Identities" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type_video_meme, visible = ifelse("Latino Culture" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(live, name = "Live", type = input$chart_type_video_meme, visible = ifelse("Live" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type_video_meme, visible = ifelse("Music" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type_video_meme, visible = ifelse("News & Issues" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(others, name = "Others", type = input$chart_type_video_meme, visible = ifelse("Others" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type_video_meme, visible = ifelse("Sex & Relationships" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type_video_meme, visible = ifelse("Sports" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type_video_meme, visible = ifelse("TV & Movies" %in% input$category_buttons_video_meme, TRUE, FALSE)) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_video_meme == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_video_meme == "area" || input$chart_type_video_meme == "column"){if(input$chart_stack_video != "none"){input$chart_stack_video_meme}}, dataGrouping = list(approximation = input$chart_avg_total_video_meme, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_video_meme, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$PlotCategoriesVideoMemeOverview2 <- renderHighchart({
    
    input$plot_categories_video_meme2
    isolate({ 

      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      animals <- merge(dates, DataVideos[which(DataVideos$category == "Animals" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      animals <- as.xts(animals[,"post_reach"], order.by = animals[,"created_time"])
      
      beauty <- merge(dates, DataVideos[which(DataVideos$category == "Beauty & Fashion" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      beauty <- as.xts(beauty[,"post_reach"], order.by = beauty[,"created_time"])
      
      celeb <- merge(dates, DataVideos[which(DataVideos$category == "Celebrity & Gossip" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      celeb <- as.xts(celeb[,"post_reach"], order.by = celeb[,"created_time"])
      
      citizen <- merge(dates, DataVideos[which(DataVideos$category == "Citizenship & Politics" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      citizen <- as.xts(citizen[,"post_reach"], order.by = citizen[,"created_time"])
      
      diy <- merge(dates, DataVideos[which(DataVideos$category == "DIY" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      diy <- as.xts(diy[,"post_reach"], order.by = diy[,"created_time"])
      
      family <- merge(dates, DataVideos[which(DataVideos$category == "Family & Friend" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      family <- as.xts(family[,"post_reach"], order.by = family[,"created_time"])
      
      food <- merge(dates, DataVideos[which(DataVideos$category == "Food & Beverages" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      food <- as.xts(food[,"post_reach"], order.by = food[,"created_time"])
      
      identities <- merge(dates, DataVideos[which(DataVideos$category == "Identities" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      identities <- as.xts(identities[,"post_reach"], order.by = identities[,"created_time"])
      
      latino <- merge(dates, DataVideos[which(DataVideos$category == "Latino Culture" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      latino <- as.xts(latino[,"post_reach"], order.by = latino[,"created_time"])
      
      live <- merge(dates, DataVideos[which(DataVideos$category == "Live" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      live <- as.xts(live[,"post_reach"], order.by = live[,"created_time"])
      
      music <- merge(dates, DataVideos[which(DataVideos$category == "Music" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      music <- as.xts(music[,"post_reach"], order.by = music[,"created_time"])
      
      news <- merge(dates, DataVideos[which(DataVideos$category == "News & Issues" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      news <- as.xts(news[,"post_reach"], order.by = news[,"created_time"])
      
      others <- merge(dates, DataVideos[which(DataVideos$category == "Others" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      others <- as.xts(others[,"post_reach"], order.by = others[,"created_time"])
      
      sensat <- merge(dates, DataVideos[which(DataVideos$category == "Sensationalist" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sensat <- as.xts(sensat[,"post_reach"], order.by = sensat[,"created_time"])
      
      sex <- merge(dates, DataVideos[which(DataVideos$category == "Sex & Relationships" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sex <- as.xts(sex[,"post_reach"], order.by = sex[,"created_time"])
      
      sports <- merge(dates, DataVideos[which(DataVideos$category == "Sports" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      sports <- as.xts(sports[,"post_reach"], order.by = sports[,"created_time"])
      
      tv <- merge(dates, DataVideos[which(DataVideos$category == "TV & Movies" & DataVideos$video_meme == 1), c("created_time","post_reach")], by = "created_time", all = TRUE)
      tv <- as.xts(tv[,"post_reach"], order.by = tv[,"created_time"])
      
      colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
      
      hc <-highchart(type = "stock") %>%
        
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        
        hc_add_series(animals, name = "Animals", type = input$chart_type_video_meme2, visible = ifelse("Animals" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(beauty, name = "Beauty & Fashion", type = input$chart_type_video_meme2, visible = ifelse("Beauty & Fashion" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(celeb, name = "Celebrity & Gossip", type = input$chart_type_video_meme2, visible = ifelse("Celebrity & Gossip" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(citizen, name = "Citizenship & Politics", type = input$chart_type_video_meme2, visible = ifelse("Citizenship & Politics" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(diy, name = "DIY", type = input$chart_type_video_meme2, visible = ifelse("DIY" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(family, name = "Family & Friends", type = input$chart_type_video_meme2, visible = ifelse("Family & Friends" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(food, name = "Food & Beverages", type = input$chart_type_video_meme2, visible = ifelse("Food & Beverages" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(identities, name = "Identities", type = input$chart_type_video_meme2, visible = ifelse("Identities" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(latino, name = "Latino Culture", type = input$chart_type_video_meme2, visible = ifelse("Latino Culture" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(live, name = "Live", type = input$chart_type_video_meme2, visible = ifelse("Live" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(music, name = "Music", type = input$chart_type_video_meme2, visible = ifelse("Music" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(news, name = "News & Issues", type = input$chart_type_video_meme2, visible = ifelse("News & Issues" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(others, name = "Others", type = input$chart_type_video_meme2, visible = ifelse("Others" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(sex, name = "Sex & Relationships", type = input$chart_type_video_meme2, visible = ifelse("Sex & Relationships" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(sports, name = "Sports", type = input$chart_type_video_meme2, visible = ifelse("Sports" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        hc_add_series(tv, name = "TV & Movies", type = input$chart_type_video_meme2, visible = ifelse("TV & Movies" %in% input$category_buttons_video_meme2, TRUE, FALSE)) %>%
        
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_video_meme2 == "day", 0, 3)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_video_meme2 == "area" || input$chart_type_video_meme2 == "column"){if(input$chart_stack_video_meme2 != "none"){input$chart_stack_video_meme2}}, dataGrouping = list(approximation = input$chart_avg_total_video_meme2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_video_meme2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  # 6.1.3.2. Top Days --------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoMeme4 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    
    Catdf <- ddply(WeekLinks, "date", summarize, AnimalsVV = sum(ifelse(category == "Animals", post_video_views, 0)), BeautyVV = sum(ifelse(category == "Beauty & Fashion", post_video_views, 0)), CelebVV = sum(ifelse(category == "Celebrity & Gossip", post_video_views, 0)), CitiPoliticsVV = sum(ifelse(category == "Citizenship & Politics", post_video_views, 0)), DIYVV = sum(ifelse(category == "DIY", post_video_views, 0)), FamilyVV = sum(ifelse(category == "Family & Friends", post_video_views, 0)), FoodVV = sum(ifelse(category == "Food & Beverages", post_video_views, 0)), IdentityVV = sum(ifelse(category == "Identities", post_video_views, 0)), LatinoVV = sum(ifelse(category == "Latino Culture", post_video_views, 0)), LiveVV = sum(ifelse(category == "Live", post_video_views, 0)), MusicVV = sum(ifelse(category == "Music", post_video_views, 0)), NewsVV = sum(ifelse(category == "News & Issues", post_video_views, 0)), SexVV = sum(ifelse(category == "Sex & Relationships", post_video_views, 0)), SportsVV = sum(ifelse(category == "Sports", post_video_views, 0)), TvVV = sum(ifelse(category == "TV & Movies", post_video_views, 0)), OthersVV = sum(ifelse(category == "Others", post_video_views, 0)), TotalVV = sum(post_video_views))
    
    Catdf <- Catdf[order(Catdf$TotalVV  , decreasing = TRUE),]
    Catdf <- Catdf[1:min(nrow(Catdf),5), -18]
    
    Catdfg <- gather(Catdf, "date", "post_video_views", 2:17)
    names(Catdfg) <- c("date", "category", "y")
    
    Catdfs <- spread(Catdfg, "date", "y")
    Catdfs$category <- as.character(unique(DataVideos[order(DataVideos$category, decreasing = FALSE),]$category))
    Catdfs <- Catdfs[,c("category",as.character(Catdf$date))]
    
    colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
    
    ds <- lapply(1:nrow(Catdfs), function(x){
      
      list(name = as.character(Catdfs[x,]$category), 
           
           data = lapply(2:ncol(Catdfs), function(y){
             
             list(name = as.character(names(Catdfs[y])), y = Catdfs[x,y], drilldown = as.character(names(Catdfs[y])))}
             
           ))
    })
    
    
    drilldown <- lapply(1:nrow(Catdf), function(x){
      
      list(name = "Video Views", id = as.character(Catdf[x,]$date), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]), function(y){
             
             WeekLinksDay <- WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]
             WeekLinksDay <- WeekLinksDay[order(WeekLinksDay$post_video_views, decreasing = TRUE),]
             CatColors <- data.frame(category = as.character(unique(DataVideos[order(DataVideos$category, decreasing = FALSE),]$category)), color = colores)
             
             list(name = WeekLinksDay[y,]$sharetext, y = WeekLinksDay[y,]$post_video_views, url = WeekLinksDay[y,]$permalink, 
                  color = as.character(CatColors[which(CatColors$category == WeekLinksDay[y,]$category),]$color))}
             
           ))
    })
    
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black"), series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
      hc_xAxis(type = "category", labels = list(formatter = JS("function () {formatted = this.value.length > 10? this.value.substring(0,10) + '...' : this.value;return formatted;}"))) %>% 
      hc_yAxis(title = list(text = "Video Views")) %>% 
      hc_add_series_list(ds) %>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.3.3. Pie -------------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoMeme1 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    Catdfm <- as.data.frame(table(WeekLinks$category))
    colnames(Catdfm) <- c("category", "Freq")
    
    colores<- c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494', '#081D58')
    
    ds <- lapply(1:nrow(Catdfm), function(x){
      list(name = as.character(Catdfm[x,]$category), y = Catdfm[x,]$Freq, drilldown = as.character(Catdfm[x,]$category))
    })
    
    drilldown <- lapply(1:nrow(Catdfm), function(x){
      
      list(name = "Video Views" , id = as.character(Catdfm[x,]$category), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]), function(y){
             
             WeekLinksCat <- WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]
             WeekLinksCat <- WeekLinksCat[order(WeekLinksCat$post_video_views, decreasing = TRUE),]
             
             list(name = WeekLinksCat[y,]$sharetext, y = WeekLinksCat[y,]$post_video_views, url = WeekLinksCat[y,]$permalink)}
             
           ))
    })
    
    colorCount = length(unique(DataArticles$category))
    
    hc <- highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), pie = list(borderColor = "black", dataLabels = list(formatter = JS("function () {formatted = this.point.name.length > 25? this.point.name.substring(0,25) + '...' : this.point.name;return formatted;}"), style = list(width = "100px"))))%>%
      
      hc_add_series(data = ds, name = "Number of Videos")%>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  # 6.1.3.4. Reach & Video Views ---------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoMeme2 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgReach = mean(na.omit(post_reach)), AvgVideoViews = mean(na.omit(post_video_views)))
    
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgVideoViews <- as.numeric(formatC(Catdfm1$AvgVideoViews, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgVideoViews, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "") %>% 
      hc_add_series(data = Catdfm1$AvgReach, name = "Avg. Reach") %>% 
      hc_add_series(data = Catdfm1$AvgVideoViews, name = "Avg. Video Views") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  }) 
  
  # 6.1.3.5. IR --------------------------------------------------------------------------------------------------------------
  
  output$PlotCategoriesVideoMeme3 <- renderHighchart({
    
    WeekLinks <- DataVideos[which(DataVideos$video_meme == 1 & DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgIR = mean(na.omit(interaction_rate)), AvgVideoViews = mean(na.omit(post_video_views)))
    
    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgVideoViews, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %")) %>% 
      hc_add_series(data = Catdfm1$AvgIR, name = "Avg. IR") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b><br/>", valueSuffix = "%") %>%
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
  
  
  # ---------------------------------------------------------------------------------------------------------------------
  
  # 6.2. Categories - We Are Mitú ---------------------------------------------------------------------------------------------------------
  
  output$BHPlotCategories4 <- renderHighchart({
    
    WeekLinks <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    
    Catdf <- ddply(WeekLinks, "date", summarize, AnimalsLC = sum(ifelse(category == "Animals", link_clicks, 0)), BeautyLC = sum(ifelse(category == "Beauty & Fashion", link_clicks, 0)), CelebLC = sum(ifelse(category == "Celebrity & Gossip", link_clicks, 0)), CitiPoliticsLC = sum(ifelse(category == "Citizenship & Politics", link_clicks, 0)), FamilyLC = sum(ifelse(category == "Family & Friends", link_clicks, 0)), FoodLC = sum(ifelse(category == "Food & Beverages", link_clicks, 0)), IdentityLC = sum(ifelse(category == "Identities", link_clicks, 0)), LatinoLC = sum(ifelse(category == "Latino Culture", link_clicks, 0)), MusicLC = sum(ifelse(category == "Music", link_clicks, 0)), NewsLC = sum(ifelse(category == "News & Issues", link_clicks, 0)), SensationLC = sum(ifelse(category == "Sensationalist", link_clicks, 0)), SexLC = sum(ifelse(category == "Sex & Relationships", link_clicks, 0)), SportsLC = sum(ifelse(category == "Sports", link_clicks, 0)), TvLC = sum(ifelse(category == "TV & Movies", link_clicks, 0)), TotalLC = sum(link_clicks))
    
    Catdf <- Catdf[order(Catdf$TotalLC  , decreasing = TRUE),]
    Catdf <- Catdf[1:min(nrow(Catdf),5), -16]
    
    Catdfg <- gather(Catdf, "date", "link_clicks", 2:15)
    names(Catdfg) <- c("date", "category", "y")
    
    Catdfs <- spread(Catdfg, "date", "y")
    Catdfs$category <- as.character(unique(DataArticles[order(DataArticles$category, decreasing = FALSE),]$category))
    Catdfs <- Catdfs[,c("category",as.character(Catdf$date))]
    
    colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58')
    
    ds <- lapply(1:nrow(Catdfs), function(x){
      
      list(name = as.character(Catdfs[x,]$category), 
           
           data = lapply(2:ncol(Catdfs), function(y){
             
             list(name = as.character(names(Catdfs[y])), y = Catdfs[x,y], drilldown = as.character(names(Catdfs[y])))}
             
           ))
    })
    
    drilldown <- lapply(1:nrow(Catdf), function(x){
      
      list(name = "Link Clicks", id = as.character(Catdf[x,]$date), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]), function(y){
             
             WeekLinksDay <- WeekLinks[which(WeekLinks$date == Catdf[x,]$date),]
             WeekLinksDay <- WeekLinksDay[order(WeekLinksDay$link_clicks, decreasing = TRUE),]
             CatColors <- data.frame(category = as.character(unique(DataArticles[order(DataArticles$category, decreasing = FALSE),]$category)), color = colores)
             
             list(name = WeekLinksDay[y,]$headline, y = WeekLinksDay[y,]$link_clicks, url = WeekLinksDay[y,]$permalink, 
                  color = as.character(CatColors[which(CatColors$category == WeekLinksDay[y,]$category),]$color))}
             
           ))
    })
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black"), series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
      hc_xAxis(type = "category", labels = list(formatter = JS("function () {formatted = this.value.length > 10? this.value.substring(0,10) + '...' : this.value;return formatted;}"))) %>% 
      hc_yAxis(title = list(text = "Link Clicks")) %>% 
      hc_add_series_list(ds) %>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })

  
  output$BHPlotCategories1 <- renderHighchart({
    WeekLinks <- DataArticlesBH[which(DataArticlesBH$date >= input$dateRange1[1] & DataArticlesBH$date <= input$dateRange1[2]),]
    Catdfm <- as.data.frame(table(WeekLinks$category))
    colnames(Catdfm) <- c("category", "Freq")
    
    colores<- c('#B2182B','#D73027','#F46D43','#FDAE61','#FEE090','#FFFFD9','#EDF8B1','#C7E9B4','#7FCDBB','#41B6C4','#1D91C0','#225EA8','#253494','#081D58')
    
    ds <- lapply(1:nrow(Catdfm), function(x){
      list(name = Catdfm[x,]$category, y = Catdfm[x,]$Freq, drilldown = as.character(Catdfm[x,]$category))
    })
    
    drilldown <- lapply(1:nrow(WeekLinks), function(x){
      
      list(name = "Link Clicks" , id = as.character(Catdfm[x,]$category), 
           
           data = lapply(1:nrow(WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]), function(y){
             
             WeekLinksCat <- WeekLinks[which(WeekLinks$category == Catdfm[x,]$category),]
             WeekLinksCat <- WeekLinksCat[order(WeekLinksCat$link_clicks, decreasing = TRUE),]
             
             list(name = WeekLinksCat[y,]$headline, y = WeekLinksCat[y,]$link_clicks, url = WeekLinksCat[y,]$permalink)}
             
           ))
    })
    
    colorCount = length(unique(DataArticles$category))
    
    hc <- highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_colors(colores) %>%
      hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), pie = list(borderColor = "black", dataLabels = list(formatter = JS("function () {formatted = this.point.name.length > 25? this.point.name.substring(0,25) + '...' : this.point.name;return formatted;}"), style = list(width = "100px"))))%>%
      hc_add_series(data = ds, name = "Number of Articles")%>% 
      hc_drilldown(series = drilldown) %>% 
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = FALSE)%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })
 
  
  output$BHPlotCategories2 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgCTR = mean(na.omit(ctr)), AvgIR = mean(na.omit(interaction_rate)), AvgReach = mean(na.omit(post_reach)), AvgLinkClicks = mean(na.omit(link_clicks)))
    
    Catdfm1$AvgCTR <- as.numeric(format(Catdfm1$AvgCTR*100, digits= 3))
    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 3))
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgLinkClicks <- as.numeric(formatC(Catdfm1$AvgLinkClicks, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgLinkClicks, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "") %>% 
      hc_add_series(data = Catdfm1$AvgReach, name = "Avg. Reach") %>% 
      hc_add_series(data = Catdfm1$AvgLinkClicks, name = "Avg. Link Clicks") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
  })

  
  output$BHPlotCategories3 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    Catdfm1 <- ddply(WeekLinks, "category", summarize, AvgCTR = mean(na.omit(ctr)), AvgIR = mean(na.omit(interaction_rate)), AvgReach = mean(na.omit(post_reach)), AvgLinkClicks = mean(na.omit(link_clicks)))
    
    Catdfm1$AvgCTR <- as.numeric(format(Catdfm1$AvgCTR*100, digits= 2))
    Catdfm1$AvgIR <- as.numeric(format(Catdfm1$AvgIR*100, digits = 2))
    Catdfm1$AvgReach <- as.numeric(formatC(Catdfm1$AvgReach, format = "f", digits = 2))
    Catdfm1$AvgLinkClicks <- as.numeric(formatC(Catdfm1$AvgLinkClicks, format = "f", digits = 2))
    Catdfm1 <- Catdfm1[order(Catdfm1$AvgLinkClicks, decreasing = TRUE),]
    
    hc <- highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>% 
      hc_xAxis(categories = Catdfm1$category) %>% 
      hc_yAxis(title = "",labels = list(format = "{value} %")) %>% 
      hc_add_series(data = Catdfm1$AvgCTR, name = "Avg. CTR") %>% 
      hc_add_series(data = Catdfm1$AvgIR, name = "Avg. IR") %>%
      hc_exporting(enabled = FALSE)%>% 
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b><br/>", valueSuffix = "%") %>%
      hc_add_theme(hc_theme_smpl())
    
    hc
    
  })
 
  
  # 7. Reposts --------------------------------------------------------------------------------------------------------------------
  
  # 7.1. Reposts - We Are Mitú ---------------------------------------------------------------------------------------------------------
  
  
  
  # 7.1.1. Articles ------------------------------------------------------------------------------------------------------------------
  
  output$PlotRepostsOverviewArticles1 <- renderHighchart({
    
    input$plot_repost_articles
    isolate({ 
      
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$repost == 1),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$original == 1),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_repost_articles) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_repost_articles) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_articles == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_articles == "area" || input$chart_type_repost_articles == "column"){if(input$chart_stack_repost_articles != "none"){input$chart_stack_repost_articles}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_articles, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewArticles2 <- renderHighchart({
    
    input$plot_repost_articles2
    isolate({ 
      
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_reach <- merge(dates, DataArticles[which(DataArticles$repost == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      reposts_reach <- as.xts(reposts_reach[,"post_reach"], order.by = reposts_reach[,"created_time"])
      
      originals_reach <- merge(dates, DataArticles[which(DataArticles$original == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      originals_reach <- as.xts(originals_reach[,"post_reach"], order.by = originals_reach[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(originals_reach, name = "Originals", type = input$chart_type_repost_articles2) %>%
        hc_add_series(reposts_reach, name = "Reposts", type = input$chart_type_repost_articles2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_articles2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_articles2 == "area" || input$chart_type_repost_articles2 == "column"){if(input$chart_stack_repost_articles2 != "none"){input$chart_stack_repost_articles2}}, dataGrouping = list(approximation = input$chart_avg_total_repost_articles2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_articles2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewArticles3 <- renderHighchart({
    
    input$plot_repost_articles3
    isolate({ 
    
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_clicks <- merge(dates, DataArticles[which(DataArticles$repost == 1),c("created_time", "link_clicks")], by = "created_time", all = TRUE)
      reposts_clicks <- as.xts(reposts_clicks[,"link_clicks"], order.by = reposts_clicks[,"created_time"])
      
      originals_clicks <- merge(dates, DataArticles[which(DataArticles$original == 1),c("created_time", "link_clicks")], by = "created_time", all = TRUE)
      originals_clicks <- as.xts(originals_clicks[,"link_clicks"], order.by = originals_clicks[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Link Clicks")) %>% 
        hc_add_series(originals_clicks, name = "Originals", type = input$chart_type_repost_articles3) %>%
        hc_add_series(reposts_clicks, name = "Reposts", type = input$chart_type_repost_articles3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_articles3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_articles3 == "area" || input$chart_type_repost_articles3 == "column"){if(input$chart_stack_repost_articles3 != "none"){input$chart_stack_repost_articles3}}, dataGrouping = list(approximation = input$chart_avg_total_repost_articles3, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_articles3, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$TableRepostsArticles1 = DT::renderDataTable({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    
    if(nrow(WeekLinksReposts) != 0){
      
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    WeekLinksReposts$post_reach <- format( WeekLinksReposts$post_reach, big.mark = ",")
    WeekLinksReposts$link_clicks <- format( WeekLinksReposts$link_clicks, big.mark = ",")
    WeekLinksReposts$headline <- paste0("<a href='", WeekLinksReposts$permalink,"' target='_blank'>", WeekLinksReposts$headline,"</a>")
    WeekLinksReposts$ctr <- paste0(formatC(100 * WeekLinksReposts$ctr, format = "f", digits = 2), "%")
    WeekLinksReposts$interaction_rate <- paste0(formatC(100 * WeekLinksReposts$interaction_rate, format = "f", digits = 2), "%")
    
    WeekLinksReposts[, input$show_vars_repost_articles1, drop = FALSE]
    
    }
    
    WeekLinksReposts[, input$show_vars_repost_articles1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6)))))
  
  output$TitleRepostSelectedArticles1 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    s = input$TableRepostsArticles1_rows_selected
    
    if(length(s)){
      
      post_id <- WeekLinksReposts[s,]$status_id
      
      Headline <- DataArticles[which(DataArticles$status_id == post_id),]$headline
      ShareText <- DataArticles[which(DataArticles$status_id == post_id),]$sharetext
      
      hc <- highchart() %>% 
        hc_title(text = Headline) %>%
        hc_subtitle(text = ShareText) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedArticles1 <- renderHighchart({

    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]

    s = input$TableRepostsArticles1_rows_selected

    if(length(s)){

      Post_url <- WeekLinksReposts[s,]$mitu_link
      RepostsGroup <- DataArticles[which(DataArticles$mitu_link == Post_url),]

      RepostsGroup$ctr <- as.numeric(format(RepostsGroup$ctr*100, digits= 2))
      RepostsGroup$interaction_rate <- as.numeric(format(RepostsGroup$interaction_rate*100, digits = 2))

      ds_ctr <- lapply(1:nrow(RepostsGroup), function(x){

        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$ctr, url = RepostsGroup[x,]$permalink)
      })

      ds_ir <- lapply(1:nrow(RepostsGroup), function(x){

        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$interaction_rate, url = RepostsGroup[x,]$permalink)
      })

      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "CTR & IR") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = "",labels = list(format = "{value} %")) %>%
        hc_add_series(data = ds_ctr, name = "CTR") %>%
        hc_add_series(data = ds_ir, name = "IR") %>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = TRUE, floating = TRUE, align = "right", verticalAlign = "top") %>%
        hc_tooltip(valueSuffix = " %") %>%
        hc_add_theme(hc_theme_smpl())

      hc

    }

  })
  
  output$PlotRepostSelectedArticles2 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    s = input$TableRepostsArticles1_rows_selected
    
    if(length(s)){
      
      Post_url <- WeekLinksReposts[s,]$mitu_link
      RepostsGroup <- DataArticles[which(DataArticles$mitu_link == Post_url),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_reach, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Reach") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Reach")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedArticles3 <- renderHighchart({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    s = input$TableRepostsArticles1_rows_selected
    
    if(length(s)){
      
      Post_url <- WeekLinksReposts[s,]$mitu_link
      RepostsGroup <- DataArticles[which(DataArticles$mitu_link == Post_url),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$link_clicks, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>% 
        hc_title(text = "Link Clicks") %>%
        hc_chart(type = "column") %>% 
        hc_plotOptions(borderColor = "black", series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>% 
        hc_xAxis(type = "category") %>% 
        hc_add_series(data = dslc, name = "Link Clicks")%>%
        hc_exporting(enabled = FALSE)%>% 
        hc_legend(enabled = FALSE)%>% 
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$picture_article <- renderText({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    s = input$TableRepostsArticles1_rows_selected
    
    if(length(s)){
      image_url <- WeekLinksReposts[s,]$full_picture
      image_text <- paste("<img src ='", image_url,"'",'title=""', 'alt="" border="0" height="100" width="100">')
      image_text
    }
    
  })
  
  output$TableRepostSelectedArticles1 = DT::renderDataTable({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    WeekLinksReposts <- WeekLinks[which(WeekLinks$repost == 1),]
    WeekLinksReposts <- WeekLinksReposts[order(WeekLinksReposts$link_clicks, decreasing = TRUE),]
    
    s = input$TableRepostsArticles1_rows_selected
    
    if(length(s)){
      
      Post_url <- WeekLinksReposts[s,]$mitu_link
      RepostsGroup <- DataArticles[which(DataArticles$mitu_link == Post_url),]
      
      RepostsGroup$post_reach <- format( RepostsGroup$post_reach, big.mark = ",")
      RepostsGroup$link_clicks <- format( RepostsGroup$link_clicks, big.mark = ",")
      RepostsGroup$headline <- paste0("<a href='", RepostsGroup$permalink,"' target='_blank'>", RepostsGroup$headline,"</a>")
      RepostsGroup$ctr <- paste0(formatC(100 * RepostsGroup$ctr, format = "f", digits = 2), "%")
      RepostsGroup$interaction_rate <- paste0(formatC(100 * RepostsGroup$interaction_rate, format = "f", digits = 2), "%")
      
      RepostsGroup[, input$show_vars_repost_selected_articles1, drop = FALSE]
    }
  
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6)))))
  
  # 7.1.2. Videos --------------------------------------------------------------------------------------------------------------------
  
  output$PlotRepostsOverviewVideos1 <- renderHighchart({
    
    input$plot_repost_videos
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 0),"created_time"], num = rep(1,nrow(DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 0),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 0),"created_time"], num = rep(1,nrow(DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 0),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_repost_videos) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_repost_videos) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_videos == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_videos == "area" || input$chart_type_repost_videos == "column"){if(input$chart_stack_repost_videos != "none"){input$chart_stack_repost_videos}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_videos, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewVideos2 <- renderHighchart({
    
    input$plot_repost_videos2
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_reach <- merge(dates, DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 0),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      reposts_reach <- as.xts(reposts_reach[,"post_reach"], order.by = reposts_reach[,"created_time"])
      
      originals_reach <- merge(dates, DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 0),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      originals_reach <- as.xts(originals_reach[,"post_reach"], order.by = originals_reach[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(originals_reach, name = "Originals", type = input$chart_type_repost_videos2) %>%
        hc_add_series(reposts_reach, name = "Reposts", type = input$chart_type_repost_videos2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_videos2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_videos2 == "area" || input$chart_type_repost_videos2 == "column"){if(input$chart_stack_repost_videos2 != "none"){input$chart_stack_repost_videos2}}, dataGrouping = list(approximation = input$chart_avg_total_repost_videos2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_videos2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewVideos3 <- renderHighchart({
    
    input$plot_repost_videos3
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_views <- merge(dates, DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 0),c("created_time", "post_video_views")], by = "created_time", all = TRUE)
      reposts_views <- as.xts(reposts_views[,"post_video_views"], order.by = reposts_views[,"created_time"])
      
      originals_views <- merge(dates, DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 0),c("created_time", "post_video_views")], by = "created_time", all = TRUE)
      originals_views <- as.xts(originals_views[,"post_video_views"], order.by = originals_views[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Video Views")) %>% 
        hc_add_series(originals_views, name = "Originals", type = input$chart_type_repost_videos3) %>%
        hc_add_series(reposts_views, name = "Reposts", type = input$chart_type_repost_videos3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_videos3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_videos3 == "area" || input$chart_type_repost_videos3 == "column"){if(input$chart_stack_repost_videos3 != "none"){input$chart_stack_repost_videos3}}, dataGrouping = list(approximation = input$chart_avg_total_repost_videos3, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_videos3, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$TableRepostsVideos1 = DT::renderDataTable({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    
    if(nrow(WeekVideosReposts) != 0){
      
      WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
      
      WeekVideosReposts$post_reach <- format( WeekVideosReposts$post_reach, big.mark = ",")
      WeekVideosReposts$post_video_views <- format( WeekVideosReposts$post_video_views, big.mark = ",")
      WeekVideosReposts$sharetext <- paste0("<a href='", WeekVideosReposts$permalink,"' target='_blank'>", WeekVideosReposts$sharetext,"</a>")
      WeekVideosReposts$interaction_rate <- paste0(formatC(100 * WeekVideosReposts$interaction_rate, format = "f", digits = 2), "%")
      
      WeekVideosReposts[, input$show_vars_repost_videos1, drop = FALSE]
      
    }
    
    WeekVideosReposts[, input$show_vars_repost_videos1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5)))))
  
  output$TitleRepostSelectedVideos1 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      post_id <- WeekVideosReposts[s,]$status_id
      
      ShareText <- DataVideos[which(DataVideos$status_id == post_id),]$sharetext
      # ImageText <- DataPhotos[which(DataPhotos$status_id == post_id),]$image_text_py
      
      hc <- highchart() %>% 
        hc_title(text = ShareText) %>%
        # hc_subtitle(text = ImageText) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedVideos1 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      # RepostsGroup$share_rate <- as.numeric(format(RepostsGroup$ctr*100, digits= 2))
      RepostsGroup$interaction_rate <- as.numeric(format(RepostsGroup$interaction_rate*100, digits = 2))
      
      # ds_ctr <- lapply(1:nrow(RepostsGroup), function(x){
      #   
      #   list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$ctr, url = RepostsGroup[x,]$permalink)
      # })
      
      ds_ir <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$interaction_rate, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "TOCA CAMBIALO") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = "",labels = list(format = "{value} %")) %>%
        # hc_add_series(data = ds_ctr, name = "CTR") %>%
        hc_add_series(data = ds_ir, name = "IR") %>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = TRUE, floating = TRUE, align = "right", verticalAlign = "top") %>%
        hc_tooltip(valueSuffix = " %") %>%
        hc_add_theme(hc_theme_smpl())
      
      hc
      
    }
    
  })
  
  output$PlotRepostSelectedVideos2 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_reach, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Reach") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Reach")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedVideos3 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_video_views, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Video Views") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Video Views")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$picture_video <- renderText({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      image_url <- WeekVideosReposts[s,]$full_picture
      image_text <- paste("<img src ='", image_url,"'",'title=""', 'alt="" border="0" height="100" width="100">')
      image_text
    }
    
  })
  
  output$TableRepostSelectedVideos1 = DT::renderDataTable({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 0),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideos1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      RepostsGroup$post_reach <- format( RepostsGroup$post_reach, big.mark = ",")
      RepostsGroup$post_video_views <- format( RepostsGroup$post_video_views, big.mark = ",")
      RepostsGroup$sharetext <- paste0("<a href='", RepostsGroup$permalink,"' target='_blank'>", RepostsGroup$sharetext,"</a>")
      RepostsGroup$interaction_rate <- paste0(formatC(100 * RepostsGroup$interaction_rate, format = "f", digits = 2), "%")
      
      RepostsGroup[, input$show_vars_repost_selected_videos1, drop = FALSE]
    }
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5)))))
  
  # 7.1.3. Video Memes ---------------------------------------------------------------------------------------------------------------
  
  output$PlotRepostsOverviewVideoMemes1 <- renderHighchart({
    
    input$plot_repost_video_memes
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 1),"created_time"], num = rep(1,nrow(DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 1),"created_time"], num = rep(1,nrow(DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_repost_video_memes) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_repost_video_memes) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_video_memes == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_video_memes == "area" || input$chart_type_repost_video_memes == "column"){if(input$chart_stack_repost_video_memes != "none"){input$chart_stack_repost_video_memes}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_video_memes, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewVideoMemes2 <- renderHighchart({
    
    input$plot_repost_video_memes2
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_reach <- merge(dates, DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      reposts_reach <- as.xts(reposts_reach[,"post_reach"], order.by = reposts_reach[,"created_time"])
      
      originals_reach <- merge(dates, DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      originals_reach <- as.xts(originals_reach[,"post_reach"], order.by = originals_reach[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>% 
        hc_add_series(originals_reach, name = "Originals", type = input$chart_type_repost_video_memes2) %>%
        hc_add_series(reposts_reach, name = "Reposts", type = input$chart_type_repost_video_memes2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_video_memes2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_video_memes2 == "area" || input$chart_type_repost_video_memes2 == "column"){if(input$chart_stack_repost_video_memes2 != "none"){input$chart_stack_repost_video_memes2}}, dataGrouping = list(approximation = input$chart_avg_total_repost_video_memes2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_video_memes2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewVideoMemes3 <- renderHighchart({
    
    input$plot_repost_video_memes3
    isolate({ 
      
      dates <- data.frame(created_time = DataVideos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts_views <- merge(dates, DataVideos[which(DataVideos$repost == 1 & DataVideos$video_meme == 1),c("created_time", "post_video_views")], by = "created_time", all = TRUE)
      reposts_views <- as.xts(reposts_views[,"post_video_views"], order.by = reposts_views[,"created_time"])
      
      originals_views <- merge(dates, DataVideos[which(DataVideos$original == 1 & DataVideos$video_meme == 1),c("created_time", "post_video_views")], by = "created_time", all = TRUE)
      originals_views <- as.xts(originals_views[,"post_video_views"], order.by = originals_views[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Video Views")) %>% 
        hc_add_series(originals_views, name = "Originals", type = input$chart_type_repost_video_memes3) %>%
        hc_add_series(reposts_views, name = "Reposts", type = input$chart_type_repost_video_memes3) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_video_memes3 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_video_memes3 == "area" || input$chart_type_repost_video_memes3 == "column"){if(input$chart_stack_repost_video_memes3 != "none"){input$chart_stack_repost_video_memes3}}, dataGrouping = list(approximation = input$chart_avg_total_repost_video_memes3, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_video_memes3, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$TableRepostsVideoMemes1 = DT::renderDataTable({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    
    if(nrow(WeekVideosReposts) != 0){
      
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    WeekVideosReposts$post_reach <- format( WeekVideosReposts$post_reach, big.mark = ",")
    WeekVideosReposts$post_video_views <- format( WeekVideosReposts$post_video_views, big.mark = ",")
    WeekVideosReposts$sharetext <- paste0("<a href='", WeekVideosReposts$permalink,"' target='_blank'>", WeekVideosReposts$sharetext,"</a>")
    WeekVideosReposts$interaction_rate <- paste0(formatC(100 * WeekVideosReposts$interaction_rate, format = "f", digits = 2), "%")
    
    WeekVideosReposts[, input$show_vars_repost_videos1, drop = FALSE]
    
    }
    
    WeekVideosReposts[, input$show_vars_repost_videos1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5)))))
  
  output$TitleRepostSelectedVideoMemes1 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      post_id <- WeekVideosReposts[s,]$status_id
      
      ShareText <- DataVideos[which(DataVideos$status_id == post_id),]$sharetext
      # ImageText <- DataPhotos[which(DataPhotos$status_id == post_id),]$image_text_py
      
      hc <- highchart() %>% 
        hc_title(text = ShareText) %>%
        # hc_subtitle(text = ImageText) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedVideoMemes1 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      # RepostsGroup$share_rate <- as.numeric(format(RepostsGroup$ctr*100, digits= 2))
      RepostsGroup$interaction_rate <- as.numeric(format(RepostsGroup$interaction_rate*100, digits = 2))
      
      # ds_ctr <- lapply(1:nrow(RepostsGroup), function(x){
      #   
      #   list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$ctr, url = RepostsGroup[x,]$permalink)
      # })
      
      ds_ir <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$interaction_rate, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "TOCA CAMBIALO") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = "",labels = list(format = "{value} %")) %>%
        # hc_add_series(data = ds_ctr, name = "CTR") %>%
        hc_add_series(data = ds_ir, name = "IR") %>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = TRUE, floating = TRUE, align = "right", verticalAlign = "top") %>%
        hc_tooltip(valueSuffix = " %") %>%
        hc_add_theme(hc_theme_smpl())
      
      hc
      
    }
    
  })
  
  output$PlotRepostSelectedVideoMemes2 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_reach, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Reach") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Reach")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedVideoMemes3 <- renderHighchart({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_video_views, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Video Views") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Video Views")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$picture_video_meme <- renderText({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      image_url <- WeekVideosReposts[s,]$full_picture
      image_text <- paste("<img src ='", image_url,"'",'title=""', 'alt="" border="0" height="100" width="100">')
      image_text
    }
    
  })
  
  output$TableRepostSelectedVideoMemes1 = DT::renderDataTable({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2] & DataVideos$video_meme == 1),]
    WeekVideosReposts <- WeekVideos[which(WeekVideos$repost == 1),]
    WeekVideosReposts <- WeekVideosReposts[order(WeekVideosReposts$post_video_views, decreasing = TRUE),]
    
    s = input$TableRepostsVideoMemes1_rows_selected
    
    if(length(s)){
      
      Video_text <- WeekVideosReposts[s,]$video_repost_sharetext
      RepostsGroup <- DataVideos[which(DataVideos$video_repost_sharetext == Video_text),]
      
      RepostsGroup$post_reach <- format( RepostsGroup$post_reach, big.mark = ",")
      RepostsGroup$post_video_views <- format( RepostsGroup$post_video_views, big.mark = ",")
      RepostsGroup$sharetext <- paste0("<a href='", RepostsGroup$permalink,"' target='_blank'>", RepostsGroup$sharetext,"</a>")
      RepostsGroup$interaction_rate <- paste0(formatC(100 * RepostsGroup$interaction_rate, format = "f", digits = 2), "%")
      
      RepostsGroup[, input$show_vars_repost_selected_video_memes1, drop = FALSE]
    }
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:5)))))
  
  # 7.1.4. Memes ---------------------------------------------------------------------------------------------------------------------
  
  output$PlotRepostsOverviewMemes1 <- renderHighchart({
    
    input$plot_repost_memes
    isolate({ 
      
      dates <- data.frame(created_time = DataPhotos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      reposts <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$repost == 1),"created_time"], num = rep(1,nrow(DataPhotos[which(DataPhotos$repost == 1),]))), by = "created_time", all = TRUE)
      reposts <- as.xts(reposts[,"num"], order.by = reposts[,"created_time"])
      
      originals <- merge(dates, data.frame(created_time = DataPhotos[which(DataPhotos$original == 1),"created_time"], num = rep(1,nrow(DataPhotos[which(DataPhotos$original == 1),]))), by = "created_time", all = TRUE)
      originals <- as.xts(originals[,"num"], order.by = originals[,"created_time"])
      
      colores<- c('#D55200', '#2580B9')
      
      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Number of Posts")) %>% 
        hc_add_series(originals, name = "Originals", type = input$chart_type_repost_memes) %>%
        hc_add_series(reposts, name = "Reposts", type = input$chart_type_repost_memes) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_memes == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_memes == "area" || input$chart_type_repost_memes == "column"){if(input$chart_stack_repost_memes != "none"){input$chart_stack_repost_memes}}, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_memes, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$PlotRepostsOverviewMemes2 <- renderHighchart({
    
    input$plot_repost_memes2
    isolate({

      dates <- data.frame(created_time = DataPhotos$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)

      reposts_reach <- merge(dates, DataPhotos[which(DataPhotos$repost == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      reposts_reach <- as.xts(reposts_reach[,"post_reach"], order.by = reposts_reach[,"created_time"])

      originals_reach <- merge(dates, DataPhotos[which(DataPhotos$original == 1),c("created_time", "post_reach")], by = "created_time", all = TRUE)
      originals_reach <- as.xts(originals_reach[,"post_reach"], order.by = originals_reach[,"created_time"])

      colores<- c('#D55200', '#2580B9')

      hc <-highchart(type = "stock") %>%
        hc_colors(colores) %>%
        hc_yAxis(offset = 30, title = list(text = "Reach")) %>%
        hc_add_series(originals_reach, name = "Originals", type = input$chart_type_repost_memes2) %>%
        hc_add_series(reposts_reach, name = "Reposts", type = input$chart_type_repost_memes2) %>%
        hc_scrollbar(enabled = FALSE) %>%
        hc_rangeSelector(selected = ifelse(input$chart_time_repost_memes2 == "day", 0, 4)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = if(input$chart_type_repost_memes2 == "area" || input$chart_type_repost_memes2 == "column"){if(input$chart_stack_repost_memes2 != "none"){input$chart_stack_repost_memes2}}, dataGrouping = list(approximation = input$chart_avg_total_repost_memes2, enabled = TRUE, forced = TRUE, units = list(list(input$chart_time_repost_memes2, list(1)))))) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_add_theme(hc_theme_smpl())
      hc

    })
  })
  
  output$TableRepostsMemes1 = DT::renderDataTable({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    
    if(nrow(WeekPhotosReposts) != 0){
      
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    WeekPhotosReposts$post_reach <- format( WeekPhotosReposts$post_reach, big.mark = ",")
    WeekPhotosReposts$total_comments <- format( WeekPhotosReposts$total_comments, big.mark = ",")
    WeekPhotosReposts$total_likes <- format( WeekPhotosReposts$total_likes, big.mark = ",")
    WeekPhotosReposts$total_shares <- format( WeekPhotosReposts$total_shares, big.mark = ",")
    WeekPhotosReposts$sharetext <- paste0("<a href='", WeekPhotosReposts$permalink,"' target='_blank'>", WeekPhotosReposts$sharetext,"</a>")
  
    WeekPhotosReposts[, input$show_vars_repost_memes1, drop = FALSE]
    
    }
    
    WeekPhotosReposts[, input$show_vars_repost_memes1, drop = FALSE]
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6)))))
  
  output$TitleRepostSelectedMemes1 <- renderHighchart({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    s = input$TableRepostsMemes1_rows_selected
    
    if(length(s)){
      
      post_id <- WeekPhotosReposts[s,]$status_id
      
      ShareText <- DataPhotos[which(DataPhotos$status_id == post_id),]$sharetext
      ImageText <- DataPhotos[which(DataPhotos$status_id == post_id),]$image_text_py
      
      hc <- highchart() %>% 
        hc_title(text = ShareText) %>%
        hc_subtitle(text = ImageText) %>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$PlotRepostSelectedMemes1 <- renderHighchart({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    s = input$TableRepostsMemes1_rows_selected
    
    if(length(s)){
      
      Post_text <- WeekPhotosReposts[s,]$image_text_py
      RepostsGroup <- DataPhotos[which(DataPhotos$image_text_py == Post_text),]
      
      # RepostsGroup$share_rate <- as.numeric(format(RepostsGroup$ctr*100, digits= 2))
      RepostsGroup$interaction_rate <- as.numeric(format(RepostsGroup$interaction_rate*100, digits = 2))
      
      # ds_ctr <- lapply(1:nrow(RepostsGroup), function(x){
      #   
      #   list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$ctr, url = RepostsGroup[x,]$permalink)
      # })
      
      ds_ir <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$interaction_rate, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "TOCA CAMBIALO") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(dataLabels = list(enabled = FALSE),stacking = "normal",enableMouseTracking = TRUE, borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_yAxis(title = "",labels = list(format = "{value} %")) %>%
        # hc_add_series(data = ds_ctr, name = "CTR") %>%
        hc_add_series(data = ds_ir, name = "IR") %>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = TRUE, floating = TRUE, align = "right", verticalAlign = "top") %>%
        hc_tooltip(valueSuffix = " %") %>%
        hc_add_theme(hc_theme_smpl())
      
      hc
      
    }
    
  })
  
  output$PlotRepostSelectedMemes2 <- renderHighchart({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    s = input$TableRepostsMemes1_rows_selected
    
    if(length(s)){
      
      Post_text <- WeekPhotosReposts[s,]$image_text_py
      RepostsGroup <- DataPhotos[which(DataPhotos$image_text_py == Post_text),]
      
      dslc <- lapply(1:nrow(RepostsGroup), function(x){
        
        list(name = as.Date(RepostsGroup[x,]$date), y = RepostsGroup[x,]$post_reach, url = RepostsGroup[x,]$permalink)
      })
      
      hc <- highchart() %>%
        hc_title(text = "Reach") %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }")))), column = list(borderColor = "black")) %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(data = dslc, name = "Reach")%>%
        hc_exporting(enabled = FALSE)%>%
        hc_legend(enabled = FALSE)%>%
        hc_add_theme(hc_theme_smpl())
      hc
    }
  })
  
  output$picture_meme <- renderText({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    s = input$TableRepostsMemes1_rows_selected
    
    if(length(s)){
      
      image_url <- WeekPhotosReposts[s,]$full_picture
      image_text <- paste("<img src ='", image_url,"'",'title=""', 'alt="" border="0" height="100" width="100">')
      image_text
    }
    
  })
  
  output$TableRepostSelectedMemes1 = DT::renderDataTable({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    WeekPhotosReposts <- WeekPhotos[which(WeekPhotos$repost == 1),]
    WeekPhotosReposts <- WeekPhotosReposts[order(WeekPhotosReposts$post_reach, decreasing = TRUE),]
    
    s = input$TableRepostsMemes1_rows_selected
    
    if(length(s)){
      
      Post_text <- WeekPhotosReposts[s,]$image_text_py
      RepostsGroup <- DataPhotos[which(DataPhotos$image_text_py == Post_text),]
      
      RepostsGroup$post_reach <- format( RepostsGroup$post_reach, big.mark = ",")
      RepostsGroup$total_comments <- format( RepostsGroup$total_comments, big.mark = ",")
      RepostsGroup$total_likes <- format( RepostsGroup$total_likes, big.mark = ",")
      RepostsGroup$total_shares <- format( RepostsGroup$total_shares, big.mark = ",")
      RepostsGroup$sharetext <- paste0("<a href='", RepostsGroup$permalink,"' target='_blank'>", RepostsGroup$sharetext,"</a>")
      
      RepostsGroup[, input$show_vars_repost_selected_memes1, drop = FALSE]
    }
    
  }, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(columnDefs = list(list(className = 'dt-center', targets = c(1:6)))))
  
  # ---------------------------------------------------------------------------------------------------------------------
  
  
  # 11. Sprint ---------------------------------------------------------------------------------------------------------------------
  
  output$infobox1 <- renderInfoBox({
    
    WeekLinks <- DataArticles[which(DataArticles$date >= input$dateRange1[1] & DataArticles$date <= input$dateRange1[2]),]
    
    actual_pageviews <- mean(WeekLinks$link_clicks)
    
    if(actual_pageviews > sprint_benchs[1]){
      
      infoBox("Actual",formatC(actual_pageviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "olive", icon = icon("fa fa-check"), fill = TRUE)
      
    }
    
    else {
      
      infoBox("Actual",formatC(actual_pageviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "red", icon = icon("fa fa-times"), fill = TRUE)
      
    }
    
  })
  
  output$infobox2 <- renderInfoBox({
    
    WeekPhotos <- DataPhotos[which(DataPhotos$date >= input$dateRange1[1] & DataPhotos$date <= input$dateRange1[2]),]
    
    actual_photoviews <- mean(WeekPhotos$post_reach)
    
    if(actual_photoviews > sprint_benchs[2]){
      
      infoBox("Actual",formatC(actual_photoviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "olive", icon = icon("fa fa-check"), fill = TRUE)
      
    }
    
    else {
      
      infoBox("Actual",formatC(actual_photoviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "red", icon = icon("fa fa-times"), fill = TRUE)
      
    }
    
  })
  
  output$infobox3 <- renderInfoBox({
    
    WeekVideos <- DataVideos[which(DataVideos$date >= input$dateRange1[1] & DataVideos$date <= input$dateRange1[2]),]
    
    actual_videoviews <- mean(WeekVideos$post_video_views)
    
    if(actual_videoviews > sprint_benchs[3]){
      
      infoBox("Actual",formatC(actual_videoviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "olive", icon = icon("fa fa-check"), fill = TRUE)
      
    }
    
    else {
      
      infoBox("Actual",formatC(actual_videoviews, format = "f", digits = 0, big.mark = ","), width = 12, color = "red", icon = icon("fa fa-times"), fill = TRUE)
      
    }
    
  })
  
}

# SHINY APP ========================================================================================================================

shinyApp(ui, server)

