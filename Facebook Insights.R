
library(curl)
library(jsonlite)

since = as.Date("2017-03-22")
until = as.Date("2017-03-27")
post_type = 'all'


##############################################################################################
########################################## INSIGHTS ##########################################
##############################################################################################


GetPostIds = function(since, until){
base = "https://graph.facebook.com/v2.6/1405630409737397/posts"
cross= paste("?fields=id,link,created_time,type,message,description&limit=10&since=", since, "T00:00:00-8:00&until=", until,"T00:00:00-8:00&access_token=", sep = "")
token= "EAAZAEuju9Pm4BAAlQBQaRYNjPwknyn7RP70MY1g0VkI864cwQZA77wSWDPnJz4dVaPZC8Hc8JqjqZBdSAW5PdXmsFpqHqoIz8mM46rvxHdmuFeBw873mZAh5nT9wu1YqFLmhKlBC1pCGYR6x0JKLFb0IHpBOgvZBsZD"

url  = paste(base, cross, token, sep="")

id_data  = fromJSON(url)
}

id_data = GetPostIds(since, until)
posts   = id_data$data

has_next_page = TRUE
num_processed = 0
statuses = vector()
scrape_starttime = datetime.datetime.now()

while (has_next_page == TRUE ){
  for (i in posts$id){

    if (post_type == "all"){
      statuses = append(statuses, i)
      num_processed = num_processed + 1
      if(num_processed %% 100 == 0){
        print(paste(num_processed, 'Kekerino!!!'))
      } ##Falta la fecha
    }

    else if(posts$type[match(i,posts$id)] == post_type)
    {
      statuses = append(statuses, i)
      num_processed = num_processed + 1
      if(num_processed %% 100 == 0){
        print(paste(num_processed, 'Kekerino!!!'))
      } ##Falta la fecha
    }

  }
  if('paging' %in% names(id_data)){
    id_data = fromJSON(id_data$paging$'next')
    posts = id_data$data
  }
  else{
    has_next_page = FALSE
    }
}

print(paste('Severo perro...', num_processed, "Posts Procesados"))

GetPostData = function(status){
  base = paste("https://graph.facebook.com/v2.6/", status, sep="")
  cross= "?fields=id,created_time,message,link,name,type,comments.limit(1).summary(true),shares,reactions.limit(1).summary(true),full_picture&limit=100&access_token="
  token= "EAAZAEuju9Pm4BAAlQBQaRYNjPwknyn7RP70MY1g0VkI864cwQZA77wSWDPnJz4dVaPZC8Hc8JqjqZBdSAW5PdXmsFpqHqoIz8mM46rvxHdmuFeBw873mZAh5nT9wu1YqFLmhKlBC1pCGYR6x0JKLFb0IHpBOgvZBsZD"

  url  = paste(base, cross, token, sep="")
  
post_data  = fromJSON(url)
return(post_data)
}

getFacebookPageFeedData = function(status){
  base = paste("https://graph.facebook.com/v2.6/", status, "/insights/", sep = '')
  cross= "?limit=100&period=lifetime&access_token="
  token= "EAAZAEuju9Pm4BAAlQBQaRYNjPwknyn7RP70MY1g0VkI864cwQZA77wSWDPnJz4dVaPZC8Hc8JqjqZBdSAW5PdXmsFpqHqoIz8mM46rvxHdmuFeBw873mZAh5nT9wu1YqFLmhKlBC1pCGYR6x0JKLFb0IHpBOgvZBsZD"
  url  = paste(base, cross, token, sep = '')
  data = fromJSON(url)
return(data)
}

endate = since + 30

if(endate > until){
  endate = until}

GetPageData = function(since, until){
  base = "https://graph.facebook.com/v2.6/1405630409737397/insights"
  cross= paste("/page_fans,page_fan_adds?since=", since, "T00:00:00&until=", endate,"T00:00:00&access_token=", sep = "")
  token= "EAAZAEuju9Pm4BAAlQBQaRYNjPwknyn7RP70MY1g0VkI864cwQZA77wSWDPnJz4dVaPZC8Hc8JqjqZBdSAW5PdXmsFpqHqoIz8mM46rvxHdmuFeBw873mZAh5nT9wu1YqFLmhKlBC1pCGYR6x0JKLFb0IHpBOgvZBsZD"

  url  = paste(base, cross, token, sep="")

  id_data  = fromJSON(url)
}

pi = data.frame()

finish = FALSE
while(finish == FALSE){
  if(endate == until){
    finish = TRUE}
  pagedata = GetPageData(since, until)
  pi = rbind(pi, data.frame('daily_new_likes' = pagedata$data$values[[1]]$value,
                            'total_likes' = pagedata$data$values[[2]]$value,
                            'date' = pagedata$data$values[[1]][2]))
  since = endate
  if(endate + 30 >= until){
    endate = until}
  else{endate = endate + 30}
}

pi$date = format(as.Date(pi$end_time), "%Y-%m-%d")
pi = within(pi, rm(end_time))

################

processFacebookPageFeedStatus = function(status){

  post_data = GetPostData(status)
  data = getFacebookPageFeedData(status)
  
  status_id = post_data['id'][[1]]
  permalink = paste('https://www.facebook.com/', post_data['id'][[1]], sep = '')
  post_type = post_data['type'][[1]]
  
  full_picture = post_data['full_picture'][[1]]
  
  if(is.null(post_data['shares']$shares$count[[1]])){shares_on_post = 0} else {shares_on_post = post_data['shares']$shares$count[[1]]}
  if(is.null(post_data['comments']$comments$summary$total_count[[1]])){comments_on_post = 0} else {comments_on_post = post_data['comments']$comments$summary$total_count[[1]]}
  if(is.null(post_data['reactions']$reactions$summary$total_count[[1]])){likes_on_post = 0} else {likes_on_post = post_data['reactions']$reactions$summary$total_count[[1]]}
  
  if(is.null(post_data['message'][[1]])){sharetext = ''} else {sharetext = post_data['message'][[1]]}
  if(is.null(post_data['name'][[1]])){headline = ''} else {headline = post_data['name'][[1]]}
  if(is.null(post_data['link'][[1]])){link = ''} else {link = post_data['link'][[1]]}

  
  status_published = format(as.POSIXct(strptime(post_data['created_time'][[1]], "%Y-%m-%dT%H:%M:%OS", tz="UTC")), tz="America/Los_Angeles",usetz=TRUE)
  status_published = as.POSIXct(status_published)
  created_time = format(status_published, '%Y-%m-%d %H:%M:%S')
  date = format(status_published, '%Y-%m-%d')
  hour = format(status_published, "%H:%M")
  
  post_consumptions_by_type = data$data$values[[match("post_consumptions_by_type", data$data$name)]][[1]]
  colnames(post_consumptions_by_type) = gsub('\\s+', '_', colnames(post_consumptions_by_type))
  
  
  post_story_adds_by_action_type = data$data$values[[match("post_story_adds_by_action_type", data$data$name)]][[1]]
  colnames(post_story_adds_by_action_type) = gsub('\\s+', '_', colnames(post_story_adds_by_action_type))
  
  
  if(is.null(data$data$values[[match('post_impressions',data$data$name)]][[1]])){post_impressions = 0} else {post_impressions =data$data$values[[match('post_impressions',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_impressions_unique',data$data$name)]][[1]])){post_reach = 0} else {post_reach =data$data$values[[match('post_impressions_unique',data$data$name)]][[1]]}
  
  if(is.null(data$data$values[[match('post_impressions_fan_unique',data$data$name)]][[1]])){post_reach_fan_unique = 0} else {post_reach_fan_unique =data$data$values[[match('post_impressions_fan_unique',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_impressions_viral_unique',data$data$name)]][[1]])){post_reach_viral_unique = 0} else {post_reach_viral_unique =data$data$values[[match('post_impressions_viral_unique',data$data$name)]][[1]]}
  
  
  if(is.null(post_story_adds_by_action_type$comment)){total_comments = 0} else {total_comments = post_story_adds_by_action_type$comment}
  if(is.null(post_story_adds_by_action_type$like)){total_likes = 0} else {total_likes = post_story_adds_by_action_type$like}
  if(is.null(post_story_adds_by_action_type$share)){total_shares = 0} else {total_shares = post_story_adds_by_action_type$share}
  
  likes_on_shares    = total_likes - likes_on_post
  comments_on_shares = total_comments - comments_on_post
  shares_on_shares   = total_shares - shares_on_post
  
  if(likes_on_shares<0){likes_on_shares = 0}
  if(comments_on_shares<0){comments_on_shares = 0}
  if(shares_on_shares<0){shares_on_shares = 0}
  
  if(is.null(post_consumptions_by_type$link_clicks)){link_clicks = 0} else {link_clicks = post_consumptions_by_type$link_clicks}
  if(is.null(post_consumptions_by_type$photo_view)){photo_view = 0} else {photo_view = post_consumptions_by_type$photo_view}
  if(is.null(post_consumptions_by_type$video_play)){video_play = 0} else {video_play = post_consumptions_by_type$video_play}
  if(is.null(post_consumptions_by_type$other_clicks)){other_clicks = 0} else {other_clicks = post_consumptions_by_type$other_clicks}
  
  if(is.null(data$data$values[[match('post_negative_feedback',data$data$name)]][[1]])){post_negative_feedback = 0} else {post_negative_feedback =data$data$values[[match('post_negative_feedback',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_story_adds',data$data$name)]][[1]])){post_story_adds = 0} else {post_story_adds =data$data$values[[match('post_story_adds',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_stories',data$data$name)]][[1]])){post_stories = 0} else {post_stories =data$data$values[[match('post_stories',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_storytellers',data$data$name)]][[1]])){post_storytellers = 0} else {post_storytellers =data$data$values[[match('post_storytellers',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_consumptions',data$data$name)]][[1]])){post_consumptions = 0} else {post_consumptions =data$data$values[[match('post_consumptions',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_engaged_users',data$data$name)]][[1]])){post_engaged_users = 0} else {post_engaged_users =data$data$values[[match('post_engaged_users',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_engaged_fan',data$data$name)]][[1]])){post_engaged_fan = 0} else {post_engaged_fan =data$data$values[[match('post_engaged_fan',data$data$name)]][[1]]}
  
  if(is.null(data$data$values[[match('post_video_complete_views_30s_autoplayed',data$data$name)]][[1]])){post_video_complete_views_30s_autoplayed = 0} else {post_video_complete_views_30s_autoplayed =data$data$values[[match('post_video_complete_views_30s_autoplayed',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_complete_views_30s_clicked_to_play',data$data$name)]][[1]])){post_video_complete_views_30s_clicked_to_play = 0} else {post_video_complete_views_30s_clicked_to_play =data$data$values[[match('post_video_complete_views_30s_clicked_to_play',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_complete_views_30s_organic',data$data$name)]][[1]])){post_video_complete_views_30s_organic = 0} else {post_video_complete_views_30s_organic =data$data$values[[match('post_video_complete_views_30s_organic',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_complete_views_30s_paid',data$data$name)]][[1]])){post_video_complete_views_30s_paid = 0} else {post_video_complete_views_30s_paid =data$data$values[[match('post_video_complete_views_30s_paid',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_complete_views_30s_unique',data$data$name)]][[1]])){post_video_complete_views_30s_unique = 0} else {post_video_complete_views_30s_unique =data$data$values[[match('post_video_complete_views_30s_unique',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_avg_time_watched',data$data$name)]][[1]])){post_video_avg_time_watched = 0} else {post_video_avg_time_watched =data$data$values[[match('post_video_avg_time_watched',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_complete_views_organic_unique',data$data$name)]][[1]])){post_video_complete_views_organic_unique = 0} else {post_video_complete_views_organic_unique =data$data$values[[match('post_video_complete_views_organic_unique',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_length',data$data$name)]][[1]])){post_video_length = 0} else {post_video_length =data$data$values[[match('post_video_length',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views',data$data$name)]][[1]])){post_video_views = 0} else {post_video_views =data$data$values[[match('post_video_views',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_autoplayed',data$data$name)]][[1]])){post_video_views_autoplayed = 0} else {post_video_views_autoplayed =data$data$values[[match('post_video_views_autoplayed',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_clicked_to_play',data$data$name)]][[1]])){post_video_views_clicked_to_play = 0} else {post_video_views_clicked_to_play =data$data$values[[match('post_video_views_clicked_to_play',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_10s_unique',data$data$name)]][[1]])){post_video_views_10s_unique = 0} else {post_video_views_10s_unique =data$data$values[[match('post_video_views_10s_unique',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_10s_autoplayed',data$data$name)]][[1]])){post_video_views_10s_autoplayed = 0} else {post_video_views_10s_autoplayed =data$data$values[[match('post_video_views_10s_autoplayed',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_10s_clicked_to_play',data$data$name)]][[1]])){post_video_views_10s_clicked_to_play = 0} else {post_video_views_10s_clicked_to_play =data$data$values[[match('post_video_views_10s_clicked_to_play',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_10s_sound_on',data$data$name)]][[1]])){post_video_views_10s_sound_on = 0} else {post_video_views_10s_sound_on =data$data$values[[match('post_video_views_10s_sound_on',data$data$name)]][[1]]}
  if(is.null(data$data$values[[match('post_video_views_sound_on',data$data$name)]][[1]])){post_video_views_sound_on = 0} else {post_video_views_sound_on =data$data$values[[match('post_video_views_sound_on',data$data$name)]][[1]]}

  
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'0')){s0 = ''} else {s0 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'0'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'1')){s1 = ''} else {s1 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'1'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'2')){s2 = ''} else {s2 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'2'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'3')){s3 = ''} else {s3 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'3'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'4')){s4 = ''} else {s4 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'4'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'5')){s5 = ''} else {s5 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'5'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'6')){s6 = ''} else {s6 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'6'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'7')){s7 = ''} else {s7 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'7'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'8')){s8 = ''} else {s8 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'8'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'9')){s9 = ''} else {s9 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'9'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'10')){s10 = ''} else {s10 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'10'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'11')){s11 = ''} else {s11 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'11'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'12')){s12 = ''} else {s12 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'12'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'13')){s13 = ''} else {s13 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'13'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'14')){s14 = ''} else {s14 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'14'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'15')){s15 = ''} else {s15 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'15'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'16')){s16 = ''} else {s16 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'16'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'17')){s17 = ''} else {s17 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'17'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'18')){s18 = ''} else {s18 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'18'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'19')){s19 = ''} else {s19 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'19'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'20')){s20 = ''} else {s20 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'20'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'21')){s21 = ''} else {s21 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'21'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'22')){s22 = ''} else {s22 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'22'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'23')){s23 = ''} else {s23 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'23'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'24')){s24 = ''} else {s24 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'24'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'25')){s25 = ''} else {s25 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'25'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'26')){s26 = ''} else {s26 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'26'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'27')){s27 = ''} else {s27 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'27'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'28')){s28 = ''} else {s28 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'28'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'29')){s29 = ''} else {s29 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'29'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'30')){s30 = ''} else {s30 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'30'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'31')){s31 = ''} else {s31 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'31'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'32')){s32 = ''} else {s32 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'32'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'33')){s33 = ''} else {s33 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'33'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'34')){s34 = ''} else {s34 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'34'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'35')){s35 = ''} else {s35 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'35'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'36')){s36 = ''} else {s36 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'36'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'37')){s37 = ''} else {s37 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'37'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'38')){s38 = ''} else {s38 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'38'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'39')){s39 = ''} else {s39 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'39'}
  if(is.null(data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'40')){s40 = ''} else {s40 = data$data$values[match('post_video_retention_graph',data$data$name)][[1]][,1]$'40'}
  
  page_total_likes = pi$total_likes[match(date, pi$date)]
  page_new_likes   = pi$daily_new_likes[match(date, pi$date)]
  
  result = t(as.matrix(c(status_id,permalink,post_type,sharetext,headline,link,full_picture,created_time,date,hour,page_total_likes,page_new_likes,post_impressions,post_reach,post_reach_fan_unique,post_reach_viral_unique,comments_on_post,likes_on_post,shares_on_post,total_comments,total_likes,total_shares,comments_on_shares,likes_on_shares,shares_on_shares,link_clicks,photo_view,video_play,other_clicks,post_negative_feedback,post_story_adds,post_stories,post_storytellers,post_consumptions,post_engaged_users,post_engaged_fan,post_video_complete_views_30s_autoplayed,post_video_complete_views_30s_clicked_to_play,post_video_complete_views_30s_organic,post_video_complete_views_30s_paid,post_video_complete_views_30s_unique,post_video_avg_time_watched,post_video_complete_views_organic_unique,post_video_length,post_video_views,post_video_views_autoplayed,post_video_views_clicked_to_play,post_video_views_10s_unique,post_video_views_10s_autoplayed,post_video_views_10s_clicked_to_play,post_video_views_10s_sound_on,post_video_views_sound_on, s0,	s1,	s2,	s3,	s4,	s5,	s6,	s7,	s8,	s9,	s10,	s11,	s12,	s13,	s14,	s15,	s16,	s17,	s18,	s19,	s20,	s21,	s22,	s23,	s24,	s25,	s26,	s27,	s28,	s29,	s30,	s31,	s32,	s33,	s34,	s35,	s36,	s37,	s38,	s39,	s40)))
return(result)
}

# statuses = c('1405630409737397_1510785635888540','1405630409737397_1511436792490091','1405630409737397_1511436792490091','1405630409737397_1512136232420147','1405630409737397_1532264077074029','1405630409737397_1532392773727826','1405630409737397_1560033190963784','1405630409737397_1647037648930004','1405630409737397_1653348084965627','1405630409737397_1653363801630722','1405630409737397_1653349724965463','1405630409737397_1653346828299086','1405630409737397_1653375138296255','1405630409737397_1653423528291416','1405630409737397_1653337191633383','1405630409737397_1653411344959301','1405630409737397_1653338324966603','1405630409737397_1653295401637562','1405630409737397_1653423781624724','1405630409737397_1653339828299786','1405630409737397_1653285528305216','1405630409737397_1653425824957853','1405630409737397_1653715424928893','1405630409737397_1653298638303905','1405630409737397_1653754348258334','1405630409737397_1653786898255079','1405630409737397_1653773454923090','1405630409737397_1653837734916662','1405630409737397_1653864711580631','1405630409737397_1653826244917811','1405630409737397_1653823791584723','1405630409737397_1653860541581048','1405630409737397_1653841741582928','1405630409737397_1653883098245459','1405630409737397_1653829834917452','1405630409737397_1654051918228577','1405630409737397_1653345221632580','1405630409737397_1654052261561876','1405630409737397_1650206555279780','1405630409737397_1653850838248685','1405630409737397_1654052101561892','1405630409737397_1653253214975114','1405630409737397_1654294671537635','1405630409737397_1653851061581996','1405630409737397_1653773421589760','1405630409737397_1654295988204170','1405630409737397_1653304694969966','1405630409737397_1653302434970192','1405630409737397_1650666325233803','1405630409737397_1654435468190222','1405630409737397_1654435698190199','1405630409737397_1659396867694082','1405630409737397_1671326359834466','1405630409737397_1673861356247633','1405630409737397_1673848899582212','1405630409737397_1682996722000763')

np = 0
final = data.frame(matrix(ncol = 92, nrow = 0))
for(status in statuses){
  final = rbind(final, processFacebookPageFeedStatus(status))
  np = np +1
  if(np %% 20 == 0){
    print(paste(np, 'Posts Procesados (Woooooot!!?!!!?)'))}
  }
colnames(final) = c('status_id','permalink','post_type','sharetext','headline','link','full_picture','created_time','date','hour','page_total_likes','page_new_likes','post_impressions','post_reach','post_reach_fan_unique','post_reach_viral_unique','comments_on_post','likes_on_post','shares_on_post','total_comments','total_likes','total_shares','comments_on_shares','likes_on_shares','shares_on_shares','link_clicks','photo_view','video_play','other_clicks','post_negative_feedback','post_story_adds','post_stories','post_storytellers','post_consumptions','post_engaged_users','post_engaged_fan','post_video_complete_views_30s_autoplayed','post_video_complete_views_30s_clicked_to_play','post_video_complete_views_30s_organic','post_video_complete_views_30s_paid','post_video_complete_views_30s_unique','post_video_avg_time_watched','post_video_complete_views_organic_unique','post_video_length','post_video_views','post_video_views_autoplayed','post_video_views_clicked_to_play','post_video_views_10s_unique','post_video_views_10s_autoplayed','post_video_views_10s_clicked_to_play','post_video_views_10s_sound_on','post_video_views_sound_on','s0',	's1',	's2',	's3',	's4',	's5',	's6',	's7',	's8',	's9',	's10',	's11',	's12',	's13',	's14',	's15',	's16',	's17',	's18',	's19',	's20',	's21',	's22',	's23',	's24',	's25',	's26',	's27',	's28',	's29',	's30',	's31',	's32',	's33',	's34',	's35',	's36',	's37',	's38',	's39',	's40')

write.csv(final, paste(as.character(Sys.Date()), ".csv"), row.names = FALSE)

