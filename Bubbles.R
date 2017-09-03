
video_data <- DataVideos[which(DataVideos$video_meme == 0),]
video_meme_data <- DataVideos[which(DataVideos$video_meme == 1),]

video_categories <- data.frame(category = unique(video_data$category), colores = c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494'))

video_meme_categories <- data.frame(category = unique(video_meme_data$category), colores = c('#B2182B', '#D73027', '#F46D43', '#FDAE61', '#fdc835','#FEE090', '#FFFFD9', '#EDF8B1', '#C7E9B4', '#7FCDBB', '#7FCDBB', '#36a3b0', '#41B6C4', '#225EA8', '#253494'))

hchart(video_data$post_video_views, name = "Video Views")

hchart(video_meme_data$post_video_views, name = "Video Meme Views")

# Video Categories Bubbles - Engagement, Reach, Video Views -------------------------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares), y = video_data1[z,]$post_reach, z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})

hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "Engagement")) %>%
  hc_yAxis(title = list(text = "Reach")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>Engagement: </b> {point.x:,.0f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc



# Video Categories Bubbles - Engagement, 30s Complete Views, Reach ------------------------------------------------------------------


ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares), y = video_data1[z,]$post_video_complete_views_30s_unique, z = video_data1[z,]$post_reach, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "Engagement")) %>%
  hc_yAxis(title = list(text = "30s Complete Views")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>Engagement: </b> {point.x:,.0f}<br> <b>30s Complete Views: </b> {point.y}<br><b>Reach:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc


# Video Categories Bubbles - Engagement, 10s Complete Views, Reach ------------------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares), y = video_data1[z,]$post_video_views_10s_unique, z = video_data1[z,]$post_reach, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "Engagement")) %>%
  hc_yAxis(title = list(text = "10s Complete Views")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>Engagement: </b> {point.x:,.0f}<br> <b>10s Complete Views: </b> {point.y}<br><b>Reach:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc


# Video Categories Bubbles - Reach, % Viewed 10 seconds, Video Views ----------------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = video_data1[z,]$s10, y = video_data1[z,]$post_reach, z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "% Viewed 10 seconds")) %>%
  hc_yAxis(title = list(text = "Reach")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>% Viewed 10 seconds: </b> {point.x:,.2f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc

# Video Categories Bubbles - Engagement, % Viewed 10 seconds, Video Views -----------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = video_data1[z,]$s10, y = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares), z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "% Viewed 10 seconds")) %>%
  hc_yAxis(title = list(text = "Engagement")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>% Viewed 10 seconds: </b> {point.x:,.2f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc

# Video Categories Bubbles - Reach, % Viewed 30 seconds, Video Views ----------------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = video_data1[z,]$s30, y = video_data1[z,]$post_reach, z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "% Viewed 30 seconds")) %>%
  hc_yAxis(title = list(text = "Reach")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>% Viewed 30 seconds: </b> {point.x:,.2f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc

# Video Categories Bubbles - Engagement, % Viewed 30 seconds, Video Views -----------------------------------------------------------

ds_video <- lapply(1:nrow(video_categories), function(x){
  
  list(name = as.character(video_categories[x,]$category),
       
       data = lapply(1:nrow(video_data[which(video_data$category == as.character(video_categories[x,]$category)),]), function(z){
         
         video_data1 <- video_data[which(video_data$category == as.character(video_categories[x,]$category)),]
         
         list(x = video_data1[z,]$s30, y = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares)/video_data1[z,]$post_reach, z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_categories[x,]$colores)
  )
})


hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "% Viewed 10 seconds")) %>%
  hc_yAxis(title = list(text = "Engagement")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>% Viewed 10 seconds: </b> {point.x:,.2f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video)
hc


# Video Meme Categories Bubbles - Engagement, Reach, Video Views --------------------------------------------------------------------

ds_video_meme <- lapply(1:nrow(video_meme_categories), function(x){
  
  list(name = as.character(video_meme_categories[x,]$category),
       
       data = lapply(1:nrow(video_meme_data[which(video_meme_data$category == as.character(video_meme_categories[x,]$category)),]), function(z){
         
         video_meme_data1 <- video_meme_data[which(video_meme_data$category == as.character(video_meme_categories[x,]$category)),]
         
         list(x = (video_meme_data1[z,]$total_comments + video_meme_data1[z,]$total_likes + video_meme_data1[z,]$total_shares), y = video_meme_data1[z,]$post_reach, z = video_meme_data1[z,]$post_video_views, name = video_meme_data1[z,]$sharetext, url = video_meme_data1[z,]$permalink)
         
       }),
       
       color = as.character(video_meme_categories[x,]$colores)
  )
})

hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "Engagement")) %>%
  hc_yAxis(title = list(text = "Reach")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>Engagement: </b> {point.x:,.0f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video_meme)
hc




# Video Series Bubbles - Engagement, Reach, Video Views -----------------------------------------------------------------------------

video_series <- data.frame(series = unique(video_data$series))
video_series <- as.data.frame(video_series[!is.na(video_series)])

ds_video_series <- lapply(1:nrow(video_series), function(x){
  
  list(name = as.character(video_series[x,]),
       
       data = lapply(1:nrow(video_data[which(video_data$series == as.character(video_series[x,])),]), function(z){
         
         video_data1 <- video_data[which(video_data$series == as.character(video_series[x,])),]
         
         list(x = (video_data1[z,]$total_comments + video_data1[z,]$total_likes + video_data1[z,]$total_shares), y = video_data1[z,]$post_reach, z = video_data1[z,]$post_video_views, name = video_data1[z,]$sharetext, url = video_data1[z,]$permalink)
         
       })
       
  )
})

hc <-highchart() %>%
  hc_chart(zoomType = "xy", type = "bubble") %>%
  hc_plotOptions(series = list (cursor = "pointer", point = list(events = list(click = JS("function () { window.open(this.options.url); }"))))) %>%
  hc_xAxis(title = list(text = "Engagement")) %>%
  hc_yAxis(title = list(text = "Reach")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(headerFormat = '<table>', pointFormat ='<b>{point.name}</b><br> <b>Engagement: </b> {point.x:,.0f}<br> <b>Reach: </b> {point.y}<br><b>Video Views:  </b> {point.z}') %>%
  hc_add_series_list(ds_video_series)
hc




