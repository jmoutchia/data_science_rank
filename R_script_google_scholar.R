library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(stringr)
library(glue)
library(dplyr)

###set working directory
setwd("dir")

###define functions
##Function to search profiles with a labeled category (e.g. data science) and a keyword (e.g. professor or university name)
scrape_all_profiles <- function(label, keyword) {
  headers <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
  
  # remove trailing whitespaces/hidden characters
  label <- trimws(label)
  keyword <- trimws(keyword)
  
  # replace spaces with hyphen for google scholar
  label <- gsub(" ", "_", label, fixed = TRUE)
  keyword <- gsub(" ", "_", keyword, fixed = TRUE)
  
  params <- list(
    view_op = "search_authors",
    mauthors = glue("label:{label} '{keyword}'"),
    hl = "en",
    astart = 0
  )
  
  # empty list to store future the profile results
  all_profile_results <- list()
  
  profiles_is_present <- TRUE
  while (profiles_is_present) {
    response <- GET("https://scholar.google.com/citations", query = params, add_headers(.headers = headers),handle=handle(''))
    page <- read_html(content(response, "text"))
    print(paste0("extracting authors at page #", params$astart))
    
    profiles <- page %>% html_elements(".gs_ai_chpr")
    profile_results <- map(profiles, function(profile) {
      name <- profile %>% html_element(".gs_ai_name a") %>% html_text()
      link <- paste0("https://scholar.google.com", profile %>% html_element(".gs_ai_name a") %>% html_attr("href"))
      id <- sub(".*user=", "", profile %>% html_element(".gs_ai_name a") %>% html_attr("href"))
      affiliations <- profile %>% html_element(".gs_ai_aff") %>% html_text(trim = TRUE)
      email <- profile %>% html_element(".gs_ai_eml") %>% html_text()
      cited_by <- profile %>% html_element(".gs_ai_cby") %>% html_text() %>% gsub(pattern = "[^0-9]", replacement = "") # Cited by 17143 -> 17143
      interests <- profile %>% html_elements(".gs_ai_one_int") %>% html_text()
      
      # scalar values instead of single-value vectors
      # or data.frame() could be used instead here
      list(
        profile_name = name[[1]],
        profile_link = link[[1]],
        id = id[[1]],
        profile_affiliations = affiliations[[1]],
        profile_email = email[[1]],
        profile_cited_by_count = cited_by[[1]],
        profile_interests = interests
      )
    })
    
    # append profile results to the list
    all_profile_results <- c(all_profile_results, profile_results)
    
    # pagination
    next_page_button <- page %>% html_elements("button.gs_btnPR") %>% html_attr("onclick")
    if (!is.na(next_page_button)) {
      # extract the "after_author" parameter from the "onclick" attribute of the "Next" button using regex
      # and assign it to the "after_author" URL parameter which is the next token pagination
      # along with "astart" URL param
      params$after_author <- str_match(next_page_button, "after_author\\\\x3d(.*)\\\\x26")[, 2]
      params$astart <- params$astart + 10
    } else {
      profiles_is_present <- FALSE
    }
    print(paste0("after author ", params$after_author))
  }
  
  # convert to data frame
  all_profile_results <- data.frame(do.call(rbind, all_profile_results), stringsAsFactors = FALSE)
  return(all_profile_results)
}

#Example: search 'data science' labels from harvard
dt<-scrape_all_profiles("data science", "harvard")



##Function to extract profile details (total citation, h index, number of citations in a year, etc)
#profile link provided from the 'scrape_all_profiles' function above
get_profile <- function(profile) {
  url <- as.character(profile)
  response <- GET(url, add_headers(.headers = headers),handle=handle(''))
  
  ## Generate a list of all the tables identified
  if (is.null(response)) return(NA)
  
  page <- response %>% read_html()
  tables <- page %>% html_table()
  
  
  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)
  stats[,2]<-ifelse(colnames(stats)[2]!="All",rep("",rows),stats[,2])
  stats[,3]<-ifelse(colnames(stats)[3]!="Since 2019",rep("",rows),stats[,3])
  
  tab<-cbind.data.frame(
    id = sub(".*user=", "", profile),
    total_cites = as.numeric(as.character(stats[rows - 2,2])),
    h_index = as.numeric(as.character(stats[rows - 1, 2])),
    i10_index = as.numeric(as.character(stats[rows, 2])),
    total_cites_since2019 = as.numeric(as.character(stats[rows - 2,3])),
    h_index_since2019 = as.numeric(as.character(stats[rows - 1, 3])),
    i10_index_since2019 = as.numeric(as.character(stats[rows, 3])))
  
  #get citation numbers for years 2021-2023 (could customize)
  years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>%
    html_text() %>% as.numeric()
  vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
    html_text() %>% as.numeric()
  if(length(years)>length(vals)){
    # Some years don't have citations.
    # We need to match the citation counts and years
    style_tags=page %>% html_nodes(css = '.gsc_g_a') %>%
      html_attr('style')
    # these z indices seem to be the indices starting with the last year
    zindices=as.integer(stringr::str_match(style_tags, 'z-index:([0-9]+)')[,2])
    # empty vector of 0s
    allvals=integer(length=length(years))
    # fill in
    allvals[zindices]=vals
    # and then reverse
    vals=rev(allvals)
  }
  df <- data.frame(year=years, cites=vals)
  
  tab1<-cbind.data.frame(id = sub(".*user=", "", profile),cites_in2021=df[df$year=="2021","cites"],
                       cites_in2022=df[df$year=="2022","cites"],
                       cites_in2023=df[df$year=="2023","cites"])
  
  tab2<-left_join(tab, tab1)

  return(tab2)
}

#Example: get profile descriptions for 'data science - harvard' search above
prof<-get_profile(dt$profile_link[1])
for (p in dt$profile_link){
  #could pause command for 2 secs with 'sys.sleep' below if Google is rate limiting requests
  #Sys.sleep(2)
  prof1<-get_profile(p)
  prof<-rbind.data.frame(prof,prof1)
} 
prof<-prof[-1,]



##Function to extract date of oldest article (first publication) and total number of articles
#profile id provided from the 'scrape_all_profiles' function above
get_publications <- function(id) {
  
  headers <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")
  
  params <- list(
    hl = "en",
    user = paste(id),
    cstart = 0,
    pagesize=100,
    view_op = "list_works",
    sortby="pubdate"
  )
  
  # empty list to store future the profile results
  all_years <- c()
  
  profiles_is_present <- TRUE
  while (profiles_is_present) {
    #Sys.sleep(2)
    response <- GET("https://scholar.google.com/citations", query = params, add_headers(.headers = headers), handle=handle(''))
    page <- read_html(response)
    print(paste0("extracting author", params$user))
    
    cites <- page %>% html_nodes(xpath="//tr[@class='gsc_a_tr']")
    years <- cites %>% html_nodes(".gsc_a_y") %>% html_text() %>%
      as.numeric()
    
    # pagination
    if (length(years)>0&length(years)==100) {
      # extract next page if the current page has exactly 100 publications
      # google shows a maximum of 100 publications per page
      params$cstart <- params$cstart + 100
    } else {
      profiles_is_present <- FALSE
    }
    
    # append profile results to the list
    all_years <- c(all_years, years)
  }
  #replace single digit years as NA
  all_years[nchar(all_years)<4]<-NA
  
  tab3<-cbind.data.frame("id" = id,oldest_year=min(all_years, na.rm = T),
                         latest_year=max(all_years, na.rm = T),
                         num_publications=length(all_years))
  
  return(tab3)
}

#Example: get publication data for 'data science - harvard' search above
pub<-get_publications(dt$id[1])
for (i in dt$id){
  #could pause command for 2 secs with 'sys.sleep' below if Google is rate limiting requests
  #Sys.sleep(2)
  pub1<-get_publications(i)
  pub<-rbind.data.frame(pub,pub1)
} 
pub<-pub[-1,]







#### Extract all google scholar profiles with the tags "data science" or "information science".
dt_ds<-scrape_all_profiles(label="data_science", keyword="")
dt_is<-scrape_all_profiles(label="data_science", keyword="")
dt_dsis<-rbind.data.frame(dt_ds, dt_is)
#remove duplicates
dt_dsis<-dt_dsis%>%distinct()

#### Obtain profile details
prof<-get_profile(dt_dsis$profile_link[1])
for (p in dt_dsis$profile_link){
  #could pause command for 2 secs with 'sys.sleep' below if Google is rate limiting requests
  #Sys.sleep(2)
  prof1<-get_profile(p)
  prof<-rbind.data.frame(prof,prof1)
} 
prof<-prof[-1,]

#### Obtain publication details
pub<-get_publications(dt_dsis$id[1])
for (i in dt_dsis$id){
  #could pause command for 2 secs with 'sys.sleep' below if Google is rate limiting requests
  #Sys.sleep(2)
  pub1<-get_publications(i)
  pub<-rbind.data.frame(pub,pub1)
} 
pub<-pub[-1,]

#### Join the data
#unlist dt_dsis
dt_dsis[,c(1:5)]<-dt_dsis[,c(1:5)]%>%unlist()
dt_dsis[,6]<-as.numeric(dt_dsis[,6]%>%unlist())
dt_dsis[,7]<-paste(dt_dsis[,7])
sch<-left_join(dt_dsis,prof)
sch<-left_join(sch,pub)

#### Subset profiles with first publication since 2017
sch1<-sch[!is.na(sch$oldest_year)&(sch$oldest_year>=2017|is.infinite(sch$oldest_year)),]


#### calculate percentiles for number of citation, h index, and total number of publications
percentile <- ecdf(dff_dsis$profile_cited_by_count)
dff_dsis$perc_cites<-round(percentile(dff_dsis$profile_cited_by_count)*100,2)
percentile <- ecdf(dff_dsis$h_index)
dff_dsis$perc_hindex<-round(percentile(dff_dsis$h_index)*100,2)
percentile <- ecdf(dff_dsis$num_publications)
dff_dsis$perc_num<-round(percentile(dff_dsis$num_publications)*100,2)

#### calculate ranks for number of citation, h index, and total number of publications
dff_dsis2$rank_cites<-ifelse(dff_dsis2$perc_cites>99&dff_dsis2$perc_cites<99.95,paste0("Top ", round(100-dff_dsis2$perc_cites,1),"%"),
                             ifelse(dff_dsis2$perc_cites>99.94,"Top 0.01%",paste0("Top ", round(100-dff_dsis2$perc_cites,0),"%")))
dff_dsis2$rank_hindex<-ifelse(dff_dsis2$perc_hindex>99&dff_dsis2$perc_hindex<99.95,paste0("Top ", round(100-dff_dsis2$perc_hindex,1),"%"),
                              ifelse(dff_dsis2$perc_hindex>99.94,"Top 0.01%",paste0("Top ", round(100-dff_dsis2$perc_hindex,0),"%")))
dff_dsis2$rank_num<-ifelse(dff_dsis2$perc_num>99&dff_dsis2$perc_num<99.95,paste0("Top ", round(100-dff_dsis2$perc_num,1),"%"),
                           ifelse(dff_dsis2$perc_num>99.94,"Top 0.01%",paste0("Top ", round(100-dff_dsis2$perc_num,0),"%")))


#### Store the data
write.csv(sch1, "data_science_profiles.csv")


#### generate plots
##Plot number of publications
ggplot(dff_dsis2, aes(y=perc_num,x=num)) +
  geom_step(size=1,color="steelblue")+
  geom_point(aes(y=98.2,x=33))+
  labs(x = "Number of publications", y = "Percentile of Data Scientists") +
  scale_x_continuous(breaks = seq(0,180,20))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 33,linetype="dashed", color = "black")+
  #geom_hline(yintercept = 98, linetype="dashed", color = "black")+
  annotate("text", x = 35, y = 94, hjust = 0, label ="33 publications = 98th percentile (Top 2%)", size=3.5)+
  theme_minimal()%+replace%
  theme(legend.text = element_text(size=11),axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        legend.title = element_text(face="bold",size=12),plot.title = element_text(face="bold"),strip.text.x = element_text(face="bold", size=12),
        axis.title=element_text(face="bold", size=12), axis.text=element_text( size=10),legend.spacing.y=unit(0.5, "cm"))+guides(color = guide_legend(byrow = TRUE))

##Plot number of citations
dff_dsis$cites<-ifelse(dff_dsis$profile_cited_by_count>2000,2000,dff_dsis$profile_cited_by_count)
dff_dsis[dff_dsis$h_index==0,"perc_cites"]<-0
ggplot(dff_dsis2, aes(y=perc_cites,x=cites)) +
  geom_step(size=1,color="steelblue")+
  geom_point(aes(y=94.8,x=230))+
  labs(x = "Total number of citations", y = "Percentile of Data Scientists") +
  scale_x_continuous(breaks = seq(0,2000,200),labels=c(seq(0,1800,200),">=2000"))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 230,linetype="dashed", color = "black")+
  #geom_hline(yintercept = 98, linetype="dashed", color = "black")+
  annotate("text", x = 270, y = 92, hjust = 0, label ="230 citations = 95th percentile (Top 5%)", size=3.5)+
  theme_minimal()%+replace%
  theme(legend.text = element_text(size=11),axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        legend.title = element_text(face="bold",size=12),plot.title = element_text(face="bold"),strip.text.x = element_text(face="bold", size=12),
        axis.title=element_text(face="bold", size=12), axis.text=element_text( size=10),legend.spacing.y=unit(0.5, "cm"))+guides(color = guide_legend(byrow = TRUE))

##Plot H index
dff_dsis[dff_dsis$h_index==0,"perc_hindex"]<-0
ggplot(dff_dsis2, aes(y=perc_hindex,x=h_index)) +
  geom_step(size=1,color="steelblue")+
  geom_point(aes(y=96.5,x=7))+
  labs(x = "H-index", y = "Percentile of Data Scientists") +
  scale_x_continuous(breaks = seq(0,40,10))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 7,linetype="dashed", color = "black")+
  #geom_hline(yintercept = 97, linetype="dashed", color = "black")+
  annotate("text", x = 8, y = 92, hjust = 0, label ="H-index of 7 = 97th percentile (Top 3%)", size=3.5)+
  theme_minimal()%+replace%
  theme(legend.text = element_text(size=11),axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        legend.title = element_text(face="bold",size=12),plot.title = element_text(face="bold"),strip.text.x = element_text(face="bold", size=12),
        axis.title=element_text(face="bold", size=12), axis.text=element_text( size=10),legend.spacing.y=unit(0.5, "cm"))+guides(color = guide_legend(byrow = TRUE))

