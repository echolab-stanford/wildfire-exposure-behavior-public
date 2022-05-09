path_gtrends = file.path(path_github, "data/google_trends")
if (!dir.exists(path_gtrends)) dir.create(path_gtrends)

#-------------------------------------------------------------------------------
# Get Google Trends for Search Terms
# Written by: Anne Driscoll
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
#### Bring in the locations to query for ####
temp_file = tempfile()
file = download.file(
  "www.google.com/help/hc/downloads/ds3/Location-Language-Codes-AdWords.xlsx",
  destfile = temp_file)
usa = readxl::read_excel(temp_file, skip = 1, .name_repair = "minimal")[, c(11:14)]
usa = usa[!is.na(usa$`Metro code`), ]

locs = data.frame(country_code = "US", name = usa$Metro,
                  state = sapply(usa$Metro, 
                                 function(x){y = strsplit(x, ",")[[1]]
                                 y = y[length(y)]
                                 return(str_trim(y))})) %>%
  mutate(sub_code = paste("US", state, usa$`Metro code`, sep = "-")) %>%
  filter(nchar(sub_code) == 9)
file.remove(temp_file)

#-------------------------------------------------------------------------------
#### Get the keywords etc ready ####
keywords = c("fire", "wildfire", "smoke", 
             "air quality", "purple air",
             "air purifier", "air filter", "smoke mask", 
             
             "fuego", "incendio", "humo", 
             "calidad del aire", 
             "purificador de aire", "filtro de aire", "mascara antihumo",
             
             "火", "山火", "烟雾", 
             "空气质量", 
             "空气净化器", "空气过滤器", "口罩",
             
             "floods", "inundaciones",
             "hurricanes", "huracanes",
             "dinosaurs", "dinosaurios",
             "steph curry", "USWNT",
             
             "air pollution", "contaminacion del aire",
             "wildfire smoke", "humo de incendios forestales")
time = "2016-01-01 2020-12-31"
n = ceiling(nrow(locs)/5)
full_output = as.list(rep(NA, length(keywords)))


#-------------------------------------------------------------------------------
#### Loop through keywords and locations, normalizing within DMA ####

prog = txtProgressBar(min=0, max=length(keywords), initial=0, style = 3)
for (i in 1:length(keywords)) {
  
  word = keywords[i]
  variable = as.list(rep(NA, n))
  locs_cur = unique(locs$sub_code)
  w = 1 #used to iterate over locations
  
  for (j in 1:n) {
    
    #get the locations we want to pull data for
    loc_cur = as.character(locs$sub_code[w:min(c(w+4, nrow(locs)))])
    w = w+5
    
    #pull the data
    cur = gtrends(keyword=word, geo=loc_cur, time=time, onlyInterest=T)
    cur = cur$interest_over_time
    
    if (is.null(cur)) { print(paste0("No data returned for '", word, "'.")); next} #make sure we got data back
    
    cur = cur %>% #clean the data so that hits is numeric 
      dplyr::select(date, hits, keyword, geo) %>%
      mutate(hits=as.numeric(gsub("[a-zA-Z<>=!]", "", hits))) 
    
    for (k in 1:5) {
      hit_vec = cur$hits[cur$geo == loc_cur[k]]
      if (all(is.na(hit_vec))) {next}
      if (sd(hit_vec) == 0) {
        cur$hits[cur$geo == loc_cur[k]] = NA
      } else {
        cur$hits[cur$geo == loc_cur[k]] = rescale(hit_vec, to=c(0, 100))
      }
    }
    
    variable[[j]] = cur
    
    Sys.sleep(0.5)
  }
  
  setTxtProgressBar(prog, i)
  nulls = is.na(variable)
  variable = rbindlist(variable[!nulls])
  out_word = word # out_word = gsub(" ", "_", word)
  saveRDS(variable, file.path(path_gtrends, paste0("google_trends_smoke_DMA_normalized_keyword_", 
                           out_word, ".RDS")))
}

#-------------------------------------------------------------------------------
#### combine all the searches ####
f = list.files(path_gtrends)
f = f[grepl("smoke_DMA_normalized_keyword", f)]
f = f[!grepl('\"', f)]
t = as.list(rep(NA, length(f)))

temp = readRDS(file.path(path_gtrends, f[1]))
panel = expand.grid(geo=locs$sub_code, keyword=NA, date=unique(temp$date))

for (i in 1:length(t)) {

  cur = readRDS(file.path(path_gtrends, f[i]))
  
  print(summary(cur))
  
  geos = unique(cur$geo)
  keyword = str_split(f[i], "_")[[1]][7]
  keyword = substr(keyword, 1, nchar(keyword)-4)
  panel$keyword = keyword
  
  print(keyword)
  
  if (nrow(cur) == 0) { # if no locations have data put empty panel
    
    cur = panel
    cur$hits = NA
    t[[i]] = cur
    
  } else { # if anywhere has data
    
    for (j in 1:length(geos)) { 
      # if no data in geo set to NA, since querying multiple locs at a time it 
      #   returns 0 instead of NA in the group. 
      hit_vec = cur$hits[cur$geo == geos[j]]
      if (sd(hit_vec) == 0 | all(is.na(hit_vec))) {
        cur$hits[cur$geo == geos[j]] = NA
      }
    }
    
    # combine with panel
    cur = merge(cur, panel, by=c("geo", "keyword", "date"), all=T)
    
    t[[i]] = cur
    
  }
  
}

t = rbindlist(t)
saveRDS(t, file.path(path_gtrends, "google_trends_smoke_DMA_normalized_complete.RDS"))


dma_data = readRDS(file.path(path_dropbox, "panel_dma_pm_smoke_day_weekly.RDS"))
combined = t %>% 
  mutate(dma = as.factor(gsub("[a-zA-Z\\-]", "", geo)), 
         date = as.Date(date)) %>%
  merge(dma_data, by.x=c("dma", "date"), by.y=c("dma", "week"), all.x=T)
saveRDS(combined, file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
