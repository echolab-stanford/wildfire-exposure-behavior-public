#-------------------------------------------------------------------------------
# Process National ATUS
# Written by: Anne Driscoll
# Last edited by: Jessica Li
# 
# Get the time Americans spend outside.
#-------------------------------------------------------------------------------
#### Read in data ####
ddi = read_ipums_ddi(file.path(path_atus, "atus_00006.xml"))
time = read_ipums_micro(ddi) 

time = time %>%
    rename(id=CASEID, year=YEAR, month=MONTH, dow=DAY, state=STATEFIP, county=COUNTY, 
           age=AGE, sex=SEX, race=RACE, hispanic=HISPAN, citizen=CITIZEN,
           weekly_wage=EARNWEEK, hhld_income=FAMINCOME, weight=WT06)

#split out household level records
home = time %>%
    dplyr::filter(RECTYPE == 1) %>% 
    dplyr::select(id, year, state, county, hhld_income) %>%
    mutate(hhld_income=as_label(hhld_income))
#split out individual level records
ppl = time %>%
    dplyr::filter(RECTYPE == 2) %>%
    select(id, year, age, sex, race, hispanic, citizen, month, dow, weight) %>%
    as_label(sex, race, hispanic, citizen) %>%
    mutate(season="", 
           season=ifelse(month %in% c(12, 1, 2), "winter", season), 
           season=ifelse(month %in% c(3, 4, 5), "spring", season), 
           season=ifelse(month %in% c(6, 7, 8), "summer", season), 
           season=ifelse(month %in% c(9, 10, 11), "fall", season))
ppl = merge(ppl, home, by=c("id", "year"), all=T) #combine hhld and person

#split out actual time observations
time = time %>%
    dplyr::filter(RECTYPE == 3) %>%
    rename(activity=ACTIVITY, where=WHERE, minutes=DURATION) %>%
    dplyr::select(id, year, activity, where, minutes) 

#-------------------------------------------------------------------------------
#### Figure out what % of time individuals spend outside ####
time = time %>% as.data.frame %>% as_label(activity, where) %>%
    mutate(out=NA,
           
           # catch all indoor at home
           out=ifelse(where %in% c("R's home or yard", "NIU (Not in universe)"), 0, out), 
           
           # catch everything that's explicitly outdoors
           out=ifelse(where %in% c("Outdoors--not at home", "Walking", "Bicycle"), 1, out), 
           
           # catch activities that are exterior
           out=ifelse(activity %in% c("Lawn, garden, and houseplant care", 
                                      "Walking, exercising, playing with animals (2008+)", 
                                      "Exterior cleaning", 
                                      "Exterior repair, improvements, and decoration", 
                                      "Exterior maintenance, repair and decoration, n.e.c."), 
                      1, out),
           
           # catch walks that ppl go on that are in their yard or other 
           out=ifelse(activity %in% c("Walking", "Running") & where %in% 
                          c("R's home or yard", "Other place", "Unspecified place"), 
                      1, out), 
           
           # catch all travel in vehicles
           out=ifelse(where %in% c("Driver of car, truck, or motorcycle", 
                                   "Passenger of car, truck, or motorcycle", "Bus", 
                                   "Other mode of transportation", "Subway/train", 
                                   "Boat/ferry", "Taxi/limousine service", "Airplane"), 
                      2, out), 
           
           # set the rest to indoor but not home
           out=ifelse(is.na(out), 3, out)) 

person_time = time %>%
    group_by(id, year, out) %>%
    summarise(minutes_spent = sum(minutes)) %>%
    spread(out, minutes_spent) %>%
    rename(home=`0`, outside=`1`, travel=`2`, inside=`3`) %>%
    mutate(perc_out = outside/1440, 
           perc_home = home/1440, 
           perc_inside = (home+inside)/1440)
person_time[is.na(person_time)] = 0
person_time = merge(person_time, ppl, by=c("id", "year"))


#-------------------------------------------------------------------------------
#### Plot state level estimates ####
state_time = person_time %>%
    group_by(state) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=mean(perc_out, na.rm=T), 
              perc_home=mean(perc_home, na.rm=T)) %>%
    mutate(state=str_pad(state, 2, "left", "0"))

states = readOGR(file.path(path_dropbox, "tl_2019_us_state"), "tl_2019_us_state")
states = states[!states$NAME %in% c("Alaska", "Hawaii", "Guam", 
                                    "United States Virgin Islands", 
                                    "Commonwealth of the Northern Mariana Islands", 
                                    "American Samoa", "Puerto Rico"), ]
s = gSimplify(states, 0.002)
s = SpatialPolygonsDataFrame(s, states@data)

states_geo = fortify(s, region="GEOID")
states_geo = merge(states_geo, state_time, by.x="id", by.y="state")
states_geo = states_geo %>%
    arrange(id, group, piece, order)

cols = c("#F7D2CA", "#EC1317")
diff = getJenksBreaks(state_time$perc_home, 9)
lim  = c(min(diff), max(diff))

ggplot(states_geo, aes(long,lat,group=group, fill=perc_home)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=lim, colors=cols, values=scales::rescale(diff)) + 
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(states_geo$lat)) + labs(fill="")
ggsave(file.path(path_github, "figures/perc_time_home_map.jpg"), width=8, height=5)


#-------------------------------------------------------------------------------
#### Plot temporal variation ####
age_time = person_time %>%
    mutate(age=ifelse(age<=24, "<=24", age), age=ifelse(age>24&age<65, "25-64", age), 
           age=ifelse(age>=65, "65+", age)) %>%
    group_by(year, age) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=mean(perc_out, na.rm=T), 
              perc_home=mean(perc_home, na.rm=T)) 
ggplot(age_time) + 
    geom_line(aes(x=year, y=perc_home, group=age, color=age), size=1.5) + 
    ylim(0.63, 0.835) + theme_anne(size=25) + 
    xlab("Year") + ylab("% of time spent at home") 
ggsave(file.path(path_github, "figures/Figure1b_perc_time_home_age.jpg"), width=6.5, height=5)


inc_time = person_time %>%
    mutate(income=ifelse(hhld_income %in% 
                             c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999", 
                               "$10,000 to $12,499", "$12,500 to $14,999", 
                               "$15,000 to $19,999", "$20,000 to $24,999"), 
                         "<25k", NA), 
           income=ifelse(hhld_income %in% 
                             c("$25,000 to $29,999", "$30,000 to $34,999", 
                               "$35,000 to $39,999", "$40,000 to $49,999"), 
                         "25-50k", income), 
           income=ifelse(hhld_income %in% 
                             c("$50,000 to $59,999", "$60,000 to $74,999"), 
                         "50k-75k", income), 
           income=ifelse(hhld_income %in% 
                             c("$75,000 to $99,999", "$100,000 to $149,999",
                               "$150,000 and over"), ">75k", income), 
           income=factor(income, levels = c("<25k", "25-50k", "50k-75k", ">75k"))) %>%
    group_by(year, income) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=wtd.mean(perc_out, weight, normwt=T, na.rm=T), 
              perc_home=wtd.mean(perc_home, weight, normwt=T, na.rm=T)) %>%
    filter(!is.na(income))

ggplot(inc_time) + 
    geom_line(aes(x=year, y=perc_home, group=income, color=income), size=1.5) + 
    ylim(0.63, 0.835) + theme_anne(size=25) + 
    xlab("Year") + ylab("% of time spent at home") 
ggsave(file.path(path_github, "figures/Figure1a_perc_time_home_income.jpg"), width=6.5, height=5)

race_time = person_time %>%
    mutate(race=as.character(race)) %>%
    filter(race %in% c("Black only", "White only", "Asian only", "Hispanic"))  %>%
    group_by(year, race) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=wtd.mean(perc_out, weight, normwt=T, na.rm=T), 
              perc_home=wtd.mean(perc_home, weight, normwt=T, na.rm=T))
hisp_time = person_time %>%
    mutate(race=as.character(race), 
           race=ifelse(hispanic!="Not Hispanic", "Hispanic", race)) %>%
    filter(race=="Hispanic")  %>%
    group_by(year, race) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=wtd.mean(perc_out, weight, normwt=T, na.rm=T), 
              perc_home=wtd.mean(perc_home, weight, normwt=T, na.rm=T))
race_time = rbind(race_time, hisp_time)

ggplot(race_time) + 
    geom_line(aes(x=year, y=perc_home, group=race, color=race), size=1.5) + 
    ylim(0.63, 0.835) + theme_anne(size=25) + 
    xlab("Year") + ylab("% of time spent at home") + labs(color="race/ethnicity")
ggsave(file.path(path_github, "figures/Figure1d_perc_time_home_race.jpg"), width=7.5, height=5)



season_time = person_time %>%
    group_by(year, season) %>%
    summarise(inside=wtd.mean(inside, weight, normwt=T, na.rm=T), 
              outside=wtd.mean(outside, weights=weight, normwt=T, na.rm=T), 
              travel=wtd.mean(travel, weights=weight, normwt=T, na.rm=T), 
              perc_out=wtd.mean(perc_out, weight, normwt=T, na.rm=T), 
              perc_home=wtd.mean(perc_home, weight, normwt=T, na.rm=T))

ggplot(season_time) + 
    geom_line(aes(x=year, y=perc_home, group=season, color=season), size=1.5) + 
    ylim(0.63, 0.835) + theme_anne(size=25) + 
    xlab("Year") + ylab("% of time spent at home")
ggsave(file.path(path_github, "figures/Figure1c_perc_time_home_season.jpg"), width=6.5, height=5)
