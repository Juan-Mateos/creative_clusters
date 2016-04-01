#Creative clusters data analysis
setwd("Desktop/Creative Clusters/")
source("Rcode/clusters_load_data.R")
source("Rcode/clusters_data_analysis_functions.R")
#Preparation
#Create df with all the TTWA data (also normalised)

#Rbind variables of interest (year, ttwa.name, industry, variables (employment, bcs,
# other performance variables we'll use later on))
#First all.cis

all.cis_ttwa_conc <- all.cis_ttwa %>%
     filter(year!="2010") %>%
     select(period.string,ttwa.name,business.count,employment,turnover,turn_pw,work_pb,
            business_lq,turnover_lq,emp_lq) %>%
     mutate(industry="All creative industries") %>%
     select(period.string,industry,ttwa.name,business.count,turnover,employment,
            turn_pw,work_pb,business_lq,turnover_lq,emp_lq) %>%
     melt(id.vars=c("period.string","ttwa.name","industry"))


#Second ttwa subsector
subsector_ttwa_conc <- subsectors_ttwa %>%
     select(year, ttwa.name,industry.short, 
            business.count, turnover,employment,turn_pw,work_pb,
            business_lq,turnover_lq,emp_lq) %>%
     mutate(industry.short=as.character(industry.short),
            period.string=ifelse(year=="2007_10","first.period",
                                 "second.period")) %>%
     select(period.string,industry.short,ttwa.name,business.count,turnover,employment,
            turn_pw,work_pb,business_lq,turnover_lq,emp_lq) %>%
     rename(industry=industry.short) %>%
     melt(id.vars=c("period.string","ttwa.name","industry"))

#all ttwas
total_activity_ttwa_conc <- ttwa_all_industries %>% 
     rename(ttwa.code=ttwa.2011.code..last.ons.revision.feb.2016.) %>%
     merge(ttwa_names,by.x="ttwa.code",by.y="TTWA11CD",all.x=T) %>% 
     rename(ttwa.name=TTWA11NM) %>%
     mutate(industry="All Industries",
            period.string=ifelse(year<=2010,"first.period","second.period")) %>%
     group_by(ttwa.name,period.string,industry) %>%
     summarise(employment=mean(total.employment,na.rm=T),
               business.count=mean(total.number.of.enterprises,na.rm=T),
               turnover=mean(total.turnover,na.rm=T)) %>%
     mutate(turn_pw = turnover/employment,
            work_pb = employment/business.count) %>%
     select(period.string,industry,ttwa.name,business.count,employment,turnover,
            turn_pw,work_pb) %>%
     melt(id.vars=c("period.string","ttwa.name","industry"))

#Bind
ttwa_merged <- rbind(all.cis_ttwa_conc,
                     subsector_ttwa_conc,
                     total_activity_ttwa_conc)

#Now we want to normalise by the "all industries" variables
ttwa_merged_no_lq <- ttwa_merged %>% filter(grepl("lq",variable)==FALSE) %>%
     droplevels()

ttwa_merged_w_norm <- ldply(
     split(ttwa_merged_no_lq,ttwa_merged_no_lq$variable), 
     function(x) {
          #Cast to
          x2 <-x %>% 
               dcast(period.string+ttwa.name~industry)
          
          x2[,paste0(names(x2)[3:ncol(x2)],"_norm")] <- sapply(
               x2[,3:ncol(x2)], function(x){
                    return(x/x2["All Industries"])
               }, USE.NAMES = F) 
          
          x3 <- x2 %>% melt(id.vars=c("period.string","ttwa.name"))
          x3
     },.id="metric")

#Analysis
#Scatter plot
all.cis_ttwa_shares <- all.cis_ttwa %>%
     select(year,ttwa.name,
            business.count,
            employment,
            business_local.share,
            emp_local.share,
            region) %>%
     filter(!is.na(business_local.share),
            !is.na(emp_local.share),
            year!=2010)

#Convert regions to aggregate regions
big_regions <- as.list(c("Midlands","East","London",
                         "North","North","Northern Ireland",
                         "Scotland","South","South","Wales",
                         "Midlands","North"))
names(big_regions) <- levels(as.factor(all.cis_ttwa$region))

#Prepare factors
my_palette4 <- c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')

#Change them
all.cis_ttwa_shares$big_region <- as.factor(unlist(big_regions[all.cis_ttwa_shares$region]))
all.cis_ttwa_shares$big_region <- factor(all.cis_ttwa_shares$big_region,
                                         levels=c("Northern Ireland",
                                                  "Scotland","North","Wales",
                                                  "Midlands","East","London","South"))
#Create average scores for each variable.
mean_econ_imp_scores <- all.cis_ttwa_shares %>%
     select(year,business_local.share,emp_local.share)

mean_econ_imp_scores2 <- ldply(split(mean_econ_imp_scores,
                                     as.factor(mean_econ_imp_scores$year)),
                               function(x){
                                    la <- colMeans(x[,-1],na.rm=T)
                                    return(la)
                               },.id="year")
mean_econ_imp_scores2$year <- as.numeric(as.character(mean_econ_imp_scores2$year))


#Create labels
scatter_labels <- ldply(split(all.cis_ttwa_shares,
                              all.cis_ttwa_shares$year),
                        function(x){
                             x$ttwa.name <- as.character(x$ttwa.name)
                             top10_b_share <-
                                  x %>% arrange(desc(business_local.share)) %>%
                                  as.data.frame() %>%
                                  extract(1:8,"ttwa.name")
                             top10_e_share <- 
                                  x %>% arrange(desc(emp_local.share)) %>%
                                  as.data.frame() %>%
                                  extract(1:8,"ttwa.name")
                             top10_emp <-
                                  x %>% arrange(desc(employment)) %>%
                                  as.data.frame() %>%
                                  extract(1:8,"ttwa.name")
                             
                             top_ttwas <- c(as.character(top10_emp),
                                            as.character(top10_e_share),
                                            as.character(top10_b_share)) %>%
                                  unique() %>% as.factor()
                             x$ttwa_lab <- ifelse(x$ttwa.name %in% top_ttwas,
                                                  x$ttwa.name,"")
                             return(x)
                        },.id="year") %>% select(emp_local.share,business_local.share,
                                                 year,ttwa_lab) %>%
     filter(ttwa_lab!="")

#Plot
econ_import_scatter <- ggplot(data=all.cis_ttwa_shares,
                 aes(y=business_local.share,
                     x=emp_local.share,size=employment,
                     fill=big_region))+
     scale_fill_manual(values=rev(my_palette4),
                       limits=rev(levels(all.cis_ttwa_shares$big_region)))+
     geom_point(alpha=0.9,pch=21,colour="darkgrey")+
     geom_hline(data=mean_econ_imp_scores2,
                aes(yintercept=business_local.share),colour="darkblue",
                linetype=2)+
     geom_vline(data=mean_econ_imp_scores2,
                aes(xintercept=emp_local.share),
                colour="darkblue",
                linetype=2)+
     geom_label_repel(data=scatter_labels,
                      aes(y=business_local.share,
                          x=emp_local.share,
                          label=ttwa_lab,fill=NULL,size=NULL),size=1.7,
                      label.padding=unit(0.15,"lines"),
                     force=2)+
     scale_x_continuous(label=percent)+
     scale_y_continuous(label=percent)+
     labs(title="Creative industries as a share of local economies, 2010 and 2014 \n (dashed lines represent average for all areas for each variable and year)",
            x="Creative industries as % of employment",
          y="Creative industries as % of business",
          fill="Region",
          size="Total employment \n in creative industries")+
     facet_grid(.~year)+
     theme(axis.text=element_text(size=7),
           panel.margin=unit(2,"lines"))
WriteChart(econ_import_scatter,"final-report-figures/",w=9,h=6)

#Consider other economic performance metrics
#Without normalising
performance_raw_df_not_norm <- ttwa_merged_w_norm %>%
     filter(metric %in% c("turn_pw","work_pb"),
            grepl("norm",variable)==FALSE,
            grepl("All Industries",variable)==FALSE,
            period.string=="second.period") %>% droplevels()

#The problem here is that turn per worker and worker per firm
     #are in very different scales.
#Plot with log scale

#Get labels
ttwa_econ_perf_nn_labels <- ldply(split(performance_raw_df_not_norm,
                                     list(performance_raw_df$metric,
                                          performance_raw_df$variable)),
                               function(x){
                                    #Extract my variable
                                    my_var <- x$variable[[1]] %>% as.character()
                                    x$ttwa.name <- as.character(x$ttwa.name)
                                    
                                    #Extract my metric
                                    my_metric <- x$metric[[1]] %>% as.character()
                                    
                                    #Use it to find top locations
                                    #in ttwa_merged_norm
                                    valid_ttwas_df <- ttwa_merged_w_norm %>%
                                         filter(period.string=="first.period",
                                                metric=="employment",
                                                variable==my_var,
                                                !is.na(value))
                                    
                                    valid_ttwas <- valid_ttwas_df$ttwa.name[
                                         valid_ttwas_df$value > quantile(valid_ttwas_df$value,
                                                                         na.rm=T)[[4]]]
                                    
                                    top_5_areas <-
                                         x %>% filter(ttwa.name %in% valid_ttwas) %>%
                                         arrange(desc(value)) %>%
                                         as.data.frame() %>% extract(1:5,"ttwa.name")
                                    
                                    ttwa_lab <- data.frame(metric=my_metric,
                                                           variable=my_var,
                                                           tops=paste(c("Top Areas:",top_5_areas),
                                                                      collapse="\n "))
                                    return(ttwa_lab)
                               },.id=NULL)
#Some location variables to plot the labels
ttwa_econ_perf_nn_labels$location <- 
     ifelse(ttwa_econ_perf_nn_labels$metric=="turn_pw",
            0,7)

ttwa_econ_perf_nn_labels$h_just <- 
     ifelse(ttwa_econ_perf_nn_labels$metric=="turn_pw",
            0,1)


#Reorder variable
sectors_ranked_by_tpw_nn <- performance_raw_df_not_norm %>% 
     filter(metric=="turn_pw") %>%
     group_by(variable) %>% summarise(median=median(value,na.rm=T)) %>%
     arrange(desc(median)) %>% extract(,"variable") %>% as.data.frame()
#Reorder
performance_raw_df_not_norm$variable <- ReorderFactor(
     performance_raw_df_not_norm$variable,sectors_ranked_by_tpw_nn$variable,z=T)

#Change metric labels
levels(performance_raw_df_not_norm$metric) <- c("Turnover per worker","Average business size")
levels(ttwa_econ_perf_nn_labels$metric) <- c("Turnover per worker","Average business size")

#Plot
econ_perf_plot_not_n <- ggplot(data=performance_raw_df_not_norm,
                         aes(x=variable,y=log(value)))+
     geom_boxplot(fill="#ffeda0",outlier.size=0.2)+
     geom_point(position=position_jitter(width=0.3), alpha=0.1,
                colour="orange",size=1)+
     geom_text(data=ttwa_econ_perf_nn_labels,
                aes(label=tops,y=location,hjust=h_just),size=1.5)+
     facet_grid(.~metric)+
     labs(title="Creative sales per worker and average firm size \n in UK locations by sector",
          x=NULL,y="Log(metric)")+
     coord_flip()

WriteChart(econ_perf_plot_not_n,"final-report-figures/",w=9,h=6)

#Plot normalised
performance_raw_df <- ttwa_merged_w_norm %>%
     filter(metric %in% c("turn_pw","work_pb"),
            grepl("norm",variable)==TRUE,
            grepl("All Industries",variable)==FALSE,
            period.string=="second.period") %>% droplevels()

#Reorder factors (based on turnover per worker)
#Get ordered sectors
sectors_ranked_by_tpw <- performance_raw_df %>% 
     filter(metric=="turn_pw") %>%
     group_by(variable) %>% summarise(median=median(value,na.rm=T)) %>%
     arrange(desc(median)) %>% extract(,"variable") %>% as.data.frame()
#Reorder
performance_raw_df$variable <- ReorderFactor(
     performance_raw_df$variable,sectors_ranked_by_tpw$variable,z=T)

#Tidy variable names
levels(performance_raw_df$variable) <- gsub("_norm","",levels(performance_raw_df$variable))

levels(performance_raw_df$metric) <- c("Turnover per worker","Average business size")


#Get labels
#We apply over a split variable and extract the top 5 areas for each sector/metric
     #after focusing on areas on the top quartile of employment

ttwa_econ_perf_labels <- ldply(split(performance_raw_df,
                                     list(performance_raw_df$metric,
                                       performance_raw_df$variable)),
                                     function(x){
                                          #Extract my variable
                                          my_var <- x$variable[[1]] %>% as.character()
                                          x$ttwa.name <- as.character(x$ttwa.name)
                                          
                                          #Extract my metric
                                          my_metric <- x$metric[[1]] %>% as.character()
                                          
                                          #Use it to find top locations
                                             #in ttwa_merged_norm
                                          valid_ttwas_df <- ttwa_merged_w_norm %>%
                                               filter(period.string=="first.period",
                                                      metric=="employment",
                                                      variable==my_var,
                                                      !is.na(value))
                                          
                                          valid_ttwas <- valid_ttwas_df$ttwa.name[
                                               valid_ttwas_df$value > quantile(valid_ttwas_df$value,
                                                                             na.rm=T)[[4]]]
                                         
                                          top_5_areas <-
                                               x %>% filter(ttwa.name %in% valid_ttwas) %>%
                                               arrange(desc(value)) %>%
                                               as.data.frame() %>% extract(1:5,"ttwa.name")
                                          
                                          ttwa_lab <- data.frame(metric=my_metric,
                                                                 variable=my_var,
                                                                 tops=paste(c("Top Areas:",top_5_areas),
                                                                            collapse="\n "))
                                          return(ttwa_lab)
                                     },.id=NULL)


#Plot
econ_perf_plot <- ggplot(data=performance_raw_df,
                         aes(x=variable,y=log(value)))+
     geom_boxplot(fill="#ffeda0",outlier.size=0.2)+
     geom_point(position=position_jitter(width=0.3), alpha=0.1,
                colour="orange",size=1)+
     geom_text(data=ttwa_econ_perf_labels,
                      aes(label=tops,y=3.5),size=1.5,hjust=1)+
     facet_grid(.~metric)+
     geom_hline(yintercept=0,colour="darkblue",linetype=2)+
     labs(title="Creative sales per worker and average firm size \n in UK locations by sector, normalised by other industries",
          x=NULL,y="Log(normalised score value)")+
     coord_flip()

WriteChart(econ_perf_plot,"final-report-figures/",w=9,h=6)

#LOOK AT CHANGE BY SECTOR, ALSO NORMALISING BY "GENERAL GROWTH"
#How? Split by variable, metric and location, and return change
     #where second period is divided by first period. 
     #Also need to consider the normalised variables. Once
     #we have that, it's just a matter of plotting with the right facets.
ttwa_econ_change_df <- ldply(
     split(ttwa_merged_w_norm,
           list(ttwa_merged_w_norm$metric,ttwa_merged_w_norm$ttwa.name)),
     function(x){
          #Remove the normalised scores (irrelevant)
          x2 <- x %>% filter(grepl("norm",variable)==FALSE)
          
          #Cast to create a third "change" row;
               #Convert period.string to character to facilitate this.
          casted_df <- x2 %>% dcast(ttwa.name+period.string+metric~variable) %>%
               mutate(period.string=as.character(period.string))
          
          #Create change variable
          casted_df[3,-c(1:3)] <- casted_df[2,-c(1:3)]/
               casted_df[1,-c(1:3)]
          
          #Rename variables
          casted_df$period.string[3] <- "change"
          casted_df$ttwa.name[3] <- casted_df$ttwa.name[2]
          casted_df$metric[3] <- casted_df$metric[2]
          
          #Normalise numerical variables by the "All industries variable"
          norm_names <- paste0(names(casted_df[,-c(1:3)]),"_norm")
          
          casted_df[,norm_names] <- sapply(
               casted_df[,-c(1:3)],function(x){
                    n <- x/casted_df[,"All Industries"]
                    return(n)
               })
          return(casted_df)
     },.id=NULL) %>% melt(id.vars=c("ttwa.name","period.string",
                                        "metric")) %>%
     mutate(is_normalised=ifelse(grepl("norm",variable)==TRUE,
                                 "normalised","not.normalised"),
            variable = gsub("_norm","",as.character(variable)))
   
#Plot.
#Focus only on the change variable, only on normalised varianles
ttwa_econ_change_to_plot <- ttwa_econ_change_df %>%
     filter(period.string=="change",
            variable!="All Industries",
            value < 10,
            value >0.05)
            #is_normalised=="not.normalised")

#Reorder factors usign the mean of growth across four variables
industries_ranked_change <- ttwa_econ_change_to_plot %>%
     filter(is_normalised=="not.normalised") %>%
     group_by(variable) %>% summarise(mean=mean(value,na.rm=T)) %>%
     arrange(desc(mean)) %>% as.data.frame() %>% extract(,"variable")

ttwa_econ_change_to_plot$variable <- ReorderFactor(ttwa_econ_change_to_plot$variable,
                                                   y=industries_ranked_change,
                                                   z=T)

ttwa_econ_change_to_plot$is_normalised <- factor(ttwa_econ_change_to_plot$is_normalised,
                                                 levels=c("not.normalised","normalised"))

levels(ttwa_econ_change_to_plot$is_normalised) <- c("Not normalised","Normalised")

#Rename variables
levels(ttwa_econ_change_to_plot$metric) <- c("Number of businesses",
                                             "Turnover","Employment",
                                             "Turnover per worker",
                                             "Average firm size")
ttwa_econ_change_to_plot$metric <- 
     factor(ttwa_econ_change_to_plot$metric,
            levels=rev(levels(ttwa_econ_change_to_plot$metric)))
     
#My palette for filling values.
change_palette <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')

econ_change_plot <- ggplot(ttwa_econ_change_to_plot,
                           aes(x=variable,y=log(value),fill=metric)) +
     geom_boxplot(outlier.size=0.2)+
     labs(title="Economic performance of creative industries and their subsectors, 2007-14",
          fill="Measure",x=NULL,y="log(change between first and last period)")+
     geom_hline(yintercept=0,colour="darkblue")+
     scale_fill_manual(values=change_palette,
                       limits=rev(levels(ttwa_econ_change_to_plot$metric)))+
     facet_grid(.~is_normalised)+
     coord_flip()
     
WriteChart(econ_change_plot,"final-report-figures/",w=9,h=6)

#Change as a binary variable (grew / didn't grow)
ttwa_econ_change_to_plot_binary <- 
     ttwa_econ_change_to_plot %>%
     mutate(grew=value>1) %>% 
     group_by(variable,metric,is_normalised) %>%
     summarise(grew_tot=mean(grew))

econ_change_plot_binary <- ggplot(data=ttwa_econ_change_to_plot_binary,
                                  aes(x=variable,y=grew_tot,fill=metric))+
     geom_hline(yintercept=0.5,colour="darkblue",linetype=2)+
     geom_bar(stat="identity",width=0.5,
              position=position_dodge(width=0.8),colour="darkgrey",size=0.1)+
     facet_grid(.~is_normalised)+coord_flip()+
     scale_fill_manual(values=change_palette,
                       limits=rev(levels(ttwa_econ_change_to_plot_binary$metric)))+
     scale_y_continuous(label=percent)+
     labs(title="Economic performance of creative industries by sector, binary outcomes (2007,2014)",
          y="% of areas that experienced growth in sector",x=NULL,fill="Metric")
WriteChart(econ_change_plot_binary,"final-report-figures/",w=9,h=6)

#Concentration
#What are the concentration levels in the creative industries in the UK?
#% jobs in top locations, how do they compare with total levels of employment.

#We use a combination of ttwa all cis, 


#Plot levels of concentration in business and employment, by sector.
#Remove turn_pw and work_pb
concentration_raw_df <- ttwa_merged %>% filter(variable %in% 
                                                    c("business.count",
                                                      "turnover",
                                                      "employment")) %>%
     droplevels()

concentration_raw_df$variable))[[1]]
#Extract info
concentration_prop_df <- ldply(split(concentration_raw_df,
                                     list(concentration_raw_df$period.string,
                                          concentration_raw_df$industry,
                                          concentration_raw_df$variable)),
                               function(x){
                                    my_df <- x %>% arrange(desc(value)) %>%
                                         mutate(prop=value/sum(value,na.rm=T),
                                                prop2=cumsum(prop))
                                    my_df$index = 1:nrow(my_df)
                                    return(my_df)
                               },.id=NULL) %>% select(-ttwa.name) %>%
     filter(variable!="turnover") %>% droplevels()

#Focus on the current period
concentration_prop_df_current <- concentration_prop_df %>%
     filter(period.string=="second.period")

#Palette for plotting
my_palette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')

#Order of variables
ranking <- concentration_prop_df_current %>% filter(index==20) %>%
     select(-value) %>%
     group_by(industry) %>% summarize(mean=mean(prop2)) %>%
     arrange(desc(mean))

concentration_prop_df_current$industry <- 
     ReorderFactor(concentration_prop_df_current$industry,
                                                y=ranking$industry,z=F)
#Change levels
concentration_prop_df_current$variable <-
     factor(concentration_prop_df_current$variable,
            levels=c("employment","business.count"))

levels(concentration_prop_df_current$variable) <-
     c("Employment","Business Count")
     
#Plot
conc_plot <- ggplot(data=concentration_prop_df_current,aes(x=index,
                                               y=prop2,
                                               group=industry,
                                               colour=industry))+
     geom_line(size=0.9,alpha=0.8)+
     #geom_smooth() +
     scale_y_continuous(label=percent)+
     scale_color_manual(values=my_palette)+
     labs(title="Distribution of creative employment and business, average 2011-2014",
           y="% of economic activity covered",x="Areas (TTWAs) decreasing by size",
          colour="industry")+
     facet_grid(variable~.)

WriteChart(conc_plot,"final-report-figures/",w=9,h=6)

#Change in concentration
#We will use concentration_prop_df (with both periods) but
#focusing on the top 50 areas for each sector. We will
#order them by the % of activity covered by those top 50 in the 3 metrics
     #we are considering
concentration_prop_df_top50 <- concentration_prop_df %>%
     filter(index<=30)

#Then: get cumulative change at different positions in the ranking
concentration_change_cs <- ldply(split(concentration_prop_df_top50,
                                            list(concentration_prop_df_top50$industry,
                                                 concentration_prop_df_top50$variable)),
                                      function(x){
                                           #For each position in the ranking,
                                             #we look at the change in level of activity.
                                           wide <- dcast(x,index+industry+variable~period.string,
                                                         value.var = "prop") %>%
                                                mutate(change=second.period-first.period,
                                                       cum_change=cumsum(change))
                                           return(wide)
                                      },.id=NULL)

#Then: get ranking of industries for plotting?
#Do it by cumchange in level50
concentration_change_ranking <- concentration_change_cs %>%
     filter(index==30) %>% select(industry,variable,index,cum_change) %>%
     group_by(industry) %>%
     summarise(mean=mean(cum_change)) %>%
     arrange(desc(mean)) %>% as.data.frame() %>% extract(,"industry")

concentration_change_cs$industry <- as.factor(concentration_change_cs$industry)

#Reorder levels
concentration_change_cs$industry <- ReorderFactor(concentration_change_cs$industry,
                                                  concentration_change_ranking,
                                                  z=F)

#Add line breaks to labels
levels(concentration_change_cs$industry) <- 
     gsub(" ","\n",levels(concentration_change_cs$industry))

#Create palette
conc_change_palette <- c('#e41a1c','#377eb8')

change_conc <- ggplot(data=concentration_change_cs,
                      aes(x=index,y=cum_change,
                          group=variable,colour=variable))+
     geom_line(size=1,alpha=0.8)+
     scale_y_continuous(label=percent)+
     scale_x_continuous(labels=seq(0,30,10),
                        breaks=seq(0,30,10))+
     scale_colour_manual(values=conc_change_palette,
                         labels=c("Business count","Employment"))+
     geom_hline(yintercept=0,colour="black",linetype=2)+
     labs(title="Change in concentration in top 30 areas by creative subsector",
          x="Top 50 areas in decreasing order of importance",
          y="Cumulative change in economic activity represented by top areas",
          fill="Metric of activity")+
     facet_grid(.~industry)+
     theme(legend.position="bottom",
           panel.margin=unit(0.75,"lines"),
           axis.text.x=element_text(size=8))

WriteChart(change_conc,"final-report-figures/",w=9,h=6)     
     
#Then: change in concentration by region and industry.
#Need to merge this with a TTWA-Region lookup
ttwa_region_lu <- all.cis_ttwa %>% filter(year=="2014") %>%
     select(ttwa.name,region)

#And get big region
ttwa_region_lu$big_region <- sapply(ttwa_region_lu$region,
                                    function(x){
                                         return(big_regions[[x]])
                                    })

#Done
ttwa_econ_change_to_plot_region_binary <- 
     ttwa_econ_change_to_plot %>% 
     merge(ttwa_region_lu,by="ttwa.name") %>%
     group_by(variable,metric,big_region,is_normalised) %>%
     summarise(grew_mean=mean(value)-1) %>%
     filter(metric %in% c("Employment","Number of businesses"),
            is_normalised=="Normalised") %>% select(-is_normalised)
#levels(ttwa_econ_change_to_plot$is_normalised)

#Order regions by average scores.
region_change_rankings <- ttwa_econ_change_to_plot_region_binary %>%
     group_by(big_region) %>%
     #filter(metric=="Employment") %>%
     summarise(mean=mean(grew_mean)) %>% arrange(desc(mean)) %>%
     as.data.frame() %>% extract(,"big_region")

#Reorder region factors
ttwa_econ_change_to_plot_region_binary$big_region <- 
     ReorderFactor(ttwa_econ_change_to_plot_region_binary$big_region,
                   region_change_rankings,z=T)

#Palette
my_palette2 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#cab2d6','#ff7f00')


#Plot
region_change_plot <- ggplot(data=ttwa_econ_change_to_plot_region_binary,
                             aes(x=big_region,y=grew_mean,fill=variable))+
     geom_hline(yintercept=0,colour="darkgrey")+
     geom_bar(stat="identity",position="dodge",width=0.8,
              colour="darkgrey",size=0.1)+coord_flip()+
     scale_fill_manual(values=rev(my_palette2),
                       limits=rev(levels(ttwa_econ_change_to_plot_region_binary$variable)))+
     scale_y_continuous(label=percent)+
     labs(title="Mean growth in areas (TTWAs) in region, normalised, 2010-2014",
          y="Mean growth (%)",x=NULL,fill="Creative subsector")+
     facet_grid(.~metric)

WriteChart(region_change_plot,"final-report-figures/",w=9,h=6)

#Change in % represented by every region.
#First create a data frame aggregating levels of activity over region
ttwa_region_concentration_change_df <- 
     ttwa_merged_w_norm %>% 
     merge(ttwa_region_lu,by="ttwa.name") %>%
     filter(metric %in% c("business.count","employment"),
            grepl("norm",variable)==FALSE) %>%
     dcast(ttwa.name+big_region+period.string+variable~metric) %>%
     group_by(big_region,period.string,variable) %>%
     summarise(region_emp = sum(employment,na.rm=T),
               region_business.count = sum(business.count,na.rm=T)) %>%
     melt(id.vars=c("big_region","period.string","variable")) %>%
     droplevels()
names(ttwa_region_concentration_change_df)[4] <- "metric"
    
ttwa_region_concentration_change_df2 <- 
     ldply(split(ttwa_region_concentration_change_df,
                 list(ttwa_region_concentration_change_df$variable,
                 ttwa_region_concentration_change_df$metric)),
           function(x){
                x2 <- x %>% dcast(big_region+variable+metric~period.string) %>%
                     mutate(period_1_perc=first.period/sum(first.period),
                            period_2_perc=second.period/sum(second.period),
                            change = period_2_perc - period_1_perc)
                return(x2)   
           },.id=NULL) %>%
     filter(variable!="All Industries")

#Need to reorder factors for visualisation
ttwa_region_concentration_change_df2$variable <- 
     ReorderFactor(ttwa_region_concentration_change_df2$variable,
                   levels(ttwa_econ_change_to_plot_region_binary$variable),
                   z=F)

#And reorder regions by concentration levels
regions_conc_change_ranked <- 
     ttwa_region_concentration_change_df2 %>%
     select(big_region,variable,change) %>%
     group_by(big_region) %>%
     summarise(mean_change = mean(change,na.rm=T)) %>%
     arrange(desc(mean_change)) %>% as.data.frame() %>%
     extract(,"big_region")

ttwa_region_concentration_change_df2$big_region <- 
     ReorderFactor(ttwa_region_concentration_change_df2$big_region,
                   regions_conc_change_ranked,z=T)

#Change in variable names and palettes
#We use my_palette 2

levels(ttwa_region_concentration_change_df2$metric) <- 
     c("Employment","Business Count")

conc_change_plot_reg <- ggplot(data=ttwa_region_concentration_change_df2,
                aes(x=big_region,y=change,fill=variable)) +
     geom_hline(yintercept=0,colour="darkgrey")+
     geom_bar(stat="identity",position="dodge",width=0.7)+
     facet_grid(.~metric)+
     scale_fill_manual(values=rev(my_palette2),
                       limits=rev(levels(ttwa_region_concentration_change_df2$variable))) +
     scale_y_continuous(labels=percent)+
     labs(title="Change in % of economic activity represented by areas in the region",
          fill="Creative sector/subsector",y="Change in % of economic activity represented by area")+
     coord_flip()

WriteChart(conc_change_plot_reg,"final-report-figures/",w=9,h=6)

#Interlude: How do we create a palette based on two colours?
#We created some functions to do it.

#Colours with RGB white,yellow,blue,green
colours <- list(c(224,224,224),
                c(255,255,102),
                c(40,40,255),
                c(78,205,47))

palette_2_vars <- Get_RGB_values(colours,dims=20)

#Create a function to produce alphas

Get_Alphas <- function(lowest,dims) {
     mat <- matrix(NA,dims,dims)
     mat[1,] <- seq(lowest,1,length.out = dims)
     mat[,1] <- seq(lowest,1,length.out = dims)
     mat[,dims] <- seq(1,1,length.out=dims)
     
     for (i in 2:(ncol(mat))) {
          mat[i,] <- seq(mat[i,1],mat[i,dims],length.out=dims)
     }
     return(mat)
}

alpha_values <- Get_Alphas(lowest=0.4,dims=20)


#MAPPING
library(ggrepel)
library(maptools)
library(gridExtra)
library(grid)


#Import the shapefile as a df
ttwa_shape <- CreateTTWAmapping_df_2011() %>%
     rename(ttwa.name=TTWA11NM)

#Also read the shapefile to get the centroids
uk_sh <-readOGR("metadata/Travel_to_Work_Areas_(UK)_2011_boundaries_(super_generalised_clipped)_V3/",
               layer="TTWA_2011_UK_BSC_V3")

uk_cen <- cbind(ttwa.name=uk_sh@data$TTWA11NM,as.data.frame(coordinates(uk_sh))) %>%
     rename(long=V1,lat=V2)

#We will merge the geo df with other dfs capturing TTWA activity/change
#-> these ones.

     #1. Use all cis TTWA
     #Select relevant variables and melt (to split by variables later)
     all_cis_mapping_1 <- all.cis_ttwa %>%
          filter(year!="2010") %>%
          select(period.string,ttwa.name,
                 business.count,business_lq,
                 employment,emp_lq) %>%
          melt(id.vars=c("ttwa.name","period.string")) %>% tbl_df()
     
     #Split apply combine
     all_cis_for_mapping <- ldply(split(all_cis_mapping_1,
                                        list(all_cis_mapping_1$ttwa.name,
                                             all_cis_mapping_1$variable)),
                                  function(x){
                                       x2 <- dcast(x,ttwa.name+variable~period.string) %>%
                                            rename(metric=variable)
                                       x2$change <- (x2$second.period/x2$first.period) -1
                                       x3 <- melt(x2,id.vars=c("ttwa.name","metric"))
                                       return(x3)
                                  },.id=NULL) %>%
          mutate(industry.short="All creative industries")
     
     #Widen df to merge with shapefiles
     #This is not really needed)
     all_cis_for_mapping_wide <- dcast(all_cis_for_mapping,
                                       ttwa.name~metric+variable)
     
     #2. Use subsector ttwa.
     #Get relevant variables
     subsectors_ttwa_mapping_1 <- subsectors_ttwa %>% 
          select(year,industry.short,ttwa.name,
                 business.count,business_lq,
                 employment,emp_lq) %>%
          mutate(period.string=ifelse(year=="2007_10","first.period","second.period")) %>%
          select(-year) %>%
          melt(id.vars=c("period.string","industry.short","ttwa.name")) %>%
          rename(metric=variable)
     
     
     #Split, apply combine (also including industry)
     subsectors_ttwa_mapping <- ldply(split(subsectors_ttwa_mapping_1,
                                            list(subsectors_ttwa_mapping_1$ttwa.name,
                                                 subsectors_ttwa_mapping_1$industry.short,
                                                 subsectors_ttwa_mapping_1$metric)),
                                      function(x){
                                           if(nrow(x)==2) {
                                                x2 <- dcast(x,ttwa.name+industry.short+metric~period.string)
                                                x2$change <- (x2$second.period/x2$first.period) -1
                                                x3 <- melt(x2,id.vars=c("ttwa.name","industry.short","metric"))
                                                return(x3)
                                           }
                                      },.id=NULL)
     
     #3. Rbind all CI and subsector DFs
     ttwa_ci_data_mapping <- rbind(subsectors_ttwa_mapping,all_cis_for_mapping) 

#4. Split by metric (employment or business), 
     #create a ranking in top for both and use that to select
     #the right colour and alphas in the palette_2_vars and
          #values_alphas matrices
#First: create a variable we can use to split (LQ or employment)
ttwa_ci_data_mapping <- ttwa_ci_data_mapping %>%
     mutate(var_type=ifelse(grepl("lq",metric)==TRUE,"lq","total"))

                                

#colours_2 <- list(c(232,232,232),c(190,100,172),c(90,200,200),c(59,73,148))


#Extract plotting objects                                  
plotting_objects <- Get_Sector_Mapping_Df(qs = 4)

#We then need to feed these to a mapping function (which
     #subsets by sector)

#Output maps
my_maps <- lapply(unique(ttwa_ci_data_mapping$industry.short),
                  Make_Sector_Maps)
names(my_maps) <- unique(ttwa_ci_data_mapping$industry.short)
#Print maps (with viewports)


for (i in 1:length(my_maps)) {
     #Finally: Matrix legend
     legend_mat <- plotting_objects[[3]] %>%
          melt()
     
     leg_plot_cur <- ggplot(data=legend_mat,
                            aes(x=Var2,y=Var1,fill=value))+
          geom_tile()+
          scale_fill_identity(guide="none")+
          labs(title="Current concentration \n colour legend",
               x="Business concentration \n (quantile)",
               y="Employment concentration \n (quantile)")+
          theme(title=element_text(size=5),
                axis.text=element_text(size=4))
     
     leg_plot_ch <- ggplot(data=legend_mat,
                           aes(x=Var2,y=Var1,fill=value))+
          geom_tile()+
          scale_fill_identity(guide="none")+
          labs(title="Change in concentration \n colour legend",
               x="Business concentration \n increase (quantile)",
               y="Employment concentration \n increase (quantile)")+
          theme(title=element_text(size=5),
                axis.text=element_text(size=4))
     
     vp <- viewport(x=0.101,y=0.8,width=0.2,height=0.21)
     vp2 <- viewport(x=0.601,y=0.8,width=0.2,height=0.21)
     
     name <- names(my_maps)[i]
     
     pdf(paste0("final-report-figures/map_",name,".pdf"),w=9,h=6)
     print(my_maps[[i]])
     print(leg_plot_cur,vp=vp)
     print(leg_plot_ch,vp=vp2)
     dev.off()
}
     

#Preliminary cluster analysis
#1st task: Identify similar sets of industries.
#Using locational correlation data (do industries tend to locate in the same places?)
#Using correlational data (do industries tend to employ people from the same
#occupations)

#Locational correlation.
#This is based on subsectors_ttwa
#Create a data.frame with columns for the variables we're interested in, and correlate.

#Produce correlation matrices
#Get DF with relevant data (industry, year, ttwa.name)
loc_correlation_tmp1 <- subsectors_ttwa %>% filter(
     industry != "All creative industries") %>%
     select(industry,year,ttwa.name,business_lq,emp_lq) %>% 
     mutate(period.string=ifelse(year=="2007_10","first.period","second.period")) %>%
     select(-year) %>%
     melt(id.vars=c("ttwa.name","industry","period.string"))

loc_correlation_tmp1$industry.short <- unlist(shorter.labels[loc_correlation_tmp1$industry])

loc_correlation_tmp1 <- loc_correlation_tmp1 %>% select(-industry)

#Extract correlation matrices.
     #Pseudo-code: split by industry, calculate change in concentration,
          #produce correlation matrix for concentration and change.
loc_correlation_matrices <- lapply(split(loc_correlation_tmp1,
                                         loc_correlation_tmp1$variable),
                                   function(x){
                                        #Widen to get change in specialisation
                                        x_wide <- x %>% select(-variable) %>%
                                             dcast(ttwa.name+industry.short~period.string,
                                                   value.var="value") %>%
                                             mutate(change=second.period-first.period)
                                        
                                        #Run correlations
                                        cor_mats <- lapply(list("second.period","change"),
                                                           function(x){
                                                                my_x <- x_wide %>% 
                                                                     select_("ttwa.name","industry.short",x) %>%
                                                                     dcast(ttwa.name~industry.short)
                                                                my_mat <- cor(my_x[,-1],
                                                                              use="pairwise.complete.obs")
                                                                return(my_mat)
                                                           })
                                        names(cor_mats) <- list("second.period","change")
                                        return(cor_mats)
                                   })

my_colocation_matrices <- unlist(loc_correlation_matrices,recursive = F)

#Generate hierarchical clusters with occupational data
pdf("final-report-figures/dendrograms.pdf")
dendronames <- names(my_colocation_matrices)
for (i in 1:length(my_colocation_matrices)) {
     d <- dist(my_colocation_matrices[[i]],method="euclidean")
     fit <- hclust(d,method="ward")
     plot(fit,main=paste("Cluster dendrogram - ",dendronames[i]))
}
dev.off()

#Colour branches based in our clusters
#install.packages("dendextend")
library(dendextend)

pdf("final-report-figures/dendrograms.pdf",w=9,h=6)
par(mfrow=c(1,2))
par(mar=c(5,5,5,5))
#Create two dendograms
ds <- lapply(as.list(c("business_lq.second.period","emp_lq.second.period")),
             function(x){
                  my_c <- my_colocation_matrices[[x]]
                  colnames(my_c) <- sapply(colnames(my_c),function(x){
                       return(unlist(str_split(x," |,"))[[1]])
                  },USE.NAMES = F)
                  
                  rownames(my_c) <- sapply(colnames(my_c),function(x){
                       return(unlist(str_split(x," |,"))[[1]])
                  },USE.NAMES = F)
                       
                  d <- dist(my_c,method="euclidean")
                  fit <- hclust(d,method="ward")
                  p <- as.dendrogram(fit)
                  return(p)
             })

d1 <- color_branches(ds[[1]],k=3,groupLabels = T)
d2 <- color_branches(ds[[2]],k=3,groupLabels = T)

plot(d1,lab.cex=0.8,nodePar=list(lab.cex=0.9),main="Business co-location dendrogram")
plot(d2,lab.cex=0.8,nodePar=list(lab.cex=0.9),main="Employment co-location dendrogram")
dev.off()

plot(ds[[1]],main="Business co-location dendrogram")
plot(ds[[2]],main="Employment co-location dendrogram")
dev.off()

#Tasks:
#Generate metrics for our key variables:
#business and employment lqs 
#along the 6 clusters we have identified.
#Select top locations in each of these groups.

#Code: extract the sectors from employment lq and business lq, create a new
#dataframe with those sectors, and re-calculate employment, business count,
#turnover, and their lqs.

#First extract the labels
#Business counts
business.lq_clusters <- my_colocation_matrices$business_lq.second.period %>% dist(method="euclidean") %>%
     hclust(method="ward") %>% cutree(h=1) %>% Make_keyed_frame()

business.lq_clusters$label <- NA


business.lq_clusters$label[business.lq_clusters$x==1] <- "bc_services"
business.lq_clusters$label[business.lq_clusters$x==2] <- "bc_architecture"
business.lq_clusters$label[business.lq_clusters$x==3] <- "bc_content"

#Employment
emp.lq_clusters <- my_colocation_matrices$emp_lq.second.period %>% dist(method="euclidean") %>%
     hclust(method="ward") %>% cutree(h=1.4) %>% Make_keyed_frame()

emp.lq_clusters$label <- NA
emp.lq_clusters$label[emp.lq_clusters$x==1] <- "emp_services"
emp.lq_clusters$label[emp.lq_clusters$x==2] <- "emp_content"
emp.lq_clusters$label[emp.lq_clusters$x==3] <- "emp_publishing"

#Automate this process
#Input a variable, return a df with scores by location
#on that variable (in totals as well as LQs)
#Let's use TTWA merged.
#We have ttwa_merged

ttwa_merged_labs <- ttwa_merged %>% filter(variable %in% c("business.count",
                                                           "employment"),
                                           industry!="All creative industries") %>%
     droplevels()


#Generate clustering scores     
_df <- ldply(list("business"=business.lq_clusters,
                                 "employment"=emp.lq_clusters), function(x) {
                                      #Subset df
                                      #Find the variable to do this (inside the
                                        #x' label)
                                      my_var <- ifelse(grepl("bc",x$label[1])==TRUE,
                                                       "business.count","employment")
                                      
                                      #Add labels
                                      df_labelled <- ttwa_merged_labs %>%
                                           filter(variable==my_var) %>%
                                           merge(x,by.x="industry",by.y="names",
                                                 all.x=T) %>%
                                           rename(cluster=label)
                                      
                                      #Keep all industries (for normalising)
                                      df_labelled$cluster[is.na(df_labelled$cluster)] <-
                                           df_labelled$industry[is.na(df_labelled$cluster)]
                                     
                                      #Group by ttwa.name,cluster and period, and produce
                                      #aggregate stats. We will then create measures of
                                      #Clustering, and change. That should be that.
                                      ind_cluster_stats <- df_labelled %>% select(-x) %>%
                                           group_by(ttwa.name,period.string,cluster) %>%
                                           summarise(total=sum(value,na.rm=T)) %>%
                                           dcast(ttwa.name+period.string~cluster) %>%
                                           ungroup()
                                      
                                      #Then we dcount and divide all of them 
                                        v_names <- names(ind_cluster_stats)[3:ncol(ind_cluster_stats)]
                                      
                                     #Generate LQs for each of them.
                                        ind_cluster_stats[,paste0(v_names,"_lq")] <- 
                                             sapply(ind_cluster_stats[,v_names],
                                                    function(x){
                                                         lq <- (x/sum(x,na.rm=T))/
                                                              (ind_cluster_stats[,"All Industries"]/
                                                                    sum(ind_cluster_stats[,"All Industries"],na.rm=T))
                                                         return(lq)
                                                    })
                            
                                        names(ind_cluster_stats)
                                   #Get change
                                   ind_cluster_stats_w_change <- 
                                        ldply(split(ind_cluster_stats,
                                                    ind_cluster_stats$ttwa.name),
                                              function(x){
                                                   x[3,-c(1:2)] <- (x[2,-c(1:2)]/
                                                        x[1,-c(1:2)])-1
                                                   x$ttwa.name[3] <- x$ttwa.name[2]
                                                   x$period.string[3] <- "change"
                                                   return(x)
                                              },.id=NULL) %>% melt(
                                                   id.vars=c("ttwa.name","period.string"))
                                   return(ind_cluster_stats_w_change)
                                 }) %>% filter(
                                      grepl("All Industries",variable)==FALSE) %>%
     mutate(var_type=ifelse(grepl("lq",variable)==TRUE,"lq","total"),
            variable = gsub("_lq","",variable))
                                   
#Get_Clusters function extracts top clusters based
     #on a variable (second period or change)
     #a size threshold to be considered, and a top number of clusters to return


Get_UK_Clusters <- function(option,threshold=0.75,return=10) {
     #Option: second period or change?
     if(option=="second.period"){
          my_df <- _df %>% filter(period.string=="second.period") %>%
               select(-.id)
          gimme_tops <- ldply(
               split(my_df,my_df$variable),
               function(x){
                    x_tops <- x %>% dcast(ttwa.name+period.string~var_type) %>%
                         mutate(is_big = total >= quantile(total,probs=c(threshold),
                                                           na.rm=T)) %>%
                         filter(is_big==TRUE) %>% arrange(desc(lq)) %>% 
                         as.data.frame() %>%
                         extract(1:return,"ttwa.name")
                    return(x_tops=data.frame(ttwa.name=x_tops))
               })
          return(gimme_tops)
     }
     if(option=="change") {
          my_df <- _df %>% filter(period.string!="second.period") %>%
               select(-.id)
          gimme_tops <- ldply(
               split(my_df,my_df$variable),
               function(x){
                    selectable <- x %>% filter(period.string=="first.period",
                                               var_type=="total") %>%
                         mutate(is_big=value >= quantile(value,probs=c(threshold),
                                                         na.rm=T)) %>%
                         filter(is_big==TRUE) %>% as.data.frame() %>%
                         extract(,"ttwa.name")
                    
                    x_tops <- x %>% filter(period.string=="change",
                                           var_type=="lq") %>%
                         filter(ttwa.name %in% selectable) %>% 
                         arrange(desc(value)) %>%
                         extract(1:return,"ttwa.name")
                    return(x_tops=data.frame(ttwa.name=x_tops))
               })
          return(gimme_tops)
     }}

#Now what
# my_clusters_conc <- Get_UK_Clusters("second.period",threshold=0.75,return=10) %>%
#      mutate(type="concentration") %>% droplevels()
# my_clusters_growth <- Get_UK_Clusters("change",threshold=0.75,return=10) %>%
#      mutate(type="change") %>% droplevels()
# 
# #my_clusters_final <- rbind(my_clusters_conc,my_clusters_growth)

my_clusters_final2 <- ldply(list("concentration"="second.period",
                            "change"="change"),Get_UK_Clusters,.id="type") %>% droplevels()

#sort(table(my_clusters_final2$ttwa.name))

#Let's see what they look like.
agglom_stats_selected <- _df %>% filter(ttwa.name %in% my_clusters_final2$ttwa.name) %>%
     tbl_df()

agglom_stats_for_hm <- agglom_stats_selected %>% select(-.id) %>%
     filter(period.string !="first.period",var_type!="total") %>% 
     select(-var_type) %>% dcast(ttwa.name~period.string+variable)

#Extract deciles for plotting
agglom_stats_for_hm[,paste0(names(agglom_stats_for_hm[-1]),"_qt")] <-
     sapply(agglom_stats_for_hm[,names(agglom_stats_for_hm)[-1]],
            function(x){
                 cut_v <- cut(x,breaks = quantile(x,probs=seq(0,1,0.10),
                                                  na.rm = T),
                              labels = FALSE)
                 cut_v[is.na(cut_v)] <- 0
                 return(cut_v)
            })

#Get the DF ready for plotting
agglom_stats_for_hm2 <- agglom_stats_for_hm %>% melt(id.vars="ttwa.name") %>%
     filter(grepl("_qt",variable)==TRUE) %>%
     mutate(src=ifelse(grepl("change",variable)==TRUE,"Change","Concentration"))

agglom_stats_for_hm2$var_name <- sapply(agglom_stats_for_hm2$variable,
                                        function(x){
                                        get <- str_split_fixed(x,"_",2)
                                        return(get[[2]])})

#Sort clusters (by average scores)
cluster_ranks <- agglom_stats_for_hm2 %>% 
     select(ttwa.name,variable,value) %>%
     group_by(ttwa.name) %>%
     summarise(mean_score=mean(value,na.rm=T)) %>%
     arrange(desc(mean_score)) %>% as.data.frame() %>% extract(,"ttwa.name")
#Reorder variables
agglom_stats_for_hm2$src <-factor(agglom_stats_for_hm2$src,
                                  levels=c("Concentration","Change"))
     

#Reorder factors
agglom_stats_for_hm2$ttwa.name <- ReorderFactor(agglom_stats_for_hm2$ttwa.name,
                                                cluster_ranks,z=T)

#Rename Variable names
agglom_stats_for_hm2$var_name <- ifelse(grepl("^bc",agglom_stats_for_hm2$var_name)==TRUE,
                                        gsub("bc_","Business clustering: ",agglom_stats_for_hm2$var_name),
                                        gsub("emp_","Employment clustering: ",agglom_stats_for_hm2$var_name))
agglom_stats_for_hm2$var_name <- gsub("_qt","",agglom_stats_for_hm2$var_name)

     
cluster_scores <- ggplot(data=agglom_stats_for_hm2,
                 aes(x=var_name,y=ttwa.name,fill=value))+
     geom_tile(colour="azure4",size=0.001)+
     facet_grid(.~src)+
     scale_fill_gradient2(high="red",low="blue",midpoint = 5)+
     labs(title="Clustering scores for top Areas",
          x="Measure",y=NULL,fill="Decile")+
     theme(axis.text.x=element_text(angle=45,size=7,hjust=1),
           axis.text.y=element_text(size=7))

WriteChart(cluster_scores,"final-report-figures/",w=9,h=6)

#Produce a chart with average performance by area and regions.
s_cluster_perf_bubble <-
     agglom_stats_for_hm %>% merge(ttwa_region_lu,by="ttwa.name") %>%
     select(-contains("qt"))

#Get high concentration
s_cluster_perf_bubble$high_conc<- 
     apply(s_cluster_perf_bubble[,grep("second.period",names(s_cluster_perf_bubble))],
           1,
           function(x){
                return(mean(x,na.rm=T))
           })


#Get high diversity
s_cluster_perf_bubble$high_div<- 
     apply(s_cluster_perf_bubble[,grep("second.period",names(s_cluster_perf_bubble))],
           1,
           function(x){
                return(sum(x>1,na.rm=T))
           })

#Get high growth
s_cluster_perf_bubble$high_change <- 
     apply(s_cluster_perf_bubble[,grep("change",names(s_cluster_perf_bubble))],1,
           function(x){
                #highg <- x > 0
                #return(sum(highg))
                return(mean(x,na.rm=T))
           })

#Subset df for plotting
s_cluster_perf_bubble2 <- s_cluster_perf_bubble %>%
     select(ttwa.name,big_region,contains("high"))

#Plot
#Reorder area levels for colouring

s_cluster_perf_bubble2$big_region <- factor(s_cluster_perf_bubble2$big_region,
                                       levels=c("Northern Ireland",
                                                "Scotland","North","Wales",
                                                "Midlands","East","London","South"))


cluster_perf_bubble <- ggplot(data=s_cluster_perf_bubble2,
                             aes(x=high_conc,y=high_change,
                                 fill=big_region,size=high_div))+
#      geom_jitter(position=position_jitter(w=.05,h=.05),
#                  pch=21,colour="azure4") +
     geom_hline(yintercept=mean(s_cluster_perf_bubble2$high_change),
                linetype=2,size=0.4,col="darkblue")+
     geom_vline(xintercept=mean(s_cluster_perf_bubble2$high_conc),
                linetype=2,size=0.4,col="darkblue")+
     geom_point(alpha=0.75,pch=21,colour="azure4")+
     scale_size(range=c(1,5))+
     scale_fill_manual(values=my_palette4)+
     scale_y_continuous(label=percent)+
     geom_text_repel(aes(label=ttwa.name,fill=NULL,size=NULL,colour=NULL),
                     size=1.3,segment.size=0.001)+
     labs(title="Average concentration, growth and sectoral diversity",
          x="Mean clustering",
          y="Mean growth",fill="Region",
          size="Number of sectors with high \n specialisation")
WriteChart(cluster_perf_bubble,"final-report-figures/",w=9,h=6)

#What's the coverage of the 45 areas?
clust_coverage <- all.cis_ttwa %>% filter(period.string=="second.period") %>%
     mutate(selected = ttwa.name %in% my_clusters) %>%
     group_by(selected) %>%
     summarise(tot.emp=sum(employment,na.rm=T),
               tot.turn=sum(turnover,na.rm=T),
               tot.bc=sum(business.count,na.rm=T))

#Calculate coverage
clust_coverage["props",-1] <- 100*(clust_coverage[2,-1]/
     colSums(clust_coverage[,-1]))

#Sectoral distribution.
my_clusters_w_region <- ttwa_region_lu %>%
     mutate(in_top=ifelse(ttwa.name %in% my_clusters,"yes","no"))
Percentify(prop.table(table(my_clusters_w_region$big_region,
      my_clusters_w_region$in_top),1))

my_clusters_w_region %>% filter(in_top=="yes") %>% select(ttwa.name,region) %>%
     arrange(ttwa.name) %>%
     as.data.frame()

#Produce cluster composition measures.
my_clusters <- unique(my_clusters_final2$ttwa.name)
cluster_composition_selected <- ttwa_merged %>% 
     filter(ttwa.name %in% my_clusters, 
            variable %in% c("business.count","employment"),
            period.string == "first.period",
            !(industry %in% c("All creative industries",
                              "All Industries"))) %>% select(-period.string) %>%
     droplevels()

#Calculate objects for plotting (props we use for stacked barchart and
     #herfindahls for ordering)
cluster_composition_plot_objects <- 
     lapply(split(cluster_composition_selected,
                 list(cluster_composition_selected$variable,
                      cluster_composition_selected$ttwa.name)),
           function(x){
                x$prop = x$value / sum(x$value,na.rm=T)
                
                herf = data.frame(
                     ttwa.name=x$ttwa.name[1],
                     var=x$variable[1],
                     herf=sum(x$prop^2))
                
                return(list(x,herf))
           })
                
#Extract them. We want a df with the props stuff and a
     #vector with 
#df
cluster_composition_to_plot <- ldply(cluster_composition_plot_objects,
                                     function(x){
                                          return(x[[1]])
                                     },.id=NULL)     

cluster_composition_diversity <- ldply(cluster_composition_plot_objects,
                                       function(x){
                                            return(x[[2]])
                                       },.id=NULL) %>%
     group_by(ttwa.name) %>%
     filter(var!="business.count") %>%
     summarise(mean_conc=mean(herf,na.rm=T)) %>%
     arrange(desc(mean_conc)) %>% as.data.frame() %>%
     extract(,"ttwa.name")

#Reordering of variables and renaming of levels for plotting
#Area names
cluster_composition_to_plot$ttwa.name <- ReorderFactor(
     cluster_composition_to_plot$ttwa.name,
     cluster_composition_diversity,z=F)

#Industries (by level of importance)
cluster_composition_to_plot$industry <- 
     with(cluster_composition_to_plot,
          reorder(industry,prop,FUN = mean))

levels(cluster_composition_to_plot$variable) <- c("Business count","Employment")

#Palette is my_palette2

#Plot
cluster_comp_chart2 <- ggplot(data=
                                   cluster_composition_to_plot[
                                        order(cluster_composition_to_plot$prop),],
                    aes(x=ttwa.name,y=prop,fill=factor(industry)))+
     geom_bar(stat="identity") +
     scale_fill_manual(values=my_palette2,
                       breaks=rev(levels(cluster_composition_to_plot$industry)))+
     scale_y_continuous(label=percent)+
     labs(title="Industrial composition of selected clusters",
          x=NULL,y=NULL,fill="Creative \n subsector")+
     facet_grid(.~variable) + coord_flip()+
     theme(legend.position="bottom",
           axis.text.y=element_text(size=7))

WriteChart(cluster_comp_chart2,"final-report-figures/",w=9,h=6)

#Plot cluster growth rankings.
#What exactly?
     #For each cluster, growth in CIs.
     #While alphaing over the total levels of activity



#Get a dataframe to work with
selected_ttwas_plot <- ttwa_econ_change_df %>% 
     filter(period.string %in% c("change","first.period"),
            ttwa.name %in% my_clusters,
            !(variable %in% c("All Industries","All creative industries")),
            is_normalised=="not.normalised") %>%
     droplevels() %>% select(-is_normalised)


#Produce alphas
selected_ttwas_plot2 <- ldply(split(selected_ttwas_plot,
                                    list(selected_ttwas_plot$metric,
                                         selected_ttwas_plot$variable)),
                              function(x){
                                   x_wide <- 
                                        dcast(x,
                                                  ttwa.name+variable+metric~period.string,
                                              value.var="value") %>%
                                        rename(industry=variable)
                                   x_wide$my_alpha <- 
                                        cut(x_wide$first.period,
                                            breaks = quantile(x_wide$first.period,
                                                              probs=c(0,0.5,1)),
                                                              include.lowest=TRUE,
                                            labels=1:2)
                                   x_out <- x_wide %>% select(-first.period) %>%
                                        melt(id.vars=c("ttwa.name",
                                                       "industry","metric","my_alpha")) %>%
                                        select(-variable)
                                   return(x_out)
                              },.id=NULL)

#Plot for every variable in metric.

clean_var_names <- list("business count","turnover","employment",
                        "turnover per worker","average firm size")

names(clean_var_names) <- levels(selected_ttwas_plot$metric)

selected_growth_plots <- lapply(
     levels(selected_ttwas_plot$metric), function(x){
          sub_df <- 
               selected_ttwas_plot2 %>% filter(metric == x)
          
          #Order TTWAs
          ttwas_ranked <- sub_df %>% group_by(ttwa.name) %>%
               summarise(mean_g = mean(value,na.rm=T)) %>%
               arrange(desc(mean_g)) %>% as.data.frame() %>%
               extract(,"ttwa.name")
          
          #Title
          my_title = paste("Change in",clean_var_names[[x]],"by selected area and creative subsector, 2007/10-2011/14")
          
          sub_df$ttwa.name <- ReorderFactor(sub_df$ttwa.name,
                                            ttwas_ranked,z=F)
          
          
          #Plot
          qplot <- 
               ggplot(data=sub_df,
                 aes(x=industry,y=log(value),fill=industry,alpha=my_alpha))+
               geom_bar(stat="identity",position="dodge",
                        colour="azure4",size=0.01)+
               facet_wrap(~ttwa.name,nrow = 8,ncol=6)+
               coord_flip()+
               scale_fill_manual(values=my_palette2,
                                 breaks=rev(levels(cluster_composition_to_plot$industry)))+
               scale_alpha_discrete(range=c(0.8,1),guide="none")+
               labs(title=my_title,x=NULL,y="Growth (logged)")+
               theme(axis.text.y=element_blank(),
                     axis.text.x=element_text(size=6),
                     axis.ticks.y=element_blank(),
                     legend.position="bottom",
                     strip.text=element_text(size=7,lineheight = 0.02))     
               
               return(qplot)
     }
)

pdf("final-report-figures/growth_plots.pdf",w=9,h=6)
for (i in 1:length(selected_growth_plots)) {
     print(selected_growth_plots[[i]])
}
dev.off()

#Create a bubblechart considering growth in business, growth in
     #employment and growth in sales per worker and 
     #growth in average firm sizes. Colour by regions.

#Label each location with its "top cluster"?
top_sector_cluster <- agglom_stats_for_hm %>%
     select(ttwa.name, contains("second.period")) %>%
     select(-contains("_qt"))
top_sector_cluster$top_area <- apply(top_sector_cluster[,c(2:ncol(top_sector_cluster))],1,
                                     function(x){
                                          index <- which.max(x)
                                          name_opts1 <- colnames(top_sector_cluster)[2:ncol(top_sector_cluster)]
                                          name <- name_opts1[index]
                                          name_clean <- unlist(str_split(name,"_"))[3]
                                          return(capitalize(name_clean))
                                     })


bubble_plot_chart <- 
     selected_ttwas_plot2 %>% select(-my_alpha) %>%
     filter(metric!="turnover") %>% droplevels() %>%
     group_by(ttwa.name,metric) %>%
     summarise(mean_value=mean(value,na.rm=T)) %>%
     dcast(ttwa.name~metric) %>% left_join(ttwa_region_lu,
                                           by="ttwa.name") %>%
     left_join(top_sector_cluster[,c("ttwa.name","top_area")],by="ttwa.name")
#Reorder region names for colouring
bubble_plot_chart$big_region <- factor(bubble_plot_chart$big_region,
                                         levels=c("Northern Ireland",
                                                  "Scotland","North","Wales",
                                                  "Midlands","East","London","South"))

#Get horizontal and vertical lines
x_intercept = mean(bubble_plot_chart$employment,na.rm=T)-1
y_intercept = mean(bubble_plot_chart$business.count,na.rm=T)-1

#Palette
activity_palette <- c('#8dd3c7','#ffffb3','#bebada','#fb8072')

selected_bubble_chart <- ggplot(data=bubble_plot_chart,
                aes(x=employment-1,
                    y=business.count-1,fill=big_region))+
     geom_hline(aes(yintercept = y_intercept),
                linetype=2,size=0.5,colour="darkgrey")+
     geom_vline(aes(xintercept = x_intercept),
                linetype=2,size=0.5,colour="darkgrey")+
     geom_point(
          pch=21,colour="azure4",size=2)+
     labs(title="Average growth in business count and employment by area",
          x="Average growth in employment",
          y="Average growth in  business count",
          fill="region")+
     scale_fill_manual(values=my_palette4)+
     scale_x_continuous(label=percent)+
     scale_y_continuous(label=percent)+
     geom_text_repel(aes(label=ttwa.name,fill=NULL,size=NULL,colour=NULL),
                      size=1.3,segment.size=0.001)



WriteChart(selected_bubble_chart,"final-report-figures/",w=9,h=6)

#Waste of time!

#Correlation matrices.

sector_change_corrmatr <- ldply(split(selected_ttwas_plot2,
                                      selected_ttwas_plot2$industry),
                                function(x){
                                     x2 <- x %>% select(-my_alpha) %>%
                                          dcast(ttwa.name~metric,
                                                value.var="value")
                                     x2[,-1] <- sapply(x2[,-1],log)
                                     c_mat <- cor(x2[,-1],
                                                  use="pairwise.complete.obs") %>%
                                          melt()
                                     names(c_mat) <- c("variable1","variable2","r")
                                     return(c_mat)
                                },.id="industry")

sector_change_corrmatr$variable1 <- sapply(sector_change_corrmatr$variable1,
                                           function(x){
                                                return(capitalize(clean_var_names[[x]]))
                                           })
sector_change_corrmatr$variable2 <- sapply(sector_change_corrmatr$variable2,
                                           function(x){
                                                return(capitalize(clean_var_names[[x]]))
                                           })

#Reorder levels for variables
sector_change_corrmatr$variable1 <- factor(sector_change_corrmatr$variable1,
                                           levels=c("Business count",
                                                    "Employment",
                                                    "Turnover",
                                                    "Turnover per worker",
                                                    "Average firm size"))

sector_change_corrmatr$variable2 <- factor(sector_change_corrmatr$variable2,
                                           levels=rev(c("Business count",
                                                    "Employment",
                                                    "Turnover",
                                                    "Turnover per worker",
                                                    "Average firm size")))

#And for sectors
levels(sector_change_corrmatr$industry)[5] <- "Music & \n performing arts"

sector_change_corrmatr$industry <- factor(sector_change_corrmatr$industry,
                                          levels=c("Publishing","Film, radio & TV",
                                                   "Music & \n performing arts",
                                                   "Advertising","Design","Software & digital",
                                                   "Architecture"))

#Plot                                          
sector_corrplot <- ggplot(data=sector_change_corrmatr,
                 aes(x=variable1,y=variable2,fill=r))+
     geom_tile(colour="azure4",size=0.001)+
     scale_fill_gradient2(high="red",low="blue",midpoint = 0)+
     facet_grid(.~industry)+
     labs(title="Correlations in growth rates for different variables by creative sub-sector",
          y=NULL,x=NULL,fill="Correlation coefficient")+
     theme(axis.text=element_text(size=8),
           axis.text.x=element_text(angle=45,hjust=1),
           legend.position="bottom",
           strip.text=element_text(size=9))

WriteChart(sector_corrplot,"final-report-figures/",w=9,h=3.4)


#Consider capabilities
#We need to integrate: 
     #HESA data
     #REF data
     #HE BCI data
     #Network data?

#Select variables:
     #HESA data:

hesa_ttwa_analysis <- ttwa.subject.year %>%
     filter(subject.short %in% c("art.design","compsci"),
            .id=="2013/14") %>% dcast(ttwa.name~subject.short,value.var="qual.totals")

ref_ttwa_analysis <- ref.outcomes_with.lq %>%
     filter(measure=="Overall") %>% 
     dcast(ttwa.name~subject,value.var="fte.4star_ttwa")

he_bci_analysis <- he.bci.cc_ttwa %>%
     select(ttwa.name,business.turnover,
            sme.engagement,
            sme.training,
            event.attendees)

capability_ttwa_df <- join_all(list(hesa_ttwa_analysis,ref_ttwa_analysis,he_bci_analysis),
                               by="ttwa.name",type="full") %>%
     filter(ttwa.name %in% my_clusters)

#Create deciles
capability_ttwa_df[,paste0(names(capability_ttwa_df)[-1],"_decile")] <-
     sapply(capability_ttwa_df[,names(capability_ttwa_df)[-1]],
            function(x){
                 y <- cut(x,breaks=quantile(x,probs=seq(0,1,0.1),
                          na.rm=TRUE),include.lowest = TRUE,labels=1:10)
                 y <- as.numeric(y)
                 #Make missing zeros = no activity in that area
                 y[is.na(y)] <- 0
                 return(as.numeric(y))
            })


capability_ttwa_df_dec <- capability_ttwa_df %>% select(ttwa.name,contains("_decile")) %>%
     melt(id.var="ttwa.name")

#Clean and rank variable names
ttwa_ranks <- capability_ttwa_df_dec %>% group_by(ttwa.name) %>%
     summarise(m_val=mean(value,na.rm=T)) %>% arrange(desc(m_val)) %>%
     as.data.frame() %>% extract(,"ttwa.name")

capability_ttwa_df_dec$ttwa.name <- ReorderFactor(capability_ttwa_df_dec$ttwa.name,
                                                  y=ttwa_ranks,z=T)

clean_local_asset_names <- 
     list("Qualifiers- Computer Science","Qualifiers- Arts & Design",
          "Research- Arts and Design","Research- Computer Science",
          "Knowledge Exchange- Spin-out turnover",
          "Knowledge Exchange- SME engagement",
          "Knowledge Exchange- SME training",
          "Knowledge Exchange- Event attendees")

names(clean_local_asset_names) <- levels(capability_ttwa_df_dec$variable)

capability_ttwa_df_dec$clean_name <- as.character(clean_local_asset_names[capability_ttwa_df_dec$variable])

#Reorder levels
capability_ttwa_df_dec$clean_name <- factor(capability_ttwa_df_dec$clean_name,
                                            levels=c(
                                                 "Qualifiers- Computer Science","Qualifiers- Arts & Design",
                                                 "Research- Arts and Design","Research- Computer Science",
                                                 "Knowledge Exchange- Spin-out turnover",
                                                 "Knowledge Exchange- SME engagement",
                                                 "Knowledge Exchange- SME training",
                                                 "Knowledge Exchange- Event attendees"))
#Plot
cap_heatmap <- 
     ggplot(capability_ttwa_df_dec,
          aes(y=ttwa.name,x=clean_name,fill=value))+
     geom_tile(colour="azure4",size=0.001)+
     scale_fill_gradient2(midpoint = 5,high="red",low="blue",
                          na.value = "lightgrey")+
     labs(title="Local knowledge capabilities in selected areas",
          y=NULL,x=NULL,fill="Decile of capability \n compared to other areas")+
     theme(axis.text.x=element_text(angle=45,,size=8,hjust=1))


WriteChart(cap_heatmap,"final-report-figures/",w=9,h=6)

#And a couple of bubble charts with levels of qualifiers/reseaech vs.
     #activity in relevant areas, in terms of activity and growth.
agglom_stats_bubble <- agglom_stats_for_hm2 %>%
     filter(grepl("bc_services|bc_content",variable)==TRUE) %>%
     mutate(sector_type=ifelse(grepl("services",variable)==TRUE,"Services","Content")) %>%
     select(ttwa.name,src,value,sector_type) %>%
     rename(score=value)

capab_stats_bubble <- capability_ttwa_df_dec %>%
     filter(grepl("Qualifiers|Research",clean_name)) %>%
     mutate(sector_type=ifelse(grepl("Arts",clean_name)==TRUE,
            "Content","Services"),
            cap_type=ifelse(grepl("Qualifiers",clean_name)==TRUE,
            "Qualifiers","Research")) %>%
     select(ttwa.name,cap_type,value,sector_type) %>%
     dcast(ttwa.name+sector_type~cap_type)

bubble_chart2 <- merge(agglom_stats_bubble,capab_stats_bubble,
                       by=c("ttwa.name","sector_type")) %>%
     filter(src=="Concentration")

bubble_chart2[is.na(bubble_chart2)] <- 0

bubble_chart2 %>% filter(ttwa.name=="Cambridge")


qbplot <- ggplot(data=bubble_chart2,
                 aes(x=Qualifiers,y=Research,size=score,fill=score))+
     geom_hline(yintercept = 5,linetype=2,size=0.3)+
     geom_vline(xintercept = 5,linetype=2,size=0.3)+
     geom_point(pch=21,colour="azure4",alpha=0.8)+
     geom_text_repel(aes(label=ttwa.name,fill=NULL,size=NULL,colour=NULL),
                     size=1.8,segment.size=0.1,nudge_x=-0.05)+
     facet_grid(sector_type~.)+
     labs(title="Local capabilities (Research and Qualifiers) and creative activities",
          x="Location decile in qualifier numbers",
          y="Location decile in high quality research activity",
          size="Levels of \nbusiness activity \nin sector (Decile)")+
     scale_fill_gradient2(midpoint = 5,high="red",low="blue",guide="none")
     
WriteChart(qbplot,"final-report-figures/",w=9,h=6)


