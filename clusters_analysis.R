#Preliminary cluster analysis
#1st task: Identify similar sets of industries.
     #Using locational correlation data (do industries tend to locate in the same places?)
     #Using correlational data (do industries tend to employ people from the same
          #occupations)

#Locational correlation.
#This is based on subsectors_ttwa
#Create a data.frame with columns for the variables we're interested in, and correlate.

loc_correlation_tmp1 <- subsectors_ttwa %>% filter(year == "2011_14",
                                                   industry != "All creative industries") %>%
     select(industry,ttwa.name,business.count,employment, business_lq,emp_lq)

#Produce correlation matrices
loc_correlation_matrices <- lapply(list("business.count"="business.count",
                                        "business_lq"="business_lq",
                                        "employment"="employment",
                                        "employment_lq"="emp_lq"), function(x){
                                             my_df <- loc_correlation_tmp1 %>%
                                                  dcast(ttwa.name~industry,
                                                        value.var=x)
                                             #print(pairs(my_df[,-1]))
                                             cor_mat <- cor(my_df[,-1],
                                                            use="pairwise.complete.obs",
                                                            method = "pearson")
                                             return(cor_mat)
                                        })

#Produce some heatmaps (loop over names to title them)
pdf("final-report-figures/heatmaps.pdf",width=8,height=8)
names <- names(loc_correlation_matrices)
for (i in 1:length(loc_correlation_matrices)) {
     hm <- heatmap(loc_correlation_matrices[[i]],main=names[i],
                   margins=c(20,20))
     print(hm)
}
dev.off()

#Consistent with the idea of services vs content clusters.

#This didn't work 
# #Produce matrix with coagglomeraton index based in Glaeser.
# #We need total levels of employment (get them from all.cis)
# total_activity_ttwa <- all.cis_ttwa %>% 
#      filter(year!="2007") %>%
#      select(ttwa.name,year,
#             business.count,business_local.share,
#             employment,emp_local.share) %>%
#      mutate(business_total = business.count/business_local.share,
#             employment_total=employment/emp_local.share) %>%
#      group_by(ttwa.name) %>%
#      summarise(employment_total=mean(employment_total,na.rm=T),
#                business.count_total=mean(business_total,na.rm=T)) %>%
#      ungroup() %>%
#      select(ttwa.name,contains("_total"))
# 
# #Merge with the agglomeration index
# loc_coagglomeration_index <- subsectors_ttwa %>%
#      filter(year=="2011_14",industry!="All creative industries") %>%
#      select(ttwa.name,industry,business.count,employment) %>% droplevels() %>%
#      merge(total_activity_ttwa,by="ttwa.name")
#      
# 
# coi_matrices <- lapply(list("business"="business.count",
#                             "employment"="employment"), function(x){
#                                  #Create repository matrix
#                                  sector_matrix <- matrix(data=NA,
#                                                          nrow=length(levels(loc_coagglomeration_index$industry)),
#                                                          ncol = length(levels(loc_coagglomeration_index$industry)))
#                                  row.names(sector_matrix) <- levels(loc_coagglomeration_index$industry)
#                                  colnames(sector_matrix) <- levels(loc_coagglomeration_index$industry)
#                                  
#                                  #Assign my Df
#                                  #We need to calculate the share of total
#                                         #activity represented by each area
#                                  my_df <- loc_coagglomeration_index %>%
#                                       select(ttwa.name,industry,contains(x)) 
#                                  
#                                  names(my_df)[length(names(my_df))] <- "total_act"
#                               
#                                  my_df_wide <- dcast(my_df,ttwa.name+total_act~industry,
#                                                      value.var=x) %>%
#                                       mutate(share_of_total=total_act/sum(total_act,na.rm=T))
# 
#                                  #Now for every combination of sectors, 
#                                    #calculate the index and assign it to the right cell in the matrix
#                                  for (i in colnames(sector_matrix)) {
#                                       for (j in row.names(sector_matrix)) {
#                                                      my_df_subset <-my_df_wide[,c(i,j,
#                                                                              "total_act","share_of_total")] 
#                                                      
#                                                      coi <- sum(
#                                                           (((my_df_subset[i]/sum(my_df_subset[i],na.rm=T)) - 
#                                                                  my_df_subset$share_of_total) *
#                                                                      (
#                                                                           (my_df_subset[j]/
#                                                                                 sum(my_df_subset[j],na.rm=T))- 
#                                                                                my_df_subset$share_of_total)),
#                                                           na.rm=T)/
#                                                                      (1 - sum(my_df_subset$share_of_total^2,na.rm=T))
# 
#                                                      sector_matrix[i,j] <- coi
#                                            }
#                                  }
#                                    return(sector_matrix)
#                                  })
# 
# coi_matrices[1]
# 
# 
# 
# pdf("final-report-figures/heatmaps_coi.pdf",width=8,height=8)
# names <- names(coi_matrices)
# for (i in 1:length(coi_matrices)) {
#      hm <- heatmap(coi_matrices[[i]],main=names[i],
#                    margins=c(20,20))
#      print(hm)
# }
# dev.off()   
     
#Calculate occupational correlation between industries
#This is based on 2014 APS data.
#Get shares of jobs in all industries
jobs_share <- aps_2014 %>%
     select(weight,soc4) %>%
     group_by(soc4) %>% summarise(occ_jobs=sum(weight)) %>%
     mutate(jobs_share_all=occ_jobs/sum(occ_jobs))

# ggplot(data=jobs_share,aes(x=as.factor(soc4),y=jobs_share_all)) +
#      geom_bar(stat="identity")
#jobs_share %>% arrange(desc(jobs_share_all))

excluded_subsectors <- c("Crafts","Museums, galleries and libraries")

#Calculate job shares inside creative industries, and estimate LQs
occ_correlation_tmp1 <- aps_2014 %>% filter(!is.na(sic4_label) &
                                                 !(sic4_label %in% excluded_subsectors)) %>%
     group_by(sic4_label,soc4) %>%
     summarise(jobs_sector=sum(weight,na.rm=T)) %>%
     mutate(jobs_prop=jobs_sector/sum(jobs_sector,na.rm=T)) %>%
     ungroup() %>% left_join(jobs_share[,-2],by="soc4") %>%
     mutate(jobs_lq=jobs_prop/jobs_share_all) %>% droplevels()

#Get Df and correlation matrix
occ_correlation_df <- occ_correlation_tmp1 %>% select(sic4_label,soc4,jobs_lq) %>%
     dcast(soc4~sic4_label,value.var="jobs_lq")

#Remove occupations only employed by one CIs
occ_nas <- apply(occ_correlation_df,1,function(x){
     tots <- sum(is.na(x))
     value <- ifelse(tots>=6,FALSE,TRUE)
})

#Done
occ_correlation_df_clean <- occ_correlation_df[occ_nas,]

#Make missing values = NAs
occ_correlation_df_clean[is.na(occ_correlation_df_clean)] <- 0

#Correlation matrix!
occ_corr_matrix <- cor(log(occ_correlation_df_clean[,-1]+0.0001),method='pearson',
                       use='pairwise.complete.obs')

#pairs(occ_corr_matrix[,-1])
pdf("final-report-figures/occ_heatmap.pdf")
heatmap(occ_corr_matrix,main="occupations",margins=c(10,10))
dev.off()

#Interesting: when we look at occupations, the situation is quite different from
     #co-location. 

#Generate hierarchical clusters with occupational data
pdf("final-report-figures/dendrograms.pdf")
dendronames <- names(loc_correlation_matrices)
for (i in 1:length(loc_correlation_matrices)) {
     d <- dist(loc_correlation_matrices[[i]],method="euclidean")
     fit <- hclust(d,method="ward")
     plot(fit,main=paste("Cluster dendrogram - ",dendronames[i]))
}
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
     business.lq_clusters <- loc_correlation_matrices$business_lq %>% dist(method="euclidean") %>%
          hclust(method="ward") %>% cutree(h=1) %>% Make_keyed_frame()
     
     business.lq_clusters$label <- NA
     
     business.lq_clusters$label[business.lq_clusters$x==1] <- "district_services"
     business.lq_clusters$label[business.lq_clusters$x==2] <- "district_architecture"
     business.lq_clusters$label[business.lq_clusters$x==3] <- "district_content"

#Employment
     emp.lq_clusters <- loc_correlation_matrices$employment_lq %>% dist(method="euclidean") %>%
     hclust(method="ward") %>% cutree(h=1.4) %>% Make_keyed_frame()
     
     emp.lq_clusters$label <- NA
     emp.lq_clusters$label[emp.lq_clusters$x==1] <- "employment_services"
     emp.lq_clusters$label[emp.lq_clusters$x==2] <- "employment_content"
     emp.lq_clusters$label[emp.lq_clusters$x==3] <- "employment_publishing"

#Automate this process
     #Input a variable, return a df with scores by location
          #on that variable (in totals as well as LQs)
 
        
#This requires total levels of employment
     total_activity_ttwa <- ttwa_all_industries %>% 
          rename(ttwa.code=ttwa.2011.code..last.ons.revision.feb.2016.) %>%
          merge(ttwa_names,by.x="ttwa.code",by.y="TTWA11CD",all.x=T) %>%
          filter(year>=2011) %>% rename(ttwa.name=TTWA11NM) %>%
          group_by(ttwa.name) %>%
          summarise(employment_total=mean(total.employment,na.rm=T),
                    business.count_total=mean(total.number.of.enterprises,na.rm=T))


        
                  
#      total_activity_ttwa <- all.cis_ttwa %>% 
#           filter(year!="2007") %>%
#           select(ttwa.name,year,
#                  business.count,business_local.share,
#                  employment,emp_local.share) %>%
#           mutate(business_total = business.count/business_local.share,
#                  employment_total=employment/emp_local.share) %>%
#           group_by(ttwa.name) %>%
#           summarise(employment_total=mean(employment_total,na.rm=T),
#                     business.count_total=mean(business_total,na.rm=T)) %>%
#           ungroup() %>%
#           select(ttwa.name,contains("_total"))     
 

#Generate clustering scores     
     clustering_scores <- lapply(list(business.lq_clusters,
                                      emp.lq_clusters), function(x) {
          #Merge the subsector dataset with the labelled industries                                
          subsector_labelled <- subsectors_ttwa %>%
               filter(year=="2011_14") %>% merge(x,
                                                 by.x="industry",
                                                 by.y="names",
                                                 all.x=T)
          
          my_metric <- ifelse(grepl("district",x$label[1])==TRUE,
                              "business.count","employment")
     
          names(subsector_labelled)[grep(my_metric,names(subsector_labelled))] <-
               "my_metric"
          
          #We need to merge the subsector labelled dataframe with
               #the totals. We select variables of interest based on my_metric
          totals <- total_activity_ttwa %>% select_("ttwa.name",
                                                    paste0(my_metric,"_total"))
          
          names(totals)[grep(my_metric,names(totals))] <- "my_metric_total"
          
          #Merge with subsector data
          subsector_labelled_w_totals <- subsector_labelled %>% 
               merge(totals,by.x="ttwa.name", by.y="ttwa.name")
          
          #Generate outputs
          output <- subsector_labelled_w_totals %>% 
               group_by(ttwa.name,label) %>%
               summarise(metric=sum(my_metric,na.rm=T),
                         metric_total=sum(my_metric_total,na.rm=T))
          
          #We need to split, apply combine to generate LQs
          output_w_lq <- ldply(split(output, output$label), function(x){
               x$metric_lq = (x$metric/sum(x$metric,na.rm=T))/
                    (x$metric_total/sum(x$metric_total,na.rm=T))
               return(x)
          }) 

          return(output_w_lq)
     })
          
#Get top clusters in each of these variables (for sense-checking)
top_clusters <- lapply(clustering_scores,function(x){
     my_df <- x
     top_scores <- lapply(split(x,x$label), function(y) {
          threshold <- quantile(y$metric)[[4]]
          rankings <- y %>% filter(metric>threshold) %>%
               arrange(desc(metric)) %>%
               extract(1:40,"ttwa.name") %>% as.data.frame()
          #names(rankings) <- paste0("ttwa_name_",y)
          return(rankings)
     })
     return(top_scores)
})

#Some checks: correlations between metrics.

combined_df <- do.call(rbind,clustering_scores) %>% dcast(ttwa.name~label,
                                                          value.var="metric_lq")

correlations <- cor(combined_df[,-1],use="pairwise.complete.obs")

WriteChart(heatmap(correlations,margins = c(20,20)),"final-report-figures/")












