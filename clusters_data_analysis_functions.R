#Cluster data analysis functions
#Get_Range_Matrix function
#Takes 4 values for its extremes (a vector with 4 elements) 
#And the dimensions (number cell/columns)
#and interpolates between them.
Get_Range_Matrix <- function(coords,dims) {
     mat <- matrix(NA,dims,dims)
     mat[dims,] <- seq(coords[2],coords[4],length.out = dims)
     mat[1,] <- seq(coords[1],coords[3],length.out = dims)
     mat[,1] <- seq(coords[1],coords[2],length.out = dims)
     mat[,dims] <- seq(coords[3],coords[4],length.out = dims)
     
     for (i in 2:(ncol(mat)-1)) {
          mat[i,] <- seq(mat[i,1],mat[i,dims],length.out=dims)
     }
     #mat <- mat/255
     return(mat/255)
}

#Get_RGB_Values function
#Takes a list with 3 elements with RGB scores,
#and dims (number of rows in the square matrix)
#and returns a matrix with RGBs in each cell.
Get_RGB_values <- function(colours,dims) {
     d <- dims
     #Extract out_scores
     r_scores <- sapply(colours,function(x){return(x[[1]])})
     g_scores <- sapply(colours,function(x){return(x[[2]])})
     b_scores <- sapply(colours,function(x){return(x[[3]])})
     
     my_mats <- lapply(list(r_scores,g_scores,b_scores), 
                       Get_Range_Matrix,dims=d)
     names(my_mats) <- c("red","green","blue")
     
     #Put them in Out_mat
     out_mat <- matrix(NA,dims,dims)
     for (i in 1:dims) {
          for (j in 1:dims) {
               value <- c(my_mats$red[i,j],my_mats$green[i,j],
                          my_mats$blue[i,j])
               out_mat[i,j] <- rgb(value[1],value[2],value[3])
          }
     }
     return(out_mat)
}

#Get_Sector_Mapping_Df returns a df with the
#right data for mapping. It takes
#the number of quantiles we want to consider.
#Function from hell
Get_Sector_Mapping_Df <- function(qs,
                                  cols=colours,
                                  lowest_alpha=0.4) {
     #Get colour matrix
     palette_2_vars <- Get_RGB_values(cols,dims=qs)
     
     #Get Alpha matrix
     alpha_values <- Get_Alphas(lowest=lowest_alpha,
                                dims=qs)
     
     #Get DF
     output_df <- ldply(
          split(ttwa_ci_data_mapping,
                ttwa_ci_data_mapping$industry.short),
          function(x){
               #First extract the colours
               x_wide <- x %>%
                    select(ttwa.name,industry.short,metric,value,
                           variable,
                           var_type) %>%
                    droplevels() %>%
                    dcast(ttwa.name+industry.short~metric+variable+var_type)
               
               #Outputs df where we add outputs for plotting
               x_outs <- x_wide %>% select(ttwa.name,industry.short)
               
               #Extract quartiles
               x_outs[,c("bc_q","emp_q")] <- 
                    lapply(x_wide[,c("business_lq_second.period_lq",
                                     "emp_lq_second.period_lq")],
                           function(x){
                                q <- cut(x,
                                         breaks=quantile(x,
                                                         probs=seq(0,1,length.out = qs),
                                                         na.rm=T),
                                         labels=FALSE)
                                q[is.na(q)] <- min(q)
                                return(q)
                           })
               
               #Extract colours
               x_outs[,"colour_sp"] <- 
                    apply(x_outs[,c("bc_q","emp_q")],1,
                          function(x){
                               rind <- as.numeric(x[[1]])
                               cind <- as.numeric(x[[2]])
                               return(palette_2_vars[rind,cind])
                          })
               
               #Alphas
               x_outs[,c("bc_t_q","emp_t_q")] <- 
                    lapply(x_wide[,c("business.count_second.period_total",
                                     "employment_second.period_total")],
                           function(x){
                                q <- cut(x,
                                         breaks=quantile(x,
                                                         probs=seq(0,1,length.out = qs),
                                                         na.rm=T),
                                         labels=FALSE)
                                q[is.na(q)] <- min(q)
                                return(q)
                           })
               
               x_outs[,c("alpha_sp")] <- 
                    apply(x_outs[,c("bc_t_q","emp_t_q")],1,
                          function(x){
                               if (is.na(x[[1]])==TRUE |
                                   is.na(x[[2]])==TRUE) {
                                    rind=1
                                    cind=1
                               }
                               else {
                                    rind <- as.numeric(x[[1]])
                                    cind <- as.numeric(x[[2]])
                               }
                               
                               return(alpha_values[rind,cind])
                          })
               
               #Extract quartiles for change
               x_outs[,c("bc_q_c","emp_q_c")] <- 
                    lapply(x_wide[,c("business_lq_change_lq",
                                     "emp_lq_change_lq")],
                           function(x){
                                q <- cut(x,
                                         breaks=quantile(x,
                                                         probs=seq(0,1,length.out = qs),
                                                         na.rm=T),
                                         labels=FALSE)
                                q[is.na(q)] <- min(q)
                                return(q)
                           })
               
               #Extract colours (change)
               x_outs[,"colour_ch"] <- 
                    apply(x_outs[,c("bc_q_c","emp_q_c")],1,
                          function(x){
                               rind <- as.numeric(x[[1]])
                               cind <- as.numeric(x[[2]])
                               return(palette_2_vars[rind,cind])
                          })
               
               
               #Extract alphas (change)
               x_outs[,c("bc_t_q_ch","emp_t_q_ch")] <- 
                    lapply(x_wide[,c("business.count_first.period_total",
                                     "employment_first.period_total")],
                           function(x){
                                q <- cut(x,
                                         breaks=quantile(x,
                                                         probs=seq(0,1,length.out = qs),
                                                         na.rm=T),
                                         labels=FALSE)
                                q[is.na(q)] <- min(q)
                                return(q)
                           })
               
               x_outs[,c("alpha_ch")] <- 
                    apply(x_outs[,c("bc_t_q_ch","emp_t_q_ch")],1,
                          function(x){
                               if (is.na(x[[1]])==TRUE |
                                   is.na(x[[2]])==TRUE) {
                                    rind=1
                                    cind=1
                               }
                               else {
                                    rind <- as.numeric(x[[1]])
                                    cind <- as.numeric(x[[2]])
                               }
                               
                               return(alpha_values[rind,cind])
                          })
               return(x_outs)
          },.id=NULL)
     
     #Get labels
     top_areas <- lapply(split(ttwa_ci_data_mapping,
                               ttwa_ci_data_mapping$industry.short),
                         function(x){
                              #Get wide df for sorting
                              x_wide <- x %>%
                                   select(ttwa.name,industry.short,metric,value,
                                          variable,
                                          var_type) %>%
                                   droplevels() %>%
                                   dcast(ttwa.name+industry.short~metric+variable+var_type)
                              
                              #Extract top activity areas
                              
                              top_areas <- lapply(
                                   as.list(c(list(c("business_lq_second.period_lq",
                                                    "business.count_second.period_total")),
                                             list(c("emp_lq_second.period_lq",
                                                    "employment_second.period_total")),
                                             list(c("business_lq_change_lq",
                                                    "business.count_first.period_total")),
                                             list(c("emp_lq_change_lq",
                                                    "employment_first.period_total")))),
                                   function(x){
                                        size_control <- x[[2]]
                                        ranker <- x[[1]]
                                        
                                        big <- x_wide[,size_control] >=
                                             quantile(x_wide[,size_control],
                                                      na.rm=T)[[4]]
                                        
                                        labels_df <- x_wide[big,]
                                        labels_df_orf <- labels_df[order(labels_df[,ranker],
                                                                         decreasing=T),]
                                        
                                        labels <- labels_df_orf[1:5,"ttwa.name"]
                                        return(labels)
                                   })
                              names(top_areas) <- c("bc_sp","emp_sp","bc_c","emp_c")
                              return(top_areas)
                         })
     
     unique_top_areas <- lapply(top_areas,
                                function(x){
                                     sp <- unique(c(as.character(x$bc_sp),
                                                    as.character(x$emp_sp)))
                                     ch <- unique(c(as.character(x$bc_c),
                                                    as.character(x$emp_c)))
                                     out <- list("sp"=sp,
                                                 "ch"=ch)
                                })
     
     return(list(output_df,unique_top_areas,palette_2_vars))
}

#Make_Sector_Maps function
#Takes an industry and returns a map, just like that.
Make_Sector_Maps <- function(industry){
     mapping_df <- plotting_objects[[1]] %>% 
          filter(industry.short==industry) %>%
          select(ttwa.name,colour_sp,alpha_sp,colour_ch,alpha_ch)
     
     mapping_df_list <- lapply(c("colour","alpha"),
                               function(x){
                                    map_df <- mapping_df %>% select(ttwa.name,
                                                                    contains(x)) %>%
                                         melt(id.vars="ttwa.name") %>%
                                         mutate(variable=ifelse(grepl("ch",variable)==TRUE,
                                                                "change","second_period"))
                                    names(map_df)[grep("value",names(map_df))] <- x
                                    return(map_df)
                               })
     
     #Merge outputs
     mapping_df_2 <- mapping_df_list[[1]] %>%
          merge(mapping_df_list[[2]],by=c("ttwa.name","variable"))
     
     #Reorder levels
     mapping_df_2$variable <- factor(mapping_df_2$variable,
                                     levels=c("second_period","change"))
     levels(mapping_df_2$variable) <- c("Current concentration",
                                        "Change between 2007 and 2014")
     
     #Merge with the Shapefile df
     mapping_polys <- merge(ttwa_shape,mapping_df_2,
                            by="ttwa.name") %>%
          arrange(desc(order)) %>% tbl_df() %>%
          rename(col=colour,
                 my_alpha=alpha)
     
     #Get the labels.
     labels_list <- plotting_objects[[2]][[industry]]
     
     labels_df <- ldply(labels_list,
                        function(x){
                             return(data.frame(ttwa.name=x))
                        },.id="variable")
     
     #Merge them with the centroids
     cen_df <- merge(uk_cen,labels_df,by="ttwa.name")
     
     cen_df$variable <- factor(cen_df$variable,
                               levels=c("sp","ch"))
     levels(cen_df$variable) <- c("Current concentration",
                                  "Change between 2007 and 2014")
     
     map_plot <-ggplot(data=mapping_polys,
                       aes(x=long,y=lat,fill=col,group=group,
                           alpha=my_alpha))+
          geom_polygon(colour="azure4",size=0.001)+
          geom_label_repel(data=cen_df,aes(x=long,y=lat,group=NULL,
                                           alpha=NULL,fill=NULL,
                                           label=ttwa.name),
                           size=1.5,colour="black",
                           label.padding=unit(0.15,'lines'))+
          scale_fill_identity()+
          scale_alpha_continuous(guide="none")+
          facet_grid(.~variable)+
          labs(title=paste(
               "Concentration and change in",industry," in the period 2007-2014"))+
          #facet_grid(.~measure)+
          map_theme
     
     return(map_plot)
}




