#########################################################################
#this script is for cleaning data on invasive species from ***** ##
# it does xx main things:
# 1 - removes irrelevant data
# 2 - reformats data to make it more user-friendly
# 3 - adds columns
# 4 - calculate effect sizes
# 5 - plot forest plot of outcomes


.libPaths("C:/R/Library")
.Library<-("C:/R/Library")

rm(list = ls())


#load packages
library(data.table)
library(ggplot2)
library(stringr)
library(stringi)
library(maps)
library(cowplot)
library(tidyverse)
library(metafor)
library(mice)
library(ggbeeswarm)
library(scales)
library(plyr)
library(tidyverse)
library(cowplot)
library(metafor)
library(data.table)
library(grid)
library(gridExtra)
#1 load dataset
inv_data<-read.csv("data/inv_data.csv")


##############################################################
#2 - reformat data to make it more user-friendly##############
##############################################################

inv_data<-inv_data %>% 
  select(-X)

#make more concise column names
names(inv_data)[c(23:44)]<-
  c("confound","temp_spatial","exp_correl","field_lab","rep_dist",
    "treat_dist","sample_unit_area","plot_homo","unclear_n","ecosystem",
    "inv_time","int_area","int_duration","int_freq","int_time","herbicide_type",
    "herb_rate_foliar","herb_rate_subsurface","introduced_animal","int_animal_density",
    "costs_included","native_non_native")

#shorten citation field [Not useful here but from the original code]
inv_data$study<-gsub("\\[.*?\\]", "", inv_data$citation)


#add columns for each design element
inv_data$ba<-grepl("Before-and-after",inv_data$Design,fixed=TRUE)
inv_data$blocked<-grepl("Blocked",inv_data$Design,fixed=TRUE)
inv_data$controlled<-grepl("Controlled",inv_data$Design,fixed=TRUE)
inv_data$correlative<-grepl("Correlated",inv_data$Design,fixed=TRUE)
inv_data$ma<-grepl("Meta-analysis",inv_data$Design,fixed=TRUE)
inv_data$paired<-grepl("Paired",inv_data$Design,fixed=TRUE)
inv_data$randomised<-grepl("Randomised",inv_data$Design,fixed=TRUE)
inv_data$replicated<-grepl("Replicated",inv_data$Design,fixed=TRUE)

#alter levels for all variables that are strings
#this removes all numbers and white space around the string
cat_vars<-inv_data[,c(23:44)] %>% select_if(is.character) %>% names()

for (i in 1:length(cat_vars)){
  col_vals<-inv_data %>% pull(cat_vars[i])
  new_col_vals<-str_trim(gsub('[0-9]+', '',
                              str_replace_all(col_vals,
                                              "[^[:alnum:]]", " ")))
  inv_data
  col_index<-which(names(inv_data)==cat_vars[i])
  inv_data[,col_index]<-new_col_vals
}

##########################
#alter intervention names#
##########################

#rename interventions so that they are shorter
inv_data$new_int<-mapvalues(inv_data$intervention, from = sort(unique(inv_data$intervention)), 
                            to = c("Biological control","Burning","Burning","Burning","Burning","Covering",
                                   "Cutting and covering","Cutting and flooding","Cutting and replanting",
                                   "Cutting and herbicide","Cutting","Decontamination","Digging and replanting",
                                   "Digging and herbicide","Draining","Excluding grazers","Soil amendment","Flooding",
                                   "Grazing","Grazing","Invasive management","Mowing","Plowing","Habitat restoration",
                                   "Uprooting","Herbicide","Different physical control","Herbicide","Grazing"))



#high-level interventions
inv_data$hli<-mapvalues(inv_data$new_int, from = sort(unique(inv_data$new_int)), 
                        to = c("Biological control","Physical control","Physical control","Physical control",
                               "Physical control","Integrated control","Integrated control","Integrated control",
                               "Decontamination","Physical control","Integrated control","Integrated control",
                               "Habitat management","Habitat management","Habitat management","Habitat management",
                               "Habitat management","Chemical control","Other","Habitat management",
                               "Habitat management","Habitat management","Physical control"))

####################################
#alter outcome names################
####################################
#rename outcomes so that they are shorter

inv_data$new_out<-mapvalues(inv_data$outcome,from=sort(unique(inv_data$outcome)), 
                            to = c("Cation exchange capacity","Electrical conductivity","Fish abundance","Fish size",
                                   "Fungi abundance","Fungi diversity","Invertebrate abundance","Invertebrate diversity",
                                   "Invertebrate abundance","Soil nitrogen","Soil nutrients","Water oxygen","Soil pH",
                                   "SOil phosphorus","Plant biomass","Invasive biomass","Invasive condition","Plant cover",
                                   "Invasive cover","Invasive damage","Plant density","Invasive density","Plant diversity","Plant evenness",
                                   "Plant fecundity","Invasive fecundity","Plant mortality","Invasive mortality","Invasive occurence",
                                   "Plant size","Invasive size","Plant survival","Invasive survival","Soil potassium","Soil bulk density",
                                   "Soil elements","Soil enzymes","Soil enzymes","Soil formation","Soil microbial biomass",
                                   "Soil micronutrients","Soil mineralisation","Soil organic carbon","Soil organic matter",
                                   "Soil organic matter","Soil redox potential","Soil pH","Soil respiration","Soil salinity",
                                   "Soil temperature","Soil texture","Soil water content","Water chemistry","Water quality",
                                   "Water velocity"))


#produce high-level outcomes 
inv_data$hlo<-mapvalues(inv_data$new_out,
                        from=unique(sort(inv_data$new_out)),
                        to=c("Cation exchange capacity","Electrical conductivity","Animal abundance","Animal condition",
                             "Fungi abundance","Fungi diversity","Invasive abundance","Invasive condition",
                             "Invasive abundance","Invasive condition","Invasive abundance","Invasive fecundity","Invasive condition",
                             "Invasive abundance","Invasive condition","Invasive condition","Animal abundance","Animal diversity",
                             "Plant abundance","Plant abundance","Plant abundance","Plant diversity","Plant diversity","Plant fecundity",
                             "Plant condition","Plant condition","Plant condition","Soil bulk density","Soil elements",
                             "Soil enzymes","Soil formation","Soil microbial biomass","Soil micronutrients","Soil mineralisation",
                             "Soil nitrogen","Soil nutrients","Soil organic carbon","Soil organic matter","Soil pH","Soil phosphorus",
                             "Soil potassium","Soil redox","Soil respiration","Soil salinity","Soil temperature","Soil texture",
                             "SOil water content","Water chemistry","Water oxygen","Water quality","Water velocity"))



inv_data$hlo<-ifelse(inv_data$new_out=="invasive biomass"&inv_data$biomass_eq_abundance==FALSE,
                     "invasive condition",inv_data$hlo)
inv_data$hlo<-ifelse(inv_data$new_out=="plant biomass"&inv_data$biomass_eq_abundance==FALSE,
                     "plant condition",inv_data$hlo)


#save data
write.csv(inv_data,"data/cleaned_data.csv")



#load data
inv_data<-read.csv("data/cleaned_data.csv")

########################
#first split - outcomes#
########################
inv_data%>%dplyr::group_by(population)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#only include outcomes relating to invasive species abundance, condition, etc
inv_data$population<-ifelse(inv_data$population=="Pathogens, pests, weeds, and invasive species","Invasive plants",inv_data$population)

inv_data%>%dplyr::group_by(population)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
inv_data%>%dplyr::group_by(hlo)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

#subset into different groups for different populations
#1 - Invasive plants, 2 - native plants, 3. animals and 4 - carbon
inv_sub<-subset(inv_data,population=="Invasive plants")
plant_sub<-subset(inv_data,hlo=="Plant abundance"|hlo=="Plant condition"|hlo=="Plant diversity"|hlo=="Plant fecundity")
animal_sub<-subset(inv_data,hlo=="Animal abundance"|hlo=="Animal condition"|hlo=="Animal diversity")
carbon_sub<-subset(inv_data,hlo=="SOil organic carbon"|hlo=="Soil organic matter"|hlo=="Soil microbial biomass")

#run different meta-analyses for each of these subsets

##[takes too long to run so just load the saved object]
#inv_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
#                  control=list(maxiter=1000),data=inv_sub)
#saveRDS(inv_out_m1,"inv_out_m1.RDS")

inv_out_m1<-readRDS("data/inv_out_m1.RDS")

plant_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                     control=list(maxiter=1000),data=plant_sub)
animal_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                      control=list(maxiter=1000),data=animal_sub)
carbon_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                      control=list(maxiter=1000),data=carbon_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
outcome_results<-data.frame(split=rep("Different outcomes",4),
                            outcome=c("Invasive\nplants","Native\nplants","Native\nanimals","Carbon"),
                            n=c(165,34,14,11),
                            k=c(4298,863,219,90),
                            estimate=c(inv_out_m1$beta,plant_out_m1$beta,animal_out_m1$beta,carbon_out_m1$beta),
                            se=c(inv_out_m1$se,plant_out_m1$se,animal_out_m1$se,carbon_out_m1$se))
#convert effect sizes to percentages
outcome_results$perc<-(exp(outcome_results$estimate)-1)*100
outcome_results$lci<-(exp(outcome_results$estimate-(1.96*outcome_results$se))-1)*100
outcome_results$uci<-(exp(outcome_results$estimate+(1.96*outcome_results$se))-1)*100

########################
#second split - species#
########################
inv_sub$species_split<-as.factor(ifelse(inv_sub$species=="Spartina","Spartina","Other species"))
inv_sub%>%dplyr::group_by(species)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#subset into different groups for different invasive species
#1 - Spartina, 2 - Parrot's feather, 3. Japanese knotweed, and 4 - Giant hogweed
spar_sub<-subset(inv_sub,species=="Spartina")
pf_sub<-subset(inv_sub,species=="Parrot's feather")
jk_sub<-subset(inv_sub,species=="Japanese knotweed")
gh_sub<-subset(inv_sub,species=="Giant hogweed")
#run meta-analyses for each group
#[takes too long - load RDS]
#spar_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
#               control=list(maxiter=1000),data=spar_sub)

spar_m1<-readRDS("data/spar_m1.RDS")
jk_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=jk_sub)
gh_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=gh_sub)
pf_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=pf_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
species_results<-data.frame(split=rep("Different invasives",4),
                            outcome=c("Spartina","Japanese\nknotweed",
                                      "Parrot's\nfeather","Giant\nhogweed"),
                            k=c(2386,484,508,199),
                            n=c(89,30,17,9),
                            estimate=c(spar_m1$beta,jk_m1$beta,gh_m1$beta,pf_m1$beta),
                            se=c(spar_m1$se,jk_m1$se,gh_m1$se,pf_m1$se))
species_results$perc<-(exp(species_results$estimate)-1)*100
species_results$lci<-(exp(species_results$estimate-(1.96*species_results$se))-1)*100
species_results$uci<-(exp(species_results$estimate+(1.96*species_results$se))-1)*100

#############################################
#third split - different broad interventions#
#############################################

spar_sub%>%dplyr::group_by(hli)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)
#subset into different groups for different types of intervention
#1 - Physical control, 2 - habtiat management, 3. Chemical control, and 4 - Biological control
phys_sub<-subset(spar_sub,hli=="Physical control")
hab_sub<-subset(spar_sub,hli=="Habitat management")
chem_sub<-subset(spar_sub,hli=="Chemical control")
biol_sub<-subset(spar_sub,hli=="Biological control")
#run meta-analyses for each group
phys_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=phys_sub)
hab_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=hab_sub)
chem_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=chem_sub)
biol_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=biol_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
int_results<-data.frame(split=rep("Different interventions",4),
                        outcome=c("Physical\ninterventions","Habitat\nmanagement",
                                  "Chemical\ncontrol","Biological\ncontrol"),
                        k=c(1353,578,259,33),
                        n=c(46,30,17,7),
                        estimate=c(phys_m1$beta,hab_m1$beta,chem_m1$beta,biol_m1$beta),
                        se=c(phys_m1$se,hab_m1$se,chem_m1$se,biol_m1$se))
int_results$perc<-(exp(int_results$estimate)-1)*100
int_results$lci<-(exp(int_results$estimate-(1.96*int_results$se))-1)*100
int_results$uci<-(exp(int_results$estimate+(1.96*int_results$se))-1)*100


##############################################
#fourth split - different types of herbicide##
##############################################
chem_sub%>%dplyr::group_by(herbicide_type)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#change name of glyphosate from "Glyphosate   Roundup" to "Glyphosate"
chem_sub$herbicide_type<-ifelse(chem_sub$herbicide_type=="Glyphosate   Roundup","Glyphosate",chem_sub$herbicide_type)
#run meta-analysis for each different type of herbicide and save parameter estimates
herb_results<-NULL
un_herb<-unique(chem_sub$herbicide_type)
for (i in 1:length(un_herb)){
  sub_temp<-subset(chem_sub,herbicide_type==un_herb[i]&!is.na(selected_v)&!is.na(log_response_ratio))
  temp_n<-length(unique(sub_temp$study))
  temp_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                  control=list(maxiter=1000),data=sub_temp)
  temp_res<-data.frame(split=("Different herbicides"),
                       outcome=un_herb[i],
                       estimate=c(temp_m1$beta),
                       se=c(temp_m1$se),
                       k=temp_m1$k,n=temp_n)
  temp_res$perc<-(exp(temp_res$estimate)-1)*100
  temp_res$lci<-(exp(temp_res$estimate-(1.96*temp_res$se))-1)*100
  temp_res$uci<-(exp(temp_res$estimate+(1.96*temp_res$se))-1)*100
  herb_results<-rbind(herb_results,temp_res)
}

herb_results$outcome<-c("Glyphosate","Vinegar","Monodosium\nmethanearsonate",
                        "Dalapon","Atrazine","Imazapyr","Fluazifop\np butyl",
                        "Haloxyfop r\nmethyl","Cyhalofop\nbutyl","Haloxyfop p\nmethyl",
                        "Imazameth")

######################################
#figure###############################
######################################
#combine results for different meta-analyses into one dataframe
combined_results<-rbind(outcome_results,species_results,int_results,herb_results)
#plot results

combined_results$outcome
comb_plot<-combined_results%>%group_by(split)%>%slice_max(k,n=4)%>%
  mutate(outcome=as.factor(outcome),split=as.factor(split))%>%
  mutate(outcome=fct_relevel(outcome,(c("Invasive\nplants","Native\nplants","Native\nanimals","Carbon",
                                        "Spartina","Japanese\nknotweed","Parrot's\nfeather","Giant\nhogweed",
                                        "Physical\ninterventions","Chemical\ncontrol",
                                        "Biological\ncontrol","Habitat\nmanagement"))),
         split=fct_relevel(split,c("Different outcomes","Different invasives",
                                   "Different herbicides","Different interventions")))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=k,size=k))+
  geom_point()+geom_errorbar(size=0.5)+geom_point(shape=1,colour="black",stroke=1)+
  theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  scale_y_discrete(limits=rev,position = "right")+
  facet_wrap(~split,scales = "free",ncol=2)+
  xlab("Percentage change in outcome")+
  #scale_x_continuous(breaks = c(-75,-50,-25,0,25))+
  scale_size_continuous(range=c(1, 4), breaks=c(5,15,78,496,4298),limits=c(1,5000),trans="sqrt")+
  scale_colour_continuous(low="grey90",high="blue3",breaks=c(5,15,78,496,4298),limits=c(1,5000),trans="sqrt")+
  guides(color= guide_legend(title="no. of comparisons",title.position="top",title.hjust = 0.5), 
         size=guide_legend(title="no. of comparisons",title.position="top",title.hjust = 0.5))+
  theme(legend.position = "bottom")+
  theme(axis.line=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

comb_plot

