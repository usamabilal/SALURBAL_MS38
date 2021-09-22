rm(list=ls())
library(pals)
library(ggforce)
library(RColorBrewer)
library(readxl)
library(vroom)
library(concaveman)
library(tidyverse)
library(sf)
library(broom)
library(grid)
library(gridExtra)
library(ggrepel)
load("analytic files/ms38data.rdata")
source("MS38_HELPER_FUNCTIONS.R")
# Table 1: overall, and five large groupings, unadjusted, adjusted, US, LAC, BR, MX
# get all-cause mortality by summing over all categories
acm<-mortality_both_collapse %>% 
  group_by(us, iso2, city) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_both_collapse) %>% 
  left_join(popage_collapse)
summary(acm);head(acm);nrow(acm)
# get results for first row
outcome_list_category5<-c("All-Cause Mortality", "CMNN", "Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")
model_list<-c("unadj", "adj", "US", "LAC", "BR", "MX", "restLAC")
row1<-map(model_list, function(model){
  if(model=="unadj"){
    m<-lm(formula=log(deaths)~log(pop), data=acm)
  } else if (model=="adj"){
    m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=acm)
  } else {
    if (model=="LAC"){
      m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=acm %>% filter(us==0))
    } else if (model=="restLAC"){
      m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=acm %>% filter(us==0, !iso2%in%c("MX", "BR")))
    } else {
      m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=acm %>% filter(iso2==model))
    }
  }
  m
})
large_groups<-mortality_both_collapse %>% 
  left_join(cause_names) %>% 
  group_by(us, iso2, city, category5) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_both_collapse) %>% 
  left_join(popage_collapse)
rows25<-large_groups %>% 
  group_by(category5) %>% 
  group_map(~{
    map(model_list, function(model){
      #print(.y$category5);print(model)
      #.x<-large_groups %>% filter(category5=="Homicides");model<-"unadj"
      # TEMP
      .x<-.x %>% filter(deaths>0)
      if(model=="unadj"){
        m<-lm(formula=log(deaths)~log(pop), data=.x)
      } else if (model=="adj"){
        m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x)
      } else {
        if (model=="LAC"){
          m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x %>% filter(us==0))
        } else if (model=="restLAC"){
          m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x %>% filter(us==0, !iso2%in%c("MX", "BR")))
        } else {
          m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x %>% filter(iso2==model))
        }
      }
      list(m, outcome=.y$category5)
    })
  })

table1_row1<-map_dfr(row1, function(model){
  #model<-row1[[1]]
  coefs<-tidy(model) %>% 
    filter(term=="log(pop)") %>% 
    mutate(coef=paste0(new_format(estimate, digits=2), " (",
                       new_format(estimate-1.96*std.error, digits=2), ";",
                       new_format(estimate+1.96*std.error, digits=2), ")"),
           r2=(glance(model) %>% pull(r.squared)),
           r2=paste0(new_format(r2*100, digits=1), "%")) %>% 
    select(coef, r2)
}) %>% 
  mutate(model=model_list,
         outcome=outcome_list_category5[[1]])
table1_row25<-map_dfr(rows25, function(temp){
  #temp<-rows25[[1]]
  map_dfr(temp, function(model){
    #model<-temp[[1]]
    outcome<-model$outcome
    model<-model[[1]]
    coefs<-tidy(model) %>% 
      filter(term=="log(pop)") %>% 
      mutate(coef=paste0(new_format(estimate, digits=2), " (",
                         new_format(estimate-1.96*std.error, digits=2), ";",
                         new_format(estimate+1.96*std.error, digits=2), ")"),
             r2=(glance(model) %>% pull(r.squared)),
             r2=paste0(new_format(r2*100, digits=1), "%")) %>% 
      select(coef, r2) %>% mutate(outcome=outcome)
  }) %>% 
    mutate(model=model_list)
})
outcome_list<-unique(table1_row25$outcome)
table1<-bind_rows(table1_row1, table1_row25) %>% 
  mutate(outcome=factor(outcome,
                        levels=outcome_list_category5),
         model=factor(model, levels=model_list)) %>% 
  arrange(outcome, model) %>% 
  gather(type, value, -model, -outcome) %>% 
  filter(type=="coef") %>% 
  spread(model, value) %>% select(-type)
table1
write_csv(table1, path ="Results/table1.csv")
table1_r2<-bind_rows(table1_row1, table1_row25) %>% 
  mutate(outcome=factor(outcome,
                        levels=outcome_list_category5),
         model=factor(model, levels=model_list)) %>% 
  arrange(outcome, model) %>% 
  gather(type, value, -model, -outcome) %>% 
  filter(type=="r2") %>% 
  spread(model, value) %>% select(-type)
table1_r2
write_csv(table1_r2, path ="Results/Supplementary_Table_S2.csv")
  
# - Figure 1: overall scaling and five large groupings for US and LAC
# unadjusted
all<-bind_rows(acm %>% mutate(category5="All-Cause Mortality"),
  large_groups) %>% 
  mutate(category5=factor(category5, outcome_list_category5)) %>% 
  mutate(us=factor(us, levels=c(1, 0), labels=c("US", "Latin America")))
info<-all %>% 
  group_by(category5, us) %>% 
  group_modify(~{
    .x<-.x %>% filter(deaths>0)
    # unadjusted
    model<-lm(log(deaths)~log(pop), data=.x)
    # age and country adjusted
    if (.y$us==0){
      model2<-lm(log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x)
    } else {
      model2<-lm(log(deaths)~log(pop)+prop15+prop40+prop65, data=.x)
    }
    intercept<-tidy(model) %>% filter(term=="(Intercept)") %>% 
      pull(estimate)
    slope<-tidy(model) %>% filter(term=="log(pop)") %>% 
      select(estimate, std.error) %>% 
      mutate(coef=paste0(new_format(estimate, digits=2),
                         " (", new_format(estimate-1.96*std.error, digits=2), ";",
                         new_format(estimate+1.96*std.error, digits=2), ")")) %>% 
      select(estimate, coef)
    r2<-glance(model) %>% pull(r.squared)
    r2<-paste0(new_format(r2*100, digits=1), "%")
    data.frame(intercept=intercept, slope=slope$estimate,coef=slope$coef,r2=r2)
  }) %>% 
  mutate(intercept100k=exp(intercept+log(100000)*slope),
         interceptmax=exp(intercept+log(max(all$pop))*1))
colors <- c("Reference Line" = "red", "Linear Fit" = "blue")
ggplot(all, aes(x=pop, y=deaths))+
  geom_point(fill="gray", pch=21, color="black")+
  geom_segment(data=info, aes(y=intercept100k, yend=interceptmax,
                              color="Reference Line",
                              x=10^5, xend=max(all$pop)), lty=2, size=1.1)+
  stat_smooth(data=all, method="lm", se=F, aes(color="Linear Fit"))+ 
  geom_text(data=info, aes(x=10^5, y=3*10^-1, label=coef), color="blue",
            hjust=0, size=6)+
  scale_color_manual(values=colors, name="")+
  scale_y_log10(breaks=10^(0:6),
                labels=c("1", "10", "100", "1K", "10K", "100K", "1M"),
                limits=c(10^-1, 10^6))+
  scale_x_log10(breaks=10^c(3:7),
                labels=c("1K", "10K", "100K", "1M", "10M"))+
  annotation_logticks()+
  guides(color = guide_legend(override.aes = list(alpha=1, size=2)),
         linetype = F)+
  labs(x="Population size",
       y="Deaths")+
  facet_grid(us~category5)+#, labeller = labeller(category3=labels, us=labels_us))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background=element_blank(),
        legend.position = "bottom")
ggsave("results/Figure1.pdf", width=20, height=10)

# adjusted
all_adjusted<-all %>% 
  group_by(category5) %>% 
  group_modify(~{
    #.x<-all %>% filter(category5=="All-Cause Mortality")
    .x<-.x %>% filter(deaths>0)
    model_y<-lm(log(deaths)~iso2+prop15+prop40+prop65, data=.x)
    model_x<-lm(log(pop)~iso2+prop15+prop40+prop65, data=.x)
    .x$res_y<-residuals(model_y)
    .x$res_x<-residuals(model_x)
    .x
  })
info_adjusted<-all_adjusted %>% 
  group_by(category5, us) %>% 
  group_modify(~{
    #.x<-all_adjusted %>% filter(category5=="All-Cause Mortality", us=="US")
    model<-lm(res_y~res_x, data=.x)
    intercept<-tidy(model) %>% filter(term=="(Intercept)") %>% 
      pull(estimate)
    slope<-tidy(model) %>% filter(term=="res_x") %>% 
      select(estimate, std.error) %>% 
      mutate(coef=paste0(new_format(estimate, digits=2),
                         " (", new_format(estimate-1.96*std.error, digits=2), ";",
                         new_format(estimate+1.96*std.error, digits=2), ")")) %>% 
      select(estimate, coef)
    data.frame(intercept=intercept, slope=slope$estimate,coef=slope$coef)
  }) %>% 
  mutate(intercept100k=(intercept+min(all_adjusted$res_x)*slope),
         interceptmax=(intercept+max(all_adjusted$res_x)*1))
colors <- c("Reference Line" = "red", "Linear Fit" = "blue")
ggplot(all_adjusted, aes(x=res_x, y=res_y))+
  geom_point(fill="gray", pch=21, color="black")+
  geom_segment(data=info_adjusted,aes(y=intercept100k, yend=interceptmax,
                              color="Reference Line",
                              x=min(all_adjusted$res_x), xend=max(all_adjusted$res_x)), 
               lty=2, size=1.1)+
  stat_smooth(data=all_adjusted, method="lm", se=F, aes(color="Linear Fit"))+ 
  geom_text(data=info_adjusted, aes(x=0, y=-2.5, label=coef),color="blue",
            hjust=0, size=6)+
  scale_color_manual(values=colors, name="")+
  guides(color = guide_legend(override.aes = list(alpha=1, size=2)),
         linetype = F)+
  labs(x="Population size (Normalized)",
       y="Deaths (Normalized)")+
  facet_grid(us~category5)+#, labeller = labeller(category3=labels, us=labels_us))+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background=element_blank(),
        legend.position = "bottom")
ggsave("results/Supplementary_Figure_S1.pdf", width=20, height=10)

# table 2: count of causes by pattern in US, LAC, BR, and MX
small_groups<-mortality_both_collapse %>% 
  left_join(cause_names) %>% 
  group_by(us, iso2, city, category4, name) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_both_collapse) %>% 
  left_join(popage_collapse)
small_groups_models<-small_groups %>% 
  group_by(category4, name) %>% 
  group_map(~{
    #.x<-small_groups %>% filter(name=="Breast cancer")
    .x<-.x %>% filter(deaths>0)
    m_us<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x %>% filter(us==1))
    m_lac<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x %>% filter(us==0))
    m_br<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x %>% filter(iso2=="BR"))
    m_mx<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x %>% filter(iso2=="MX"))
    m_lacnobrmx<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x %>% filter(us==0, !iso2%in%c("BR", "MX")))
    m_us_unadj<-lm(formula=log(deaths)~log(pop), data=.x %>% filter(us==1))
    m_lac_unadj<-lm(formula=log(deaths)~log(pop), data=.x %>% filter(us==0))
    m_mx_unadj<-lm(formula=log(deaths)~log(pop), data=.x %>% filter(iso2=="MX"))
    m_br_unadj<-lm(formula=log(deaths)~log(pop), data=.x %>% filter(iso2=="BR"))
    m_lacnobrmx_unadj<-lm(formula=log(deaths)~log(pop), data=.x %>% filter(us==0, !iso2%in%c("BR", "MX")))
    m_unadj<-lm(formula=log(deaths)~log(pop), data=.x)
    m_adj<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65, data=.x)
    list(m_us=m_us, m_lac=m_lac, m_br=m_br, m_mx=m_mx,m_lacnobrmx=m_lacnobrmx, 
         m_lac_unadj=m_lac_unadj, m_us_unadj=m_us_unadj, m_lacnobrmx_unadj=m_lacnobrmx_unadj,
         m_mx_unadj=m_mx_unadj, m_br_unadj=m_br_unadj,
         m_unadj=m_unadj,m_adj=m_adj,
         category=.y$category4, name=.y$name)
  }) 

small_groups_results<-small_groups_models%>% 
  map_dfr(function(models){
    model_list<-names(models)
    model_list<-model_list[grepl("m_", model_list)]
    map_dfr(model_list, function(model){
      index<-which(names(models)%in%model)
      tidy(models[[index]]) %>% filter(term=="log(pop)") %>% mutate(area=model) %>% 
        mutate(intercept=tidy(models[[index]]) %>% filter(term=="(Intercept)") %>% pull(estimate),
               sd=sd(models[[index]]$model$`log(deaths)`),
               lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               category4=models$category,
               name=models$name) %>% 
        select(area, estimate, lci, uci, category4, name, intercept, sd)
    })
  }) %>% 
  mutate(pattern=case_when(
    uci<1 ~ "sublinear",
    lci>1 ~ "superlinear",
    T ~ "linear"
  ),
  #area=factor(area, levels=c("US", "LAC", "BR", "MX", "LAC_no_BRMX")),
  area=sub("m_", "", area),
  pattern=factor(pattern, levels=c("sublinear", "linear", "superlinear"))) 
  

small_groups_r2<-small_groups_models%>% 
  map_dfr(function(models){
    bind_rows(glance(models$m_us) %>% select(r.squared) %>% mutate(area="US"),
              glance(models$m_lac) %>% select(r.squared) %>% mutate(area="LAC"),
              glance(models$m_lacnobrmx) %>% select(r.squared) %>% mutate(area="LAC_no_BRMX"),
              glance(models$m_br) %>% select(r.squared) %>% mutate(area="BR"),
              glance(models$m_mx) %>% select(r.squared) %>% mutate(area="MX")) %>% 
      mutate(category4=models$category,
             name=models$name,
             r2=paste0(new_format(r.squared*100, digits=1), "%"),
             area=factor(area, levels=c("US", "LAC", "BR", "MX", "LAC_no_BRMX"))) %>% 
        select(area,category4, name, r2)
  })

# table 2
table2<-small_groups_results %>% 
  filter(area%in%c("us", "lac", "br", "mx", "lacnobrmx")) %>% 
  mutate(area=factor(area, levels=c("us", "lac", "br", "mx", "lacnobrmx"))) %>% 
  group_by(category4, area, pattern) %>% 
  summarise(n=n()) %>% 
  spread(area, n) %>% 
  mutate_all(~replace_na(., 0))
table2
write_csv(table2, path ="Results/Supplementary_Table_S5.csv")

#"table" figure

table1 %>% 
  gather(area, est, -outcome) %>% 
  mutate(estimate=substr(est, 1, 4) %>% as.numeric, 
         lci=substr(est, 7,10) %>% as.numeric,
         uci=substr(est, 12,15) %>% as.numeric,
         area=factor(area, levels=c("unadj", "adj", "US", "LAC", "BR", "MX", "restLAC"),
                     labels=c("Unadjusted", "Adjusted",
                              "US", "LA", "BR", "MX", "LA (no BR/MX)"))) %>% 
  arrange(area, (estimate)) %>% 
  group_by(area) %>% 
  mutate(order=row_number(),
         category4=ifelse(outcome%in%c("Homicides","Suicides", "Non-violent injuries"),
                          "Injuries", as.character(outcome)),
         category4=factor(category4,
                          levels=c("All-Cause Mortality", "CMNN", "Cancer", "NCDs", "Injuries")),
         pattern=case_when(
           uci<1 ~ "sublinear",
           lci>1 ~ "superlinear",
           T ~ "linear"),
         outcome2=paste0(outcome, "\n", est)) %>% 
  ggplot(aes(x=area, y=order))+
  geom_tile(aes(fill=category4, alpha=pattern, linetype=pattern), 
            width=.85, color="black", size=1) +
  geom_text(aes(label=outcome2), color="black", size=6)+
  scale_alpha_manual(values=c(0.5, 1, 1)) +
  scale_linetype_manual(values=c(0, 2, 1))+
  #scale_fill_brewer(type="qual", palette=2) +
  scale_fill_manual(values=c("gray50", brewer.pal(4, "Dark2")))+
  #guides(alpha=F, linetype=guide_legend(override.aes = list(fill=NA)))+
  guides(alpha=F, linetype=F, fill=guide_legend(override.aes=list(color=NA)))+
  labs(x="", fill="",
       y="Causes of Death Sorted in Descending Order\n(higher=more superlinear, lower=more sublinear)")+
  theme_void() +
  theme(axis.text.x=element_text(color="black", size=20, face="bold"),
        axis.title.x=element_text(color="black", face="bold", size=20),
        axis.title.y=element_text(color="black", face="bold", size=20, angle=90),
        #axis.text.y=element_blank(),
        plot.title =element_text(color="black", face="bold",size=20),
        strip.text =element_text(color="black", face="bold",size=20),
        legend.text =element_text(color="black", face="bold",size=20),
        strip.background=element_blank(),
        legend.position = "bottom")
ggsave("results/Figure2.pdf", width=20, height=7.5)


small_groups_results %>% 
  ungroup() %>% 
  arrange(area, (estimate)) %>% 
  group_by(area) %>% 
  mutate(order=row_number()) %>% 
  filter(area%in%c("us", "lac", "br", "mx", "lacnobrmx")) %>% 
  mutate(area=factor(area, levels=c("us", "lac", "br", "mx", "lacnobrmx"),
                     labels=c("US", "LA", "BR", "MX", "LA (-BR/MX)")),
         name=sub("and other endocrine disorders", "", name),
         est=paste0(new_format(estimate, digits=2), " (",
                     new_format(lci, digits=2), ";",
                     new_format(uci, digits=2), ")"),
         name2=paste0(name, "\n", est)) %>% 
  ggplot(aes(x=area, y=order)) +
  geom_tile(aes(fill=category4, alpha=pattern, linetype=pattern), 
            width=.85, color="black", size=1) +
  geom_text(aes(label=name2), color="black", lineheight=0.7)+
  scale_y_continuous(expand=expansion(mult=c(0, 0.05)))+
  scale_alpha_manual(values=c(1, 0.5, 1)) +
  scale_linetype_manual(values=c(2, 0, 1))+
  scale_fill_brewer(type="qual", palette=2) +
  guides(alpha=F, linetype=F, fill=guide_legend(override.aes=list(color=NA)))+
  labs(x="", fill="",
       y="Causes of Death Sorted in Descending Order\n(higher=more superlinear, lower=more sublinear)")+
  theme_void() +
  theme(axis.text.x=element_text(color="black", size=14, face="bold"),
        axis.title.x=element_text(color="black", face="bold", size=16),
        axis.title.y=element_text(color="black", face="bold", size=14, angle=90),
        #axis.text.y=element_blank(),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background=element_blank(),
        legend.position = "bottom")
ggsave("results/Figure3.pdf", width=15, height=15)
  

# table for appendix
table2_appendix<-small_groups_results %>% 
      mutate(coef=paste0(new_format(estimate, digits=2), " (",
                         new_format(lci, digits=2), ";",
                         new_format(uci, digits=2), ")"),
             name=factor(name, levels=unique(cause_names$name))) %>% 
  select(category4, name, area, coef) %>% 
  spread(area, coef) %>% 
  select(category4, name, us, lac, br, mx, lacnobrmx)
write_csv(table2_appendix, path ="Results/Supplementary_Table_S3.csv")
table2_r2_appendix<-small_groups_r2 %>% 
  mutate(name=factor(name, levels=unique(cause_names$name))) %>% 
  spread(area, r2)
write_csv(table2_r2_appendix, path ="Results/Supplementary_Table_S4.csv")


small_groups_results_f2<-small_groups_results %>% filter(area%in%c("us", "lac")) %>% 
  select(area, name, category4, pattern, estimate)
small_groups_results_f2<-
  full_join(small_groups_results_f2 %>% select(category4, area, name, estimate) %>% spread(area, estimate),
          small_groups_results_f2 %>% select(area, name, pattern) %>% spread(area, pattern) %>% 
    rename(patternUS=us, patternLAC=lac) %>% 
  select(name, patternUS, patternLAC)) %>% 
  mutate(dif=patternUS!=patternLAC,
         verydif=dif==T & patternUS!="linear" & patternLAC !="linear",
         type=case_when(
           patternUS=="superlinear" & patternLAC=="superlinear" ~ "Super US / Super LAC",
           patternUS=="sublinear" & patternLAC=="sublinear" ~ "Sub US / Sub LAC",
           patternUS=="superlinear" & patternLAC=="sublinear" ~ "Super US / Sub LAC",
           patternUS=="sublinear" & patternLAC=="superlinear" ~ "Sub US / Super LAC",
           patternUS=="superlinear" & patternLAC=="linear" ~ "Super US / Linear LAC",
           patternUS=="linear" & patternLAC=="superlinear" ~ "Linear US / Super LAC",
           patternUS=="sublinear" & patternLAC=="linear" ~ "Sub US / Linear LAC",
           patternUS=="linear" & patternLAC=="sublinear" ~ "Linear US / Sub LAC",
           patternUS=="linear" & patternLAC=="linear" ~ "Linear US / Linear LAC",
         )) 
small_groups_results_brmx<-small_groups_results %>% filter(area%in%c("br", "mx")) %>% 
  select(area, name, category4, pattern, estimate)
small_groups_results_brmx<-
  full_join(small_groups_results_brmx %>% select(category4, area, name, estimate) %>% spread(area, estimate),
            small_groups_results_brmx %>% select(area, name, pattern) %>% spread(area, pattern) %>% 
              rename(patternMX=mx, patternBR=br) %>% 
              select(name, patternMX, patternBR)) 
# bivariat3 palette
cols<-brewer.divdiv(n=9)
# change middle color for a darker gray
cols[5]<-"gray50"


# colors of this palette start at bottom left, go towards right, then second row, etc.
# so order should be  subsub Lsub supersub; 
#                     subL linearlinear superL;
#                     subsuper Lsuper supersuper
small_groups_results_f2<-small_groups_results_f2 %>% 
  mutate(type=factor(type,
                     levels=c(
                              "Sub US / Sub LAC",
                              "Linear US / Sub LAC",
                              "Super US / Sub LAC",
                              "Sub US / Linear LAC",
                              "Linear US / Linear LAC",
                              "Super US / Linear LAC",
                              "Sub US / Super LAC",
                              "Linear US / Super LAC",
                              "Super US / Super LAC"
                              )))
table(small_groups_results_f2$type, useNA = "always")

# hulls: https://luisdva.github.io/rstats/Grouping-points/ 

# cancer annotations
small_groups_results_f2 %>% filter(category4=="Cancer") %>% arrange(type) %>% print(n=50)
cancer_anot<-small_groups_results_f2 %>% 
  filter(grepl("phagus|Stomach|Kidney|Leukaemia|Breast|Liver", name)) %>% 
  mutate(x=case_when(grepl("phagus", name) ~ 0.935,
                     grepl("Stomach", name) ~ 1.04,
                     grepl("Kidney", name) ~ .9,
                     grepl("Leukaemia", name) ~ .9,
                     grepl("Breast", name) ~ 1.01,
                     grepl("Liver", name) ~ 1.05),
         y=case_when(grepl("phagus", name) ~ .93,
                     grepl("Stomach", name) ~ .96,
                     grepl("Kidney", name) ~ 1.12,
                     grepl("Leukaemia", name) ~ 1.01,
                     grepl("Breast", name) ~ 1.06,
                     grepl("Liver", name) ~ 1.01),
         name=case_when(grepl("phagus", name) ~ "Esophagus",
                     grepl("Stomach", name) ~ "Stomach",
                     grepl("Kidney", name) ~ "Kidney, Colon/rectum, Larynx, Lymphoma, Ovary, Pancreas",
                     grepl("Leukaemia", name) ~ "Lung, Prostate, Melanoma, Leukaemia, Brain, Other",
                     grepl("Breast", name) ~ "Breast, Bladder, Cervix, Gallblader",
                     grepl("Liver", name) ~ "Liver, Uterus, Mouth/oropharynx" ))

small_groups_results_f2 %>% filter(category4=="NCDs") %>% arrange(type) %>% print(n=50)
ncd_anot<-small_groups_results_f2 %>% arrange(name) %>% 
  filter(grepl("NCD|Cirrhosis|Cardiovascular", name)) %>% 
  mutate(x=case_when(grepl("NCD", name) ~ 0.9,
                     grepl("Cirrhosis", name) ~ 0.95,
                     grepl("Cardiovascular", name) ~ 0.97),
         y=case_when(grepl("NCD", name) ~ 1.1,
                     grepl("Cirrhosis", name) ~ 0.95,
                     grepl("Cardiovascular", name) ~ 1.05),
         name=ifelse(grepl("Cardiovascular", name), "Cardiovascular, metabolic, digestive, respiratory,kidney, congenital, and neuropsychiatric ",name))
small_groups_results_f2 %>% filter(category4=="CMNN") %>% arrange(type) %>% print(n=50)
cmnn_anot<-small_groups_results_f2 %>% arrange(name) %>% 
  filter(grepl("Maternal", name)) %>% 
  mutate(x=case_when(grepl("Maternal", name) ~ .95),
         y=case_when(grepl("Maternal", name) ~ 1.06),
         name=ifelse(grepl("Maternal", name), "Respiratory and other infections, maternal conditions",name))


# figure 2: comparison of US and LAC with some labels
ggplot(data=small_groups_results_f2, aes(x=us, y=lac))+
  geom_abline(intercept=0, slope=1, lty=1, color="black")+
  annotate("label", x = 0.86, y=0.80, hjust=.5, size=6,
           label="Sub US/Sub LA", color=cols[1])+
  annotate("label", x = 0.86, y=1.25, hjust=.5, size=6,
           label="Sub US/Super LA", color=cols[7])+
  annotate("label", x = 1.22, y=0.80, hjust=.5, size=6,
           label="Super US/Sub LA", color=cols[3])+
  annotate("label", x = 1.22, y=1.25, hjust=.5, size=6,
           label="Super US/Super LA", color=cols[9])+
  geom_hline(yintercept=1,lty=2, color="black")+
  geom_vline(xintercept=1,lty=2, color="black")+
  geom_mark_ellipse(data=small_groups_results_f2 %>% 
                      filter(category4==("CMNN"),
                             type%in%c("Sub US / Linear LAC"),
                             lac>1),
                    con.type="elbow",con.border="none",
                    concavity = 5,expand=0.005,radius=0,aes(fill=type))+
  geom_mark_ellipse(data=small_groups_results_f2 %>% 
                   filter(category4==("Cancer"),
                          type%in%c("Sub US / Super LAC",
                                    "Linear US / Super LAC",
                                    "Sub US / Linear LAC",
                                    "Linear US / Linear LAC")),
                 con.type="elbow",con.border="none",
                 concavity = 5,expand=0.005,radius=0,aes(fill=type))+
  geom_mark_ellipse(data=small_groups_results_f2 %>% 
                   filter(category4==("NCDs"),
                          type%in%c("Sub US / Linear LAC")),
                 con.type="elbow",con.border="none",
                 concavity = 5,expand=0.01,radius=0,aes(fill=type))+
  geom_point(color="black", aes(fill=type), size=3, pch=21)+
  geom_text_repel(data=small_groups_results_f2 %>% 
                    filter(category4%in%c("CMNN"),
                           !grepl("Infect|Maternal", name)),
                 aes(label=name, color=type), min.segment.length = unit(10, "cm"),
                 size=5)+
  geom_text_repel(data=small_groups_results_f2 %>% 
                    filter(category4%in%c("Injuries")),
                  aes(label=name, color=type), min.segment.length = unit(10, "cm"),
                  size=5)+
  geom_text(data=cancer_anot %>% filter(!grepl("Lung", name)), 
            aes(x=x, y=y, label=str_wrap(name, width=40), color=type), 
            lineheight=0.75, size=5, hjust=0)+
  geom_text(data=cancer_anot %>% filter(grepl("Lung", name)), 
            aes(x=x, y=y, label=str_wrap(name, width=10), color=type), 
            lineheight=0.75, size=5, hjust=1)+
  geom_text(data=ncd_anot, aes(x=x, y=y, label=str_wrap(name, width=40), color=type), 
            lineheight=0.75, size=5, hjust=0)+
  geom_text(data=cmnn_anot, aes(x=x, y=y, label=str_wrap(name, width=30), color=type), 
            lineheight=0.75, size=5, hjust=0)+
  scale_color_manual(values=cols, drop=F,name="")+
  scale_fill_manual(values=cols, drop=F,name="")+
  scale_size_manual(values=c(rep(3, times=4), 
                             rep(3, times=4),
                             1.5))+
  scale_shape_manual(values=c(rep(21, times=4), rep(22, times=5)))+
  labs(x="Scaling Coefficient for US Cities",
       y="Scaling Coefficient for Latin American Cities")+
  guides(shape=F, size=F, color=F,
         fill=guide_legend(nrow=3, 
                           override.aes = list(size=6, pch=21, color=NA)))+
  scale_y_continuous(limits=c(0.80, 1.3))+
  scale_x_continuous(limits=c(0.80, 1.3))+
  facet_wrap(~category4, nrow=2)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        panel.border = element_rect(size=1.5),
        strip.background = element_blank(),
        legend.position = "bottom")
ggsave("results/Figure4.pdf", width=15, height=10)


full_join(small_groups_results_f2 %>% filter(category4=="CMNN"),
          small_groups_results_brmx %>% filter(category4=="CMNN")) %>% 
  select(-category4) %>% select(name, patternUS, patternLAC, patternMX, patternBR, us, lac, mx, br)
full_join(small_groups_results_f2 %>% filter(category4=="Cancer"),
          small_groups_results_brmx %>% filter(category4=="Cancer")) %>% 
  select(-category4) %>% select(name, patternUS, patternLAC, patternMX, patternBR, us, lac, mx, br) %>% 
  arrange(patternUS,patternLAC) %>% 
  print(n=50) 
full_join(small_groups_results_f2 %>% filter(category4=="NCDs"),
          small_groups_results_brmx %>% filter(category4=="NCDs")) %>% 
  select(-category4) %>% select(name, patternUS, patternLAC, patternMX, patternBR, us, lac, mx, br) %>% 
  arrange(patternUS,patternLAC) %>% 
  print(n=50) 
full_join(small_groups_results_f2 %>% filter(category4=="Injuries"),
          small_groups_results_brmx %>% filter(category4=="Injuries")) %>% 
  select(-category4) %>% select(name, patternUS, patternLAC, patternMX, patternBR, us, lac, mx, br) %>% 
  arrange(patternUS,patternLAC) %>% 
  print(n=50) 

table(small_groups_results_f2$dif)
table(small_groups_results_f2[,c("dif", "verydif")])

## ILL-DEFINED
ill_defined_collapse<-ill_defined_collapse %>% 
  left_join(acm)

map_dfr(model_list, function(model){
  if(model=="unadj"){
    m<-lm(formula=log(ill_defined_diseases)~log(pop), data=ill_defined_collapse)
  } else if (model=="adj"){
    m<-lm(formula=log(ill_defined_diseases)~log(pop)+iso2+prop15+prop40+prop65, data=ill_defined_collapse)
  } else {
    if (model=="LAC"){
      m<-lm(formula=log(ill_defined_diseases)~log(pop)+iso2+prop15+prop40+prop65, data=ill_defined_collapse %>% filter(us==0))
    } else if (model=="restLAC"){
      m<-lm(formula=log(ill_defined_diseases)~log(pop)+iso2+prop15+prop40+prop65, data=ill_defined_collapse %>% filter(us==0, !iso2%in%c("MX", "BR")))
    } else {
      m<-lm(formula=log(ill_defined_diseases)~log(pop)+prop15+prop40+prop65, data=ill_defined_collapse %>% filter(iso2==model))
    }
  }
  tidy(m) %>% 
    filter(term=="log(pop)") %>% 
    mutate(coef=paste0(new_format(estimate, digits=2), " (",
                       new_format(estimate-1.96*std.error, digits=2), ";",
                       new_format(estimate+1.96*std.error, digits=2), ")"),
           r2=(glance(m) %>% pull(r.squared)),
           r2=paste0(new_format(r2*100, digits=1), "%")) %>% 
    select(coef, r2) %>% 
    mutate(model=model)
})


# intercepts

small_groups_results_f3<-small_groups_models%>% 
  map_dfr(function(models){
    model_list<-names(models)
    model_list<-model_list[grepl("m_", model_list)]
    map_dfr(model_list, function(model){
      index<-which(names(models)%in%model)
      tidy(models[[index]]) %>% filter(term=="log(pop)") %>% mutate(area=model) %>% 
        mutate(intercept=tidy(models[[index]]) %>% filter(term=="(Intercept)") %>% pull(estimate),
               sd=sd(models[[index]]$model$`log(deaths)`),
               lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               category4=models$category,
               name=models$name) %>% 
        select(area, estimate, lci, uci, category4, name, intercept, sd)
    })
  })

  
f3a<-ggplot(small_groups_results_f3 %>%   filter(area%in%c("m_us", "m_lac")) %>% 
              mutate(area=factor(area, levels=c("m_us", "m_lac"),
                                 labels=c("US", "LA"))), 
       aes(x=intercept, y=estimate)) +
  geom_hline(yintercept = 1, lty=2)+
  stat_smooth(method="lm", lty=2, se=F, color="black")+
  geom_point(aes(fill=category4), pch=21, color="black", size=4) +
  labs(y=expression("Scaling Coefficient ("*beta*")"), 
       x=expression("Intercept ("*alpha*")"), fill="",
       title="Scaling vs Levels")+
  scale_fill_brewer(type="qual", palette=2)+
  facet_wrap(~area, nrow=1)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        plot.title =element_text(color="black", face="bold",size=20),
        panel.border = element_rect(size=1.5),
        strip.background = element_blank(),
        legend.position = "bottom")
f3b<-ggplot(small_groups_results_f3 %>%   filter(area%in%c("m_us", "m_lac")) %>% 
              mutate(area=factor(area, levels=c("m_us", "m_lac"),
                                 labels=c("US", "LA"))), 
       aes(x=sd, y=estimate)) +
  geom_hline(yintercept = 1, lty=2)+
  stat_smooth(method="lm", lty=2, se=F, color="black")+
  geom_point(aes(fill=category4), pch=21, color="black", size=4) +
  labs(y=expression("Scaling Coefficient ("*beta*")"), 
       x=expression("Standard Deviation ("*sigma*")"), fill="",
       title="Scaling vs Variability")+
  scale_fill_brewer(type="qual", palette=2)+
  facet_wrap(~area, nrow=1)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        plot.title =element_text(color="black", face="bold",size=20),
        panel.border = element_rect(size=1.5),
        strip.background = element_blank(),
        legend.position = "bottom")
legend<-get_legend(f3a)
f3a<-f3a+guides(fill=F)
f3b<-f3b+guides(fill=F)
f3<-arrangeGrob(grobs=list(f3a, f3b), ncol=1)
f3<-arrangeGrob(grobs=list(f3, legend), ncol=1, heights=c(10, 1))
ggsave("results/Figure5.pdf", f3, width=11, height=10)


small_groups_results_f3 %>% 
  group_by(area) %>% 
  group_modify(~{
    cora<-cor(.x$estimate, .x$intercept, method="spearman")
    corsd<-cor(.x$estimate, .x$sd, method="spearman")
    data.frame(cor_intercept=cora, cor_sd=corsd)
  }) %>% 
  filter(area%in%c("m_us", "m_lac"))


# sens analysis adding city rank
# get all-cause mortality by summing over all categories
acm<-acm %>% group_by(iso2) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank=row_number(),
         rank=ifelse(rank>10, 10, rank),
         rank=rank==1)
large_groups<-large_groups %>% group_by(iso2, category5) %>% 
  arrange(desc(pop)) %>% 
  mutate(rank=row_number(),
         rank=ifelse(rank>10, 10, rank),
         rank=rank==1)

row1_rank<-map(model_list, function(model){
  print(model)
  if(model=="unadj"){
    m<-lm(formula=log(deaths)~log(pop)+rank, data=acm)
  } else if (model=="adj"){
    m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=acm)
  } else {
    if (model=="LAC"){
      m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=acm %>% filter(us==0))
    } else if (model=="restLAC"){
      m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=acm %>% filter(us==0, !iso2%in%c("MX", "BR")))
    } else {
      m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65+rank, data=acm %>% filter(iso2==model))
    }
  }
  m
})

rows25_rank<-large_groups %>% 
  group_by(category5) %>% 
  group_map(~{
    map(model_list, function(model){
      #print(.y$category5);print(model)
      #.x<-large_groups %>% filter(category5=="Homicides");model<-"unadj"
      # TEMP
      .x<-.x %>% filter(deaths>0)
      if(model=="unadj"){
        m<-lm(formula=log(deaths)~log(pop)+rank, data=.x)
      } else if (model=="adj"){
        m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=.x)
      } else {
        if (model=="LAC"){
          m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=.x %>% filter(us==0))
        } else if (model=="restLAC"){
          m<-lm(formula=log(deaths)~log(pop)+iso2+prop15+prop40+prop65+rank, data=.x %>% filter(us==0, !iso2%in%c("MX", "BR")))
        } else {
          m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65+rank, data=.x %>% filter(iso2==model))
        }
      }
      list(m, outcome=.y$category5)
    })
  })
# extract
extract_main1<-map_dfr(row1, function(model){
  #model<-row1[[1]]
  coefs<-tidy(model) %>% 
    filter(term=="log(pop)") %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error) %>% 
    select(coef, lci, uci)
}) %>% 
  mutate(model=model_list,
         outcome=outcome_list_category5[[1]],
         type="main")
extract_rank1<-map_dfr(row1_rank, function(model){
  #model<-row1[[1]]
  coefs<-tidy(model) %>% 
    filter(term=="log(pop)") %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error) %>% 
    select(coef, lci, uci)
}) %>% 
  mutate(model=model_list,
         outcome=outcome_list_category5[[1]],
         type="rank")
extract_main25<-map_dfr(rows25, function(temp){
  #temp<-rows25[[1]]
  map_dfr(temp, function(model){
    #model<-temp[[1]]
    outcome<-model$outcome
    model<-model[[1]]
    coefs<-tidy(model) %>% 
      filter(term=="log(pop)") %>% 
      mutate(coef=estimate,
             lci=estimate-1.96*std.error,
             uci=estimate+1.96*std.error) %>% 
      select(coef, lci, uci) %>% mutate(outcome=outcome)
  }) %>% 
    mutate(model=model_list)
}) %>% mutate(type="main")
extract_rank25<-map_dfr(rows25_rank, function(temp){
  #temp<-rows25[[1]]
  map_dfr(temp, function(model){
    #model<-temp[[1]]
    outcome<-model$outcome
    model<-model[[1]]
    coefs<-tidy(model) %>% 
      filter(term=="log(pop)") %>% 
      mutate(coef=estimate,
             lci=estimate-1.96*std.error,
             uci=estimate+1.96*std.error) %>% 
      select(coef, lci, uci) %>% mutate(outcome=outcome)
  }) %>% 
    mutate(model=model_list)
}) %>% mutate(type="rank")

comparison_rank<-bind_rows(extract_rank1, extract_main1, extract_main25, extract_rank25)
comparison_rank<-full_join(comparison_rank %>% filter(type=="main") %>% select(-type) %>% 
            rename(coef_main=coef, lci_main=lci, uci_main=uci),
          comparison_rank %>% filter(type=="rank") %>% select(-type) %>% 
            rename(coef_rank=coef, lci_rank=lci, uci_rank=uci)) %>% 
  mutate(category5=factor(outcome,
                          levels=c("All-Cause Mortality", "CMNN", "Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides"))) %>% 
  filter(model%in%c("US", "restLAC", "MX", "BR")) %>% 
  mutate(model=factor(model, levels=c("US", "restLAC", "MX", "BR"),
                      labels=c("US", "LAC (-MX/BR)", "MX", "BR")))
          

ggplot(comparison_rank, aes(x=coef_main, y=coef_rank)) +
  geom_abline(intercept = 0, slope=1, lty=2)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_linerange(aes(ymin=lci_rank, ymax=uci_rank, color=category5))+
  geom_linerange(aes(xmin=lci_main, xmax=uci_main, color=category5))+
  geom_point(aes(fill=category5), pch=21, color="black") +
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(limits=c(min(comparison_rank %>% select(lci_rank, lci_main)),
                              max(comparison_rank %>% select(uci_rank, uci_main))))+
  scale_y_continuous(limits=c(min(comparison_rank %>% select(lci_rank, lci_main)),
                              max(comparison_rank %>% select(uci_rank, uci_main))))+
  labs(x="Scaling Coefficient (95% CI) from the main model",
       y="Scaling Coefficient (95% CI) adding covariate for top city",
       title="")+
  facet_wrap(~model) +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background = element_blank(),
        legend.position = "bottom")
ggsave("results/Supplementary_Figure_S3.pdf", width=8, height=8)

# cz sensitivity analysis
load("analytic files/ms38data_cz.rdata")
# Table 1: overall, and five large groupings, unadjusted, adjusted, US, LAC, BR, MX
# get all-cause mortality by summing over all categories
acm_cz<-mortality_us_cz %>% 
  group_by(city) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_us_cz) %>% 
  left_join(popage_us_cz)
summary(acm_cz);head(acm_cz);nrow(acm_cz)
# get results for first row
outcome_list_category5<-c("All-Cause Mortality", "CMNN", "Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")
model_list<-c("unadj", "adj")
#model<-model_list[[1]]
res1_cz<-map_dfr(model_list, function(model){
  if(model=="unadj"){
    m<-lm(formula=log(deaths)~log(pop), data=acm_cz)
  } else if (model=="adj"){
    m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=acm_cz)
  } 
  m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
})
large_groups_cz<-mortality_us_cz %>% 
  left_join(cause_names) %>% 
  group_by(city, category5) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_us_cz) %>% 
  left_join(popage_us_cz)
res2_cz<-large_groups_cz %>% 
  group_by(category5) %>% 
  group_modify(~{
    map_dfr(model_list, function(model){
      #print(.y$category5);print(model)
      #.x<-large_groups %>% filter(category5=="Homicides");model<-"unadj"
      # TEMP
      .x<-.x %>% filter(deaths>0)
      if(model=="unadj"){
        m<-lm(formula=log(deaths)~log(pop), data=.x)
      } else if (model=="adj"){
        m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x)
      } 
      m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
    })
  })
# ua sensitivity analysis
load("analytic files/ms38data_ua.rdata")
# Table 1: overall, and five large groupings, unadjusted, adjusted, US, LAC, BR, MX
# get all-cause mortality by summing over all categories
acm_ua<-mortality_us_ua %>% 
  group_by(city) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_us_ua) %>% 
  left_join(popage_us_ua)
summary(acm_ua);head(acm_ua);nrow(acm_ua)
# get results for first row
outcome_list_category5<-c("All-Cause Mortality", "CMNN", "Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")
model_list<-c("unadj", "adj")
#model<-model_list[[1]]
res1_ua<-map_dfr(model_list, function(model){
  if(model=="unadj"){
    m<-lm(formula=log(deaths)~log(pop), data=acm_ua)
  } else if (model=="adj"){
    m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=acm_ua)
  } 
  m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
})
large_groups_ua<-mortality_us_ua %>% 
  left_join(cause_names) %>% 
  group_by(city, category5) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_us_ua) %>% 
  left_join(popage_us_ua)
res2_ua<-large_groups_ua %>% 
  group_by(category5) %>% 
  group_modify(~{
    map_dfr(model_list, function(model){
      #print(.y$category5);print(model)
      #.x<-large_groups %>% filter(category5=="Homicides");model<-"unadj"
      # TEMP
      .x<-.x %>% filter(deaths>0)
      if(model=="unadj"){
        m<-lm(formula=log(deaths)~log(pop), data=.x)
      } else if (model=="adj"){
        m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x)
      } 
      m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
    })
  })


# repeat for cbsas
acm_cbsa<-mortality_both_collapse %>% 
  filter(us==1) %>% 
  group_by(city) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_both_collapse) %>% 
  left_join(popage_collapse)
summary(acm_cbsa);head(acm_cbsa);nrow(acm_cbsa)
res1_cbsa<-map_dfr(model_list, function(model){
  if(model=="unadj"){
    m<-lm(formula=log(deaths)~log(pop), data=acm_cbsa)
  } else if (model=="adj"){
    m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=acm_cbsa)
  } 
  m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
})
large_groups_cbsa<-mortality_both_collapse %>% 
  filter(us==1) %>% 
  left_join(cause_names) %>% 
  group_by(city, category5) %>% 
  summarise(deaths=sum(deaths)) %>% 
  left_join(pop_both_collapse) %>% 
  left_join(popage_collapse)
res2_cbsa<-large_groups_cbsa %>% 
  group_by(category5) %>% 
  group_modify(~{
    map_dfr(model_list, function(model){
      #print(.y$category5);print(model)
      #.x<-large_groups %>% filter(category5=="Homicides");model<-"unadj"
      # TEMP
      .x<-.x %>% filter(deaths>0)
      if(model=="unadj"){
        m<-lm(formula=log(deaths)~log(pop), data=.x)
      } else if (model=="adj"){
        m<-lm(formula=log(deaths)~log(pop)+prop15+prop40+prop65, data=.x)
      } 
      m %>% tidy %>% filter(term=="log(pop)") %>% mutate(model=model)
    })
  })

comparison<-bind_rows(res1_cbsa %>% mutate(level="cbsa"),
                      res1_cz %>% mutate(level="cz"),
                      res1_ua %>% mutate(level="ua")) %>% 
  mutate(category5=outcome_list_category5[[1]]) %>% 
  bind_rows(res2_cbsa %>% mutate(level="cbsa"),
            res2_cz %>% mutate(level="cz"),
            res2_ua %>% mutate(level="ua")) %>% 
  mutate(category5=factor(category5, levels=outcome_list_category5),
         coef=estimate,
         lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error) %>% 
  select(category5, model, level, coef, lci, uci)
comparison<-full_join(comparison %>% select(category5, model, level, coef) %>% 
            spread(level, coef),
  comparison %>% select(category5, model, level, lci) %>% 
    spread(level, lci) %>% 
    rename(cbsa_lci=cbsa, cz_lci=cz, ua_lci=ua)) %>% 
  full_join(comparison %>% select(category5, model, level, uci) %>% 
    spread(level, uci) %>% 
    rename(cbsa_uci=cbsa, cz_uci=cz, ua_uci=ua)) %>% 
  mutate(model=factor(model, levels=c("unadj", "adj"),
                      labels=c("Unadjusted", "Adjusted for age")))
p1<-ggplot(comparison, aes(x=cbsa, y=cz)) +
  geom_abline(intercept = 0, slope=1, lty=2)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_linerange(aes(ymin=cz_lci, ymax=cz_uci, color=category5))+
  geom_linerange(aes(xmin=cbsa_lci, xmax=cbsa_uci, color=category5))+
  geom_point(aes(fill=category5), pch=21, color="black") +
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(limits=c(min(comparison %>% select(cz_lci, cbsa_lci)),
                              max(comparison %>% select(cz_uci, cbsa_uci))))+
  scale_y_continuous(limits=c(min(comparison %>% select(cz_lci, cbsa_lci)),
                              max(comparison %>% select(cz_uci, cbsa_uci))))+
  labs(x="Scaling Coefficient (95% CI) using CBSAs",
       y="Scaling Coefficient (95% CI) using Commuting Zones",
       title="Commuting Zones vs Core-Based Statistical Areas")+
  facet_wrap(~model) +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background = element_blank(),
        legend.position = "bottom")
p2<-ggplot(comparison, aes(x=cbsa, y=ua)) +
  geom_abline(intercept = 0, slope=1, lty=2)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_linerange(aes(ymin=ua_lci, ymax=ua_uci, color=category5))+
  geom_linerange(aes(xmin=cbsa_lci, xmax=cbsa_uci, color=category5))+
  geom_point(aes(fill=category5), pch=21, color="black") +
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  scale_x_continuous(limits=c(min(comparison %>% select(ua_lci, cbsa_lci)),
                              max(comparison %>% select(ua_uci, cbsa_uci))))+
  scale_y_continuous(limits=c(min(comparison %>% select(ua_lci, cbsa_lci)),
                              max(comparison %>% select(ua_uci, cbsa_uci))))+
  labs(x="Scaling Coefficient (95% CI) using CBSAs",
       y="Scaling Coefficient (95% CI) using Urban Areas",
       title="Urban Areas vs Core-Based Statistical Areas")+
  facet_wrap(~model) +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16),
        strip.text =element_text(color="black", face="bold",size=16),
        legend.text =element_text(color="black", face="bold",size=16),
        strip.background = element_blank(),
        legend.position = "bottom")
pall<-arrangeGrob(grobs=list(p1, p2), ncol=1)
ggsave("results/Supplementary_Figure_S2.pdf", pall, width=12, height=14)

comparison %>% group_by(model) %>% 
  group_modify(~{
    data.frame(corcz=cor(.x$cz, .x$cbsa, method="spearman"),
               corua=cor(.x$ua, .x$cbsa, method="spearman"))
    })
