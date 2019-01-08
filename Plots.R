# PART 3: Plots of different scenarios ####
pacman::p_load(cowplot, tidyverse, lubridate)

convertP.ug = 30.97 # P conversion factor: mmol/m3 to ug/L

dataLong <- list.files(path = "./Hedonic_sims/output/", pattern="*Long.csv", full.names = T) %>% 
  map_df(~read_csv(.)) %>%
  mutate(sim = as.factor(sim), 
         sim2 = recode(sim, "0" = "100% decrease",
                       "0.25" = "75% decrease",
                       "0.5" = "50% decrease",
                       "0.75" = "25% decrease",
                       "1" = "Baseline",
                       "1.25" = "25% increase",
                       "1.5" = "50% increase",
                       "1.75" = "75% increase",
                       "2" = "100% increase"),
         year = year(DateTime),
         value = ifelse(variable == 'TP', value*convertP.ug, value))

dataWide <- list.files(path = "./Hedonic_sims/output/", pattern="*Wide.csv", full.names = T) %>% 
  map_df(~read_csv(.)) %>%
  mutate(sim = as.factor(sim), 
         year = year(DateTime), 
         month = month(DateTime), 
         TP = TP*convertP.ug)

jet.colors <- colorRampPalette(c("cyan", "deepskyblue", "dodgerblue1", "dodgerblue4", # less than baseline
                                 "black",  # baseline
                                 "yellow", "orange2", "red1", "red4")) # greater than baseline

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=16, colour='black'), axis.text.y=element_text(size=16, colour='black'), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))

# Time series Baseline plots ####
secchiPlot1 <- ggplot(subset(dataLong, variable =="Secchi" & sim=='1' & year >2007), 
                      aes(x= DateTime, y = value)) +
  geom_point(colour='black') + mytheme + 
  scale_colour_manual(values=jet.colors(9)) + # use with as.factor
  scale_y_continuous("Secchi depth (m)", limits=c(0,10), breaks=seq(0,10,2)) +
  scale_x_datetime("",date_breaks="1 year", date_labels=" ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

chlaPlot1 <- ggplot(subset(dataLong, variable=='Chla' & sim=='1' & year >2007), 
                    aes(x= DateTime, y = value)) +
  geom_point(col='black') + mytheme + 
  scale_colour_manual("Scenario", values=jet.colors(9)) + # use with as.factor
  scale_y_continuous(limits=c(0,200)) +
  labs(y = expression(Surface~chl-a~~'('*mu*'g'~~L^-1*')')) +
  scale_x_datetime("",date_breaks="1 year", date_labels="%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') 

plot_grid(secchiPlot1, chlaPlot1, ncol=1, axis="l", labels=c("A","B")) # Figure 1 in Hedonic

# Time series plots with all scenarios ####
secchiPlot <- ggplot(data=subset(dataLong, variable=='Secchi'& year >2007), 
                     aes(x= DateTime, y = value, fill=sim2, group=sim2, color=sim2)) +
  geom_line(lwd=1.05) + mytheme + 
  scale_colour_manual(values=jet.colors(9)) + # use with as.factor
  scale_y_continuous("Secchi depth (m)", limits=c(0,10), breaks=seq(0,10,2)) +
  scale_x_datetime("",date_breaks="1 year", date_labels=" ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'right')+
  guides(colour = guide_legend(ncol = 1)) 

chlaPlot <- ggplot(data=subset(dataLong, variable=='Chla'& year >2007), aes(x= DateTime, y = value,  fill=sim2, group=sim2, color=sim2)) +
  geom_line(lwd=1.05) + mytheme + 
  scale_colour_manual("Scenario", values=jet.colors(9)) + # use with as.factor
  scale_y_continuous(limits=c(0,200)) +
  scale_x_datetime("",date_breaks="1 year", date_labels="%b-%Y") +
  labs(y = expression(Surface~chl-a~~'('*mu*'g'~~L^-1*')')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='right') +
  guides(colour = guide_legend(ncol = 1)) 

plot_grid(secchiPlot, chlaPlot, ncol=1, align='hv', labels=c("A","B")) # Figure 3 in Hedonic

# Time series plots: Extreme max/min and baseline scenarios only ####
a <- ggplot(data=subset(dataLong, variable=='Secchi' & (sim=='0'|sim=='1'|sim=='2')), aes(x= DateTime, y = value, fill=sim, group=sim, colour=sim)) +
  geom_line(lwd=1.05) + mytheme + 
  scale_colour_manual("Scenario",values=jet.colors(3)) + # use with as.factor
  scale_y_continuous("Secchi (m)", limits=c(0,12), breaks=seq(0,12,2)) +
  scale_x_datetime(date_breaks="1 year", date_labels=" ") +
  theme(axis.text.x = element_blank(), axis.title.x=element_blank(), legend.position='top')

b <- ggplot(data=subset(dataLong, variable=='Chla' & (sim=='0'|sim=='1'|sim=='2')), aes(x= DateTime, y = value, fill=sim, group=sim, colour=sim)) +
  geom_line(lwd=1.05) + mytheme + 
  scale_colour_manual(values=jet.colors(3)) +
  scale_y_continuous("Surface chl-a (ug/L)", limits=c(0,200)) +
  scale_x_datetime(date_breaks="1 year", date_labels="%b-%Y")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x=element_blank(), legend.position="none")

plot_grid(a,b, align='hv', ncol=1, axis="l")

#### Summer (Jun-Aug) only ####
summerLong <- dataLong %>% mutate(month = month(DateTime), day = day(DateTime), 
                                  Jday = yday(DateTime)) %>%
  mutate(Date = as.Date(with(., paste(month, day,sep="-")), "%m-%d")) %>%
  filter(month %in% c(6,7,8), year > 2007)

# Time series: Baseline summer 
secchiSummer <- ggplot(subset(summerLong, variable =="Secchi" & sim=='1'), 
                       aes(x= DateTime, y = value)) +
  geom_point(colour='black') + mytheme + 
  scale_colour_manual(values=jet.colors(9)) + # use with as.factor
  scale_y_continuous("Secchi depth (m)", limits=c(0,10), breaks=seq(0,10,2)) +
  scale_x_datetime("",date_breaks="6 months", date_labels=" ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

chlaSummer <- ggplot(subset(summerLong, variable=='Chla' & sim=='1'), 
                     aes(x= DateTime, y = value)) +
  geom_point(col='black') + mytheme + 
  scale_colour_manual("Scenario", values=jet.colors(9)) + # use with as.factor
  scale_y_continuous(limits=c(0,200)) +
  labs(y = expression(Surface~chl-a~~'('*mu*'g'~~L^-1*')')) +
  scale_x_datetime("",date_breaks="6 months", date_labels="%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') 

plot_grid(secchiSummer, chlaSummer, ncol=1, axis="l", labels=c("A","B")) # Figure 1 in Hedonic

# Current Fig 4 in WW hedonic paper ####
secchiJday <- ggplot(subset(summerLong, variable =="Secchi" & sim=='1'), 
                     aes(x= Date, y = value, fill=as.factor(year))) +
  geom_point(pch=21) + mytheme + 
  geom_smooth(col='black',linetype=0, aes(fill = as.factor(year)), show.legend=F) +
  scale_colour_manual("Year", values=jet.colors(9)) + # use with as.factor
  scale_y_continuous("Secchi depth (m)", limits=c(0,6), breaks=seq(0,6,1)) +
  #scale_x_datetime("", date_labels="%b-%d", limits=as.Date(c('2018-06-01', '2018-09-02'))) + 
  guides(fill=guide_legend("Year", ncol=2, override.aes = list(size=5))) +
  theme(legend.position = c(1,1), legend.justification = c(1,1), legend.title.align=0.5)

chlaJday <- ggplot(subset(summerLong, variable =="Chla" & sim=='1'), 
                   aes(x= Date, y = value, fill=as.factor(year))) +
  geom_point(pch=21) + mytheme + 
  geom_smooth(col='black',linetype=0, aes(fill = as.factor(year)), show.legend=F) +
  scale_colour_manual(values=jet.colors(9)) + # use with as.factor
  labs(y = expression(Surface~chl-a~~'('*mu*'g'~~L^-1*')')) +
  scale_y_continuous(limits=c(0,200)) +
  #scale_x_datetime(date_labels="%b-%d", limits=as.Date(c('2018-06-01', '2018-09-02'))) + 
  theme(legend.position = 'none')

plot_grid(secchiJday, chlaJday, ncol=1, align='hv', labels=c("A","B"))

# Current Fig 5 in WW hedonic paper ####
scenarioMean <- summerLong %>% group_by(sim, variable) %>% 
  summarize_at(vars(value),
               funs(mean(., na.rm=T), se=sd(., na.rm=T)/sqrt(n()), min(., na.rm=T), max(., na.rm=T)))

baselineMeans <- scenarioMean %>% filter(sim == 1) %>% 
  mutate_at(3:6, round, 2)

my.labels <- c("100%\ndecrease","75%\ndecrease","50%\ndecrease","25%\ndecrease","Baseline","25%\nincrease",
               "50%\nincrease","75%\nincrease","100%\nincrease") 

summerSecchi <- ggplot(subset(summerLong, variable =="Secchi"), aes(x= sim2, y = value, fill=sim)) +
  geom_jitter(pch=21, col='black', fill='gray50', alpha=0.2) +
  geom_boxplot(alpha=0.7) + 
  stat_summary(fun.y=mean, geom="point",shape=18, size=5,show.legend = FALSE) +
  scale_fill_manual(values=jet.colors(9)) + 
  scale_y_continuous("Secchi depth (m)", limits=c(0,8), breaks=seq(0,8,2)) +
  #scale_x_discrete("", labels= my.labels) +
  theme(legend.position = 'none', axis.text.x=element_blank(),axis.title.x=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

summerChla <- ggplot(subset(summerLong, variable =="Chla"), aes(x= sim2, y = value, fill=sim)) +
  geom_jitter(pch=21, col='black', fill='gray50', alpha=0.2) +
  geom_boxplot(alpha=0.7) + 
  stat_summary(fun.y=mean, geom="point",shape=18, size=5,show.legend = FALSE) +
  scale_fill_manual(values=jet.colors(9)) + 
  scale_y_continuous(limits=c(0,200)) +
  scale_x_discrete("Scenario", labels= my.labels) +
  labs(y = expression(Surface~chl-a~~'('*mu*'g'~~L^-1*')')) +
  theme(legend.position = 'none', axis.text.x = element_text(vjust=1),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

plot_grid(summerSecchi, summerChla, ncol=1, axis="l", labels=c("A","B"), align= 'hv') # Figure 1 in Hedonic