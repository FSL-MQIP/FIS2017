### bulk tank counts over time
library(ggplot2)
tsc %>%
  group_by(FarmID,Visit,DayFromTraining) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=Visit)) +
  geom_line() +
  facet_grid(~FarmID)

msc %>%
  group_by(FarmID,Visit,DayFromTraining) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=Visit)) +
  geom_line() +
  facet_grid(~FarmID)


### bulk tank counts by shift time
tsc %>%
  group_by(ShiftStartHour,intervention_recoded,FarmID) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(y=log_mean_count,x=ShiftStartHour,color=intervention_recoded)) +
  geom_point(position=position_dodge(1)) +
  ylim(-0.35, 0.95) +
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se),
                position=position_dodge(1)) +
  facet_grid(.~FarmID)

msc %>%
  group_by(ShiftStartHour,intervention_recoded,FarmID) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(y=log_mean_count,x=ShiftStartHour,color=intervention_recoded)) +
  geom_point(position=position_dodge(1)) +
  ylim(-0.35, 0.95) +
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se),
                position=position_dodge(1)) +
  facet_grid(.~FarmID)

### bulk tank counts over time, aggregated
tsc %>%
  group_by(DayFromTraining,Visit,intervention_recoded) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=intervention_recoded)) +
  labs(title="TSC") +
  geom_point() +
  ylim(-0.4,0.55)+
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 1, 2, 3, 4),
                     labels = c(-7, -5, -3, -1, 1, 3, 5, 7))+
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se)) +
  facet_grid(~Visit)


msc %>%
  group_by(DayFromTraining,Visit,intervention_recoded) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=intervention_recoded)) +
  labs(title="MSC") +
  geom_point() +
  ylim(-0.4,0.55)+
  scale_x_continuous(breaks = c(-4, -3, -2, -1, 1, 2, 3, 4),
                     labels = c(-7, -5, -3, -1, 1, 3, 5, 7))+
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se)) +
  facet_grid(~Visit)

msc %>%
  ggplot(aes(x=intervention_recoded,
             y=log_mean_count,
             color=intervention_recoded)) +
  labs(x="",y="CHANGE ME",guide="") +
  scale_color_discrete("")+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=60,hjust=1))

#MSC by farm
msc %>%
  ggplot(aes(x=intervention_recoded,
             y=log_mean_count,
             color=intervention_recoded)) +
  labs(x="",y="Spore Count (CFU/mL)",guide="") +
  scale_color_discrete("")+
  geom_boxplot() +
  ylim(-2.0, 2.0)+
  facet_grid(~FarmID) +
  theme(axis.text.x = element_blank())

#TSC by farm
tsc %>%
  ggplot(aes(x=intervention_recoded,
             y=log_mean_count,
             color=intervention_recoded)) +
  labs(x="",y="Spore Count (CFU/mL",guide="") +
  scale_color_discrete("")+
  geom_boxplot() +
  ylim(-2.0, 2.0)+
  facet_grid(~FarmID) +
  theme(axis.text.x = element_blank())


#overall BTM and towel graph
library(multcomp)
micro<-read.csv("Query_for_Stats.txt")

micro$FarmNameCoded <- micro$FarmName
levels(micro$FarmNameCoded) <- c("A","B","C","D","E")

micro %>%
  mutate(mean_count=ifelse(Type=="BTM",
                           (Rep1+Rep2+Rep3+Rep4+Rep5+Rep6+Rep7+Rep8+Rep9+Rep10)/10,
                           (Rep1+Rep2)/2)) %>%
  mutate(log_mean_count=log10(mean_count)) %>%
  filter(Test!="PSC") %>%
  mutate(intervention_recoded=ifelse(PostIntervention=="Yes","After Intervention","Before Intervention")) -> micro

micro$intervention_recoded <- factor(micro$intervention_recoded,
                                     levels=c("Before Intervention",
                                              "After Intervention"))


# Pretty ggplot boxplot
ggplot(micro,
       aes(x=Test,
           y=mean_count,
           color=intervention_recoded)) +
  geom_boxplot() +
  labs(x="Spore Test",
       y="Spore Count (CFU/mL)",
       color="Sample Taken") +
  scale_y_log10() +
  facet_wrap(~Type)

ggplot(micro[micro$Type=="BTM",],
       aes(x=Test,
           y=mean_count,
           color=intervention_recoded)) +
  geom_boxplot() +
  labs(x="Spore Test",
       y="Spore Count (CFU/mL)",
       color="Sample Taken") +
  scale_y_log10() +
  facet_wrap(~FarmNameCoded)

ggplot(micro[micro$Type=="Towel",],
       aes(x=Test,
           y=mean_count,
           color=intervention_recoded)) +
  geom_boxplot() +
  labs(x="Spore Test",
       y="Spore Count (CFU/mL)",
       color="Sample Taken") +
  scale_y_log10() +
  facet_wrap(~FarmNameCoded)

### towel counts over time, aggregated
ttsc %>%
  group_by(DayFromTraining,Visit,intervention_recoded) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=intervention_recoded)) +
  labs(title="TSC") +
  geom_point() +
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se)) +
  facet_grid(~Visit)

tmsc %>%
  group_by(DayFromTraining,Visit,intervention_recoded) %>%
  summarize(se=sd(log_mean_count)/sqrt(n()),
            log_mean_count=mean(log_mean_count)) %>%
  ggplot(aes(x=DayFromTraining,y=log_mean_count,color=intervention_recoded)) +
  labs(title="MSC") +
  geom_point() +
  geom_errorbar(aes(ymin=log_mean_count-se,
                    ymax=log_mean_count+se)) +
  facet_grid(~Visit)

tmsc %>%
  ggplot(aes(x=intervention_recoded,
             y=log_mean_count,
             color=intervention_recoded)) +
  labs(x="",y="CHANGE ME",guide="") +
  scale_color_discrete("")+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=60,hjust=1))

ttsc %>%
  ggplot(aes(x=intervention_recoded,
             y=log_mean_count,
             color=intervention_recoded)) +
  labs(x="",y="CHANGE ME",guide="") +
  scale_color_discrete("")+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=60,hjust=1))

