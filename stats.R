library(lmerTest)
source("read_data.R")
# fit tsc model
m_tsc <- lmer(log_mean_count ~ (1|FarmName:Visit) +
            (1|FarmName) +
            Visit +
              observed +
            I(100*hyg2_agg_prop) +
            I(100*hyg3_agg_prop) +
            I(100*hyg4_agg_prop) +
            I(100*teat_rough_agg_prop) +
            I(100*teat_smooth_agg_prop) +
            I(100*teat_veryrough_agg_prop) +
            kickoffs_per_hundred +
            intervention_recoded,
          data=tsc)
qqnorm(resid(m_tsc));qqline(resid(m_tsc)) # not great
plot(predict(m_tsc),resid(m_tsc)) # looks fine
summary(m_tsc)


# fit msc model
m_msc <- lmer(log_mean_count ~ (1|FarmName:Visit) +
            (1|FarmName) +
            Visit +
              observed+
            I(100*hyg2_agg_prop) +
            I(100*hyg3_agg_prop) +
            I(100*hyg4_agg_prop) +
            I(100*teat_rough_agg_prop) +
            I(100*teat_smooth_agg_prop) +
            I(100*teat_veryrough_agg_prop) +
            kickoffs_per_hundred +
            intervention_recoded,
          data=msc)
qqnorm(resid(m_msc));qqline(resid(m_msc)) # looks fine
plot(predict(m_msc),resid(m_msc)) # looks fine
summary(m_msc)

#fit tsc towel model 
m_tow_tsc <- lmer(log_mean_count ~ (1|FarmName:Visit) +
                (1|FarmName) +
                Visit +
                observed +
                I(100*hyg2_agg_prop) +
                I(100*hyg3_agg_prop) +
                I(100*hyg4_agg_prop) +
                I(100*teat_rough_agg_prop) +
                I(100*teat_smooth_agg_prop) +
                I(100*teat_veryrough_agg_prop) +
                kickoffs_per_hundred +
                intervention_recoded,
              data=ttsc)
qqnorm(resid(m_tow_tsc));qqline(resid(m_tow_tsc)) 
plot(predict(m_tow_tsc),resid(m_tow_tsc)) 
summary(m_tow_tsc)

# fit towel msc model
m_tow_msc <- lmer(log_mean_count ~ (1|FarmName:Visit) +
                (1|FarmName) +
                Visit +
                observed+
                I(100*hyg2_agg_prop) +
                I(100*hyg3_agg_prop) +
                I(100*hyg4_agg_prop) +
                I(100*teat_rough_agg_prop) +
                I(100*teat_smooth_agg_prop) +
                I(100*teat_veryrough_agg_prop) +
                kickoffs_per_hundred +
                intervention_recoded,
              data=tmsc)
qqnorm(resid(m_tow_msc));qqline(resid(m_tow_msc)) 
plot(predict(m_tow_msc),resid(m_tow_msc)) 
summary(m_tow_msc)

