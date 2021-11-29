# Programmer:  Lu Tianyu (Sky)
# Student ID: 1930026092
# Date:2021/11/28

source("./lty.R")

# packages for drawing a word cloud
library(jiebaR,jiebaRD) 
library(wordcloud2)

donations <- read_csv("donations.csv") %>% na.omit()
projects  <- read_csv("projects.csv")

projects <- projects %>% subset(select = -c(project_id)) 
projects <- projects %>% rename(project_id = id)

proj <- projects %>% select(project_id, percent_total) %>% na.omit()
proj <- proj %>% filter(percent_total >= 100.0)

don_proj <- left_join(donations, proj, by = "project_id") %>% na.omit()

# ----------------------------------------------------------------

save_dens_plot_con(don_proj %>% select(total_amount))
save_freq_plot_dis(don_proj %>% select(dollar_amount))
save_freq_plot_dis(don_proj %>% select(payment_method))
save_freq_plot_dis(don_proj %>% select(was_promo_matched))
save_freq_plot_dis(don_proj %>% select(via_giving_page))
save_freq_plot_dis(don_proj %>% select(for_honoree))

save_freq_plot_dis(don_proj %>% select(includes_campaign_gift_card))
save_freq_plot_dis(don_proj %>% select(includes_web_gift_card))
save_freq_plot_dis(don_proj %>% select(includes_acct_credit))

# ----------------------------------------------------------------
# show(cor(x = don_proj[,"for_honoree"], y = don_proj[,"total_amount"]))
# # variable "for_honoree" , corr = 0.01631839
# show(cor(x = don_proj[,"was_promo_matched"], y = don_proj[,"total_amount"]))
# # =variable "was_promo_matched" , corr = -0.01380398
# show(cor(x = don_proj[,"via_giving_page"], y = don_proj[,"total_amount"]))
# # variable "via_giving_page", corr = 0.03855945
# show(cor(x = don_proj[,"funded"], y = don_proj[,"total_amount"]))
# # variable "funded", corr = -0.007831298

bianryVar_donationProportion <- function(don_proj_grouped){
  don_tmp <- don_proj_grouped %>% summarise(sum = sum(total_amount))
  var_0 <- don_tmp[1,2] / (don_tmp[1,2] + don_tmp[2,2])
  var_1 <- don_tmp[2,2] / (don_tmp[1,2] + don_tmp[2,2])
  return(c(var_0,var_1))
}

show(bianryVar_donationProportion(don_proj %>% group_by(via_giving_page)))
# via_giving_page = 0, percent of donation = 0.7030444
# via_giving_page = 1, percent of donation = 0.2969552

show(bianryVar_donationProportion(don_proj %>% group_by(was_promo_matched)))
# was_promo_matched = 0, percent of donation = 0.9882232
# was_promo_matched = 1, percent of donation = 0.01177684

show(bianryVar_donationProportion(don_proj %>% group_by(for_honoree)))
# for_honoree = 0, percent of donation = 0.9731194
# for_honoree = 1, percent of donation = 0.02688055

show(bianryVar_donationProportion(don_proj %>% 
                                    group_by(includes_acct_credit)))
# includes_acct_credit = 0, percent of donation = 0.9882232
# includes_acct_credit = 1, percent of donation = 0.01177684

show(bianryVar_donationProportion(don_proj %>% 
                                    group_by(includes_web_gift_card)))
# includes_web_gift_card = 0, percent of donation = 0.971305
# includes_web_gift_card = 1, percent of donation = 0.02869503

show(bianryVar_donationProportion(don_proj %>% 
                                    group_by(includes_campaign_gift_card)))
# includes_web_gift_card = 0, percent of donation = 0.7549167
# includes_web_gift_card = 1, percent of donation = 0.2450833


don_tmp <- don_proj %>% group_by(payment_method) %>% 
  summarise(sum = sum(total_amount))
show(don_tmp)


# ----------------------------------------------------------------

text <- don_proj$donation_message

mixseg <- worker("mix", stop_word = "stopwords_eng.txt")
a <- segment(text, mixseg)
freq <- table(a)
wordcloud2(freq, size = 3)






