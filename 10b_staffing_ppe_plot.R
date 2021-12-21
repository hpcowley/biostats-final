###############################################################################
# Biostatistics 652 Final Project
# Create forest plots from 10a_staff_ppe_analysis.R
# December 2021
###############################################################################
library(dplyr)
library(writexl)
library(gtsummary)
library(abd)
library(dplyr)
library(ggplot2)
library(forestplot)
library(tibble)
library(gridExtra)

#MAKE THE FOREST PLOTS AND STUFF
rescases2
staffcases2


#######for rescases2
#sort the data
arrange(rescases2, exposure)

rescases2 <- rescases2 %>%
    select(-Index)

rescases2a <-  rescases2 %>% 
    add_column(Index = c(1,4,7,10,13,16,2,5,8,11,14,17,3,6,9,12,15,18))

rescases2a <- rescases2a %>% arrange(Index)

rescases2ordered <- rescases2a

###constructing the forest plot
as_tibble(rescases2ordered)
#rescases2ordered <- rename(rescases2ordered, "Lower 95% CI Limit" = lower_CI)
#rescases2ordered <- rename(rescases2ordered, "Upper 95% CI Limit" = upper_CI)


plot1 <- ggplot(rescases2ordered, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:18, labels = rescases2ordered$exposure, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab("Exposure") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1 + labs(title = "OR for Exposure vs Resident Infections \n Stratified by 75% Staff Vaccination Rate", y ="Exposure")

table_base <- ggplot(rescases2ordered, aes(y=exposure)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(OR, digits = 1))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(plot1, tab1, tab2, layout_matrix = lay)





#####forest plot for staff cases

arrange(staffcases2, exposure)

staffcases2 <- staffcases2 %>%
  select(-Index)

staffcases2a <-  staffcases2 %>% 
  add_column(Index = c(1,4,7,10,13,16,2,5,8,11,14,17,3,6,9,12,15,18))

staffcases2a <- staffcases2a %>% arrange(Index)

staffcases2ordered <- staffcases2a

###constructing the forest plot
as_tibble(staffcases2ordered)
#staffcases2ordered <- rename(staffcases2ordered, "Lower 95% CI Limit" = lower_CI)
#staffcases2ordered <- rename(staffcases2ordered, "Upper 95% CI Limit" = upper_CI)


plot2 <- ggplot(staffcases2ordered, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:18, labels = staffcases2ordered$exposure, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab("Exposure") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot2 + labs(title = "OR for Exposure vs Staff Infections \n Stratified by 75% Staff Vaccination Rate", ylab ="Exposure")






#####forest plot for staff vs resident cases



staffvres <-  staff2res2_OR_table %>% 
  add_column(Index = c(1,2,3))

staffvres2 <- staffvres %>% arrange(Index)

###constructing the forest plot
as_tibble(staffvres2)


plot3 <- ggplot(staffvres2, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:3, labels = staffvres2$exposure, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab("Exposure") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot3 + labs(title = "OR for Staff Infections vs Resident Infections \n Stratified by 75% Staff Vaccination Rate", ylab ="Exposure")

g <- grid.arrange(plot1, plot2, ncol = 2)
