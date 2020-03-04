
library(shiny)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(formattable)

# Import the data I pre-processed
demographics<- read.csv("../tidy_demographics.csv", stringsAsFactors = F)

recent<- filter(demographics, Year == 2018) 

# plot frequency for all schools in the most recent year
df<- recent %>%
  group_by(students_faculty) %>%
  mutate(stu_fac_total=sum(count)) %>%
  ungroup() %>%
  group_by(students_faculty, Ethnicity, stu_fac_total) %>%
  summarize(stu_fac_eth_total = sum(count)) %>%
  ungroup() %>%
  mutate(proportion = stu_fac_eth_total/stu_fac_total) %>%
  select(students_faculty, Ethnicity, proportion) %>%
  pivot_wider(names_from = students_faculty, values_from = proportion) %>%
  mutate(difference = round((Faculty - Students)*100,2)) %>%
  mutate(ethnicity_fct = reorder(as.factor(Ethnicity), difference)) %>%
  filter(ethnicity_fct != 'Two or more' &
           ethnicity_fct != 'Unknown')

### Attempt a modified lollipop chart:
  # https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values.html
p<- ggplot(df) + 
  geom_segment(aes(x=ethnicity_fct, xend=ethnicity_fct, 
                   y=Students,yend=Faculty),
               color = "grey", size = 3) + 
  # Students are Green-ish
  geom_point(aes(x=ethnicity_fct, y = Students), color="#3CAEA3", size=5 ) +
  # Faculty are Yellow
  geom_point(aes(x=ethnicity_fct, y = Faculty), color="#F6D55C", size=5 ) +
  scale_fill_manual(values = c("#3CAEA3", "#F6D55C")) +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.y = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  xlab("") +
  ylab("") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1)) +
  labs(caption = "Percent of Students and Faculty of each ethnicity.")
p

#ggsave("total.png", p, device = "png")


df %>%
  select(-ethnicity_fct) %>%
  mutate(Faculty = round(Faculty,4)*100,
         Students = round(Students,4)*100) %>%
  arrange(desc(difference)) %>%
  formattable()
## Do more customizing?? https://github.com/lgellis/MiscTutorial/blob/master/Austin/formattable.R



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
output$eth_plot <- renderPlot({
  p
})
  
output$eth_table <- renderFormattable({
  df %>%
    select(-ethnicity_fct) %>%
    mutate(Faculty = round(Faculty,4)*100,
           Students = round(Students,4)*100) %>%
    arrange(desc(difference)) %>%
    formattable()
  })

  
})
