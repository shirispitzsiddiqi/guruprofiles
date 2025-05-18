#### Shiri Spitz Siddiqi ####
#### May 18, 2025 ####


# Libraries----
library(ggplot2)
library(tidyverse)
library(tidyLPA)
library(ggradar)
library(flextable)
library(rstatix)


# Read in the data----
df <- read.csv("C:/Users/shiri/Downloads/gurometer_scores.csv") %>% # Change path according to your machine
  # Clean up the "flag" type data to all 0s and 1s
  mutate(across(Monomania:Never.admitting.error, \(x) as.numeric(x))) %>% 
  mutate(across(Monomania:Never.admitting.error, \(x) case_when(x != 1 ~ NA,
                                                                T ~ x))) %>% 
  mutate(across(Monomania:Never.admitting.error, \(x) replace_na(x, 0))) %>% 
  type_convert()


# Check for any gurus who only have 1 rating - could indicate a misspelling or different spelling
table(df$Guru) %>% sort(decreasing=T)


# Clean up names
df <- df %>% 
  mutate(Guru = case_when(Guru == "Sabine" ~ "Sabine Hossenfelder",
                          Guru == "MIkhaila Peterson" ~ "Mikhaila Peterson",
                          Guru == "Anna" ~ "Anna Khachiyan",
                          Guru == "Dasha" ~ "Dasha Nekrasova",
                          Guru == "Zizek" ~ "Slavoj Zizek",
                          T ~ Guru)) %>% 
  rename(Self.Aggrandisement.and.Narcissism = Self.Aggrandisement.and.Narcicissm)

# All now have 2 or more
table(df$Guru) %>% sort(decreasing=T)



# LPA----

df_avg <- df %>% 
  # I realized after publishing the figure originally that the results I shared included ratings from RK. The results shift slightly when including only Chris and Matt (a few gurus get categorized differently). Comment out the following line to exclude RK's ratings and reorder the Class factor levels to 2, 1, 4, 3 to reproduce what I shared in the chat.
  filter(Coder %in% c("CK", "MB")) %>% 
  group_by(Guru) %>% 
  select(Guru, Galaxy.Brainness:Moral.Grandstanding) %>% 
  get_summary_stats() %>% 
  pivot_wider(id_cols=c(Guru), names_from=variable, values_from=mean)


# Check for the optimal number of profiles
df_avg %>% select(Galaxy.Brainness:Moral.Grandstanding) %>% 
  single_imputation() %>% 
  estimate_profiles(1:6) %>% 
  compare_solutions()


## Plots----

##### Line plot of the profiles
df_avg %>% select(Galaxy.Brainness:Moral.Grandstanding) %>% 
  single_imputation() %>% 
  estimate_profiles(4) %>% 
  get_estimates() %>% 
  mutate(Class = factor(Class, levels=c(1, 3, 4, 2), 
                        labels=c("Public Intellectuals", "Big Brains", "Critics", "Messiahs"))) %>%
  mutate(Parameter = factor(Parameter, levels=c("Galaxy.Brainness", "Revolutionary.Theories", "Pseudo.Profound.Bullshit", 
                                                "Cultishness", "Profiteering", "Grievance.Mongering", "Conspiracy.Mongering", "Cassandra.Complex", 
                                                "Self.Aggrandisement.and.Narcissism",  "Moral.Grandstanding", "Anti.Establishment"),
                            labels=c("Galaxy Brainness", "Revolutionary Theories", "Pseudo Profound Bullshit",
                                     "Cultishness", "Profiteering", "Grievance Mongering", "Conspiracy Mongering", "Cassandra Complex", 
                                     "Self Aggrandisement and Narcissism",  "Moral Grandstanding",  "Anti Establishment") %>% str_wrap(width=15))) %>% 
  filter(Category=="Means") %>% 
  ggplot(aes(x=Parameter, y=Estimate, group=Class, color=Class, shape=Class)) +
  geom_point(size=3) + 
  geom_line(linewidth=1) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="bottom") +
  labs(x="Dimension", y="Predicted level", color=NULL, shape=NULL) +
  coord_cartesian(ylim=c(1,5)) +
  scale_y_continuous(breaks=seq(1,5,1)) +
  scale_color_manual(values=c("coral", "seagreen", "midnightblue", "goldenrod"))





##### Create a spider plot of the profiles using ggradar
factorgroups <- data.frame(Parameter = c("Galaxy.Brainness", "Revolutionary.Theories", "Pseudo.Profound.Bullshit", "Anti.Establishment", 
                                         "Cultishness", "Profiteering", "Grievance.Mongering", "Conspiracy.Mongering", "Cassandra.Complex", 
                                         "Self.Aggrandisement.and.Narcissism",  "Moral.Grandstanding"),
                           num = seq(1:11))
df_avg %>% select(Galaxy.Brainness:Moral.Grandstanding) %>% 
  single_imputation() %>% 
  estimate_profiles(4) %>% 
  get_estimates() %>% 
  mutate(Class = factor(Class)) %>% 
  filter(Category=="Means") %>% 
  select(Parameter, Estimate, Class) %>% 
  left_join(factorgroups) %>% 
  arrange(num) %>% 
  mutate(Parameter = gsub("\\.", " ", Parameter) %>% str_wrap(width=10)) %>% 
  pivot_wider(id_cols=Class, names_from=Parameter, values_from=Estimate) %>% 
  mutate(Class = factor(Class, levels=c(1, 3, 4, 2),
                        labels=c("Public Intellectuals", "Big Brains", "Critics", "Messiahs"))) %>% 
  ggradar(grid.min=1, grid.mid=3, grid.max=5, centre.y=0, group.colours=c("coral", "seagreen", "midnightblue", "goldenrod"),
          legend.position="bottom", axis.label.size=3, legend.text.size=10, fill=T, fill.alpha=.15,
          label.gridline.min=F, label.gridline.mid=F, label.gridline.max=F) 
#ggsave("C:/Users/shiri/Downloads/guruplot.png", width=10, height=6) # Change path according to your machine to save out the plot




## See how each guru got categorized----
lpa_res <- df_avg %>% select(Galaxy.Brainness:Moral.Grandstanding) %>% 
  single_imputation() %>% 
  estimate_profiles(4) %>% 
  get_data() %>% 
  mutate(Guru = df_avg$Guru) %>% 
  mutate(Class = factor(Class, levels=c(1, 3, 4, 2),
                        labels=c("Public Intellectuals", "Big Brains", "Critics", "Messiahs"))) 


# How many gurus are in each category?
lpa_res %>% 
  count(Class)


# Save out the results (uncomment first)
# lpa_res %>% 
#   arrange(Class) %>% 
#   select(Class, Guru) %>% 
#   write.csv("C:/Users/shiri/Downloads/gurucategories.csv")


# Create a nice table of the results viewable within R Studio
lpa_res %>% 
  arrange(Class) %>% 
  select(Class, Guru) %>% 
  flextable() %>% 
  autofit() %>% 
  theme_zebra()

