library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(forcats)
library(grid)
library(gridExtra)

numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

df_perf <- read.csv('data/clf_perf.csv')
df_labs <- read.csv('data/labeled_and_preds.csv')
df_marked <- read.csv('data/rf_pred.csv')
nimh_papers <- read.csv('data/nimh_papers.csv')
nimh_proj <- read.csv('data/nimh_projects.csv')

length(unique(df_marked$pmcid))/dim(nimh_papers)[1]

#link tables for years 2004-2018 downloaded from FederalReporter (https://federalreporter.nih.gov/FileDownload)
links <- list.files('data/RePORTER/Link_tables/', full.names = T)
df_links <- do.call(rbind, lapply(links, fread))

df_labs %>%
  filter(data_statement==1) %>%
  select(pmcid) %>%
  distinct() %>%
  summarise(n_papes = n())

df_perf %>%
  select(clf, f1, prec, rec) %>%
  gather(metric, value, f1:rec) %>%
  mutate(metric = fct_recode(metric, 'F1 score' = 'f1',
                             'Precision' = 'prec', 
                             'Recall' = 'rec'),
         clf = fct_recode(clf, 'Easy Ensemble' = 'ez_ens',
                          'Two Stage' = 'rf')) %>%
  distinct() %>%
  mutate(text = paste(metric, ' = ', numformat(value))) -> text_labs

text_labs$x_pos <- .75
text_labs$y_pos <- c(.75, .75, .68, .68, .61, .61)

df_perf %>%
  mutate(fold = factor(fold)) %>%
  mutate(clf = fct_recode(clf, 'Easy Ensemble' = 'ez_ens',
                          'Two Stage' = 'rf')) %>%
  filter(fold==1)-> plot_dat
  

ggplot(plot_dat, aes(x=fpr, y=tpr)) + 
  geom_line(size=1.5) + 
  facet_wrap(~clf, ncol=1) +
  theme_classic() +
  geom_text(data=text_labs, aes(x=x_pos, y=y_pos, label=text), color='black', size=6) +
  ylab('true positive rate') +
  xlab('false positive rate') +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face='bold'),
        strip.text = element_text(size=16),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16, face='bold'),
        legend.position='top')-> p1
  
df_labs %>% 
  mutate(pmcid_offset = paste(pmcid, paper_offset, sep='-')) %>%
  select(data_statement, pmcid_offset, pred_RF, pred_ez_ens) %>%
  gather(clf, prediction, pred_RF, pred_ez_ens) %>%
  mutate(clf = fct_recode(clf, 'Easy Ensemble' = 'pred_ez_ens',
                          'Two Stage' = 'pred_RF')) -> plot_dat

ggplot(plot_dat, aes(x=data_statement, y=1, color=factor(prediction))) + 
  geom_point(position=position_jitter(height=1)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size=14),
        strip.text = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position='top') +
  scale_color_viridis(option='B', discrete=T, begin = .1, end = .7, 
                      labels=c('no predicted\ndata statement',
                               'predicted\ndata statement')) +
  scale_x_continuous(breaks=c(0, 1),
                   labels=c("sentences w/o \n data statements", 
                            "sentences w/ \n data statements")) +
  
  ylab('') + 
  xlab('') +
  facet_wrap(~clf, ncol=1) -> p2

fig <- arrangeGrob(p1, p2, nrow=1, 
                   top = textGrob("Cross-validation\nClassifier Performance",
                                  gp=gpar(fontsize=20)))

ggsave(file="figures/fig1.jpeg", fig, width = 9, height=9)


nimh_proj %>% 
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '\\s?\\([0-9]+\\)')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '-.+')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '^[0-9]')) %>%
  group_by(PROJECT_NUMBER) %>%
  filter(FY == max(FY, na.rm=T)) %>%
  filter(is.na(FY_TOTAL_COST_SUB_PROJECTS)) %>%
  filter(FY_TOTAL_COST==max(FY_TOTAL_COST)) %>%
  ungroup() %>%
  select(PROJECT_ID, PROJECT_TERMS, PROJECT_TITLE, DEPARTMENT, IC_CENTER, 
         CONTACT_PI_PROJECT_LEADER, AGENCY, PROJECT_NUMBER, PROJECT_START_DATE, 
         PROJECT_END_DATE, ORGANIZATION_NAME) -> unique_grants

nimh_papers$data_statement <- nimh_papers$pmcid %in% df_marked$pmcid

df_links %>%
  filter(PMID %in% nimh_papers$PMID) %>%
  filter(PROJECT_NUMBER %in% unique_grants$PROJECT_NUMBER) %>%
  left_join(nimh_papers) %>%
  left_join(unique_grants) -> project_paper_pairs

project_paper_pairs %>%
  select(ORGANIZATION_NAME, data_statement, PMID, pmcid) %>%
  distinct() %>%
  group_by(ORGANIZATION_NAME) %>%
  summarise(data_statement_count = sum(data_statement, na.rm=T),
            total_papes = n()) %>%
  mutate(data_statement_prop = data_statement_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_statement_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(data_statement_count),
         cumulative_perc = cumsum(data_statement_count)/sum(data_statement_count),
         cumulative_count = cumsum(data_statement_count),
         institution_number = row_number()) %>%
  ggplot(aes(x=institution_number, y=data_statement_prop)) +
  geom_point() +
  xlab('Sorted Institution (min. 3 pubs)') +
  ylab('Predicted Prop. of papers w/data statements') +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face='bold')) -> p1

project_paper_pairs %>%
  select(CONTACT_PI_PROJECT_LEADER, data_statement, PMID, pmcid) %>%
  distinct() %>%
  group_by(CONTACT_PI_PROJECT_LEADER) %>%
  summarise(data_statement_count = sum(data_statement, na.rm=T),
            total_papes = n()) %>%
  mutate(data_statement_prop = data_statement_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_statement_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(data_statement_count),
         cumulative_perc = cumsum(data_statement_count)/sum(data_statement_count),
         cumulative_count = cumsum(data_statement_count),
         PI_number = row_number()) %>%
  ggplot(aes(x=PI_number, y=data_statement_prop)) +
  geom_point() +
  xlab('Sorted PI (min. 3 pubs)') +
  ylab('Predicted Prop. of papers w/data statement') +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face='bold')) -> p2

fig <- arrangeGrob(p1, p2, nrow=1, 
                   top = textGrob("We extrapolate that most data statements come from a small\nfraction of NIMH-funded investigators and institutions",
                                  gp=gpar(fontsize=20)))

ggsave(file="figures/fig2.jpeg", fig, width = 9, height=9)

project_paper_pairs %>%
  select(CONTACT_PI_PROJECT_LEADER, data_statement, PMID, pmcid) %>%
  distinct() %>%
  group_by(CONTACT_PI_PROJECT_LEADER) %>%
  summarise(data_statement_count = sum(data_statement, na.rm=T),
            total_papes = n()) %>%
  mutate(data_statement_prop = data_statement_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_statement_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(data_statement_count),
         cumulative_perc = cumsum(data_statement_count)/sum(data_statement_count),
         cumulative_count = cumsum(data_statement_count),
         PI_number = row_number()) -> dist

dist %>% filter(data_statement_prop>.33)

project_paper_pairs %>% 
  filter(journal_year>2007) %>%
  filter(journal_year<2018) %>%
  group_by(journal_year) %>% 
  summarise(yrly_papes = n(), data_states = sum(data_statement)) %>% 
  mutate(prop = data_states/yrly_papes,
         journal_year = as.character(journal_year)) %>%
  ggplot(aes(x=journal_year, y=prop, group=1)) + 
  geom_line() +
  xlab('Publication Year') +
  ylab('Predicted Prop. of\npapers w/data statement') +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face='bold')) -> p1

fig <- arrangeGrob(p1, top=textGrob('We extrapolate that data statements are\nincreasing overtime, but still infrequent',
                                    gp=gpar(fontsize=20)))
ggsave(file="figures/fig3.jpeg", 
       fig, width = 10, height=4)


df_labs %>% 
  filter(data_statement==0) %>%
  filter(pred_RF==1) -> false_alarms
  
df_labs %>%
  filter(data_statement==1) %>%
  filter(pred_RF==0) -> misses
  
