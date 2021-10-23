source('loadpackages.r')

design <- '36p'


states <- c('1'='DMN-', '2'='VIS+', '3'='DMN+', '4'='FPCN-', '5'='VIS-')
preds <- c('WMHsmooth')
preds.all <- c('WMHsmooth','psmd')

d.struct <- read.csv('./../../Input/structural_params.csv', header = TRUE) %>% 
  mutate(across(starts_with('WMH'), ~./1000))  %>% 
  dplyr::select(c(ID,all_of(preds)))


dd <- read.csv(paste0('./../../derivatives/data/statedata_', design, '.dat'), header = TRUE, sep = ',') %>% 
  mutate(ID = stringr::str_sub(ID, 5, 13)) %>% 
  merge(d.struct) %>% 
  pivot_longer(cols = starts_with(c('fracocc','dwell'))) %>% 
  separate(name , '_', into = c('meas', 'state')) %>% 
  mutate(state = recode(state, !!!states) %>% factor(levels = states[c(2,4,5,1,3)])
         , meas = factor(meas, levels = c('fracocc', 'dwell'), ordered = TRUE)) %>% 
  arrange(state) %>% 
  dplyr::filter(WMHsmooth > 0)


clindata <- read.csv('./../../Input/data.csv', header = TRUE)  %>% 
  dplyr::select(ID = DisclosureID
                , age, sex
                , TMTA, TMTB, MMST, Recall_Sumwords, Wortschatz, animaltest
                , riskgroup_yn
                , educationyears
                , BMI, smoking_yn, diabetes, hypertension
                , psmd) %>%
  mutate(across(c(ID, sex, smoking_yn, diabetes, hypertension), as.factor)) %>% 
  filter(ID %in% dd$ID) %>% 
  pivot_longer(cols = TMTA:animaltest, names_to = 'cogmeas', values_to = 'cogvalue') %>% 
  mutate(cogmeas = factor(cogmeas, levels = c('MMST','animaltest','Recall_Sumwords','Wortschatz','TMTA','TMTB'), ordered = TRUE))

dd <- dd %>% filter(ID %in% clindata$ID)

d.struct <- d.struct %>% right_join(clindata %>% dplyr::select(ID, psmd) %>% unique()) 



dd.unique <- dd %>% filter(meas == unique(meas)[1] & state == unique(dd$state)[1]) 
clindata.unique <- clindata %>% filter(cogmeas == unique(cogmeas)[1]) 

df.mean <- read.csv(paste0('./../../derivatives/data/meandynamics', design, '.dat')
                    , header = TRUE, sep = ',') %>% 
  right_join(clindata.unique) %>% 
  merge(d.struct)

states.TP <- c( '1'='FPCN-', '2'='VIS-', '3'='VIS+','4'='DMN+', '5'='DMN-')

TP.data <- read.csv(paste0('./../../derivatives/data/transitionprobsdata_', design, '.dat'), header = TRUE) %>% 
  mutate(state_in = recode(state_in, !!!states.TP) %>% factor(levels = c('DMN+','DMN-', 'VIS+', 'VIS-','FPCN-'), ordered = TRUE)
         , state_out = recode(state_out, !!!states.TP) %>% factor(levels = c('DMN+','DMN-', 'VIS+', 'VIS-','FPCN-'), ordered = TRUE)) %>% 
  arrange(state_in, state_out) %>% 
  filter(ID %in% d.struct$ID)

d.struct$ID <- as.numeric(factor(d.struct$ID))
clindata$ID <- as.numeric(factor(clindata$ID))
clindata.unique$ID <- as.numeric(factor(clindata.unique$ID))
dd$ID <- as.numeric(factor(dd$ID))
dd.unique$ID <- as.numeric(factor(dd.unique$ID))
df.mean$ID <- as.numeric(factor(df.mean$ID))
TP.data$ID <- as.numeric(factor(TP.data$ID))

scientific_10 <- function(x) {
  if(is.na(x)){return(NA)}
  if(abs(log(x,10))<=0){return(parse(text = x))}
  parse(text=substr(gsub("e", " %*% 10^", scales::scientific_format()(x)),7,20))
}
scientific_10_vec <- Vectorize(scientific_10)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

save.image('prepdata.RData')
