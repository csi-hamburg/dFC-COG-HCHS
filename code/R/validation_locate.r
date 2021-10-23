d.validation <- read.csv('./../../Input/validation_data.csv', header = TRUE) %>% 
  set_names(c("ID","minsize","SI","FDR","FNR","FDR_clusters","FNR_clusters","DER","OER","MTA","LESION_vol","MANUAL_vol","mhdmean", "mhdmedian", "mhdp95")) %>% 
  mutate(across(3:12, as.numeric)) %>% 
  as_tibble() %>% 
  filter(!is.na(SI))

d.validation 

d.validation %>% 
  filter(minsize == 30) %>% 
  ggplot(aes(x = MANUAL_vol/1000, y = LESION_vol/1000, fill = as.factor(minsize), color = as.factor(minsize))) +
  geom_smooth(method = 'lm', se = TRUE, size=.5) +
  geom_segment(data=NULL, inherit.aes = FALSE, aes(x=0.01, xend=25, y=0.01, yend=25), color= 'black') +
  geom_point(shape = 21, color = 'black', stroke = .25, alpha = .5) +
  scale_y_log10('BIANCA/LOCATE segmentation (ml)') +
  scale_x_log10('Manual annotation (ml)') +
  guides(color = FALSE, fill=FALSE) +
  theme_minimal()+
  theme(axis.text = element_text(size = 8)
        , axis.title = element_text(size = 8))

ggsave('./derivatives/figures/volvol.tiff', dpi = 600, width = 8, height = 8, units = 'cm')

d.validation %>% 
  filter(minsize == 30 & is.finite(MANUAL_vol)) %$% 
  lm(scale(log(MANUAL_vol/1000)) ~ scale(log(LESION_vol/1000)), data = .) %>% 
  tidy(conf.int = TRUE)

d.validation %>% 
  pivot_longer(cols = 3:15, names_to = 'meas', values_to = 'val') %>% 
  filter((meas %in% c("FDR", "FDR_clusters", "FNR", "FNR_clusters", "mhdmean", "SI"))) %>% 
  ggplot(aes(y = val, x = as.factor(minsize), color = minsize==30)) +
  #geom_boxplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", size = .5) +
  stat_summary(fun.data = mean_se, geom = "point", size = .75, shape = 21, fill = 'white') +
  facet_wrap(~meas, scales = 'free_y', ncol = 3, labeller = labeller(meas = c(FDR='FDR (voxels)'
                                                                              , FDR_clusters = 'FDR (clusters)'
                                                                              , FNR='FNR (voxels)'
                                                                              , FNR_clusters = 'FNR (clusters)'
                                                                              , SI = 'Dice index'
                                                                              , mhdmean = 'mod. Hausdorff distance (mm)'))) +
  scale_y_continuous('', labels = scales::number_format()) +
  scale_x_discrete('Minimum cluster size of lesioned voxels', guide = guide_axis(n.dodge = 2)) +
  scale_color_manual('', values = c('black', 'darkred')) +
  guides(color = FALSE) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))
ggsave('./derivatives/figures/BIANCAoverlap.tiff', dpi = 600, width = 18, height = 8, units = 'cm')
