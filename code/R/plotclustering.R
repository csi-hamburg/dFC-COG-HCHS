design <- '36p'

d.clustering <- read.csv(paste0('./../../derivatives/data/kmeansdata_', design, '.dat'), header = FALSE) %>% 
  setNames(c('k', 'rep', 'var', 'incomplete')) %>% 
  group_by(k)

p.bar <- d.clustering %>% 
  filter(rep == which.min(var)) %>% 
  mutate(facet = 'dummy') %>% 
  ggplot(aes(x = k, y = incomplete)) +
  geom_col() +
  #geom_errorbar(aes(ymin =  mean_incomplete - sd_incomplete, ymax =  mean_incomplete + sd_incomplete), width = .5) + 
  facet_wrap(~facet, labeller = labeller(facet = c(dummy = '# subjects not visiting all states'))
             , strip.position = 'left') +
  scale_y_continuous('', expand = c(0,0)) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal(base_size = 10) +
  theme(strip.placement = 'outside'
        , strip.text = element_text(size = 6)
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))
p.bar

d0 <- 1.283120133375530e+05;

p.var <- d.clustering %>% 
  filter(rep == which.min(var)) %>% 
  ungroup() %>% 
  mutate(var = 1 - var/d0
         , diffvar = var - lag(var)) %>% 
  pivot_longer(cols = ends_with('var'), names_to = 'key', values_to = 'value') %>% 
  mutate(key = factor(key, levels = c('var', 'diffvar'), ordered = TRUE)) %>% 
  filter(k>1) %>% 
  ggplot(aes(x = k, y = value, color = k==5)) +
  geom_line(color = 'black') +
  geom_point(aes(fill = k==5), shape = 21) +
  facet_wrap(~ key, scales = 'free_y', ncol = 2
             , labeller = labeller(key = c(var = 'Variance explained', diffvar = 'Incremental variance'))
             , strip.position = 'left') +
  scale_y_continuous('', labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c('black', 'darkred')) +
  scale_fill_manual(values = c('white', 'red')) +
  guides(color = 'none', fill = 'none') +
  theme_minimal(base_size = 10) +
  theme(strip.placement = 'outside'
        , strip.text = element_text(size = 6)
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))


require(patchwork)
p.var + p.bar + plot_layout(widths = c(3,2))


d.states <- read.table(paste0('./../../derivatives/data/stateidxdata_', design, '.dat'), header = FALSE, sep = ',') 
colnames(d.states) <- 1:ncol(d.states)
rownames(d.states) <- 1:nrow(d.states)

d.states %<>% 
  t() %>% as.table() %>% as.data.frame() %>% as_tibble() %>% 
  setNames(c('TP', 'ID', 'IDX')) %>% 
  mutate(TP = as.numeric(TP), ID = as.numeric(ID))

p.TR <- d.states %>% 
  filter(ID %in% unique(ID)[1:10] & TP < 50) %>% 
  mutate(IDX = recode(IDX, !!!states) %>% factor(levels = c('DMN+','DMN-', 'VIS+', 'VIS-','FPCN-'), ordered = TRUE)) %>% 
  ggplot(aes(x = TP, y = ID, fill = as.factor(IDX))) +
  geom_tile(color = NA) +
  facet_wrap(~ID, ncol = 1, strip.position = 'left', scales = 'free_y') + 
  scale_x_continuous('Time (TR=3s)', breaks = scales::pretty_breaks(5), expand = c(0,0)) +
  scale_y_reverse('Subject', breaks = 1:10, expand = c(0,0)) +
  scale_fill_nejm() +
  #guides(fill = FALSE) + #guide_legend(title = 'Brain state')
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(hjust = .5)
        , legend.position = NULL
        , strip.text = element_text(size = 6)
        , axis.title = element_text(size = 6, margin = margin(-10,-10,-10,-10, unit = 'cm'))
        , axis.text = element_text(size = 6)
        , panel.grid.major.y = element_blank()
        , strip.placement = 'outside'
        , strip.background = element_blank()
        , strip.text.y = element_blank())

p.TR


require(patchwork)
layout <- "
AAB
CCC
"
p.var + p.bar + p.TR  +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A', tag_suffix = ')') & theme(plot.tag = element_text(size = 8))

ggsave(paste0('./../../derivatives/figures/clustering_', design, '.png'), dpi = 600, width = 18, height = 9, units = 'cm')


require(ggradar)
spiderdata <- read.csv(paste0('./../../derivatives/data/spiderdatall_', design, '.dat'), header = FALSE) %>% 
  set_names(c('state', 'network', 'sign', 'value'))


p.spider <- spiderdata %>% 
  mutate(state = recode(state, !!!states) %>% factor(levels = c('DMN+','DMN-', 'VIS+', 'VIS-','FPCN-'), ordered = TRUE)) %>% 
  arrange(state) %>% 
  group_by(state) %>% nest() %>%  
  mutate(plt = map(data, function(d){d %>% pivot_wider(names_from = network, values_from = value) %>% 
      ggradar(base.size = 6
              , axis.labels = c('FPCN','DMN','DAT','LIM','SAL','SMN','Vis')
              , group.point.size = 1, group.line.width = .25
              , axis.label.size = 2, grid.label.size = 2
              , gridline.mid.colour = 'grey'
              , group.colours = c('-1'='darkgreen', '1'='darkred')
              , gridline.max.linetype	= 'solid', values.radar = c("", "50%", "100%")
              , plot.title = state, legend.text.size = 2) + 
      guides(color = 'none') +
      theme(plot.tag = element_text(size = 8)
            , title = element_text(size = 8)
            , legend.title = element_text(size = 8))}
  )) %>% 
  pull('plt') %$% 
  do.call(patchwork::wrap_plots, c(., ncol = 5))


p.spider
ggsave(paste0('./../../derivatives/figures/spider_', design, '.png'), dpi = 600, width = 18, height = 5, units = 'cm')
