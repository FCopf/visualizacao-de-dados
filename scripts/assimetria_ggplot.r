assimetria_ggplot = function(){
  set.seed(2)
  df = data.frame(x1 = rlnorm(n = 1000, mean = 2, sd = 0.4),
                  x2 = rnorm(n = 1000),
                  x3 = rbeta(n = 1000, shape1 = 30, shape2 = 3))
  
  h1 = ggplot(df, aes(x = x1, y = ..count../sum(..count..)))
  h2 = ggplot(df, aes(x = x2, y = ..count../sum(..count..)))
  h3 = ggplot(df, aes(x = x3, y = ..count../sum(..count..)))
  slab = 3
  p1 = h1 + 
    geom_histogram(bins = 15, 
                   fill = 'darkblue', 
                   color = 'white') +
    annotate(geom = 'segment', 
             x = c(mean(range(df$x1)),
                   mean(df$x1),
                   median(df$x1),
                   getmode(df$x1) + 1),
             xend = c(mean(range(df$x1)),
                      mean(df$x1),
                      median(df$x1),
                      getmode(df$x1) + 1),
             y = c(0.22, 0.24, 0.26, 0.28),
             yend = c(0.06, 0.21, 0.22, 0.23),
             color = 'darkred', size = 1.2, 
             arrow=arrow(length = unit(0.2, "inches"))) +
    annotate(geom = 'label', 
             x = c(mean(range(df$x1)),
                   mean(df$x1),
                   median(df$x1),
                   getmode(df$x1)), 
             y = c(0.23, 0.25, 0.27, 0.29), 
             label = c('Ponto médio',
                       'Média',
                       'Mediana',
                       'Moda'), 
             size = slab, hjust = 0) +
    theme_void() + 
    theme(axis.line.x = element_line(),
          axis.line.y = element_line())
  
  p2 = h2 + 
    geom_histogram(bins = 15, 
                   fill = 'darkblue', 
                   color = 'white') +
    annotate(geom = 'segment', 
             x = median(df$x2),
             xend = median(df$x2),
             y = 0.26,
             yend = 0.22,
             color = 'darkred', size = 1.2, 
             arrow=arrow(length = unit(0.2, "inches"))) +
    annotate(geom = 'label', 
             x = median(df$x2), 
             y = 0.27, 
             label = c('Ponto médio = Média = Mediana = Moda'), 
             size = slab) +
    theme_void() + 
    theme(axis.line.x = element_line(),
          axis.line.y = element_line())
  
  p3 = h3 + 
    geom_histogram(bins = 15, 
                   fill = 'darkblue', 
                   color = 'white') +
    annotate(geom = 'segment', 
             x = c(mean(range(df$x3)),
                   mean(df$x3),
                   median(df$x3),
                   getmode(df$x3)-0.05),
             xend = c(mean(range(df$x3)),
                      mean(df$x3),
                      median(df$x3),
                      getmode(df$x3)-0.05),
             y = c(0.22, 0.24, 0.26, 0.28),
             yend = c(0.06, 0.21, 0.22, 0.215),
             color = 'darkred', size = 1.2, 
             arrow=arrow(length = unit(0.2, "inches"))) +
    annotate(geom = 'label', 
             x = c(mean(range(df$x3)),
                   mean(df$x3),
                   median(df$x3),
                   getmode(df$x3)-0.05), 
             y = c(0.23, 0.25, 0.27, 0.29), 
             label = c('Ponto médio',
                       'Média',
                       'Mediana',
                       'Moda'), 
             size = slab, hjust = c(0.2,1,0.4,0.3)) +
    theme_void() + 
    theme(axis.line.x = element_line(),
          axis.line.y = element_line())
  
  p1 + p2 + p3
}
