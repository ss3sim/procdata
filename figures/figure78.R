myylim <- c(-.6,.6)
xx$estimated2 <- xx$estimated
levels(xx$estimated2) <- c('M0&h0', 'h1', 'M1')
g <- ggplot(xx, aes(x=estimated2, y=SSB_MSY_re)) +
    geom_hline(yintercept=0, col='black')  + ylab(expression(Relative~Error:~SSB[MSY]))+
      geom_violin(fill=gray(.8))+ facet_grid(weighted~om.process+em.process) +
        theme_bw() + coord_cartesian(ylim=myylim) +
          theme(legend.position='none',
                axis.text=element_text(size=8, color=col.label),
                panel.margin.x = unit(c(0), "lines")) + xlab(NULL)
ggsave('figure7_SSB_MSY.png', g, width=width, height=5)

yy$estimated2 <- yy$estimated
levels(yy$estimated2) <- c('M0&h0', 'h1', 'M1')
g <- ggplot(yy, aes(x=estimated2, y=SpawnBio_re)) +
    geom_hline(yintercept=0, col='black')  + ylab(expression(Relative~Error:SSB))+
      geom_violin(fill=gray(.8))+ facet_grid(weighted~om.process+em.process) +
        theme_bw() +  coord_cartesian(ylim=myylim) +
          theme(legend.position='none',
                axis.text=element_text(size=8, color=col.label),
                panel.margin.x = unit(c(0), "lines")) + xlab(NULL)
ggsave('figure8_SSB.png', g, width=width, height=5)
