#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);  library(tidyverse); library(ggplot2); library(shinythemes); library(patchwork); library(DT)

site_dat <- read.csv("Data/Table_S1.csv")  

load('Data/rich.mod.dat.Rdata')
load('Data/bm.mod.dat.Rdata')
load('Data/sgain_dat.Rdata')
load('Data/sloss.n.mod.dat.Rdata')
load('Data/sl.n.mod.dat.Rdata')
load('Data/sg_dat.Rdata')
load('Data/cde.mod.dat.Rdata')
load('Data/study.p.effs.Rdata')
load('Data/p.effs.Rdata')
load('Data/study.price.p.effs.Rdata')
load('Data/study.p.all.Rdata')


sitenames <- as.character(unique(site_dat$site_code))


# Define UI for application that draws a histogram
ui <- fixedPage(theme = shinytheme("readable"),

    fixedRow(
        column(12,
               h1("Nutrient Network Site-Level Responses to NPK Treatments"),
               br(),
               selectInput("selected_site", "Chosen Site", 
                           sitenames, selected = "ahth.is", multiple = FALSE,
                           selectize = TRUE, width = NULL, size = NULL),
               h3(textOutput("sitename") ),
               
        fixedRow(
               column(12,
                      br(), br(),
                      h2("Site-Level Effect Estimates"),
                      br(),
                      "Models are fitted linearly in a Bayesian hierarchical framework fit with a nested random effects structure of site, block and plot.",
                      br(), br(),
                      "Each set of plots corresponds respectively to Figure 2, Figure 3, and Figure 4 in the main text, but visualises only the site-level estimated effects here.",
                      br(), br(),
                      "Figure captions for each set of plots are written above each figure."
               ),
               column(12,
                      br(),
                      h2("Figure 2: Species Richness & Plot Biomass"),
                      br(),
                      "In regressions represented in A) and B), each green point represents a plot treated with NPK and each grey open circle represents a control plot. Each green thin line represents the estimated NPK slope of every experimental site as a random effect and each black dashed line represents the estimated slope of control plots across time. The inset plots represent the site-level slope of Control (black) and NPK (colored) treatments, error bars represent 95% credible intervals, calculated from site-level posterior distributions, and the dashed reference line at 0 represents a slope of 0 .",
                      br(), br(),
                      "In C), each green point represents the slope for richness (x-axis) and biomass (y-axis) of an experimental site (n=58) treated with NPK, and error bars represent the 95% credible intervals for each site. The dashed reference line at 0 represents a slope of 0 for each metric.",
                      br(), br()),
               fixedRow(column(12, align = "center",
                               plotOutput('richbmviz', height = 1000, width = 800),
                               br(), br(),
               )
               ),
               column(12,
                      br(),
                      h2("Figure 3: Species Loss, Species Gains, Persistent Species and the Effect of Each on Biomass Change Across Time"),
                      br(),
                      "In regressions each colored solid point represents a pairwise comparison of a single plot before NPK nutrient addition (year 0) and for each year after treatment. Each grey open circle represents a pairwise comparison of a control plot at year 0 for each year measured after. Each colored thin solid line represents the estimated slope of NPK, and every black dashed line represents the estimate slope of control plots for every experimental site (n=58) as a random effect.  The inset plots represent the site-level slope estimate of Control (black) and NPK (colored) treatments, error bars represent 95% credible intervals, calculated from site-level posterior distributions, and the dashed reference line at 0 represents a slope of 0 for each metric.",
                      br(), br()),
               fixedRow(column(12, align = "center",
                               plotOutput('slosssgainviz', height = 500, width = 800),
                               br(), br(),
               )
               ),
               fixedRow(column(12, align = "center",
                               plotOutput('priceviz', height = 500, width = 1100),
                               br(), br(),
               )
               ),
               column(12,
                      br(),
                      h2("Figure 4: Consider All Effects Together"),
                      br(),
                      "The dashed reference line at 0 represents a slope of 0 for each metric. The lines show the site-level effect estimate of each response, added together to consider all components of change on communities for NPK treatments. Both y and x-axes are not fixed, and are allowed to vary between site visualisations for clarity.  ",
                      br(), br()),
               fixedRow(column(12, align = "center",
                               plotOutput('pricevectorviz', height = 500, width = 500),
                               br(), br(),
               )
               ),
               column(12,
                      br(), br(),
                      h2("Site-Level Effect Sizes"),
                      "Site-level effect estimates and 95% credible intervals (CI’s) calculated from study level posterior distributions for each Treatment and Model. These effect estimates correspond to visualisations for each figure above. The estimates reported here can be thought of as a rate of change over time in terms of change/year for every site. ",
                      br(), br(),
                      tableOutput('resprange'),
                      br(), br()),
               fluidRow(column(12), 
                        DTOutput('siteeffstable') 
            )
        )
    )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sitename <- renderText(paste("Site:", 
                                        unique(site_dat$site_name[site_dat$site_code == input$selected_site]),
                                        "| Country:",
                                        unique(site_dat$country[site_dat$site_code == input$selected_site]),
                                        "| Habitat:",
                                        unique(site_dat$habitat[site_dat$site_code == input$selected_site])))
    
    output$richbmviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        yr<-plot.rich_coef3 %>% select(site_code,xmax)
        plot.rich_fitted.npk <- plot.rich_fitted.npk %>% left_join(yr)
        plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% left_join(yr)
        
        
           rich.r<- ggplot()+
            geom_point(data = plot.rich_fitted.npk %>% filter(site_code == input$selected_site) ,
                       aes(x = year_trt, y = all.div), colour = "#0B775E",
                       size = 1.3,alpha=0.5) +
               geom_point(data = plot.rich_fitted.ctl %>% filter(site_code == input$selected_site) ,
                          aes(x = year_trt, y = all.div),  colour = "black",
                          size = 1.3, shape=1, alpha=0.5) +
            geom_segment(data = plot.rich_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                             yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                             group = site_code), colour = "#0B775E",
                         size = .7) +
               geom_segment(data = plot.rich_coef3 %>% filter(site_code == input$selected_site) ,
                            aes(x = xmin, 
                                xend = xmax,
                                y = (Intercept   + (ISlope) * xmin),
                                yend =  (Intercept  + (ISlope) * xmax),
                                group = site_code), colour = "black", linetype = "dashed",
                            size = .7) +
             ylim(0,40)+
            scale_x_continuous(breaks=c(0,1,3,6,9,12),limits=c(0, 12)) +
            labs(
                x = 'Year',
                y = ' Species richness', title= 'A) Species Richness') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               plot.title = element_text(size=12))
           
           
           rich.eff<-ggplot() + 
               geom_point(data = study.rich.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff, color=response),size = 2) +
               geom_errorbar(data = study.rich.p %>% filter(site_code == input$selected_site), aes(x = response, ymin = eff_lower,
                                                        ymax = eff_upper, color=response),
                             width = 0, size = 0.7) +
               # facet_wrap(~Model)+
               labs(x = '',
                    # y= expression(paste('Effect of NPK on Species Richness'))
                    y='')+
               geom_hline(yintercept = 0, lty = 2) +
               # scale_y_continuous(breaks=c(0,-0.5)) +
               scale_color_manual(values = c("#000000","#0B775E")) +
               theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                            axis.text.y = element_text(size=6),
                                            axis.text.x = element_text(size=6),
                                            title=element_text(size=8),
                                            strip.background = element_blank(),legend.position="none")
           
           
           richviz <- rich.r +  annotation_custom(ggplotGrob(rich.eff), xmin = 7, xmax = 12, 
                                           ymin = 28, ymax = 40)
           
           yr<-plot.bm_coef3 %>% select(site_code,xmax)
           plot.bm_fitted.npk <- plot.bm_fitted.npk %>% left_join(yr)
           plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% left_join(yr)
           
           
 
           
           
          bm.r <- ggplot() +
               geom_point(data = plot.bm_fitted.npk %>% filter(site_code == input$selected_site),
                          aes(x = year_trt, y = plot.mass), alpha=0.5,colour = "#0B775E",
                          size = 1.3,alpha=0.7) +
              geom_point(data = plot.bm_fitted.ctl %>% filter(site_code == input$selected_site) ,
                         aes(x = year_trt, y = plot.mass), alpha=0.5, colour = "black",
                         size = 1.3, shape=1, alpha=0.7) +
               geom_segment(data = plot.bm_coef3 %>% filter(site_code == input$selected_site),
                            aes(x = xmin, 
                                xend = xmax,
                                y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                                yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                                group = site_code),  colour = "#0B775E",
                            size = 0.7) +
              geom_segment(data = plot.bm_coef3 %>% filter(site_code == input$selected_site) ,
                           aes(x = xmin, 
                               xend = xmax,
                               y = (Intercept   + (ISlope) * xmin),
                               yend =  (Intercept  + (ISlope) * xmax),
                               group = site_code), colour = "black", linetype = "dashed",
                           size = .7) +
               # uncertainy in fixed effect
               labs(x='Year',
                    #x = 'Years',
                    y = expression(paste('Biomass (g/',m^2, ')')), title= 'B) Biomass') +
              ylim(-20,2000)+
               scale_x_continuous(breaks=c(0,1,3,6,9,12),limits=c(0, 12)) +
               theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                                  plot.title = element_text(size=12))
           
          bm.eff<-ggplot() + 
              geom_point(data = study.bm.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff, color=response),size = 2) +
              geom_errorbar(data = study.bm.p %>% filter(site_code == input$selected_site), aes(x = response, ymin = eff_lower,
                                                                                                  ymax = eff_upper, color=response),
                            width = 0, size = 0.7) +
              # facet_wrap(~Model)+
              labs(x = '',
                   # y= expression(paste('Effect of NPK on Species bmness'))
                   y='')+
              geom_hline(yintercept = 0, lty = 2) +
               scale_y_continuous(breaks=c(0,30)) +
              scale_color_manual(values = c("#000000","#0B775E")) +
              theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                           axis.text.y = element_text(size=6),
                                           axis.text.x = element_text(size=6),
                                           title=element_text(size=8),
                                           strip.background = element_blank(),legend.position="none")
          
          
          bmviz <- bm.r +  annotation_custom(ggplotGrob(bm.eff), xmin = 7, xmax = 12, 
                                             ymin = 1400 ,ymax = 2000)
          
          
          plot.rich_fitted.ctl$Plot<-"Control Plot"
          plot.rich_fitted.npk$Plot<-"NPK Plot"
          
          
          plot.rich_coef.ctl <- plot.rich_coef3
          plot.rich_coef.ctl$Treatment<-"Control"
          
          
          plot.rich_coef.npk <- plot.rich_coef3
          plot.rich_coef.npk$Treatment<-"NPK"
          
          
          r.leg<- ggplot()+
              geom_point(data = plot.rich_fitted.npk %>% filter(site_code == input$selected_site) ,
                         aes(x = year_trt, y = all.div,shape=Plot), #colour = "#0B775E",
                         size = 1.3,alpha=0.5) +
              geom_point(data = plot.rich_fitted.ctl %>% filter(site_code == input$selected_site) ,
                         aes(x = year_trt, y = all.div,shape=Plot), # colour = "black",
                         size = 1.3,  alpha=0.5) +
              geom_segment(data = plot.rich_coef.npk %>% filter(site_code == input$selected_site) ,
                           aes(x = xmin, 
                               xend = xmax,
                               y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                               yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                               group = site_code,linetype=Treatment), #colour = "#0B775E",
                           size = .7) +
              geom_segment(data = plot.rich_coef.ctl %>% filter(site_code == input$selected_site) ,
                           aes(x = xmin, 
                               xend = xmax,
                               y = (Intercept   + (ISlope) * xmin),
                               yend =  (Intercept  + (ISlope) * xmax),
                               group = site_code,linetype=Treatment), #colour = "black", linetype = "dashed",
                           size = .7) +
              ylim(0,40)+
              scale_x_continuous(breaks=c(0,1,3,6,9,12),limits=c(0, 12)) +
              scale_shape_manual(values=c(1,16))+
              scale_linetype_manual(values=c("dashed", "solid"))+
              labs(
                  x = 'Year',
                  y = ' Species richness', title= 'A) Species Richness') +
              theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                                 plot.title = element_text(size=12))
          
          
          
          study.rich.p2 <-study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 
          
          study.bm.p2<-study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 
          
          study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2) %>% filter(response == "NPK")
          
          bef.q <- ggplot()+
              geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
              geom_point(data= study.effs.p %>% filter(site_code == input$selected_site), aes(x= r.eff , y= b.eff),colour = "#0B775E", alpha=0.5,size=2) +
              geom_errorbar(data= study.effs.p %>% filter(site_code == input$selected_site),aes(x= r.eff, y= b.eff,ymin = b.eff_lower, ymax = b.eff_upper), colour = "#0B775E", alpha=0.5, width = 0, size = 0.75) +
              geom_errorbarh(data= study.effs.p %>% filter(site_code == input$selected_site),aes(x= r.eff, y= b.eff,xmin =  r.eff_lower, xmax =r.eff_upper), colour = "#0B775E", alpha=0.5, width = 0, size = 0.75) +
              # geom_point(data = effs.p, aes(x= r.eff, #loss
              #                               y=  b.eff ),
              #            fill="#0B775E",color="#0B775E",size=8, alpha=0.6)+
              # geom_errorbar(data = effs.p,aes(x=r.eff,
              #                                 ymin = b.eff_lower, ymax = b.eff_upper),width=0,colour = "#0B775E", size = 2,alpha=0.9) +
              # geom_errorbarh(data = effs.p,aes(y=b.eff,
              #                                  xmin = r.eff_lower, xmax = r.eff_upper),height=0,colour = "#0B775E", size = 2, alpha=0.9) +
              scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5), limits=c(-5.5,7.5)) +
              scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200,-250),limits=c(-250,200)) +
              labs(x = 'Rate of change in species richness (species/year)',
                   y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
                   # title= 'Control Slope + NPK Slope'
                   subtitle = ' C)')
          
          #extract legend
          #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
          g_legend<-function(a.gplot){
              tmp <- ggplot_gtable(ggplot_build(a.gplot))
              leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
              legend <- tmp$grobs[[leg]]
              return(legend)}
          
          
          rlegend<-g_legend(r.leg)
         
          
          ( richviz | bmviz ) / ( rlegend ) / ( bef.q ) + plot_layout(heights = c(10,1,10))
          

        
    })
    
    
    output$slosssgainviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        yr<-sloss.trt_coef3 %>% select(site_code,xmax)
        sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% left_join(yr)
        sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% left_join(yr)
        
        sloss.trt_coef3$xs<-1
    
        
        sloss.r <- ggplot()  +
            geom_point(data = sloss.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = s.loss.n,),  alpha=0.5, color = "#B40F20",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sloss.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = s.loss.n), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sloss.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color = "#B40F20",
                         size = .7) +
            geom_segment(data = sloss.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
           scale_x_continuous(breaks=c(1,3,6,9,12), limits=c(0,12)) +
            scale_y_continuous(breaks=c(0,-5,-10,-15,-20,-25), limits=c(-25,2)) +
            labs(x = 'Year',
                 y = expression(paste('Species Loss')),  title= 'Species Loss') +
           # scale_color_viridis(discrete=FALSE,name="Length of Study") +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        sloss.eff<-ggplot() + 
            geom_point(data = study.sloss.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data = study.sloss.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            #facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Loss'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
            scale_color_manual(values = c("#000000","#B40F20")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        slossviz <- sloss.r +  annotation_custom(ggplotGrob(sloss.eff),  xmin = 7, xmax = 12, 
                                                 ymin = -25, ymax = -18)
        
        yr<-sgain.trt_coef3 %>% select(site_code,xmax)
        sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% left_join(yr)
        sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% left_join(yr)
        sgain.trt_coef3$xs<-1
        

        
        sgain.r <- ggplot()  +
            geom_point(data = sgain.trt_fitted.npk  %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = s.gain),  alpha=0.5,color= "#046C9A",
                       size = 1.3,  alpha=0.7) +
            geom_point(data = sgain.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = s.gain), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sgain.trt_coef3  %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color= "#046C9A",
                         size = .7) +
            geom_segment(data = sgain.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            scale_x_continuous(breaks=c(1,3,6,9,12),limits=c(0,12)) +
            scale_y_continuous(breaks=c(0,5,10,15,20,25), limits=c(-2,25)) +
            labs(x = 'Year',
                 y = expression(paste('Species Gain')), title= 'Species Gain') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        sgain.eff<-ggplot() + 
            geom_point(data = study.sgain.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data =  study.sgain.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            # facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Gain'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(0,0.05,0.3)) +
            scale_color_manual(values = c("#000000","#046C9A")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        sgainviz <- sgain.r +  annotation_custom(ggplotGrob(sgain.eff), xmin = 7, xmax = 12, 
                                                 ymin = 18, ymax = 25)
        
        
         (slossviz | sgainviz)
        
        
    })
    
    
    output$priceviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        #SL
        yr<-sl.trt_coef3 %>% select(site_code,xmax)
        sl.trt_fitted.npk <- sl.trt_fitted.npk %>% left_join(yr)
        sl.trt_fitted.ctl <- sl.trt_fitted.ctl %>% left_join(yr)
        sl.trt_coef3$xs<-1
        
        
        sl.r <- ggplot()  +
            geom_point(data = sl.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SL),  alpha=0.5,color = "#B40F20",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sl.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = SL), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sl.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color = "#B40F20",
                         size = .7) +
            geom_segment(data = sl.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
             scale_x_continuous(breaks=c(1,3,6,9,12), limits=c(0,12)) +
             ylim(-400,0) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
                title= 'Biomass Change Due To Species Loss') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        
        
        sl.eff<-ggplot() + 
            geom_point(data = study.sl.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data = study.sl.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                                                                                 ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            #facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Loss'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
            scale_color_manual(values = c("#000000","#B40F20")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        slviz <- sl.r +  annotation_custom(ggplotGrob(sl.eff),  xmin = 7, xmax = 12, 
                                           ymin = -400, ymax = -275)
        
        
        yr<-sg.trt_coef3 %>% select(site_code,xmax)
        sg.trt_fitted.npk <- sg.trt_fitted.npk %>% left_join(yr)
        sg.trt_fitted.ctl <- sg.trt_fitted.ctl %>% left_join(yr) 
        sg.trt_coef3$xs<-1
        
        
        
        sg.r <- ggplot()  +
            geom_point(data = sg.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SG), alpha=0.5,color= "#046C9A",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sg.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = SG), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sg.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ), color= "#046C9A",
                         size = .7) +
            geom_segment(data = sg.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
             scale_x_continuous(breaks=c(1,3,6,9,12),limits=c(0,12)) +
             ylim(0,400) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')),  title= 'Biomass Change Due To Species Gain') +
           # scale_color_viridis(discrete=FALSE,name="Length of Study") +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
    
        
        sg.eff<-ggplot() + 
            geom_point(data = study.sg.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data =  study.sg.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                                                                                  ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            # facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Gain'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(0,0.05,0.3)) +
            scale_color_manual(values = c("#000000","#046C9A")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        sgviz <- sg.r +  annotation_custom(ggplotGrob(sg.eff),  xmin = 7, xmax = 12, 
                                           ymin = 275, ymax = 400)
        
        yr<-cde_coef3 %>% select(site_code,xmax)
        cde_fitted.npk <- cde_fitted.npk %>% left_join(yr)
        cde_fitted.ctl <- cde_fitted.ctl %>% left_join(yr)
        cde_coef3$xs<-1
        
        
        cde.r <- ggplot()  +
            geom_point(data = cde_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = CDE),  alpha=0.5,color="#F98400",
                       size = 1.3,  alpha=0.7) +
            geom_point(data = cde_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = CDE), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = cde_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ), color="#F98400",
                         size = .7) +
            geom_segment(data = cde_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
             scale_x_continuous(breaks=c(1,3,6,9,12),limits=c(0,12)) +
             ylim(-500,1000) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
                title= 'Persistent Species Change in Biomass') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        
        cde.eff<-ggplot() + 
            geom_point(data =study.cde.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data = study.cde.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                            ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            # facet_wrap(~Model)+
            labs(x = '',
                 #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
           #scale_y_continuous(breaks=c(-8,0,4,14)) +
            scale_color_manual(values = c("#000000","#F98400")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        cdeviz <- cde.r +  annotation_custom(ggplotGrob(cde.eff), xmin = 7, xmax = 12, 
                                         ymin = 500, ymax = 1000)
        
        
        
        sg.trt_fitted.ctl$Plot<-"Pairwise Control Plot"
        sg.trt_fitted.npk$Plot<-"Pairwise NPK Plot"

        
        sg.trt_coef.ctl <- sg.trt_coef3
        sg.trt_coef.ctl$Treatment<-"Control"
        
        
        sg.trt_coef.npk <- sg.trt_coef3
        sg.trt_coef.npk$Treatment<-"NPK"
        
        
        p.leg <- ggplot()  +
            geom_point(data = sg.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SG,shape=Plot), alpha=0.5,#color= "#046C9A",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sg.trt_fitted.ctl %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SG,shape=Plot), alpha=0.5, #colour = "black",
                       size = 1.3,  alpha=0.7) +
            geom_segment(data = sg.trt_coef.npk %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             linetype=Treatment ), #color= "#046C9A",
                         size = .7) +
            geom_segment(data = sg.trt_coef.ctl %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code,linetype=Treatment), #colour = "black", linetype = "dashed",
                         size = .7) +
            scale_x_continuous(breaks=c(1,3,6,9,12),limits=c(0,12)) +
            ylim(0,400) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')),  title= 'Biomass Change Due To Species Gain') +
            scale_shape_manual(values=c(1,16))+
            scale_linetype_manual(values=c("dashed", "solid"))+
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        
        p.leg
        
        #extract legend
        #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
        g_legend<-function(a.gplot){
            tmp <- ggplot_gtable(ggplot_build(a.gplot))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            return(legend)}
        
        plegend<-g_legend(p.leg)
        
        # patchwork solution
        
       (slviz | sgviz  | cdeviz ) / (plegend) +
            plot_layout( heights = c(10,2)) 
        
        
        
    })
    
    output$pricevectorviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        
        study.price.cloud<-ggplot()+
            geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
            #treatment effects
            geom_segment(data = all.effs %>% filter(site_code == input$selected_site),
                         aes(x = 0,
                             xend = sloss.trt.rate.p  ,
                             y = 0,
                             yend = sl.trt.rate.p   ),
                         colour= "#B40F20",
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            geom_segment(data = all.effs %>% filter(site_code == input$selected_site),
                         aes(x = sloss.trt.rate.p ,
                             xend = (sloss.trt.rate.p)+(sgain.trt.rate.p ) ,
                             y = sl.trt.rate.p ,
                             yend = (sl.trt.rate.p)+(sg.trt.rate.p  ) ),
                         colour= "#046C9A",
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            geom_segment(data = all.effs %>% filter(site_code == input$selected_site),
                         aes(x = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                             xend = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                             y = (sl.trt.rate.p)+(sg.trt.rate.p ),
                             yend =(sl.trt.rate.p)+(sg.trt.rate.p)+ (cde.trt.rate.p  )),
                         colour= "#F98400",
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            labs(x = 'Rate of change in species (species/year)',
                 y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
                 # title= 'Rate of change / year '
                 title = '')
        
        study.sloss.p$Vector="Losses"
        study.sgain.p$Vector="Gains"
        study.cde.p$Vector="Persistent Sp."
        
        
        post.leg<-ggplot()+
            geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
            geom_segment(data = study.cde.p ,
                         aes(x = 0,
                             xend = 0,
                             y = 0,
                             yend = eff ,colour= Vector),
                         size = 0.2,  alpha = 0.4,
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            geom_segment(data = study.sloss.p ,
                         aes(x = 0,
                             xend = eff ,
                             y = 0,
                             yend = eff ,colour= Vector ),
                         size = 0.2,  alpha = 0.4,
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            geom_segment(data = study.sgain.p ,
                         aes(x = 0,
                             xend = eff ,
                             y = 0,
                             yend = eff ,
                             colour= Vector), size = 0.2,  alpha = 0.4,
                         arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
            scale_color_manual(name='Response',breaks=c("Losses","Gains","Persistent Sp."),
                               values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
            labs(x = 'Effect of NPK on Change in Species / Year',
                 y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
                 title= '')
        
        #extract legend
        #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
        g_legend<-function(a.gplot){
            tmp <- ggplot_gtable(ggplot_build(a.gplot))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            return(legend)}
        
        p.legend<-g_legend(post.leg)
        

        
        (study.price.cloud ) /  (p.legend) +  plot_layout(heights = c(10,1))
        
    })
    
    output$siteeffstable <- renderDT({
        
        site_effs <- p.all %>% filter(site_code == as.character(input$selected_site)) %>%
            mutate_if(is.numeric,  round, 2) %>%
            rename("Site Code" = "site_code")
        
        DT::datatable(site_effs,filter = "top",
                      options = list(pageLength = 14))
    })
    
    }



# Run the application 
shinyApp(ui = ui, server = server)
