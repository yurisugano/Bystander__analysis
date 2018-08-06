pd <- position_dodge2(.5)
Linegraph_all <- ggplot(Gather_all_summary,aes(x=Days, y=mean, color=Condition, group = Condition)) + 
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width=.5,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, fill='white') +
  scale_colour_manual(values = c("Duo" = "#e05757", "Trio" = "#827b7b")) +
  labs(
    title = "Average latency across days for simulated groups",
    y = "Average latency (min) ± SD",
    x = "Days") +
  theme_classic() +
  theme(legend.position=c(.75,.75),
        legend.background = element_rect(linetype="solid",color="black")) 
Linegraph_all



pd <- position_dodge2(.25)
Linegraph_trios <- ggplot(Gather_all_dyad,aes(x=Days, y=mean)) + 
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width=.2) +
  geom_line(mapping=aes(group = Condition)) +
  geom_point(size=2, fill='white') +
  geom_point(data=Expt_dyad_latency,mapping=aes(x=Days, y=Latency,color = Expt_dyad_latency$Cond),  size=3, alpha=.6, position = pd) +
  labs(
    title = "Average latency for simulated duos vs. observed data for duos",
    y = "Average latency (min) ± SD for simulations",
    x = "Days") +
  theme_classic() +
  theme(legend.position=c(.85,.85),
        legend.background = element_rect(linetype="solid",color="black")) +
  theme(legend.title=element_blank()) 
Linegraph_trios



pd <- position_dodge2(.25)
Linegraph_duos <- ggplot(Gather_all_dyad,aes(x=Days, y=mean)) + 
  geom_errorbar(aes(ymin = mean-ci, ymax=mean+ci),width=.2) +
  geom_line(mapping=aes(group = Condition)) +
  geom_point(size=2, fill='white') +
  geom_point(data=Expt_dyad_latency,mapping=aes(x=Days, y=Latency,color = Cond),  size=3, alpha=.6, position = pd) +
  labs(
    title = "Average latency for simulated duos vs. observed data for duos",
    y = "Average latency (min) + 95% CI for simulations",
    x = "Days") +
  theme_classic() +
  theme(legend.position=c(.85,.85),
        legend.background = element_rect(linetype="solid",color="black")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(size=18))
Linegraph_duos



Opening_graph <- ggplot(Opening_analysis, aes(x=Days, y=Latency)) +
  geom_violin(scale="area")
Opening_graph




Non_Openers_triad_graph <- ggplot(Non_Openers_Tryad_Summary,aes(x=Days, y=mean)) + 
  geom_errorbar(aes(ymin = mean-ci, ymax=mean+ci),width=.2) +
  geom_line(mapping=aes(group = Condition)) +
  geom_point(size=2, fill='white') +
  geom_point(data=Expt_tryad_nonopeners,mapping=aes(x=Days, y=Latency,color=Cond),  size=5, alpha=.6, position = pd) +
  
  labs(
    title = "Average number of non-openers for simulated trios vs. observed data for trios",
    y = "Average number of non-openers + 95% CI for simulations",
    x = "Days") +
  theme_classic() +
  theme(legend.position=c(.85,.85),
        legend.background = element_rect(linetype="solid",color="black")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(size=18))
Non_Openers_triad_graph


pd <- position_dodge2(.3)
Openers_dyad_graph <- ggplot(Openers_dyad_Summary,aes(x=Days, y=latencymed)) + 
  geom_point(data = Expt_dyad_latency, mapping = aes (x = Days, y = Latency, color = V3),
             size=4, alpha = .5, position =pd) +
  geom_errorbar(aes(ymin = cimin, ymax=cimax),
                color="#514d4d", width=.2) +
  geom_line(mapping = aes(group = Group),color="#514d4d") +
  geom_point(size=3, color="#514d4d") +
    labs(
    title = "Median latency for openers in simulations\nvs experimental groups (duos)",
    y = "Median latency + 95% CI for openers in simulations",
    x = "Days") +
  theme_classic() +
  theme(legend.background = element_rect(linetype="solid",color="black")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(size=18))
Openers_dyad_graph





pd <- position_dodge2(.3)
Openers_triad_graph <- ggplot(Openers_triad_Summary,aes(x=Days,y=latency)) + 
  geom_errorbar(aes(ymin = cimin, ymax=cimax),
                color="#a8a8a8", width=.3, size=1) +
  geom_errorbar(aes(ymin = cimin, ymax=cimax),
                color="#a8a8a8", width=.3) +
  geom_line(mapping = aes(group = Group),color="#514d4d") +
  geom_point(mapping = aes (color = Group), size=5) +
  geom_point(data = Expt_triad_latency, mapping = aes (y = Latency, color = Cond),
             size=4, alpha = .3, position =pd) +
  labs(
    title = "Median latency for openers in simulations\nvs experimental groups (trios)",
    y = "Median latency + 95% CI for openers in simulations",
    x = "Days") +
  theme_classic() +
  theme(legend.background = element_rect(linetype="solid",color="black")) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(size=18)) +
  theme(legend.position = c(0.75,0.75)) +
  scale_color_manual(values=c("#4f4b4b","#e20606"))
  
Openers_triad_graph




Expt_triad_latency[,3] <- "Observed latency\nfor experimental\ntrios (n=8)"
colnames(Expt_triad_latency) <- c("Days","Latency","Cond")
Expt_triad_latency[,1] <- as.factor(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8)))
Expt_triad_latency[,3] <- "Observed latency\nfor experimental\ntrios"
Expt_triad_latency <- gather(Expt_triad_latency,key="Day",value="Latency")
Expt_triad_latency <- read.csv(file="Expt_triad_latency.csv")

tempor <- read.csv(file="Expt_dyad_latency.csv")
