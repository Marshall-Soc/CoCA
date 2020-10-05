# ---------------------------------------------- #
## Project Title: Concept Class Analysis
## Script Purpose: Reproduction code for the paper
## Date: 2020-09-30 
## Author: Marshall A. Taylor and Dustin S. Stoltz
# ---------------------------------------------- #

# -----------------------------------------------------------------------------
# PACKAGES
# -----------------------------------------------------------------------------

    pacman::p_load(tidyverse, ggpubr,gridExtra,
                   corclass, reshape2,
                   qgraph,ggcorrplot, RCA,
                   install = TRUE)

    source("code/CoCA_repo_cca2.R")

# -----------------------------------------------------------------------------
# Figure #1 RCA Plot
# -----------------------------------------------------------------------------

    plot.a <- data.frame(A = c(4,4,4,5,5,3,3),
                         B = c(2,2,2,3,3,1,1),
                         C = c(3,3,3,1,1,5,5),
                         D = c(1,5,1,4,4,4,2)) %>% 
                        t() %>% as.data.frame() %>% 
            magrittr::set_colnames(c("Pop","Blues","Rock","Classic","Opera","Bluegrass","Country")) %>%
            tibble::rownames_to_column(var="names") %>%
            reshape2::melt(., id = "names") %>%
    ggplot(aes(y=value, x=variable)) +
    geom_line(aes(group=names, color=names), size=.5) +
    geom_point(aes(color=names, group=names, shape=names), size=3) +
    theme_bw() + xlab("") + ylab("") +
    theme(axis.text=element_text(size=12),
            legend.position="bottom",
            legend.title=element_text(face="bold", size=12),
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
    scale_shape_manual(name="Respondents", breaks=c("A","B","C","D"), values=c(16,15,17,18)) +
    scale_color_manual(name="Respondents", breaks=c("A","B","C","D"), values=c(rep("black", 4))) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
           shape = guide_legend(title.position = "top", title.hjust = 0.5))

    png("figures/01_rca_plot.png", width = 8, height = 4, units = 'in', res = 350)
    plot.a
    dev.off()


# -----------------------------------------------------------------------------
#  Figure #2 Hypothetical CoCA plot
# -----------------------------------------------------------------------------

    df.sd <- data.frame(A = c(-0.1,-0.1,-0.1,0.4,0.4,-1.6,-1.6),
                    B = c(-0.5,-0.5,-0.5,0,0,-2,-2),
                    C = c(0.2,0.2,0.2,-0.3,-0.3,1.7,1.7),
                    D = c(-1.6,2,-1.6,1.5,1.5,1.1,-0.5)) %>% 
                    t() %>% as.data.frame() %>% 
                    magrittr::set_colnames(c("Party","Race","Gender","Education","Policy","Immigration","Affluence")) %>%
                    tibble::rownames_to_column(var="names")

    # Schematic patterns plot
    plot.sd1 <- df.sd %>% 
                reshape2::melt(id = "names") %>% 
        ggplot(aes(y=value, x=as.numeric(variable))) +
        geom_line(aes(group=names, color=names), size=.5) +
        geom_point(aes(color=names, group=names, shape=names), size=3) +
        theme_bw() +
        theme(axis.text=element_text(size=10),
                legend.position="bottom",
                legend.title=element_text(face="bold", size=12),
                legend.text=element_text(size=9),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
        xlab("") + ylab("") +
        scale_shape_manual(name="Documents", breaks=c("A","B","C","D"),
                            values=c(16,15,17,18)) +
        scale_color_manual(name="Documents", breaks=c("A","B","C","D"),
                            values=c("black","black","black","black")) +
        guides(color = guide_legend(title.position = "top",
                                    title.hjust = 0.5),
                shape = guide_legend(title.position = "top",
                                    title.hjust = 0.5)) +
        scale_x_continuous(breaks=1:7,
            #breaks=c("Party","Race","Gender","Education","Policy","Immigration","Affluence"),
                        labels=c("nonwhite","unskilled","illegal","unemployed","uneducated",
                                    "open","poor"),
            sec.axis = dup_axis(labels=c(expression(atop(bold("Race"), "white")),
                                        expression(atop(bold("Skills"), "skilled")),
                                        expression(atop(bold("Legality"), "legal")),
                                        expression(atop(bold("Employment"), "employed")),
                                        expression(atop(bold("Education"), "educated")),
                                        expression(atop(bold("Borders"), "closed")),
                                        expression(atop(bold("SES"), "rich")))))

    # df.sd2 <- melt(df.sd)

    eucl <- dist(df.sd[,2:8], method = "euclidean") %>% 
            as.vector() %>% as.data.frame() %>% 
            magrittr::set_rownames(c("AB","AC","AD","BC","BD","CD")) %>%
            magrittr::set_colnames(c("eucl") )
    eucl <- eucl/10.58301 #Denominator is the maximum possible Euclidean distance
        #Show that 10.58301 is the maximum possible Euclidean distance
        # df.test <- data.frame(A = c(2,2,2,2,2,2,2),
        #                       B = c(-2,-2,-2,-2,-2,-2,-2)) %>% t() %>% as.data.frame()
        # dist(df.test, method = "euclidean")

    acorr <- t(df.sd[,2:8]) %>% cor(., use = "complete.obs")
    acorr <- acorr[lower.tri(acorr)==TRUE] %>% 
            as.data.frame() %>%
            magrittr::set_rownames(c("AB","AC","AD","BC","BD","CD")) %>%
            magrittr::set_colnames(c("acorr") )

    relat <- RCA(df.sd[,2:8], alpha=NULL, num=0)$R %>% round(.,1)
    relat <- relat[which(lower.tri(relat)==TRUE)] %>% 
             as.data.frame() %>% 
             magrittr::set_rownames(c("AB","AC","AD","BC","BD","CD")) %>%
             magrittr::set_colnames(c("relat") )

    mat.sd <- cbind(eucl, acorr, relat) %>% 
              tibble::rownames_to_column(var="names") %>% 
              reshape2::melt(id = "names") %>%
              mutate(names = forcats::fct_rev(names))

    # bar chart
    plot.sd2 <- ggplot(mat.sd, aes(x=value, y=names, group=variable, fill=variable)) +
        geom_bar(stat="identity", aes(fill=variable, group=variable), 
                color="black", position=position_dodge()) +
        xlab("") + ylab("") + theme_bw() +
        theme(axis.text=element_text(size=12),
                legend.position="bottom",
                legend.title=element_text(face="bold", size=12),
                legend.text=element_text(size=8),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
        scale_fill_manual(name="Similarity Metric", breaks=c("eucl","acorr","relat"),
                            values=c("black","gray40","gray75"),
                            labels=c("Euclidean","Correlation","Relationality")) +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))


    png("figures/02_coca_plot.png", width = 14, height = 5, units = 'in', res = 500)
    ggarrange(plot.sd1, plot.sd2, 
            align="hv", labels=c("A","B"), 
            nrow=1, ncol=2, widths=c(2,1))
    dev.off()


# df.sd.model <- sapply(df.sd[,-8], as.numeric)
# rownames(df.sd.model) <- c("A","B","C","D")
# lm(D ~ C, data=as.data.frame(t(df.sd.model))) %>% summary()

# -----------------------------------------------------------------------------
# Create Simulated Data
# -----------------------------------------------------------------------------

    # Simulated Semantic Directions
    df.sd <- data.frame(A = c(-0.1,-0.1,-0.1,0.4,0.4,-1.6,-1.6),
                        B = c(-0.5,-0.5,-0.5,0,0,-2,-2),
                        C = c(0.1,0.1,0.1,-0.4,-0.4,1.6,1.6),
                        D = c(-0.06,-0.06,-0.06,0.24,0.24,-0.96,-0.96),
                        E = c(-1.6,2,-1.6,1.5,1.5,1.1,-0.5),
                        F = c(-2,1.6,-2,1.1,1.1,0.7,-0.9),
                        G = c(2,-1.6,2,-1.1,-1.1,-0.7,0.9),
                        H = c(-1.2,0.96,-1.2,0.66,0.66,0.42,-0.54),
                        I = c(3,2.7,-1.3,-1.3,-2,3,-1.3),
                        J = c(2.6,2.3,-1.7,-1.7,-2.4,2.6,-1.7),
                        K = c(-2.6,-2.3,1.7,1.7,2.4,-2.6,1.7),
                        L = c(1.8,1.62,-0.78,-0.78,-1.2,1.8,-0.78)) %>% 
                        t() %>% as.data.frame() %>%
                        magrittr::set_colnames(c("D1","D2","D3","D4","D5","D6","D7"))

    # Replicate each row ten times
    df.sd <- df.sd[rep(seq_len(nrow(df.sd)), each = 10),]

    # CCA on initial directions
    groups <- cca2(df.sd, 
                filter.significance = TRUE,
                filter.value = 0.05, zero.action = "drop",
                verbose = FALSE) 

    # Extract class membership
    membership <- cbind(rownames(groups$cormat), 
                        groups$membership) %>% 
                        as.data.frame() 

    # add noise and extract class membership
    for (j in seq(1, 50, by = .05)) {
        temp <- data.frame(matrix(nrow=120, ncol=7), row.names = rownames(df.sd))
        # - Doesn't appear to work well
        # set.seed(123)
        for (i in rownames(df.sd) ) {
            temp[i,] <- jitter(as.numeric(df.sd[i,]), factor=j)
            }
        groups.temp <- cca2(temp, filter.significance = TRUE,
                            filter.value = 0.05, zero.action = "drop",
                            verbose = FALSE)$membership
        membership <- cbind(membership, groups.temp)
        # rm(temp)
        # rm(groups.temp)
        }

    colnames(membership) <- 1:983
    cramerv <- data.frame(matrix(ncol=983, nrow=1))
    colnames(cramerv) <- colnames(membership)

    for (i in 3:983) {
    cramerv[,i] <- rcompanion::cramerV(table(membership[,2], membership[,i]))
    }

    membership <- rbind(membership, cramerv)
    df.sim <- as.data.frame(t(membership))
    
# -----------------------------------------------------------------------------
# Descriptive Statistics for Cramér's V
# -----------------------------------------------------------------------------
    
    membership[121,] %>% as.numeric() %>% summary()
    membership[121,] %>% as.numeric() %>% psych::describe()
    membership[121,][which(membership[121,]==1)] %>% 
        as.numeric() %>% sum(., na.rm=T)

# -----------------------------------------------------------------------------
# Figure #4 Noise Factor by Cramér's V
# -----------------------------------------------------------------------------

    sim.plot <- ggplot(df.sim, aes(x=1:983, y=as.numeric(df.sim[,121]))) +
        geom_point(size=2, color="#1696d2", alpha = .5) +
        geom_smooth(method="loess", color="#000000", formula = y ~ x, span = .1) +
        ylab(expression(bold(paste("Cramér's ", italic(V))))) +
        xlab("Noise Factor") +
        theme_bw() + ylim(0,1.2) +
        theme(axis.text.y=element_text(size=12),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(size=12),
                axis.title=element_text(face="bold", size=12)) +
        scale_x_continuous(breaks=seq(1,981, by = 40), labels=seq(1, 50, by = 2)) +
        geom_hline(yintercept=mean(as.numeric(df.sim[,121]), na.rm=TRUE)) +
        geom_hline(yintercept=median(as.numeric(df.sim[,121]), na.rm=TRUE), linetype="dashed")


    png("figures/04_sim_noise_plot.png", width = 7, height = 4, units = 'in', res = 350)
    sim.plot
    dev.off()

# -----------------------------------------------------------------------------
# Plot #3 Schematic Patterns
# -----------------------------------------------------------------------------

    df.sd2 <- df.sd %>% 
              tibble::rownames_to_column(var="names") %>% 
              reshape2::melt(., id = "names")

    plot.base <- ggplot(df.sd2, aes(y = value, x = as.numeric(variable)) ) +
            xlab("") + ylab("") + ylim(-3,3) + theme_bw() +
            theme(axis.text=element_text(size=8),
                  legend.position='right',
                  legend.direction='vertical',
                  legend.title=element_text(face="bold", size=12),
                  legend.text=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank() ) +
            guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
                   shape = guide_legend(title.position = "top", title.hjust = 0.5)) +
            scale_x_continuous(breaks=1:7, labels=c(rep("Negative Pole", 7)),
                    sec.axis = dup_axis(labels=c(expression(atop(bold("Direction #1"), "Positive Pole")),
                                                expression(atop(bold("Direction #2"), "Positive Pole")),
                                                expression(atop(bold("Direction #3"), "Positive Pole")),
                                                expression(atop(bold("Direction #4"), "Positive Pole")),
                                                expression(atop(bold("Direction #5"), "Positive Pole")),
                                                expression(atop(bold("Direction #6"), "Positive Pole")),
                                                expression(atop(bold("Direction #7"), "Positive Pole")))))

    plot.sim.1 <- plot.base +
        geom_line( data=df.sd2[df.sd2$names=="A",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="A",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="B",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="B",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="C",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="C",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="D",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="D",], aes(group=names, color=names, shape=names), size=3) +   
        scale_shape_manual(name="Documents", breaks=c("A","B","C","D"), values=c(16,15,17,18)) +
        scale_color_manual(name="Documents", breaks=c("A","B","C","D"), values=c("#1696d2","black","black","black"))

    plot.sim.2 <- plot.base +
        geom_line( data=df.sd2[df.sd2$names=="E",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="E",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="F",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="F",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="G",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="G",], aes(group=names, color=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="H",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="H",], aes(group=names, color=names, shape=names), size=3) +
        scale_shape_manual(name="Documents", breaks=c("E","F","G","H"), values=c(16,15,17,18)) +
        scale_color_manual(name="Documents", breaks=c("E","F","G","H"), values=c("#1696d2","black","black","black"))

    plot.sim.3 <- plot.base +
        geom_line( data=df.sd2[df.sd2$names=="I",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="I",], aes(color=names, group=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="J",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="J",], aes(color=names, group=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="K",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="K",], aes(color=names, group=names, shape=names), size=3) +
        geom_line( data=df.sd2[df.sd2$names=="L",], aes(group=names, color=names), size=.5) +
        geom_point(data=df.sd2[df.sd2$names=="L",], aes(color=names, group=names, shape=names), size=3) +
        scale_shape_manual(name="Documents", breaks=c("I","J","K","L"), values=c(16,15,17,18)) +
        scale_color_manual(name="Documents", breaks=c("I","J","K","L"), values=c("#1696d2","black","black","black")) 


    png("figures/03_sim_plot_schemas.png", width = 8, height = 10, units = 'in', res = 600)
        gridExtra::grid.arrange(plot.sim.1, plot.sim.2, plot.sim.3, ncol=1)
    dev.off()


# -----------------------------------------------------------------------------
# Figure #5 Network Plots
# -----------------------------------------------------------------------------

    # Correlation Matrices
    cormat <- cbind(df.sd, groups$membership)
    cormat1 <- cor(cormat[cormat[,8]==1,][1:7], use = "complete.obs")
    cormat2 <- cor(cormat[cormat[,8]==2,][1:7], use = "complete.obs")
    cormat3 <- cor(cormat[cormat[,8]==3,][1:7], use = "complete.obs")

    ##
    png("figures/05_cca_plot_all.png", width = 11, height = 4, units = 'in', res = 350)
    par(mfrow=c(1,3))

    qgraph(cormat1,
        graph = "cor",
        minimum=.15, maximum=1, threshold="sig", sampleSize=40,
        #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
        layout = "spring", repulsion = 1.86, label.cex=2,
        posCol="black", negCol="black", negDashed=TRUE,
        borders=TRUE, shape = "circle", label.prop = 0.75,
        curveAll=FALSE, edge.labels=FALSE, edge.label.cex = 0.45, esize = 8,
        title="Class #1", labels=c("D1","D2","D3","D4","D5","D6","D7")
        )

    qgraph(cormat2,
        graph = "cor",
        minimum=.15, maximum=1, threshold="sig", sampleSize=40,
        #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
        layout = "spring", repulsion = 1.86, label.cex=2,
        posCol="black", negCol="black", negDashed=TRUE,
        borders=TRUE, shape = "circle", label.prop = 0.75,
        curveAll=FALSE, edge.labels=FALSE, edge.label.cex = 0.45, esize = 8,
        title="Class #2", labels=c("D1","D2","D3","D4","D5","D6","D7")
        )

    qgraph(cormat3,
        graph = "cor",
        minimum=.15, maximum=1, threshold="sig", sampleSize=40,
        #Not plotting corrs < |.15|. Max non-diagonal abs(corr) = .813.
        layout = "spring", repulsion = 1.86, label.cex=2,
        posCol="black", negCol="black", negDashed=TRUE,
        borders=TRUE, shape = "circle", label.prop = 0.75,
        curveAll=FALSE, edge.labels=FALSE, edge.label.cex = 0.45, esize = 8,
        title="Class #3", labels=c("D1","D2","D3","D4","D5","D6","D7")
        )
    par(mfrow=c(1,1))
    dev.off()

# -----------------------------------------------------------------------------
#
# -----------------------------------------------------------------------------

