library(readxl)
library(ggplot2)
library(tidyr)
library(magrittr)
library(ComplexHeatmap)
library(networkD3)

#======== READ DATA ===========================================================

pc <- read_xlsx("quarry-62834-of-articles-in-wikiproject-with-given-language-equivalent-run618788.xlsx")
map <- read_xlsx("quarry-64708-computational-biology-articles-with-spanish-language-equivalent-run639782.xlsx")
cosi_article_mtx <- read.csv("COSI-Article_matrix_v1.csv")
es_ratings <- data.frame(read_xlsx("Wikipedia en español evaluación de artículos de biología computacional v2.xlsx"))
entry_ratings <- read_xlsx("Spanish_wiki_competition_entry_ratings.xlsx",n_max = 25) %>% 
  subset(any_edits != "NO")
list_wikis <- read_xlsx("list_of_wikipedias.xlsx")

#======== FIG 2A (barplot of languages ordered by %) ==========================

t <- pc[2:21,]
t$...4 %<>% factor(.,levels=.)
ggplot(t,aes(...4,percent_of_en)) +
  geom_col(fill=ifelse(t$...4=="Spanish","#FABD00","grey"),col="black") +
  ylab("% English computational biology articles") +
  theme_bw() +
  theme(aspect.ratio=1/2,axis.text = element_text(color="black",size=12),
        axis.title.y = element_text(color="black",size=12),
        axis.text.x = element_text(angle=30,hjust=1),
        axis.title.x = element_blank())


#======== MATCH COSI-ARTICLE MATRIX AND QUARRY OUTPUT =========================

map$en_title <- gsub("_"," ",map$en_title) #clean up quarry output

#minor cleanup of cosi-article matrix titles to match quarry output
cosi_article_mtx$Article.title <- replace(cosi_article_mtx$Article.title,
                                          match(c("Gene ontology",
                                                  "Matthews correlation coefficient",
                                                  "Robert Rosen (theoretical biologist)"),
                                                cosi_article_mtx$Article.title),
                                          c("Gene Ontology","Phi coefficient",
                                            "Robert Rosen (biologist)"))
cosi_article_mtx_small <- cosi_article_mtx[,1:3]

#nothing in quarry output that doesn't match: 
map$en_title %>% .[!.%in% cosi_article_mtx_small$Article.title] 

#combine data frames:
combined <- cbind(cosi_article_mtx_small,
                  map[match(cosi_article_mtx_small$Article.title,map$en_title),3])


#======== HOW MANY ARTICLES OF GIVEN QUALITY ETC HAVE SPANISH EQUIVALENT? =====

# ie 20/20 of the 'Top' importance articles have spanish language equivalents
# and 73/122 'High' importance articles have equivalents
i <- "Top"
subset(combined,Current.Importance==i,select="es_title") %>% 
  is.na %>% table(.)/sum(combined$Current.Importance==i & 
                         !is.na(combined$Current.Importance))

#similarly for quality
q <- "Stub"
subset(combined,Current.article.quality==q,select="es_title") %>% 
  is.na %>% table(.)/sum(combined$Current.article.quality==q & 
                         !is.na(combined$Current.article.quality))


#======== FIG 2B (heatmap of proportion with Spanish equivalent) ==============

m <- matrix(nrow=5,ncol=4)
numerator <- m
denominator <- m
rownames(m) <- c("GA","B","C","Start","Stub")
colnames(m) <- c("Low","Mid","High","Top")
numerator <- m
denominator <- m
for(i in colnames(m)) {
  for(q in rownames(m)) {
    numerator[q,i] <- sum(combined$Current.Importance==i & 
                          !is.na(combined$Current.Importance) &
                          combined$Current.article.quality==q & 
                          !is.na(combined$Current.article.quality) &
                          !is.na(combined$es_title))
    denominator[q,i] <- sum(combined$Current.Importance==i & 
                            !is.na(combined$Current.Importance) &
                            combined$Current.article.quality==q & 
                            !is.na(combined$Current.article.quality))
  }
}
m <- numerator/denominator


row_anno_data <- sapply(rownames(m),function(q){
  sum(!is.na(subset(combined,Current.article.quality==q,select="es_title")))/
    sum(combined$Current.article.quality==q & !is.na(combined$Current.article.quality))
})
col_anno_data <- sapply(colnames(m),function(i){
  sum(!is.na(subset(combined,Current.Importance==i,select="es_title")))/
    sum(combined$Current.Importance==i & !is.na(combined$Current.Importance))
})

h <- Heatmap(m*100, name = "proportion",cluster_columns = F,cluster_rows = F,
        border = "black",rect_gp = gpar(col="black"),
        height = unit(5,"cm"),width = unit(8,"cm"),
        col = colorRampPalette(c("#FABD00","#AD1519"))(100),na_col = "white",
        cell_fun = function(j, i, x, y, width, height, fill) {
                            grid.text(sprintf("%d/%d", numerator[i, j],denominator[i,j]), 
                                      x, y, gp = gpar(fontsize = 12))
                            },
        bottom_annotation = columnAnnotation(foo1 = 100*col_anno_data,simple_anno_size=unit(0.6,"cm"),
             col=list(foo1 = circlize::colorRamp2(seq(0, 100, length = 100),
                                                  colorRampPalette(c("#FABD00","#AD1519"))(100))),
             foo3=anno_text(sprintf("%.1f", 100*col_anno_data),rot = 0,location = unit(2.5,"npc"),
                            just = "center"),show_legend=F),
        left_annotation = rowAnnotation(foo2 = 100*row_anno_data,simple_anno_size=unit(1.2,"cm"),
             col=list(foo2 = circlize::colorRamp2(seq(0, 100, length = 100),
                                                  colorRampPalette(c("#FABD00","#AD1519"))(100))),
             foo4=anno_text(sprintf("%.1f", 100*row_anno_data),rot = 0,
                            just = "right"),show_legend=F)
)
draw(h)


#======== TIDY UP THE SPANISH RATINGS =========================================

colnames(es_ratings)[7] <- "Quality.assessment"

es_ratings$ratings_clean <- sapply(es_ratings$Quality.assessment,function(x){strsplit(x,"\\)")}) %>%
                                          lapply(function(x){gsub("\\(","",x[[1]])}) %>% unlist

es_ratings$ratings_clean %<>% replace(.,which(. %in% c("ApB","ApF","ApG")),"ApD")

map_importance <- c("Low"=1,"Mid"=2,"High"=3,"Top"=4)
map_quality_en <- c("Stub"=1,"Start"=2,"C"=3,"B"=4,"GA"=5)
map_quality_es <- c("E"=1,"ApD"=2,"A"=3,"AB"=4,"AD"=5)


#======== MATCH SPANISH RATINGS WITH OTHER DATA FRAMES ========================

es_ratings_sml <- es_ratings[,c(3,4,8,12)]
es_ratings_sml$English.title <- gsub("_"," ",es_ratings_sml$English.title) #clean up quarry output
all(es_ratings_sml$English.title %in% cosi_article_mtx_small$Article.title) #sanity check

comb_ratings <- cbind(cosi_article_mtx_small[match(es_ratings_sml$English.title,
                                                   cosi_article_mtx_small$Article.title),],
                      es_ratings_sml[,-1])

# correlate quality between matched english and spanish articles
with(comb_ratings,cor.test(map_quality_en[Current.article.quality],
                           map_quality_es[ratings_clean],method="spearman"))


#======== FIG 2C (trend of english vs spanish quality) ========================

dots_df <- data.frame(en=rep(names(map_quality_en),5),
                      es=rep(names(map_quality_es),each=5))
dots_df$articles <- apply(dots_df,1,function(x){sum(comb_ratings$Current.article.quality==x[1] & 
                                                 comb_ratings$ratings_clean==x[2])})
dots_df$en %<>% factor(levels=names(map_quality_en))
dots_df$es %<>% factor(levels=names(map_quality_es))

ggplot() +
  geom_point(data=dots_df,aes(en,es,size=articles)) +
  scale_size_area() +
  geom_smooth(data=comb_ratings,aes(map_quality_en[Current.article.quality],
                                    map_quality_es[ratings_clean]),col="#AD1519",method="glm") +
  ggpubr::stat_cor(data=comb_ratings,aes(map_quality_en[Current.article.quality],
                                         map_quality_es[ratings_clean]),
                   method = "spearman",label.x.npc=0.0,label.y.npc = 1,size=5,
                   label.sep = "\n") +
  xlab("English Wikipedia quality") +
  ylab("Spanish Wikipedia quality") +
  theme_bw() +
  theme(aspect.ratio=1/2,axis.text = element_text(color="black",size=10))


#======== MAKE A FULL COMBINED DATA FRAME =====================================

combined_full <- cbind(cosi_article_mtx,map[match(cosi_article_mtx$Article.title,map$en_title),3])


#======== FIG 2D (which COSIs are underrepresented in Spanish?) ==============

apply(combined_full[,-c(1,2,3,25)],2,function(x){
  (((!is.na(combined_full$es_title[x==1])) %>% sum)/sum(x)*100) %>% round(.,3)}) %>% 
  .[order(.,decreasing=T)] %>%
  data.frame(coverage=., COSIs=names(.)) -> coverage

coverage$COSIs  <- replace(coverage$COSIs,
                           match(c("Text.Mining","X3D.SIG","COMP.MS","BIOINFO.CORE","Bio.Ontologies"),
                                 coverage$COSIs),
                           c("Text Mining","3D-SIG","COMP-MS","BIOINFO-CORE","Bio-Ontologies"))
coverage$COSIs %<>% factor(.,levels=.)
ggplot(coverage,aes(COSIs,coverage)) +
  geom_col(col="black",fill="#FABD00",width=0.75) +
  ylab("% coverage") +
  theme_bw() +
  theme(aspect.ratio=1/2,axis.text = element_text(color="black",size=10),
        axis.text.x = element_text(angle=30,hjust=1),
        axis.title.x = element_blank())


#======== CORRELATE NUM ENGLISH ARTICLES PER COSI WITH SPANISH COVERAGE =======

cosi_sizes <- apply(combined_full[,-c(1,2,3,25)],2,sum) %>% data.frame(size=.,COSIs=names(.))
cosi_sizes$COSIs  <- replace(cosi_sizes$COSIs,
                           match(c("Text.Mining","X3D.SIG","COMP.MS","BIOINFO.CORE","Bio.Ontologies"),
                                 cosi_sizes$COSIs),
                           c("Text Mining","3D-SIG","COMP-MS","BIOINFO-CORE","Bio-Ontologies"))

cov_size <- cbind(coverage,size=cosi_sizes[match(coverage$COSIs,cosi_sizes$COSIs),1])
cor.test(cov_size$coverage,cov_size$size,method="spearman")


#======== FIG 3A (arrowplot of en vs es article sizes, before and after) ======

t <- entry_ratings
t <- t[,c(4,11,12,14,15)]
t %<>% subset(!is.na(es_bytes_before) & !is.na(en_bytes_before))
t$Titulo.Español %<>% replace(match(c("Biología matemática y teórica",
                                      "Matriz de distancias",
                                      "Variante de Secuencia de Amplicón"),.),
                              c("Biología matemática\ny teórica",
                                "Matriz de\ndistancias",
                                "Variante de\nSecuencia de\nAmplicón"))

df <- pivot_longer(t,cols=2:5) %>% 
  separate_wider_delim(cols=name,delim="_bytes_",names=c("lang","time")) %>% 
  pivot_wider(names_from = lang,values_from = value)
ggplot(df,aes(x=en+1,y=es+1)) +
  geom_abline(slope=1,intercept=0,lty=3,color="grey") +
  geom_path(aes(group=Titulo.Español),
            arrow = grid::arrow(length=unit(0.08,"inches")),col="#AD1519",lwd=0.8) +
  ggrepel::geom_label_repel(data=subset(df,time=="after" & 
                                          Titulo.Español %in% c("Pangenoma",
                                                                "Biología matemática\ny teórica",
                                                                "Matriz de\ndistancias",
                                                                "Variante de\nSecuencia de\nAmplicón")),
                            aes(label=Titulo.Español),size=3.5,
                            max.overlaps = 20,
                            min.segment.length = 0,max.iter = 20000
  ) +
  scale_x_continuous(limits=c(0,70000)) +
  scale_y_continuous(limits=c(0,70000)) +
  xlab("English Wikipedia article size (bytes)") +
  ylab("Spanish Wikipedia article size (bytes)") +
  theme_bw() +
  theme(aspect.ratio=1)


#======== FIG 3B (boxplot of final quality vs article size) ===================

entry_ratings$updated_rating %<>% factor(levels = names(map_quality_es))

#does page size correlate with article quality
cor.test(entry_ratings$es_bytes_after,map_quality_es[entry_ratings$updated_rating],method = "spearman")

ggplot(entry_ratings,aes(updated_rating,es_bytes_after)) +
  geom_boxplot(aes(fill=updated_rating)) +
  geom_point(alpha=0.7,size=2) +
  scale_fill_manual(values=c("#eeeb87","#3a80b6","#009422","#e0a600")) +
  scale_y_continuous(limits = c(0,70000)) +
  ggpubr::stat_cor(data=data.frame(updated_rating=map_quality_es[entry_ratings$updated_rating],
                                   es_bytes_after=entry_ratings$es_bytes_after),
                   method = "spearman",label.x.npc=0,label.y.npc = 0.7,size=6,
                   label.sep = "\n") +
  xlab("Final quality rating") +
  ylab("Spansh Wikipedia article size (bytes)") +
  theme_bw() +
  theme(aspect.ratio=1,legend.position = "none")


#======== FIG 3C (Sankey plot of articles at timepoints) ======================

colors <- paste(c(rep("#e0a600",3),rep("#009422",3),rep("#3a80b6",3),
                  rep("#eeeb87",3),rep("#ff4530",3),rep("grey",2)), collapse = '", "')
colorJS <- paste('d3.scaleOrdinal(["',colors , '"])')

s_data <- list(links=data.frame(source=c(0,3,6,9,9,9, 12,12,12,12,12,15,  1,4,7, 10,13,16,16),
                                target=c(0,4,7,4,7,10,1, 4, 7, 10,13,4,   2,5,8, 11,14,5, 8),
                                value= c(0,2,5,3,3,4, 1, 1, 1, 2, 1, 1,   1,7,10,6, 1 ,2, 1)),
               nodes=data.frame(name=c("AD_before","AD_after","AD_final",
                                       "AB_before","AB_after","AB_final",
                                       "A_before","A_after","A_final",
                                       "ApD_before","ApD_after","ApD_final",
                                       "E_before","E_after","E_final",
                                       "New article","New articles")))

sankeyNetwork(s_data$links,s_data$nodes,"source","target","value","name",
              fontSize = 12,fontFamily = "arial", nodeWidth = 30,height=500,width=500,
              colourScale = colorJS, iterations=0)


#======== FIG 4A (expectation ratio of languages) =============================

w_speakers <- pc %>% subset(!is.na(`speakers (m)`))
priority <- cbind(w_speakers,list_wikis[match(w_speakers$lang,list_wikis$Wiki),c("Language","Articles")])
priority %<>% rbind(c("en",1501,100,"English",380,1456,NA,NA,"English",6754848))
priority$articles %<>% as.numeric
priority$Articles %<>% as.numeric
priority$`speakers (m)` %<>% as.numeric
priority$total_speakers %<>% as.numeric
priority$percent_of_en %<>% as.numeric
priority$cb_ratio <- (priority$articles/priority$Articles)/(priority$articles[31]/priority$Articles[31])
rownames(priority) <- priority$Language

priority %<>% .[order(.$cb_ratio,decreasing = T),]
priority$...4 %<>% factor(.,levels=.)
ggplot(priority,aes(...4,cb_ratio)) +
  geom_hline(yintercept = 1,col="grey",lty=3) +
  geom_col(fill=ifelse(priority$...4=="English","#AD1519",ifelse(t$...4=="Spanish","#FABD00","grey")),
           col="black") +
  ylab("Expectation ratio") +
  theme_bw() +
  theme(aspect.ratio=1/2,axis.text = element_text(color="black",size=10),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x = element_blank())


#======== FIG 4B (normalise num articles by num speakers) =====================

t$norm_articles <- t$articles/t$`speakers (m)`
t %<>% .[order(.$norm_articles,decreasing = T),]
t$...4 %<>% factor(.,levels=.)

ggplot(t,aes(...4,norm_articles)) +
  geom_col(fill=ifelse(t$...4=="English","#AD1519",ifelse(t$...4=="Spanish","#FABD00","grey")),
           col="black") +
  ylab("#articles / native speakers (m)") +
  theme_bw() +
  theme(aspect.ratio=1/2,axis.text = element_text(color="black",size=10),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x = element_blank())
