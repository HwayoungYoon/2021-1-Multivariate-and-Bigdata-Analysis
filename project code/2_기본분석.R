# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco_data.RData")


##########################################################################

attach(Eco_data)

# 교차분석
## sex, address, house.type
library(sjPlot)

tab_xtab(Eco_data$sex, Eco_data$address, show.col.prc=TRUE, 
         var.labels=c("성별", "행정구역"), encoding="UTF-8")
set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(Eco_data$sex, Eco_data$address, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)

tab_xtab(Eco_data$sex, Eco_data$house.type, show.col.prc=TRUE, 
         var.labels=c("성별", "세대구분"), encoding="UTF-8")
set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(Eco_data$sex, Eco_data$house.type, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)

tab_xtab(Eco_data$address, Eco_data$house.type, show.col.prc=TRUE, 
         var.labels=c("행정구역", "세대구분"), encoding="UTF-8")
set_theme(geom.label.size=4.5, axis.textsize=1.1, legend.pos="bottom")
plot_xtab(Eco_data$address, Eco_data$house.type, type="bar", y.offset=0.01,
          margin="col", coord.flip=TRUE, wrap.labels=7, geom.colors="Set2",
          show.summary=TRUE)

##########################################################################

# 상관분석
## 모든 순서형, 연속형 변수
cor.var <- Eco_data[c("age","health","stress","family.relationship",
                      "environmental.cost","study","income","restrictions",
                      "social.safety","environment.feel",
                      "environmental.issues","prevention.pollution")]
library(sjPlot)
tab_corr(cor.var, corr.method="spearman", na.deletion="pairwise", 
         p.numeric=TRUE, triangle="lower", encoding="EUC-KR")
set_theme(axis.textsize=1.0)
sjp.corr(cor.var, corr.method="spearman", wrap.labels=5, na.deletion="pairwise")

##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")


