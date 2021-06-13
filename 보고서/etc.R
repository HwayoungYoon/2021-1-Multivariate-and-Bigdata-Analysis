age
sex
education
restrictions
health
healthcare
stress
social.safety
environment.feel
environmental.cost
environmental.issues
prevention.pollution
income

# 여러 그래프를 한 화면에
par(mfrow=c(3,2))

# figure margin 설정
par("mar")
par(mar=c(3.1,4.1,2.1,2.1))

# xlsx로 출력
library(writexl)
write_xlsx(dataframe, path="dataframe.xlsx")
