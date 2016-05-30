str= c('Chrome CB3-111', 'Chrome 11 CB3-115', 'Chrome 15 CB5-122', 'xx10923', '13 EL1')
grep(pattern= 'Chrome', x= str)
grep(pattern='Chrome',x=str,value=TRUE)
grepl(pattern='Chrome',x=str)
sub('C','C2',str)
gsub('C','C2',str)

regexpr("1", str)

attr(regexpr("11",str),'match.length')

gregexpr("1", str[1:2])
strsplit(str,split='-')



paste('a','b',sep=',')
str = c('a','b','c')
paste(str,collapse = ',')



str=c("Cote d'Ivoire\nU.S.")
cat(str)
print(str)


str=c('Firefox Setup Stub 46.0.1.exe', "Efficcess Free-5.21.0.520-win32", 'Adobe_Flash_Player_21.0.0.242_azo.exe')
inx= regexpr('[0-9]{2}\\.[0-9]', str)
regmatches(x=str,m)
