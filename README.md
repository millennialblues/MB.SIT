###################################################
#Millennial Blues SIT
#An educational tool to teach myself R & quant investing strategies
#based on work by The Systematic Investor
#TSI Blog 1: https://systematicinvestor.wordpress.com/
#TSI Blog 2: https://systematicinvestor.github.io/
#https://github.com/systematicinvestor/SIT
#
#########
#rights: 
########
#first see derivative rights for the SIT toolkit by TSI
#Millennial Blues's rights: open source & purely for individuals or education,
#commercialization of the work, underlying, or its derivatives is strictly prohibited in any kind
#
#disclaimer: the models are incomplete and often do not include essential elements
#such as, taxes, fund management fees, trading costs, etc.
#use at your own risk, and only after doing your own due diligence on how
#all calculations are generated
####################################################

#to make SIT automatically updated & deployable to Shiny apps,
#DO NOT use the install methods mentioned on SIT blog, instead run:

install.packages("devtools")
devtools::install_github('systematicinvestor/SIT.date')
devtools::install_github('systematicinvestor/pkg/')

#and to load SIT, make sure to include
library(SIT)

#I also suggest downloading a local copy of SIT, which includes Shiny apps and more
https://github.com/systematicinvestor/SIT


