#Job Guarantee MODEL 1.3 ( = built upon MINSKY MODEL i.e. MALCOLM Model 40 & Godin, 2013;2014)

#Made by GTY, 27th March 2021

#with trade equations

############################################################################

#STEP 1: Clear the workspace and define the number of periods and scenarios

#Clear all
rm(list=ls(all=TRUE))

#Number of periods
nPeriods = 100

#Number of scenarios
nScenarios=6

#Number of Montecarlo simulations (for stochastic components)
#mc = 1000

############################################################################

# STEP 2: SELECT MODEL TYPE

type <- 0   #Note: 0 = conventional investment function and exogenous interest rate on loans
#      1 = Minsky investment function and leverage-based interest rate on loans 

############################################################################

#STEP 3: 

#Variables
af=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Amortization funds
af_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Amortization funds e sector
af_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Amortization funds c sector
af_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Amortization funds k sector
af_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Amortization funds k sector
c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Total demand for consumption
c_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Total demand for energy consumption sector k
c_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Total demand for energy consumption sector c
c_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Total demand for energy consumption government
c_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Total demand for energy consumption households
cgov=matrix(data=0,nrow=nScenarios,ncol=nPeriods)             #Demand for public goods
da=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Depreciation allowances
da_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Depreciation allowances sector e
da_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Depreciation allowances sector c
da_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Depreciation allowances sector k
da_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Depreciation allowances sector k
k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Stock of capital 
k_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Stock of capital sector e
k_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Stock of capital sector c
k_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Stock of capital sector k
k_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                #Stock of capital sector k
kt=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Target stock of capital
kt_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Target stock of capital sector e
kt_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Target stock of capital sector e
kt_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Target stock of capital sector c
kt_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Target stock of capital sector k
lf=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Demand for bank loans 
ls=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Supply of bank loans 
lf_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Demand for bank loans sector e
ls_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Supply of bank loans sector e
lf_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Demand for bank loans sector k
ls_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Supply of bank loans sector k
lf_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Demand for bank loans sector c
ls_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Supply of bank loans sector c
id=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Investment
id_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Investment sector e
id_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Investment sector c
id_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Investment sector k
id_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Investment sector k
m1h=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Cheque deposits held by households
m1s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Supply of cheque deposits
m2h=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Saving deposits held by households
m2s=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Supply of saving deposits
nd=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour demand
ns=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour supply
nd_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour demand sector e
ns_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour supply sector e
nd_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour demand sector k
ns_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour supply sector k
nd_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour demand sector c
ns_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Labour supply sector c
nn=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Workers who do not have a job in the private sector
prf=matrix(data=1,nrow=nScenarios,ncol=nPeriods)              #Labour productivity in private sector
prf_e=matrix(data=2,nrow=nScenarios,ncol=nPeriods)              #Labour productivity in private sector
prf_c=matrix(data=0.5,nrow=nScenarios,ncol=nPeriods)              #Labour productivity in private sector
prf_k=matrix(data=0.5,nrow=nScenarios,ncol=nPeriods)              #Labour productivity in private sector
pre=matrix(data=2,nrow=nScenarios,ncol=nPeriods)              #Energy productivity
prg=matrix(data=0.75,nrow=nScenarios,ncol=nPeriods)           #Labour productivity in government sector
r_star=matrix(data=0.0145,nrow=nScenarios,ncol=nPeriods)        #Policy rate
omega1=0.01                                                   #PC coefficient: speed of adjustment of un to nun
nun=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Non-inflationary rate of unemployment
w=matrix(data=0.86,nrow=nScenarios,ncol=nPeriods)             #Target wage rate
wb=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Wage bill
w_e=matrix(data=0.86,nrow=nScenarios,ncol=nPeriods)             #Target wage rate sector e
wb_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Wage bill sector e
w_k=matrix(data=0.86,nrow=nScenarios,ncol=nPeriods)             #Target wage rate sector k
wb_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Wage bill sector k
w_c=matrix(data=0.86,nrow=nScenarios,ncol=nPeriods)             #Target wage rate sector c
wb_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Wage bill sector c
y=matrix(data=40,nrow=nScenarios,ncol=nPeriods)               #Total income
y_e=matrix(data=40,nrow=nScenarios,ncol=nPeriods)               #Total income sector e
y_c=matrix(data=40,nrow=nScenarios,ncol=nPeriods)               #Total income sector c
y_k=matrix(data=40,nrow=nScenarios,ncol=nPeriods)               #Total income sector k
y_g=matrix(data=40,nrow=nScenarios,ncol=nPeriods)               #Total income sector k
yd=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Disposal income of households
ff=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Profits of firms
ff_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Profits of firms sector e
ff_g=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Profits of firms sector e
ff_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Profits of firms sector c
ff_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)               #Profits of firms sector k
fuf=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Undistributed firms profits (retained profits)
fuf_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Undistributed firms profits sector e (retained profits)
fuf_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Undistributed firms profits sector c (retained profits)
fuf_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Undistributed firms profits sector k (retained profits)
fdf=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Distributed firms profits (dividends)
fdf_e=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Distributed firms profits sector e (dividends)
fdf_c=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Distributed firms profits sector c (dividends)
fdf_k=matrix(data=0,nrow=nScenarios,ncol=nPeriods)              #Distributed firms profits sector k (dividends)
tb = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Trade balance 
im = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Imports 
ex = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Exports 
y_f = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Foreign income 
g_f = matrix(data=0.03,nrow=nScenarios,ncol=nPeriods)           #Foreign income growth
g_s = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Exogenous growth 
xr = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Exchange rate (Nominal) 

############################################################################

#STEP 4: Set values for parameters and exogenous variables 
#Parameters
alpha10=matrix(data=0.75,nrow=nScenarios,ncol=nPeriods)     #Autonomous component of propensity to consume
alpha11=matrix(data=2,nrow=nScenarios,ncol=nPeriods)        #Sensitivity of propensity to consume to interest rate
#alpha11=matrix(data=0.075,nrow=nScenarios,ncol=nPeriods)   #Sensitivity of propensity to consume to distribution (alternative formulation) 
alpha12=matrix(data=0.05,nrow=nScenarios,ncol=nPeriods)     #Sensitivity of propensity to consume to unemployment rate 
alpha1=matrix(data=0.75,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of income 
alpha2=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of wealth: cash
alpha3=matrix(data=0.10,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of wealth: cheque deposits
alpha4=matrix(data=0.05,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of wealth: saving deposits
alpha5=matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of wealth: bills
alpha6=matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)      #Propensity to consume out of wealth: shares and other firms' securities
delta=matrix(data=0.1,nrow=nScenarios,ncol=nPeriods)        #Depreciation rate
gamma=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)       #Reaction speed of adjustment of capital to its target value
gamma_c=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)       #Reaction speed of adjustment of capital to its target value sector c
gamma_g=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)       #Reaction speed of adjustment of capital to its target value sector c
gamma_k=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)       #Reaction speed of adjustment of capital to its target value sector k
gamma_e=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)       #Reaction speed of adjustment of capital to its target value sector e
kappa=matrix(data=0.05,nrow=nScenarios,ncol=nPeriods)          #Capital-Output ratio
kappa_c=matrix(data=0.7,nrow=nScenarios,ncol=nPeriods)          #Capital-Output ratio sector c
kappa_e=matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Capital-Output ratio sector e
kappa_k=matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Capital-Output ratio sector k
kappa_g=matrix(data=0.5,nrow=nScenarios,ncol=nPeriods)          #Capital-Output ratio sector k
kappa0=0.035                                                #Autonomous capital-output ratio
kappa0_e=0.035                                                #Autonomous capital-output ratio sector e
kappa0_c=0.035                                                #Autonomous capital-output ratio sector c
kappa0_k=0.035                                                #Autonomous capital-output ratio sector k
kappa1=0.1                                                  #Sensitivity of capital-output ratio to Tobin q
kappa1_e=0.8                                                  #Sensitivity of capital-output ratio to Tobin q sector e
kappa1_c=0.8                                                  #Sensitivity of capital-output ratio to Tobin q sector c
kappa1_k=0.8                                                  #Sensitivity of capital-output ratio to Tobin q sector k

#Additional parameters and variables
theta = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)     #Profit retention rate
theta_c = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)     #Profit retention rate sector c
theta_k = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)     #Profit retention rate sector k
theta_e = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)     #Profit retention rate sector e
tau0 = matrix(data=0,nrow=nScenarios,ncol=nPeriods)         #Autonomous component of tax revenue (shock)
tau1 = matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)       #Tax rate on labour income
tau2 = matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)       #Tax rate on capital income
tau3 = matrix(data=1/200,nrow=nScenarios,ncol=nPeriods)     #Tax revenue rate on wealth
tau4 = matrix(data=2,nrow=nScenarios,ncol=nPeriods)         #Other transfers 
tau5 = matrix(data=5,nrow=nScenarios,ncol=nPeriods)         #Unemployment benefits (relative to unemployment rate) 
tax = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Total tax revenue
gov = matrix(data=tax,nrow=nScenarios,ncol=nPeriods)        #Government spending
gov_c = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Government spending
gov_k = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Government spending
gov_e = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Government spending
gov_g = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Government spending
def = matrix(data=tax,nrow=nScenarios,ncol=nPeriods)        #Government deficit
fb = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Bank profits
sigma0 = matrix(data=0.1,nrow=nScenarios,ncol=nPeriods)       #Autonomout component of government spending
sigma1 = matrix(data=0.1,nrow=nScenarios,ncol=nPeriods)    #Dependent component of government spending
sigma2 = matrix(data=0.1,nrow=nScenarios,ncol=nPeriods)    #Dependent component of government spending on energy
sigma3 = matrix(data=0.055,nrow=nScenarios,ncol=nPeriods)    #Dependent component of consumption spending on energy
bs = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Bills issued by the Treasury
vh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Household wealth
bh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Household holdings of bills
lambda10 = 0.1 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of bills
lambda11 = 0.2 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of bills
lambda12 = -0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of bills
lambda13 = -0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of bils
lambda14 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of bills
lambda15 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of bills
lambda16 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of bills
lambda20 = 0.40 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of cheque deposits 
lambda21 = -0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of cheque deposits
lambda22 = -0.1 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of cheque deposits
lambda23 = 0.2 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of cheque deposits
lambda24 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of cheque deposits
lambda25 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of cheque deposits
lambda26 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of cheque deposits
lambda30 = 0.033333333 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of c firms' securities 
lambda31 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of c firms' securities
lambda32 = 0 #-0.1 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of c firms' securities
lambda33 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of c firms' securities
lambda34 = 0 #0.01 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of c firms' securities
lambda35 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of c firms' securities
lambda36 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of c firms' securities
lambda40 = 0.033333333 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of k firms' securities 
lambda41 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of k firms' securities
lambda42 = 0 #-0.1 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of k firms' securities
lambda43 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of k firms' securities
lambda44 = 0 #0.01 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of k firms' securities
lambda45 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of k firms' securities
lambda46 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of k firms' securities
lambda50 = 0.033333333 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of e firms' securities 
lambda51 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of e firms' securities
lambda52 = 0 #-0.1 #matrix(data=0.2,nrow=nScenarios,ncol=nPeriods)   #Parameter in portfolio equation of e firms' securities
lambda53 = 0 #-0.1 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of e firms' securities
lambda54 = 0 #0.01 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation of e firms' securities
lambda55 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation e firms' securities
lambda56 = 0 #matrix(data=-0.1,nrow=nScenarios,ncol=nPeriods)  #Parameter in portfolio equation e firms' securities
lambdac = 0.18                                             #Cash to consumption ratio
bcb = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #CB holdings of bills 
hs = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Supply of cash
hf = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Supply of foreign reserves
as = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Supply of advances from CB (reserves if < 0)
ad = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Demand for advances (reserves if < 0)
hh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Household holdings of cash 
mub0 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)      #Coefficient of bills' return rate 
mub1 = matrix(data=0.0025,nrow=nScenarios,ncol=nPeriods)    #Coefficient of bills' return rate 
mub = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Mark-up: bills' return rate 
mul = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)       #Mark-up: loans' interest rate
mul_c = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)       #Mark-up: loans' interest rate sector c
mul_e = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)       #Mark-up: loans' interest rate sector e
mul_k = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)       #Mark-up: loans' interest rate sector k
mul0 = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)      #Coefficient of loans' interest rate
mul1 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)      #Coefficient of loans' interest rate
mum = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)       #Mark-up: saving deposits' return rate 
mua = matrix(data=0.005,nrow=nScenarios,ncol=nPeriods)      #Mark-up: CB advances' return rate 
muh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Mark-up: reserves' return rate 
rb = matrix(data=r_star+mub,nrow=nScenarios,ncol=nPeriods)  #Return rate on bills 
rl=matrix(data=r_star+mul,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on banks loans
rl_e=matrix(data=r_star+mul_e,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on banks loans sector e
rl_c=matrix(data=r_star+mul_c,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on banks loans sector c
rl_k=matrix(data=r_star+mul_k,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on banks loans sector k
rm=matrix(data=r_star+mum,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on saving deposits
ra=matrix(data=r_star+mua,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on CB advances
rh=matrix(data=r_star+muh,nrow=nScenarios,ncol=nPeriods)    #Rate of interests on reserves
fcb = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Central bank profit
hbd = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Reserve requirement: demand
hbs = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Reserve requirement: supply
rho1 = matrix(data=0.025,nrow=nScenarios,ncol=nPeriods)     #Reserves to cheque deposits parameter
rho2 = matrix(data=0.005,nrow=nScenarios,ncol=nPeriods)     #Reserves to saving deposits parameter
bb = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Bills held by commercial banks
bb_not = matrix(data=0,nrow=nScenarios,ncol=nPeriods)       #Notional amount of bills held by banks
nu = 0.2 #matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Speed of adjustment of labour supply to labour demand
gl = 0.03 #matrix(data=0,nrow=nScenarios,ncol=nPeriods)     #Structural rate of growth of labour force
p = matrix(data=1,nrow=nScenarios,ncol=nPeriods)            #General price level
p_c = matrix(data=1,nrow=nScenarios,ncol=nPeriods)            #General price level sector c
p_k = matrix(data=1,nrow=nScenarios,ncol=nPeriods)            #General price level sector k
p_e = matrix(data=1,nrow=nScenarios,ncol=nPeriods)            #General price level sector e
pf = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Unit price of private output
pg = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Unit price of government output
ep = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Expected price level
ep_c = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Expected price level sector c
ep_k = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Expected price level sector k
ep_e = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Expected price level sector e
mup = matrix(data=0.163,nrow=nScenarios,ncol=nPeriods)      #Mark-up over labour cost
un = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Unemployment rate
psi1 = 0 #matrix(data=0.01,nrow=nScenarios,ncol=nPeriods) #Coefficient of price expectations function
psi2 = 0.01 #matrix(data=0.01,nrow=nScenarios,ncol=nPeriods) #Coefficient of price expectations function
pt=matrix(data=1,nrow=nScenarios,ncol=nPeriods)             #Target or natural price level
bpr = matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Share of bills purchased by private sector
eh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Firms' securities held by househoulds
eh_c = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Firms' securities held by househoulds sector c
eh_k = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Firms' securities held by househoulds sector k
eh_e = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Firms' securities held by househoulds sector e
esr = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities issued by firms
ehr_e = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities held by households sector e
esr_e = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities issued by firms sector e
ehr_c = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities held by households sector c
esr_c = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities issued by firms sector c
ehr_k = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities held by households sector k
esr_k = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities issued by firms sector k
ehr = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Number of securities held by households
#mue = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)      #Mark-up: return rate on firms' securities
re = matrix(data=r_star,nrow=nScenarios,ncol=nPeriods)      #Return rate on firms' securities
re_c = matrix(data=r_star,nrow=nScenarios,ncol=nPeriods)      #Return rate on firms' securities sector c
re_k = matrix(data=r_star,nrow=nScenarios,ncol=nPeriods)      #Return rate on firms' securities sector k
re_e = matrix(data=r_star,nrow=nScenarios,ncol=nPeriods)      #Return rate on firms' securities sector e
pe = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Unit price of firms' securities
pe_e = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Unit price of firms' securities sector e
pe_k = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Unit price of firms' securities sector k
pe_c = matrix(data=1,nrow=nScenarios,ncol=nPeriods)           #Unit price of firms' securities sector c
chi = 0.001 #matrix(data=0,nrow=nScenarios,ncol=nPeriods)   #Target percentage of investment to be funded by share issues
cg = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Capital gains on firms' securities
pi = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Inflation rate
epi = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Expected inflation rate: regressive expectations
epi_a = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Expected inflation rate: adaptive expectations
epi_r = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Expected inflation rate: rational expectations
pit = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Target or normal inflation rate
vareps = matrix(data=0,nrow=nScenarios,ncol=nPeriods)       #Target share of bills held by CB
vareps0 = matrix(data=1,nrow=nScenarios,ncol=nPeriods) #1     #Autonomous component of target share of bills held by CB
vareps1 = matrix(data=0.1,nrow=nScenarios,ncol=nPeriods)#0.65  #Sensitivity of target share of bills to interest rate
beta = matrix(data=0.5,nrow=nScenarios,ncol=nPeriods)       #Share of notional bills held as bills by banks
hbd_star = matrix(data=0,nrow=nScenarios,ncol=nPeriods)     #Extra reserves demanded by banks
hbs_star = matrix(data=0,nrow=nScenarios,ncol=nPeriods)     #Extra reserves supplied by the CB
hbs_tot = matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Total reserves
Omega = matrix(data=0,nrow=nScenarios,ncol=nPeriods)        #Wage share to total income
lh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Mortgages to households
mulh = matrix(data=0.02,nrow=nScenarios,ncol=nPeriods)      #Mark-up: interest rate on mortgages
rlh = matrix(data=r_star+mulh,nrow=nScenarios,ncol=nPeriods)#Interest rate on mortgages
phi = matrix(data=0.03,nrow=nScenarios,ncol=nPeriods)       #Mortgages to disposable income ratio
rep = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)       #Repayment rate on mortgages
nvh = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Household net wealth
tr = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Total transfers 
tr_g = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #Total transfers 
m0 = matrix(data=-3.1,nrow=nScenarios,ncol=nPeriods)           #Parameter in import equation 
m1 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in import equation 
m2 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in import equation 
m3 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in import equation 
x0 = matrix(data=-3.1,nrow=nScenarios,ncol=nPeriods)           #Parameter in export equation 
x1 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in export equation 
x2 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in export equation 
x3 = matrix(data=0.01,nrow=nScenarios,ncol=nPeriods)           #Parameter in export equation 
epar = (1/2)*(1/3)
fpar = (1)*(1/3)
gpar = (1/0.75)*(1/3)

#Minsky variables and indices
q = matrix(data=0.8,nrow=nScenarios,ncol=nPeriods)          #Valuation ratio (Tobin q) 
q_e = matrix(data=0.8,nrow=nScenarios,ncol=nPeriods)          #Valuation ratio (Tobin q) sector e
q_c = matrix(data=0.8,nrow=nScenarios,ncol=nPeriods)          #Valuation ratio (Tobin q) sector c
q_k = matrix(data=0.8,nrow=nScenarios,ncol=nPeriods)          #Valuation ratio (Tobin q) sector k
lev = matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Leverage ratio of firms 
lev_e = matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Leverage ratio of firms sector e
lev_c = matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Leverage ratio of firms sector c
lev_k = matrix(data=1,nrow=nScenarios,ncol=nPeriods)          #Leverage ratio of firms sector k
rhog = matrix(data=0.3,nrow=nScenarios,ncol=nPeriods)      #Goverment wage rate to private sector wage rate
wg = matrix(data=rhog*w,nrow=nScenarios,ncol=nPeriods)      #Wage rate paid by the government
ng = matrix(data=0,nrow=nScenarios,ncol=nPeriods)           #People hired by the government (job guarantee)
wbg = matrix(data=0,nrow=nScenarios,ncol=nPeriods)          #Job guarantee wage bill
wbg_perc = matrix(data=0.015,nrow=nScenarios,ncol=nPeriods) #Job guarantee spending to GDP
type_jg = matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Job guarantee trigger (for experiments)
type_un = matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Unemployment trigger (Note: 0 = unemployment rate considers public employees; 1 = unemployment rate does not consider public employees, hence no impact of on wage curve) 
type_gj = matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Sector g trigger (for experiments)  
type_gjg = matrix(data=0,nrow=nScenarios,ncol=nPeriods)      #Government Expenditure trigger (for experiments)  
def_ratio = matrix(data=0,nrow=nScenarios,ncol=nPeriods)    #Goverment deficit to GDP ratio
debt_ratio = matrix(data=0,nrow=nScenarios,ncol=nPeriods)    #Goverment debt to GDP ratio
gammag = 1                                                #Speed of adjustment of JG size to market conditions
alphag = matrix(data=0.015,nrow=nScenarios,ncol=nPeriods)   #Public goods to total consumption


############################################################################

# STEP 5: MODEL AND ITERATIONS

#Select scenarios
for (j in 1:nScenarios){
  
  #Define the time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:1000){
      
      #Define alternative scenarios
      
      #Job guarantee plan with exogenous wage rate
      if (i>=60 && j==2){
        type_jg[2,i]=1
        tau5[2,i] = 0
        #id_c[2,60]=0
        #id_k[2,60]=0
        #id_e[2,60]=0
      }    
      
      #Positive shock to government spending to be compared with JG plan
      if (i>=60 && j==3){
        tau4[3,i] = 2+nn[3,i]*0.86*0.3#0.015*y[3,59]            #[Note: JG experiment uses: wbg[j,i]]
        #sigma2[3,i]=(1-tau4[3,i]*0.01)*sigma2[3,i-1]  #endogenous reduction of energy consumption by government
        #sigma3[3,i]=(1-tau4[3,i]*0.01)*sigma3[3,i-1]  #endogenous reduction of energy consumption by households
        #pre[3,i] = (1+tau4[3,i]*0.001)*pre[3,i-1]
        tau5[3,i] = 0
        #type_gjg[3,i] = 1
        #id_c[3,60]=0
        #id_k[3,60]=0
        #id_e[3,60]=0
      }  
      
      #Green Job Guarantee plan (Godin-like)
      if (i>=60 && j==4){
        type_jg[4,i]=1
        sigma2[4,i]=(1-wbg[4,i]*0.01)*sigma2[4,i-1]  #endogenous reduction of energy consumption by government
        #sigma1[4,i]=(1+wbg[4,i]*0.01)*sigma1[4,i-1]  
        #sigma0[4,i]=(1+wbg[4,i]*0.01)*sigma0[4,i-1]  
        sigma3[4,i]=(1-wbg[4,i]*0.01)*sigma3[4,i-1]  #endogenous reduction of energy consumption by households
        #gamma_e[4,i]=(1-un[4,i]*0.05)*gamma_e[4,i-1]  
        #x2[4,i]=exp(-1+epar*pre[4,i]+fpar*prf[4,i]+gpar*prg[4,i])*m2[4,i]
        pre[4,i] = (1+wbg[4,i]*0.001)*pre[4,i-1]     #endogenous increase of energy productivity
        #prg[4,i] = (1+ng[4,i]*0.001)*prg[4,i-1]
        #prf[4,i] = (1/0.75)*prg[4,i-1]
        #id_c[4,60]=0
        #id_k[4,60]=0
        #id_e[4,60]=0
        #kt_g[4,i] = kappa_g[4,i]*y_g[4,i]*ep[4,i]/p[4,i]                          #Target capital stock g sector
        #da_g[4,i] = delta[4,i]*k_g[4,i-1]                                       #Depreciation allowances g sector
        #af_g[4,i] = da_g[4,i]                                                   #Amortization funds g sector
        #id_g[4,i] = gamma_g[4,i]*(kt_g[4,i] - k_g[4,i-1]) + da_g[4,i]                 #Demand for investment: conventional g sector
        #k_g[4,i] = k_g[4,i-1] + id_g[4,i] - da_g[4,i]                               #Accumulation of capital - Investment behaviour g sector
        #y_k[4,i] = id_e[4,i] + id_c[4,i] + id_k[4,i] + id_g[4,i] + (sigma1[4,i])*(y_k[4,i-1]) #Revenues of the k sector
        #y_g[4,i] = c_g[4,i] + gov_g[4,i] + kappa[4,i]*(y_c[4,i] / 2) + kappa[4,i]*(y_k[4,i] / 2 ) #-wbg[4,i]Revenues of the k sector
        #y[4,i] = y_c[4,i] + y_k[4,i] + y_e[4,i] + y_g[4,i]  + tb[4,i]                               #+ y_g[4,i]+ gov[j,i]National income identity (GDP)
        #c[4,i] = alpha1[4,i]*yd[4,i]*ep[4,i]/p[4,i]+alpha2[4,i]*hh[4,i-1]+alpha3[4,i]*m1h[4,i-1]+alpha4[4,i]*m2h[4,i-1]+alpha5[4,i]*bh[4,i-1]+alpha6[4,i]*eh[4,i-1] - sigma3[4,i]*(c[4,i]) -c_g[4,i] #Household total consumption
        #c_g[4,i] = (0.055-sigma3[4,i])*(c[4,i]) 
        #gov[4,i] = sigma0[4,i]*(y_c[4,i-1]) + (sigma1[4,i])*(y_k[4,i-1]) + sigma2[4,i]*(y_e[4,i-1]) + (0.1-sigma2[4,i])*(y_g[4,i-1])                        #Government spending: purchase of goods and services
        #ff_g[4,i] = y_g[4,i]  - af_g[4,i]   #- cgov[j,i]#Firms profits (residual) g sector
        #tr_g[4,i] = id_g[4,i] - af_g[4,i] - ff_g[4,i]
        #ff_c[4,i] = y_c[4,i] - rl_c[4,i-1]*lf_c[4,i-1] - af_c[4,i] - wb_c[4,i]  - (1-kappa[4,i])*c_c[4,i] - kappa[4,i]*(y_c[4,i] / 3) #- cgov[j,i]#Firms profits (residual) c sector
        #ff_k[4,i] = y_k[4,i] - rl_k[4,i-1]*lf_k[4,i-1] - af_k[4,i] - wb_k[4,i]  - (1-kappa[4,i])*c_k[4,i] - kappa[4,i]*(y_k[4,i] / 3 )#- cgov[j,i]#Firms profits (residual)  k sector
        #kappa[4,i]=(1+wbg[4,i]*0.01)*kappa[4,i-1]  #endogenous reduction of energy consumption by households
        #y_c[4,i] = c[4,i] + sigma0[4,i]*(y_c[4,i-1])#+ tb[j,i]                               #Revenues of the c sector
        #tr[4,i] =  (y[4,59] - y[4,i])                            #Transfers (including unemployment benefits)
        #type_gjg[4,i] = 1
        #if (y[4,59]>y[4,i-1]){
        #type_gjg[4,i] = 1
        
        #}
        tau5[4,i] = 0
      }
      
      #Positive shock to government spending to be compared with GJG plan
      if (i>=60 && j==5){
        tau4[5,i] = 2+nn[5,i]*0.86*0.3#0.015*y[3,59]            #[Note: JG experiment uses: wbg[j,i]]
        sigma2[5,i]=(1-tau4[5,i]*0.01)*sigma2[5,i-1]  #endogenous reduction of energy consumption by government
        sigma3[5,i]=(1-tau4[5,i]*0.01)*sigma3[5,i-1]  #endogenous reduction of energy consumption by households
        pre[5,i] = (1+tau4[5,i]*0.001)*pre[5,i-1]     #endogenous increase of energy productivity
        tau5[5,i] = 0
        #type_gjg[3,i] = 1
        #id_c[3,60]=0
        #id_k[3,60]=0
        #id_e[3,60]=0
      }
      
      #Green Job Guarantee plan (Godin-like) with constant public investment
      if (i>=60 && j==6){
        type_jg[6,i]=1
        sigma2[6,i]=(1-wbg[6,i]*0.01)*sigma2[6,i-1]  #endogenous reduction of energy consumption by government
        sigma3[6,i]=(1-wbg[6,i]*0.01)*sigma3[6,i-1]  #endogenous reduction of energy consumption by households
        #sigma2[6,i]=(1-tau4[6,i]*0.01)*sigma2[6,i-1]  #endogenous reduction of energy consumption by government
        #sigma3[6,i]=(1-tau4[6,i]*0.01)*sigma3[6,i-1]  #endogenous reduction of energy consumption by households
        #x2[6,i]=exp(-1+epar*pre[6,i]+fpar*prf[6,i]+gpar*prg[6,i])*m2[6,i]
        #pre[6,i] = (1+tau4[6,i]*0.001)*pre[6,i-1]
        pre[6,i] = (1+wbg[6,i]*0.001)*pre[6,i-1]
        #prg[6,i] = (1+un[6,i]*0.02)*prg[6,i-1]
        #prf[6,i] = (1/0.75)*prg[6,i-1]
        #tau4[6,i] = 2+nn[6,i]*0.86*0.3#0.015*y[3,59]            #[Note: JG experiment uses: wbg[j,i]]
        tau5[6,i] = 0
        type_gjg[6,i] = 1
      }
      
      
      #MODEL EQUATIONS      
      
      #Non-financial firms
      if (type_gj[j,i]==1) {y[j,i] = y_c[j,i] + y_k[j,i] + y_e[j,i] + y_g[j,i]  }      #National income identity (GDP)
      else{y[j,i] = y_c[j,i] + y_k[j,i] + y_e[j,i]  }                                 #National income identity (GDP)
      
      
      y_c[j,i] = c[j,i] + gov_c[j,i] + tb[j,i]                              #Revenues of the c sector
      c_c[j,i] = y_c[j,i] / pre[j,i]                               #Energy consumption of the c sector
      c_k[j,i] = y_k[j,i] / pre[j,i]                                #Energy consumption of the k sector
      c_e[j,i] = sigma3[j,i]*(c[j,i])                               #Energy consumption of households
      if (type_gj[j,i]==1) {y_e[j,i] = (1-kappa[j,i])*c_c[j,i] + (1-kappa[j,i])*c_k[j,i]  +  gov_e[j,i]   +  c_e[j,i]}      #Revenues of the e sector
      else{y_e[j,i] = c_c[j,i] + c_k[j,i]  +  gov_e[j,i]   +  c_e[j,i]}                                 
      
      
      if (type_gj[j,i]==1) {y_k[j,i] = id_e[j,i] + id_c[j,i] + id_k[j,i] + id_g[j,i] + gov_k[j,i]}      #Revenues of the k sector
      else{y_k[j,i] = id_e[j,i] + id_c[j,i] + id_k[j,i] + gov_k[j,i]}                                 
      
      
      id[j,i] =  y_k[j,i]                            #Gross investments
      fdf[j,i] = fdf_e[j,i] + fdf_c[j,i] + fdf_k[j,i]    #Aggregate Firms profits (residual) 
      kt_e[j,i] = kappa_e[j,i]*y_e[j,i]*ep[j,i]/p[j,i]                          #Target capital stock e sector
      da_e[j,i] = delta[j,i]*k_e[j,i-1]                                       #Depreciation allowances e sector
      af_e[j,i] = da_e[j,i]                                                   #Amortization funds e sector
      id_e[j,i] = gamma_e[j,i]*(kt_e[j,i] - k_e[j,i-1]) + da_e[j,i]                 #Demand for investment: conventional e sector
      if (type==0){kappa_e[j,i] = 1}  
      else {kappa_e[j,i] = kappa0_e + kappa1_e*q_e[j,i-1]}                        #Capital-output ratio e sector
      k_e[j,i] = k_e[j,i-1] + id_e[j,i] - da_e[j,i]                               #Accumulation of capital - Investment behaviour e sector
      ff_e[j,i] = y_e[j,i] - rl_e[j,i-1]*lf_e[j,i-1] - af_e[j,i] - wb_e[j,i] #- cgov[j,i] #Firms profits (residual) e sector
      fdf_e[j,i] = (1-theta_e[j,i])*ff_e[j,i]                                   #Distributed profits e sector
      fuf_e[j,i] = theta_e[j,i]*ff_e[j,i]                                       #Retained profits e sector
      lf_e[j,i] = lf_e[j,i-1] + id_e[j,i] - af_e[j,i] - fuf_e[j,i] - (esr_e[j,i]-esr_e[j,i-1])*pe_e[j,i]  #Demand for bank loans e sector
      if (i<5) {esr_e[j,i] = 10}
      else {esr_e[j,i] = esr_e[j,i-1] + chi*id_e[j,i-1]/pe_e[j,i-1]}              #Number of securities (equity, shares and bonds) issued by the firms e sector
      kt_k[j,i] = kappa_k[j,i]*y_k[j,i]*ep[j,i]/p[j,i]                          #Target capital stock k sector
      da_k[j,i] = delta[j,i]*k_k[j,i-1]                                       #Depreciation allowances k sector
      af_k[j,i] = da_k[j,i]                                                   #Amortization funds k sector
      id_k[j,i] = gamma_k[j,i]*(kt_k[j,i] - k_k[j,i-1]) + da_k[j,i]                 #Demand for investment: conventional k sector
      if (type==0){kappa_k[j,i] = 1}  
      else {kappa_k[j,i] = kappa0_k + kappa1_k*q_k[j,i-1]}                        #Capital-output ratio k sector
      k_k[j,i] = k_k[j,i-1] + id_k[j,i] - da_k[j,i]                               #Accumulation of capital - Investment behaviour k sector
      if (type_gj[j,i]==1) {ff_k[j,i] = y_k[j,i] - rl_k[j,i-1]*lf_k[j,i-1] - af_k[j,i] - wb_k[j,i]  - (1-kappa[j,i])*c_k[j,i] - kappa[j,i]*(y_k[j,i] / 2 )}      
      else{ff_k[j,i] = y_k[j,i] - rl_k[j,i-1]*lf_k[j,i-1] - af_k[j,i] - wb_k[j,i]  - c_k[j,i]}          #- cgov[j,i]#Firms profits (residual)  k sector                       
      
      
      fdf_k[j,i] = (1-theta_k[j,i])*ff_k[j,i]                                   #Distributed profits k sector
      fuf_k[j,i] = theta_k[j,i]*ff_k[j,i]                                       #Retained profits k sector
      lf_k[j,i] = lf_k[j,i-1] + id_k[j,i] - af_k[j,i] - fuf_k[j,i] - (esr_k[j,i]-esr_k[j,i-1])*pe_k[j,i]  #Demand for bank loans k sector
      if (i<5) {esr_k[j,i] = 10}
      else {esr_k[j,i] = esr_k[j,i-1] + chi*id_k[j,i-1]/pe_k[j,i-1]}              #Number of securities (equity, shares and bonds) issued by the firms k sector
      kt_c[j,i] = kappa_c[j,i]*y_c[j,i]*ep[j,i]/p[j,i]                          #Target capital stock c sector
      da_c[j,i] = delta[j,i]*k_c[j,i-1]                                       #Depreciation allowances c sector
      af_c[j,i] = da_c[j,i]                                                   #Amortization funds c sector
      id_c[j,i] = gamma_c[j,i]*(kt_c[j,i] - k_c[j,i-1]) + da_c[j,i]                 #Demand for investment: conventional c sector
      if (type==0){kappa_c[j,i] = 1}  
      else {kappa_c[j,i] = kappa0_c + kappa1_c*q_c[j,i-1]}                        #Capital-output ratio c sector
      k_c[j,i] = k_c[j,i-1] + id_c[j,i] - da_c[j,i]                               #Accumulation of capital - Investment behaviour c sector
      if (type_gj[j,i]==1) {ff_c[j,i] = y_c[j,i] - rl_c[j,i-1]*lf_c[j,i-1] - af_c[j,i] - wb_c[j,i]  - (1-kappa[j,i])*c_c[j,i] - kappa[j,i]*(y_c[j,i] / 2)}      
      else{ff_c[j,i] = y_c[j,i] - rl_c[j,i-1]*lf_c[j,i-1] - af_c[j,i] - wb_c[j,i]  - c_c[j,i]}                                 #- cgov[j,i]#Firms profits (residual) c sector
      
      
      fdf_c[j,i] = (1-theta_c[j,i])*ff_c[j,i]                                   #Distributed profits c sector
      fuf_c[j,i] = theta_c[j,i]*ff_c[j,i]                                       #Retained profits c sector
      lf_c[j,i] = lf_c[j,i-1] + id_c[j,i] - af_c[j,i] - fuf_c[j,i] - (esr_c[j,i]-esr_c[j,i-1])*pe_c[j,i]  #Demand for bank loans c sector
      if (i<5) {esr_c[j,i] = 10}
      else {esr_c[j,i] = esr_c[j,i-1] + chi*id_c[j,i-1]/pe_c[j,i-1]}              #Number of securities (equity, shares and bonds) issued by the firms c sector
      
      #Households
      yd[j,i] = wb[j,i] + rm[j,i-1]*m2h[j,i-1] + rb[j,i-1]*bh[j,i-1] - (rep[j,i-1]+rlh[j,i-1])*lh[j,i-1] + fdf[j,i] + fb[j,i] + tr[j,i] - tax[j,i] + wbg[j,i] - cgov[j,i] #Disposable income
      if (type_gj[j,i]==1) {c[j,i] = alpha1[j,i]*yd[j,i]*ep[j,i]/p[j,i]+alpha2[j,i]*hh[j,i-1]+alpha3[j,i]*m1h[j,i-1]+alpha4[j,i]*m2h[j,i-1]+alpha5[j,i]*bh[j,i-1]+alpha6[j,i]*eh[j,i-1]-sigma3[j,i]*c[j,i]-c_g[j,i]}      #-c_g[j,i]Household total consumption
      else{c[j,i] = (alpha1[j,i]*yd[j,i]*ep[j,i]/p[j,i]+alpha2[j,i]*hh[j,i-1]+alpha3[j,i]*m1h[j,i-1]+alpha4[j,i]*m2h[j,i-1]+alpha5[j,i]*bh[j,i-1]+alpha6[j,i]*eh[j,i-1])/(1+sigma3[j,i])}                                 
      
      
      wb[j,i] = wb_e[j,i] + wb_c[j,i] + wb_k[j,i] 
      alpha1[j,i] = alpha10[j,i] - alpha11[j,i]*r_star[j,i-1] - alpha12[j,i]*un[j,i-1]      #Endogenous propensity to consume
      #alpha1[j,i] = alpha10[j,i] + alpha11[j,i]*Omega[j,i-1] - alpha12[j,i]*un[j,i-1]      #Endogenous propensity to consume (alternative formulation)
      nvh[j,i] = nvh[j,i-1] + yd[j,i] + cg[j,i] - c[j,i]                  #Household NET wealth
      vh[j,i] = nvh[j,i] + lh[j,i]                                        #Household GROSS wealth
      cg[j,i] = esr_c[j,i-1]*(pe_c[j,i] - pe_c[j,i-1]) + esr_k[j,i-1]*(pe_k[j,i] - pe_k[j,i-1]) + esr_e[j,i-1]*(pe_e[j,i] - pe_e[j,i-1])                         #Capital gains
      Omega[j,i]=(wb[j,i] + wbg[j,i])/y[j,i]                              #Wage share to total income
      lh[j,i]=lh[j,i-1]*(1-rep[j,i-1]) + phi[j,i]*yd[j,i]                 #Mortgages to households
      
      #Commercial banks
      lf[j,i] = lf_c[j,i] + lf_e[j,i] + lf_k[j,i]
      ls[j,i] = lf[j,i] + lh[j,i]                                         #Supply of bank credit (loans and mortgages)
      m1s[j,i] = m1h[j,i]                                                 #Supply of cheque deposits 
      m2s[j,i] = m2h[j,i]                                                 #Supply of saving deposits 
      fb[j,i] = rl_k[j,i-1]*lf_k[j,i-1] +rl_e[j,i-1]*lf_e[j,i-1] + rl_c[j,i-1]*lf_c[j,i-1] + (rlh[j,i-1]+rep[j,i-1])*lh[j,i-1] + rb[j,i-1]*bb[j,i-1] - rm[j,i-1]*m2s[j,i-1] - ra[j,i-1]*ad[j,i-1] + rh[j,i-1]*(hbd[j,i-1]+hbd_star[j,i-1])   #Bank profits
      bb_not[j,i] = m1s[j,i] + m2s[j,i] - ls[j,i] - hbd[j,i]              #Notional amount of bills held by banks
      if (bb_not[j,i]>0){bb[j,i] = bb_not[j,i]*beta[j,i]} else {bb[j,i] = 0} #Bills actually held by commercial banks
      if (bb_not[j,i]>0){hbd_star[j,i] = bb_not[j,i]*(1-beta[j,i])} else {hbd_star[j,i] = 0} #Extra reserves held by commercial banks
      if (bb_not[j,i]>0){ad[j,i] = 0} else {ad[j,i] = -bb_not[j,i]}       #Demand for advances
      
      #Government
      tax[j,i] = tau0[j,i] + tau1[j,i]*wb[j,i]+tau2[j,i]*(rm[j,i-1]*m2h[j,i-1]+rb[j,i-1]*bh[j,i-1]+fdf[j,i]+fb[j,i]) + tau3[j,i]*vh[j,i-1] #Total tax revenue
      if (type_gj[j,i]==1) {tr[j,i] = 0.25*(y[j,59] - y[j,i-1])}      #Transfers (including unemployment benefits)
      else{tr[j,i] = tau4[j,i]+tau5[j,i]*un[j,i-1]}                                 
      
      if (type_gj[j,i]==1) {gov[j,i] = gov_c[j,i] + gov_k[j,i] + gov_e[j,i]+gov_g[j,i]}      
      else{gov[j,i] = gov_c[j,i] + gov_k[j,i] + gov_e[j,i]}                                 #Government spending: purchase of goods and services
      
      
      
      if (type_gj[j,i]==1) {gov_c[j,i] = sigma0[j,i]*(y_c[j,i-1])}      
      else{gov_c[j,i] = sigma0[j,i]*(y_c[j,i-1])}                                 #Government spending: purchase of goods and services sector c
      
      if (type_gjg[j,i]==1) {gov_k[j,i] = sigma1[j,i]*(y_k[j,i-1])+(0.1-sigma2[j,i])*(y_e[j,i-1])}      
      else{gov_k[j,i] = sigma1[j,i]*(y_k[j,i-1])}                                 #Government spending: purchase of goods and services sector e
      
      if (type_gj[j,i]==1) {gov_e[j,i] = sigma2[j,i]*(y_e[j,i-1])}      
      else{gov_e[j,i] = sigma2[j,i]*(y_e[j,i-1])}                                 #Government spending: purchase of goods and services sector k
      
      if (type_gj[j,i]==1) {gov_g[j,i] = (0.1-sigma2[j,i])*(y_e[j,59])}      
      else{gov_g[j,i] = (0.1-sigma2[j,i])*(y_e[j,i-1])}                                 #Government spending: purchase of goods and services sector g
      
      
      
      
      
      #c_g[j,i] = (0.5-sigma2[j,i])*(y_e[j,i-1])                             #Energy consumption government
      
      #A) Job guarantee with exogenous wg/w and exogenous employees 
      
      if (type_jg[j,i]==1){
        
        if (ng[j,i]==0){wg[j,i]=0}
        else {wg[j,i] = rhog[j,i]*w[j,i]}                               #Wage rate paid under the JG programme
        
        nn[j,i] = ns[j,i] - nd[j,i]                                     #Workers who do not have a job in the private sector
        ng[j,i] = ng[j,i-1]+gammag*(nn[j,i]-ng[j,i-1])                  #Workers hired under JG programme
        wbg[j,i] = wg[j,i]*ng[j,i]                                #Total spending for the JG programme
        cgov[j,i] = min(alphag[j,i]*c[j,i],wbg[j,i])                    #Consumption of public goods
      }
      
      else {
        
        wg[j,i] = 0
        ng[j,i] = 0
        nn[j,i] = ns[j,i] - nd[j,i]
        wbg[j,i] = 0      
        cgov[j,i] = 0
        
      }    
      
      #B) Job guarantee with endogenous employees and exogenous wg/w (Scenario 5 and 6)
      
      if (type_jg[j,i]==2){
        
        wg[j,i] = rhog[j,i]*w[j,i]  
        wbg[j,i] = wbg_perc[j,i]*y[j,59]                                #Total spending for the JG programme  
        ng[j,i] = wbg[j,i]/wg[j,i]                                       #Workers hired under JG programme
        nn[j,i] = ns[j,i] - nd[j,i]                                     #Workers who do not have a job in the private sector  
        
        #if (ng[j,i]==0){wg[j,i]=0}
        #else {wg[j,i] = rhog[j,i]*w[j,i]}                               #Wage rate paid under the JG programme
        
        cgov[j,i] = alphag[j,i]*c[j,i]                                  #Consumption of public goods
        
      }
      
      if (type_gj[j,i]==1) {def[j,i] = gov[j,i] + tr[j,i] + tr_g[j,i] + rb[j,i-1]*bs[j,i-1] - tax[j,i] - fcb[j,i] + wbg[j,i]  - cgov[j,i]}      #Government deficit
      else{def[j,i] = gov[j,i] + tr[j,i] + rb[j,i-1]*bs[j,i-1] - tax[j,i] - fcb[j,i] + wbg[j,i] - cgov[j,i]}                                 
      
      
      
      bs[j,i] = bs[j,i-1] + def[j,i]                                      #Supply of bills
      
      #Portfolio decisions
      esr[j,i] = esr_c[j,i] + esr_k[j,i] + esr_e[j,i]   #Number of securities supplied to households
      ehr[j,i] = ehr_c[j,i] + ehr_k[j,i] + ehr_e[j,i]   #Number of securities held by households
      ehr_e[j,i] = esr_e[j,i]                                                 #Number of securities e held by households
      ehr_c[j,i] = esr_c[j,i]                                                 #Number of securities c held by households
      ehr_k[j,i] = esr_k[j,i]                                                 #Number of securities k held by households
      eh[j,i] = eh_c[j,i] + eh_e[j,i] + eh_k[j,i]                                          #Nominal amount of securities demanded by households
      eh_c[j,i] = ehr_c[j,i]*pe_c[j,i]                                          #Nominal amount of securities c demanded by households
      eh_k[j,i] = ehr_k[j,i]*pe_k[j,i]                                          #Nominal amount of securities k demanded by households
      eh_e[j,i] = ehr_e[j,i]*pe_e[j,i]                                          #Nominal amount of securities e demanded by households
      bh[j,i] = lambda10*vh[j,i-1] + lambda11*vh[j,i-1]*rb[j,i-1] + lambda12*vh[j,i-1]*rm[j,i-1] + lambda13*yd[j,i-1] + lambda14*vh[j,i-1]*re_c[j,i-1] + lambda15*vh[j,i-1]*re_e[j,i-1] + lambda16*vh[j,i-1]*re_k[j,i-1]#Demand for bills
      m1h[j,i] = lambda20*vh[j,i-1] + lambda21*vh[j,i-1]*rb[j,i-1] + lambda22*vh[j,i-1]*rm[j,i-1] + lambda23*yd[j,i-1] + lambda24*vh[j,i-1]*re_c[j,i-1] + lambda25*vh[j,i-1]*re_e[j,i-1] + lambda26*vh[j,i-1]*re_k[j,i-1]#Demand for cheque deposits
      pe_c[j,i] = (lambda30*vh[j,i-1] + lambda31*vh[j,i-1]*rb[j,i-1] + lambda32*vh[j,i-1]*rm[j,i-1] + lambda33*yd[j,i-1] + lambda34*vh[j,i-1]*re_c[j,i-1] + lambda35*vh[j,i-1]*re_e[j,i-1] + lambda36*vh[j,i-1]*re_k[j,i-1])/ehr_c[j,i]#Demand for c firms' securities (equity, shares and bonds)
      pe_k[j,i] = (lambda40*vh[j,i-1] + lambda41*vh[j,i-1]*rb[j,i-1] + lambda42*vh[j,i-1]*rm[j,i-1] + lambda43*yd[j,i-1] + lambda44*vh[j,i-1]*re_c[j,i-1] + lambda45*vh[j,i-1]*re_e[j,i-1] + lambda46*vh[j,i-1]*re_k[j,i-1])/ehr_k[j,i]#Demand for k firms' securities (equity, shares and bonds)
      pe_e[j,i] = (lambda50*vh[j,i-1] + lambda51*vh[j,i-1]*rb[j,i-1] + lambda52*vh[j,i-1]*rm[j,i-1] + lambda53*yd[j,i-1] + lambda54*vh[j,i-1]*re_c[j,i-1] + lambda55*vh[j,i-1]*re_e[j,i-1] + lambda56*vh[j,i-1]*re_k[j,i-1])/ehr_e[j,i]#Demand for e firms' securities (equity, shares and bonds)
      hh[j,i] = lambdac*c[j,i]*ep[j,i]/p[j,i]                             #Demanded stock of banknotes (cash)
      m2h[j,i] = vh[j,i] - hh[j,i] - m1h[j,i] - bh[j,i] - eh[j,i]         #Saving deposits held by households (buffer stock)
      
      #Central bank
      bcb[j,i] = bs[j,i] - bh[j,i] - bb[j,i]                              #CB holdings of bills
      hs[j,i] = bcb[j,i] + as[j,i] - (hbs[j,i] + hbs_star[j,i]) + hf[j,i]#*xr[j,i]          #Supply of cash
      as[j,i] = ad[j,i]                                                   #Supply of advances
      fcb[j,i] = rb[j,i-1]*bcb[j,i-1] + ra[j,i-1]*as[j,i-1] - rh[j,i-1]*(hbs[j,i-1]+hbs_star[j,i-1]) #Central bank profit (seigniorage)
      hbd[j,i] = rho1[j,i]*m1s[j,i-1] + rho2[j,i]*m2s[j,i-1]              #Reserve requirement: demand
      hbs[j,i] = hbd[j,i]                                                 #Reserve requirement: supply
      hbs_star[j,i] = hbd_star[j,i]                                       #Extra reserves: supply
      
      #Quantitative easing (not sure if shoud substitute the other equations, results does not vary anyway)
      
      bcb[j,i] = bs[j,i-1]*vareps[j,i] #Bills held by CB under QE
      
      ################################
      #NEW EQUATIONS
      
      hh[j,i] = hh[j,i] + bh[j,i] - (bs[j,i]-bb[j,i]-bcb[j,i]) #Cash held by households under QE
      
      hs[j,i] = hh[j,i]    #Supply of cash under QE
      
      ################################
      
      bh[j,i] = bs[j,i]-bb[j,i]-bcb[j,i] #Bills held by households under QE
      
      vareps[j,i] = vareps0[j,i] - vareps1[j,i]*r_star[j,i]               #Target share of bills held by CB
      
      
      #Interest rates
      re_c[j,i] = r_star[j,i]                                 #Return rate on c firms' securities
      re_e[j,i] = r_star[j,i]                                 #Return rate on e firms' securities
      re_k[j,i] = r_star[j,i]                                 #Return rate on k firms' securities
      rb[j,i] = r_star[j,i] + mub[j,i]                                    #Return rate on bills
      rl_e[j,i] = r_star[j,i] + mul_e[j,i]                                    #Interest rate on e loans
      rl_c[j,i] = r_star[j,i] + mul_c[j,i]                                    #Interest rate on c loans
      rl_k[j,i] = r_star[j,i] + mul_k[j,i]                                    #Interest rate on k loans
      rlh[j,i] = r_star[j,i] + mulh[j,i]                                  #Interest rate on mortgages
      rm[j,i] = r_star[j,i] + mum[j,i]                                    #Return rate on saving deposits
      ra[j,i] = r_star[j,i] + mua[j,i]                                    #Interest rate on CB advances
      rh[j,i] = r_star[j,i] + muh[j,i]                                    #Return rate on reserves
      if (type==0){mul_e[j,i] = mul0[j,i]}                                  #Mark-up of interest rate on e loans: exogenous
      else {mul_e[j,i] = mul0[j,i] + mul1[j,i]*lev_e[j,i-1]}                  #Mark-up of interest rate on e loans: endogenous (Minsky)
      if (type==0){mul_c[j,i] = mul0[j,i]}                                  #Mark-up of interest rate on c loans: exogenous
      else {mul_c[j,i] = mul0[j,i] + mul1[j,i]*lev_c[j,i-1]}                  #Mark-up of interest rate on c loans: endogenous (Minsky)
      if (type==0){mul_k[j,i] = mul0[j,i]}                                  #Mark-up of interest rate on k loans: exogenous
      else {mul_k[j,i] = mul0[j,i] + mul1[j,i]*lev_k[j,i-1]}                  #Mark-up of interest rate on k loans: endogenous (Minsky)
      mub[j,i] = mub0[j,i] - mub1[j,i]*(bpr[j,i]-bpr[j,i-1])              #Mark-up of bills yield
      bpr[j,i] = (bh[j,i] + bb[j,i])/bs[j,i]                              #Share of bills demanded by private sector
      
      #Labour market
      wb_e[j,i] = w_e[j,i]*nd_e[j,i]                                            #Wage bill e sector
      wb_c[j,i] = w_c[j,i]*nd_c[j,i]                                            #Wage bill c sector  
      wb_k[j,i] = w_k[j,i]*nd_k[j,i]                                            #Wage bill k sector  
      nd_e[j,i] = (y_e[j,i])/prf_e[j,i]                               #Employment e sector (labour demand)
      nd_c[j,i] = (y_c[j,i])/prf_c[j,i]                               #Employment c sector (labour demand)
      nd_k[j,i] = (y_k[j,i])/prf_k[j,i]                               #Employment k sector (labour demand)
      nd[j,i] = nd_e[j,i] + nd_c[j,i] + nd_k[j,i]                              #Employment (labour demand)
      
      ns[j,i] = ns[j,i-1]*(1+gl) + nu*(nd[j,i-1]-ns[j,i-1])
      
      if (ns[j,i]<nd[j,i]) {ns[j,i]=nd[j,i]}                              #Labour force (labour supply)
      
      if (type_un[j,i]==0) {un[j,i] = 1-((nd[j,i]+ng[j,i])/ns[j,i])}      #Unemployment rate considering public employees
      else{un[j,i] = 1-(nd[j,i]/ns[j,i])}                                 #Unemployment rate without public employees
      
      if (i<3) {w_e[j,i] = 0.86}
      else{w_e[j,i] = (1 - omega1*(un[j,i-1] - nun[j,i]))*ep[j,i]*w_e[j,i-1]/p[j,i-1]} #PC: Nominal wage equation sector e (linear)
      if (i<3) {w_c[j,i] = 0.86}
      else{w_c[j,i] = (1 - 0.005*(un[j,i-1] - nun[j,i]))*ep[j,i]*w_c[j,i-1]/p[j,i-1]} #PC: Nominal wage equation sector c (linear)
      if (i<3) {w_k[j,i] = 0.86}
      else{w_k[j,i] = (1 - 0.005*(un[j,i-1] - nun[j,i]))*ep[j,i]*w_k[j,i-1]/p[j,i-1]} #PC: Nominal wage equation sector k (linear)
      
      w[j,i] = (w_e[j,i] + w_c[j,i]+w_k[j,i])/3 #PC: Nominal wage equation (linear)
      
      prf[j,i]= (prf_e[j,i]+prf_c[j,i]+prf_k[j,i])/3
      
      #Prices and expectations
      pf[j,i] = (w[j,i]/prf[j,i])*(1+mup[j,i])                            #Unit price of private output
      if (ng[j,i]==0) {pg[j,i]=0}
      else {pg[j,i] = cgov[j,i]/(prg[j,i]*ng[j,i])}                       #Unit price of public goods (tariffs, tickets, etc.)
      
      if (type_jg[j,i]==0 && j!=11) {p[j,i] = pf[j,i]}                     #General price level
      else {p[j,i] = pf[j,i]*(1-(cgov[j,i-1]/y[j,i-1])) + pg[j,i]*(cgov[j,i-1]/y[j,i-1])}   
      
      pi[j,i] = (p[j,i]/p[j,i-1])-1                                       #Inflation rate
      #pi[j,i] = pi0 + pi[j,i-1] - beta*un[j,i]                          #Inflation rate: Phillips Curve
      #p[j,i] = p[j,i-1]*(1+pi[j,i])                                     #Price level derived from PC
      if (i<3) {epi_a[j,i] = pi[j,i]}
      else {epi_a[j,i] = epi_a[j,i-1] + psi1 + psi2*(pi[j,i-1]-epi_a[j,i-1])} #Expected inflation rate: adaptive expectations
      set.seed(i)
      if (i<3) {epi_r[j,i] = pi[j,i]}
      else {epi_r[j,i] = pi[j,i] + runif(1,min=-0.01,max=0.01)}           #Expected inflation rate: rational expectations
      if (i<3) {epi[j,i] = pi[j,i]}
      else {epi[j,i] = pi[j,i-1] + psi1 + psi2*(pit[j,i-1]-pi[j,i-1])}    #Expected inflation rate: regressive expectations
      ep[j,i] = p[j,i-1]*(1+epi_a[j,i])                                     #Expected price level
      
      
      #Minsky variables and indices
      q_e[j,i] = (esr_e[j,i]*pe_e[j,i] + lf_e[j,i])/k_e[j,i]                        #Valuation ratio e sector (Tobin q)
      q_c[j,i] = (esr_c[j,i]*pe_c[j,i] + lf_c[j,i])/k_c[j,i]                        #Valuation ratio c sector (Tobin q)
      q_k[j,i] = (esr_k[j,i]*pe_k[j,i] + lf_k[j,i])/k_k[j,i]                        #Valuation ratio k sector (Tobin q)
      lev[j,i] = lev_c[j,i]  + lev_e[j,i] + lev_k[j,i]                    #Leverage ratio of firms
      lev_e[j,i] = lf_e[j,i]/(esr_e[j,i]*pe_e[j,i] + lf_e[j,i])                     #Leverage ratio of e firms
      lev_c[j,i] = lf_c[j,i]/(esr_c[j,i]*pe_c[j,i] + lf_c[j,i])                     #Leverage ratio of c firms
      lev_k[j,i] = lf_k[j,i]/(esr_k[j,i]*pe_k[j,i] + lf_k[j,i])                     #Leverage ratio of k firms
      def_ratio[j,i] = def[j,i]/y[j,i]                                    #Goverment deficit to GDP ratio
      debt_ratio[j,i] = bs[j,i]/y[j,i]                                    #Goverment deficit to GDP ratio
      
      #Foreign sector
      im[j,i] = exp(m0[j,i] + m1[j,i]*(xr[j,i-1]) + m2[j,i]*(y_c[j,i-1]) + m3[j,i]*p[j,i-1])    #Exports
      ex[j,i] = exp(x0[j,i] + x1[j,i]*(xr[j,i-1]) + x2[j,i]*(y_f[j,i-1]) + x3[j,i]*p[j,i-1])  #Imports
      #if (i<3) {y_f[j,i] = 40}
      y_f[j,i] = y_f[j,i-1]*(1+g_f[j,i])                    #Foreign Output      
      #if (i<3) {g_f[j,i] = 0.03}
      #else {g_f[j,i] = g_f[j,i-1]*(1-g_s[j,i])}                    #Rate of growth of Output (Foreign)
      g_s[j,i] = (y[j,i]/y[j,i-1])-1                               #Rate of growth of Output (Local)
      hf[j,i] = hf[j,i-1] + tb[j,i]#/xr[j,i]                       #Foreign Reserves
      tb[j,i] = ex[j,i] - im[j,i]                                  #Trade Balance
      #xr[j,i] = tb[j,i]/(hf[j,i]-hf[j,i-1])
      
      #Redundant equation
      #hh[j,i] = hs[j,i]
      
      
      
    }
  }
}

############################################################################

# STEP 6: CHECKS AND BASIC PLOTS

#Create custom color
mycol1 <- rgb(0,255,0, max = 255, alpha = 50, names = "mygreen")
mycol2 <- rgb(255,0,0, max = 255, alpha = 50, names = "myred")
mycol3 <- rgb(0, 0, 255, max = 255, alpha = 50, names = "blue50")


#Output components under baseline
plot(y[1,2:nPeriods], type="l", lty = 1, lwd = 2, col=1, font.main=1,cex.main=0.75,main="i) Output components under baseline",ylab = '?',xlab = '',cex.axis=0.75,cex.lab=1)
lines(y_c[1,2:nPeriods], type="l", lty = 2, lwd = 2, col=2)
lines(y_k[1,2:nPeriods], type="l", lty = 3, lwd = 2, col=3)
#lines(gov[1,2:nPeriods], type="l", lty = 4, lwd = 2, col=4)
lines(y_e[1,2:nPeriods], type="l", lty = 5, lwd = 2, col=5)
#lines(tb[1,2:nPeriods], type="l", lty = 6, lwd = 2, col=6)
legend("topleft",c("Output","Consumption","Gross investment", "Energy"),  bty = "n", cex = 0.80, lty=c(1,2,3,4,5,6), lwd=c(2,2,2,2,2,2), col = c(1,2,3,4,5,6), box.lty=0)



#Interest rates under baseline
library('pracma')
plot(r_star[1,20:40],type="l", col=8, lwd=2, lty=1, font.main=1,cex.main=1, main="ii) Interest rates under baseline (periods: 20-40)",ylab = 'x 100',xlab = '',ylim = range(0,0.30),cex.axis=1,cex.lab=0.75)
lines(rb[1,25:nPeriods],type="l", col=2,lty=1,lwd=2)
lines(re_e[1,25:nPeriods],type="l", col="purple",lty=1,lwd=2)
lines(re_c[1,25:nPeriods],type="l", col="purple",lty=1,lwd=2)
lines(re_k[1,25:nPeriods],type="l", col="purple",lty=1,lwd=2)
lines(rlh[1,25:nPeriods],type="l", col=7,lty=1,lwd=2)
lines(rl[1,25:nPeriods],type="l", col=3,lty=2,lwd=2)
lines(rm[1,25:nPeriods],type="l", col=4,lty=2,lwd=2)
lines(ra[1,25:nPeriods],type="l", col=5,lty=1,lwd=2)
lines(rh[1,25:nPeriods],type="l", col="orange",lty=1,lwd=2)
legend("topleft",c("Policy rate","Bills","Shares e","Shares c","Shares k","Mortgages","Loans","Saving deposits","Advances","Reserves"),  bty = 'n', cex=1, lty=c(1,1,1,1,2,2,1,1), lwd=c(2,2,2,2,2,2,2,2), col = c(8,2,"purple",7,3,4,5,"orange"), box.lty=0)


#Consistency check: all scenarios
plot(hh[1,1:nPeriods]-hs[1,1:nPeriods], type="b", col=1, font.main=1,cex.main=0.75,main="iii) Consistency check: \n cash demand - cash supply (2 scenarios)",ylab = '?',xlab = '', ylim = range(-100,100),cex.axis=0.75,cex.lab=1) 
lines(hh[2,1:nPeriods]-hs[2,1:nPeriods], type="l", col=2)
lines(hh[3,1:nPeriods]-hs[3,1:nPeriods], type="l", lty = 3, lwd = 4, col=3)
lines(hh[4,1:nPeriods]-hs[4,1:nPeriods], type="l", lty = 3, lwd = 4, col=4)
lines(hh[5,1:nPeriods]-hs[5,1:nPeriods], type="l", lty = 3, lwd = 4, col=5)
lines(hh[6,1:nPeriods]-hs[6,1:nPeriods], type="l", lty = 3, lwd = 4, col=6)



############################################################################

# STEP 7: Experiments

#Output
plot(y[2,58:nPeriods]/y[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="iv) Output under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0.5,2),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(y[3,58:nPeriods]/y[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(y[4,58:nPeriods]/y[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(y[5,58:nPeriods]/y[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(y[6,58:nPeriods]/y[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Output Energy
plot(y_e[2,58:nPeriods]/y_e[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="v) Output E sector under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0,2),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(y_e[3,58:nPeriods]/y_e[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(y_e[4,58:nPeriods]/y_e[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(y_e[5,58:nPeriods]/y_e[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(y_e[6,58:nPeriods]/y_e[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Consumption
plot(c[2,58:nPeriods]/c[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="vi) Consumption under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0.8,2),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(c[3,58:nPeriods]/c[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(c[4,58:nPeriods]/c[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(c[5,58:nPeriods]/c[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(c[6,58:nPeriods]/c[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Consumption Energy (Households)
plot(c_e[2,58:nPeriods]/c_e[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="vii) Consumption Energy (Households)",ylab = 'Ratio to baseline',xlab = '',ylim = range(0,2),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(c_e[3,58:nPeriods]/c_e[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(c_e[4,58:nPeriods]/c_e[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(c_e[5,58:nPeriods]/c_e[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(c_e[6,58:nPeriods]/c_e[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Consumption Energy (Government)
plot(gov_e[2,58:nPeriods]/gov_e[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="viii) Consumption Energy (Government) under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0,2),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(gov_e[3,58:nPeriods]/gov_e[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(gov_e[4,58:nPeriods]/gov_e[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(gov_e[5,58:nPeriods]/gov_e[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(gov_e[6,58:nPeriods]/gov_e[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)


#Investment 
plot(id[2,58:nPeriods]/id[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="ix) Investment under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0,2.5),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(id[3,58:nPeriods]/id[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(id[4,58:nPeriods]/id[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(id[5,58:nPeriods]/id[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(id[6,58:nPeriods]/id[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
#abline(h=1.3,col=1,lty=3,lwd=1)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Investment E
plot(id_e[2,58:nPeriods]/id_e[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="x) Energy Investment under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0,2.5),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(id_e[3,58:nPeriods]/id_e[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(id_e[4,58:nPeriods]/id_e[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(id_e[5,58:nPeriods]/id_e[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(id_e[6,58:nPeriods]/id_e[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
#abline(h=1.3,col=1,lty=3,lwd=1)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Unemployment rate 
plot(un[2,58:nPeriods]-un[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xi) Unemployment rate under alternative scenarios",ylab = '% difference with baseline',xlab = '',ylim = range(-0.15,0.05),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-10,ytop=500,col=mycol1,border=NA)
lines(un[3,58:nPeriods]-un[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(un[4,58:nPeriods]-un[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(un[5,58:nPeriods]-un[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(un[6,58:nPeriods]-un[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Employment rate
plot(-un[2,58:nPeriods]+un[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xii) Employment rate under alternative scenarios",ylab = 'Difference with baseline',xlab = '',ylim = range(-0.05,0.15),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-10,ytop=500,col=mycol1,border=NA)
lines(-un[3,58:nPeriods]+un[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(-un[4,58:nPeriods]+un[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(-un[5,58:nPeriods]+un[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(-un[6,58:nPeriods]+un[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Deficit to GDP ratio
plot(def_ratio[2,58:nPeriods]-def_ratio[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xiii) Deficit to GDP ratio under alternative scenarios",ylab = 'Difference with baseline',xlab = '',ylim = range(-0.01,0.04),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-5,ytop=500,col=mycol1,border=NA)
lines(def_ratio[3,58:nPeriods]-def_ratio[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(def_ratio[4,58:nPeriods]-def_ratio[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(def_ratio[5,58:nPeriods]-def_ratio[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(def_ratio[6,58:nPeriods]-def_ratio[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Price of private goods (long run)
plot(pf[2,58:nPeriods]/pf[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xiv) Price of private goods under alternative scenarios (long run)",ylab = 'Ratio to baseline',xlab = '',ylim = range(0.5,1.5),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(pf[3,58:nPeriods]/pf[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(pf[4,58:nPeriods]/pf[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(pf[5,58:nPeriods]/pf[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(pf[6,58:nPeriods]/pf[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Price level: short run
plot(pf[2,58:80]/p[1,58:80],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xv) Prices of private goods under alternative scenarios (short run)",ylab = 'Ratio to baseline',xlab = '',ylim = range(0.5,1.5),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(pf[3,58:80]/p[1,58:80],type="l", col=8,lty=1, lwd=2)
lines(pf[4,58:80]/p[1,58:80],type="l", col=3,lty=1, lwd=2)
lines(pf[5,58:80]/pf[1,58:80],type="l", col=2,lty=1, lwd=2)
lines(pf[6,58:80]/pf[1,58:80],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Income distribution
plot(Omega[2,58:nPeriods]-Omega[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xvi) Wage share under alternative scenarios",ylab = '% difference with baseline',xlab = '',ylim = range(0,0.5),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-10,ytop=500,col=mycol1,border=NA)
lines(Omega[3,58:nPeriods]-Omega[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(Omega[4,58:nPeriods]-Omega[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(Omega[5,58:nPeriods]-Omega[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(Omega[6,58:nPeriods]-Omega[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Trade Balance
plot(tb[2,58:nPeriods]-tb[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xvii) Trade Balance under alternative scenarios",ylab = '% difference with baseline',xlab = '',ylim = range(-10,10),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-10,ytop=500,col=mycol1,border=NA)
lines(tb[3,58:nPeriods]-tb[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(tb[4,58:nPeriods]-tb[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(tb[5,58:nPeriods]-tb[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(tb[6,58:nPeriods]-tb[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Output growth
plot(g_s[2,58:nPeriods]-g_s[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xviii) Output growth under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(-0.02,0.04),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-1,ytop=500,col=mycol1,border=NA)
lines(g_s[3,58:nPeriods]-g_s[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(g_s[4,58:nPeriods]-g_s[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(g_s[5,58:nPeriods]-g_s[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(g_s[6,58:nPeriods]-g_s[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Bills held by CB
plot(bcb[2,58:nPeriods]/bcb[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xix) Bills held by CB under alternative scenarios",ylab = 'Ratio to baseline',xlab = '',ylim = range(0.5,1.3),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=0,ytop=500,col=mycol1,border=NA)
lines(bcb[3,58:nPeriods]/bcb[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(bcb[4,58:nPeriods]/bcb[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(bcb[5,58:nPeriods]/bcb[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(bcb[6,58:nPeriods]/bcb[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)

#Debt to GDP ratio
plot(debt_ratio[2,58:nPeriods]-debt_ratio[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="xx) Debt to GDP ratio under alternative scenarios",ylab = 'Difference with baseline',xlab = '',ylim = range(-0.5,1),cex.axis=1,cex.lab=1)
rect(xleft=-5,xright=3,ybottom=-5,ytop=500,col=mycol1,border=NA)
lines(debt_ratio[3,58:nPeriods]-debt_ratio[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
lines(debt_ratio[4,58:nPeriods]-debt_ratio[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
lines(debt_ratio[5,58:nPeriods]-debt_ratio[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
lines(debt_ratio[6,58:nPeriods]-debt_ratio[1,58:nPeriods],type="l", col=7,lty=2, lwd=2)
legend("topleft",c("Scenario 2","Scenario 3","Scenario 4", "Scenario 5","Scenario 6"),  bty = "n", cex = 0.45, lty=c(1,1,1,1,1,2), lwd=c(2,2,2,2,2,2), col = c(5,8,3,2,7), box.lty=0)


#Leverage ratio of firms
#plot(lev[2,58:nPeriods]-lev[1,58:nPeriods],type="l", col=5, lty=1, lwd=2, font.main=1,cex.main=1,main="f) Leverage ratio under alternative scenarios",ylab = 'Difference with baseline',xlab = '',ylim = range(-0.03,0.035),cex.axis=1,cex.lab=1)
#rect(xleft=-5,xright=3,ybottom=-10,ytop=500,col=mycol1,border=NA)
#lines(lev[3,58:nPeriods]-lev[1,58:nPeriods],type="l", col=8,lty=1, lwd=2)
#lines(lev[4,58:nPeriods]-lev[1,58:nPeriods],type="l", col=3,lty=1, lwd=2)
#lines(lev[5,58:nPeriods]-lev[1,58:nPeriods],type="l", col=2,lty=1, lwd=2)
#lines(lev[6,58:nPeriods]-lev[1,58:nPeriods],type="l", col=6,lty=1, lwd=2)