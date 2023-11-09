use "/Users/wilsontai/Downloads/assignment2.dta", clear //Importing Data


matrixA=[1,. ,.,0 .,1,.,00,0,1,.0,1,0,. ] //SettingSVARMatrix B0
matrix list A

egen t = seq() //Setting Time Series
tsset t


drop if USACPICORMINMEI == .


// creating new variables called growth and inflation
gen growth = d.GDPC1
gen inflation = d.USACPICORMINMEI

//Running SVAR
svar FF growth inflation UNRATE,aeq(A) lags(1 2) nocnsreport nolog esttab svar


//Plotting IRF and FEVD
irf create svar, set(SVAR,replace) replace
irf graph sirf, byopts(yrescale)
irf graph sfevd, byopts(yrescale)
