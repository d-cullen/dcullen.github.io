import excel "/Users/sethaniel/Documents/School/UCSB/Courses/Economics/PwC Programming Module/Stata Module/New Material/apple.xlsx", firstrow clear //first row is only necessary with .xlsx and .xls files
//alternatively (the general case; for csv's): import delimited "/Users/sethaniel/Documents/School/UCSB/Courses/Economics/PwC Programming Module/Stata Module/Previous Material/apple.xlsx", clear
drop if date==.
//labelling
label variable volume "num shares sold"
label variable open "opening price"
label variable close "closing price"
label variable high "highest price"
label variable low "lowest price"
//Variable generation
gen open_abv100 = 0
replace open_abv100 = 1 if open > 100
gen close110_120 = 0
replace close110_120 = 1 if close >= 110 & close <= 120
gen lowLs100OrHighGr115 = 0
replace lowLs100OrHighGr115 = 1 if low < 100 | high > 115
//Variables part 2
gen absdiff_OpenClose = abs(open - close)
gen diff_Gr5 = 0
replace diff_Gr5 = 1 if (absdiff_OpenClose > 5)
gen bigGain = 0
replace bigGain = 1 if ((diff_Gr5 == 1) & (close > open))
save apple_v2, replace


//Pokemon data
import excel "/Users/sethaniel/Documents/School/UCSB/Courses/Economics/PwC Programming Module/Stata Module/New Material/pokemon.xlsx", firstrow clear
merge 1:1 date using apple_v2
sort date
//Graphing
graph bar (mean) close, over (month)
histogram pokemoncaught, frequency name(hist_pokecaught)
twoway (line high date) (line low date), name(tseries_highLow)
twoway (line close date) (line pokemoncaught date), name(tseries_closePokeCaught)

//Regression (a very poor example)
gen diff_CloseOpen = close - open
gen diff_pokecaught = pokemoncaught - pokemoncaught[_n-1]

regress diff_CloseOpen diff_pokecaught
//"_cons" is the constant coefficient
//P>|t| is the P value. P value>.05 means variable fails to predict, and thus you fail to reject the null
//Coef. indicates the size of the effect of the independent variable on the dependent variable
//R-squared is how well the model predicts the behavior of the dependent variable
