/*-------------------------------------------------------------------------------
# Name:		01_PlotFFP
# Purpose:	Plot the FFP data for exploratory analysis
# Author:	Tim Essam, Ph.D.
# Created:	06/29/2016
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/
clear
capture log close

use "$pathout/ffp_procurement.dta", clear



* First, look at the distribution of prices by month for a single commodity
mean po_unitcost if inlist(category, "Peas"), over(monthpo)
coefplot
reg po_unitcost ib(2013).yearpo ib(8).monthpo podate if inlist(category, "Peas")

twoway (histogram po_unitcost if inlist(category, "Peas")) if monthpo!=10, by(monthpo)


mean po_unitcost mat_count if inlist(material, "LENTILS BAG-50 KG"), over(monthpo)
coefplot

reg po_unitcost ib(2013).yearpo ib(9).monthpo podate  if inlist(material, "LENTILS BAG-50 KG")
predict resid
mean po_unitcost if inlist(material, "PEAS, YELLOW, SPLIT BAG-50 KG"), over(monthpo)

* Plotting
*Basic time series plot of total povalue by recipient country
graph set window fontface "Segoe UI"
format povalue %16.0fc

* General plot of peas, corn and oil
 twoway(rcapsym daily_min_categ daily_max_categ podate, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter daily_mean_categ podate, m(O) mc("65 171 93")) /*
*/ if inlist(category, "Peas"),/*
*/ by(categoryOrd, row(3)) ylabel(,angle(horizontal)) xlabel(18712(60)20332, labsize(tiny) angle(vertical) /*
*/ format(%tdMon_CCYY) ticks grid)

 twoway(rcapsym daily_min_categ daily_max_categ podate, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter daily_mean_categ podate, m(O) mc("65 171 93")) /*
*/ if inlist(category, "Peas", "Corn", "Oil"),/*
*/ by(category, row(3)) ylabel(,angle(horizontal)) xlabel(18712(60)20332, labsize(tiny) angle(vertical) /*
*/ format(%tdMon_CCYY) ticks grid)



twoway(rcapsym daily_min_categ daily_max_categ podate, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter daily_mean_categ podate, m(O) mc("65 171 93")) /*
*/ if inlist(category, "Peas") &/*
*/ inlist(recipientcountry, "ETHIOPIA", "KENYA", "FOREIGN-PREP"), /*
*/ by(categoryOrd, row(3)) ylabel(,angle(horizontal)) xlabel(18712(60)20332, labsize(tiny) angle(vertical) /*
*/ format(%tdMon_CCYY) ticks grid)

twoway(rcapsym daily_min_com daily_max_com podate, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter daily_mean_com podate, m(O) mc("65 171 93")) /*
*/ (lowess po_unitcost podate if material == "PEAS, YELLOW, SPLIT BAG-50 KG")/*
*/ if inlist(material, "PEAS, YELLOW, SPLIT BAG-50 KG"), /*
*/ ylabel(,angle(horizontal)) xlabel(18712(60)20332, labsize(tiny) angle(vertical) /*
*/ format(%tdMon_CCYY) ticks grid)


twoway(rcapsym monthly_min_com monthly_max_com monthpo, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter monthly_mean_com monthpo, m(O) mc("65 171 93")) /*
*/ (lowess po_unitcost monthpo if material == "PEAS, YELLOW, SPLIT BAG-50 KG")/*
*/ if inlist(material, "PEAS, YELLOW, SPLIT BAG-50 KG"), /*
*/ ylabel(,angle(horizontal)) xlabel(, labsize(tiny) angle(vertical) /*
*/ format(%tdMon) ticks grid)


twoway(rcapsym daily_min daily_max podate, mc("190 190 190") lc("190 190 190"))/*
*/ (scatter daily_mean podate, m(O) mc("65 171 93")) /*
*/ if inlist(material, "WHEAT, HARD RED WINTER BULK") &/*
*/ inlist(recipientcountry, "ETHIOPIA", "KENYA"), /*
*/ by(peaOrd, row(2)) xlabel(18712(60)20332, labsize(tiny) angle(vertical) /*
*/ format(%tdMon_CCYY) ticks grid)

* Now, focus on the countries themselves and look at most active 






















graph bar (sum) povalue if recipientcountry == "ETHIOPIA", over(month) over(fiscalyear) by(material)

twoway scatter povalue podate if recipientcountry == "ETHIOPIA", by(material)


twoway bar datecount moYear







graph hbar (sum) forprep if forprep == 1, over(material, sort((sum ) forprep) /*
*/ descending label(labcolor(gs6) labsize(tiny)) axis(lcolor(none))) /*
*/ bar(1, fcolor(gs10) lcolor(gs14)) blabel(bar, color(gs6) position(outside))/*
*/ yscale(noline) ylabel(, nolabels noticks nogrid) legend(off) scheme(s1mono)/*
*/ graphregion(fcolor(none) lcolor(none) ifcolor(none)) plotregion(fcolor(none)/*
*/ lcolor(none) ifcolor(none) ilcolor(none))

tabsort soldtoparty, sum(povalue)

* Create a millions var for povalue
g povalue2 = povalue/1000000

graph hbar (sum) povalue2, nofill over(buyer, sort((sum) povalue2 ) /*
*/ descending label(labcolor(gs6) labsize(tiny)) axis(lcolor(none))) /*
*/ bar(1, fcolor(gs10) lcolor(gs14)) blabel(bar, color(gs6) size(tiny) format(%16.0fc) position(outside))/*
*/ yscale(noline) ylabel(, nolabels noticks nogrid) legend(off) scheme(s1mono)/*
*/ graphregion(fcolor(none) lcolor(none) ifcolor(none)) plotregion(fcolor(none)/*
*/ lcolor(none) ifcolor(none) ilcolor(none)) by(fiscalyear)

* Benefitting countries in dollar terms
graph hbar (sum) povalue2, nofill over(recipientcountry, sort((sum) povalue)  /*
*/ descending label(labcolor(gs6) labsize(half_tiny))  axis(lcolor(none))) /*
*/ bar(1, fcolor(gs10) lcolor(gs14)) blabel(bar, color(gs6) size(half_tiny) format(%16.0fc) position(outside))/*
*/ yscale(noline) ylabel(, nolabels noticks nogrid) legend(off) scheme(s1mono)/*
*/ graphregion(fcolor(none) lcolor(none) ifcolor(none)) plotregion(fcolor(none)/*
*/ lcolor(none) ifcolor(none) ilcolor(none)) by(fiscalyear)

table recipientcountry fiscalyear, c(sum povalue2) format(%16.2fc) row col
table material fiscalyear, c(sum povalue2) format(%16.2fc) row col

tabsort material fiscalyear, sum(povalue)


* Create groupings of 



* By commodity over time
graph hbar (sum) povalue2, nofill over(material, sort((sum) povalue)  /*
*/ descending label(labcolor(gs6) labsize(half_tiny))  axis(lcolor(none))) /*
*/ bar(1, fcolor(gs10) lcolor(gs14)) blabel(bar, color(gs6) size(half_tiny) format(%16.0fc) position(outside))/*
*/ yscale(noline) ylabel(, nolabels noticks nogrid) legend(off) scheme(s1mono)/*
*/ graphregion(fcolor(none) lcolor(none) ifcolor(none)) plotregion(fcolor(none)/*
*/ lcolor(none) ifcolor(none) ilcolor(none)) by(fiscalyear)
