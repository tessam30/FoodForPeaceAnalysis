/*-------------------------------------------------------------------------------
# Name:		00_SetupAndEDA
# Purpose:	Conduct a series of exploratory analyses on FFP data
# Author:	Tim Essam, Ph.D.
# Created:	06/29/2016
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/
clear
capture log close

global homein "C:\Users\t\Documents\GitHub\FoodForPeaceAnalysis\Datain"
global workin "C:\Users\Tim\Documents\GitHub\FoodForPeaceAnalysis\Datain"
global path "C:\Users\Tim\Documents\GitHub\FoodForPeaceAnalysis"
global pathout "C:\Users\Tim\Documents\GitHub\FoodForPeaceAnalysis\Dataout"
import excel "$workin\ffp_procurement.xlsx", sheet("data") firstrow clear

* Check for missing
d
mdesc

* Rename all the variables to shorten them
ren _all, lower

* Fix recipient countries names to be consistent
replace recipientcountry = "REPUBLIC OF SOUTH SUDAN" if recipientcountry == "SOUTH SUDAN"

* List all the Congo or Dem. Republic names
tab recipientcountry if regexm(recipientcountry , "(AFR|DEM|CONGO)") == 1

g byte forprep = recipientcountry =="FOREIGN-PREP"
la var forprep "foreign prep records only"

* Create a grouping for the commodities
local comtype BEANS CORN EMERG FLOUR LENTILS OIL PEAS POTATO RICE SORGHUM WHEAT
foreach x of local comtype {
	tabsort  material if regexm(material, "`x'") == 1
	}
*end

/* Create a dataset of crosswalk data here --  used for consultation
preserve
g byte freq = 1
collapse (count) freq, by(material)
export delimited using "$pathout/materials_cw.csv", replace
restore
*/

* Parse out numbers from material title in case they are needed for normalization
* First find all instances of XX KG
gen wgt = regexs(0) if(regexm(material, "[0-9]*[\/]*[0-9]*[ ]*[A-Z]*$"))
replace wgt = regexs(0) if(regexm(material,"[0-9][\/]*[0-9][ ]*[L]"))
replace wgt = regexs(0) if(regexm(material,"[0-9][\/]*[0-9][ ]*[L][B]")) //punds
replace wgt = regexs(0) if(regexm(material, "[0-9][0-9][0-9][ ]*[L]")) // Oil drums
replace wgt = regexs(0) if(regexm(material, "[0-9][0-9] [A-Z][A-Z]$")) // taking care of 50 KG

gen wgttype = regexs(0) if(regexm(wgt, "[A-Z]*$"))

* create a few labels for customer
la var wgt "commodity weight"
la var wgttype "commodity weight type"


tabsort material if forprep ==1

* Basic bar graph of material counts collected at foreign prep
graph hbar (sum) forprep if forprep == 1, over(material, sort((sum ) forprep) /*
*/ descending label(labcolor(gs6) labsize(tiny)) axis(lcolor(none))) /*
*/ bar(1, fcolor(gs10) lcolor(gs14)) blabel(bar, color(gs6) position(outside))/*
*/ yscale(noline) ylabel(, nolabels noticks nogrid) legend(off) scheme(s1mono)/*
*/ graphregion(fcolor(none) lcolor(none) ifcolor(none)) plotregion(fcolor(none)/*
*/ lcolor(none) ifcolor(none) ilcolor(none))


tabsort soldtoparty, sum(povalue)
