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

* Merge in consumer price index information to check whether or not inflation matters
/* gen statadate = var5 + td(30dec1899)
format statadate  %td
*/
* Create a few date variables
g monthpo = month(podate)
g yearpo = year(podate)
g moYear = mdy(monthpo, 15, yearpo )
format moYear %td

merge m:1 moYear using "$pathout/inflation.dta"
drop if _merge == 2

* Create inflation adjusted povalue
g double povalue_2011 = round((povalue/cpi_updated)*100, 100)


* Create a grouping for the commodities
local comtype BEANS CORN EMERG FLOUR LENTILS OIL PEAS POTATO RICE SORGHUM WHEAT
foreach x of local comtype {
	tabsort  material if regexm(material, "`x'") == 1
	}
*end

g category = "";
replace category = "Bean" if inlist(material, "BEANS, PINTO BAG-50 KG",  /*
			*/ "BEANS, BLACK BAG-50 KG", "BEANS, KIDNEY, DARK RED BAG-50 KG",  /*
			*/ "BEANS, GARBANZO, KABULI BAG-50 KG", /*
			*/ "BEANS, SMALL RED BAG-50 KG",  /*
			*/ "BEANS, GREAT NORTHERN BAG-50 KG")

replace category = "Bulgur" if inlist(material, "BULGUR, SOY-FORT BAG-50 KG", /*
			*/ "BULGUR  BAG-50 KG")
			
replace category = "Corn" if inlist(material, "CORN-SOY BLEND PLUS BAG-HP-25 KG",/*
			*/ "CORNMEAL BAG-HP-25 KG",/*
			*/ "CORN-SOY BLEND BAG-HP-25 KG",/*
			*/ "CORN-SOY BLEND BAG-25 KG",/*
			*/ "CORNMEAL, SOY-FORT BAG-HP-25 KG",/*
			*/ "CORN, YELLOW BAG-50 KG",/*
			*/ "CORN, YELLOW BULK")

replace category = "Ready To Use Foods" if inlist(material, "EMERGNCY FD, RUTF SPRD POUCH-150/92 G",/*
			*/ "EMERGNCY FD, RUSF POUCH-150/100 G",/*
			*/ "EMERGNCY FD, A20 PASTE POUCH-18-9/50 G",/*
			*/ "EMERGNCY FD, A28 RICE BAR-24-9/55 G",/*
			*/ "EMERGNCY FD, A29 WHEAT BAR-24-9/55 G",/*
			*/ "EMERGENCY FD, RUSF POUCH-135/100 G")
			
replace category = "Flour" if inlist(material, "FLOUR, ALL PURPOSE BAG-50 KG",/*
			*/ "FLOUR, BREAD BAG-50 KG")

replace category = "Lentils" if inlist(material, "LENTILS BAG-50 KG")

replace category = "Oil" if inlist(material, "OIL, VEGETABLE CAN-6/4 L",/*
			*/ "OIL, VEGETABLE BOTTLE, PLS-6/4 L 670-719",/*
			*/ "OIL, VEGETABLE PAIL-20 L",/*
			*/ "OIL, VEGETABLE BOTTLE, PLS-6/4 L 800-899",/*
			*/ "OIL, SOYBEAN, CRUDE, DEGUMMED BULK")
		
replace category = "Oil" if inlist(material, "OIL, VEGETABLE BOTTLE, PLS-6/4 L 720-799",/*
			*/ "OIL, VEGETABLE DRUM-208 L",/*
			*/ "OIL, SUNFLOWERSEED PAIL-20 L",/*
			*/ "OIL, SUNFLWRSD BOTTLE, PLS-6/4 L 720-799",/*
			*/ "OIL, SUNFLOWERSEED CAN-6/4 L",/*
			*/ "OIL, SUNFLWRSD BOTTLE, PLS-6/4 L 670-719")

replace category = "Peas" if inlist(material, "PEAS, YELLOW, SPLIT BAG-50 KG",/*
			*/ "PEAS, YELLOW, WHOLE BAG-50 KG",/*
			*/ "PEAS, GREEN, SPLIT BAG-50 KG",/*
			*/ "PEAS, GREEN, WHOLE BAG-50 KG",/*
			*/ "PEAS, YELLOW, WHOLE PKG-6/4 LB")
			
replace category = "Rice" if inlist(material, "RICE, 5/20 LG, W-MLD BAG-50 KG",/*
			*/"RICE, 2/7 LG, W-MLD BAG-50 KG",/*
			*/"RICE, 2/7 LG, W-MLD, PRBL BAG-50 KG",/*
			*/"RICE, MILLED BULK",/*
			*/"RICE, 5/20 MG, W-MLD BAG-50 KG",/*
			*/"RICE, 2/4 LG, H-MLD BAG-25 KG")
			
replace category = "Rice" if inlist(material, "RICE, 5/20 LG, W-MLD, PRBL BAG-50 KG",/*
			*/"RICE, 3/15 LG, W-MLD BAG-50 KG",/*
			*/"RICE, 5/20 LG, W-MLD, FORT BAG-50 KG",/*
			*/"RICE, 2/7 LG, W-MLD PKG-6/4 LB",/*
			*/"RICE, 3/15 MG, W-MLD BAG-50 KG")
			
replace category = "Wheat" if inlist(material, "WHEAT, HARD RED WINTER BULK",/*
			*/"WHEAT, SOFT WHITE BULK",/*
			*/"WHEAT-SOY BLEND BAG-25 KG",/*
			*/"WHEAT-SOY BLEND BAG-HP-25 KG",/*
			*/"WHEAT, HARD RED WINTER BAG-50 KG")
replace category = "Wheat" if inlist(material, "WHEAT, NORTHERN SPRING BULK",/*
			*/"WHEAT, HARD RED SPRING BULK",/*
			*/"WHEAT, SOFT WHITE BAG-50 KG",/*
			*/"WHEAT, DARK NORTHERN SPRING BULK",/*
			*/"WHEAT, HARD RED SPRING BAG-50 KG")
			
replace category = "Sorghum" if inlist(material, "SORGHUM BULK", "SORGHUM BAG-50 KG")
replace category = "Fortified Blended Foods" if inlist(material, "CSB SUPER CEREAL PLUS BOX-10/1.5 KG")

drop if regexm(material, "(POTATO FLAKES|POTATO GRANULES|SALMON|SOY FLOUR, DEFATTED|SOY PROTIEN, ISOLATE)")

* Create a second level category variable
g sec_category = "other"
replace sec_category = "Fortified Blended Foods" if inlist(material, "CORN-SOY BLEND PLUS BAG-HP-25 KG",/*
			*/ "CORNMEAL BAG-HP-25 KG",/*
			*/ "CORN-SOY BLEND BAG-HP-25 KG",/*
			*/ "CORN-SOY BLEND BAG-25 KG",/*
			*/ "CORNMEAL, SOY-FORT BAG-HP-25 KG",/*
			*/ "CSB SUPER CEREAL PLUS BOX-10/1.5 KG")

replace sec_category = "Ready to Use Foods" if category == "Ready To Use Foods"

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

* Potential unit cost?
g po_unitcost = povalue / metrictons

* TO DO:Create a few common groupings for the commodities


*
* For now, we are filtering on only Title II programming
keep if program == "480-TITLE_II"

* Focus on yellow split peas for the time being
egen totMatPOV = total(povalue), by(material recipientcountry)
la var totMatPOV "total povalue by material and country"

egen totCatPOV = total(povalue), by(category recipientcountry)
la var totCatPOV "total povalue by category and country"

local poStat mean min max
foreach x of local poStat {
	egen daily_`x' = `x'(po_unitcost), by(material podate recipientcountry)
	la var daily_`x' "`x' unit cost by material date and recipient country"
	
	egen daily_`x'_com = `x'(po_unitcost), by(material podate)
	la var daily_`x'_com "`x' unit cost by material and date"
	
	egen monthly_`x'_com = `x'(po_unitcost), by(material moYear)
	la var monthly_`x'_com "`x' unit cost by material and month and year"
	
	egen daily_`x'_country = `x'(po_unitcost), by(categ podate recipientcountry)
	la var daily_`x'_country "`x' unit cost by cateogry, date and recipient country"
	
	egen daily_`x'_category = `x'(po_unitcost), by(categ podate)
	la var daily_`x'_category  "`x' unit cost by category and date"
	
	egen monthly_`x'_category = `x'(po_unitcost), by(categ moYear)
	la var monthly_`x'_category "`x' unit cost by category and month and year"
	
}
*end

* Sort on tht total povalue for the country
g tmpvar = -totMatPOV
g tmpvar2 = -totCatPOV
egen materialOrd = axis(tmpvar recipientcountry), label(recipientcountry)
egen categoryOrd = axis(tmpvar2 recipientcountry), label(recipientcountry)


* Create a variable to track who commodities are being sold to, grouping those obs w/ 
* fewer than 10 entires
clonevar buyer = soldtoparty
bys buyer: gen freqtmp = _N
replace buyer = "OTHER" if freqtmp < 10
bys podate: g datecount =_N

* create frequencies of purchases
egen mat_count = count(povalue), by(podate material)
egen cat_count = count(povalue), by(podate category)

tabsort material if forprep ==1

* Sorting variables
egen totCatValue = total(povalue/1000000), by(category)
egen totMatValue = total(povalue/1000000), by(material)


* Add a regional variable to the mix to capture geography in the model

g region = ""
replace region = "East Africa" if regexm(recipientcountry, /*
 */"(BURUNDI|RWANDA|TANZANIA|UGANDA|DJIBOUTI|ETHIOPIA|KENYA|SOMALIA|SUDAN)")

replace region = "Southern Africa" if regexm(recipientcountry, /*
*/"(ZIMBABWE|MADAGASCAR|MALAWI|AFRICAN REP.|CONGO-DEM|MOZAMBIQUE)")

replace region = "west Africa" if regexm(recipientcountry, /*
*/"(NIGER|BURKINA|MALI|GHANA|CHAD|SIERRA|MAURITANIA|IVORY|CAMEROON|SENEGAL|LIBERIA)")

replace region = "Middle East and North Africa" if regexm(recipientcountry, /*
*/"(YEMEN|ALGERIA|WEST BANK|SYRIA)")

replace region = "Prep" if regexm(recipientcountry, /*
*/"(DOMESTIC|FOREIGN)")

replace region = "Latin America & Caribbean" if regexm(recipientcountry, /*
*/"(HAITI|GUATEMALA|COLOMBIA|ECUADOR)")

replace region = "Asia" if regexm(recipientcountry, /*
*/"(BANGLADESH|PAKISTAN|AFGHAN|NEPAL|SRI LANKA)")



bys fiscalyear: tabsort category region, mi
compress

* Create some labels to identify new variables
la var category "simplified categories"
la var sec_category "secondary simplified categories"
la var po_unitcost "unit cost of commodity"
la var monthpo "month of purchase order"
la var yearpo "year of purchase order"
la var moYear "month and year of purchase order"
la var datecount "number of transactions by date"
la var mat_count "number of materials procured by day"
la var cat_count "number of categories procured by day"
la var totCatValue "total value by commodity"
la var totMatValue "total value by materials"

saveold "$pathout/ffp_procurement.dta", replace

