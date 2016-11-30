/*-------------------------------------------------------------------------------
# Name:		03_CombinedAnalysis
# Purpose:	Answer a series of business intelligence questions related to FFP data
# Author:	Tim Essam, Ph.D.
# Created:	2016/11/29
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/03_CombinedAnalysis.txt", replace

import delimited "$pathin\ffp_procurement.csv", varnames(1) case(preserve) clear 
drop v1

* Clean up the dates to get years and months as variables
	foreach x of varlist CreationDate HdrCloseoutDate Deliverydate StartDeliveryDate EndDeliveryDate {
		gen `x'_d = date(`x', "YMD")
		format `x'_d %td
		}
*end

* Create year and month variables for Creation date; Fiscal quarter information below
	/*  1st quarter: 1 October 2016 – 31 December 2016
		2nd quarter: 1 January 2017 – 31 March 2017
		3rd quarter: 1 April 2017 – 30 June 2017
		4th quarter: 1 July 2017 – 30 September 2017 */
		
	g yearCreated = year(CreationDate_d)
	g monthCreated = month(CreationDate_d)
	g dayOfWeekCreated =dow(CreationDate_d)
	la def dow 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday"
	la val dayOfWeekCreated dow
	
	* Create a fiscal year variable based on the creation date
	g FiscalYear = .
	g FiscalQtr = .
		forvalues i = 2011(1)2016 {
		replace FiscalYear = `i' if inrange(CreationDate_d, mdy(10, 1, `i'-1), mdy(9,30,`i'))
		replace FiscalQtr = 1 if inrange(CreationDate_d, mdy(10, 1, `i'), mdy(12,31,`i'))
		replace FiscalQtr = 2 if inrange(CreationDate_d, mdy(1, 1, `i'), mdy(3, 31,`i'))
		replace FiscalQtr = 3 if inrange(CreationDate_d, mdy(4, 1, `i'), mdy(6, 30,`i'))
		replace FiscalQtr = 4 if inrange(CreationDate_d, mdy(7, 1, `i'), mdy(9, 30,`i'))
	}					
	*end					
	
* Flags for easy filtering
* for 480-Title II Procurments (can toggle this to include 1 grant)
	g byte titleII = regexm(FunctionalArea, ("480-TITLE_II"))==1
	g byte bulkProduct = regexm(ProductShortText, "BULK") == 1
	g byte bulkCategory = regexm(CategoryDescription, "BULK") == 1

* Customer would like same tables over 8 different variables; Write a program to accomplish this


	
	
	
	
	
	
	
* Metric Tons totals	
* 1. How many total metric tons were purchased
	* a. Inclusive of all commodities
	table FunctionalArea FiscalYear, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	* b. for eacho of the commodities (second table is more detailed)
	table CategoryDescription FiscalYear, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	table ProductShortText FiscalYear, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	* c. for all packaged commodities
	* d. for all bulk commodities
	table CategoryDescription bulkCategory, /*
	*/ c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	table ProductShortText bulkProduct, /*
	*/ c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	* e. for each vendor
	table VendorName, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
		
	* f. by vendor plant location
	table VendPlName, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
		
	* g. by load port/terminal
	table LoadportName, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	table LoadPointName, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
* Metric Tonnes purchased each Quarter?
	table FunctionalArea FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	* b. for eacho of the commodities (second table is more detailed)
	table CategoryDescription FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	table ProductShortText FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	
	* c. for all packaged commodities
	* d. for all bulk commodities
	table CategoryDescription bulkCategory FiscalQtr, /*
	*/ c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row
	
	table ProductShortText bulkProduct FiscalQtr, /*
	*/ c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row 
	
	* e. for each vendor
	table VendorName FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
		
	* f. by vendor plant location
	table VendPlName FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
		
	* g. by load port/terminal
	table LoadportName FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col
	table LoadPointName FiscalQtr, c(sum QuantityinNetMetricTonsMT) format(%12.0fc) row col

	