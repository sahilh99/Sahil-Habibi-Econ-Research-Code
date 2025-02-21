*import our concert data
import excel "Amphitheater.xlsx", clear firstrow

*Get rid of comedy observations to just keep music conerts
drop if substr(Genre,1,6) == "Comedy"

*Convert our Date variable to Stata Date to extract Year
gen date = date(StartDate, "MDY")

*Take the year from the date
gen Year = year(date)
drop date

*Get rid of covid years
drop if Year == 2020 | Year == 2021

*Merge on CPIAUCSL data to get inflation data to normalize the prices
merge m:1 Year using CPI.dta

drop _merge
*Create Local Macros to loop through for variable generation for real ticket prices
local Reals RealTicketPriceMax RealTicketPriceMin RealTicketPriceAverage
local Noms TicketPriceMax TicketPriceMin TicketPriceAverage

*Intialize the local macro
local i = 1
*Use a foreach loop to go through 
foreach Real of local Reals {
	*Start a local to go through the nominal names
    local nom: word `i' of `Noms'
	*Create our real ticket price variables using the formula:
    gen `Real' = (`nom' / CPI)*166.583
	*move to the next word
    local ++i
}

*drop observations where RealTicketPriceMax is missing
drop if missing(RealTicketPriceMax)

*Preserve data for a collapse to go through graphs
preserve 

*Collapse by the variable means for each year for the real and nominal ticket prices
collapse `Reals' `Noms', by(Year)

*Create scheme to graph
set scheme s2color 

*Create local macros for y axis titles and colors to loop through
local Realytitles `" "Max Ticket Price (Real)" "Min Ticket Price (Real)" "Average Ticket Price (Real)" "'
local colors red blue green

*Tokenize the local macro to treat each element of the macro phrases and not one word
capture{tokenize "`Realytitles'"}

*Start an index 
local i 0

*Start foreach loop through Reals
foreach var of local Reals {
	*Boost local macro up by one ot move through 
    local i = `i' + 1
	
	*Take word of Realytitles
    local title : word `i' of `Realytitles'

	*Takes word of colors
    local color : word `i' of `colors'

	*Create a line graph with our details
    line `var' Year, xtitle(Year) ytitle("`title'") title("`title' Over Time") lcolor(`color')

    *Export the graph to save it to view
    graph export "`var'.png", replace
}


*Use local macros for nominal ticket prices
local Nomytitles `" "Max Ticket Price (Nominal)" "Min Ticket Price (Nominal)" "Average Ticket Price (Nominal)" "'
local colors red blue green

*Tokenize so that we have phrase elements instead of words of the local macro
capture{tokenize "`Nomytitles'"}

*Start up the index
local i 0

*Initialize the foreach loop 
foreach var of local Noms {
	*Go through i
    local i = `i' + 1

	*Goes through nominal titles
    local title : word `i' of `Nomytitles'

	*Go through the word of colors
    local color : word `i' of `colors'

	*Create a line graph for each one
    line `var' Year, xtitle(Year) ytitle("`title'") title("`title' Over Time") lcolor(`color')

    *Save the graph files
    graph export "`var'.png", replace
}

*Bring old dataset back
restore

*Create a list of genres to collapse repeated observation values in the Genre variable for cleaning
local Genres "Americana Asian Blues Christian Classical Country Dance Easy Family Folk Jazz Latin Multi Pop RB Rap Spoken Theatrical Tribute"

*Use a foreach loop 
foreach Genre of local Genres {
	*Replace Genre equal to the names in the local macro if the start of the word is that value
    replace Genre = "`Genre'" if strpos(Genre, "`Genre'") == 1
}
*Save dataset again
preserve

*Collapse down data along the means to make bar graphs
collapse RealTicketPriceMin RealTicketPriceMax RealTicketPriceAverage, by (Year Genre)

*Drop all years except 1999 and 2024 for a comparison
keep if Year == 1999 | Year == 2024

*Loop through creation of horizontal bar graphs for visuals to show how genres have changed over time, especially 
*a large fall off for Classical music over time
foreach var in RealTicketPriceAverage RealTicketPriceMax {
    foreach genres in `"Pop" "Rap" "Jazz" "Country"' `"Pop" "Rap" "Classical" "Jazz" "Country"' {
        graph hbar (mean) `var' ///
            if inlist(Genre, `genres'), ///
            over(Year) ///
            over(Genre, sort(1)) ///
            asyvars ///
            bar(1, fcolor(red) lcolor(black) lwidth(medium)) ///
            bar(2, fcolor(blue) lcolor(black) lwidth(medium)) ///
            title("Real `=proper("`var'")' (1999 vs. 2024)") ///
            ytitle("Real `=proper("`var'")'") ///
            legend(order(1 "1999" 2 "2024")) ///
            ylabel(, labsize(vsmall))
    }
}


*Bring back dataset
restore

*Drop observations where the venue only has 5 observations to clean data
bysort Venue (Venue): gen count = _N
drop if count < 5
drop count

*Create a list of promoters to collapse observations similar to how we did genre & venue
local Promoters "AC AEG Alex Alvarez Andrew Another Atlanta Avalaon Awakening BCIRE BRE Bay Bear Beaver Belkin Blue C3 CMoore Cardenas Cat's Cellar Chuck Clear Concrete DCF Dewey Din Double Eletric Emporium Evening FPC Goldenvoice Green Hauser Haymon House Jack Knitting Kroenke Lakeside Live Messina Metropolitan Mike Music Nederlander Outback Pace Palace Prmier Rams SFX C3 AnotherPromoter"

*Use a foreach loop to go through the list of Promoters and set those values to clean
foreach Promoter of local Promoters {
    replace Promoter = "`Promoter'" if strpos(Promoter, "`Promoter'") == 1
}

*condense the in house promotion observations to just one value for Promoter
replace Promoter = "(In-House Promotion)" if strpos(Promoter, "(In-House Promotion)") == 1

*Drop observations that have a promoter with less than 5 observations to clean data
bysort Promoter (Promoter): gen count = _N
drop if count < 5
drop count

*We need our variables which are strings to be encoded as numerical variables so we can use them for fixed effects
local categories Headliner Market Genre
*intialize the local i
local i = 1
*use for loop to go through the local
foreach category of local categories {
	*Encode the variable with same name but just s at the end i.e. "Market" --> "Markets"
	encode(`category'), gen(`category's)
	*push through the next word
local i = `i' + 1
}
*Create our variable that is our post-period for our DiD estimation
gen Post = (Year>2014)

*Set our indices for our panel data
capture{xtset Headliners Year}
*This will look if we have headliners that are in both, as we wish to have a panel regression that only has artists in both time periods
bysort Headliners (Post): gen count = Post[_N] - Post[1] 
*keep means that they are in both time periods, which we want
keep if count == 1
drop count

*Drop genres that are not wanted
drop if Genre == "World" | Genre == "Theatrical" | Genre == "Latin"
*Create our treatment and interaction variable between treat and post
gen STRM = (inlist(Genre,"Rap","Pop","RB"))
gen STRMxPOST = STRM*Post

*preserve as we wish to create a visual representation of our data
preserve

*collapse along Year and STRM to compare the RealTicketPricMax over time for the treatment
*and control group
collapse RealTicketPriceMax, by(Year STRM)
*Make the data look closer into the 2015-2019 range of interst
drop if Year>2023 & Year<2006
*Code our lines all on one graph, with a line break to show where we wish to look at our treatment
twoway (line RealTicketPriceMax Year if STRM == 1) (line RealTicketPriceMax Year if STRM == 0), legend(label(1 "Treatment Group") label(2 "Non Treatment Group")) xline(2016)
*export graph
graph export "/Users/Sahil Habibi/Downloads/Ticket Price DiD.png", replace as(png) name("Graph")

*Now we wish to get back to our original dataset
restore

*create locals for the real ticket prices and their log transforms
local Reals RealTicketPriceMax RealTicketPriceMin RealTicketPriceAverage
local LogReals LogRTPMax LogRTPMin LogRTPAve

*initiate i for our local to create log transforms of our ticket prices 
local i = 1
*a for loop to loop through the real ticket variable names
foreach Real of local Reals {
	*this is a local that will go through the local of log transform names
    local LogReal : word `i' of `LogReals'
	*generate log transform of the real variables
    gen `LogReal' = log(`Real')
	*iterate through the loop for i
    local i = `i' + 1
}

*install to be able to output regression results to latex for proper presentation
ssc install outreg2
*begin for loop, taking variable names from the local LogReals
foreach LogReal of local LogReals {
	*run our DiD estimation regression, using fixed effects and robust standard errors
	regress `LogReal' STRM Post STRMxPOST i.Year i.Genres, robust
	*output the latex code so we can use proper presentation for results
	outreg2 using price_regression.tex, append tex se label
}

*create a local for our ticket quantity variable to test hypothesis that upward demand is pushing up ticket prices
local Tickets AverageTicketsSold TotalTicketsSold
*create log transform local
local LogTix LogATS LogTTS 

*start with i 
local i = 1
*a for loop to loop through our tickets variables
foreach Ticket of local Tickets {
	*loop through logvar local
    local LogVar : word `i' of `LogTix'
	*generate the log transform variable
    gen `LogVar' = log(`Ticket')
	*iterate through i by pushing forward
    local i = `i' + 1
}

*use a for loop to go through our regression to test the hypothesis
foreach LogTixs of local LogTix {
	*regress with fixed effects and robust standard errors
	regress `LogTixs' STRM Post STRMxPOST i.Year i.Genres, robust
	outreg2 using tickets_regression.tex, append tex se label
}

*Let's create a regression to look at the differences between the pre and post period for each genre, with confidence intervals
foreach genre in Americana Blues Christian Classical Country Dance Easy Family Folk Jazz Multi Pop RB Rap Reggae Tribute {
	*regression to look at the change against baseline of real max ticket price for each genre
    regress LogRTPMax Post if Genre == "`genre'", robust
	*store the estimates for the regression
    estimates store `genre'
}
*creates locals storing genres and color names to loop through
local genres "Americana Blues Christian Classical Country Dance Easy Family Folk Jazz Multi Pop RB Rap Reggae Tribute"
local colors "blue red green orange purple black cyan magenta yellow brown navy ltblue olive teal maroon gray"

*start a for loop to go through genres
foreach g of local genres {
	*use a local to go through the colors in the colors local
    local color : word `=`g'' of `colors'
	*utilize coefplot to plot the genre, dot color, and line color for the 95% confidence interval
    coefplot (`g', mcolor(`color') lcolor(`color')) ///
        , drop(_cons) vertical title("Increase In Real Max Ticket Price Post 2015") ///
        ytitle("Percent/100") xtitle("") xlabel(none) byopts(yrescale) ///
        legend(rows(2) size(small))
}












	


