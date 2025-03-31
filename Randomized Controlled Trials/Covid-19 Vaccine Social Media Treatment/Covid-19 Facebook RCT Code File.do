*Clear any previous dataset
clear
*Generate 5,000 observations, 1 per unique id in trial
set obs 5000
*Generate id to be equal to observation since only one observation for each right now
gen id = _n
*Generate a gender variable with effectively equal chance of being either one
gen male = runiform() < 0.5

*Set seed
set seed 1

*Create a uniform random variable (0,1) interval
gen u = runiform()

*Create local macros of variable names, along with boundary markers for probability
local vars race political_leaning news_modality education // variables
local categories_race "0.616 0.804 0.925 0.984 0.991 0.993 1" // boundary values of u
local categories_political_leaning "0.45 0.91 1" // boundary values of u 
local categories_news_modality "0.33 0.59 0.86 0.99 1" // boundary values of u
local categories_education "0.089 0.316 0.538 0.643 0.826 0.932 1" // boundary values of u

*Start for loop for each variable
foreach var in race political_leaning news_modality education {
	*Initialize it as missing (to then replace)
    gen `var' = .
	*Create a local macro of the cutoff points
    local cutoffs : word count `categories_`var'' 
	*Start a forval loop going through the number of boundaries
    forvalues i = 1/`cutoffs' {
		*Create local to choose the proper categories_x variable (correct x)
        local p = word("`categories_`var''", `i')
		*Set the variable value if u is beneath p, and we avoid over-writing by using missing
        replace `var' = `i' if u <= `p' & missing(`var')
    }
}

*Define labels to make dataset nice
label define race_lbl 1 "White" 2 "Hispanic or Latino" 3 "Black or African American" 4 "Asian" ///
                      5 "American Indian or Alaska Native" 6 "Native Hawaiian or Other Pacific Islander" ///
                      7 "Two or More Races"
label values race race_lbl

label define political_leaning_lbl 1 "Democrat" 2 "Republican" 3 "Independent/Other"
label values political_leaning political_leaning_lbl

label define news_modality_lbl 1 "Television" 2 "Online News Websites" 3 "Social Media" ///
                              4 "Radio" 5 "Print Newspapers"
label values news_modality news_modality_lbl

label define education_lbl 1 "No High School Diploma" 2 "High School Graduate" ///
                          3 "Some College, No Degree" 4 "Associate's Degree" ///
                          5 "Bachelor's Degree" 6 "Master's Degree" 7 "Doctoral or Professional Degree"
label values education education_lbl

*Get rid of the uniform variable
drop u

*Set seed
set seed 1

*Create income variable meaned at $50k, with SD $50k to allow for some variation for higher and lower incomes
gen income = rnormal(50000, 50000)

*Set to zero if it became negative as that is of course non-sensical
replace income = 0 if income < 0

*Set seed
set seed 1

*Create normal random variable ~ N(38,15)
gen age = round(rnormal(38, 15))

*Make sure age is between 18 and 100
replace age = max(age, 18)
replace age = min(age, 100)

*Variable generation to create dummy variables for covariates

*Create local macro for education variable names
local education NoHS HSGrad SomeColl AA BA MA
*Local macro for values of the underlying variable to condition it on
local values 1 2 3 4 5 6
*Start local macro for i to push through values macro
local i = 1
*Use for loop to go through education local
foreach educ of local education {
	*Take word i of the values macro
	local value : word `i' of `values'
	*Create the education variable = 1 if education is equal to that value
	gen `educ' = (education == `value')
	local i = `i' + 1
}

*The same exact process follows from above for the next three loops
* (1) Local macro for variable names
* (2) Local macro for underlying variable values to condition
* (3) Start Local macro for i
* (4) Loop through each variable, conditioning on the value and pushing through

local political Dem Rep Ind
local values 1 2 3
local i = 1
foreach politic of local political {
	local value : word `i' of `values'
	gen `politic' = (political_leaning == `value')
	local i = `i' + 1
}

local news Television Online SocialMedia Radio Newspaper
local values 1 2 3 4 5
local i = 1
foreach new of local news {
	local value : word `i' of `values'
	gen `new' = (news_modality == `value')
	local i = `i' + 1
}

local races White Hispanic Black Asian AInd NHOIP
local values 1 2 3 4 5 6
local i = 1
foreach race of local races {
	local value : word `i' of `values'
	gen `race' = (race == `value')
	local i = `i' + 1
}

*Save our baseline dataset
save baseline.dta, replace

*Create a variable that is our weighted likelihood variable, our dependent variable of interest
gen vaccine_likelihood = ///
    0.05 * White + 0.05 * Hispanic + 0.05 * Black + 0.05 * Asian + 0.05 * AInd + 0.05 * NHOIP + ///
    0.025 * NoHS + 0.025 * HSGrad + 0.025 * SomeColl + 0.025 * AA + 0.025 * BA + 0.025 * MA + ///
    0.05 * Dem + 0.05 * Rep + 0.05 * Ind + ///
    0.02 * Television + 0.02 * Online + 0.02 * SocialMedia + 0.02 * Radio + 0.02 * Newspaper + ///
    0.10 * age + 0.10 * income + 0.1 * male
	
graph box vaccine_likelihood, over(news_modality, label(labsize(small))) title("Vaccine Likelihood by News Modality") ytitle("Vaccine Likelihood Statistic")
graph export "vaxxlikelihood.png", replace

*Add the endline observations for each id
expand 2

*Set order
bysort id: gen obs_num = _n

*Give time t = 0 if pre-treatment, and t = 1 if post
bysort id: gen time = cond(obs_num == 1, 0, 1)

*Drop the variable created above
drop obs_num

*Generate treatment variables

local treatments treatlogic treatemotion control
foreach treatment of local treatments {
	gen `treatment' = 0
}
*Set seed
set seed 1

*Create uniform variable
gen rand = runiform()

*Random assignment of treatments to individuals
replace treatlogic  = 1 if time == 1 & rand < 1/3
replace treatemotion = 1 if time == 1 & rand >= 1/3 & rand < 2/3
replace control = 1 if time == 1 & rand >= 2/3

*Get rid of the uniform variable
drop rand

*Create tempfile of current dataset
tempfile orig
save `orig', replace

*Create IDs dataset
preserve
keep id
duplicates drop id, force

*Take a randomized sample of 500 ids to drop due to sample attrition
set seed 1
sample 500, count

*Give them a variable to mark them to be dropped
gen drop_id = 1

*Save the IDs
tempfile dropids
save `dropids', replace
restore

*Merge back the datasets
merge m:1 id using `dropids'

*Get rid of observations if drop_id == 1

drop if drop_id == 1

*Get rid of the merge variables
drop drop_id _merge

*Create local macros for treatments and shocks
local groups "treatlogic treatemotion control"
local means "5 10 0"

*Create for loop to give shocks to treatments 
local i = 1
foreach group in `groups' {
    local mean : word `i' of `means'
    replace vaccine_likelihood = vaccine_likelihood + rnormal(`mean',2) if time == 1 & `group' == 1
    local ++i
}

save endline.dta, replace

*Set panel ID
xtset id time

*HAUSMAN TEST
xtreg vaccine_likelihood treatemotion treatlogic age income news_modality political_leaning, fe
estimates store fe_model

xtreg vaccine_likelihood treatemotion treatlogic age income news_modality political_leaning, re
estimates store re_model

hausman fe_model re_model, sigmamore

*RANDOM EFFECTS REGRESSION
xtreg vaccine_likelihood treatemotion treatlogic age income news_modality political_leaning education, re vce(cluster id) // cluster on id

*save into latex format
outreg2 using "vaccine.tex", replace




