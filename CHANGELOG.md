# Changelog

## v1.1.5 (25/02/2020)

#### Bug Fixes:

- [#183](https://github.com/martingerdin/bengaltiger/issues/183) Replace rowr dependency 

---

## v1.1.4 (05/12/2019)

#### Bug Fixes:

- [#179](https://github.com/martingerdin/bengaltiger/issues/179) Make sure event proportion is maintained when splitting dataset using events 
- [#178](https://github.com/martingerdin/bengaltiger/issues/178) Prevent SplitDataset from changing column types when splitting using events 
- [#156](https://github.com/martingerdin/bengaltiger/issues/156) CreateFlowchart fails 

---

## v1.1.3 (28/11/2019)

#### Bug Fixes:

- [#175](https://github.com/martingerdin/bengaltiger/issues/175) Implement sample.names in SplitDataset 

---

## v1.1.2 (28/11/2019)

#### Bug Fixes:

- [#174](https://github.com/martingerdin/bengaltiger/issues/174) Fix events argument checking in SplitDataset 

---

## v1.1.1 (28/11/2019)

#### Enhancements:

- [#170](https://github.com/martingerdin/bengaltiger/issues/170) Add only.codebook.variables to CreateSampleCharacteristicsTable 

#### Bug Fixes:

- [#171](https://github.com/martingerdin/bengaltiger/issues/171) Fix how variables missing from the codebook are assigned labels 
- [#169](https://github.com/martingerdin/bengaltiger/issues/169) Remove object column from split dataset 
- [#168](https://github.com/martingerdin/bengaltiger/issues/168) Abbreviations and explanations are not exported to file from CreateSampleCharacteristicsTable 

---

## v1.1.0 (25/11/2019)

#### New Functions

- [#167](https://github.com/martingerdin/bengaltiger/issues/167) SplitDataset 

---

## v1.0.3 (25/11/2019)

#### Enhancements:

- [#166](https://github.com/martingerdin/bengaltiger/issues/166) Recommend use of version lables until milestones can be created with ghi 
- [#165](https://github.com/martingerdin/bengaltiger/issues/165) Update complete workflow to include merge of master into develop after releasing a new version 

---

## v1.0.2 (21/11/2019)

#### Enhancements:

- [#163](https://github.com/martingerdin/bengaltiger/issues/163) Update workflow to get gren to get it right 
- [#162](https://github.com/martingerdin/bengaltiger/issues/162) Update complete workflow in CONTRIBUTING 

---

## v1.0.1 (21/11/2019)

#### Enhancements:

- [#161](https://github.com/martingerdin/bengaltiger/issues/161) Modify README 
- [#160](https://github.com/martingerdin/bengaltiger/issues/160) Change from - to : as recommended pratice in commit messages 
- [#159](https://github.com/martingerdin/bengaltiger/issues/159) Add use of --no-ff back into CONTRIBUTING 
- [#158](https://github.com/martingerdin/bengaltiger/issues/158) Simplify workflow in CONTRIBUTING 
- [#157](https://github.com/martingerdin/bengaltiger/issues/157) Split README into README and CONTRIBUTING 
- [#149](https://github.com/martingerdin/bengaltiger/issues/149) Replace read.csv with fread in ImportStudyData 
- [#148](https://github.com/martingerdin/bengaltiger/issues/148) Add argument to return labels rather than values from GetRevisedTraumaScoreComponents 
- [#142](https://github.com/martingerdin/bengaltiger/issues/142) Add argument to allow users to return sample characteristics table as data.frame 
- [#140](https://github.com/martingerdin/bengaltiger/issues/140) Add missing data count in each variable to last exclusion node if complete.cases in CreateFlowchart 
- [#139](https://github.com/martingerdin/bengaltiger/issues/139) Return abbreviations with table from CreateSampleCharacteristicsTable 
- [#131](https://github.com/martingerdin/bengaltiger/issues/131) Add argument to return incomplete data from CreateStudySample 
- [#129](https://github.com/martingerdin/bengaltiger/issues/129) Use codebook to make sample characteristics tables pretty 
- [#116](https://github.com/martingerdin/bengaltiger/issues/116) Change exclusion text in flowchart 
- [#115](https://github.com/martingerdin/bengaltiger/issues/115) Add option to change font in CreateFlowchart 

#### Bug Fixes:

- [#155](https://github.com/martingerdin/bengaltiger/issues/155) Fix undefined columns selected error caused by OnlyHemothoraxPatients 
- [#154](https://github.com/martingerdin/bengaltiger/issues/154) Correct comments in OnlyBluntTraumaPatients 
- [#150](https://github.com/martingerdin/bengaltiger/issues/150) Fix error in CreateSampleCharacteristicsTable when calculating % of total sample when data is multiple imputed 
- [#146](https://github.com/martingerdin/bengaltiger/issues/146) Fix error handling of flowchart.elements in CreateFlowchart 
- [#137](https://github.com/martingerdin/bengaltiger/issues/137) Add missing to OnlyTransferredPatients text 
- [#135](https://github.com/martingerdin/bengaltiger/issues/135) Throw warning if a variable is not in the codebook when creating sample characteristics table 
- [#130](https://github.com/martingerdin/bengaltiger/issues/130) Fix error when producing the manual caused by ≥ 

#### New Functions

- [#153](https://github.com/martingerdin/bengaltiger/issues/153) OnlyHemothoraxPatients 
- [#134](https://github.com/martingerdin/bengaltiger/issues/134) OnlyTransferredPatients 
- [#133](https://github.com/martingerdin/bengaltiger/issues/133) OnlyPatientsPresentingAfter 
- [#132](https://github.com/martingerdin/bengaltiger/issues/132) OnlyPatientsPresentingBefore 
- [#126](https://github.com/martingerdin/bengaltiger/issues/126) OnlyAdults 
- [#70](https://github.com/martingerdin/bengaltiger/issues/70) CreateFlowchart 

---

## v1.0.0 (21/11/2019)

#### Enhancements:

- [#141](https://github.com/martingerdin/bengaltiger/issues/141) Add argument to allow user to choose to return raw or pretty table 
- [#136](https://github.com/martingerdin/bengaltiger/issues/136) Add argument to change name of missingness and exclusions result in CreateStudySample 
- [#121](https://github.com/martingerdin/bengaltiger/issues/121) Add strata argument to CreateBootstrapSamples 
- [#120](https://github.com/martingerdin/bengaltiger/issues/120) Add names to bootstrap samples generated by CreateBootstrapSamples 
- [#119](https://github.com/martingerdin/bengaltiger/issues/119) Rewrite CreateBootstrapSamples to conform to project style 
- [#117](https://github.com/martingerdin/bengaltiger/issues/117) Add option to return bootstrap samples instead of saving to disk 
- [#111](https://github.com/martingerdin/bengaltiger/issues/111) Add cross-validation to CreateFlowchart 
- [#58](https://github.com/martingerdin/bengaltiger/issues/58) Add package documentation 

#### Bug Fixes:

- [#138](https://github.com/martingerdin/bengaltiger/issues/138) Fix strange argument is of length zero error when using codebook argument in CreateSampleCharacteristicsTable 

#### New Functions

- [#127](https://github.com/martingerdin/bengaltiger/issues/127) OnlyEmergencyAdmission 
- [#114](https://github.com/martingerdin/bengaltiger/issues/114) AddAISSeverityIndicator 
- [#113](https://github.com/martingerdin/bengaltiger/issues/113) AddTimeBetweenArrivalAndDischarge 

---

## v0.0.30.9000 (29/03/2019)

#### Bug Fixes:

- [#110](https://github.com/martingerdin/bengaltiger/issues/110) Characteristic column comes out formatted weird with lots of ... 

#### New Functions

- [#108](https://github.com/martingerdin/bengaltiger/issues/108) AddSpecificInjuryVariable 

---

## v0.0.29.9000 (12/03/2019)

#### Bug Fixes:

- [#106](https://github.com/martingerdin/bengaltiger/issues/106) Fix error in CreateSampleCharacteristicsTable, replace raw.table.data with table.data 
- [#103](https://github.com/martingerdin/bengaltiger/issues/103) Fix author names in description 

---

## v0.0.28.9000 (12/03/2019)

#### Enhancements:

- [#102](https://github.com/martingerdin/bengaltiger/issues/102) Have CreateSampleCharacteristicsTable use table.name to name file when saved to disk 

---

## v0.0.27.9000 (10/03/2019)
*No changelog for this release.*

---

## v.0.0.26.9000 (08/03/2019)

#### Enhancements:

- [#98](https://github.com/martingerdin/bengaltiger/issues/98) Place (median [IQR]) in column header if only quantitative variables are reported by CreateSampleCharacteristicsTable 
- [#97](https://github.com/martingerdin/bengaltiger/issues/97) Place n in column header if only overall table is created by CreateSampleCharacteristicsTable 
- [#96](https://github.com/martingerdin/bengaltiger/issues/96) Indicate in sample characteristics table caption that data is multiple imputed 
- [#95](https://github.com/martingerdin/bengaltiger/issues/95) Remove levels column in sample characteristics table if only quantitative data is reported 
- [#93](https://github.com/martingerdin/bengaltiger/issues/93) Add message to CreateSampleCharacteristicsTable if data detected as multiple imputed 

#### Bug Fixes:

- [#100](https://github.com/martingerdin/bengaltiger/issues/100) .complete is not added to variables in CreateSampleCharacteristicsTable 
- [#94](https://github.com/martingerdin/bengaltiger/issues/94) Remove original data from study.sample when producing sample characteristics table of multiple imputed data 

---

## v.0.0.25.9000 (07/03/2019)

#### Enhancements:

- [#92](https://github.com/martingerdin/bengaltiger/issues/92) Make CreateSampleCharacteristicsTable handle multiple imputed data 
- [#91](https://github.com/martingerdin/bengaltiger/issues/91) Update LogisticRegression 
- [#87](https://github.com/martingerdin/bengaltiger/issues/87) Update CreateLogisticRegressionTable to report both unadjusted and adjusted estimates 

---

## v0.0.24 (02/03/2019)

#### Enhancements:

- [#77](https://github.com/martingerdin/bengaltiger/issues/77) Add argument to allow custom sample characteristics table name 
- [#74](https://github.com/martingerdin/bengaltiger/issues/74) Add script to update documentation 
- [#73](https://github.com/martingerdin/bengaltiger/issues/73) Create PDF manual 
- [#71](https://github.com/martingerdin/bengaltiger/issues/71) Modify how inclusions and exclusions are saved to results 
- [#54](https://github.com/martingerdin/bengaltiger/issues/54) Save results to .results 

#### Bug Fixes:

- [#79](https://github.com/martingerdin/bengaltiger/issues/79) CreateSampleCharacteristicsTable do not report all numeric variables as non-normal 
- [#76](https://github.com/martingerdin/bengaltiger/issues/76) SourceAdditionalFiles function.files.paths is a vector but should be a list 
- [#69](https://github.com/martingerdin/bengaltiger/issues/69) Add digits to table.options in LogisticRegression 

#### New Functions

- [#90](https://github.com/martingerdin/bengaltiger/issues/90) ICDVariables 
- [#88](https://github.com/martingerdin/bengaltiger/issues/88) EstimateInHospitalMortality 
- [#86](https://github.com/martingerdin/bengaltiger/issues/86) CreateLogisticRegressionSubTable 
- [#85](https://github.com/martingerdin/bengaltiger/issues/85) CreateBootstrapSamples 
- [#84](https://github.com/martingerdin/bengaltiger/issues/84) AddTriageRevisedTraumaScore 
- [#83](https://github.com/martingerdin/bengaltiger/issues/83) AddTimeToFirstVitals 
- [#82](https://github.com/martingerdin/bengaltiger/issues/82) AddTimeBetweenInjuryAndArrival 
- [#81](https://github.com/martingerdin/bengaltiger/issues/81) Add24HoursInHospitalMortality 
- [#80](https://github.com/martingerdin/bengaltiger/issues/80) AISVariables 
- [#75](https://github.com/martingerdin/bengaltiger/issues/75) OnlyAdolescentsAndYoungAdults 
- [#72](https://github.com/martingerdin/bengaltiger/issues/72) CompileResults 
- [#68](https://github.com/martingerdin/bengaltiger/issues/68) Add SaveToResults 
- [#67](https://github.com/martingerdin/bengaltiger/issues/67) Add CreateLogisticRegressionTable 
- [#66](https://github.com/martingerdin/bengaltiger/issues/66) Add LogisticRegression 
- [#60](https://github.com/martingerdin/bengaltiger/issues/60) SourceAdditionalFunctions 

---

## v0.0.23.9000 (02/11/2018)

#### Enhancements:

- [#65](https://github.com/martingerdin/bengaltiger/issues/65) Add label so that gren ignores certain issues when creating release notes 

#### Bug Fixes:

- [#63](https://github.com/martingerdin/bengaltiger/issues/63) knitr and tableone are not loaded 

---

## v0.0.22.9000 (02/11/2018)

#### Bug Fixes:

- [#62](https://github.com/martingerdin/bengaltiger/issues/62) Fix error in CreateSampleCharacteristicsTable error handling 

---

## v0.0.21.9000 (10/10/2018)

#### Bug Fixes:

- [#57](https://github.com/martingerdin/bengaltiger/issues/57) Update function documentation 
- [#56](https://github.com/martingerdin/bengaltiger/issues/56) Fix syntax errors in AddTraumaticBrainInjury and OnlyPediatricPatients 

#### New Functions

- [#53](https://github.com/martingerdin/bengaltiger/issues/53) OnlyIsolatedTraumaticBrainInjuryPatients 
- [#51](https://github.com/martingerdin/bengaltiger/issues/51) EstimateTraumaticBrainInjuryProportion 
- [#46](https://github.com/martingerdin/bengaltiger/issues/46) AddTraumaticBrainInjury 

---

## v0.0.20.9000 (09/10/2018)

#### Enhancements:

- [#48](https://github.com/martingerdin/bengaltiger/issues/48) Add ISS to default variables to include in study sample 
- [#43](https://github.com/martingerdin/bengaltiger/issues/43) Update README to include R package version update 
- [#37](https://github.com/martingerdin/bengaltiger/issues/37) Switch to ghi for command line issue management 
- [#25](https://github.com/martingerdin/bengaltiger/issues/25) Add function to import NTDB study data from mysql database 

#### New Functions

- [#49](https://github.com/martingerdin/bengaltiger/issues/49) MergeRoadTrafficInjuryCategories 
- [#47](https://github.com/martingerdin/bengaltiger/issues/47) CreateSampleCharacteristicsTable 
- [#44](https://github.com/martingerdin/bengaltiger/issues/44) Add30DayInHospitalMortality 

---

## v0.0.19.9000 (30/08/2018)

#### Enhancements:

- [#41](https://github.com/martingerdin/bengaltiger/issues/41) Add steps and remaining observations to exclusions document 
- [#39](https://github.com/martingerdin/bengaltiger/issues/39) Add save missing report functionality to CreateStudySample 
- [#35](https://github.com/martingerdin/bengaltiger/issues/35) Add keep complete cases functionality to CreateStudySample 
- [#31](https://github.com/martingerdin/bengaltiger/issues/31) Change default data path in ImportStudyData 
- [#23](https://github.com/martingerdin/bengaltiger/issues/23) ImportTitcoMySQL 

#### Bug Fixes:

- [#38](https://github.com/martingerdin/bengaltiger/issues/38) CreateStudySample replaces exclusions file with each iteration 
- [#33](https://github.com/martingerdin/bengaltiger/issues/33) Replace \n with \n\n in OnlyPediatricPatients and OnlyPolytraumaPatients 

#### New Functions

- [#34](https://github.com/martingerdin/bengaltiger/issues/34) OnlyPolytraumaPatients 
- [#32](https://github.com/martingerdin/bengaltiger/issues/32) OnlyPediatricPatients 
- [#30](https://github.com/martingerdin/bengaltiger/issues/30) CreateStudySample 

---

## v0.0.18.9000 (28/08/2018)

#### Enhancements:

- [#18](https://github.com/martingerdin/bengaltiger/issues/18) Add RMySQL to imports 

#### New Functions

- [#12](https://github.com/martingerdin/bengaltiger/issues/12) Init 

---

## v0.0.17.9000 (23/08/2018)

#### Bug Fixes:

- [#17](https://github.com/martingerdin/bengaltiger/issues/17) Remove RunStudy.R from repository 

---

## v0.0.16.9000 (22/08/2018)

#### Enhancements:

- [#16](https://github.com/martingerdin/bengaltiger/issues/16) Relax R version dependence to 3.3 

---

## v0.0.15.9000 (22/08/2018)

#### Enhancements:

- [#15](https://github.com/martingerdin/bengaltiger/issues/15) Modify git workflow in README 

---

## v0.0.14.9000 (22/08/2018)

#### Bug Fixes:

- [#14](https://github.com/martingerdin/bengaltiger/issues/14) Remove space in full.path in CreateStudyTemplate 
- [#13](https://github.com/martingerdin/bengaltiger/issues/13) Correct first additional note 

#### New Functions

- [#11](https://github.com/martingerdin/bengaltiger/issues/11) IsLength1 
- [#10](https://github.com/martingerdin/bengaltiger/issues/10) CreateStudyTemplate 

---

## 0.0.0.9000 (22/08/2018)
*No changelog for this release.*

---

## v0.0.1.9000 (22/08/2018)
*No changelog for this release.*

---

## v0.0.2.9000 (22/08/2018)

#### Bug Fixes:

- [#1](https://github.com/martingerdin/bengaltiger/issues/1) Fix formatting of branch name headings and links in README 
