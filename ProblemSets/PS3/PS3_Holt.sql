--Read initial CSV
CREATE TABLE FLInsur(
  "policyID",
  "statecode",
  "county",
  "eq_site_limit",
  "hu_site_limit",
  "fl_site_limit",
  "fr_site_limit",
  "tiv_2011",
  "tiv_2012",
  "eq_site_deductible",
  "hu_site_deductible",
  "fl_site_deductible",
  "fr_site_deductible",
  "point_latitude",
  "point_longitude",
  "line,construction",
  "point_granularity" 
);
.mode csv
.import FL_insurance_sample.csv FLInsur
--Print out the first ten rows
SELECT * FROM FLInsur LIMIT 10;
--List out the counties in the sample
SELECT county FROM FLInsur;
--Compute average property appreciation from 2011-2012
SELECT AVG(tiv_2012 - tiv_2011) FROM FLInsur;
--Create a frequency table of the construction variable to see what fraction
--of buildings are made out of wood or some other material
SELECT construction, COUNT(*) FROM FLInsur GROUP BY construction;
