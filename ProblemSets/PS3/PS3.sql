--Read initial CSV
.mode csv;
.import /DScourseS19/ProblemSets/PS3/FL_insurance_sample.csv FLInsur;
--Print out the first ten rows
SELECT * FROM FLInsur LIMIT 10;
--List out the counties in the sample
SELECT county FROM FLInsur;
--Compute average property appreciation from 2011-2012
SELECT AVG(tiv_2012 - tiv_2011) FROM FLInsur;
--Create a frequency table of the construction variable to see what fraction
--of buildings are made out of wood or some other material
SELECT construction, COUNT(*) FROM FLInsur GROUP BY construction;
