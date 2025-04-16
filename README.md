# Education Completion and Fertility Analysis

This repository contains code for the analysis presented in my blog post on finishing education and fertility [https://luziabruckamp.com/2025/04/16/finishing-education-and-fertility/].

## Data Source and running the code

This analysis uses data from the National Longitudinal Survey of Youth 1997 (NLSY97), which follows individuals born between 1980-1984.

To get the data and replicate my analyses, you have to follow the steps below. The data is downloaded from the NLS Investigator website and the data cleaning and analysis is done in R. You can find the tagset and the code in the folder called ‘code’.

Go to https://www.nlsinfo.org/investigator/pages/home and click on register to create an account.
Once you’re logged in, it should prompt you to choose the study you want to work with. (If you’re not prompted automatically, click on ‘Search’ in the menu in the top right corner.) Choose NLSY97. When prompted to choose a substudy next, choose ‘NLSY97 1997-2021 (rounds 1-20)’.
You should be in the ‘Choose tagsets’ tab automatically. Now you need to upload the tagset called ‘tagset.NLSY97’ in order to choose the right variables. Once the tagset has been uploaded, you should see that there are 1,234 variables selected. Now go to the ‘Save/Download’ tab on the right and then select ‘Basic download’ in the second tab. You can change the filename to ‘nlsy97’ from ‘default’ to have the name right for the cleaning script immediately. Download the folder. (You might have to wait a few seconds for the download to become available.)
Now you are ready to run the cleaning script ‘1_nlsy_clean.R’. Make sure that you set the working directory in the R script to the folder with the data. You will be using only the csv file from the download and it should be named ‘nlsy97.csv’.
The cleaning script exports the cleaned data to the working directory with the original data that you set in the first step. Make sure that you set the same working directory in the analysis script ‘2_nlsy_analysis.R’ and then you can run this as well.

## Methodology

All the data that I’m using is from the 2021 round of the NLSY97 (round 20). You can find more information on the NLSY97 here. The survey follows Americans born in 1980-1984 over time, with the first round having been conducted 1997/1998 and the most recent available round having been conducted in 2021/2022. There are 6,713 respondents who actually participated in round 20 and therefore have a non-zero sampling weight.

For all respondents, I count the number of months that they spent in each of the education categories (2-year college, 4-year college or graduate program). I define the respondent’s highest education as the highest education that they were ever enrolled in, regardless of how long they were enrolled and whether they finished that education. I define the date at which they finish their education as the last month in which they were enrolled in the highest education category that they ever enrolled in.

Throughout the analysis, I use the sample weights from round 20. These are designed so that they should make the results representative of the American population born 1980-1984. The sample weights are integers with an implied precision of 2 decimal points. I therefore divide by 100 to get the more intuitive sampling weight which gives us the number of people in the population that a respondent is supposed to represent.

Age-specific fertility rates (ASFR) are defined as the ratio of births to exposure in a certain age group. That means the numerator counts all births to women in that age group and the denominator counts the person-years lived in that age group. For example, if we use 5-year age groups, as is common, and we observe 100 women living through the entire five years of that age group, that would give us 500 person years.

I use the age groups 15-19, 20-24, 25-29, 30-34, 35-39, and 40-44. The exposure, i.e. the number of person-years in each age group, is just five times the number of women in the sample for the age groups up to 34 since we observe all women in the sample at all these ages. For the age groups 35-39 and 40-44, we have to more carefully count the number of person-years based on when the respondents were born and when they were last interviewed since they might for example be 38 and therefore have only been exposed for four instead of five years in the 35-39 age group and for zero years in the 40-44 age group. The code deals with this by using the interview month and year for each respondent to calculate their exact exposure.

To count the births at each age, I combine the birth dates of all children with the birth dates of the respondents to calculate the respondents’ age at each birth.

I also calculate ASFR for men in the sample, using the same methodology.
