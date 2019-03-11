Analytics&gt;Forward 2019
================
Rick Pack (VP of Research Triangle Analysts)
March 10, 2019

"Analytics&gt;Forward" (2019) - 5th annual unconference by Research Triangle Analysts
=====================================================================================

"Pitch a talk, hear some talks, eat good food" \#AnalyticsForward <http://bit.ly/AF2019Signup>

This page's data visualizations will be updated daily.

They primarily depict Meetup Registrations for Analytics&gt;Forward, with the purpose of indicating sell-out potential and to help the planning comittee with elements including the catering order.

Thank you to R-Ladies for the meetupr package.

Keynote
-------

![Marketplace.org article - Jordan Meyer (Zillow)](Marketplace_Zillow_JordanMeyer.png)

Charts
------

![Chart 1](af_2019-1.png) ![Chart 2](af_2019-2.png) ![Chart 3](af_2019-3.png) ![Chart 4](af_2019-4.png) ![Chart 5](af_2019-5.png) ![Chart 6](af_2019-6.png) ![Chart 7](af_2019-7.png)

Week of event average Registrations per day
-------------------------------------------

### Prior to the current year and after 2015 (first year, lower Registrations)

| weekday\_rsvp |  Average\_RSVP\_weekday|
|:--------------|-----------------------:|
| Sun           |               3.0000000|
| Mon           |               8.3333333|
| Tue           |               7.6666667|
| Wed           |               4.3333333|
| Thu           |               6.0000000|
| Fri           |               7.3333333|
| Sat           |               0.6666667|

### Current year

| weekday\_rsvp |  RSVP\_weekday|
|:--------------|--------------:|
| Sun           |              5|
| Mon           |              6|
| Tue           |              5|
| Wed           |              6|
| Thu           |              7|
| Fri           |             15|
| Sat           |              1|

Highest Performing Days (Registrations)
---------------------------------------

### All years - 5 Highest Performing Days

| name                                                                  | Meetup\_Start\_Date | Registration\_Date | Weekday |  Registration\_Count|
|:----------------------------------------------------------------------|:--------------------|:-------------------|:--------|--------------------:|
| Analytics&gt;Forward 2019                                             | 2019-01-31          | 2019-01-31         | Thu     |                   19|
| Analytics&gt;Forward, with Keynote by John Sall, EVP of SAS Institute | 2016-01-29          | 2016-01-29         | Fri     |                   17|
| Analytics&gt;Forward 2019                                             | 2019-01-31          | 2019-03-08         | Fri     |                   16|
| Analytics&gt;Forward, with Keynote by John Sall, EVP of SAS Institute | 2016-01-29          | 2016-02-26         | Fri     |                   11|
| Analytics&gt;Forward, with Keynote by John Sall, EVP of SAS Institute | 2016-01-29          | 2016-03-07         | Mon     |                   11|
| Analytics&gt;Forward 2018                                             | 2018-01-10          | 2018-03-07         | Wed     |                   11|
| Analytics&gt;Forward 2018                                             | 2018-01-10          | 2018-03-08         | Thu     |                   11|

### Current year - 5 Highest Performing Days

| name                      | Meetup\_Start\_Date | Registration\_Date | Weekday |  Registration\_Count|
|:--------------------------|:--------------------|:-------------------|:--------|--------------------:|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-01-31         | Thu     |                   19|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-03-08         | Fri     |                   16|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-02-26         | Tue     |                   10|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-02-27         | Wed     |                    9|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-02-28         | Thu     |                    9|
| Analytics&gt;Forward 2019 | 2019-01-31          | 2019-03-07         | Thu     |                    9|

Total Registrations for Analytics&gt;Forward per year
-----------------------------------------------------

    ## [1] "As of  2019-03-11 06:52:47"

| name                                                                  |  Year|  Total\_Registrations|
|:----------------------------------------------------------------------|-----:|---------------------:|
| Analytics Forward - An Unconference                                   |  2015|                    42|
| Analytics&gt;Forward, with Keynote by John Sall, EVP of SAS Institute |  2016|                   124|
| Analytics&gt;Forward                                                  |  2017|                   110|
| Analytics&gt;Forward 2018                                             |  2018|                   161|
| Analytics&gt;Forward 2019                                             |  2019|                   156|

Current Year Analytics&gt;Forward Registration Link
===================================================

[Current Year Meetup Link](http://bit.ly/AF2019Signup "Analytics>Forward 2019 Meetup page")

For future collaborators
========================

To facilitate engagement by potential collaborators, the following discusses available files.

Loading the R Project
---------------------

After installing R and R-Studio, fork this repo by clicking fork at the top-right. Then follow [these instructions from the RStudio web site](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) to create a new project that is a clone of this version-controlled (think Github) repository. In the future, you can then double-click the AnalyticsForward\_2019.Rproj file to open RStudio with the files of this project being easily accessible.

![R Project image](Rproj_image.PNG)

Files
-----

1.  Readme.Rmd creates what appears on the Github page, including this text. +One
2.  Meetup\_RSVP\_Yes\_latest.R contains the Meetup\_RSVP\_Yes\_Count() function. You can use Readme.Rmd to understand how to use it.
3.  The .csv files contain data one could use with any program for analysis.
    -   Chuck Jaeger used Tableau to create this [Tableau storyboard](https://public.tableau.com/profile/chuck.jaeger#!/vizhome/AnalyticsForward2019/Storyboard).
    -   Related, although he used the JMP Meetup API, Xan Gregg created images including [this one on Twitter](https://twitter.com/xangregg/status/1104359495059337217). Xan inspired many updates to the data visualizations presented here and Chuck also provided some thoughts, including the key stimulus that began all my early 2019 work.
4.  animate\_sponsor\_coord.R is a quick-reference for stitching together images into an animated .GIF
5.  AF\_Pres.Rpres is a Xaringan presentation that has obvious imperfections I would love for others to help me fix.
6.  The RTAgrp .html files are what I called "Easter eggs" in my A&gt;F presentation. These can be opened in a web browser to see the use of [Joshua Kunst's](https://twitter.com/jbkunst) highcharter package, inspired by the work of 2018 keynoter, [Mara Averick](http://rpubs.com/maraaverick/470388).
7.  Other files are images used in the Analytics&gt;Forward presentation I did with Dr. Zeydy Ortiz. You can her gganimate() use for RSVPs at:
    -   <http://rpubs.com/zortiz/AnimatePlots>
    -   code available at <https://github.com/DrZeydy/share/blob/master/RSVP-animate.R>

Special Notes
-------------

Make sure you get your Meetup Key and place that value here in README.Rmd or before you run the Meetup\_RSVP\_Yes\_Count() function, or you will see an Error concerning the Meetup Key environment variable:

![Meetup Key Image](meetup_key_image.PNG "Must run this line as an uncommented line using your Meetup API Key.")

You might also consult the [R-Ladies meetupr package on Github](https://github.com/rladies/meetupr).

More images
-----------

Plenty of \#AnalyticsForward images are available on Twitter and LinkedIn via \#AnalyticsForward as well as this Facebook album: (<http://bit.ly/2O126hX>)
