Data Preparation
================
Derek Castleman
9/1/2021

In this notebook I will take the various excel files that were collected
at the civil rights website. I will use Tidyverse to fix the data tables
changing rows into columns and renaming columns. After this I will join
them with the data from the census bureau on poverty. I will use the
proportion of students in the district to create proportions for the
different characteristics of the school districts. I will use inner
joins on some of the datatables since I want the districts that have
these features represented and left join on others (like sports) which
have some districts that do not have any of these and I do not want to
lose them.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.6     v dplyr   1.0.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Reading in Chronic Absent File and then taking a look at the data
afterwards.

``` r
chronic_absent <- read_csv("cummalitiveabsent.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Sex = col_character(),
    ##   Total = col_double()
    ## )

``` r
chronic_absent
```

    ## # A tibble: 16,862 x 5
    ##    `Lea State` LEA                                    ID Sex   Total
    ##    <chr>       <chr>                               <dbl> <chr> <dbl>
    ##  1 MA          Pembroke                          2509420 M       315
    ##  2 MA          Pembroke                          2509420 F       286
    ##  3 VA          CUMBERLAND CO PBLC SCHS           5101080 M       113
    ##  4 VA          CUMBERLAND CO PBLC SCHS           5101080 F       110
    ##  5 VA          WILLIAMSBURG-JAMES CITY PBLC SCHS 5104020 M       700
    ##  6 VA          WILLIAMSBURG-JAMES CITY PBLC SCHS 5104020 F       539
    ##  7 MA          Walpole                           2511970 M       123
    ##  8 MA          Walpole                           2511970 F        97
    ##  9 MA          Chelmsford                        2503510 M       156
    ## 10 MA          Chelmsford                        2503510 F       163
    ## # ... with 16,852 more rows

Grouping the schools together.

``` r
chronic_absent_group <- group_by(chronic_absent, LEA, ID)
chronic_absent_group
```

    ## # A tibble: 16,862 x 5
    ## # Groups:   LEA, ID [8,431]
    ##    `Lea State` LEA                                    ID Sex   Total
    ##    <chr>       <chr>                               <dbl> <chr> <dbl>
    ##  1 MA          Pembroke                          2509420 M       315
    ##  2 MA          Pembroke                          2509420 F       286
    ##  3 VA          CUMBERLAND CO PBLC SCHS           5101080 M       113
    ##  4 VA          CUMBERLAND CO PBLC SCHS           5101080 F       110
    ##  5 VA          WILLIAMSBURG-JAMES CITY PBLC SCHS 5104020 M       700
    ##  6 VA          WILLIAMSBURG-JAMES CITY PBLC SCHS 5104020 F       539
    ##  7 MA          Walpole                           2511970 M       123
    ##  8 MA          Walpole                           2511970 F        97
    ##  9 MA          Chelmsford                        2503510 M       156
    ## 10 MA          Chelmsford                        2503510 F       163
    ## # ... with 16,852 more rows

Collapse the chronic absent data down to just one total for the entire
school district.

``` r
final_chronic_absent_data <- summarise(chronic_absent_group, Total_Absent = sum(Total))
```

    ## `summarise()` has grouped output by 'LEA'. You can override using the `.groups` argument.

``` r
final_chronic_absent_data
```

    ## # A tibble: 8,431 x 3
    ## # Groups:   LEA [8,257]
    ##    LEA                                  ID Total_Absent
    ##    <chr>                             <dbl>        <dbl>
    ##  1 A-C Central CUSD 262            1700105           75
    ##  2 A.C.G.C. PUBLIC SCHOOL DISTRICT 2700106           95
    ##  3 Abbotsford School District      5500030           38
    ##  4 ABC Unified                      601620         1603
    ##  5 ABERDEEN DISTRICT               1600030           38
    ##  6 ABERDEEN SCHOOL DIST            2800360          174
    ##  7 Aberdeen School District        5300030          884
    ##  8 Aberdeen School District 06-1   4602070          592
    ##  9 ABERNATHY ISD                   4807410           57
    ## 10 Abilene                         2003180          173
    ## # ... with 8,421 more rows

Reading in the suspension data and taking a look at it.

``` r
suspension_data <- read_csv('cummalativesuspension.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_character(),
    ##   Sex = col_character(),
    ##   Total = col_double()
    ## )

``` r
suspension_data
```

    ## # A tibble: 18,494 x 5
    ##    `Lea State` LEA                    ID      Sex   Total
    ##    <chr>       <chr>                  <chr>   <chr> <dbl>
    ##  1 MA          Westford               2512660 M       121
    ##  2 MA          Westford               2512660 F         0
    ##  3 MA          Auburn                 2502220 M       181
    ##  4 MA          Auburn                 2502220 F        62
    ##  5 RI          Central Falls          4400120 M       147
    ##  6 RI          Central Falls          4400120 F        86
    ##  7 CT          SOMERS SCHOOL DISTRICT 904140  M        98
    ##  8 CT          SOMERS SCHOOL DISTRICT 904140  F        22
    ##  9 ME          RSU 40/MSAD 40         2311550 M       288
    ## 10 ME          RSU 40/MSAD 40         2311550 F       119
    ## # ... with 18,484 more rows

Grouping districts then summarizing the two genders for each district
down to one total number.

``` r
final_suspension_data <- suspension_data %>% group_by(LEA, ID) %>%
  summarise(Total_Suspended = sum(Total))
```

    ## `summarise()` has grouped output by 'LEA'. You can override using the `.groups` argument.

``` r
final_suspension_data
```

    ## # A tibble: 8,697 x 3
    ## # Groups:   LEA [8,522]
    ##    LEA                              ID      Total_Suspended
    ##    <chr>                            <chr>             <dbl>
    ##  1 A-C Central CUSD 262             1700105              54
    ##  2 A.C.G.C. PUBLIC SCHOOL DISTRICT  2700106              24
    ##  3 Abbeville County School District 4500690            1363
    ##  4 Abbotsford School District       5500030              60
    ##  5 ABC Unified                      601620             1478
    ##  6 ABERDEEN DISTRICT                1600030              44
    ##  7 ABERDEEN SCHOOL DIST             2800360            1429
    ##  8 Aberdeen School District         5300030             762
    ##  9 Aberdeen School District 06-1    4602070             752
    ## 10 ABERNATHY ISD                    4807410               3
    ## # ... with 8,687 more rows

``` r
sum(is.na(final_suspension_data))
```

    ## [1] 0

Reading in the AP test data then taking a look at what it has.

``` r
ap_data <- read_csv('cummalativeap.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

    ## Warning: 6 parsing failures.
    ##   row col               expected  actual                file
    ##  3679  ID no trailing characters 06CC366 'cummalativeap.csv'
    ##  3680  ID no trailing characters 06CC366 'cummalativeap.csv'
    ##  3681  ID no trailing characters 06CC370 'cummalativeap.csv'
    ##  3682  ID no trailing characters 06CC370 'cummalativeap.csv'
    ## 11165  ID no trailing characters 24SOP02 'cummalativeap.csv'
    ## ..... ... ...................... ....... ...................
    ## See problems(...) for more details.

``` r
ap_data
```

    ## # A tibble: 16,610 x 5
    ##    `Lea State` LEA                               ID Category               Total
    ##    <chr>       <chr>                          <dbl> <chr>                  <dbl>
    ##  1 AZ          Cave Creek Unified District   400001 Enrollment in AP clas~   523
    ##  2 AZ          Cave Creek Unified District   400001 Taking AP tests for s~   524
    ##  3 AZ          Chino Valley Unified District 400003 Enrollment in AP clas~    66
    ##  4 AZ          Chino Valley Unified District 400003 Taking AP tests for s~     0
    ##  5 AZ          Ombudsman Educational Servic~ 400811 Enrollment in AP clas~     0
    ##  6 AZ          Ombudsman Educational Servic~ 400811 Taking AP tests for s~     0
    ##  7 AZ          Ash Fork Joint Unified Distr~ 400910 Enrollment in AP clas~     0
    ##  8 AZ          Ash Fork Joint Unified Distr~ 400910 Taking AP tests for s~     0
    ##  9 AZ          Bisbee Unified District       401180 Enrollment in AP clas~     0
    ## 10 AZ          Bisbee Unified District       401180 Taking AP tests for s~     0
    ## # ... with 16,600 more rows

Going to pivot the table so that each category becomes its own separate
column and rename the columns to shorter names

``` r
ap_fixed <- ap_data %>% pivot_wider(names_from = Category, values_from = Total) %>%
  rename(
    AP_Enrollment = 'Enrollment in AP classes',
    AP_Test_Takers = 'Taking AP tests for some AP courses taken'
  )
ap_fixed
```

    ## # A tibble: 8,305 x 5
    ##    `Lea State` LEA                               ID AP_Enrollment AP_Test_Takers
    ##    <chr>       <chr>                          <dbl>         <dbl>          <dbl>
    ##  1 AZ          Cave Creek Unified District   400001           523            524
    ##  2 AZ          Chino Valley Unified District 400003            66              0
    ##  3 AZ          Ombudsman Educational Servic~ 400811             0              0
    ##  4 AZ          Ash Fork Joint Unified Distr~ 400910             0              0
    ##  5 AZ          Bisbee Unified District       401180             0              0
    ##  6 AZ          Buckeye Union High School Di~ 401410           236              0
    ##  7 AZ          Sierra Vista Unified District 401460           260            195
    ##  8 AZ          Camp Verde Unified District   401600            18             16
    ##  9 AZ          Catalina Foothills Unified D~ 401760           555            371
    ## 10 AZ          Chandler Unified District #80 401870          4847           3553
    ## # ... with 8,295 more rows

Creating a final column with proportion of those enrolled in ap courses
that took the test.

``` r
ap_new_column <- mutate(ap_fixed, Takers_Proportion = AP_Test_Takers / AP_Enrollment)
ap_new_column
```

    ## # A tibble: 8,305 x 6
    ##    `Lea State` LEA              ID AP_Enrollment AP_Test_Takers Takers_Proporti~
    ##    <chr>       <chr>         <dbl>         <dbl>          <dbl>            <dbl>
    ##  1 AZ          Cave Creek ~ 400001           523            524            1.00 
    ##  2 AZ          Chino Valle~ 400003            66              0            0    
    ##  3 AZ          Ombudsman E~ 400811             0              0          NaN    
    ##  4 AZ          Ash Fork Jo~ 400910             0              0          NaN    
    ##  5 AZ          Bisbee Unif~ 401180             0              0          NaN    
    ##  6 AZ          Buckeye Uni~ 401410           236              0            0    
    ##  7 AZ          Sierra Vist~ 401460           260            195            0.75 
    ##  8 AZ          Camp Verde ~ 401600            18             16            0.889
    ##  9 AZ          Catalina Fo~ 401760           555            371            0.668
    ## 10 AZ          Chandler Un~ 401870          4847           3553            0.733
    ## # ... with 8,295 more rows

Have to fix the NaN columns to a 0 since that is what they should
actually be.

``` r
final_ap <- replace(ap_new_column, is.na(ap_new_column), 0)
final_ap
```

    ## # A tibble: 8,305 x 6
    ##    `Lea State` LEA              ID AP_Enrollment AP_Test_Takers Takers_Proporti~
    ##    <chr>       <chr>         <dbl>         <dbl>          <dbl>            <dbl>
    ##  1 AZ          Cave Creek ~ 400001           523            524            1.00 
    ##  2 AZ          Chino Valle~ 400003            66              0            0    
    ##  3 AZ          Ombudsman E~ 400811             0              0            0    
    ##  4 AZ          Ash Fork Jo~ 400910             0              0            0    
    ##  5 AZ          Bisbee Unif~ 401180             0              0            0    
    ##  6 AZ          Buckeye Uni~ 401410           236              0            0    
    ##  7 AZ          Sierra Vist~ 401460           260            195            0.75 
    ##  8 AZ          Camp Verde ~ 401600            18             16            0.889
    ##  9 AZ          Catalina Fo~ 401760           555            371            0.668
    ## 10 AZ          Chandler Un~ 401870          4847           3553            0.733
    ## # ... with 8,295 more rows

Load the testing data and then take a look at it.

``` r
testing_data <- read_csv('cummalativetesting.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_character(),
    ##   Sex = col_character(),
    ##   Total = col_double()
    ## )

``` r
testing_data
```

    ## # A tibble: 17,394 x 5
    ##    `Lea State` LEA                            ID      Sex   Total
    ##    <chr>       <chr>                          <chr>   <chr> <dbl>
    ##  1 NH          Franklin School District       3303090 M        21
    ##  2 NH          Franklin School District       3303090 F        25
    ##  3 NH          Moultonborough School District 3304960 M        15
    ##  4 NH          Moultonborough School District 3304960 F        14
    ##  5 MA          Lynnfield                      2507140 M        88
    ##  6 MA          Lynnfield                      2507140 F       122
    ##  7 MA          Medfield                       2507530 M       159
    ##  8 MA          Medfield                       2507530 F       146
    ##  9 MA          Sharon                         2510620 M       265
    ## 10 MA          Sharon                         2510620 F       251
    ## # ... with 17,384 more rows

Make sure that the districts are all grouped together then summarize the
testing data down to one value to get rid of the genders.

``` r
final_testing <- testing_data %>% group_by(LEA, ID) %>%
  summarise(Total_Test = sum(Total))
```

    ## `summarise()` has grouped output by 'LEA'. You can override using the `.groups` argument.

``` r
final_testing
```

    ## # A tibble: 8,697 x 3
    ## # Groups:   LEA [8,522]
    ##    LEA                              ID      Total_Test
    ##    <chr>                            <chr>        <dbl>
    ##  1 A-C Central CUSD 262             1700105         30
    ##  2 A.C.G.C. PUBLIC SCHOOL DISTRICT  2700106         46
    ##  3 Abbeville County School District 4500690        171
    ##  4 Abbotsford School District       5500030          0
    ##  5 ABC Unified                      601620         274
    ##  6 ABERDEEN DISTRICT                1600030         33
    ##  7 ABERDEEN SCHOOL DIST             2800360         84
    ##  8 Aberdeen School District         5300030         64
    ##  9 Aberdeen School District 06-1    4602070        472
    ## 10 ABERNATHY ISD                    4807410         40
    ## # ... with 8,687 more rows

Loading in the teaching data then looking at it to see the layout.

``` r
teaching_data <- read_csv('cummalativeteachingfixed.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_character(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

``` r
teaching_data
```

    ## # A tibble: 43,485 x 5
    ##    `Lea State` LEA       ID     Category                                   Total
    ##    <chr>       <chr>     <chr>  <chr>                                      <dbl>
    ##  1 MA          Fairhaven 25048~ Classroom Teachers                        148.  
    ##  2 MA          Fairhaven 25048~ Classroom teachers in their first year o~   0   
    ##  3 MA          Fairhaven 25048~ Classroom teachers in their second year ~   0   
    ##  4 MA          Fairhaven 25048~ Teachers absent more than 10 days of the~  16.0 
    ##  5 MA          Fairhaven 25048~ High-School Counselors                      5   
    ##  6 MA          Sudbury   25113~ Classroom Teachers                        208.  
    ##  7 MA          Sudbury   25113~ Classroom teachers in their first year o~   2   
    ##  8 MA          Sudbury   25113~ Classroom teachers in their second year ~   8   
    ##  9 MA          Sudbury   25113~ Teachers absent more than 10 days of the~ 107   
    ## 10 MA          Sudbury   25113~ High-School Counselors                      7.51
    ## # ... with 43,475 more rows

Pivot the table for the categories to be turned into columns.

``` r
teaching_fixed <- teaching_data %>% pivot_wider(names_from = Category, values_from = Total) %>%
  rename(
    Total_Teachers = 'Classroom Teachers',
    First_Year = 'Classroom teachers in their first year of teaching',
    Second_Year = 'Classroom teachers in their second year of teaching',
    Chronic_Absent_Teachers = 'Teachers absent more than 10 days of the school year',
    Counselors = 'High-School Counselors'
  )
teaching_fixed
```

    ## # A tibble: 8,697 x 8
    ##    `Lea State` LEA   ID    Total_Teachers First_Year Second_Year
    ##    <chr>       <chr> <chr>          <dbl>      <dbl>       <dbl>
    ##  1 MA          Fair~ 2504~           148.       0            0  
    ##  2 MA          Sudb~ 2511~           208.       2            8  
    ##  3 NH          Some~ 3306~           130.      15.5         16  
    ##  4 MA          Tyng~ 2511~           136.       7            5.2
    ##  5 NH          Timb~ 3306~           316.      12.6         11  
    ##  6 ME          Wins~ 2313~            97        6            4  
    ##  7 CT          EAST~ 9012~           519       40           36  
    ##  8 RI          Cent~ 4400~           243.      14            6  
    ##  9 MA          Ames~ 2501~           187.       0.69        19.5
    ## 10 CT          TOLL~ 9045~           193.       5            3  
    ## # ... with 8,687 more rows, and 2 more variables:
    ## #   Chronic_Absent_Teachers <dbl>, Counselors <dbl>

Create columns with proportion of new teachers (ones in their first and
second year) and proportion of teachers chronically absent. Then
removing any districts with an NaN because this would mean they have no
teachers (yes I found that one district had 0 teachers).

``` r
final_teaching <- mutate(teaching_fixed, New_Teachers_Proportion = (First_Year + Second_Year) / Total_Teachers, Absent_Teacher_Proportion = Chronic_Absent_Teachers / Total_Teachers)
final_teaching <- na.omit(final_teaching)
final_teaching
```

    ## # A tibble: 8,696 x 10
    ##    `Lea State` LEA   ID    Total_Teachers First_Year Second_Year
    ##    <chr>       <chr> <chr>          <dbl>      <dbl>       <dbl>
    ##  1 MA          Fair~ 2504~           148.       0            0  
    ##  2 MA          Sudb~ 2511~           208.       2            8  
    ##  3 NH          Some~ 3306~           130.      15.5         16  
    ##  4 MA          Tyng~ 2511~           136.       7            5.2
    ##  5 NH          Timb~ 3306~           316.      12.6         11  
    ##  6 ME          Wins~ 2313~            97        6            4  
    ##  7 CT          EAST~ 9012~           519       40           36  
    ##  8 RI          Cent~ 4400~           243.      14            6  
    ##  9 MA          Ames~ 2501~           187.       0.69        19.5
    ## 10 CT          TOLL~ 9045~           193.       5            3  
    ## # ... with 8,686 more rows, and 4 more variables:
    ## #   Chronic_Absent_Teachers <dbl>, Counselors <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>

Loading in the data for sports and then taking a look at what is
present.

``` r
sports_data <- read_csv('cummalativesports.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

    ## Warning: 4 parsing failures.
    ##   row col               expected  actual                    file
    ## 13289  ID no trailing characters 06CC366 'cummalativesports.csv'
    ## 13290  ID no trailing characters 06CC366 'cummalativesports.csv'
    ## 13291  ID no trailing characters 06CC370 'cummalativesports.csv'
    ## 13292  ID no trailing characters 06CC370 'cummalativesports.csv'

``` r
sports_data
```

    ## # A tibble: 14,810 x 5
    ##    `Lea State` LEA                 ID Category                         Total
    ##    <chr>       <chr>            <dbl> <chr>                            <dbl>
    ##  1 MA          Medfield       2507530 Single-sex teams                    49
    ##  2 MA          Medfield       2507530 Participants on single-sex teams   767
    ##  3 MA          Clinton        2503750 Single-sex teams                    21
    ##  4 MA          Clinton        2503750 Participants on single-sex teams   304
    ##  5 MA          Oxford         2509270 Single-sex teams                    10
    ##  6 MA          Oxford         2509270 Participants on single-sex teams   149
    ##  7 ME          RSU 37/MSAD 37 2311430 Single-sex teams                    14
    ##  8 ME          RSU 37/MSAD 37 2311430 Participants on single-sex teams    85
    ##  9 RI          Westerly       4401170 Single-sex teams                    38
    ## 10 RI          Westerly       4401170 Participants on single-sex teams   557
    ## # ... with 14,800 more rows

I am going to pivot the categories to make columns for each one and then
rename them.

``` r
final_sports <- sports_data %>% pivot_wider(names_from = Category, values_from = Total) %>%
  rename(
    Teams = 'Single-sex teams',
    Athletes = 'Participants on single-sex teams'
  )
final_sports
```

    ## # A tibble: 7,405 x 5
    ##    `Lea State` LEA                          ID Teams Athletes
    ##    <chr>       <chr>                     <dbl> <dbl>    <dbl>
    ##  1 MA          Medfield                2507530    49      767
    ##  2 MA          Clinton                 2503750    21      304
    ##  3 MA          Oxford                  2509270    10      149
    ##  4 ME          RSU 37/MSAD 37          2311430    14       85
    ##  5 RI          Westerly                4401170    38      557
    ##  6 CT          GROTON SCHOOL DISTRICT   901770    58      938
    ##  7 ME          RSU 54/MSAD 54          2314590    44      528
    ##  8 NH          Windham School District 3307170    46      606
    ##  9 RI          Providence              4400900    96     2311
    ## 10 MA          Norwell                 2509030    18      272
    ## # ... with 7,395 more rows

Importing the data for the student population and the number in poverty
for that district.

``` r
poverty_data <- read_csv('povertylevelfixed.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   State = col_double(),
    ##   ID = col_character(),
    ##   LEA = col_character(),
    ##   Area_Population = col_number(),
    ##   District_Population = col_number(),
    ##   Children_Poverty = col_number()
    ## )

``` r
poverty_data
```

    ## # A tibble: 13,222 x 7
    ##    `Lea State` State ID    LEA   Area_Population District_Popula~
    ##    <chr>       <dbl> <chr> <chr>           <dbl>            <dbl>
    ##  1 AL              1 00190 Alab~           34015             6703
    ##  2 AL              1 00005 Albe~           21786             4115
    ##  3 AL              1 00030 Alex~           17073             2659
    ##  4 AL              1 00060 Anda~            8854             1444
    ##  5 AL              1 00090 Anni~           22350             3184
    ##  6 AL              1 00100 Arab~            8305             1520
    ##  7 AL              1 00120 Athe~           24963             3948
    ##  8 AL              1 00180 Atta~            5952              927
    ##  9 AL              1 00210 Aubu~           61570             7322
    ## 10 AL              1 00240 Auta~           55504             9990
    ## # ... with 13,212 more rows, and 1 more variable: Children_Poverty <dbl>

I need to combine the State Code with the ID because that is how it is
represented in all of the other datatables that I have worked with so
far. I learned how to do this from
<https://stackoverflow.com/questions/24248029/how-to-concatenate-numeric-columns-in-r>
by user sbha at the bottom of the page.

``` r
fixed_poverty_data <- poverty_data %>%
  mutate(fixed_ID = paste0(State, ID))
fixed_poverty_data
```

    ## # A tibble: 13,222 x 8
    ##    `Lea State` State ID    LEA   Area_Population District_Popula~
    ##    <chr>       <dbl> <chr> <chr>           <dbl>            <dbl>
    ##  1 AL              1 00190 Alab~           34015             6703
    ##  2 AL              1 00005 Albe~           21786             4115
    ##  3 AL              1 00030 Alex~           17073             2659
    ##  4 AL              1 00060 Anda~            8854             1444
    ##  5 AL              1 00090 Anni~           22350             3184
    ##  6 AL              1 00100 Arab~            8305             1520
    ##  7 AL              1 00120 Athe~           24963             3948
    ##  8 AL              1 00180 Atta~            5952              927
    ##  9 AL              1 00210 Aubu~           61570             7322
    ## 10 AL              1 00240 Auta~           55504             9990
    ## # ... with 13,212 more rows, and 2 more variables: Children_Poverty <dbl>,
    ## #   fixed_ID <chr>

I am going to fix the table by deleting the columns for State and ID,
renaming the fixed\_Id so it matches the other databables and then
shifting its position to the front. I will also create a new column that
is the proportion of students that are living in poverty within the
school district (this will be the target for the machine learning model
that will be developed).

``` r
final_poverty_table <- fixed_poverty_data %>%
  select(-State, -ID) %>%
  rename(ID = fixed_ID) %>%
  select(ID, everything()) %>%
  mutate(Poverty_Proportion = Children_Poverty / District_Population, Student_Prop = District_Population / Area_Population) %>%
  select(-District_Population)
final_poverty_table
```

    ## # A tibble: 13,222 x 7
    ##    ID    `Lea State` LEA   Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>       <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL          Alab~           34015              860            0.128
    ##  2 1000~ AL          Albe~           21786             1546            0.376
    ##  3 1000~ AL          Alex~           17073              832            0.313
    ##  4 1000~ AL          Anda~            8854              386            0.267
    ##  5 1000~ AL          Anni~           22350             1106            0.347
    ##  6 1001~ AL          Arab~            8305              329            0.216
    ##  7 1001~ AL          Athe~           24963              685            0.174
    ##  8 1001~ AL          Atta~            5952              280            0.302
    ##  9 1002~ AL          Aubu~           61570              888            0.121
    ## 10 1002~ AL          Auta~           55504             1842            0.184
    ## # ... with 13,212 more rows, and 1 more variable: Student_Prop <dbl>

``` r
demo_data <- read_csv('cummalativedemo.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   ID = col_character(),
    ##   White = col_double(),
    ##   District_Population = col_double()
    ## )

``` r
demo_data
```

    ## # A tibble: 8,697 x 3
    ##    ID     White District_Population
    ##    <chr>  <dbl>               <dbl>
    ##  1 900002  4941               10803
    ##  2 900004    40                 135
    ##  3 900060   850                2271
    ##  4 900070   511                1941
    ##  5 903630   834                 912
    ##  6 903660  1695                2197
    ##  7 903720  1000                1057
    ##  8 903810  4018                4897
    ##  9 903840  1496                2693
    ## 10 903990  1647                2167
    ## # ... with 8,687 more rows

``` r
final_demo_table <- demo_data %>%
  mutate(Non_White_Students = (District_Population - White) / District_Population) %>%
  select(-White)
final_demo_table
```

    ## # A tibble: 8,697 x 3
    ##    ID     District_Population Non_White_Students
    ##    <chr>                <dbl>              <dbl>
    ##  1 900002               10803             0.543 
    ##  2 900004                 135             0.704 
    ##  3 900060                2271             0.626 
    ##  4 900070                1941             0.737 
    ##  5 903630                 912             0.0855
    ##  6 903660                2197             0.228 
    ##  7 903720                1057             0.0539
    ##  8 903810                4897             0.179 
    ##  9 903840                2693             0.444 
    ## 10 903990                2167             0.240 
    ## # ... with 8,687 more rows

I am going to do an inner join with the poverty table and the
demographics table. I am doing an inner join so that I can keep the
districts that we have demographics data for and get rid of the
districts where it does not exist. I am going to join on the ID because
this is the distinct common feature that they should share in common. I
learned how to do this from:
<https://dplyr.tidyverse.org/reference/join.html>

``` r
combined_demo_poverty <- inner_join(final_poverty_table, final_demo_table, by = 'ID')
combined_demo_poverty
```

    ## # A tibble: 8,301 x 9
    ##    ID    `Lea State` LEA   Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>       <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL          Alab~           34015              860            0.128
    ##  2 1000~ AL          Albe~           21786             1546            0.376
    ##  3 1000~ AL          Alex~           17073              832            0.313
    ##  4 1000~ AL          Anda~            8854              386            0.267
    ##  5 1000~ AL          Anni~           22350             1106            0.347
    ##  6 1001~ AL          Arab~            8305              329            0.216
    ##  7 1001~ AL          Athe~           24963              685            0.174
    ##  8 1001~ AL          Atta~            5952              280            0.302
    ##  9 1002~ AL          Aubu~           61570              888            0.121
    ## 10 1002~ AL          Auta~           55504             1842            0.184
    ## # ... with 8,291 more rows, and 3 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>

I am going to combined the new datatable with the teacher one with an
inner join since I only want districts where we have teaching data for.

``` r
combined_poverty_teacher <- inner_join(combined_demo_poverty, final_teaching, by = 'ID')
combined_poverty_teacher
```

    ## # A tibble: 8,300 x 18
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,290 more rows, and 12 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>, `Lea State.y` <chr>,
    ## #   LEA.y <chr>, Total_Teachers <dbl>, First_Year <dbl>, Second_Year <dbl>,
    ## #   Chronic_Absent_Teachers <dbl>, Counselors <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>

Create two new columns. One of them will be the number of counselors per
each student using the student population and counselors data then the
number of teachers per a student by using the total teachers and the
student population.

``` r
combined_teacher_new_data <- mutate(combined_poverty_teacher, Counselor_Ratio = Counselors / District_Population, Student_Teacher_Ratio = District_Population / Total_Teachers)
combined_teacher_new_data
```

    ## # A tibble: 8,300 x 20
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,290 more rows, and 14 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>, `Lea State.y` <chr>,
    ## #   LEA.y <chr>, Total_Teachers <dbl>, First_Year <dbl>, Second_Year <dbl>,
    ## #   Chronic_Absent_Teachers <dbl>, Counselors <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>

I am going to remove the columns that are listed twice from the inner
join as well as some of the columns no longer needed as I move forward
(number of teachers, counselors, etc.)

``` r
final_teacher_combined <- select(combined_teacher_new_data, -'Lea State.y', -LEA.y, -Total_Teachers, -First_Year, -Second_Year, -Chronic_Absent_Teachers, -Counselors)
final_teacher_combined
```

    ## # A tibble: 8,300 x 13
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,290 more rows, and 7 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>

Combine chronic abseentism with the data table using an inner join of ID
once again.Have to change ID to string first from the absent one so that
they can be combined.

``` r
class(final_chronic_absent_data$ID) = "character"
combined_with_abseentism <- inner_join(final_teacher_combined, final_chronic_absent_data, by = 'ID')
combined_with_abseentism
```

    ## # A tibble: 8,143 x 15
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 9 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, LEA <chr>,
    ## #   Total_Absent <dbl>

Going to create a new column with proportion of chronic abseentism based
off the student population then I am going to drop the LEA and
Total\_Absent column since it will no longer be needed.

``` r
final_absent_combined <- combined_with_abseentism %>%
  mutate(Absent_Prop = Total_Absent / District_Population) %>%
  select(-LEA, -Total_Absent)
final_absent_combined
```

    ## # A tibble: 8,143 x 14
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 8 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>

I am going to do another inner join with the testing data to combine
with this datatable.

``` r
combined_with_testing <- inner_join(final_absent_combined, final_testing, by = 'ID')
combined_with_testing
```

    ## # A tibble: 8,143 x 16
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 10 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   LEA <chr>, Total_Test <dbl>

Going to create a new column with proportion of test takers based off
the student population then I am going to drop the LEA and Total\_Test
since it will no longer be needed.

``` r
final_test_combined <- combined_with_testing %>%
  mutate(Test_Prop = Total_Test / District_Population) %>%
  select(-LEA, -Total_Test)
final_test_combined
```

    ## # A tibble: 8,143 x 15
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 9 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>

I am going to combine the sports data with the other combined data. This
time I am going to do a left join since I know that some districts do
not have sports and I do not want to lose those. Have to change the ID
of the sports to character to allow for it to be combined.

``` r
class(final_sports$ID) = "character"
combined_with_sports <- left_join(final_test_combined, final_sports, by = 'ID')
combined_with_sports
```

    ## # A tibble: 8,143 x 19
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 13 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, `Lea State` <chr>, LEA <chr>, Teams <dbl>, Athletes <dbl>

Turn the NA values into 0 because these are districts that do not have
any sports so their true value should be 0. I can only do this for the
numeric columns. I found out how to do this from:
<https://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns>
especially from user sbha once again.

``` r
fixed_combined_sports <- combined_with_sports %>%
  mutate_if(is.numeric, ~replace_na(., 0))
fixed_combined_sports
```

    ## # A tibble: 8,143 x 19
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 13 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, `Lea State` <chr>, LEA <chr>, Teams <dbl>, Athletes <dbl>

I will turn the teams and athletes columns into a proportion based off
the student population and then I will drop the columns that are no
longer needed.

``` r
final_sports_combined <- fixed_combined_sports %>%
  mutate(Teams_Prop = Teams / District_Population, Athletes_Prop = Athletes / District_Population) %>%
  select(-'Lea State', -LEA, -Teams, -Athletes)
final_sports_combined
```

    ## # A tibble: 8,143 x 17
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 11 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>

I am going to join the AP test data with the datatable but I will be
using a left join once again since there could be some districts that
are not represented due to not having any AP courses at all.

``` r
class(final_ap$ID) = "character"
combined_with_ap <- left_join(final_sports_combined, final_ap, by = 'ID')
combined_with_ap
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>, `Lea State` <chr>,
    ## #   LEA <chr>, AP_Enrollment <dbl>, AP_Test_Takers <dbl>,
    ## #   Takers_Proportion <dbl>

Checking to see how many missing values for AP might exist.

``` r
sum(is.na(combined_with_ap))
```

    ## [1] 1645

I am going to insert the value of 0 in for any of the number columns
that are missing AP courses. Then I will create a new column that has
the proportion of students taking AP courses within the district. Then
drop the columns that are no longer needed.

``` r
final_ap_combined <- combined_with_ap %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(AP_Prop = AP_Enrollment /District_Population) %>% 
  select(-'Lea State', -LEA, -AP_Enrollment, -AP_Test_Takers)
final_ap_combined
```

    ## # A tibble: 8,143 x 19
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 13 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>

Combine suspension data in with the other datatable. Going to use a left
join once again in case there is any data missing.

``` r
combined_with_suspension <- left_join(final_ap_combined, final_suspension_data, by = 'ID')
combined_with_suspension
```

    ## # A tibble: 8,143 x 21
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 15 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, LEA <chr>, Total_Suspended <dbl>

Check if there are any values that are null in the datatable.

``` r
sum(is.na(combined_with_suspension))
```

    ## [1] 0

These are total days suspended so I will keep the category the way that
it is. I will just drop the excess category that is no longer needed.

``` r
final_susp_combined <- combined_with_suspension %>%
  select(-LEA)
final_susp_combined
```

    ## # A tibble: 8,143 x 20
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 14 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, Total_Suspended <dbl>

I am going to load the total suspension counts to create an average with
the days suspended.

``` r
suspension_data_total <- read_csv('cummalativesuspensioncount.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   ID = col_character(),
    ##   Total_Suspension = col_double()
    ## )

``` r
suspension_data_total
```

    ## # A tibble: 8,688 x 2
    ##    ID      Total_Suspension
    ##    <chr>              <dbl>
    ##  1 3303090              156
    ##  2 3304960                2
    ##  3 3307170               25
    ##  4 2508730               40
    ##  5 4400900             2310
    ##  6 2509030               12
    ##  7 2512210               65
    ##  8 2307320              310
    ##  9 2512750               13
    ## 10 2310590               60
    ## # ... with 8,678 more rows

I am going to combine the total suspend count with the other datatable.
I am going to do a left join because the website where I gathered data
is having issues and the region that represents Chicago is having errors
that will not allow for the data to be gathered.

``` r
combined_with_suspension_count <- left_join(combined_with_suspension, suspension_data_total, by = 'ID')
combined_with_suspension_count
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, LEA <chr>, Total_Suspended <dbl>,
    ## #   Total_Suspension <dbl>

CHecking how many missing values I have now that it has been combined
with Chicago region missing.

``` r
sum(is.na(combined_with_suspension_count))
```

    ## [1] 2

I am going to replace the missing values from Chicago with the mean
values for the column to deal with the issue of not being able to obtain
the data. I learned how to do this method from:
<https://www.tutorialspoint.com/how-to-replace-na-values-in-columns-of-an-r-data-frame-form-the-mean-of-that-column>

``` r
combined_with_suspension_count$Total_Suspension[is.na(combined_with_suspension_count$Total_Suspension)]<-mean(combined_with_suspension_count$Total_Suspension,na.rm=TRUE)
combined_with_suspension_count
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, LEA <chr>, Total_Suspended <dbl>,
    ## #   Total_Suspension <dbl>

Checking if any missing values still remain.

``` r
sum(is.na(combined_with_suspension_count))
```

    ## [1] 0

Going to create a new column that creates the average days of suspension
by taking the number of days suspended divided by the suspension count.
This will allow for better comparison between districts of different
sizes. Then I will drop the columns that are no longer needed.

``` r
final_with_suspension_count <- combined_with_suspension_count %>% 
  mutate(AVG_Suspenion = Total_Suspension / District_Population) %>%
  select(-LEA, -Total_Suspended, -Total_Suspension)
final_with_suspension_count
```

    ## # A tibble: 8,143 x 20
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 14 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>

Loading the discipline data and then taking a look at it.

``` r
discipline_data <- read_csv('cummalativediscpline.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   ID = col_character(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

``` r
discipline_data
```

    ## # A tibble: 17,954 x 3
    ##    ID      Category                             Total
    ##    <chr>   <chr>                                <dbl>
    ##  1 3303090 Expulsions with educational services     0
    ##  2 3303090 Referral to law enforcement              5
    ##  3 3304960 Expulsions with educational services     0
    ##  4 3304960 Referral to law enforcement              1
    ##  5 3307170 Expulsions with educational services     0
    ##  6 3307170 Referral to law enforcement              0
    ##  7 2508730 Expulsions with educational services     0
    ##  8 2508730 Referral to law enforcement              0
    ##  9 4400900 Expulsions with educational services     3
    ## 10 4400900 Referral to law enforcement             85
    ## # ... with 17,944 more rows

Going to pivot the table wider to turn the categories into columns. Then
change the name of the columns.

``` r
discipline_fixed <- discipline_data %>% pivot_wider(names_from = Category, values_from = Total) %>%
  rename(
    Expulsions = 'Expulsions with educational services',
    Police_Involvement = 'Referral to law enforcement'
  )
discipline_fixed
```

    ## # A tibble: 8,977 x 3
    ##    ID      Expulsions Police_Involvement
    ##    <chr>        <dbl>              <dbl>
    ##  1 3303090          0                  5
    ##  2 3304960          0                  1
    ##  3 3307170          0                  0
    ##  4 2508730          0                  0
    ##  5 4400900          3                 85
    ##  6 2509030          0                  1
    ##  7 2512210          0                  0
    ##  8 2307320          0                 20
    ##  9 2512750          0                  0
    ## 10 2310590          0                  4
    ## # ... with 8,967 more rows

Going to do an left join with the other datatable since the Chicago
districts are missing from the data.

``` r
combined_with_discipline <- left_join(final_with_suspension_count, discipline_fixed, by = 'ID')
combined_with_discipline
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsions <dbl>, Police_Involvement <dbl>

Checking for how many na data there is.

``` r
sum(is.na(combined_with_discipline))
```

    ## [1] 0

Going to drop the few rows that are missing values for this.

``` r
combined_with_discipline <- drop_na(combined_with_discipline)
combined_with_discipline
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsions <dbl>, Police_Involvement <dbl>

Checking the missing value count again.

``` r
sum(is.na(combined_with_discipline))
```

    ## [1] 0

Create new columns with proportions of expulsions and police involvement
and then drop all the columns that are not needed.

``` r
final_with_discipline <- combined_with_discipline %>% 
  mutate(Expulsion_Prop = Expulsions / District_Population, Police_Prop = Police_Involvement / District_Population) %>%
  select(-Expulsions, -Police_Involvement)
final_with_discipline
```

    ## # A tibble: 8,143 x 22
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 16 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>

Going to load the data for bullying based on race.

``` r
bully_race_data <- read_csv('cummalativerace.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

    ## Warning: 10 parsing failures.
    ##  row col               expected  actual                  file
    ## 1365  ID no trailing characters 06CC370 'cummalativerace.csv'
    ## 1366  ID no trailing characters 06CC370 'cummalativerace.csv'
    ## 1745  ID no trailing characters 06CC366 'cummalativerace.csv'
    ## 1746  ID no trailing characters 06CC366 'cummalativerace.csv'
    ## 1945  ID no trailing characters 06CC256 'cummalativerace.csv'
    ## .... ... ...................... ....... .....................
    ## See problems(...) for more details.

``` r
bully_race_data
```

    ## # A tibble: 18,068 x 5
    ##    `Lea State` LEA                    ID Category        Total
    ##    <chr>       <chr>               <dbl> <chr>           <dbl>
    ##  1 GA          Fannin County     1302100 Discplined_Race     1
    ##  2 GA          Fannin County     1302100 Reported_Race       1
    ##  3 FL          BROWARD           1200180 Discplined_Race     7
    ##  4 FL          BROWARD           1200180 Reported_Race       0
    ##  5 GA          Greene County     1302490 Discplined_Race     0
    ##  6 GA          Greene County     1302490 Reported_Race       0
    ##  7 GA          Union County      1305250 Discplined_Race     0
    ##  8 GA          Union County      1305250 Reported_Race       0
    ##  9 TN          Washington County 4704380 Discplined_Race     4
    ## 10 TN          Washington County 4704380 Reported_Race       6
    ## # ... with 18,058 more rows

Pivot the table to create rows out of the columns then rename the
columns.

``` r
bully_race_fixed <- bully_race_data %>% 
  distinct() %>% 
  pivot_wider(names_from = Category, values_from = Total)
bully_race_fixed
```

    ## # A tibble: 8,697 x 5
    ##    `Lea State` LEA                           ID Discplined_Race Reported_Race
    ##    <chr>       <chr>                      <dbl>           <dbl>         <dbl>
    ##  1 GA          Fannin County            1302100               1             1
    ##  2 FL          BROWARD                  1200180               7             0
    ##  3 GA          Greene County            1302490               0             0
    ##  4 GA          Union County             1305250               0             0
    ##  5 TN          Washington County        4704380               4             6
    ##  6 TN          Greeneville City Schools 4701500               1             1
    ##  7 AL          Alabaster City            100190              44            36
    ##  8 GA          White County             1305670               7             7
    ##  9 TN          Sullivan County          4703990               2             2
    ## 10 AL          Bullock County            100480               0             0
    ## # ... with 8,687 more rows

Combine the bullying based on race with the other table.

``` r
class(bully_race_fixed$ID) = "character"
combined_with_race <- left_join(final_with_discipline, bully_race_fixed, by = 'ID')
combined_with_race
```

    ## # A tibble: 8,143 x 26
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 20 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, `Lea State` <chr>, LEA <chr>,
    ## #   Discplined_Race <dbl>, Reported_Race <dbl>

``` r
sum(is.na(combined_with_race))
```

    ## [1] 0

``` r
final_with_race <- combined_with_race %>% 
  mutate(Race_Disc = Discplined_Race / District_Population, Race_Reports = Reported_Race / District_Population) %>%
  select(-Discplined_Race, -Reported_Race, -LEA, -'Lea State')
final_with_race
```

    ## # A tibble: 8,143 x 24
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 18 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>

I will load the data for being bullied by gender.

``` r
bully_gender_data <- read_csv('cummalativegender.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

    ## Warning: 10 parsing failures.
    ##  row col               expected  actual                    file
    ## 1175  ID no trailing characters 25SOP01 'cummalativegender.csv'
    ## 1176  ID no trailing characters 25SOP01 'cummalativegender.csv'
    ## 2243  ID no trailing characters 06CC370 'cummalativegender.csv'
    ## 2244  ID no trailing characters 06CC370 'cummalativegender.csv'
    ## 2391  ID no trailing characters 06CC256 'cummalativegender.csv'
    ## .... ... ...................... ....... .......................
    ## See problems(...) for more details.

``` r
bully_gender_data
```

    ## # A tibble: 17,394 x 5
    ##    `Lea State` LEA                    ID Category          Total
    ##    <chr>       <chr>               <dbl> <chr>             <dbl>
    ##  1 GA          Fannin County     1302100 Discplined_Gender     0
    ##  2 GA          Fannin County     1302100 Reported_Gender       1
    ##  3 FL          BROWARD           1200180 Discplined_Gender   176
    ##  4 FL          BROWARD           1200180 Reported_Gender       0
    ##  5 GA          Greene County     1302490 Discplined_Gender     0
    ##  6 GA          Greene County     1302490 Reported_Gender       0
    ##  7 GA          Union County      1305250 Discplined_Gender     0
    ##  8 GA          Union County      1305250 Reported_Gender       0
    ##  9 TN          Washington County 4704380 Discplined_Gender     0
    ## 10 TN          Washington County 4704380 Reported_Gender       0
    ## # ... with 17,384 more rows

Fix the datatable turning the categories into columns.

``` r
bully_gender_fixed <- bully_gender_data %>% 
  distinct() %>% 
  pivot_wider(names_from = Category, values_from = Total)
bully_gender_fixed
```

    ## # A tibble: 8,697 x 5
    ##    `Lea State` LEA                           ID Discplined_Gend~ Reported_Gender
    ##    <chr>       <chr>                      <dbl>            <dbl>           <dbl>
    ##  1 GA          Fannin County            1302100                0               1
    ##  2 FL          BROWARD                  1200180              176               0
    ##  3 GA          Greene County            1302490                0               0
    ##  4 GA          Union County             1305250                0               0
    ##  5 TN          Washington County        4704380                0               0
    ##  6 TN          Greeneville City Schools 4701500                5               4
    ##  7 AL          Alabaster City            100190               13               5
    ##  8 GA          White County             1305670               20              20
    ##  9 TN          Sullivan County          4703990                3               2
    ## 10 AL          Bullock County            100480                0               0
    ## # ... with 8,687 more rows

Now I will join this with the other datatable.

``` r
class(bully_gender_fixed$ID) = "character"
combined_with_gender <- left_join(final_with_race, bully_gender_fixed, by = 'ID')
combined_with_gender
```

    ## # A tibble: 8,143 x 28
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 22 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, `Lea State` <chr>, LEA <chr>, Discplined_Gender <dbl>,
    ## #   Reported_Gender <dbl>

``` r
sum(is.na(combined_with_gender))
```

    ## [1] 0

Now I will changed the columns to proportions and drop the excess.

``` r
final_with_gender <- combined_with_gender %>% 
  mutate(Gender_Disc = Discplined_Gender / District_Population, Gender_Reports = Reported_Gender / District_Population) %>%
  select(-Discplined_Gender, -Reported_Gender, -LEA, -'Lea State')
final_with_gender
```

    ## # A tibble: 8,143 x 26
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 20 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>

I will bring in the data on math and science courses that are offered.

``` r
courses_data <- read_csv('cummalativescience.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   `Lea State` = col_character(),
    ##   LEA = col_character(),
    ##   ID = col_double(),
    ##   Category = col_character(),
    ##   Total = col_double()
    ## )

    ## Warning: 32 parsing failures.
    ##  row col               expected  actual                     file
    ## 1153  ID no trailing characters 06CC370 'cummalativescience.csv'
    ## 1154  ID no trailing characters 06CC370 'cummalativescience.csv'
    ## 1155  ID no trailing characters 06CC370 'cummalativescience.csv'
    ## 1156  ID no trailing characters 06CC370 'cummalativescience.csv'
    ## 1157  ID no trailing characters 06CC370 'cummalativescience.csv'
    ## .... ... ...................... ....... ........................
    ## See problems(...) for more details.

``` r
courses_data
```

    ## # A tibble: 77,376 x 5
    ##    `Lea State` LEA                            ID Category  Total
    ##    <chr>       <chr>                       <dbl> <chr>     <dbl>
    ##  1 CA          Kings Canyon Joint Unified 619700 Algebra1     51
    ##  2 CA          Kings Canyon Joint Unified 619700 Geometry     27
    ##  3 CA          Kings Canyon Joint Unified 619700 Algebra2     15
    ##  4 CA          Kings Canyon Joint Unified 619700 Adv_Math     24
    ##  5 CA          Kings Canyon Joint Unified 619700 Calculus      4
    ##  6 CA          Kings Canyon Joint Unified 619700 Biology      32
    ##  7 CA          Kings Canyon Joint Unified 619700 Chemistry    19
    ##  8 CA          Kings Canyon Joint Unified 619700 Physics       1
    ##  9 CA          Pleasanton Unified         600020 Algebra1     27
    ## 10 CA          Pleasanton Unified         600020 Geometry     21
    ## # ... with 77,366 more rows

Fixing the table by changing rows to columns.

``` r
classes_fixed <- courses_data %>% 
  distinct() %>% 
  pivot_wider(names_from = Category, values_from = Total)
classes_fixed
```

    ## # A tibble: 8,640 x 11
    ##    `Lea State` LEA       ID Algebra1 Geometry Algebra2 Adv_Math Calculus Biology
    ##    <chr>       <chr>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
    ##  1 CA          King~ 619700       51       27       15       24        4      32
    ##  2 CA          Plea~ 600020       27       21       17       44       20      48
    ##  3 CA          Powa~ 631530      102       89       62       91       93     135
    ##  4 CA          Tula~ 639870        0        0        0        0        0       0
    ##  5 CA          Pana~ 606390        0        0        0        0        0       0
    ##  6 CA          Oakl~ 628050      180      140      110      163       14     165
    ##  7 CA          Fols~ 613890       54       66       38       66       11      68
    ##  8 CA          Plac~ 630750       34       30       25       21        8      56
    ##  9 CA          Lamm~ 601410       11        9       10       11        3      13
    ## 10 CA          Los ~ 622890        0        0        0        0        0       0
    ## # ... with 8,630 more rows, and 2 more variables: Chemistry <dbl>,
    ## #   Physics <dbl>

Joining the courses with the other datatable.

``` r
class(classes_fixed$ID) = "character"
combined_with_courses <- left_join(final_with_gender, classes_fixed, by = 'ID')
combined_with_courses
```

    ## # A tibble: 8,143 x 36
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 30 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, `Lea
    ## #   State` <chr>, LEA <chr>, Algebra1 <dbl>, Geometry <dbl>, Algebra2 <dbl>,
    ## #   Adv_Math <dbl>, Calculus <dbl>, Biology <dbl>, Chemistry <dbl>,
    ## #   Physics <dbl>

``` r
sum(is.na(combined_with_courses))
```

    ## [1] 330

Replacing NA values with the mean for the column since this data was not
able to be retrieved from the system.

``` r
combined_with_courses <-   select(combined_with_courses,-'LEA', -'Lea State')
combined_with_courses$Algebra1[is.na(combined_with_courses$Algebra1)]<-mean(combined_with_courses$Algebra1,na.rm=TRUE)
combined_with_courses$'Geometry'[is.na(combined_with_courses$'Geometry')]<-mean(combined_with_courses$'Geometry',na.rm=TRUE)
combined_with_courses$Algebra2[is.na(combined_with_courses$Algebra2)]<-mean(combined_with_courses$Algebra2,na.rm=TRUE)
combined_with_courses$'Adv_Math'[is.na(combined_with_courses$'Adv_Math')]<-mean(combined_with_courses$'Adv_Math',na.rm=TRUE)
combined_with_courses$'Calculus'[is.na(combined_with_courses$'Calculus')]<-mean(combined_with_courses$'Calculus',na.rm=TRUE)
combined_with_courses$'Biology'[is.na(combined_with_courses$'Biology')]<-mean(combined_with_courses$'Biology',na.rm=TRUE)
combined_with_courses$'Chemistry'[is.na(combined_with_courses$'Chemistry')]<-mean(combined_with_courses$'Chemistry',na.rm=TRUE)
combined_with_courses$'Physics'[is.na(combined_with_courses$'Physics')]<-mean(combined_with_courses$'Physics',na.rm=TRUE)
```

``` r
sum(is.na(combined_with_courses))
```

    ## [1] 0

Creating proportions and removing the extra columns.

``` r
final_with_courses <- combined_with_courses %>% 
  mutate(Alg1 = Algebra1 / District_Population, Alg2 = Algebra2 / District_Population, Geo = Geometry / District_Population, AdvMath = Adv_Math / District_Population, Calc = Calculus / District_Population, Bio = Biology / District_Population, Chem = Chemistry / District_Population, Phys = Physics / District_Population) %>%
  select(-Algebra1, -Algebra2, -'Geometry', -Adv_Math, -Calculus, -Biology, -Physics, -Chemistry)
final_with_courses
```

    ## # A tibble: 8,143 x 34
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 28 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>

Loading the data on students that passed Algebra.

``` r
passed_data <- read_csv('mathperformance.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   ID = col_character(),
    ##   Category = col_character(),
    ##   Total = col_character()
    ## )

``` r
passed_data
```

    ## # A tibble: 37,991 x 3
    ##    ID     Category             Total
    ##    <chr>  <chr>                <chr>
    ##  1 619700 Algebra_Early        927  
    ##  2 619700 Algebra_Early_Passed 751  
    ##  3 619700 Algebra_Late         98   
    ##  4 619700 Algebra_Late_Passed  52   
    ##  5 600020 Algebra_Early        690  
    ##  6 600020 Algebra_Early_Passed 614  
    ##  7 600020 Algebra_Late         23   
    ##  8 600020 Algebra_Late_Passed  6    
    ##  9 631530 Algebra_Early        2327 
    ## 10 631530 Algebra_Early_Passed 2175 
    ## # ... with 37,981 more rows

I will change the rows into columns to work with the data.

``` r
passed_fixed <- passed_data %>% 
  distinct() %>% 
  pivot_wider(names_from = Category, values_from = Total)
passed_fixed
```

    ## # A tibble: 8,640 x 5
    ##    ID     Algebra_Early Algebra_Early_Passed Algebra_Late Algebra_Late_Passed
    ##    <chr>  <chr>         <chr>                <chr>        <chr>              
    ##  1 619700 927           751                  98           52                 
    ##  2 600020 690           614                  23           6                  
    ##  3 631530 2327          2175                 196          173                
    ##  4 639870 0             <NA>                 0            <NA>               
    ##  5 606390 0             <NA>                 0            <NA>               
    ##  6 628050 2390          1189                 231          81                 
    ##  7 613890 857           710                  122          81                 
    ##  8 630750 897           756                  76           59                 
    ##  9 601410 657           140                  7            2                  
    ## 10 622890 0             <NA>                 0            <NA>               
    ## # ... with 8,630 more rows

``` r
sum(is.na(passed_fixed))
```

    ## [1] 679

Turning the na into zero since there were no students that took the
course.

``` r
class(passed_fixed$Algebra_Early) = "double"
class(passed_fixed$Algebra_Early_Passed) = "double"
```

    ## Warning in class(passed_fixed$Algebra_Early_Passed) = "double": NAs introduced
    ## by coercion

``` r
class(passed_fixed$Algebra_Late) = "double"
class(passed_fixed$Algebra_Late_Passed) = "double"
```

    ## Warning in class(passed_fixed$Algebra_Late_Passed) = "double": NAs introduced by
    ## coercion

``` r
passed_completed <- replace(passed_fixed, is.na(passed_fixed), 0)
```

``` r
sum(is.na(passed_completed))
```

    ## [1] 0

``` r
courses_proportion <- passed_completed %>%
  mutate(Early_Pass = Algebra_Early_Passed / Algebra_Early, Late_Pass = Algebra_Late_Passed / Algebra_Late)
courses_proportion <- replace(courses_proportion, is.na(courses_proportion), 0)
courses_proportion
```

    ## # A tibble: 8,640 x 7
    ##    ID    Algebra_Early Algebra_Early_P~ Algebra_Late Algebra_Late_Pa~ Early_Pass
    ##    <chr>         <dbl>            <dbl>        <dbl>            <dbl>      <dbl>
    ##  1 6197~           927              751           98               52      0.810
    ##  2 6000~           690              614           23                6      0.890
    ##  3 6315~          2327             2175          196              173      0.935
    ##  4 6398~             0                0            0                0      0    
    ##  5 6063~             0                0            0                0      0    
    ##  6 6280~          2390             1189          231               81      0.497
    ##  7 6138~           857              710          122               81      0.828
    ##  8 6307~           897              756           76               59      0.843
    ##  9 6014~           657              140            7                2      0.213
    ## 10 6228~             0                0            0                0      0    
    ## # ... with 8,630 more rows, and 1 more variable: Late_Pass <dbl>

Joining with the other data table.

``` r
combined_with_passing <- left_join(final_with_courses, courses_proportion, by = 'ID')
combined_with_passing
```

    ## # A tibble: 8,143 x 40
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 34 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>, Algebra_Early <dbl>, Algebra_Early_Passed <dbl>,
    ## #   Algebra_Late <dbl>, Algebra_Late_Passed <dbl>, Early_Pass <dbl>,
    ## #   Late_Pass <dbl>

Cleaning up the table by dropping extra columns.

``` r
final_passing <- combined_with_passing %>%
  select(-Algebra_Early, -Algebra_Early_Passed, -Algebra_Late, -Algebra_Late_Passed)
final_passing
```

    ## # A tibble: 8,143 x 36
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 8,133 more rows, and 30 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>, Early_Pass <dbl>, Late_Pass <dbl>

Goinng to load in the graduation rate data file.

``` r
grad_data <- read_csv('graduationrate.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   ID = col_double(),
    ##   Grad_Rate = col_character()
    ## )

``` r
grad_data
```

    ## # A tibble: 12,051 x 2
    ##        ID Grad_Rate
    ##     <dbl> <chr>    
    ##  1 100005 94       
    ##  2 100006 91       
    ##  3 100007 94       
    ##  4 100008 96       
    ##  5 100011 95       
    ##  6 100012 94       
    ##  7 100013 97       
    ##  8 100030 89       
    ##  9 100060 95       
    ## 10 100090 79       
    ## # ... with 12,041 more rows

I am going to do an inner join with the other datatable to make sure
that there is a grad rate for each of the districts.

``` r
class(grad_data$ID) = "character"
combined_with_grad <- inner_join(final_passing, grad_data, by = 'ID')
combined_with_grad
```

    ## # A tibble: 7,746 x 37
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 7,736 more rows, and 31 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>, Early_Pass <dbl>, Late_Pass <dbl>, Grad_Rate <chr>

Creating one last feature finding the proportion of students compared to
the area population. This will be the final part for the datatable.

``` r
final_datatable <- mutate(combined_with_grad, Student_Prop = District_Population / Area_Population)
final_datatable
```

    ## # A tibble: 7,746 x 37
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 7,736 more rows, and 31 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>, Early_Pass <dbl>, Late_Pass <dbl>, Grad_Rate <chr>

Filter out outliers that do not make sense.

``` r
final_table <- final_datatable %>%
  filter(Absent_Prop <= 1) %>%
  filter(Athletes_Prop <= 1) %>%
  filter(Takers_Proportion <= 1) %>%
  filter(Late_Pass <= 1)

final_table
```

    ## # A tibble: 7,577 x 37
    ##    ID    `Lea State.x` LEA.x Area_Population Children_Poverty Poverty_Proport~
    ##    <chr> <chr>         <chr>           <dbl>            <dbl>            <dbl>
    ##  1 1001~ AL            Alab~           34015              860            0.128
    ##  2 1000~ AL            Albe~           21786             1546            0.376
    ##  3 1000~ AL            Alex~           17073              832            0.313
    ##  4 1000~ AL            Anda~            8854              386            0.267
    ##  5 1000~ AL            Anni~           22350             1106            0.347
    ##  6 1001~ AL            Arab~            8305              329            0.216
    ##  7 1001~ AL            Athe~           24963              685            0.174
    ##  8 1001~ AL            Atta~            5952              280            0.302
    ##  9 1002~ AL            Aubu~           61570              888            0.121
    ## 10 1002~ AL            Auta~           55504             1842            0.184
    ## # ... with 7,567 more rows, and 31 more variables: Student_Prop <dbl>,
    ## #   District_Population <dbl>, Non_White_Students <dbl>,
    ## #   New_Teachers_Proportion <dbl>, Absent_Teacher_Proportion <dbl>,
    ## #   Counselor_Ratio <dbl>, Student_Teacher_Ratio <dbl>, Absent_Prop <dbl>,
    ## #   Test_Prop <dbl>, Teams_Prop <dbl>, Athletes_Prop <dbl>,
    ## #   Takers_Proportion <dbl>, AP_Prop <dbl>, AVG_Suspenion <dbl>,
    ## #   Expulsion_Prop <dbl>, Police_Prop <dbl>, Race_Disc <dbl>,
    ## #   Race_Reports <dbl>, Gender_Disc <dbl>, Gender_Reports <dbl>, Alg1 <dbl>,
    ## #   Alg2 <dbl>, Geo <dbl>, AdvMath <dbl>, Calc <dbl>, Bio <dbl>, Chem <dbl>,
    ## #   Phys <dbl>, Early_Pass <dbl>, Late_Pass <dbl>, Grad_Rate <chr>

Writing the datatable as a csv file now that it is complete.

``` r
write.csv(x=final_table, file="ProjectDataTableNewData.csv")
```
