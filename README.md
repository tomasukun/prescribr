[![Travis-CI Build Status](https://travis-ci.org/tomasukun/prescribr.svg?branch=master)](https://travis-ci.org/tomasukun/prescribr)

# prescribr
The prescribr package uses several publicly available (at least for now) data sources from the Centers for Medicaid and Medicare Services to study the relationship between pharmaceutical industry payments to physicians and their prescribing behavior. This package is the continuation of work that originally published in *[Jama Internal Medicine](http://jamanetwork.com/journals/jamainternalmedicine/article-abstract/2528290)* in June 2016. Since then,  2014 Medicare Part D prescribing data and 2015 Open Payments data has been made publicly available.

###### Note:
This package was designed to reproduce the finding of the work mentioned above. That being said, generalizing the code to work with any class of drugs is the long-term goal.

#### Data Sources

i.	 [CMS Part D Public Use File and Part D Prescriber Summary for 2013 and 2014 Files](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/index.html)

ii.  [Open Payments General File and Summary File for 2013 - 2015](https://www.cms.gov/OpenPayments/Explore-the-Data/Dataset-Downloads.html)

iii. [Physician Compare](https://data.medicare.gov/Physician-Compare/Physician-Compare-National-Downloadable-File/mj5m-pzi6)

#### Set-up:
To use the package, you will need to download the files listed above and specify the directory of where the files were downloaded. In addition, you will need to specify the directory you wish to save the processed `.rData` files are saved and since the package was designed as a collaborative project, a shared documents directory must be specified. These directories should be specified in your `.Renviron` file.

```
SOURCE_DIR = "Some/Directory/'
PROCESSED_DIR = 'Some/Other/Directory/'
SHARED_DIR = 'Some/Collaborative/Directory'
```
