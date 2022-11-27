# README

## The US Farm Credit System and Agricultural Development: Evidence from an Early Expansion, 1920-1940 

**Author:** Jared Hutchins, jhtchns2@illinois.edu

To keep up to date with any changes or point out errors in this code, please refer to [the companion GitHub repository](https://github.com/jphutch/farm_credit).

### Abstract

I explore the impact of the Production Credit Associations (PCAs), an arm of the early Farm Credit System, on agricultural yield and input use following the farm crisis of the 1920s. Like many low- and middle-income countries today, farmers in the early 20th century United States found it difficult to access credit. The PCAs were established in 1933 and significantly increased the supply of short-term credit available to farmers. Using distance from the serving PCA as a proxy for credit access, I find that counties within 30 kilometers of a PCA had seven to fourteen percent higher crop revenue per acre and nine percent higher corn yields than counties more than 60 kilometers from a PCA. These areas also had a small but statistically significant increase in the use of tractors (one to two percent). These results provide crucial evidence of the impact of government-sponsored enterprises on the early US agricultural economy and its use as a cost-effective tool to address market frictions.

### Directories
The files necessary for replicating this paper are organized into four main directories:

\code

- Code in Python and R for cleaning the data and producing the results.


\docs

- Latex code for generating the paper, plus supplementary materials.


\figs

- Figures generated for the paper.

### Code
The analysis and data cleaning was done using Python 3.8.5 and R 4.0.2. The python code files are in the form of Jupyter notebooks, which allow for interactive coding. The Python environment used to run the notebooks is in the file __environment.yml__ and can be created after installing Anaconda 3 and running the command:

`conda env create -f environment.yml`

The R packages used to run the analysis are stored in the file `R_packages`.

Each code file starts with a two digit number indicating the order in which the files should be run. For more description on how the files relate to each other, see the README within that directory.

Tables used in the paper are all generated in the file "60_R_Analysis.ipynb" using the command "stargazer," but most tables have changed formatting when they are eventually placed in the paper to fit the style of the article.

Figures are almost all created with `matplotlib` with the exception of Figure 2, which was created using a program called [Latex Draw](http://latexdraw.sourceforge.net/). This figure can be changed using the .svg version that is included in the directory. Note that depending on how your environment looks the Figures may look slightly different than those that are in the paper.

### Docs
The two subdirectories are "paper" and "supplementary_materials." The main document in the "supplementary materials" is the original PCA map which could previously be found at [this public url](http://www.farmcredit100.com/our-history/archive/location-of-production-credit-associations) (unfortunately, the site has since been shut down). The paper subdirectory has the references, the appendix, and the text of the paper which grabs the figures from the directory "figs."

### Generating the Data
Since the data cannot be stored on GitHub, the code files need to be given the data directory, which is a text string stored in the file "data_dir.txt". Once this is supplied, running the scripts will populate the "clean_data" folder with the created data products. Currently the data folder also contains the created data products. The data files are stored in a zip folder at the AJAE website.


### Data Sources
Haines, Michael, Price Fishback, and Paul Rhode. United States Agriculture Data, 1840 - 2010. ICPSR35206-v1. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2014-12-22. http://doi.org/10.3886/ICPSR35206.v1

Farm Credit Administration. "Location of Production Credit Associations 1937" (found in the \docs\supplementary_material)

PRISM Climate Group (2004). Oregon State University. http://prism.oregonstate.edu.

Federal Deposit Insurance Corporation (1992). Federal Deposit Insurance Corporation Data on Banks in the United States, 1920-1936. ICPSR 7 https://doi.org/10.3886/ICPSR00007.v1.

FAO GAEZ (2016). Food and Agriculture Organization of the United Nations. http://gaez.fao.org/Main.html

Fishback, P. V., S. Kantor, and J. J. Wallis (2003). Can the New Deal’s Three Rs Be Rehabilitated? A Program-by-Program, County-by-County Analysis. Explorations in Economic History 40(3), 278–307.
http://www.u.arizona.edu/~fishback/Published_Research_Datasets.html

Hornbeck, Richard. 2012. "The Enduring Impact of the American Dust Bowl: Short- and Long-Run Adjustments to Environmental Catastrophe." American Economic Review, 102 (4): 1477-1507. http://doi.org/10.3886/E112528V1