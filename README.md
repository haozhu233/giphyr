# giphyr
![CRAN_version](http://www.r-pkg.org/badges/version/giphyr)
![CRAN_download](http://cranlogs.r-pkg.org/badges/giphyr)


Want to add some GIFs to your awesome rmarkdown presentation?

![exciting](https://raw.githubusercontent.com/haozhu233/giphyr/master/img/exciting_rDbelKPujYEBq.gif)

## Introduction
I guess this is one of the least productive R packages in our community...

No more to say. Enjoy more GIFs in your Rmarkdown documents. 

![Screenshot](https://raw.githubusercontent.com/haozhu233/giphyr/master/img/Screenshot.png)

## Install
```r
install.packages("giphyr")

# For dev version
devtools::install_github("haozhu233/giphyr")
```

## Usage
If you are using a recent version of RStudio (v0.99.878 or later), you should be able to find an "Addins" dropdown menu on the toolbar. This package comes with a RStudio Addin called "Add GIFs", which has a user friendly interface you can use to browse, download or insert GIFs or links to a rmarkdown document. 

<img src='https://raw.githubusercontent.com/haozhu233/giphyr/master/img/rstudio_addins.png' width='400'></img>

For useRs who are not using RStudio, I'm planning to include a stand-alone shiny app that can be run directly in the console in the next release. 

## Note
I'm currently using the public beta key for the Giphy API. They say there is a rate limit. I will request a Production key if needed. 

Also, I noticed that after GIPHY made some API change recently (the link is no longer point directly to a .gif file but a web page), GIF images can't be seen correctly on some old versions of RStudio. You might need to update your RStudio version if it occurs to you. 
