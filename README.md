# TrussellTrust_green_revelation
Data analysis in support of Operational Research society pro-bono work for Trussell Trust: project Green Relevation

Effort so far: combine multiple data sources to a single table, and plot a scatter graph.

Data used not included in repo - download from dropbox and and put in folder ./data

RProject used so all file paths relative to the root directory where the .RProject file is.

To pull data from Stat-Xplore, use the code in `Stat-Xplore_API_funcs.R` as demonstrated in `Stat-Xplore_API_demo`. To use it you must have a json file of your query, and a Stat-Xplore key stored in a text file.

The easiest way to get the json file is to use the Stat-Xplore website (https://stat-xplore.dwp.gov.uk/webapi/jsf/dataCatalogueExplorer.xhtml). Get the table you want, and in the top right hand corner there's a dropdown box next to download. Choose the 'Open Data API Query (.json)' option and click go.
 
To get an API key you need a log-in on the Stat-Xplore website. On home page click on 3 dots in right hand corner, choose Account, under Open Data API Access click copy to copy the key to the clipboard. Paste into a .txt document, and the filepath of this document is the api_key_filename.

The nomisr package is used for getting data from nomis (https://www.nomisweb.co.uk/), see an example of how to use it in `TEST_Nomis_API_Access.R`.