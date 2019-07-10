The protein dataset

To demonstrate clusterboot(), we’ll use a small dataset from 1973 on protein consumption from nine different food groups in 25 countries in Europe. The original dataset can be found here. A tab-separated text file with the data can be found in this directory. The data file is called protein.txt; additional information can be found in the file protein_README.txt.

The goal is to group the countries based on patterns in their protein consumption. The dataset is loaded into R as a data frame called protein, as shown in the next listing.