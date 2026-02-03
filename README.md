# Calls_to_Communities
Using acoustic data to construct movement networks describing the spatial structure of St. Lawrence Estuary belugas

## Data

**File:** `contact_calls_by_site.csv`

- **Description:** Beluga contact call detections at 6 sites in 2017, 2018, 2021, 2022, 2023
- **Columns:**
  - 'datetime': local time of the hydrophone when the call was detected
  - 'type': contact call type (classified subjectively by human judge and validated by DFA)
  - 'site': site of the hydrophone
  - 'hf': maximum frequency of the signature element
  - 'lf': minimum frequency of the signature element
  - 'cf': center frequency of the signature element
  - 'df': delta frequency of the signature element
  - 'pf': peak frequency of the signature element
  - 'dt': duration of the signature element
  - 'snr': signal to noise ratio of the signature element
  - 'ip': number of inflection points in the signature element
  - 'sig_segm': number of discrete segments in the signature element
  - 'sf': initial frequency of the signature element
  - 'ef': terminal frequency of the signature element
  - 'ff': fundamental frequency of the signature element
  - df_cc: delta frequency of the broadband element
  - pf_cc: peak frequency of the broadband element
  - cf_cc: center frequency of the broadband element
  - f5_cc: frequency 5% of the broadband element
  - f25_cc: frequency 25% of the broadband element
  - f75_cc: frequency 75% of the broadband element
  - f95_cc: frequency 95% of the broadband element
  - lf_cc: minimum frequency  of the broadband element
  - hf_cc: maximum frequency  of the broadband element

## Requirements

**R version:** 4.5 or higher

**Required packages:**
- data.table
- ggplot2
- stringr
- lubridate
- klaR
- MASS
- caret
- dplyr
- UpSetR
- CMRnet
- timeDate (version = "3043.102")
- igraph
# Note: Requires older version of timeDate package
remotes::install_version("timeDate", version = "3043.102"


## Contact

Jaclyn A. Aubin
jaclyn.a.aubin@gmail.com

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


