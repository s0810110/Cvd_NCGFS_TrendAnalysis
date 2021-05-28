# Cvd_NCGFS_TrendAnalysis


## Deep Learning Reinforced RNN for Covid-19 trend prediction 

<div align="left">
  <img src="https://github.com/s0810110/Cvd_NCGFS_TrendAnalysis/blob/master/github_images/summary.PNG" width="750" >
</div>

We have developed a deep learning model that provides predictions of the COVID-19 related number of cases and mortality in the upcoming 5 weeks and simulates the effect of policy changes targeting COVID-19 spread.

We developed a Deep Recurrent Reinforced Learning (DRRL) based model. The data used to train the DRRL model was based on various available datasets that have the potential to influence the trend in the number of COVID-19 cases and mortality. Analyses were performed based on the simulation of policy changes targeting COVID-19 spread, and the geographical representation of these effects.

Model predictions of the number of cases and mortality of COVID-19 in the upcoming 5 weeks closely matched the actual values. Local lockdown with social distancing (LD_SD) was found to be ineffective compared to national lockdown. The ranking of effectiveness of supplementary measures for LD_SD were found to be consistent across national hotspots and local areas. Measure effectiveness were ranked from most effective to least effective: 1) full lockdown; 2) LD_SD with international travel −50%; 3) LD_SD with 100% quarantine; 4) LD_SD with closing school −50%; 5) LD_SD with closing pubs −50%. There were negligible differences observed between LD_SD, LD_SD with −50% food & Accommodation and LD_SD with −50% Retail.

The second national lockdown should be followed by measures which are more effective than LD_SD alone. Our model suggests the importance of restrictions on international travel and travel quarantines, thus suggesting that follow-up policies should consist of the combination of LD_SD and a reduction in the number of open airports within close proximity of the hotspot regions. Stricter measures should be placed in terms travel quarantine to increase the impact of this measure. It is also recommended that restrictions should be placed on the number of schools and pubs open.

# Citing NCGFS
If you use NCGFS in a scientific publication, please cite the following paper:
 
Dong, Tim, et al. ["A Deep Recurrent Reinforced Learning model to compare the efficacy of targeted local vs. national measures on the spread of COVID-19 in the UK"](https://www.medrxiv.org/content/10.1101/2021.05.21.20248630v1) medrxiv preprint medrxiv:2021.05.21.20248630v1 (2021).

BibTeX entry:

```bibtex
@article{dong_deep_2021,
	title = {A Deep Recurrent Reinforced Learning model to compare the efficacy of targeted local vs. national measures on the spread of COVID-19 in the UK},
	author = {Dong, Tim and Benedetto, Umberto and Sinha, Shubhra and Dimagli, Arnaldo and Caputo, Massimo and Angelini, Gianni D},
	journal = {medRxiv},
	date = {2021}
}

## License

This library is licensed under the MIT License.

