# MPB

Development of a mountain pine beetle spread model for the Canadian western boreal forest

Alex Chubaty (<achubaty@for-cast.ca>)

**Collaborators:**

- Eliot McIntire (<eliot.mcintire@canada.ca>)
- Barry Cooke (<barry.cooke@canada.ca>)

## Getting the code

```bash
git clone --recurse-submodules -j8 https://github.com/achubaty/MPB
```

## Data sources

<!-- TODO: update repo urls -->

- Climate projections are in [climate-maps/](https://github.com/achubaty/MPB/tree/master/climate-maps)
- Digital elevation models are in [dem-maps/](https://github.com/achubaty/MPB/tree/master/dem-maps)
- MPB outbreak maps and climate suitability projections are in [mpb-maps/](https://github.com/achubaty/MPB/tree/master/mpb-maps)
- Pine maps are in [pine-maps/](https://github.com/achubaty/MPB/tree/master/pine-maps)
- Wind maps have yet to be developed.

Data used by modules are automatically downloaded during first-run of the model.
However, note that proprietary/confidetial data used by modules requires authorization.

## Model development

### Modules

- [mpbClimateData](https://github.com/achubaty/mpbClimateData)
- [mpbManagement](https://github.com/achubaty/mpbManagement)
- [mpbMassAttacksData](https://github.com/achubaty/mpbMassAttacksData)
- [mpbPine](https://github.com/achubaty/mpbPine)
- [mpbRandomLandscapes](https://github.com/achubaty/mpbRandomLandscapes)
- [mpbRedTopGrowth](https://github.com/achubaty/mpbRedTopGrowth)
- [mpbRedTopSpread](https://github.com/achubaty/mpbRedTopSpread)

### Other related components

- Modelling MPB climatic suitabiliy using [BioSim](https://github.com/achubaty/mpb-biosim)
- Modelling MPB reproductive biology (from data) is in [r-values/](https://github.com/achubaty/MPB/tree/master/r-values)
- The Safranyik *et al.* (1999) model has been ported to GNU Fortran by Bill Riel and Alex Chubaty in the [safranyik-mpb repository](https://github.com/achubaty/safranyik-mpb); elements of which are of interest to the development of this model.

## Notes

Notes on various related topics and models are in [notes/](https://github.com/achubaty/MPB/tree/master/notes).

## Reports and Presentations

<!-- TODO: add google drive links -->

- <https://example.com>

## References

Additonal literature citations and references are included within each sub-component of the project.

Safranyik, L., Barclay, H. J., Thomson, A. J., & Riel, W. G. (1999). A population dynamics model for the mountain pine beetle, *Dendroctonus ponderosae* Hopk. (Coleoptera: Scolytidae) (Information Report BC-X-386). Victoria, BC.
