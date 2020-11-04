# BibTeX to JSON converter

The program reads BibTex records from the stdin and outputs
their corresponding JSON representations to stdout. E.g.,
given a BibTeX file test.bib with the following contents

```bibtex
Comments and preambles are ignored.

@preamble{This part will be ignored.}

String variables are supported. There can be no undefined
variables inside definitions of other variables, but there
may be undefined variables in BibTeX fields; names of the
variables will be used in this case.

String concatenation is supported.

@string(sep = "Septe" # "mber")

@manual{round2019,
  title        = {{Australian phonemic inventories contributed to
                   PHOIBLE 2.0: Essential explanatory notes}},
  author       = {Round, Erich},
  month        = sep,
  year         = 2019,
  doi          = {10.5281/zenodo.3464333},
  url          = {https://doi.org/10.5281/zenodo.3464333}
}

@article{stevens1989quantal,
  title={On the quantal nature of speech},
  author={Stevens, Kenneth N.},
  journal={Journal of Phonetics},
  month=unknown,
  volume={17},
  number={1-2},
  pages={3--45},
  year={1989},
  publisher={Elsevier}
}
```

this is the output of using the program (the default Chicken JSON egg does
not support pretty printing, and we use jq for that):

```bash
cat test.bib | ../bin/bib2json | jq
[
  {
    "cite-key": "round2019",
    "entry-type": "manual",
    "description": {
      "url": "https://doi.org/10.5281/zenodo.3464333",
      "month": "September",
      "year": "2019",
      "author": "Round, Erich",
      "doi": "10.5281/zenodo.3464333",
      "title": "{Australian phonemic inventories contributed to\n PHOIBLE 2.0: Essential explanatory notes}"
    }
  },
  {
    "cite-key": "stevens1989quantal",
    "entry-type": "article",
    "description": {
      "publisher": "Elsevier",
      "number": "1-2",
      "journal": "Journal of Phonetics",
      "volume": "17",
      "year": "1989",
      "title": "On the quantal nature of speech",
      "month": "unknown",
      "author": "Stevens, Kenneth N.",
      "pages": "3--45"
    }
  }
]
```

## Installation

A static binary compiled under 64-bit Ubuntu is provided
in the `bin` directory. Binaries for macOS and Windows 
will be added after additional testing. You can also compile
from source using a recent Chicken Scheme compiler.
