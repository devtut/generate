---
metaTitle: "LaTex - Add Citation"
description: "Add citation to already existing LaTeX document"
---

# Add Citation




## Add citation to already existing LaTeX document


At the end of the document add the following:

**\bibliographystyle{**style**}**

**\bibliography{**file location**}**

Create a file with extension **.bib** and save the citation as follows:

```latex
@inproceedings{citation_name,
  title={Paper Title},
  author={List Authors},
  pages={45--48},
  year={2013},
  organization={organization name}
}

```

To cite use the following: **\citet{**citation_name**}**

