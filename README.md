# Drawing Pedigree Charts from Simple Text Descriptions

## Proposal
Pedigree charts are used in Medicine to record a patient’s family history. They are particularly useful to document genetic diseases that can be inherited from one generation to the next. The example below shows a pedigree chart of a (fictitious) family where multiple individuals are affected by breast cancer:

In most clinical settings, pedigree charts are still drawn by hand on paper, which limits their utility. Pedigree drawing software tools exist but are complicated to use and/or record their data in proprietary formats that are unsuitable for medical research purposes.

In this project, we aim to develop a pedigree drawing engine that can create pedigree charts as the example above from simple textual descriptions. The concept is similar to Graphviz (https://www.graphviz.org/) and mermaid (https://mermaidjs.github.io/), two projects that allow creating graphs and diagrams using a simple text syntax.

## Use and Install
There are currently two ways to access the Pedigree Engine:
1. Directly access through web:

    (Maybe not able to access due to limit active policy on shinyapps.io (we were using a free account). )
    https://6112project.shinyapps.io/Pedigree_project/


2. Clone the repo and run the shiny.R in Rstudio.


~~To be able to use this pedigree engine, R need to be installed. To have a better experience, python3 will be required to install. Python wil auto install any needed python package. But R module kinship2 needed manual install. To install kinship 2 use command 'install.packages('kinship 2') in R console.~~
Python version was no longer in develop

## Examples
**using command below will generate graph:**
1. A father_of C,D,F
2. B partner_of A
3. E father_of G,I,J
4. H partner_of E
5. F father_of Z,V
6. G mother_of Z,V
7. C,D,I gender_is male
8. J,Z,V gender_is female
9. A,C,F,Z,V affected_is diseaseA
10. E,G,Z affected_is diseaseB
11. Z d_twin V

![alt text](https://github.com/cloudyaaron/6112project/blob/master/src/example.jpg "Example pedigree graph")

More example input can be found in the sampleinput folder.

## The list of all possible input is listed below.
### List of Terms:

1. A gender_is male
2. A,B,C gender_is female
3. A name_is "John Doe"
4. A DOSB_is "13 Oct 1970"
5. A affect_is diseaseA
6. A deceased_is true
7. A,B,C deceased_is true
8. A affected_is diseaseA,B,C
9. A,B,C affected_is diseaseX,Y,Z

### Relationships (Edges):
1. A father_of B
2. A father_of B,C,D
3. A mother_of B
4. A mother_of B,C,D
5. A partner_of B
6. A m_twin B
7. A d_twin B
8. A unkonwn_twin B

**TO FIND More help plz find documentation in the REPO**


