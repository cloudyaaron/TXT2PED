# Drawing Pedigree Charts from Simple Text Descriptions

## Proposal
Pedigree charts are used in Medicine to record a patient’s family history. They are particularly useful to document genetic diseases that can be inherited from one generation to the next. The example below shows a pedigree chart of a (fictitious) family where multiple individuals are affected by breast cancer:

In most clinical settings, pedigree charts are still drawn by hand on paper, which limits their utility. Pedigree drawing software tools exist but are complicated to use and/or record their data in proprietary formats that are unsuitable for medical research purposes.

In this project, we aim to develop a pedigree drawing engine that can create pedigree charts as the example above from simple textual descriptions. The concept is similar to Graphviz (https://www.graphviz.org/) and mermaid (https://mermaidjs.github.io/), two projects that allow creating graphs and diagrams using a simple text syntax.

**To be able to use this pedigree engine, R need to be installed. To have a better experience, python3 will be required to install. Python wil auto install any needed python package. But R module kinship2 needed manual install. To install kinship 2 use command 'install.packages('kinship 2') in R console.**

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

## The list of all possible input is listed below.
### List of Terms:

A gender_is male
A,B,C gender_is female
A name_is "John Doe"
A DOSB_is "13 Oct 1970"
A affect_is diseaseA
A deceased_is true
A,B,C deceased_is true
A affected_is diseaseA,B,C
A,B,C affected_is diseaseX,Y,Z

### Relationships (Edges):
A father_of B
A father_of B,C,D
A mother_of B
A mother_of B,C,D
A partner_of B
A m_twin B
A d_twin B
A unkonwn_twin B

**TO FIND More help plz find documentation in the REPO**


