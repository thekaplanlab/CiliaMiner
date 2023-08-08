
# CiliaMiner: Ciliopathy Genes and Ciliopathies
### https://kaplanlab.shinyapps.io/ciliaminer/

If you use workflow in your research, please consider citing;
> Turan, M. G., Orhan, M. E., Cevik, S., & Kaplan, O. I. (2023). CiliaMiner: an integrated database for ciliopathy genes and ciliopathies. Database : the journal of biological databases and curation, 2023, baad047. https://doi.org/10.1093/database/baad047

[**CiliaMiner**](https://kaplanlab.shinyapps.io/ciliaminer/) is a ciliopathy-specific database that compiles and presents the up-to-date list of ciliopathies, ciliopathy-associated genes, and the clinical features for each condition. Furthermore, CiliaMiner provides classifications of ciliopathies and associated disorders based on the subcellular localization of disease-associated genes in conjunction with disease symptoms. CiliaMiner suggests easy access to all ciliopathies, disease symptoms, ciliopathy genes, ciliopathy candidate genes, and orthologs of ciliopathy genes and ciliopathy candidate genes, seeking to serve as a major place in the ciliopathy database. Each piece of information is manually curated and verified before being uploaded to the appropriate part of the CiliaMiner database, and and users can also quickly submit their own data to CiliaMiner.

> R codes which are used for creating network analysis and barplot, are found in [article_figures folder](https://github.com/thekaplanlab/CiliaMiner/tree/main/article_figures). 

## User Interface of CiliaMiner

### Homepage

The homepage provides quick access to searches for the homosapiens gene and ciliopathy. The reactivity graph that is created using user input on the homepage show changes in the publication number of ciliopathy genes yearly.

### Ciliopathy Names

The Ciliopathy Names web page presents the main categorization of diseases based on clinical symptoms and disease-associated genes. This page includes five subpanels; primary ciliopathies, secondary diseases, motile ciliopathies, atypical ciliopathies, and potential ciliopathy genes, respectively. The web page sidebar panel in primary, secondary, and motile ciliopathies subpages summarizes clinical symptoms using representative organ figures (3). Additionally, an interactive heatmap provides easy access for comparing clinical features by choosing diseases (2). 

![Sunu1](https://user-images.githubusercontent.com/66166710/204107017-de9dd977-8a59-4b59-9481-8668b43c2025.jpg)

**For each subpage in the CiliaMiner, users can search (1) their genes or diseases and download their search results with different file options.**
>Searching result of Cornelia de Lange Syndrome.
![secondary_page](https://user-images.githubusercontent.com/66166710/204107041-a76f5725-708e-4ba7-a1bb-e7b6294dbaf8.png)

### Ciliopathy Genes and Orthologs

Ciliopathy and ciliopathy-like diseases are listed based on human genes obtained from OMIM and research articles. The ortholog genes of human disease-associated genes are given on the "Ciliopathy Genes and Orthologs" page. This page presents the ortholog genes of six organisms, including Homo Sapiens, Mus musculus, Danio rerio, Xenopus laevis, Drosophila melanogaster, and Caenorhabditis elegans. [ConVarT](https://convart.org/), [OrthoList 2](http://ortholist.shaye-lab.org/), and [Wormbase](https://wormbase.org/) (for C. elegans) are used for collecting ortholog gene pieces of information, while [OMIM](https://www.omim.org/) is used for adding abbreviations of diseases and gene/disease MIM numbers. Additionally, [NCBI](https://www.ncbi.nlm.nih.gov/gene/) and [PubMed](https://pubmed.ncbi.nlm.nih.gov/) are added as an extension to tables for directing to paper and gene pages, respectively.

### Symptoms and Diseases

There are 55 diseases that are involved in primary, secondary, and motile ciliopathy groups, and all diseases have different clinical symptoms. All clinical features are collected from [OMIM](https://www.omim.org/) manually to provide easy access to all clinical features. The “Symptoms and Diseases” page consists of 4092 unique clinical features. The side panel, which is available on the page, provides choosing a disease or symptom name. Also, in the symptom-based search option, users can search for their clinical symptoms by writing to the panel.

### Submit Your Gene

Users can submit any suggestions or newly published ciliopathy genes.

##  Strength and Novelties of Database:

1. Classification of ciliopathy diseases based on symptoms and localization data helps us to suggest possible novel ciliopathy disease genes and diseases. 

![main_page](https://user-images.githubusercontent.com/66166710/204104792-fff2ea35-fcd8-41bf-a41b-51c2a719019c.PNG)

2.  We perform extensive manual data collection while adhering to the previously defined ciliopathy classifications. Our analysis has increased the number of ciliopathies overall, and we have further classified them using information on cilia-related localization and disease symptoms, including primary ciliopathies (34 diseases), secondary ciliopathies (18 diseases), and unclassified ciliopathy-associated genes (42 genes). We present all of these data, including the disease names, disease associated gene names, disease symptoms, and localization of all these ciliopathy diseases.

![symptome_page](https://user-images.githubusercontent.com/66166710/204104681-829ec179-1961-4ec1-a5e0-940e59293451.PNG)

3. CiliaMiner allows easy access to all ciliopathy disease names and genes. For each gene linked to a disease, CiliaMiner provides localization along with the corresponding references.

4. Many ciliopathy researchers use a non-human model system, and CiliaMiner allows easy access to orthologs of disease-associated genes.

5. CiliaMiner allows the interactive comparison visualization of symptoms from various ciliopathy diseases. Users can add or remove the disease from the visualization. Users are able to download heatmaps in excellent resolution. 





