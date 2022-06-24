<slides>

<!--=========================================================================-->
<!--=========================================================================-->
<title>
My name is Patrice Godard. I am a biochemist by training but I turned to
bioinformatics during my PhD thesis. I have worked in the biotech and
biopharmaceutical industry for 12 years and my main area of expertise is
in omics data analysis.

I'm going to present you the *TKCat* package which has been
developed to manage and leverage, in R, knowledge that has been extracted
from external resources or generated from internal research projects.

And to this aim,
I will discuss specific examples related to the role and the activities
of my team within the UCB company.
</title>

<!--=========================================================================-->
<!--=========================================================================-->
<ucb_tbn>
**UCB** is a biopharmaceutical company focused on creating value for people
living with severe diseases, mainly in immunology and neurology.

In this context, the translational bioinformatics team participates
in **3 main research missions**:

***

- The first one is about getting a better understanding of *disease mechanisms*
and how they can be experimentally studied

<!--
   - By characterizing the molecular and cellular status of affected tissues
   - And by assessing the relevance of experimental models in vivo or in vitro.
-->

***

- The second mission is to identify new relevant *therapeutic approaches*

<!--
   - By generating hypotheses based on molecular information
   - And by developing molecular biomarkers of a condition status
   and of target engagement.
-->

***

- And the third one is to identify *therapeutic opportunities*
in patient populations

<!--
   - By better understanding patient populations
   - And by leveraging awareness of existing solutions
-->

The examples I'll will show today are related to these activities, but the
*TKCat* package is also applicable to many other knowledge areas.
</ucb_tbn>

<!--=========================================================================-->
<!--=========================================================================-->
<knowledge>
Now, let's clarify what I mean by knowledge,
using this image I've seen floating around in social networks.

It depicts quite well, among other things, the different kind of assets
used or produced in the frame of data analysis projects.

On one hand,
original **data** is often the output of
devices such as an image, a signal or a DNA sequence.

**Information** is interpreted data, such as a genetic variant observed
in an individual compared to a reference,
or the level of expression of one gene in one sample.

And **knowledge** can be seen as consistent information which has been
combined with relationships identified between the different elements.

On the other hand,
**insights** correspond to key elements of the knowledge which are
of particular importance in a given context.
For example, the understanding of a molecular mechanism involved in a condition
of interest.

And finally, **wisdom** can be seen as relevant paths for leveraging knowledge
insights,
such as the identification of a new therapeutic approach targeting a relevant
molecular pathway.

***

Unfortunately, wisdom is not a given,
whatever the quality of the upstream assets,
and we need to be careful not to take our fantasy for it.

***

Anyway, in this presentation, I'm not going to address this point,
and I'll focus *wisely* on the knowledge itself:
how it can be structured, documented and efficiently leveraged.
</knowledge>

<!--=========================================================================-->
<!--=========================================================================-->
<knowledge_features>
More precisely, what are the expected features of the knowledge we propose to
manage with *TKCat*?

***

First, we deal with many different concepts. In our case it can be:
disease, phenotypes, genes,
proteins, tissues, organs, cells, organelles, molecular pathways, drugs,
gene expression, protein abundance and so on.

And these concepts are related to each other in a way which highly depends
on the context.

That's why documenting the knowledge with a data model is very valuable and the
*ReDaMoR* R package is dedicated to this task.

***

We also make the hypothesis that most of the data underlying the knowledge
can be organized in tables or in matrices.

We want to use them in R, of course. But, we also want them to be accessible
and reusable in other environments as much as possible.

That's why structured folders and text files have been chosen to archive
the knowledge and also as one solution to exchange it, especially with
external collaborators.

***

Developing a system integrating all pieces of knowledge is a very difficult
task and highly risky.

That's why we prefer to keep the different pieces of knowledge independent but
still ready for integration.

Indeed, the different pieces of knowledge often refer to similar concepts, like
genes or diseases in our case.
And those concepts can be used to build bridges across information areas.

But these concepts are also often implemented using different scopes and
references. For example, some of the resources we manipulate refer to proteins
whereas others refer to their coding genes.

That's why we have developed dictionaries of concepts which are used to
combine and integrate information when needed: *BED* focuses on biological
entities, such as genes and proteins, whereas *DODO* focuses on conditions like
diseases and phenotypes.

***

Data supporting the knowledge can be quite large depending on the scope.

And we want, depending on the context, either to use the corresponding tables
as a whole or by subsets.

Also, the knowledge information is not supposed to be updated very frequently.

<!--
I mean, we are
far from the frequency of the update of data in a social network or in a CRM.
-->

But it can still be valuable to keep track of the different versions
of this knowledge which, by nature, tends to evolve.

Finally, some data are more sensitive than the others, or come with license
restriction which can limit their use to a set of users.

All these different features led to the choice of the *ClickHouse* Database
Management System for spreading the knowledge internally.
</knowledge_features>

<!--=========================================================================-->
<!--=========================================================================-->
<mdb_features>
Now, I would like to spend time on a key and central type of object in *TKCat*
called **MDB**.

*MDB* stands for Modeled Database and it is
used to organize a specific knowledge resource relying on the following
organization.

***

First, an *MDB* gathers the **data** themselves, which can be tables
or matrices.

***

Those data are documented by a **data model** produced using *ReDaMoR*.

***

**General information** or metadata are also added to the object: they provide
a title, a short description and some references.

***

Finally, some tables are annotated as **collection members**.

It means that those
tables refer to key concepts that can be used to build bridges across
different knowledge resources.

<!--
In the frame of our activities, we currently manage two kinds of collections:
*biological entities* and *conditions*. Those two collections are available in
the *TKCat* package but other can be implemented by the users depending
on their needs.
-->

***

*TKCat* supports different **implementations** of *MDBs* which differ in the way
the data are made accessible: either in memory, in files or in the ClickHouse
DBMS.

***

Finally, *TKCat* stands for **Tailored Knowledge Catalog**

And, indeed, it is an R package providing
a set of tools for managing and using knowledge resources made available
in *MDBs*.

Now, I'm going to exemplify how to build and manipulate an *MDB*.
</mdb_features>

<!--=========================================================================-->
<!--=========================================================================-->
<draft>
For that purpose, I'm going to use some data made available within the *ReDaMoR*
package.

For this simple example, I'm going to use 3 tables describing the phenotypes
associated to different human diseases.

This information comes from the **Human Phenotype Ontology project** or *HPO*.

As a remember, a phenotype is an individual's observable traits, such as height,
eye color or blood type.

For example the occurrence of seizures is a strong phenotype of patients
suffering of epilepsy.

***

Using the *ReDaMoR* package, we can draft a very simple data model of these
three tables.

Each rectangle represents a table and the bullet points the fields in each
of them.

So far, excepted the data type, there is no constraint associated to the fields
and no relationship identified between the tables.
</draft>

<!--=========================================================================-->
<!--=========================================================================-->
<model>
To add such information, we can use the `model_relational_data()` function
which will launch a graphical user interface developed for creating
and manipulating relational data models.

In this case for example, we have made all the fields of all tables
non nullable. Excepted the *description* field of the hp table
which remains between brackets in this graphical representation.

The fields in bold correspond to primary keys of the tables.

We have also changed the type of a few fields and
added relevant relationships between the tables.

To summarize, the phenotypes are described in the *hp* table whereas the
diseases are described in the *disease* table.

The *diseaseHP* table is an association table between diseases
and phenotypes, each disease potentially presenting several phenotypes,
and each phenotype being potentially presented by several diseases, as
indicated by the cardinalities associated to the relationships.
</model>

<!--=========================================================================-->
<!--=========================================================================-->
<confront>
Once the data model is created it can be confronted to the data using
the `confront_data()` function.

This function returns a report that helps to efficiently correct the data or
the model when needed.

<!--

This function returns a structured report that can be formatted for display
purposes as shown on the right.

In this case the confrontation of the model to the data failed because of
inconsistent types of some fields:

- The *level* field of the *hp* table should be an *integer* according to
the data model and not a *numeric* as implemented in the data.

- Similarly the *id* columns of the *diseases* and *diseaseHP* tables
should be *characters* according to the data model.

***

When we correct the data to fit the field types documented in the data model,
then the confrontation returns a success.

The warning about missing data in the *description* field is still displayed
even if not in contradiction with the data model.

-->

</confront>

<!--=========================================================================-->
<!--=========================================================================-->
<mdb>
The confrontation of the data model to the data also occurs when creating
an *MDB*, which is achieved as shown on the left.

The process of creation of an *MDB* follows the main feature of this type of
object I've described before. It takes the data tables, the data model,
and some general information.

***

The content of an *MDB* can then be easily explored and retrieved as exemplified
on the right.

Intuitively, the `db_info()` function returns general information, and
the `data_model()` function returns the data model.

The `select()` function is used to focus the *MDB* on a few tables and
the `pull()` function to extract a specific table.
</mdb>

<!--=========================================================================-->
<!--=========================================================================-->
<filter>
More interestingly, the data model can be used to filter the data
**transitively** through all tables.

On the right you have the original data model of the *MDB* we have just
constructed.

And on the left you have the number of rows in each table.

The aim of the highlighted command is to filter the *hp* table to keep rows
with the word "eye" found in the description field.

The idea is to get all the phenotypes regarding the *eyes*.

***

After applying this filter, the data model did not change.

However, on the bottom left you can see that the number of rows
of the *hp* table
decreased but also the number of rows of the two other tables which
were filtered accordingly.
</filter>

<!--=========================================================================-->
<!--=========================================================================-->
<join>
The data model can also be used to automatically join tables of interest.

For example, after having focused the HPO *MDB* on *eyes* related phenotypes,
it can be useful to put diseases directly in front of their corresponding
phenotypes.

This is achieved by the highlighted command.

***

This command alters the data model as shown at the bottom right while keeping
all the records as you can see at the bottom left.
</join>

<!--=========================================================================-->
<!--=========================================================================-->
<implementations>
As mentioned before, the *TKCat* package supports three main implementations
of *MDBs* that can be easily interconverted.

Each implementation presents features that make it more or less relevant
for different usages.

In *memoMDB* all the data are loaded in memory.

It makes data manipulation fast but greedy.

It is convenient when the data are small or when we need to work with the whole
tables.

///

In *fileMDB* the data remain in text files until requested.

It saves memory and allows to load only a few tables of interest when needed.
However it makes data filtering slow.

The main purpose of *fileMDB* is to archive the MDB and to share it
with external collaborators.

///

Finally, in *chMDB* the data are stored in a *ClickHouse* database.

It's very efficient to load the whole tables when needed, but also to
get only records of interest by sending *SQL* queries.

Moreover, mechanisms have been implemented for managing access rights and
for supporting the versioning of *chMDB*.

The main purpose of *chMDB* is to provide an easy and flexible access to the
data, which is quite convenient for sharing them internally.
</implementations>

<!--=========================================================================-->
<!--=========================================================================-->
<tkcat>
Multiple *chMDB* can be stored in the same *ClickHouse* instance,
providing therefore a standard access to many knowledge resources.

And they can be accessed via a *chTKCat* object which is a special connector
to *ClickHouse*.

The `explore_MDBs()` function opens a shiny user interface which can be
used to browse available knowledge resources.

***

When a resource is selected, its data model can be explored and
tables can be previewed and potentially downloaded.

This application can also be deployed to improve the awareness of available
resources within a community.

<!--
Information about how to create and manage a *ClickHouse* instance for *TKCat*
is provided in the documentation.
-->
</tkcat>

<!--=========================================================================-->
<!--=========================================================================-->
<collections>
A complete *fileMDB* object is provided as an example
within the *TKCat* pakcage.

This MDB is a subset of the information provided by the *ClinVar* database which
records how diseases - or in general traits - are caused by genetic variants.

In the data model shown on the left of the slide this information is organized
as following:

- On the right, in light blue and pink, you have genes and genetics information.
- At the top left, in blue, you have traits and diseases
- At the bottom left, in green,
you have genetic variant to disease associations.

The ClinVar database does not provide the phenotypes presented by patients
suffering of the different diseases.

But it could be interesting to have this information to assess how phenotypes
are related the genetics events.

To do that we can rely on the HPO MDBs we have constructed.

ClinVar and HPO both refer to the concept of health conditions, such as diseases
or traits, colored in blue in both data model.

To formalise this information, we use the *Collections* mechanism coming with
*TKCat*.

Two collections are currently implemented in *TKCat*: *Biological entity*
and *Condition*.

***

To document a table as a member of collection, we use
the `add_collection_member()` function in which we indicate how the information
is encoded: here, the type of condition,
the database from which the condition identifiers were taken
and the identifiers themselves.

***

The `collection_members()` function is used to list collection members
already documented in the MDB.

The ClinVar MDB, provided within the *TKCat* package,
presents 3 collection members: two *Conditions* and one *Biological Entity*.
</collections>

<!--=========================================================================-->
<!--=========================================================================-->
<merging>

<!--
Collection members can then be used to integrate two MDBs.

First, collections shared by the two MDBs are identified using
the `get_shared_collections()` function.

In this case, we have documented one Condition member for HPO and two for
ClinVar.

It gives us two ways to integrate these two MDBs, using one or the other
member in ClinVar: *ClinVar_traitCref* or the *ClinVar_traits* table

***

Then, we can call the *merge()* function which will integrate the two MDBs.

On the right you can see the data model of the MDB created after merging.

Behind the scene, *TKCat* uses a dictionary function which relies on information
about collection members to map identifiers from the 2 tables and create an
association table displayed in yellow in this data model.

We have implemented dictionaries for conditions and for biological entities
in the DODO and the BED packages.

But other dictionaries could be developed and used for those collections or
for collections that will be defined in the future.
-->

*Collections* are a powerful mechanism to merge different knowledge resources
based on shared concepts.

For the sake of time, I'm not going to describe this mechanism today.
You can find more information about it in the documentation.

Quickly, here you can see the results of merging 2 MDBs sharing the concept
of diseases. Thanks the DODO dictionary, an association table, in yellow,
has been automatically created between the tables providing disease references.

<!--
In this example, our HPO *MDB* example has been merged with a MDB provided
within the *TKCat* package and which corresponds to a sample of the ClinVar
database.

**ClinVar** reports the causal impact of gene mutation on diseases
and more generally on traits.

ClinVar and HPO both refer to the concept of disease via the *traitCref* table
for ClinVar and the *diseases* table for HPO.

When merging the two *MDBs*, an association table has been automatically created
between HPO diseases and ClinVar traits.

This automatic mapping relies on dictionaries that must be developed for each
concept of interest.
-->

<!--
So far we have developed the DODO package which focuses
on diseases and the BED package which focuses on biological entities such
as genes and proteins.
-->

</merging>

<!--=========================================================================-->
<!--=========================================================================-->
<types>
I would like to finish with a comment regarding supported data types
in *ReDaMoR* and *TKCat*.

Currently, the following **canonical R types** are supported.

***

In addition, *TKCat* allows the storing of **files** in *MDBs*
using *base64* encoding.

<!--
Therefore it has been defined as a specific type in *ReDaMoR*.
-->

***

Finally, **matrices and sparse matrices** are also supported by *TKCat*.
In the data model they are represented as tables with 3 fields:

   - one for row names
   - one for column names
   - and one for the values
   
<!--
*row* and *column* correspond therefore also to specific types in *ReDaMoR*.
-->
</types>

<!--=========================================================================-->
<!--=========================================================================-->
<acknowledgements>
Obviously, *ReDaMoR* and *TKCat* packages were developed relying
on existing tools.

I wanted to highlight here those of particular importance for the success of
this project.

However, it's not an exhaustive list that you can eventually find on the CRAN
or on github repositories.

I would also like to take this opportunity to thank the CRAN team for
their efficiency in the publication of those R packages.

***

To conclude, I would like to thank people from my team at UCB who have supported
this project at different levels.

Thanks to the organizers of the *useR!* conference who gave me the opportunity
to present this work,

and thank you all for the attention you gave to this presentation...
</acknowledgements>

<!--=========================================================================-->
<!--=========================================================================-->
<closing>
Don't hesitate to contact me if you have any question.
</closing>

<!--=========================================================================-->
<!--=========================================================================-->
</slides>
