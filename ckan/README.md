# CKAN RDF Scrape

This package allows you to scrape a CKAN Web site.

[CKAN](http://ckan.org/) is a data management system.
The CKAN scrapes consist of metadata about datasets.

This can be interesting for running Big Data experiments
on multiple datasets.

# Usage

After cloning this repository locally,
you can execute the following commands.

## Scrape site

Scrape a specific CKAN site and store the results in the given file.
~~~
./run.pl --site=SITE --command=download_catalog
~~~
For example `SITE` can be `datahub_io`.

## List supported sites

Shows the list of supported CKAN sites:
~~~
./run.pl --command=list_sites
~~~

## LOD download

This command is specifically interesting for LOD researchers,
since it downloads all the databases that are described
by a certain CKAN site.

~~~
./run.pl --site=SITE --command=download_lod
~~~

