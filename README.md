
# bohemiase

`bohemiase` is an R package meant for use by the Bohemia project. It handles data retrieval, storage, backups, and some utilities for analysis and reporting.


## Installation

Either

a) clone this repo, `cd` into it, and then run `Rscript build_package.R`, or

b) from within an R session, run `remotes::install_github('databrew/bohemiase')`

If installing on a linux shiny server, instead run:

```
sudo su - -c "R -e \"remotes::install_github('databrew/bohemiase')\"";
```


## Initial configuration

For `bohemiase` functions to work, it is essential that a `bohemia_credentials.yaml` file be created an its location be exported as an environment variable named `bohemia_credentials`. In order to set up the file and the environment variable correctly, run the following in R:

```
bohemiase::credentials_check()
```

The above needs only to be run once. Thereafter, the function can be written by explicitly naming the path as follows:

```
bohemiase::credentials_check('/path/to/bohemia_credentials.yaml')
```

In the above scenario, the function does not write any follows, nor does it solicit any credentials; rather, it just sets the environment variables `bohemia_credentials` to the specified path, so that functions which require that environment variable can work.


## Use

### Data retrieval

#### Retrieve data from Central

To retrieve data from Central and save into an R object (a list of nested dataframes), run:

```
# Retrieve all forms
data_list <- retrieve_data_from_central(fids = NULL)
# Retrieve just some forms
data_list <- retrieve_data_from_central(fids = c('pigsa', 'pigsb', 'pigsc', 'pkdatacollectiontool', 'serefusalsabsences', 'varefusals'))
```

If the `fids` argument above is NULL, all forms will be retrieved; otherwise, only those specified by `fids`.

#### Retrieve data from Aggregate

To retrieve data from Aggregate and save into an R object (a list of nested dataframes), run:

```
# Retrieve all forms
data_list_agg <- retrieve_data_from_aggregate(fids = NULL)
# Retrieve just some forms
data_list_agg <- retrieve_data_from_aggregate(fids = c('pigsa', 'pigsb', 'pigsc', 'pkdatacollectiontool', 'serefusalsabsences', 'varefusals'))
```

If the `fids` argument above is NULL, all forms will be retrieved; otherwise, only those specified by `fids`.

#### Get a full dump from Central

Instead of retrieving dataframes as nested lists, one can also simply get a full dump of the ODK Central server using:

```
backup_central('path/to/file/to/save/dump.zip')
```

#### Get a full dump from Aggregate

Instead of retrieving dataframes as nested lists, one can also simply get a full dump of the ODK Aggregate server using:

```
backup_aggregate('path/to/file/to/save/dump.zip')
```


### Storage and backups

Retrieving data (above) is important. But once retrieved, data must also be stored. The `save_to_aws()` function handles this, both for (a) full dump zip files and (b) specific R files.


#### Retrieve data from AWS

Retrieving data from Central is slow. For most analytical and reporting purposes, retrieving data from AWS S3 is faster and simpler. In order to do this, use the `retrieve_data_from_aws()` function. If a `fid` is specified, it will return the most recent snapshot of data for that form ID; if it is not specified, it will return a list of _all_ dataframes, always using the most recent snapshot of data. Here is an example:

```
# Retrieve all forms from the Central server
aws_data <- bohemiase::retrieve_data_from_aws(fid = NULL)
# Retrieve all forms from the Aggregate server
aws_data_aggregate <- bohemiase::retrieve_data_from_aws(fid = NULL, central = FALSE)
```


## Where are these functions actually used?

In the `bohemia` repo at `pipeline`.

## Storage structure on AWS

The code in `pipeline` calls the above functions so as to retrieve forms from both the Central and Aggregate server, and saves them into the `bohemia2022` bucket on AWS S3. Therein, data is organized as follows.

*For individual forms*
```
<bucket>/<server>/<project>/<form ID>/<timestamp>.RData
```

*For all forms from a server in one list*
```
<bucket>/<server>/<project>/<timestamp>/data_list.RData
```

*For a zip file / backup of the server*
```
<bucket>/<server>/<project>/<timestamp>/backup.zip
```

In the case of the ODK Aggregate server, the `<project>` field consists of the word "Aggregate" (since ODK Aggregate does not have the "project" concept).



