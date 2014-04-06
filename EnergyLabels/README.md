# EnergyLabels of the Netherlands

This code creates RDF representations of energy label data
that is provided in an XML format which is difficult to process.

The code of the XML2RDF convesion is written in SWI-Prolog and requires
modules from the Prolog Generics Collection (PGC)
(see https://github.com/wouterbeek/PGC) in order to work.

## Dataset conversion procedure

The script assumes the input files are in subdirectory `Data/Input`
and places the output files in a subdirectory called `Data/Output`
(with partial results in `Data/stageN` for various stages `N`).

The translation is as follows:
  1. File `v20130401.dx.tar.gz` is extracted.
  2. File `v20130401.dx` contains one line of XML (!).
     This single line is split into multiple lines
     (cutoff between end and start tags).
     Note: for this a byte splitting method is used.
     To circumvent within-character splits we take multiples of 2 bytes.
  3. The XML file with multiple lines can be streamed on a per-line basis
     (since it is too big to load its entire DOM into memory).
     The XML file's streamed DOM is turned into RDF using
     a generic XML2RDF conversion (from the Prolog Generics Collection).
  4. The resulting RDF is stored into a Turtle file.

According to this conversion tool, the XML file contains
2.354.560 measurements.

## Origin of the data

The XML document was made available by the Dutch ministry:
  * https://data.overheid.nl/data/dataset/energielabels-agentschap-nl
  * ftp://ftp.ep-online.nl (login=`Apps4Holla`,password=`Apps4Holla_EPBD`)

Wouter Beek
me@wouterbeek.com
2013/10/05

