# Question on syntax and semantics of object properties

Datahub/CKAN uses several types objects (e.g. organizations, packages, resources). I have problems understanding the properties these resources have.

Syntax:
1) Is there documentation available that describes the various properties an object can have?
2) Is these documentation available that identifies which properties are required and which are optional? (E.g. `id` may be required, `webstore_url` may be optional.
3) Are there restrictions for the values a property may have? Several properties have values that are strings which seem to encode dates in some ISO/RFC format. Knowing the format would be helpful. Several properties have strings that represent integers. Does this mean that the value can be a non-integer as well (mayne the string "NaN")?

Semantics:
4) Is there a (semantic) distinction between a property that has the value `@null` and a property that is absent?
5) There seem to be spurious properties in the Datahub, e.g. resource X has the `SpirosAlexiou` property. Doing a Web search on that string brings up the Datahub user page of a person named Spiros Alexiou (URL: http://datahub.io/nl/user/salexiou).

I have more examples of spurious proprties. If an overview of these is considered helpful I can make it available.

# Access denied

* (http://datahub.io/api/3/action/package_show?id=agricultural-water-demand-model-report-for-metro-vancouver-april-2013)
* (http://datahub.io/api/3/action/package_show?id=all-starbucks-locations-in-the-world)
* (http://datahub.io/api/3/action/package_show?id=api-for-voting-result-of-the-legco-meetings)
* (http://datahub.io/api/3/action/package_show?id=bhh-hn-2013)
* (http://datahub.io/api/3/action/package_show?id=bhh-hn-er-2013)
* (http://datahub.io/api/3/action/package_show?id=buergerhaushalt-hn-2012)
* (http://datahub.io/api/3/action/package_show?id=city-of-columbia-spending)
* (http://datahub.io/api/3/action/package_show?id=consent-law-map)
* (http://datahub.io/api/3/action/package_show?id=fcta)
* (http://datahub.io/api/3/action/package_show?id=finance)
* (http://datahub.io/api/3/action/package_show?id=global-and-country-by-country-access-to-improved-drinking-water-1990-2011)
* (http://datahub.io/api/3/action/package_show?id=green-communities-organics-diversion-carbon-credits-tonnes-co2e-by-municipality)
* (http://datahub.io/api/3/action/package_show?id=hirmer-jnm)
* (http://datahub.io/api/3/action/package_show?id=hirmer01)
* (http://datahub.io/api/3/action/package_show?id=hirmer2)
* (http://datahub.io/api/3/action/package_show?id=ibge-simple-table)
* (http://datahub.io/api/3/action/package_show?id=ko-phangan-people)
* (http://datahub.io/api/3/action/package_show?id=mietflaeche)
* (http://datahub.io/api/3/action/package_show?id=recorded-future-web-intelligence-on-protest-events-2011-2013)
* (http://datahub.io/api/3/action/package_show?id=retail-sales)
* (http://datahub.io/api/3/action/package_show?id=south-carolina-state-agency-spending-transparency)
* (http://datahub.io/api/3/action/package_show?id=tec-research)
* (http://datahub.io/api/3/action/package_show?id=test-manual-dataset)
* (http://datahub.io/api/3/action/package_show?id=testmuni)
* (http://datahub.io/api/3/action/package_show?id=tga-kosten)
* (http://datahub.io/api/3/action/package_show?id=voting-result-of-council-meetings)
* (http://datahub.io/api/3/action/package_show?id=wartungsmangel)

