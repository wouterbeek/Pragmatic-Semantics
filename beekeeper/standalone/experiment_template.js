var d;

m.when('|someEvent|', function(data) {
	// add something to a property of d

	d.|someName|.push(data.|value|);
});

function makeExport() {
	// build and return export string for file

	return d.|someName|.join('\n');
}

/*

EVENTS


connection.js

m.when('p2p:open', logger);
m.when('p2p:conn', logger);
m.when('p2p:data', logger);
// m.when('p2p:sent', logger);
m.when('p2p:close', logger);


rdfGraph.js

m.when('rdf:initialized', logger);
m.when('rdf:loaded', logger);
m.when('rdf:cleared', logger);
m.when('rdf:nodeNew', logger);
// m.when('rdf:nodeUpd', logger);
// m.when('rdf:nodeDel', logger);
m.when('rdf:edgeNew', logger);
// m.when('rdf:edgeUpd', logger);
// m.when('rdf:edgeDel', logger);
m.when('rdf:inferredNew', logger);
// m.when('rdf:inferredUpd', logger);
// m.when('rdf:inferredDel', logger);


visualization.js

m.when('vis:loaded', logger);
m.when('vis:updated', logger);
m.when('vis:nodeNew', logger);
// m.when('vis:nodeUpd', logger);
// m.when('vis:nodeDel', logger);
m.when('vis:nodeSel', logger);
m.when('vis:nodeUnsel', logger);
m.when('vis:linkNew', logger);
// m.when('vis:linkUpd', logger);
// m.when('vis:linkDel', logger);
// m.when('vis:linkSel', logger);
// m.when('vis:linkUnsel', logger);


swarm.js

m.when('swm:initialized', logger);
m.when('swm:sctInit', logger);
m.when('swm:sctsSentTo', logger);
m.when('swm:sctMigratedTo', logger);
m.when('swm:sctMove', logger);
m.when('swm:sctFound', logger);
m.when('swm:sctRemoved', logger);
m.when('swm:fgrInit', logger);
m.when('swm:fgrsSentTo', logger);
m.when('swm:fgrMove', logger);
m.when('swm:fgrFound', logger);
m.when('swm:fgrExhausted', logger);
// m.when('swm:fgrReturnedFrom', logger);
m.when('swm:fgrRemoved', logger);
m.when('swm:nrsInit', logger);
m.when('swm:nrsMove', logger);
m.when('swm:nrsFound', logger);
// m.when('swm:nrsRemoved', logger);
m.when('swm:cycleComplete', logger);

*/