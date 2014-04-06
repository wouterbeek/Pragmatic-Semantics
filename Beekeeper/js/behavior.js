var typeURI = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    subClassOfURI = 'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    subPropertyOfURI = 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
    domainURI = 'http://www.w3.org/2000/01/rdf-schema#domain',
    rangeURI = 'http://www.w3.org/2000/01/rdf-schema#range',
    resourceURI = 'http://www.w3.org/2000/01/rdf-schema#Resource',
    classURI = 'http://www.w3.org/2002/07/owl#Class', //'http://www.w3.org/2000/01/rdf-schema#Class', 'http://www.w3.org/2002/07/owl#Class'
    propertyURI = 'http://www.w3.org/2002/07/owl#ObjectProperty', //'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property', 'http://www.w3.org/2002/07/owl#ObjectProperty', 'http://www.w3.org/2002/07/owl#DatatypeProperty'
    literalURI = 'http://www.w3.org/2000/01/rdf-schema#Literal',
    datatypeURI = 'http://www.w3.org/2000/01/rdf-schema#Datatype'
    sameAsURI = 'http://www.w3.org/2002/07/owl#sameAs';

var behavior = {

	// - number of scouts to initialize
	foragerEnergy: 3,

	initializeScouts: function() {
		//
	},

	initializeForagers: function(type, location) {
		//
	},

	initializeNurseBees: function() {
		//
	},

	scoutMove: function(scout) {
		var edge = rdfGraph.getRandomEdgeFromNode(scout.isAt);
		var target = rdfGraph.edgeGetOtherEnd(edge, scout.isAt);

		var moveType = 'moved';

		if (edge.id != sameAsURI) {
			scout.isAt = target;
			m.notify('swm:sctMove', scout);
		} else {
			var base = utils.getBase(target.id);

			if (config.linksetsEnabled && config.hostedBy[base] && config.hostedBy[base].indexOf(scout.owner) == -1 && $('#' + config.hostedBy[base][0]).hasClass('connected')) { // && config.hosts.indexOf(base) == -1
				scout.isAt = target;
				// send scout to peer that hosts target dataset
				p2p.send('scouts', [scout], config.hostedBy[base][0]); // or config.hostedBy[base][random]

				m.notify('swm:sctMigratedTo', scout, config.hostedBy[base][0]); // or m.notify('swm:sctsSentTo', scout, config.hostedBy[base][0]);
				// or implement removing and sending of scout at 'scoutMigrated' listener

				moveType = 'migrated';
			} else {
				// console.log('sameAs relation found, but no known hoster of the target');
				scout.isAt = target;
				m.notify('swm:sctMove', scout);
			}
		}

		return moveType;
	},

	scoutFoundSomething: function(scout) {
		if (scout.node == scout.isAt.id) {
			monitor('Scout from ' + scout.owner + ' found:', 'scout id: ' + scout.id);
			monitor('&nbsp;&nbsp;' + utils.getHash(scout.node), '', 'data');

			m.notify('swm:sctFound', scout);
			return true;
		} else {
			return false;
		}
	},

	foragerMove: function(forager) {
		forager.wasAt = forager.isAt;
		forager.passedEdge = rdfGraph.getRandomEdgeFromNode(forager.isAt)
		forager.isAt = rdfGraph.edgeGetOtherEnd(forager.passedEdge, forager.isAt);
		forager.energy--;

		m.notify('swm:fgrMove', forager);
	},

	foragerFoundSomething: function(forager) {
        var s = forager.triple.s || forager.wasAt.id;
        var p = forager.triple.p || forager.passedEdge.id;
        var o = forager.triple.o || forager.isAt.id;

        var wasAt = forager.wasAt.id;
        var passedEdge = forager.passedEdge.id;
        var isAt = forager.isAt.id;
        var target = forager.passedEdge.target.id;

        if (wasAt == s && passedEdge == p && isAt == o && o == target) { // TODO: && not already found/sent
			monitor('Forager from ' + forager.owner + ' found:', 'forager id: ' + forager.id);
			monitor('&nbsp;&nbsp;' + utils.getHash(s) + ' ' + utils.getHash(p) + ' ' + utils.getHash(o), '', 'data');

			forager.memory = { 'source': s, 'id': p, 'target': o, 'type': utils.getTypeURI(p) };
			// forager.energy++; // forager.energy = workerEnergy; // forager = swarm.removeForager(forager);
			m.notify('swm:fgrFound', forager);
			return true;
		} else {
			return false;
		}
	},

	foragerIsExhausted: function(forager) {
		if (forager.energy < 1) {
			monitor('Forager from ' + forager.owner + ' is exhausted.', 'forager id: ' + forager.id);

			m.notify('swm:fgrExhausted', forager);
			return true;
		} else {
			return false;
		}
	},

	nurseBeeMove: function(nurseBee) {
		nurseBee.prevEdge = nurseBee.passedEdge;
		nurseBee.wasAt = nurseBee.isAt;
		nurseBee.passedEdge = rdfGraph.getRandomEdgeFromNode(nurseBee.isAt)
		nurseBee.isAt = rdfGraph.edgeGetOtherEnd(nurseBee.passedEdge, nurseBee.isAt);

		m.notify('swm:nrsMove', nurseBee);
	},

	nurseBeeFoundSomething: function(nurseBee) {
		var found;

		switch (nurseBee.type) {
			case 'rdfs5':  // uuu rdfs:subPropertyOf vvv . && vvv rdfs:subPropertyOf xxx . > uuu rdfs:subPropertyOf xxx .
				if (nurseBee.prevEdge != null && nurseBee.prevEdge.id == subPropertyOfURI && nurseBee.passedEdge.id == subPropertyOfURI) {
					if (nurseBee.prevEdge.target == nurseBee.passedEdge.source) {
						found = rdfGraph.newInferredEdge(nurseBee.prevEdge.source, subPropertyOfURI, nurseBee.passedEdge.target, 'rdfs');
					} else if (nurseBee.prevEdge.source == nurseBee.passedEdge.target) {
						found = rdfGraph.newInferredEdge(nurseBee.passedEdge.source, subPropertyOfURI, nurseBee.prevEdge.target, 'rdfs');
					}
				}
				break;
			case 'rdfs11': // uuu rdfs:subClassOf vvv .    && vvv rdfs:subClassOf xxx .    > uuu rdfs:subClassOf xxx .
				if (nurseBee.prevEdge != null && nurseBee.prevEdge.id == subClassOfURI && nurseBee.passedEdge.id == subClassOfURI) {
					if (nurseBee.prevEdge.target == nurseBee.passedEdge.source) {
						found = rdfGraph.newInferredEdge(nurseBee.prevEdge.source, subClassOfURI, nurseBee.passedEdge.target, 'rdfs');
					} else if (nurseBee.prevEdge.source == nurseBee.passedEdge.target) {
						found = rdfGraph.newInferredEdge(nurseBee.passedEdge.source, subClassOfURI, nurseBee.prevEdge.target, 'rdfs');
					}
				}
				break;
			case 'rdfs9':  // uuu rdfs:subClassOf xxx .    && vvv rdf:type uuu .           > vvv rdf:type xxx .
				if (nurseBee.prevEdge != null && nurseBee.prevEdge.id == subClassOfURI && nurseBee.passedEdge.id == typeURI && nurseBee.prevEdge.source == nurseBee.passedEdge.target) {
					found = rdfGraph.newInferredEdge(nurseBee.passedEdge.source, typeURI, nurseBee.prevEdge.target, 'rdf');
				} else if (nurseBee.prevEdge != null && nurseBee.prevEdge.id == typeURI && nurseBee.passedEdge.id == subClassOfURI && nurseBee.prevEdge.target == nurseBee.passedEdge.source) {
					found = rdfGraph.newInferredEdge(nurseBee.prevEdge.source, typeURI, nurseBee.passedEdge.target, 'rdf');
				}
				break;
			case 'rdfs8':
				if (nurseBee.passedEdge != null && nurseBee.passedEdge.id == typeURI && nurseBee.isAt.id == classURI) {
					// add: nurseBee.wasAt | subClassOfURI | resourceURI
					// *	first add (if not exists) resourceURI node to graph
					// found = rdfGraph.newInferredEdge(nurseBee.wasAt.id, subClassOfURI, resourceURI, 'rdfs');
				}
				break;
			case 'rdfs13':
				if (nurseBee.passedEdge != null && nurseBee.passedEdge.id == typeURI && nurseBee.isAt.id == datatypeURI) {
					// add: nurseBee.wasAt | subClassOfURI | literalURI
					// *	first add (if not exists) literalURI node to graph
					// found = rdfGraph.newInferredEdge(nurseBee.wasAt.id, subClassOfURI, literalURI, 'rdfs');
				}
				break;
			default:
				found = false;
		}

		if (found) {
			var subject = utils.getHash(found.source.id);
			var predicate = utils.getHash(found.id);
			var object = utils.getHash(found.target.id);

			monitor('Nurse bee found:', 'nurse bee id: ' + nurseBee.id);
			monitor('&nbsp;&nbsp;' + subject + ' ' + predicate + ' ' + object, '', 'data');

			m.notify('swm:nrsFound', found);
			return true;
		} else {
			return false;
		}
	}

};

// found = matches(this,
// 	{ 'prevEdge': subClassOfURI, 'passedEdge': subClassOfURI, 'prevEdge.target': this.passedEdge.source },
// 	{ 'source': this.prevEdge.source, 'id': subClassOfURI, 'target': this.passedEdge.target, 'type': 'rdfs' });

function matches(context, pattern, result) {
	/*
		pattern:
			prevEdge.source
			prevEdge
			prevEdge.target
			passedEdge.source
			passedEdge
			passedEdge.target
			wasAt
			isAt
	*/
	for (item in pattern) {
		if (context[item] && context[item] != pattern[item]) return null;
	}

	return rdfGraph.newInferredEdge(result.source, result.id, result.target, result.type);
}