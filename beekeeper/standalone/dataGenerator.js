var dataGenerator = (function() {

	var predicates = [typeURI,
					typeURI,
					typeURI,
					subClassOfURI,
					subClassOfURI,
					subClassOfURI,
					subPropertyOfURI,
					subPropertyOfURI,
					subPropertyOfURI,
					domainURI,
					rangeURI];

	var counters;
	var terms;
	
	var namespaces = [];
	
	function reset() {
		counters = {};
		terms = [];
		
		if (namespaces.length) {
			for (var i = 0, l = namespaces.length; i < l; i++ ) {
				counters[namespaces[i]] = 100000 * (i + 1);
			}
			getNewUniqueTerm(getRandomItem(namespaces));
		} else {
			counters['default_namespace'] = 100000;
			getNewUniqueTerm('default_namespace');
		}
	}
	
	function getRandomItem(array) {
		return array[Math.floor(Math.random() * array.length)];
	}

	function getNewUniqueTerm(namespace) {
		terms.push(namespace + '#' + counters[namespace]);
		return '' + counters[namespace]++;
	}

	function generateTriple() {
		var namespace = (namespaces.length) ? getRandomItem(namespaces) : 'default_namespace';
		var s = getRandomItem(terms);
		var p = getRandomItem(predicates);
		if (p == typeURI) {
			var o = (Math.round(Math.random())) ? classURI : propertyURI;
		} else {
			var o = namespace + '#' + getNewUniqueTerm(namespace);
		}

		var subject = '<' + s + '> ';
		var predicate = '<' + p + '> ';
		var object = '<' + o + '> ';

		var triple = subject + predicate + object + '.\n';

		return triple;
	}
	
	function generateSameAsTriple() {
		var namespace = (namespaces.length) ? getRandomItem(namespaces) : 'default_namespace';
		var s = getRandomItem(terms);
		var p = sameAsURI;
		var o = namespace + '#' + getNewUniqueTerm(namespace);
		
		var subject = '<' + s + '> ';
		var predicate = '<' + p + '> ';
		var object = '<' + o + '> ';

		var triple = subject + predicate + object + '.\n';

		return triple;
	}
	
	return {

		addNamespace: function(namespace) {
			if (namespaces.indexOf(namespace) == -1) {
				namespaces.push(namespace);
			}
		},
		
		removeNamespace: function(namespace) {
			var index = namespaces.indexOf(namespace);
			
			if (index > -1) {
				namespaces.splice(index, 1);
			}
		},

		generate: function(numberOfTriples) {
			reset();
			
			var data = '';
			
			var numberOfSameAsTriples = Math.floor(numberOfTriples / 10);
	
			for (var i = 0; i < numberOfTriples; i++) {
				data += (i < numberOfTriples - numberOfSameAsTriples) ? generateTriple() : generateSameAsTriple();
			}
	
			return data;
		}
		
	};

})();


// Parameters:
//
// number of lines

// Procedure:
//
// - make collection of possible RDF subjects/objects
// - choose predicates (URI's) to include
// - randomly select combinations of subject+predicate+object (based on probabilities)
//
// random number in range: Math.floor(Math.random()*(to-from+1)+from);

// Output:
//
// <red wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .
// <white wine> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <wine> .
// <wine> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <beverage> .
// <beverage> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <food> .
// <food> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <consumable> .
// <liquid> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <beverage> .