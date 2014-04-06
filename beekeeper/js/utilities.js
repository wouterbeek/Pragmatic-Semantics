var utils = (function() {

	var typeToURI = {
		'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns',
		'rdfs': 'http://www.w3.org/2000/01/rdf-schema',
		'owl': 'http://www.w3.org/2002/07/owl'
		// etc.
	}

	var URItoType = {
		'http://www.w3.org/1999/02/22-rdf-syntax-ns': 'rdf',
		'http://www.w3.org/2000/01/rdf-schema': 'rdfs',
		'http://www.w3.org/2002/07/owl': 'owl'
		// etc.
	}

	return {
		
		guid: function() {
			return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) { var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8); return v.toString(16);	});
		},

		getHash: function(string) {
			var hash = string.slice(string.lastIndexOf('#') + 1);
			return (hash != string) ? hash : string.slice(string.lastIndexOf('/') + 1); // .split('#').pop();
		},

		getBase: function(string) {
			// inverse of getHash(), remove hash from string, return base uri
			return string.substring(0, string.lastIndexOf('#'));
		},

		validId: function(string) {
			return 'n' + string.replace(/[^a-z0-9]/gi, ''); // just remove whitespace: replace(/ /g,'')
		},

		getTypeURI: function(string) {
			return URItoType[this.getBase(string)];
		}

	};

})();