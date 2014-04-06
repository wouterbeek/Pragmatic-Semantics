var rdfGraph = (function() {

    var nodesIndex = {},
        edgesIndex = {},
        nodes = {},
        edges = {},
        inferredEdges = {},
        classes = {},
        properties = {};

    var numOfTriples = 0,
        numOfInferredTriples = 0;

    function newNode(id, type, RDFlinkTarget) {
        var node = {
            'id': id,
            'type': type,
            'RDFlinkTarget': RDFlinkTarget
        }

        m.notify('rdf:nodeNew', node);
        
        return node;
    }

    // TODO: add newInferredNode

    function newEdge(source, id, target, type) {
        var edge = {
            'source': source,
            'id': id,
            'target': target,
            'type': type
        }

        m.notify('rdf:edgeNew', edge);

        return edge;
    }

    function newInferredEdge(source, id, target, steps, type, inferenceType) {
        var edge = {
            'source': source,
            'id': id,
            'target': target,
            'steps': steps,
            'type': type,
			'inferenceType': inferenceType
        }

        m.notify('rdf:inferredNew', edge);

        return edge;
    }

    return {

        initialize: function() { // newGraph: function() {
            // ?
            m.notify('rdf:initialized', rdfGraph);
        },

        clearGraph: function() {
            nodesIndex = {};
            edgesIndex = {};
            nodes = {};            
            edges = {};
            inferredEdges = {};
            classes = {},
            properties = {};
            numOfTriples = 0;
            numOfInferredTriples = 0;

            m.notify('rdf:cleared');
        },
		
		getNodesIndex: function() {
			return nodesIndex;
		},
		
		getEdgesIndex: function() {
			return edgesIndex;
		},

        loadFromN3String: function(string) {
            var triples = N3parser.parse(string);
            // console.log(triples);
            numOfTriples = triples.length;

            for (var i = 0; i < numOfTriples; i++) {
                if (triples[i].o.type == 'literal') { continue; }

                var RDFlinkTarget = (triples[i].p.value == sameAsURI);

                if (triples[i].o.value == classURI && triples[i].p.value == typeURI) {
                    classes[triples[i].s.value] = 1; // classes[triples[i].s.value] + 1 || 1
                }

                if (triples[i].o.value == propertyURI && triples[i].p.value == typeURI) {
                    properties[triples[i].s.value] = 1; // properties[triples[i].s.value] + 1 || 1
                }

                var subject = this.newNode(triples[i].s.value, triples[i].s.type) || this.getNode(triples[i].s.value);
                var object = this.newNode(triples[i].o.value, triples[i].o.type, RDFlinkTarget) || this.getNode(triples[i].o.value);

                this.newEdge(subject, triples[i].p.value, object, triples[i].p.type);
            }

            console.log(classes);
            console.log(properties);
			console.log(nodesIndex);

            m.notify('rdf:loaded', numOfTriples);
        },

        loadFromString: function(string, format) {
            //
            if (format == 'n3') this.parseN3(string);

            // m.notify('rdf:loaded', rdfGraph);
        },

        loadFromWebStorage: function(data, format) {
            //
            if (localStorage[config.ownerID + '_graph']) {
                var graphData = JSON.parse(localStorage[config.ownerID + '_graph']);
                nodesIndex = graphData.nodesIndex;
                edgesIndex = graphData.edgesIndex;
                nodes = graphData.nodes;
                edges = graphData.edges;
                classes = graphData.classes;
                properties = graphData.properties;
                numOfTriples = graphData.numOfTriples;
                // add edges and nodes from data
            }

            if (format == 'n3') this.parseN3(data);

            // m.notify('rdf:loaded', rdfGraph);
        },

        loadFromFile: function(file, format) {
            // 
            if (format == 'n3') this.parseN3(file);

            // m.notify('rdf:loaded', rdfGraph);
        },

        parseN3: function(data) {

        },

        saveToWebStorage: function() {
            var graphData = {};
            graphData.nodesIndex = nodesIndex;
            graphData.edgesIndex = edgesIndex;
            graphData.nodes = nodes;
            graphData.edges = edges;
            graphData.classes = classes;
            graphData.properties = properties;
            graphData.numOfTriples = numOfTriples;

            localStorage[config.ownerID + '_graph'] = JSON.stringify(graphData);
        },

        saveToFile: function() {
            var data = [], triple = null, source = '', target = '';

            for (edge in edges) {
                triple = edges[edge];

                source = '<' + triple.source.id + '>';
                if (triple.source.type == 'blank')
                    source = triple.source.id;

                target = '<' + triple.target.id + '>';
                if (triple.target.type == 'blank')
                    target = triple.target.id;
                if (triple.target.type == 'literal')
                    target = '"' + triple.target.id + '"';

                data.push(source + ' <' + triple.id  + '> ' + target + ' .');
            }

            data = data.join('\n');
            var filename = prompt('Filename: ', 'export.nt');
            saveAsFile(filename, data);
        },

        newNode: function(id, type, RDFlinkTarget) {
            if (typeof nodesIndex[id] == 'undefined') {
                nodesIndex[id] = newNode(id, type, RDFlinkTarget);
                nodes[id] = 1;

                return nodesIndex[id];
            } else {
                nodes[id]++;

                return null;
            }
        },

        // TODO: add newInferredNode

        newEdge: function(subject, predicate, object, type) {
            var edge = newEdge(subject, predicate, object, type);

            edges[this.edgeToString(edge)] = edge;

            if (typeof edgesIndex[subject.id] == 'undefined')
                edgesIndex[subject.id] = {};
            edgesIndex[subject.id][this.edgeToString(edge)] = edge;

            if (typeof edgesIndex[object.id] == 'undefined')
                edgesIndex[object.id] = {};
            edgesIndex[object.id][this.edgeToString(edge)] = edge;

            return edge;
        },

        newInferredEdge: function(subject, predicate, object, type, inferenceType) {
            if (!edges[subject.id + ' ' + predicate + ' ' + object.id]) {
                var edge = newInferredEdge(subject, predicate, object, '[steps]', type, inferenceType);
                inferredEdges[this.edgeToString(edge)] = 1;

                numOfInferredTriples++;
                numOfTriples++;

                // console.log(inferredEdges);

                return this.newEdge(subject, predicate, object, type);
            } else {
                if (inferredEdges[subject.id + ' ' + predicate + ' ' + object.id]) {
                    inferredEdges[subject.id + ' ' + predicate + ' ' + object.id]++;
                }

                return null;
            }
        },

        getNode: function(id) {
            if (nodesIndex[id]) {
                return nodesIndex[id];
            } else {
                return null;
            }        
        },

        getRandomNode: function() {
          var keys = Object.keys(nodes);
          // console.log(keys[Math.floor(keys.length * Math.random())]);
          return nodesIndex[keys[Math.floor(keys.length * Math.random())]];
        },

        getNodes: function() {
            return nodes;
        },

        nodeToString: function(node) {
            return node.id;
        },

        getSortedNodes: function() {
            return Object.keys(nodes).sort(function(a,b) { return nodes[b] - nodes[a] });
        },

        getEdges: function(nodeId) {
            return edgesIndex[nodeId];
        },

        edgeToString: function(edge) {
            return edge.source.id + ' ' + edge.id + ' ' + edge.target.id;
        },

        edgeGetOtherEnd: function(edge, node) {
            return (node == edge.source) ? edge.target : edge.source;
        },

        getRDFSedges: function(nodeId) {
            //
        },

        getLinkEdges: function(nodeId) {
            var linkEdges = [];
            var edges = edgesIndex[nodeId];
            for (edge in edges) {
                if (edge.id == sameAsURI) linkEdges.push(edge);
            }
            return linkEdges;
        },

        getRandomEdgeFromNode: function(node) {
            var edges = edgesIndex[node.id];
            var keys = Object.keys(edges);
            return edges[keys[Math.floor(keys.length * Math.random())]];
        },

        inferredEdgeToString: function(edge) {
            return edge.source.id + ' ' + edge.id + ' ' + edge.target.id;
        },

        getClasses: function() {
            return classes;
        },

        getProperties: function() {
            return properties;
        },

        getNumOfTriples: function() {
            return numOfTriples;
        }

    };

})();