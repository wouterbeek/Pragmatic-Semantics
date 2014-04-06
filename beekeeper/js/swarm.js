var swarm = (function(behavior) {

    var foragerEnergy = behavior.foragerEnergy;

    var numOfScouts = 0,
        numOfForagers = 0,
        numOfNurseBees = 0;

    var scouts = [],
        foragers = [],
        nurseBees = [];

    var s = 0, f = 0, n = 0, cycle = 0;
    var totalTime = 0, start = 0, end = 0;

    function newScout(id, owner, node, type, location) {
        var scout = {
            'id': id,
            'owner': owner,
            'node': node,
            'type': type,
            'isAt': location
        }

        m.notify('swm:sctInit', scout);

        return scout;
    } // TODO: add type of scout (class/property) to determine which kind of foragers to initialize

    var scoutMove = behavior.scoutMove;
    var scoutFoundSomething = behavior.scoutFoundSomething;

    function newForager(id, owner, triple, location) {
        var forager = {
            'id': id,
            'owner': owner,
            'triple': triple,
            'isAt': location,
            'passedEdge': null,
            'wasAt': null,
            'energy': foragerEnergy,
            'memory': []
        }

        m.notify('swm:fgrInit', forager);

        return forager;
    }

    var foragerMove = behavior.foragerMove;
    var foragerFoundSomething = behavior.foragerFoundSomething;
    var foragerIsExhausted = behavior.foragerIsExhausted;

    function newNurseBee(type, location) {
        var nurseBee = {
            'id': utils.guid(),
            'owner': config.ownerID,
            'type': type,
            'isAt': location,
            'passedEdge': null,
            'wasAt': null,
            'prevEdge': null
        }

        m.notify('swm:nrsInit', nurseBee);

        return nurseBee;
    }

    var nurseBeeMove = behavior.nurseBeeMove;
    var nurseBeeFoundSomething = behavior.nurseBeeFoundSomething;

    function initialize() {
        for (var i = 0; i < 3; i++) {
            initializeNurseBees();
        }
        // initializeScouts();
        // initializeForagers();

        $('#sm-n').text(numOfNurseBees);

        m.notify('swm:initialized', swarm);
    }

    function initializeScouts() {
        var scouts = [];
        var classNodes = rdfGraph.getClasses();
        var propertyNodes = rdfGraph.getProperties();

        // var numOfNodes = classNodes.length;
        // if (numOfNodes > 0) {
        // for (var i = 0; i < numOfNodes; i++) {
        // for (var i = 0; i < 3; i++) {
        for (classNode in classNodes) {
            scouts.push(newScout(utils.guid(), config.ownerID, classNode, 'class', null));
        }
        // }
        // }
        // }

        for (propertyNode in propertyNodes) {
            scouts.push(newScout(utils.guid(), config.ownerID, propertyNode, 'property', null));
        }

        return scouts;
    }

    function initializeForagers(type, node, location) {
        var foragers = [];
        location = location || node;

        var triples;

        // types of foragers
        if (type == 'class') {
            triples = [
                { 's': node, 'p': 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'o': undefined },
                { 's': undefined, 'p': 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'o': node },
                { 's': undefined, 'p': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'o': node },
                { 's': node, 'p': undefined, 'o': undefined }
            ];
        } else if (type == 'property') {
            triples = [
                { 's': node, 'p': 'http://www.w3.org/2000/01/rdf-schema#domain', 'o': undefined },
                { 's': node, 'p': 'http://www.w3.org/2000/01/rdf-schema#range', 'o': undefined },
                { 's': node, 'p': 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', 'o': undefined },
                { 's': undefined, 'p': 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', 'o': node },
                { 's': node, 'p': undefined, 'o': undefined }
            ];
        }

        triples.forEach(function(triple) {
            foragers.push(newForager(utils.guid(), config.ownerID, triple, location));
        });

        return foragers;
    }

    function initializeNurseBees() {
        var types = [
            'rdfs5',    // uuu rdfs:subPropertyOf vvv . && vvv rdfs:subPropertyOf xxx . > uuu rdfs:subPropertyOf xxx .
            'rdfs9',    // uuu rdfs:subClassOf xxx .    && vvv rdf:type uuu .           > vvv rdf:type xxx .
            'rdfs11'    // uuu rdfs:subClassOf vvv .    && vvv rdfs:subClassOf xxx .    > uuu rdfs:subClassOf xxx .
        ];

        types.forEach(function(type) {
            nurseBees.push(newNurseBee(type, rdfGraph.getRandomNode()));
            numOfNurseBees++;
        });
    }

    function forScoutsFrom(owner, fn) {
        for (var i = 0; i < numOfScouts; i++) {
            if (scouts[i].owner == owner) {
                fn(i);
                i--;
            }
        };
    }

    function forForagersFrom(owner, fn) {
        for (var i = 0; i < numOfForagers; i++) {
            if (foragers[i].owner == owner) {
                fn(i);
                i--;
            }
        };
    }

    function removeScout(index) {
        var scout = scouts.splice(index, 1);
        numOfScouts--;

        $('#sm-s').text(numOfScouts);

        m.notify('swm:sctRemoved', scout[0]);
    }

    function removeForager(index) {
        var forager = foragers.splice(index, 1);
        numOfForagers--;

        $('#sm-f').text(numOfForagers);

        m.notify('swm:fgrRemoved', forager[0]);
    }

    return {

        initialize: initialize,
        initializeScouts: initializeScouts,
        initializeForagers: initializeForagers,

        addScout: function(scout) {
            if (scout.isAt && rdfGraph.getNode(scout.isAt.id)) {
                scout.isAt = rdfGraph.getNode(scout.isAt.id);
            } else {
                scout.isAt = rdfGraph.getRandomNode();
            }

            scouts.push(scout);
            numOfScouts++;

            $('#sm-s').text(numOfScouts);
        },

        addForager: function(forager) {
            forager.isAt = rdfGraph.getNode(forager.isAt);

            foragers.push(forager);
            numOfForagers++;

            $('#sm-f').text(numOfForagers);
        },

        removeScoutsFrom: function(owner) {
            forScoutsFrom(owner, function(index) {
                removeScout(index);
            });
        },

        removeForagersFrom: function(owner) {
            forForagersFrom(owner, function(index) {
                removeForager(index);
            });
        },

        step: function() {
            start = performance.now();

            if (s < numOfScouts) {
                // console.time('scout');
                if (config.visualizationEnabled) { D3graph.unstyleNode(scouts[s].isAt.id, 'scout'); }
                if (scoutMove(scouts[s]) == 'migrated') {
                    // remove ONLY if sending over successful (= not blocked etc.), maybe on listening to 'swm:sctReceived' response message?
                    removeScout(s);
                } else {
                    if (scoutFoundSomething(scouts[s])) {
                        //
                    }
                }

                s++;
                // console.timeEnd('scout');
            } else if (f < numOfForagers) {
                // console.time('forager');
                if (config.visualizationEnabled) { D3graph.unstyleNode(foragers[f].isAt.id, 'forager'); }
                foragerMove(foragers[f]);
                if (foragerFoundSomething(foragers[f])) {

                }
                if (foragerIsExhausted(foragers[f])) {
                    removeForager(f);
                }

                f++;
                // console.timeEnd('forager');
            } else if (n < numOfNurseBees) {
                // console.time('nurse');
                if (config.visualizationEnabled) { D3graph.unstyleNode(nurseBees[n].isAt.id, 'nurse'); }
                nurseBeeMove(nurseBees[n]);
                if (nurseBeeFoundSomething(nurseBees[n])) {
                    //
                }

                n++;
                // console.timeEnd('nurse');
            } else {
                s = f = n = 0;
                this.step();
            }

            cycle++;

            end = performance.now();
            totalTime += (end - start);
            $('#sm-2').text((end - start).toFixed(5)); // totalTime / cycle

            m.notify('swm:cycleComplete', cycle);

        },

        run: function() {
            while (false) { // while not 'stop' called
                this.step();
            }
        },

        stop: function() {
            //
        }

    };

})(behavior);