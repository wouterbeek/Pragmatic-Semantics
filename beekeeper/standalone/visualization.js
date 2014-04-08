var D3graph = (function () {

	var width, height;
	var svg, force, loading;
	var nodes, links;
	var path, circle;

	function setup(id) {
		width = window.innerWidth - 40; //1280 or 600
		height = window.innerHeight - 40; //550
		// TODO: update when window-size changes

		svg = d3.select('#' + id)
			.append('svg')
			.attr('width', width)
			.attr('height', height);

		nodes = [];
		links = [];
		lastNodeId = 0;

		force = d3.layout.force()
			.nodes(nodes)
			.links(links)
			.size([width, height])
			.linkDistance(100) // .distance(100)
			.charge(-500);

		// loading = svg.append('text')
		// 	.attr('x', width / 2)
		// 	.attr('y', height - 20)
		// 	.attr('dy', '.35em')
		// 	.attr('text-anchor', 'middle')
		// 	.text('');

		svg.append('svg:defs')
			.append('svg:marker')
				.attr('id', 'arrow')
				.attr('viewBox', '0 -5 10 10')
				.attr('refX', 18)
				.attr('markerWidth', 8)
				.attr('markerHeight', 8)
				.attr('orient', 'auto')
			.append('svg:path')
				.attr('class', 'marker')
				.attr('d', 'M0,-5L10,0L0,5');

		path = svg.append('svg:g').selectAll('g.pgroup');
		circle = svg.append('svg:g').selectAll('g.cgroup');

		force.on('tick', function() {
			path.selectAll('path')
				.attr('d', function(d) {
					return 'M' + d.source.x + ',' + d.source.y + 'L' + d.target.x + ',' + d.target.y;
				});
				// .attr('x1', function(d) { return d.source.x; })
				// .attr('y1', function(d) { return d.source.y; })
				// .attr('x2', function(d) { return d.target.x; })
				// .attr('y2', function(d) { return d.target.y; });

			path.selectAll('text').attr('transform', function(d) {
				return "translate(" + (d.source.x + d.target.x) / 2 + "," + (d.source.y + d.target.y) / 2 + ")";
			});

			circle.attr('transform', function(d) {
				return 'translate(' + d.x + ',' + d.y + ')';
			});
		});

		m.notify('vis:loaded', svg);
	}

	function restart() {
		path = path.data(links);

		var pg = path.enter().append('svg:g').attr('class', 'pgroup');

		pg.append('svg:path')
			.attr('class', 'link')
			.classed('locallyInferred', function(d) { return d.inferenceType == 'local'; })
			.classed('remotelyInferred', function(d) { return d.inferenceType == 'remote'; })
			.style('marker-end', (config.showMarkers) ? 'url(#arrow)' : '');

		if (config.showLabels) {
			pg.append('svg:text')
				.attr('class', 'id label')
				.attr('x', 0)
				.attr('y', 0)
				.text(function(d) { return utils.getHash(d.id); })
				.on('mouseover', function(d) { d3.select(this).text(d.id); })
				.on('mouseout', function(d) { d3.select(this).text(utils.getHash(d.id)); });
		}

		path.exit().remove();

		circle = circle.data(nodes, function(d) { return d.id; });

		var cg = circle.enter().append('svg:g').attr('class', 'cgroup');
		if (!config.staticGraph) cg.call(force.drag);

		cg.append('svg:circle')
			.attr('class', 'node')
			.attr('id', function(d) { return utils.validId(d.id); })
			.attr('r', function(d) { return (d.RDFlinkTarget) ? 9 : 6; });

		if (config.showLabels) {
			cg.append('svg:text')
				.attr('class', 'id label')
				.attr('x', 0)
				.attr('y', -18)
				.text(function(d) { return utils.getHash(d.id); })
				.on('mouseover', function(d) { d3.select(this).text(d.id); })
				.on('mouseout', function(d) { d3.select(this).text(utils.getHash(d.id)); });
		}

		cg.append('svg:text')
			.attr('class', function(d) { return (d.RDFlinkTarget) ? 'id label RDFlinkTarget' : 'id label'})
			.attr('x', 0)
			.attr('y', 20)
			.text(function(d) { return (d.RDFlinkTarget && config.hostedBy[utils.getBase(d.id)]) ? config.hostedBy[utils.getBase(d.id)].join(', ') : ''; });

		circle.exit().remove();

		force.start();

		if (config.staticGraph) {
			// loading.text('recalculating graph...');
			var i = config.staticGraphIterations;
			while (i--) force.tick();
			force.stop();
			// loading.text('');
		}
		// TODO: implement asynchronously

		m.notify('vis:updated', svg);
	}

	return {

		restart: restart,
		
		isSetup: function() {
			return (typeof path != 'undefined' && typeof circle != 'undefined');
		},

		newGraph: function(id) { // initialize: function() {
			$('#' + id).html('');
			setup(id);
		},

		newNode: function(node) {
			node.x = Math.floor(Math.random() * (500 - 460 + 1)) + 460;
			node.y = Math.floor(Math.random() * (270 - 220 + 1)) + 220;
			nodes.push(node);

			if (!config.staticGraph) restart();

			m.notify('vis:nodeNew', node);
		},

		newLink: function(link, inferenceType) {
			link.inferenceType = inferenceType;
			links.push(link);

			if (inferenceType != 'none' || !config.staticGraph) restart();

			m.notify('vis:linkNew', link);
		},

		styleNode: function(node, className) {
			var node = d3.select('#' + utils.validId(node)).classed(className, true);

			m.notify('vis:nodeSel', node);
		},

		unstyleNode: function(node, className) {
			var node = d3.select('#' + utils.validId(node)).classed(className, false);

			m.notify('vis:nodeUnsel', node);
		}

	};

})(); // import D3

// var swarmVis = (function () {

// 	var swarm;

// 	function init(id) {
// 		swarm = $('#' + id + ' svg').append('svg:g');
// 	}

// 	return {

// 		initialize: init,

// 		newScout: function(scout) {
// 			// swarm.append('svg:circle')
// 			// 	.attr('class', 'bee')
// 			// 	.attr('id', scout.id)
// 			// 	.attr('r', 2)
// 			// 	.attr('x', getNodeLocation(scout.location))
// 			// 	.attr('y', getNodeLocation(scout.location));
// 		},

// 		newForager: function(forager) {

// 		},

// 		newNurseBee: function(nurseBee) {

// 		},

// 		moveScout: function(scout) {
// 			// swarm.select('id': scout.id)
// 			// 	.attr('x', getNodeLocation(scout.location))
// 			// 	.attr('y', getNodeLocation(scout.location));
// 		},

// 		moveForager: function(forager) {

// 		},

// 		moveNurseBee: function(nurseBee) {

// 		},

// 		removeScout: function(scout) {

// 		},

// 		removeForager: function(forager) {

// 		},

// 		removeNurseBee: function(nurseBee) {

// 		},

// 	};

// })(); // import D3