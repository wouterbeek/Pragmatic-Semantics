// Helper function: `merge(source, target)`.
// Merge the dictionary contents of source into target recursively
//  and return the result.
function merge(source, target){
  // If target is not given, assume empty dict.
  if(typeof target != "object") target = {}
  // If source is not given, assume target to be the result.
  if(typeof source != "object") return target
  // Init merged result
  var merged = {}
  for(key in target){
    if(key in source){
      if(typeof target[key] == "object" && typeof source[key] == "object"){
        merged[key] = merge(source[key], target[key]);
      } else {
        merged[key] = source[key];
      }
    } else {
      merged[key] = target[key];
    }
  }
  return merged
}

// Helper function: `evaldict(dict)'.
// Iterate recursively through dict, if a value is a function
//   then replace it with the return value of that function
function evaldict(dict){
  for(key in dict){
    if(typeof dict[key] == "object"){
      dict[key] = evaldict(dict[key]);
    }else if(typeof dict[key] == "function"){
      dict[key] = dict[key]();
    }
  }
  return dict;
}

// API
API = {
  // Note that the 'options' in API do **not** denote an HTTP method.
  // Default options, can be overriden by using `API.init(opt)`.
  opt: {
    // All communication will be in JSON.
    "contentType": "application/json",
    "dataType": "json",
    // The URI locations for manipulating circles and lines.
    "endpoints": {
      "circle": "/webqr/circle",
      "line": "/webqr/line",
    },
    "headers": {}
  },
  // Initialize API with options override.
  init: function(opt){
    API.opt = merge(opt, API.opt)
  },
  _request: function(method, uri, headers, data, callback){
    // Expand URI to endpoint if applicable.
    if( uri in API.opt.endpoints) uri = API.opt.endpoints[uri]
    // Merge given headers with default headers.
    headers = merge(headers, API.opt.headers)
    // Create AJAX options.
    options = {
      "contentType": API.opt.contentType,
      "dataType": API.opt.dataType,
      "type": method,
      "url": uri
    }
    if(typeof callback == "function") options["success"] = callback
    if(typeof headers == "object") options["headers"] = headers
    // Convert data to JSON string if desired
    if(data){
      if( API.opt.contentType.indexOf("json") > -1 ){
        options["data"] = JSON.stringify(data);
      } else {
        options["data"] = data
      }
    }
    // Execute AJAX call.
    $.ajax(uri, options);
  },
  get: function(uri, callback){
    var headers = {};
    API._request("GET", uri, headers, null, callback);
  },
  post: function(uri, data, callback){
    var headers = {};
    API._request("POST", uri, headers, data, callback);
  },
  put: function(uri, data, callback){
    var headers = {};
    API._request("PUT", uri, headers, data, callback);
  },
  delete: function(uri, callback){
    var headers = {};
    API._request("DELETE", uri, headers, null, callback);
  },
  create_circle: function(local_uri, global_uri, callback){
    var data = { "local_uri": local_uri, "global_uri": global_uri }
    API.post("circle", data, callback);
  },
  update_circle: function(local_uri, global_uri, callback){
    var data = { "global_uri": global_uri }
    API.put(local_uri, data, callback);
  },
  request_link_types: function(from, to, callback){
    var data = { "from_local_uri": from, "to_local_uri": to };
    // Request possible link types from server for this combination.
    API.post("line", data, callback);
  },
  create_line: function(from, to, type, callback){
    var data = { "from_local_uri": from, "relation": type, "to_local_uri": to };
    // Request URI from server for a new line.
    API.post("line", data, callback);
  },
  debug_uri_describe: function(uri){
    API.get(uri, function(xhr){
      alert(xhr.global_uri);
    });
  },
  debug_uri_options: function(uri){
    API._request("OPTIONS", uri, null, null, function(xhr){
      alert(xhr.getResponseHeader("Allow"));
    });
  }
};

// Application
App = {
  // Default options, can be (partially) overriden by `App.init(opt)`
  opt: {
    "api": {
      // All communication will be in JSON.
      "contentType": "application/json",
      "dataType": "json",
      // The URI locations for manipulating circles and lines.
      "endpoints": {
        "circle": "/webqr/circle",
        "line": "/webqr/line",
      },
      "headers": {}
    },
    "canvas": {
      "width": "auto",
      "height": "auto",
      "container": "content"
    },
    "core": {
      "autostart": true
    },
    "data": {
      "nodes": [],
      "links": []
    },
    "graphics":{
      "colors": null,
      "toolbelt_offset": 15,
      "label_offset": 20
    }
  },
  init: function(opt){
    // Ensure existance of d3
    if(!d3) throw "Cannot find visualisation library d3js."
    // Ensure existance of jQuery
    if(!$) throw "Cannot find jQuery library."
    $(function(){
      // Merge options
      App.opt = merge(opt, App.opt);
      // Evaluate options
      App.opt = evaldict(App.opt);
      // Init submodules
      App.data.init();
      API.init(App.opt.api);
      App.canvas.init();
      App.graphics.init();
      App.behavior.init();
      App.layout.init();
      App.dialog.init();
      if(App.opt.core.autostart){
        App.start()
      }
    });
  },
  start: function(){
    App.behavior.register();
    App.layout.update();
  },
  data: {
    nodes: [],
    nextNodeId: function(){ return App.data.nodes.length; },
    links: [],
    init: function(){
      if(App.opt.data.nodes && App.opt.data.nodes.length){
        App.data.nodes = App.opt.data.nodes
        for(var i = 0; i < App.data.nodes.length; i++){
          App.data.assignLocalURI(App.data.nodes[i])
        }
      }
      if(App.opt.data.links && App.opt.data.links.length){
        App.data.links = App.opt.data.links
      }
    },
    assignLocalURI: function(node){
      // Construct the local URI from the endpoint
      endpoint = App.opt.api.endpoints.circle
      if(endpoint[endpoint.length-1] == "/"){
        local_uri = endpoint+node.id;
      }else{
        local_uri = endpoint+"/"+node.id;
      }
      node.local_uri = local_uri
      return local_uri
    }
  },
  // handles to link and node element groups
  canvas: {
    svg: null,
    path: null,
    circle: null,
    init: function(){
      // Set dimensions to window dimensions if both are "auto"
      if(App.opt.canvas.width == "auto" && App.opt.canvas.height == "auto"){
        App.opt.canvas.width = window.innerWidth;
        App.opt.canvas.height = window.innerHeight;
        $(window).resize(function(){
          App.opt.canvas.width = window.innerWidth;
          App.opt.canvas.height = window.innerHeight;
          App.canvas.svg
            .attr('width', App.opt.canvas.width)
            .attr('height', App.opt.canvas.height);
          App.layout.force
            .size([App.opt.canvas.width, App.opt.canvas.height])
            .start();
        });
      }
      // Set width to window with if "auto"
      else if(App.opt.canvas.width == "auto"){
        App.opt.canvas.width = window.innerWidth;
        $(window).resize(function(){
          App.opt.canvas.width = window.innerWidth;
          App.canvas.svg
            .attr('width', App.opt.canvas.width);
          App.layout.force.start();
          App.layout.force
            .size([App.opt.canvas.width, App.opt.canvas.height])
            .start();
        });
      }
      // Set height to window height if "auto"
      else if(App.opt.canvas.height == "auto"){
        App.opt.canvas.height = window.innerHeight;
        $(window).bind('resize', function(){
          App.opt.canvas.height = window.innerHeight;
          App.canvas.svg
            .attr('height', App.opt.canvas.height);
          App.layout.force.start();
          App.layout.force
            .size([App.opt.canvas.width, App.opt.canvas.height])
            .start();
        });
      }
      App.canvas.svg = d3.select("#"+App.opt.canvas.container)
        .append('svg')
        .attr('width', App.opt.canvas.width)
        .attr('height', App.opt.canvas.height);
      App.canvas.path = App.canvas.svg.append('svg:g').selectAll('path');
      App.canvas.circle = App.canvas.svg.append('svg:g').selectAll('g');
    }
  },
  graphics: {
    start_arrow: null,
    end_arrow: null,
    drag_line: null,
    colors: null,
    init: function(){
      // Init color space
      if(!App.opt.graphics.colors) App.graphics.colors = d3.scale.category10();
      else App.graphics.colors = App.opt.graphics.colors;

      // define arrow markers for graph links
      App.graphics.end_arrow = App.canvas.svg.append('svg:defs')
        .append('svg:marker')
        .attr('id', 'end-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 6)
        .attr('markerWidth', 3)
        .attr('markerHeight', 3)
        .attr('orient', 'auto')
        .append('svg:path')
        .attr('d', 'M0,-5L10,0L0,5')
        .attr('fill', '#000');

      App.graphics.start_arrow = App.canvas.svg.append('svg:defs')
        .append('svg:marker')
        .attr('id', 'start-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 4)
        .attr('markerWidth', 3)
        .attr('markerHeight', 3)
        .attr('orient', 'auto')
        .append('svg:path')
        .attr('d', 'M10,-5L0,0L10,5')
        .attr('fill', '#000');

      // line displayed when dragging new nodes
      App.graphics.drag_line = App.canvas.svg.append('svg:path')
        .attr('class', 'link dragline hidden')
        .attr('d', 'M0,0L0,0');
    }
  },
  state: {
    selected_node: null,
    selected_link: null,
    mousedown_link: null,
    mousedown_node: null,
    mouseup_node: null,
    lastKeyDown: -1,
    resetMouseVars: function(){
      App.state.mousedown_node = null;
      App.state.mouseup_node = null;
      App.state.mousedown_link = null;
    }
  },
  dialog: {
    dom_modal: null,
    dom_modal_dialog: null,
    dom_modal_header: null,
    dom_modal_btn_exit: null,
    dom_modal_title: null,
    dom_modal_body: null,
    dom_modal_footer: null,
    init: function(){
      App.dialog.dom_modal = $('<div class="modal fade"></div>');

      App.dialog.dom_modal.append('<div class="modal-dialog">'+
          '<div class="modal-content"><div class="modal-header">'+
          '<button type="button" class="close" data-dismiss="modal"'+
          ' aria-hidden="true">&times;</button><h4 class="modal-title"></h4>'+
          '</div><div class="modal-body"></div><div class=\"modal-footer\">'+
          '</div></div></div>')
    },
    modal: function(title){
      App.dialog.dom_modal.find('.modal-title').html(title);
      App.dialog.dom_modal.modal();
    },
    modal_button_select: function(title, options, callback){
      var option_group = $('<div class="list-group"></div>');
      var btn;
      for(var key in options){
        option = $('<a class="list-group-item" href="#"></a>"');
        option.html(options[key])
        option.click({ key: key, cb: callback }, function(e){
            App.dialog.dom_modal.modal('hide');
            e.data.cb(e.data.key);
        });
        option_group.append(option)
      }
      App.dialog.dom_modal.find('.modal-body').html(option_group);
      App.dialog.modal(title);
    },
  },
  behavior: {
    drag_resize: null,
    drag_connect: null,
    init: function(){
      App.behavior.drag_resize = d3.behavior.drag()
        .origin(function(d){ return d })
        .on("drag", App.behavior.onToolbeltDrag)
        .on('dragend', App.state.resetMouseVars);
      App.behavior.drag_connect = d3.behavior.drag()
        .origin(function(d){ return d })
        .on("dragend", App.behavior.onConnectDragEnd)
        .on("drag", App.behavior.onConnectDrag)
        .on("dragstart", App.behavior.onConnectDragStart);
    },
    register: function(){
      App.canvas.svg.on('mousedown', App.behavior.mousedown)
        .on('mouseup', App.behavior.mouseup);
      d3.select(window)
        .on('keydown', App.behavior.keydown)
        .on('keyup', App.behavior.keyup);
    },
    spliceLinksForNode: function(node){
      var toSplice = App.data.links.filter(function(l) {
        return (l.source === node || l.target === node);
      });
      toSplice.map(function(l) {
        App.data.links.splice(App.data.links.indexOf(l), 1);
      });
    },
    createLink: function(link){
      App.data.links.push(link);
      API.request_link_types(link.source.local_uri, link.target.local_uri,
        function(result){
          relations = result['relations']
          // Convert relations to options dictionary
          options = {}
          for(var i = 0; i < relations.length; i++){
            options[relations[i].resource] = relations[i].label;
          }
          App.dialog.modal_button_select(
              "Select the relationship",
              options,
              function(link_type){
                API.create_line(
                    link.source.local_uri,
                    link.target.local_uri,
                    link_type,
                    function(){}
                );
              }
          )
        }
      )
    },
    createNode: function(node){
      // Request Remote URI from user
      remote_uri = prompt("Remote URI","http://dbpedia.org/resource/");
      // If a Remote URI has been provided (or rather if OK has been clicked)
      if( remote_uri ){
        local_uri = App.data.assignLocalURI(node)
        // Push to local set of nodes
        App.data.nodes.push(node);
        // Send API request to create the node remotely
        API.create_circle(local_uri, remote_uri, function(result){
          // Apply the retrieved label to the local node
          node.label = result.label;
          // Update the layout
          App.layout.update()
        })
      }
    },
    onConnectDragStart: function(d){
      if(d3.event.sourceEvent.ctrlKey) return;

      App.state.mousedown_node = d;

      // reposition drag line
      App.graphics.drag_line
        .style('marker-end', 'url(#end-arrow)')
        .classed('hidden', false)
        .attr('d', 'M'
          + App.state.mousedown_node.x + ','
          + App.state.mousedown_node.y + 'L'
          + App.state.mousedown_node.x + ','
          + App.state.mousedown_node.y);
      App.layout.update();
    },
    onConnectDrag: function(d){
      if(d3.event.sourceEvent.ctrlKey) return;

      m = d3.mouse(App.canvas.svg.node())
      // update drag line
      App.graphics.drag_line.attr('d', 'M'
          + App.state.mousedown_node.x + ','
          + App.state.mousedown_node.y + 'L'
          + m[0] + ',' + m[1]);
      App.layout.update();
    },
    onConnectDragEnd: function(drag_node){
      if(d3.event.sourceEvent.ctrlKey) return;

      var drop_element = d3.select(d3.event.sourceEvent.target);
      if( !drop_element.classed("node") ) return ;

      var drop_node = drop_element.datum();

      // needed by FF
      App.graphics.drag_line
        .classed('hidden', true)
        .style('marker-end', '');

      // check for drag-to-self
      if(drop_node === drag_node){
        App.state.resetMouseVars();
        return;
      }

      var source, target, direction;
      if(drag_node.id < drop_node.id) {
        source = drag_node;
        target = drop_node;
        direction = 'right';
      } else {
        target = drag_node;
        source = drop_node;
        direction = 'left';
      }

      // unenlarge target node
      drop_element.attr('transform', '');

      var link;
      link = App.data.links.filter(function(l) {
        return (l.source === source && l.target === target);
      })[0];

      if(link) {
        link[direction] = true;
        App.behavior.createLink(link);
      } else {
        link = {source: source, target: target, left: false, right: false};
        link[direction] = true;
        App.behavior.createLink(link);
      }

      // select new link
      App.state.selected_link = link;
      App.state.selected_node = null;

      App.layout.update();
    },
    onToolbeltDrag: function(d){
      // Set parameters.
      t_offset = App.opt.graphics.toolbelt_offset
      l_offset = App.opt.graphics.label_offset
      step_radius = 15;
      min_radius = 10+t_offset;
      max_radius = 200+t_offset;
      // Generate list of radius values.
      radius_list = d3.range(min_radius, max_radius, step_radius);
      // Get mouse position.
      m = d3.mouse(this)
      // Calculate absolute distance.
      diff = Math.sqrt(Math.pow(m[0],2)+Math.pow(m[1],2))-t_offset;
      // Calculate radius index, ensure boundaries of array.
      r_i = Math.max(
          Math.min(
            Math.floor(diff / step_radius),
            radius_list.length-1
          ),
          0
      );
      // Select radius from radius list.
      r = radius_list[r_i];
      // Update datum.
      d.radius = r-t_offset;
      // Update circle.
      d3.select(this.parentNode).select(".node").attr("r",r-t_offset);
      // Update toolbelt (this)
      if(r_i < radius_list.length-1 && r_i > 0){
        d3.select(this).attr("r",diff+t_offset);
      } else {
        d3.select(this).attr("r",r+t_offset);
      }
      // Update text
      d3.select(this.parentNode).select("text").attr("y",r+t_offset+l_offset);
      // Update layout
      App.layout.update();
    },
    // Mousedown on SVG
    mousedown: function(){
      // because :active only works in WebKit?
      App.canvas.svg.classed('active', true);

      if(d3.event.ctrlKey || App.state.mousedown_node || App.state.mousedown_link) return;
      // Insert a new node at the mouse pointer position.
      var point = d3.mouse(document.getElementById(App.opt.canvas.container))
      var node = {
        "id": App.data.nextNodeId()+1,
        "label": "Loading...",
        "radius": 10,
        "reflexive": false
      };
      node.x = point[0];
      node.y = point[1];
      App.behavior.createNode(node)

      App.layout.update();
    },
    mouseup: function() {
      if(App.state.mousedown_node) {
        // hide drag line
        App.graphics.drag_line
          .classed('hidden', true)
          .style('marker-end', '');
      }

      // because :active only works in WebKit?
      App.canvas.svg.classed('active', false);

      // clear mouse event vars
      App.state.resetMouseVars();
    },
    keydown: function() {
      if(App.state.lastKeyDown !== -1) return;
      App.state.lastKeyDown = d3.event.keyCode;

      // ctrl
      if(d3.event.keyCode === 17) {
        App.canvas.circle.call(App.layout.force.drag);
        App.canvas.svg.classed('ctrl', true);
      }

      if(!App.state.selected_node && !App.state.selected_link) return;
      switch(d3.event.keyCode) {
        case 8: // backspace
        case 46: // delete
          if(App.state.selected_node) {
            App.data.nodes.splice(App.data.nodes.indexOf(App.state.selected_node), 1);
            App.behavior.spliceLinksForNode(App.state.selected_node);
          } else if(App.state.selected_link) {
            App.data.links.splice(App.data.links.indexOf(App.state.selected_link), 1);
          }
          App.state.selected_link = null;
          App.state.selected_node = null;
          App.layout.update();
          break;
        case 66: // B
          if(App.state.selected_link) {
            // set link direction to both left and right
            App.state.selected_link.left = true;
            App.state.selected_link.right = true;
          }
          App.layout.update();
          break;
        case 76: // L
          if(App.state.selected_link) {
            // set link direction to left only
            App.state.selected_link.left = true;
            App.state.selected_link.right = false;
          }
          App.layout.update();
          break;
        case 82: // R
          if(App.state.selected_node) {
            // toggle node reflexivity
            App.state.selected_node.reflexive = !App.state.selected_node.reflexive;
          } else if(App.state.selected_link) {
            // set link direction to right only
            App.state.selected_link.left = false;
            App.state.selected_link.right = true;
          }
          App.layout.update();
          break;
      }
    },
    keyup: function() {
      App.state.lastKeyDown = -1;

      // ctrl
      if(d3.event.keyCode === 17) {
        App.canvas.circle
          .on('mousedown.drag', null)
          .on('touchstart.drag', null);
        App.canvas.svg.classed('ctrl', false);
      }
    }
  },
  layout: {
    force: null,
    init: function(){
      // init D3 force layout
      App.layout.force = d3.layout.force()
        .nodes(App.data.nodes)
        .links(App.data.links)
        .size([App.opt.canvas.width, App.opt.canvas.height])
        .linkDistance(function(l){ return 130+l.source.radius+l.target.radius;})
        .charge(-500)
        .on('tick', App.layout.tick)
    },
    // update force layout (called automatically each iteration)
    tick: function(){
      // draw directed edges with proper padding from node centers
      App.canvas.path.attr('d', function(d) {
        var deltaX = d.target.x - d.source.x,
          deltaY = d.target.y - d.source.y,
          dist = Math.sqrt(deltaX * deltaX + deltaY * deltaY),
          normX = deltaX / dist,
          normY = deltaY / dist,
          sourcePadding = d.left ? d.source.radius + 5 : d.source.radius,
          targetPadding = d.right ? d.target.radius + 5 : d.target.radius,
          sourceX = d.source.x + (sourcePadding * normX),
          sourceY = d.source.y + (sourcePadding * normY),
          targetX = d.target.x - (targetPadding * normX),
          targetY = d.target.y - (targetPadding * normY);
        return 'M' + sourceX + ',' + sourceY + 'L' + targetX + ',' + targetY;
      });

      App.canvas.circle.attr('transform', function(d) {
        return 'translate(' + d.x + ',' + d.y + ')';
      });
    },
    // update graph (called when needed)
    update: function() {
      var colors = App.graphics.colors
      // path (link) group
      App.canvas.path = App.canvas.path.data(App.data.links);

      // update existing links
      App.canvas.path.classed('selected', function(d) { return d === App.state.selected_link; })
        .style('marker-start', function(d) { return d.left ? 'url(#start-arrow)' : ''; })
        .style('marker-end', function(d) { return d.right ? 'url(#end-arrow)' : ''; });

      // add new links
      App.canvas.path.enter().append('svg:path')
        .attr('class', 'link')
        .classed('selected', function(d) { return d === App.state.selected_link; })
        .style('marker-start', function(d) { return d.left ? 'url(#start-arrow)' : ''; })
        .style('marker-end', function(d) { return d.right ? 'url(#end-arrow)' : ''; })
        .on('mousedown', function(d) {
          if(d3.event.ctrlKey) return;

          // select link
          App.state.mousedown_link = d;
          if(App.state.mousedown_link === App.state.selected_link) App.state.selected_link = null;
          else App.state.selected_link = App.state.mousedown_link;
          App.state.selected_node = null;
          App.layout.update();
      });

      // remove old links
      App.canvas.path.exit().remove();

      // circle (node) group
      // NB: the function arg is crucial here! nodes are known by id, not by index!
      App.canvas.circle = App.canvas.circle.data(App.data.nodes, function(d) { return d.id; });

      // update existing nodes (reflexive & selected visual states)
      App.canvas.circle.selectAll('circle .node')
        .style('fill', function(d) {
          return (
            (d === App.state.selected_node) ?
            d3.rgb(colors(d.id)).brighter().toString() :
            colors(d.id)
          );
        })
        .classed('reflexive', function(d) { return d.reflexive; });

      // update existing node labels
      App.canvas.circle.selectAll("text")
        .text(function(d) { return d.label; });

      // add new nodes
      var g = App.canvas.circle.enter().append('svg:g');

      g.append('svg:circle')
        .attr('class', 'toolbelt')
        .attr('fill', function(d){ return "none"; })
        .attr('r', function(d){ return d.radius+20; })
        .style('stroke', function(d) { return d3.rgb(colors(d.id)).darker().toString(); })
        .on('mousedown', function(d) {
          // With the CTRL key pressed, node selection by mouse is disabled.
          if(d3.event.ctrlKey) return;

          // Select a node by pressing the left mouse button.
          App.state.mousedown_node = d;
        })
        .call(App.behavior.drag_resize);

      g.append('svg:circle')
        .attr('class', 'node')
        .attr('r', function(d){ return d.radius; })
        .style('fill', function(d) { return (d === App.state.selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id); })
        .style('stroke', function(d) { return d3.rgb(colors(d.id)).darker().toString(); })
        .classed('reflexive', function(d) { return d.reflexive; })
        .on('mouseover', function(d) {
          if(!App.state.mousedown_node || d === App.state.mousedown_node) return;
          // enlarge target node
          d3.select(this).attr('transform', 'scale(1.1)');
        })
        .on('mouseout', function(d) {
          if(!App.state.mousedown_node || d === App.state.mousedown_node) return;
          // unenlarge target node
          d3.select(this).attr('transform', '');
        })
        .on('mousedown', function(d) {
          // With the CTRL key pressed, node selection by mouse is disabled.
          if(d3.event.ctrlKey) return;

          // Select a node by pressing the left mouse button.
          App.state.mousedown_node = d;
          if(App.state.mousedown_node === App.state.selected_node) App.state.selected_node = null;
          else App.state.selected_node = App.state.mousedown_node;
          App.state.selected_link = null;

        })
        .call(App.behavior.drag_connect);

      // show node IDs
      g.append('svg:text')
        .attr('x', 0)
        .attr('y', function(d){
          return d.radius +
            App.opt.graphics.toolbelt_offset +
            App.opt.graphics.label_offset;
        })
        .attr('class', 'id')
        .text(function(d) { return d.label; });

      // remove old nodes
      App.canvas.circle.exit().remove();

      // set the graph in motion
      App.layout.force.start();
    }
  }
};
