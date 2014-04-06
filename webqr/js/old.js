function EventListener(target){
  var listeners = {};

  // Register callback to be executed when an event, identified by its
  // handle, occurs.
  this.listen = function(handle, callback){
    if(handle in listeners){
      listeners[handle].push(callback);
    }else{
      listeners[handle] = [callback];
    }
  }

  // Notify all listeners that were interested in an event, identified by its
  // handle, that the event occured.
  this.tell = function(handle,context){
    context.target = target;
    if(handle in listeners){
      for(var i = 0; i < listeners[handle].length; i++){
        listeners[handle][i](context);
      }
    }
  }
}

API = {
  graph: "boyle273",
  _request: function(method, uri, headers, data, callback){
    var xhr = new XMLHttpRequest();
    xhr.open(method,uri, true)
    for(header in headers){
      xhr.setRequestHeader(header,headers[header]);
    }
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4 && xhr.status == 200) {
      if(callback) callback(xhr);
      }
    }
    // Forgetting to 'stringify' the JSON data will throw
    // an exception in Firefox (but not in Chrome) of the following kind:
    // ~~~
    // JavaScript component does not have a method named:
    // "available"' when calling method:
    // ~~~
    // We assume that `data` is 'stringified' before being passed
    // to this function.
    xhr.send(data);
  },
  get: function(uri, callback){
    headers = {"Content-Type":"application/json"};
    API._request("GET",uri,headers,null,callback);
  },
  post: function(uri, data, callback){
    headers = {"Content-Type":"application/json"};
    data = (data?JSON.stringify(data):null);
    API._request("POST",uri,headers,data,callback);
  },
  put: function(uri, data, callback){
    headers = {"Content-Type":"application/json"};
    data = (data?JSON.stringify(data):null);
    API._request("PUT",uri,headers,data,callback);
  },
  create_circle: function(paper, bg, x, y, callback){
    var uri = "/circle";
    var data = { graph: API.graph };
    // Request uri from server for a new circle
    API.post(uri, data, function(xhr){
      name = JSON.parse(xhr.responseText).name;
      // Create new circle
      c = new CircleObj(name, paper, bg, x, y);
      if(callback) callback(c, xhr);
    });
  },
  update_circle: function(uri, term, callback){
    var data = { graph: API.graph, label: term, term: term };
    API.put(uri, data, function(xhr){
      if(callback) callback(xhr);
    });
  },
  create_line: function(paper, bg, from, to, callback){
    var uri = "/line";
    var data = { graph: API.graph, from: from.uri(), to: to.uri() };
    // Request uri from server for a new line
    API.post(uri, data, function(xhr){
      // Create new line
      conn = new ConnectionObj(from, to, paper, bg);
      if(callback) callback(conn, xhr);
    });
  },
  debug_uri_describe: function(uri){
    API.get(uri, function(xhr){
      alert(xhr.responseText);
    });
  },
  debug_uri_options: function(uri){
    API._request("OPTIONS", uri, null, null, function(xhr){
      alert(xhr.getResponseHeader("Allow"));
    });
  }
}

function CircleObj(uri, paper, bg, x, y){
  // Store reference to "this" pointer to use in other scopes.
  var _obj = this;
  this.uri = function(){ return uri;};

  // Create event listener, used to dispatch events to registered listeners
  var listener = new EventListener(this);
  this.listener = function(){ return listener; };

  // Placeholder for keeping track of connections
  var connections = {};
  this.connections = function(){ return connections; };

  // Create object in Raphael
  var circle = paper.circle(x, y, 50);
  var label = paper.text(x, y, prompt("Label"));

  this.circle = function(){ return circle; };
  this.label = function(){ return label; };

  // Object is selected or not
  var selected = false;

  // Change to selected state
  this.select = function(){
    if(bg.data("selection") == undefined){
      circle.node.setAttribute("class","selected");
      bg.data("selection",this);
      selected = true;
    }else{
      to = bg.data("selection");
      // Only create new connection, if there is not already a connection
      // between these two circles.
      if(!( to.uri() in connections )){
        // Create connection
        API.create_line(paper,bg,this,to,function(conn){
          connections[to.uri()] = conn;
        });
      }
      // Remove selection
      bg.removeData("selection");
      to.unselect();
      this.unselect();
    }
  };

  // Change to unselected state
  this.unselect = function(){
    circle.node.setAttribute("class","unselected");
    selected = false;
  };

  // Toggle selection state
  this.toggle_selection = function(e){
    if(!selected){
      _obj.select();
    }else{
      _obj.unselect();
    }
  };

  // Edit properties: label
  this.edit = function(e){
    label.attr("text",prompt("New label",label.attr("text")));
    _obj.remote_update();
  }

  // Update remote RDF storage
  this.remote_update = function(){
    API.update_circle(uri, label.attr("text"));
  }

  // Event handler for drag events. Ensure all parts of the circle to move
  // along with the center.
  //   dx - distance in x direction compared to initial drag position
  //   dy - distance in y direction compared to initial drag position
  //   mx - mouse x coordinate
  //   my - mouse y coordinate
  //   e  - event object
  this.ondrag = function(dx, dy, mx, my, e){
    circle.attr({
      cx:_obj._ox+dx,
      cy:_obj._oy+dy
    });
    label.attr({
      x:_obj._ox+dx,
      y:_obj._oy+dy
    });
    listener.tell("move",{dx:dx,dy:dy});
  };

  // Event handler at start of drag event. Store orientation location.
  this.ondragstart = function(mx,my,e){
    if(this.type == "circle"){
      _obj._ox = this.attr("cx");
      _obj._oy = this.attr("cy");
    }else{
      _obj._ox = this.attr("x");
      _obj._oy = this.attr("y");
    }
  };

  // Event handler at end of drag event. Cleanup.
  this.ondragend = function(e){
    delete _obj._ox;
    delete _obj._oy;
  };

  // Connect event handlers to objects
  circle.drag(this.ondrag, this.ondragstart, this.ondragend);
  label.drag(this.ondrag, this.ondragstart, this.ondragend);
  circle.dblclick(this.edit);
  label.dblclick(this.edit);
  circle.click(this.toggle_selection);
  label.click(this.toggle_selection);

  // Update remote RDF storage
  this.remote_update();

  // Start with an unselected state
  this.unselect();
}

function ConnectionObj(from, to, paper, bg){
  // Create connection
  var connection = paper.path();
  this.connection = function(){ return connection; };

  // Update the begin and end point of the path
  this.update = function(){
    // Fetch coordinates
    fromx = from.circle().attr("cx");
    fromy = from.circle().attr("cy");
    tox = to.circle().attr("cx");
    toy = to.circle().attr("cy");

    // Update path
    connection.attr("path","M"+fromx+","+fromy+"L"+tox+","+toy);
  }

  // Register at listeners to get location updates.
  from.listener().listen("move",this.update);
  to.listener().listen("move",this.update);

  // Set to the right initial positions
  this.update();
}

