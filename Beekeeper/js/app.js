// document.addEventListener( "DOMContentLoaded", function() {
//   document.removeEventListener( "DOMContentLoaded", arguments.callee, false );
//   jQuery.ready();
// }, false );

$(function () {
  if (window.location.hash) {
    config.ownerID = window.location.hash.substring(1);
    easy_setup.start();
  } else {
    var regexp = /^[a-zA-Z0-9-_]+$/; // Only contains: a-z A-Z 0-9 - _
    do {
      config.ownerID = prompt('Choose a name. \n\nNOTE: The name needs to be alphanumeric, dashes and underscores are allowed.');
    } while (config.ownerID.search(regexp) == -1);
    window.location.hash = config.ownerID;
    easy_setup.start();
  }

  // loading session
  if (localStorage[config.ownerID]) {
    session = JSON.parse(localStorage[config.ownerID]);
  }
  if (typeof session != 'undefined' && session.config) {
    config = session.config;
  }

  p2p.setup(config.ownerID);
  $('#ownerID').html(config.ownerID);
  
  m.when('p2p:ownOpen', function(id) {
    // If Connect On Load enabled,
    // wait for 'connection with friend open' callback
    // before connecting to the next
    var CONfriends = [];
    var CONindex = 0;

    for (friend in config.friends) {
      addFriendElement(friend);
      CONfriends.push(friend);
    }
  
    if (config.connectOnLoad) {
      m.when('p2p:open', function() {
        console.log('connection open: ' + CONfriends[CONindex]);
        if (CONindex < CONfriends.length) p2p.connect(CONfriends[CONindex++]);
        if (config.easy_setup && CONindex == CONfriends.length) {
          selectAllFriends();
          p2p.send('requestHostedNamespaces');
          sendScouts();
          deselectAllFriends();
        }
      });
      p2p.connect(CONfriends[CONindex]);
    }
  
  });

  if (typeof session == 'undefined' || !session.config || !config.skipLoadScreen) {
    $('#options').show();
  }
  setMonitorEnabled(config.monitorEnabled);

  // load options
  $('#skipOptions').get(0).checked = config.skipLoadScreen;
  $('#connectOnLoad').get(0).checked = config.connectOnLoad;
  $('#requestNamespacesOnLoad').get(0).checked = config.requestNamespacesOnLoad;
  $('#configLocalStorage').get(0).checked = config.saveConfigToLocalStorage;
  $('#graphlocalstorage').get(0).checked = config.saveGraphToLocalStorage;
  $('#linksetsEnabled').get(0).checked = config.linksetsEnabled;
  $('#visEnabled').get(0).checked = config.visualizationEnabled;
  
  for (var i = 0, l = config.namespaces.length; i < l; i++) {
    $('#namespaces').append($('<option value="' + config.namespaces[i] + '">' + config.namespaces[i] + '</option>'));
    dataGenerator.addNamespace(config.namespaces[i]);
  }

  if (!config.visualizationEnabled) {
    $('.visOption').attr('disabled', true);
    $('#status-message').css('color', '#fff');
  }

  $('#showLabels').get(0).checked = config.showLabels;
  $('#showMarkers').get(0).checked = config.showMarkers;
  $('#staticGraph').get(0).checked = config.staticGraph;
  $('#staticGraphIterations').attr('value', config.staticGraphIterations);

  $('#speedSlider').attr('value', config.animationInterval);
  
  $(document).keyup(function(e) {
    // Close the settings page and data input panel (in that order), and remove cursor from new friend field
    if (e.keyCode == 27) {    // Esc
      if ($('#options').css('display') != 'none') {
        $('#options').hide();
      } else {
        hidePullover();
      }
      
      $('#connectID').blur();
    }

    if (!$(e.target).is('input, textarea')) {
      // console.log('keyCode: ', e.keyCode);
      
      // Set cursor in new friend field
      if (e.keyCode == 70) {  // F
        $('#connectID').focus();
      }

      // Toggle data input panel
      if (e.keyCode == 192 || e.keyCode == 73) {  // ~ or I
        ($('#generateOptions').css('display') == 'none') ? showLoadOptions() : hidePullover();
      }

      // Show data input panel and generate random data
      if (e.keyCode == 71) {  // G
        showLoadOptions();
        $('#inputN3').val(dataGenerator.generate($('#triplesToGenerate').val()));
      }

      // When data input panel is open, generate graph from the data and close data input panel
      if (e.keyCode == 13 && $('#generateOptions').css('display') != 'none') {  // Enter
        hidePullover();
        generateGraph($('#inputN3').val());
      }

      // Toggle settings page
      if (e.keyCode == 79) {  // O
        ($('#options').css('display') == 'none') ? $('#options').show() : $('#options').hide();
      }

      // Start animating algorithm
      if (e.keyCode == 65 && ($('#algorithmControls').css('display') != 'none')) {  // A
        $('#animate').click();
      }

      // Run algorithm
      if (e.keyCode == 82 && ($('#algorithmControls').css('display') != 'none')) {  // R
        $('#run').click();
      }
      
      // Increase animation speed slider
      if (e.keyCode == 187 && ($('#algorithmControls').css('display') != 'none')) {  // +
        var newValue = Number($('#speedSlider').val()) + Number($('#speedSlider').attr('step'));
        if (newValue <= Number($('#speedSlider').attr('max'))) $('#speedSlider').val(newValue);
        $('#speedSlider').mouseup();
      }
      
      // Decrease animation speed slider
      if (e.keyCode == 189 && ($('#algorithmControls').css('display') != 'none')) {  // -
        var newValue = Number($('#speedSlider').val()) - Number($('#speedSlider').attr('step'));
        if (newValue >= Number($('#speedSlider').attr('min'))) $('#speedSlider').val(newValue);
        $('#speedSlider').mouseup();
      }
    }
  });
});

window.onunload = window.onbeforeunload = function(e) {
  p2p.destroy();

  if ($('#run').attr('value') == 'stop' && config.monitorEnabled == false) config.monitorEnabled = true;
  
  if (config.saveConfigToLocalStorage) localStorage[config.ownerID] = '{"config":' + JSON.stringify(config) + '}';
  // var storage = [];
  // storage.push((config.saveConfigToLocalStorage) ? ('"config":' + JSON.stringify(config)) : '');
  // storage.push((config.saveGraphToLocalStorage) ? ('"data":' + /*graph data*/) : '');
  // localStorage[config.ownerID] = '{' + storage.join() + '}';
}

// document.getElementById('files').addEventListener('change', loadFiles, false);

function loadFiles(files) {
  // add support for multiple files, and multiple seperate graphs
  var file = files[0];
  var reader = new FileReader();
  reader.readAsText(file);

  reader.onloadend = function(evt) {
    if (evt.target.readyState == FileReader.DONE) {
      var content = evt.target.result;
      generateGraph(content);
      $('#files').hide();
      $('#saveFile').show();
      $('#options').hide();
    }
  };
}

function saveAsFile(filename, data) {
  // var format = format || 'text/plain';
  var blob = new Blob([data], { type: 'text/plain' });

  var file = document.createElement('a');
  file.download = filename;
  file.href = window.webkitURL.createObjectURL(blob);
  file.click();
}

function loadExperimentFile() {
  var src = prompt('Filename: ', 'experiment.js');
  src += '?' + Date.now();

  loadModule(src, function() { monitor('Experiment file loaded.'); });
}

function loadModule(src, callback) {
  var script = document.createElement("script");
  script.type = "text/javascript";
  script.src = src;

  script.onload = callback();

  document.getElementsByTagName("head")[0].appendChild(script);
}

function hidePullover() {
  $('#pull').animate({height: 10}, 500);
  $('#pull').animate({width: 10}, 500);
  $('#pull_content').hide();
  $('#data').hide();
  $('#sharing').hide();
  $('#simulation').hide();
  $('#all_options').hide();
  $('#logs').hide();
}

function showLoadOptions() {
  $('#pull').animate({width: 600}, 500);
  $('#pull').animate({height: 550}, 500);
  $('#pull_content').show();
  $('#data').show();
//  $('#sm-1').html('');
}

function generateGraph(data) {
  if (config.visualizationEnabled) D3graph.newGraph('graph');

  rdfGraph.initialize();
  rdfGraph.clearGraph();
  rdfGraph.loadFromN3String(data);

  // if (config.visualizationEnabled) {
  //   loadModule('visualization.js', function() {
  //     monitor('visualization.js loaded');
  //     D3graph.newGraph('graph');

  //     rdfGraph.initialize();
  //     rdfGraph.loadFromN3String(data);
  //   });
  // } else {
  //   rdfGraph.initialize();
  //   rdfGraph.loadFromN3String(data);
  // }
}

var m = (function() {

  window.events = {};

  var listen = function(evnt, fn) {
    if (!events[evnt]) { 
      events[evnt] = [];
    }
    
    var listener_exists = false;
    
    for (var i = 0, l = events[evnt].length; i < l; i++) {
      if (events[evnt][i].callback.toString() == fn.toString()) listener_exists = true;
    }
    
    if (!listener_exists) events[evnt].push({ context: this, callback: fn });

    return this;
  };

  var ignore = function(evnt) {
    delete events[evnt];
  };

  var trigger = function(evnt) {
    var args;

    if (!events[evnt]) {
      return false;
    } 

    args = Array.prototype.slice.call(arguments, 1);
    for (var i = 0, l = events[evnt].length; i < l; i++) {

      var listener = events[evnt][i];
      listener.callback.apply(listener.context, args);
    }
    return this;
  };

  return {
    notify: trigger,
    when: listen,
    unsubscribe: ignore // removes all listeners
  };

})();

m.when('rdf:loaded', function(numOfTriples) { $('#sm-1').html('Graph loaded. (' + numOfTriples + ' triples)').attr('title', new Date().toLocaleString()); });

m.when('rdf:loaded', function(data) {
  swarm.initialize();
//   console.log('nodesIndex:');
//   console.log(data.nodesIndex);
//   console.log('edgesIndex:');
//   console.log(data.edgesIndex);
//   console.log('nodes:');
//   console.log(data.nodes);
//   console.log('edges:');
//   console.log(data.edges);
//   // console.log('inferredEdges:');
//   // console.log(data.inferredEdges);
});

m.when('swm:sctsSentTo', function(scouts, receivers) {
  receivers.forEach(function(receiver) {
    monitor('Sent ' + scouts.length + ' scout' + ((scouts.length == 1) ? '' : 's') + ' to ' + receiver + '.');
  });
});

m.when('swm:sctMigratedTo', function(scout, receiver) {
  monitor('Scout from ' + scout.owner + ' migrated to ' + utils.getHash(scout.isAt.id) + ' at ' + receiver, 'scout id: ' + scout.id);
});

m.when('swm:fgrsSentTo', function(foragers, receiver) {
  monitor('Sent ' + foragers.length + ' forager' + ((foragers.length == 1) ? '' : 's') + ' to ' + receiver + '.');
});

m.when('swm:sctFound', function(data) {
  // connect to data.owner IF not already on friendsList (could be migrated via other peer)
  //  THEN send message first saying 'new peer connected, wants to send your scout back'.
  // TODO: remove scout
  p2p.connect(data.owner);

  console.log('Going to request foragers from: ' + data.owner);
  p2p.send('requestHelpForagers', { 'type': data.type, 'node': data.node }, data.owner); // = scout
});

m.when('swm:fgrFound', function(data) {
  console.log('Going to send forager back to: ' + data.owner);
  p2p.send('returningForagers', data.memory, data.owner); // = forager
});

m.when('swm:sctRemoved', function(data) {
  monitor('Removed scout from ' + data.owner + '.', 'scout id: ' + data.id);
});

m.when('swm:fgrRemoved', function(data) {
  monitor('Removed forager from ' + data.owner + '.', 'forager id: ' + data.id);
});

var lastInferredEdge = null;

m.when('rdf:initialized', function(data) {
  if (config.visualizationEnabled) {
    m.when('rdf:loaded', function(data) { if (config.staticGraph) D3graph.restart(); });

    m.when('rdf:nodeNew', function(data) {
      D3graph.newNode(data);
      
      var baseURI = utils.getBase(data.id);
      var reservedNamespaces = ['http://www.w3.org/2000/01/rdf-schema', 'http://www.w3.org/1999/02/22-rdf-syntax-ns', 'http://www.w3.org/2002/07/owl'];
      if (config.namespaces.indexOf(baseURI) == -1 && reservedNamespaces.indexOf(baseURI) == -1) {
        config.namespaces.push(baseURI);
        $('#namespaces').append($('<option value="' + baseURI + '">' + baseURI + '</option>'));
      }
    });
    m.when('rdf:edgeNew', function(data) {
      if (lastInferredEdge && lastInferredEdge == rdfGraph.edgeToString(data)) {
        // TODO: implement difference 'local' and 'remote' inferred triple
        D3graph.newLink(data, 'local');
        lastInferredEdge = null;
      } else {
        D3graph.newLink(data, 'none');
      }
    });
    m.when('rdf:inferredNew', function(data) { lastInferredEdge = rdfGraph.edgeToString(data); });

    // m.when('rdf:loaded', swarmVis.initialize);

    // m.when('swm:sctInit', swarmVis.newScout);
    // m.when('swm:fgrInit', swarmVis.newForager);
    // m.when('swm:nrsInit', swarmVis.newNurseBee);

    // m.when('swm:sctMove', swarmVis.moveScout);
    // m.when('swm:fgrMove', swarmVis.moveForager);
    // m.when('swm:nrsMove', swarmVis.moveNurseBee);

    // m.when('swm:sctRemoved', swarmVis.removeScout);
    // m.when('swm:fgrRemoved', swarmVis.removeForager);
    // m.when('swm:nrsRemoved', swarmVis.removeNurseBee);

    m.when('swm:sctMove', function(data) { D3graph.styleNode(data.isAt.id, 'scout'); });
    m.when('swm:sctMigratedTo', function(scout, receiver) { D3graph.styleNode(scout.isAt.id, 'scout'); });
    m.when('swm:fgrMove', function(data) { D3graph.styleNode(data.isAt.id, 'forager'); });
    m.when('swm:nrsMove', function(data) { D3graph.styleNode(data.isAt.id, 'nurse'); });

    m.when('swm:sctRemoved', function(data) { D3graph.unstyleNode(data.isAt.id, 'scout'); });
    m.when('swm:fgrRemoved', function(data) { D3graph.unstyleNode(data.isAt.id, 'forager'); });
  }
});

m.when('rdf:edgeNew', function(data) { $('#sm-1').html('Graph loaded. (' + rdfGraph.getNumOfTriples() + ' triples)').attr('title', new Date().toLocaleString()); });

function logger(data) {
  console.log(data);
}

function monitor(message, tooltip, type) {
  if (config.monitorEnabled) {
    tooltip = tooltip || '';
    type = type || '';
    var logger = $('#history');
    logger.append('<p class="' + type + '" title="' + tooltip + '">' + message + '</p>');
    logger.scrollTop(logger[0].scrollHeight);
  }
}

function setMonitorEnabled(value) {
  config.monitorEnabled = value;
  $('#monitorEnabled').get(0).checked = value;
  $('#history').css('display', value ? 'block' : 'none');
}

function addToHostedNamespaces(uri) {
  if (config.hosts.indexOf(uri) == -1) {
    config.hosts.push(uri);
    monitor(uri + ' added to hosted namespaces list.');
    return uri;
  } else {
    return null;
  }
}

function removeFromHostedNamespaces(uri) {
  if (config.hosts.indexOf(uri) != -1) {
    config.hosts.splice(config.hosts.indexOf(uri), 1);
    monitor(uri + ' removed from hosted namespaces list.');
    return uri;
  } else {
    return null;
  }
}

function addFriend(id, hosts) {
  hosts = hosts || [];
  if (!config.friends[id] && id != config.ownerID) {
    config.friends[id] = { 'hosts': hosts, 'selected': false, 'blockIn': false, 'blockOut': false };

    addFriendElement(id);
  }
  
  if (config.easy_setup) {
    $('#namespaceToAdd').val(id);
    $('#addNamespace').click();
    
    deselectAllFriends();
    
    selectFriend(id);
    p2p.send('requestHostedNamespaces');
    sendScouts();
    deselectFriend(id);
  }
}

function addFriendElement(id) {
  var friendElement = $('<div></div>').addClass('friend');
  var name = $('<span class="name" id="' + id + '"> ' + id + '</span>');
  var blockIn = $('<span class="option' + ((config.friends[id].blockIn) ? ' block' : '') + '">[block IN]</span>');
  var blockOut = $('<span class="option' + ((config.friends[id].blockOut) ? ' block' : '') + '">[block OUT]</span>');
  var remove = $('<span class="option">[X]</span>');
  friendElement.append(name);
  friendElement.append(remove);
  friendElement.append(blockOut);
  friendElement.append(blockIn);
  $('#friendsList').append(friendElement);
  name.on('click', function() {
    if ($(this).hasClass('connected')) { 
      $(this).toggleClass('selected');
      config.friends[id]['selected'] = !config.friends[id]['selected'];
    } else {
      p2p.connect(id);
    }
  });
  blockIn.on('click', function() { $(this).toggleClass('block'); config.friends[id]['blockIn'] = !config.friends[id]['blockIn']; });
  blockOut.on('click', function() { $(this).toggleClass('block'); config.friends[id]['blockOut'] = !config.friends[id]['blockOut']; });
  remove.on('click', function() { removeFriend(id); });
}

function removeFriend(id) {
  // Ask for confirmation.
  //&& confirm('Are you sure you want to remove ' + id + ' from the list?')
  if (config.friends[id]) {
    p2p.close(id);
    $('#' + id).parent().remove();
    delete config.friends[id];
  }

  // if ($('.friendsList').length === 0) {
  //   $('.filler').show();
  // }
}

function forEachSelectedConnection(fn) {
  for (friend in config.friends) {
    if (config.friends[friend]['selected']) {
      console.log(friend);
    }
  }
  // var selected = $('.selected');
  // selected.each(function() {
  //   var peerId = $(this).attr('id');
  //   var conn = peer.connections[peerId].peerjs;
  //   fn(conn, $(this));
  // });
}

function selectFriend(id) {
  config.friends[id]['selected'] = true;
  $('#' + id).addClass('selected');
}

function deselectFriend(id) {
  config.friends[id]['selected'] = false;
  $('#' + id).removeClass('selected');
}

function selectAllFriends() {
  for (friend in config.friends) {
    selectFriend(friend);
    // config.friends[friend]['selected'] = true;
    // $('.name').addClass('selected');
  }
}

function deselectAllFriends() {
  for (friend in config.friends) {
    deselectFriend(friend);
    // config.friends[friend]['selected'] = false;
    // $('.name').removeClass('selected');
  }
}

m.when('p2p:conn', function(data) {
  monitor(data.peer + ' has connected with you.');
  addFriend(data.peer);
  $('#' + data.peer).addClass('connected');

  if (config.easy_setup) {
    // alert('sending scouts');
    p2p.send('requestHostedNamespaces');
    sendScouts();
  }
});

m.when('p2p:open', function(data) {
  if (data != config.ownerID) monitor('Connection opened with ' + data + '.');
  $('#' + data).addClass('connected');
});

m.when('p2p:close', function(data) {
  $('#' + data.peer).removeClass('connected').removeClass('selected');
  config.friends[data.peer]['selected'] = false;
  monitor('Lost connection with ' + data.peer + '.');

  swarm.removeScoutsFrom(data.peer);
  swarm.removeForagersFrom(data.peer);
});

m.when('p2p:data', processIncomingMessage);

var protocol = {
  'requestHostedNamespaces': sendHostedNamespaces,
  'hostedNamespaces': addHostedNamespaces,
  'requestIdsForDataset': sendIdsForDataset,
  'requestNodesList': sendNodesList,
  'nodesList': addNodesList,
  'requestIdsForNode': sendIdsForNode,
  'friendIds': addFriends,
  'scouts': addForeignScouts,
  'foragers': addForeignForagers,
  'requestHelpForagers': sendForagers,
  'returningForagers': addTriplesFromReturnedForagers,
  'requestDenied': handleDeniedRequest
}

// TODO: extend
//    [send personal behavior functions] (when implementing configurable behavior through file/options)
//    [response to sent bees: too many/over my (total) limit]
//    [retrieve my bees]

function processIncomingMessage(data) {
  console.log('received from ' + data.sender + ':');
  console.log(data);

  if (config.friends[data.sender]['blockIn']) {
    p2p.send('requestDenied', 'Blocked.', data.sender);
    return;
  }

  if (protocol[data.type]) {
    protocol[data.type].call(undefined, data); // first arg: 'this', further args: args to function
  } else {
    // default
  }
}


// TODO: in seperate 'actions' file?
function sendHostedNamespaces(data) {
  var accept = true; //confirm('Share which namespaces you host with ' + data.sender + '?');
  if (accept && config.hosts) {
    monitor('Send information about my hosted namespaces to ' + data.sender + '.');
    p2p.send('hostedNamespaces', config.hosts, data.sender);
  } else {
    // p2p.send('requestDenied', '', data.sender); ?
  }
}

function addHostedNamespaces(data) {
  monitor('Received information about hosted namespaces from ' + data.sender + '.');
  console.log('received hosted namespaces list:');
  console.log(data.message);
  config.friends[data.sender]['hosts'] = data.message;

  for (dataset in data.message) {
    if (config.hostedBy[data.message[dataset]] && config.hostedBy[data.message[dataset]].indexOf(data.sender) == -1) {
      config.hostedBy[data.message[dataset]].push(data.sender);
    } else {
      config.hostedBy[data.message[dataset]] = [data.sender];
    }
  };
}

function sendIdsForDataset(data) {
  if (config.hostedBy[data.message]) {
    var friendIds = { 'dataset': data.message, 'ids': config.hostedBy[data.message] };
    p2p.send('friendIds', friendIds, data.sender);
  }
}

function sendNodesList(data) {
  var accept = confirm('Share your nodes list with ' + data.sender + '?');
  if (accept && rdfGraph) {
    monitor('Sent my nodes list to ' + data.sender + '.');
    p2p.send('nodesList', rdfGraph.getNodes(), data.sender); // TODO: only most important ones (e.g. 10)?
  } else {
    // p2p.send('requestDenied', '', data.sender); ?
  }
}

nodesLists = {};

function addNodesList(data) {
  monitor('Received nodes list from ' + data.sender + '.');
  // $('#' + data.sender).css('color','green'); ?
  nodesLists[data.sender] = data.message;
}

function sendIdsForNode(data) {
  if (Object.keys(nodesLists).length != 0) {
    var friendIds = { 'node': data.message, 'ids': {} };
    for (friend in nodesLists) {
      if (friend != data.sender && Object.keys(nodesLists[friend]).indexOf(data.message)) { friendIds['ids'][friend] = nodesLists[friend][data.message]; }
    }
    p2p.send('friendIds', friendIds, data.sender);
  }
}

function addFriends(data) {
  if (data.message['node']) {
    for (id in data.message['ids']) {
      monitor(id + ' has ' + data.message['ids'][id] + ' triple' + ((data.message['ids'][id] == 1) ? '' : 's') + ' with ' + data.message['node']);
    };
    // do something with received friend IDs
    // maybe connect to IDs and/or immediately send them scouts for the node (used for requesting these IDs)
  }

  if (data.message['dataset']) {
    var datasetURI = data.message['dataset'];
    if (!config.hostedBy[datasetURI]) config.hostedBy[datasetURI] = [];

    data.message['ids'].forEach(function(id) {
      monitor(id + ' hosts dataset ' + datasetURI);
      addFriend(id, datasetURI);
      config.hostedBy[datasetURI].push(id);
      p2p.connect(id);
    });
  }
}

function addForeignScouts(data) {
  monitor('Received ' + data.message.length + ' scout' + ((data.message.length == 1) ? '' : 's') + ' from ' + data.sender + '.');

  for (scout in data.message) {
    swarm.addScout(data.message[scout]);
  };
}

function addForeignForagers(data) {
  monitor('Received ' + data.message.length + ' forager' + ((data.message.length == 1) ? '' : 's') + ' from ' + data.sender + '.');

  for (forager in data.message) {
    swarm.addForager(data.message[forager]);
  };
}

function sendScouts() {
  var scouts = swarm.initializeScouts();
  p2p.send('scouts', scouts);

  var receivers = [];
  for (friend in config.friends) {
    if (config.friends[friend]['selected']) {
      receivers.push(friend);
    }
  } // TODO: still hack, properly implement p2p.send() returning array of friend IDs

  m.notify('swm:sctsSentTo', scouts, receivers);
}

function sendForagers(data) {
  monitor('Scout at ' + data.sender + ' found:');
  monitor('&nbsp;&nbsp;' + utils.getHash(data.message.node), '', 'data');

  var foragers = swarm.initializeForagers(data.message.type, data.message.node);
  p2p.send('foragers', foragers, data.sender);

  m.notify('swm:fgrsSentTo', foragers, data.sender);
}

function addTriplesFromReturnedForagers(data) {
  var source = data.message.source;
  var id = data.message.id;
  var target = data.message.target;

  monitor('Forager returned from ' + data.sender + ', found:');
  monitor('&nbsp;&nbsp;' + utils.getHash(source) + ' ' + utils.getHash(id) + ' ' + utils.getHash(target), '', 'data');
  // TODO: add found triple to graph
  var subject = rdfGraph.newNode(source, utils.getTypeURI(source), false) || rdfGraph.getNode(source); // newNode: add new / not new: increment node's frequency + getNode
  var object = rdfGraph.newNode(target, utils.getTypeURI(target), false) || rdfGraph.getNode(target);
  rdfGraph.newInferredEdge(subject, id, object, data.message.type);
}

function handleDeniedRequest(data) {
  console.log('Request denied by ' + data.sender + '.'); // include ready to use 'why denied message' ?
}

