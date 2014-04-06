var p2p = (function() {
  var peerjsAPIkey = "68b3eyjk3qt49529";
  var peer;

  function handleConnection(c) {
    c.on('data', function(data) {
      m.notify('p2p:data', data);
    });

    c.on('close', function() {
      m.notify('p2p:close', c);
    }); // TODO: accept new peer?
  }

  function eachSelectedConnection(fn) {
    for (peerId in config.friends) {
      if (config.friends[peerId]['selected']) {
        var conn = peer.connections[peerId].peerjs;
        fn(conn);
      }
    }

    // var selected = $('.selected');
    // selected.each(function() {
    //   var peerId = $(this).attr('id');
    //   var conn = peer.connections[peerId].peerjs;
    //   fn(conn, $(this));
    // });
  }

  return {
    setup: function(id) {
      peer = new Peer(id, { key: peerjsAPIkey });
      peer.on('open', function(id) {
        // own connection is ready
        console.log('p2p ownOpen: ' + id);
        m.notify('p2p:ownOpen', id);
      });
      peer.on('connection', function(c) {
        // connection with other peer is ready
        handleConnection(c);

        console.log('p2p conn: ' + c.peer);
        m.notify('p2p:conn', c);
      });
    },

    connect: function(id) { // (id, callback)
      if (!peer.connections[id]) {
        var c = peer.connect(id, { reliable: true });
        c.on('open', function() {
          handleConnection(c);

          console.log('p2p open: ' + id);
          m.notify('p2p:open', id);

          // typeof callback === 'function' && callback();
        });
        
        c.on('data', function(data){
          //code
        });
      }
    },

    send: function(type, data, to) { // TODO: implement as one object argument: p2p.send({ 'type': someType, 'data': someData, 'to': someoneID })
      data = data || '';
      to = to || null;
      message = { 'type': type, 'sender': config.ownerID, 'message': data };

      if (to) {
        if (config.friends[to]['blockOut']) {
          console.log('Outgoing message to ' + to + ' blocked.');
        } else {
          if (peer.connections[to] && peer.connections[to].peerjs.open) peer.connections[to].peerjs.send(message);
    
          console.log('sent to ' + to + ':');
          console.log(message);
        }
      } else {
        eachSelectedConnection(function(c) {
          if (config.friends[c.peer]['blockOut']) {
            console.log('Outgoing message to ' + c.peer + ' blocked.');
          } else {
            if (c.open) c.send(message); // && not 
    
            console.log('sent to ' + c.peer + ':');
            console.log(message);
          }
        });
      }
    },

    close: function(id) {
      id = id || null;

      if (id) {
        peer.connections[id].peerjs.close();
      } else {
        eachSelectedConnection(function(c) {
          c.close();
        });
      }
    },

    destroy: function() {
      if (!!peer && !peer.destroyed) {
        peer.destroy();
      }
    },
    
    connections: function() {
      return peer.connections;
    }
  };
})();
