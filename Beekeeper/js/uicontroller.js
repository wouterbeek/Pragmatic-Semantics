$(function () {
  $('#showOptions').click(function() {
    $('#options').show();
  });

  $('#hideOptions').click(function(){
    $('#options').hide();
  });

  $('#pull_options').click(function() {
    if ($('#all_options').css("display") == "block") {
      hidePullover();
    } else {
      hidePullover();
      $('#pull').animate({width: 600}, 500);
      $('#pull').animate({height: 1250});
      $('#pull_content').show();
      $('#all_options').show();
    }
  });

  $('#pull_simulation').click(function() {
    if ($('#simulation').css("display") == "block") {
      hidePullover();
    } else {
      hidePullover();
      $('#pull_content').show();
      $('#simulation').show();
      $('#simulation').css('width', 'auto');
      $('#pull').css('width', 'auto');
    }
  });

  $('#pull_data').click(function() {
    if ($('#data').css("display") == "block") {
      hidePullover();
    } else {
      hidePullover();
      showLoadOptions();
    }
  });

  $('#pull_logs').click(function() {
    if ($('#logs').css("display") == "block") {
      hidePullover();
    } else {
      hidePullover();
      $('#pull').animate({width: 600}, 500);
      $('#pull').animate({height: 600});
      $('#pull_content').show();
      $('#logs').show();
    }
  });

  $('#pull_sharing').click(function() {
    if ($('#sharing').css("display") == "block") {
      hidePullover();
    } else {
      hidePullover();
      $('#pull_content').show();
      $('#sharing').show();
      $('#sharing').css('width', 'auto');
      $('#pull').css('width', 'auto');
    }
  });
  
  $('#generate').click(function() {
    generateGraph($('#inputN3').val());
    hidePullover();
  });
  
  $('#generateData').click(function() {
    $('#inputN3').val(dataGenerator.generate($('#triplesToGenerate').val()));
  });
  
  $('#addNamespace').click(function() {
    if ($('#namespaceToAdd').val() != '')
      dataGenerator.addNamespace($('#namespaceToAdd').val());
    $('#namespaceToAdd').val('');
  });

  $('#file').click(function() {
    var file = $('#input').get(0).files[0];
    loadFile(file);
  });

  $('#saveFile').click(function() {
    rdfGraph.saveToFile();
  });

  $('#loadExperiment').click(function() {
    loadExperimentFile();
    $('#loadExperiment').hide();
    $('#saveExperiment').show();
  });

  $('#saveExperiment').click(function() {
    var filename = prompt('Filename: ', 'experiment_data.txt');
    var data = makeExport();
    saveAsFile(filename, data);
  });

  $('#skipOptions').click(function() {
    config.skipLoadScreen = $(this).is(':checked');
  });

  $('#connectOnLoad').click(function() {
    config.connectOnLoad = $(this).is(':checked');
  });

  $('#requestNamespacesOnLoad').click(function() {
    config.requestNamespacesOnLoad = $(this).is(':checked');
  });

  $('#configLocalStorage').click(function() {
    config.saveConfigToLocalStorage = $(this).is(':checked');
  });
  
  $('#removeConfigFromLocalStorage').click(function() {
    if (confirm('Are you sure you want to clear the current configuration from local storage?')) localStorage.removeItem(config.ownerID);
  });

  $('#graphlocalstorage').click(function() {
    config.saveGraphToLocalStorage = $(this).is(':checked');
  });

  $('#linksetsEnabled').click(function() {
    config.linksetsEnabled = $(this).is(':checked');
  });

  $('#monitorEnabled').click(function() {
    setMonitorEnabled($(this).is(':checked'));
  });

  $('#visEnabled').click(function() {
    config.visualizationEnabled = $(this).is(':checked');
    
    $('.visOption').attr('disabled', !config.visualizationEnabled);
  });

  $('#showLabels').click(function() {
    config.showLabels = $(this).is(':checked');
  });

  $('#showMarkers').click(function() {
    config.showMarkers = $(this).is(':checked');
  });

  $('#staticGraph').click(function() {
    config.staticGraph = $(this).is(':checked');
  });

  $('#staticGraphIterations').on('change', function() {
    config.staticGraphIterations = $(this).val();
  });

  $('#do').click(function() {
    // $('#doData').val()
    // D3graph.newNode($('#doData').val());

    // p2p.send('requestIdsForNode', $('#doData').val());

    // addToHostedNamespaces($('#doData').val());
    // removeFromHostedNamespaces($('#doData').val());

    // saveAsFile($('#doData').val(), 'Some content\non multiple lines.');
  });

  $('#addNamespace').click(function() {
    if ($('#namespaces').val() != '') {
      addToHostedNamespaces($('#namespaces').val());
    }
  });

  $('#removeNamespace').click(function() {
    if ($('#namespaces').val() != '') {
      removeFromHostedNamespaces($('#namespaces').val());
    }
  });

  $('#currentNamespaces').click(function() {
    var namespaces = config.hosts.join('\n');
    if (namespaces == '') namespaces = '(no namespaces)';
    alert('Hosted namespaces:\n' + namespaces);
    console.log(config.hosts);
  });

  $('#connect').click(function() {
    var id = $('#connectID').val();
    addFriend(id);
    p2p.connect(id);
    $('#connectID').val('');
  });
  
  $('#selectAllFriends').click(function() {
    selectAllFriends();
  });
  
  $('#deselectAllFriends').click(function() {
    deselectAllFriends();
  });

  // $('#requestList').click(function() {
  //   p2p.send('requestNodesList');
  // });

  $('#requestHostedNamespaces').click(function() {
    p2p.send('requestHostedNamespaces');
  });

  $('#sendScouts').click(function() {
    sendScouts();
  });

  $('#step').click(function() {
    swarm.step();
  });

  $('#animate').click(function() {
    if ($(this).attr('value') == 'animate') {
      $('#step').hide();
      $('#run').hide();
      $('.animationSpeed').show();
      $(this).attr('value', 'pause');
      if (window.animate) window.clearInterval(animate);
      window.animate = window.setInterval(function() { swarm.step(); }, config.animationInterval);
    } else if ($(this).attr('value') == 'pause') {
      $(this).attr('value', 'animate');
      $('.animationSpeed').hide();
      $('#run').show();
      $('#step').show();
      if (window.animate) window.clearInterval(animate);
    }
  });

  $('#speedSlider').mouseup(function(){
    var range = Number($(this).attr('min')) + Number($(this).attr('max'));
    config.animationInterval = range - this.value;

    if (window.animate) {
      window.clearInterval(animate);
      window.animate = window.setInterval(function() { swarm.step(); }, config.animationInterval);
    }
  });

  $('#run').click(function() {
    if ($(this).attr('value') == 'run') {
      $(this).attr('value', 'stop');
      setMonitorEnabled(false);
      // swarm.run();
      if (window.animate) window.clearInterval(animate);
      window.animate = window.setInterval(function() { swarm.step(); }, 0);
    } else if ($(this).attr('value') == 'stop') {
      $(this).attr('value', 'run');
      setMonitorEnabled(true);
      // swarm.stop();
      if (window.animate) window.clearInterval(animate);
    }
  });
});
