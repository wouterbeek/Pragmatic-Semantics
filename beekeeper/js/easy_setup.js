var easy_setup = (function() {
  function start_setup() {
    config.easy_setup = true;
    
    hidePullover();
    
    config.skipLoadScreen = true;
    $('#skipOptions').get(0).checked = config.skipLoadScreen;
    
    config.requestNamespacesOnLoad = true;
    $('#requestNamespacesOnLoad').get(0).checked = config.requestNamespacesOnLoad;
    
    $('#namespaceToAdd').val(config.ownerID);
    $('#addNamespace').click();
    //addToHostedNamespaces(config.ownerID);
    
    // Set the number of triples.
    $('#triplesToGenerate').val(100)
    // Generate a dataset.
    $('#generateData').click();
    // Generate the graph
    $('#generate').click();
    
    $('#animate').click();
    $('#speedSlider').val(2010);
    $('#speedSlider').mouseup();
  }
  
  return {
    start: start_setup
  };
})();
