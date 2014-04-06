var easySetup = (function() {
	
	function startSetup() {
		// config.easySetup = true;
		// 
		// config.skipLoadScreen = true;
		// $('#skipLoadScreen').get(0).checked = config.skipLoadScreen;
		// hideLoadScreen();
		// 
		// config.requestDatasetsOnLoad = true;
		// $('#requestDatasetsOnLoad').get(0).checked = config.requestDatasetsOnLoad;
		// 
		// $('#namespaceToAdd').val(config.ownerID);
		// $('#addNamespace').click();
		// 
		// $('#inputN3').val(dataGenerator.generate($('#triplesToGenerate').val()));
		// generateGraph($('#inputN3').val());
		// 
		// config.animationInterval = 10;
		// $('#animate').click();
		// $('#speedSlider').val(2010);
		// $('#speedSlider').mouseup();


		config.easySetup = true;
		
		config.skipLoadScreen = true;
		config.connectOnLoad = true;
		config.generateDataOnLoad = true;
		config.startAnimationOnLoad = true;
		config.requestDatasetsOnLoad = true;
		config.sendScoutsOnLoad = true;
		config.requestDatasetsOnConnect = true;
		config.sendScoutsOnConnect = true;
		config.monitorEnabled = true;
		config.statusMessageEnabled = true;
		config.controlPanelEnabled = true;
		config.visualizationEnabled = true;
		config.staticGraph = false;
		config.showLabels = true;
		config.showMarkers = true;
		config.animationInterval = 1910;
		
		$('#namespaceToAdd').val(config.ownerID);
		$('#addNamespace').click();
		
		reloadPage();
	}
	
	return {
		
		start: startSetup
		
	};
	
})();