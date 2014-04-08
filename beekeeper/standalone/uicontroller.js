$(function () {
    $('#showOptions').click(function() {
        showLoadScreen();
    });

    $('#hideOptions').click(function(){
        hideLoadScreen();
    });

    $('#load').click(function() {
        ($('#inputN3').css('display') == 'none') ? showLoadOptions() : hideLoadOptions();
    });

    $('#generate').click(function() {
        hideLoadOptions();
        generateGraph($('#inputN3').val());
    });
	
	$('#cancelGenerate').click(function() {
		hideLoadOptions();
	});
	
	$('#generateData').click(function() {
		$('#inputN3').val(dataGenerator.generate($('#triplesToGenerate').val()));
	});
	
	$('#addNamespace').click(function() {
		if ($('#namespaceToAdd').val() != '')
			dataGenerator.addNamespace($('#namespaceToAdd').val());
		$('#namespaceToAdd').val('');
	});
	
	$('#loadFromFile').click(function() {
		$('#files').click();
	});

    $('#saveFile').click(function() {
        rdfGraph.saveToFile();
    });
	
	$('#optionsPanel').click(function(e) {
		e.stopPropagation();
	});
	
	$('#permanentShowLoadScreen').click(function() {
		showLoadScreen();
	});
	
	$('#loadscreen').click(function() {
		hideLoadScreen();
	});
	
	$('#changeName').click(function() {
		if (setOwnerID($('#userID').val()) == false) {
			var backgroundColor = $('#userID').css('background');
			$('#userID').css('background', 'red');
			$('#userID').val(config.ownerID);
			$('#userID').animate({ background: backgroundColor });
			$('#invalidNameMessage').show();
			
			window.setTimeout(function() {
				$('#invalidNameMessage').hide();
			}, 6000);
		}
	});
	
	$('#easySetup').click(function() {
		if (!config.easySetup) {
			easySetup.start();
		} else {
			config.easySetup = false;
		}
	});
	
	$('#defaultSetup').click(function() {
		config.easySetup = false;
		config.skipLoadScreen = false;
		config.connectOnLoad = true;
		config.generateDataOnLoad = false;
		config.startAnimationOnLoad = false;
		config.requestDatasetsOnLoad = false;
		config.sendScoutsOnLoad = false;
		config.requestDatasetsOnConnect = false;
		config.sendScoutsOnConnect = false;
		config.monitorEnabled = true;
		config.statusMessageEnabled = true;
		config.controlPanelEnabled = true;
		config.visualizationEnabled = true;
		config.staticGraph = false;
		config.showLabels = true;
		config.showMarkers = true;
		config.animationInterval = 1000;
		
		reloadPage();
	});
	
	// $('label').click(function() {
	// 	// console.log(this);
	// 	if (!config[$(this).attr('for')] && $('#' + $(this).attr('for')).attr('disabled') != true) {
	// 		// Assumes width defined with 'px'.
	// 		var parentWidth = parseInt($(this).parent().css('width'), 10) - 20;
	// 		var width = parseInt($(this).css('width'), 10);
	// 		$(this).animate({ left: (parentWidth - width) + 'px', background: 'green' });
	// 	} else {
	// 		$(this).animate({ left: '0px', background: 'red' });
	// 	}
	// });

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

	$('.switch').click(function() {
		var label = $('.switchLabel', this);
		
		if (!label.hasClass('disabled')) {
			label.toggleClass('enabled');
			config[label.attr('id')] = (label.hasClass('enabled'));
		
			setSwitch(label, (label.hasClass('enabled')));
		}		
	});

    // $('#skipLoadScreen').click(function() {
    //     config.skipLoadScreen = $(this).is(':checked');
    // });

    // $('#connectOnLoad').click(function() {
    //     config.connectOnLoad = $(this).is(':checked');
    // });
	
	// $('#generateDataOnLoad').click(function() {
	// 	config.generateDataOnLoad = $(this).is(':checked');
	// });
	
	// $('#startAnimationOnLoad').click(function() {
	// 	config.startAnimationOnLoad = $(this).is(':checked');
	// });

    // $('#requestDatasetsOnLoad').click(function() {
    //     config.requestDatasetsOnLoad = $(this).is(':checked');
    // });
	
	// $('#sendScoutsOnLoad').click(function() {
	// 	config.sendScoutsOnLoad = $(this).is(':checked');
	// });
	
    // $('#requestDatasetsOnConnect').click(function() {
    //     config.requestDatasetsOnConnect = $(this).is(':checked');
    // });
	
	// $('#sendScoutsOnConnect').click(function() {
	// 	config.sendScoutsOnConnect = $(this).is(':checked');
	// });

    // $('#saveConfigToLocalStorage').click(function() {
    //     config.saveConfigToLocalStorage = $(this).is(':checked');
    // });
	
	$('#removeconfigfromlocalstorage').click(function() {
		if (confirm('Are you sure you want to clear the current configuration from local storage?')) localStorage.removeItem(config.ownerID);
	});

    // $('#saveGraphToLocalStorage').click(function() {
    //     config.saveGraphToLocalStorage = $(this).is(':checked');
    // });

    // $('#linksetsEnabled').click(function() {
    //     config.linksetsEnabled = $(this).is(':checked');
    // });

    // $('#monitorEnabled').click(function() {
    //     setMonitorEnabled($(this).is(':checked'));
    // });
	
	$('#clearMonitor').click(function() {
		clearMonitor();
	});

    // $('#visualizationEnabled').click(function() {
    //    config.visualizationEnabled = $(this).is(':checked');
    // });

    // $('#showLabels').click(function() {
    //     config.showLabels = $(this).is(':checked');
    // });

    // $('#showMarkers').click(function() {
    //     config.showMarkers = $(this).is(':checked');
    // });

		//     $('#staticGraph').click(function() {
		//         config.staticGraph = $(this).is(':checked');
		// $('#staticGraphIterations').prop('disabled', (config.staticGraph) ? false : true);
		//     });

    $('#staticGraphIterations').on('change', function() {
        config.staticGraphIterations = $(this).val();
    });

    $('#do').click(function() {
        // $('#doData').val()
        // D3graph.newNode($('#doData').val());

        // p2p.send('requestIdsForNode', $('#doData').val());

        // addToHosts($('#doData').val());
        // removeFromHosts($('#doData').val());

        // saveAsFile($('#doData').val(), 'Some content\non multiple lines.');
    });

    $('#addDataset').click(function() {
        // if ($('#datasets').val() != '') {
            addToHosts($('#datasets').val());
			// updateHostedDatasetsList();
        // }
    });

    $('#removeDataset').click(function() {
        // if ($('#datasets').val() != '') {
            removeFromHosts($('#datasets').val());
			// updateHostedDatasetsList();
        // }
    });

    // $('#currentDatasets').click(function() {
//         var datasets = config.hosts.join('\n');
//         if (datasets == '') datasets = '(no datasets)';
//         alert('Hosted Datasets:\n' + datasets);
//         console.log(config.hosts);
//     });

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
    //     p2p.send('requestNodesList');
    // });

    $('#requestHostedDatasets').click(function() {
        p2p.send('requestHostedDatasets');
    });

    $('#sendScouts').click(function() {
        sendScouts();
    });

    $('#step').click(function() {
        swarm.step();
    });

    $('#animate').click(function() {
        if ($(this).attr('value') == 'Animate') {
            $('#step').hide();
            $('#run').hide();
            $('.animationSpeed').show();
            $(this).attr('value', 'Pause');
			var sliderRange = Number($('#speedSlider').attr('min')) + Number($('#speedSlider').attr('max'));
            if (window.animate) window.clearInterval(animate);
            window.animate = window.setInterval(function() { swarm.step(); }, sliderRange - config.animationInterval);
        } else if ($(this).attr('value') == 'Pause') {
            $(this).attr('value', 'Animate');
            $('.animationSpeed').hide();
            $('#run').show();
            $('#step').show();
            if (window.animate) window.clearInterval(animate);
        }
    });

    $('#speedSlider').mouseup(function(){
        config.animationInterval = Number(this.value);

        if (window.animate) {
			var sliderRange = Number($(this).attr('min')) + Number($(this).attr('max'));
            window.clearInterval(animate);
            window.animate = window.setInterval(function() { swarm.step(); }, sliderRange - config.animationInterval);
        }
    });

    $('#run').click(function() {
        if ($(this).attr('value') == 'Run') {
            $(this).attr('value', 'Stop');
            setMonitorEnabled(false);
            // swarm.run();
            if (window.animate) window.clearInterval(animate);
            window.animate = window.setInterval(function() { swarm.step(); }, 0);
        } else if ($(this).attr('value') == 'Stop') {
            $(this).attr('value', 'Run');
            setMonitorEnabled(true);
            // swarm.stop();
            if (window.animate) window.clearInterval(animate);
        }
    });
});