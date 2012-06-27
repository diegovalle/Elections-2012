var dateFormatLegend = d3.time.format("%b-%d");
var dateFormat = d3.time.format("%b");

var percent = d3.format("1%");
var percent1 = d3.format(".1%");
var poll_date = [], kalman_date = [], poll_start = [], poll_end = [];
var forecast_date =[];

var election_day = new Date(2012,6,1);

var w = 500,
h = 600,
margin = 60,
y,x;
var hoverContainer, hoverLine, hoverLineXOffset, hoverLineYOffset, hoverLineGroup,
textDate, textEPN, textAMLO, textJVM, textGQT;
var vis = d3.select("#chart")
    .append("svg:svg")
    .attr("width", w)
    .attr("height", h);


var w_results = 300,
h_results = 400;

var yresults = d3.scale.linear().
    domain([0, 1]).
    range([0 + margin, h_results - margin]);
var xresults = d3.time.scale().
		domain([0, .55]).
		range([0 + margin, w_results - margin]);

var quants =[];
var amlo =[], epn = [], jvm = [], gqt = [], complete_date = [];

d3.json("js/json.txt", 
	function(data) {
	    
	    data.polls.date.forEach(function(d,i) {poll_date[i] = new Date(d);});
	    data.kalman.date.forEach(function(d,i) {kalman_date[i] = new Date(d);});

	    data.forecast.date.forEach(function(d,i) {forecast_date[i] = new Date(d);});
	    //data.polls.date.forEach(function(d,i) {d = new Date(d);});
	    //data.kalman.date.forEach(function(d,i) {d = new Date(d);});
	    data.polls.start.forEach(function(d,i) {poll_start[i] = new Date(d);});
	    data.polls.end.forEach(function(d,i) {poll_end[i] = new Date(d);});

	    data.kalman.epn.forEach(function(d,i) {epn[i] = d;});
	    data.forecast.epn.forEach(function(d,i) {epn[i+epn.length] = d;});

	    data.kalman.amlo.forEach(function(d,i) {amlo[i] = d;});
	    data.forecast.amlo.forEach(function(d,i) {amlo[i+amlo.length] = d;});

	    data.kalman.jvm.forEach(function(d,i) {jvm[i] = d;});
	    data.forecast.jvm.forEach(function(d,i) {jvm[i+jvm.length] = d;});

	    data.kalman.gqt.forEach(function(d,i) {gqt[i] = d;});
	    data.forecast.gqt.forEach(function(d,i) {gqt[i+gqt.length] = d;});

	    data.kalman.date.forEach(function(d,i) {complete_date[i] = new Date(d);});
	    data.forecast.date.forEach(function(d,i) {complete_date[i+complete_date.length] = new Date(d);});
	    quants = data.quants;

		
	    y = d3.scale.linear().
		domain([0, 
			d3.max(data.kalman.epn)+.07]).
		range([0 + margin, h - margin]),
	    x = d3.time.scale().
		domain([d3.min(kalman_date), 
			election_day]).
		range([0 + margin, w - margin]);

	    var g = vis.append("svg:g")
		.attr("transform", "translate(10, 600)");
			

	    function addFilter(g, color, candidateValues, dates) {
		var makeLine = d3.svg.line()
		    .x(function(d,i) { return x(kalman_date[i]); })
		    .y(function(d) { return -1 * y(d); })
		    .interpolate("linear");
	       
		g.append("svg:path")
		    .attr("d", makeLine(candidateValues))
                    .attr("stroke", color);
		return g;
	    }

	    addFilter(g, "red", data.kalman.epn, kalman_date);
	    addFilter(g, "#ffd217", data.kalman.amlo, kalman_date);
	    addFilter(g, "#00448b", data.kalman.jvm, kalman_date);
	    addFilter(g, "lightblue", data.kalman.gqt, kalman_date);
	    
	    function addForecast(g, color, candidateValues, dates) {
		var makeLine = d3.svg.line()
		    .x(function(d,i) { return x(forecast_date[i]); })
		    .y(function(d) { return -1 * y(d); })
		    .interpolate("linear");
		
		g.append("svg:path")
		   .attr("d", makeLine(candidateValues))
                    .attr("stroke", color)
		    .attr("stroke-dasharray", "4,4,4");
		return g;
	    }
	    addForecast(g, "red", data.forecast.epn, forecast_date);
	    addForecast(g, "#ffd217", data.forecast.amlo, forecast_date);
	    addForecast(g, "#00448b", data.forecast.jvm, forecast_date);
	    addForecast(g, "lightblue", data.forecast.gqt, forecast_date);

	   function addPoints(g, data, candidate, color) {
	       var name;
	       switch(candidate){
	       case "epn":
		   name="Enrique Peña Nieto (PRI-PVM)";
		   break;
	       case "amlo":
		   name="Andrés Manuel López Obrador (PRD-PT-MC)";
		   break;
	       case "jvm":
		   name="Josefina Vázquez Mota (PAN)";
		   break;
	       case "gqt":
		   name="Gabriel Quadri (PANAL)";
		   break;
	       }
	       g.selectAll("scatter-dots")
		   .data(data[candidate])
		   .enter().append("svg:circle")
		   .attr("cy", function (d,i) { return -1 * y(d); } )
		   .attr("cx", function (d,i) { return x(poll_date[i]); } )
		   .attr("r", 3.5)
		   .style("opacity", 0.8)
		   .style("fill", color)
	       .style("stroke", "#111")
		   .on("mouseover", function(d, i){
			   tooltip.html("<table class='tooltip2'><tr><th>" + name + " -  <big style='color:#CD5337;text-align:left'>" + percent1(d) + "</big></th></tr><tr><td>" + data.pollster[i] +"</td></tr><tr><td>" + "" + "</td></tr><tr><td>" + "Poll Start:" + "</td><td>" + dateFormatLegend(poll_start[i]) +"</td></tr><tr><td>" + "Poll End:" + "</td><td>" + dateFormatLegend(poll_end[i]) +"</td></tr><tr><td>" + "Sample Size:" + "</td><td>" + data.size[i] +"</td></tr></table>");
			   return tooltip.style("visibility", "visible");}).on("mousemove", function(){return tooltip.style("top", (d3.event.pageY-10)+"px").style("left",(d3.event.pageX+10)+"px");})
		   .on("mouseout", function(){return tooltip.style("visibility", "hidden");})
		   .append("svg:title");
	       return g;
	   }
	   var g2 = vis.append("svg:g")
		.attr("transform", "translate(10, 600)");	 

	    hoverLineGroup = vis.append("svg:g")
		.attr("class", "hover-line");
	    // add the line to the group
	    hoverLine = hoverLineGroup
		.append("svg:line")
		.style("stroke", "black")
		.style("opacity", 1)
		.attr("x1", 10).attr("x2", 10) // vertical line so same value on each
		.attr("y1", 0).attr("y2", h-60); // top to bottom

	   addPoints(g2, data.polls, "epn", "red");
	   addPoints(g2, data.polls, "amlo", "#ffd217");
	   addPoints(g2, data.polls, "jvm", "#00448b");
	   addPoints(g2, data.polls, "gqt", "lightblue");

	    //X axis line
	   g.append("svg:line")
	       .attr("x1", x(kalman_date[0]))
	       .attr("y1", -1 * y(0))
	       .attr("x2", x(d3.max(complete_date)))
	       .attr("y2", -1 * y(0));
	    //Y axis line
	   g.append("svg:line")
	       .attr("x1", x(kalman_date[0]))
	       .attr("y1", -1 * y(0))
	       .attr("x2", x(kalman_date[0]))
	       .attr("y2", -1 * y(d3.max(data.polls.epn)))
		.attr("stroke", "white");
	   
	   g.selectAll(".xLabel")
	       .data(x.ticks(5))
	       .enter().append("svg:text")
	       .attr("class", "xLabel")
	       .text(function(d) {return dateFormat(d); })
	       .attr("x", function(d,i) { return x(new Date(d)); })
	       .attr("y", -38)
	       .attr("text-anchor", "middle");

	   g.selectAll(".yLabel")
	       .data(y.ticks(5))
	       .enter().append("svg:text")
	       .attr("class", "yLabel")
	       .text(function(d) {return percent(d);})
	       .attr("x", 27)
	       .attr("y", function(d) { return -1 * y(d); })
	       .attr("text-anchor", "right")
	       .attr("dy", 4);
	   
	   g.selectAll(".xTicks")
	       .data(x.ticks(4))
	       .enter().append("svg:line")
	       .attr("class", "xTicks")
	       .attr("x1", function(d,i) { return x(d); })
	       .attr("y1", -1 * y(0))
	       .attr("x2", function(d,i) { return x(d); })
	       .attr("y2", -1 * y(-0.010));
	   
	   g.selectAll(".yTicks")
	       .data(y.ticks(5))
	       .enter().append("svg:line")
	       .attr("class", "yTicks")
	       .attr("y1", function(d) { return -1 * y(d); })
	       .attr("x1", x(kalman_date[0]))
	       .attr("y2", function(d) { return -1 * y(d); })
	       .attr("x2", x(kalman_date[0])-5);

	    
		
	    textDate = vis.append("svg:g")
		.attr("class", "candidate-text")
		.append("svg:text")
		.attr("x", 30)
		.attr("y", 50)
		.attr("text-anchor", "middle") 
		.attr("stroke", "steelblue")
		.attr("fill", "#1f77b4")
		.style("font-size", 13)
		.style("font-style", "normal");

	    textEPN = vis.append("svg:g")
		.attr("class", "candidate-text")
		.append("svg:text")
		.attr("x", 30)
		.attr("y", 5)
		.attr("text-anchor", "middle") 
		;
	    textAMLO = vis.append("svg:g")
		.attr("class", "candidate-text")
		.append("svg:text")
		.attr("x", 30)
		.attr("y", 50)
		.attr("text-anchor", "middle") 
		;
	    textJVM = vis.append("svg:g")
		.attr("class", "candidate-text")
		.append("svg:text")
		.attr("x", 30)
		.attr("y", 50)
		.attr("text-anchor", "middle") 
		;
	    
	    // hide it by default
	    hoverLine.classed("hide", true);

	    textGQT = vis.append("svg:g")
		.attr("class", "candidate-text")
		.append("svg:text")
		.attr("x", 30)
		.attr("y", 50)
		.attr("text-anchor", "middle") 
		;

	    ////textBackground = vis.append("svg:g")
	//	.append("svg:rect")
	//	.attr("width", xresults(upper) - xresults(lower))
	//	.attr("height", 20)
	//	.attr("y", function () { return yresults(x); } )
	//	.attr("x", function () { return xresults(lower); } );

	    setResults();

});

var tooltip = d3.select("#chart")
    .append("div")
    .attr("style", "background:black;color:white;border-radius: 5px;padding:15px;border-color:transparent;")
    .style("position", "absolute")
    .style("z-index", "1000")
    .style("visibility", "hidden");

var handleMouseOverGraph = function(event) {
    function addText(pre, g, array, y, asDate){
	var index;
	for(index = 0; index < complete_date.length; index++){
	    if(x(complete_date[index]) >= mouseX+20)
		break;
	}
	if (index < complete_date.length & index >= 0) {
	    
	//var index = Math.round(mouseX/((w-margin*2)/134));
	    var lineText = array[index];
	    if(asDate)
		lineText = dateFormatLegend(lineText);
	    else
		lineText = percent(lineText);
	
	    g.attr("x", mouseX-10)
		.attr("y", y)
		.text(function(){
			  return pre + lineText;
		      });
	
	}
    }
		var mouseX = event.pageX-hoverLineXOffset-30;
		var mouseY = event.pageY-hoverLineYOffset;
		
		//debug("MouseOver graph [" + containerId + "] => x: " + mouseX + " y: " + mouseY + "  height: " + h + " event.clientY: " + event.clientY + " offsetY: " + event.offsetY + " pageY: " + event.pageY + " hoverLineYOffset: " + hoverLineYOffset)
		if(mouseX >= 40 && mouseX <= (w-80) && mouseY >= 0 && mouseY <= h) {
			// show the hover line
			hoverLine.classed("hide", false);
		    textEPN.classed("hide", false);
		    textDate.classed("hide", false);
		    textAMLO.classed("hide", false);
		    textJVM.classed("hide", false);
		    textGQT.classed("hide", false);
			// set position of hoverLine
			hoverLine.attr("x1", mouseX+31)
			.attr("x2", mouseX+31);

		    addText("", textDate, complete_date, 10, true);		   
		    addText("EPN: ", textEPN, epn, 25, false);
		    addText("AMLO: ", textAMLO, amlo, 40, false);
		    addText("JVM: ", textJVM, jvm, 55, false);
		    addText("GQT: ", textGQT, gqt, 70, false);
//+"\n"+ epn[index]
			//vis.append("svg:text")
			//.attr("x", function() {return mouseX;})
			//.attr("y", y(mouseY))
			//.attr("text-anchor", "middle") 
			//.text(function(d,i) {return "cat";});
			//$("#candidates").html("AMLO: ");
			//displayValueLabelsForPositionX(mouseX);
			
			// user is interacting
			userCurrentlyInteracting = true;
			currentUserPositionX = mouseX+30;
		} else {
			// proactively act as if we've left the area since we're out of the bounds we want
			handleMouseOutGraph(event);
		}
	};
	
	
var handleMouseOutGraph = function(event) {	
    // hide the hover-line
    hoverLine.classed("hide", true);
    textEPN.classed("hide", true);
    textDate.classed("hide", true);
    textAMLO.classed("hide", true);
    textJVM.classed("hide", true);
    textGQT.classed("hide", true);
    $("#candidates").html("");
    //setValueLabelsToLatest();
    
    //debug("MouseOut graph [" + containerId + "] => " + mouseX + ", " + mouseY)
    //mouseX
    // user is no longer interacting
    userCurrentlyInteracting = false;
    currentUserPositionX = -1;
};

$("#chart").mouseleave(function(event) {
			   handleMouseOutGraph(event);
		       });

$("#chart").mousemove(function(event) {
			  handleMouseOverGraph(event);
		      });	

var initDimensions = function() {
		// automatically size to the container using JQuery to get width/height
	//	w = $("#" + containerId).width() - margin[1] - margin[3]; // width
	//	h = $("#" + containerId).height() - margin[0] - margin[2]; // height
		
		// make sure to use offset() and not position() as we want it relative to the document, not its parent
		hoverLineXOffset = $("#chart").offset().left;
		hoverLineYOffset = $("#chart").offset().top;
	};

var TO = false;
$(window).resize(function(){
		     if(TO !== false)
		    	 clearTimeout(TO);
		     TO = setTimeout(handleWindowResizeEvent, 200); //200 is time in miliseconds
		 });

var handleWindowResizeEvent = function() {
	 	//debug("Window Resize Event [" + containerId + "] => resizing graph")
	 	initDimensions();
}
initDimensions();


var can = d3.select("#results")
    .append("svg:svg")
    .attr("width", w_results)
    .attr("height", h_results);
function candidateInterval(g, lower, upper, x) {
    g.append("svg:g")
	.append("svg:rect")
	.attr("width", xresults(upper) - xresults(lower))
        .attr("height", 20)
	.attr("y", function () { return yresults(x); } )
	.attr("x", function () { return xresults(lower); } )
	.attr("r", 20)
	.style("opacity", 0.8)
	.style("fill", "red");
};
function candidateMean(g, mean, x) {
    g.append("svg:g")
.append("svg:line")
          .attr("class", "marker")
          .attr("x1", x)
          .attr("x2", x)
          .attr("y1", xresults(mean))
          .attr("y2", xresults(mean));
};

function drawResults() {
    
    candidateInterval(can, quants.epn[0], quants.epn[1], .2);
    candidateInterval(can, quants.amlo[0], quants.amlo[1], .5);
    candidateInterval(can, quants.jvm[0], quants.jvm[1], .7);
    candidateInterval(can, quants.gqt[0], quants.gqt[1], .9);
    
    
    candidateMean(can, quants.epn[2], .2);
    candidateMean(can, quants.amlo[2], .5);
    candidateMean(can, quants.jvm[2], .7);
    candidateMean(can, quants.gqt[2], .9);
}

function setResults() {
    function createText(candidate, array){
	var name;
	switch(candidate){
	       case "epn":
		   name="Enrique Peña Nieto:<br/> ";
		   break;
	       case "amlo":
		   name="Andrés Manuel López Obrador:<br/> ";
		   break;
	       case "jvm":
		   name="Josefina Vázquez Mota:<br/> ";
		   break;
	       case "gqt":
		   name="Gabriel Quadri:<br/> ";
		   break;
	       }
        name = "<h3>" + name + "</h3>";
	$("#" + candidate).html(percent1(array[2]) + " (95% CI " + percent1(array[0]) + ", " + percent1(array[1]) + ")");

    }
createText("amlo", quants.amlo);
createText("epn", quants.epn);
createText("jvm", quants.jvm);
createText("gqt", quants.gqt);
   
}