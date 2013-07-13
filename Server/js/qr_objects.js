function CircleObj(id, paper, bg, x, y){
	// Store reference to "this" pointer to use in other scopes.
	var _obj = this;
	this.id = id;

	// Create object in Raphael
	var circle = paper.circle(x, y, 50);
	circle.attr({fill:"#fff",stroke:"#333"});
	var label = paper.text(x, y, prompt("Label"));

	// Object is selected or not
	var selected = false;

	// Change to selected state
	this.select = function(){
		if(bg.data("selection") == undefined){
			circle.attr({"stroke-width":3});
			bg.data("selection",1);
			selected = true;
		}else{
			// Make connection between "this" and bg.data("selection") 
			this.unselect();
		}
	};

	// Change to unselected state
	this.unselect = function(){
		circle.attr({"stroke-width":1});
		//bg.removeData("selection");
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
		this.remote_update();
	}

	// Update remote RDF storage
	this.remote_update = function(){
		xhr = new XMLHttpRequest();
		xhr.open("POST","/circle/"+this.id, true)
		xhr.onreadystatechange = function(){}
		xhr.send({method:"edit",name:label.attr("text")});
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
}
