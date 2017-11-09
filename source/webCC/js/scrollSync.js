var onMouseElemName = '';
var scrollSync = function (){
	var switchFlg = 0;
	var centralScroll = function(event){
		if(event.target.className == onMouseElemName){
			var TopElem = $('.tableR.h_scrl')[0];
			var LeftElem = $('.tableL.v_scrl')[0];
			var centralElem = $('.h_scrl.v_scrl')[0];

			// remEvent('scroll',LeftElem, vhScroll);
			// remEvent('scroll',TopElem, vhScroll);
			LeftElem.scrollTop = centralElem.scrollTop;
			TopElem.scrollLeft = centralElem.scrollLeft;
			// addEvent('scroll',LeftElem, vhScroll);
			// addEvent('scroll',TopElem, vhScroll);
		}
	}

	var vhScroll = function (event){
		if(event.target.className == onMouseElemName){
			var TopElem = $('.tableR.h_scrl')[0];
			var LeftElem = $('.tableL.v_scrl')[0];
			var centralElem = $('.h_scrl.v_scrl')[0];
//
			// remEvent('scroll',centralElem, centralScroll);
			centralElem.scrollTop  = LeftElem.scrollTop;
			centralElem.scrollLeft = TopElem.scrollLeft ;
			// addEvent('scroll',centralElem, centralScroll);
		}
	}
	var setMouseOverTargetName = function (event){
		onMouseElemName = event.currentTarget.className;
	}

	var targElem = $('.h_scrl.v_scrl')[0];
	addEvent('scroll',targElem, centralScroll);
	addEvent('mouseover',targElem, setMouseOverTargetName);

	targElem = $('.tableL.v_scrl')[0];
	addEvent('scroll',targElem, vhScroll);
	addEvent('mouseover',targElem, setMouseOverTargetName);

	targElem = $('.tableR.h_scrl')[0];
	addEvent('scroll',targElem, vhScroll);
	addEvent('mouseover',targElem, setMouseOverTargetName);
}

addEvent('load',window,scrollSync);
