var submitButton = function (){
	var submit = function(event){
		$("#action").val(event.target.id);
		if(event.target.id != "download" )
		{
			$('.button').attr('disabled', true);
		}
		document.frm.submit();
	}
	
	var elems = $('.submit');
	for(var i = 0;i < elems.length;i++){
		addEvent('click' , elems[i],
			function(event){
				return submit( event );
			}
		);
	}
}
addEvent('load',window,submitButton);