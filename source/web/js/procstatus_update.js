var procStatusUpdate = function () {
	var procStatusUpdateTime;
	var procStatusUpdateTimeoutTime = '1000';

	function changeStatusFromAjax(responceText){
		let targElement = $('.screen')[0];
		let changeObj = document.createElement('div');
		changeObj.innerHTML = responceText;
		//changeObjの中からstatus2Getを消す
		if($(changeObj).find('#status2').length > 0){
			$("#status2").html( "<span>" + $(changeObj).find('#status2')[0].value + "</span>");
			$(changeObj).find('#status2').remove();
		}

		//changeObjの中からstatus4Getを消す
		if($(changeObj).find('#status4').length > 0){
			$("#status4").html( "<span>" + $(changeObj).find('#status4')[0].value + "</span>");
			$(changeObj).find('#status4').remove();
		}
	}
	function loadStatusList(){
		let stpid = document.getElementById("pid").value;
		$.ajax({
			type: "POST",
			url: "proc_status.php",
			data:{ pid:stpid },
			success: function(msg,txt){
				changeStatusFromAjax(msg);
				procStatusUpdateTime = setTimeout(loadStatusList, procStatusUpdateTimeoutTime);
			},
			error: function(jqXHR,textStatus,errorThrown ){
				if(errorThrown){
					console.log("Status connect Error"+":" + errorThrown.message);
				}
				procStatusUpdateTime = setTimeout(loadStatusList, procStatusUpdateTimeoutTime);
			}
		});
	}
	procStatusUpdateTime = setTimeout(loadStatusList, procStatusUpdateTimeoutTime);
};
addEvent('load',window,procStatusUpdate);
