<?php
//header("Content-type: text/html; charset=SJIS");
//header("Cache-Control: Private");
?>
<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_disp.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_backButton.js?var=20160613"></script>
</head>
<body>
	<div class="contentbody">
		<div class="hblock">
			DataView
		</div>
		<div class="mblock">
<?php
require_once('./view/vw_Part_DataView_LoginInfo.php');
require_once('./view/vw_Part_DataView_ConstMessage.php');
require_once('./view/vw_Part_DataView_Menu.php');
?>

			<div class="main">
				<div class="navigate">
<?php
if(!empty($selectedCommandId)){
	echo '<span class="bold">�����^�O����</span>';
}else{
	echo '<span class="bold">���R����</span>';
}
?>
					<div class="description">
<?php
if(!empty($selectedCommandId)){
	echo '�����^�O�I����<span class="red">���ʕ\��</span>';
}else{
	echo '�e�[�u���I�������������ݒ聄�\�����ڑI�������o�����ݒ聄���בւ��ݒ聄<span class="red">���ʕ\��</span>';
}
?>
					</div>
				</div>
				<div class="summary">
<?php
echo '<input type="hidden" id="prePgName" name="prePgName" value="'.$prePgName.'">';
echo '<input type="hidden" id="pgName" name="pgName" value="DataView_disp.php">';
echo '<input type="hidden" id="Sql_str" name="Sql_str" value="'.htmlspecialchars($sql,ENT_COMPAT ,'SJIS').'">';
echo '<input type="hidden" id="PID" name="PID" value="">';
?>
<?php
if(!empty($selectedCommandId)){
	echo '<div class=summaryTitle>�I�������^�O</div>';
	echo '<div class="descriptionCommand">';
	echo htmlEscape($selectedCommandName);
	echo '<input type="hidden" id="selectedCommandId" name="selectedCommandId" value="'.$selectedCommandId.'">';
	echo '</div>';
}else{
	require_once('./view/vw_Part_DataView_summaryTables.php');
	require_once('./view/vw_Part_DataView_summaryBonds.php');
	require_once('./view/vw_Part_DataView_summaryItemNames.php');
	require_once('./view/vw_Part_DataView_summaryFilters.php');
	require_once('./view/vw_Part_DataView_summarySorts.php');
}
?>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="backButton <?php echo basename($prePgName,".php") ?> center">�߂�</p>
<?php
if(!empty($getDatas)){
?>
					<p class="csvButton center">CSV�쐬</p>
<?php
}
?>
<?php
if(empty($selectedCommandId)){
?>
					<p class="modalOpenButton center">�����^�O�ۑ�</p><span id="massage2"></span>
<?php
}
?>
				</div>
				<div class="contents">
<?php
if(!empty($getDatas)){
?>
	<p>���݂̃y�[�W�́u<?php echo $genzai_page;?>�v�ł��B</p>
	<p><?php echo ($start+1);?>���ڂ���<?php echo (($start)+$hyouji_kazu);?>���ڂ̃f�[�^��\�����Ă��܂��B</p>
	<div class="pagenation">
		<input type="hidden" id="Output_file" name="Output_file" value="'.$outputfile.'">
		<span class="link page=1 pageing ">[�ŏ���]</span>
<?php
	//�O�̃y�[�W�\��
	for ($i = 0; $i < PAGINUM; $i++) {
		$tmp = $genzai_page - (PAGINUM - $i);
		if($tmp > 0){
			echo '<span class="link page='.$tmp.' pageing ">['.$tmp.']</span> ';
		}else{
			echo '�@';
		}
	}

	echo '<span class="link page='.$genzai_page.' pageing2 ">['.$genzai_page.']</span> ';

	//���̃y�[�W�\��
	foreach($NextPage as $next){
		echo '<span class="link page='.$next.' pageing ">['.$next.']</span> ';
	}
?>
	</div>
<?php
	$oddFlg = -1;
	$roopCount = 0;
	$kensu = $start+1;
	if(count($getDatas) > 0){
		echo '<div class="tableStyle">';
		echo '<table class="table">';
		foreach($getDatas as $getData){
			if($roopCount == 0){
				echo '<tr class="header">';
				echo '<th class="header recordNum" nowrap>No</th>';
				foreach ($getData as $key => $value){
					echo '<th nowrap class="header">'.$key.'</th>';
				}
				echo '</tr>';
			}
			$tmp = '';
			if($oddFlg == 1){
				$tmp ='odd';
			}
			echo '<tr class="'.$tmp.'">';
			echo '<td class="recordNum" nowrap >'.$kensu.'</td>';
			foreach ($getData as $key => $value){
				echo '<td nowrap >'.htmlEscape($value).'</td>';
			}
			echo '</tr>';
			$oddFlg = $oddFlg * -1;
			$roopCount++;
			$kensu++;
		}
		echo '</table>';
		echo '</div>';
	}
}else{
	echo '<div class="tableStyle">';
	echo '�Y���f�[�^�Ȃ��B';
	echo '</div>';
}

?>
				</div>
				<div class="transition">
					<p class="backButton <?php echo basename($prePgName,".php") ?> center">�߂�</p>
<?php
if(!empty($getDatas)){
?>
					<p class="csvButton center">CSV�쐬</p>
<?php
}
?>
<?php
if(empty($selectedCommandId)){
?>
					<p class="modalOpenButton center">�����^�O�ۑ�</p><span id="massage2"></span>
<?php
}
?>
				</div>
			</div>
		</div>
	</div>
	<div id="modal">
		<div id="modalBody" class="Height150">
			<div id="modalHeader">&nbsp;</div>
			<div>
				�ۑ����閼�O����͂��Ă��������B
				<input type="text" id="Command_Name" name="Command_Name" value=""><br/>
			</div>
			<div>
				<p class="modalCloseButton center">����</p>
				<p class="signButton center">�o�^</p>
			</div>
			<div id="errMessageModal" class="errMessageModal"></div>
		</div>
	</div>
	<div id="DLmodal">
		<div id="DLmodalBody" class="Height150">
			<div id="DLmodalHeader">&nbsp;</div>
			<div>
				<div id="DLmodalInfo">CSV�t�@�C���������ݒ��ł��B<br/>�������݊�����Ƀ_�E�����[�h���J�n���܂��B</div>
				<div id="DLmodalWritingCount"></div>���������ݒ�
			</div>
		</div>
	</div>
</body>
</html>
