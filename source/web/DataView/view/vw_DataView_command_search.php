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
<script type="text/javascript" src="./js/DataView_command_search.js?var=20160613"></script>
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
					<span class="bold">�����^�O����</span>
					<div class="description">
						�����^�O��I�����Ă��������B<br/>
						<span class="red">�����^�O�I��</span>�����ʕ\��
					</div>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="nextButton center">�����J�n</p>
				</div>
				<div class="contents">
					<div id="lcontents" class="lcontents Wide32">
<?php
echo '<input type="hidden" id="pgName" name="pgName" value="'.basename($_SERVER['PHP_SELF']).'">';
?>
						�����^�O�ꗗ
						<div id="commandListHeader" class="commandListHeader">
							<div class="listHead commandListHead">
								<span>�����^�O��</span><span>�쐬��</span>
							</div>
						</div>
						<div>
							<div id="commandList" class="commandList">

<?php
$oddFlg = -1;
if(!empty($commands)){
	foreach($commands as $command){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="Command_Name bgChItems"><?php echo htmlEscape($command['Command_Name']); ?></span><span
									class="User_Id bgChItems"><?php echo htmlEscape($command['User_Id']); ?></span><span
									class="hidden rightChildLast id bgChItems"><?php echo htmlEscape($command['Command_Id']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>

							</div>
						</div>
						<img src="./img/btntbn06-2.png" id="commandDeleteButton" alt="�ナ�X�g�I���s���폜" width="" height="">
					</div>
				</div>
				<div class="transition">
					<p class="nextButton center">�����J�n</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
