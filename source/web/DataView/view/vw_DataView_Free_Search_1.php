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
<script type="text/javascript" src="./js/DataView_free_search_1.js?var=20160613"></script>
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
					<span class="bold">���R����</span>
					<div class="description">
						�e�[�u����I�����Ă��������B<br/>
						<span class="red">�e�[�u���I��</span>�����������ݒ聄�\�����ڑI�������o�����ݒ聄���בւ��ݒ聄���ʕ\��
					</div>
				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="nextButton center">����</p>
				</div>
				<div class="contents">
					<div id="lcontents" class="lcontents Wide13">
						�e�[�u���ꗗ
						<div id="leftListHeader"  class="leftListHeader">
							<div class="listHead">
								<span class="listTableName" >�e�[�u����</span>
								<!-- <span class="hidden listId">id</span> -->
							</div>
						</div>

						<div>
							<div id="leftList"  class="leftList" tabIndex="1">
<?php
$oddFlg = -1;
if(!empty($tables)){
	foreach($tables as $table){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span>
									<span class="listTableName bgChItems"><?php echo htmlEscape($table['TableName']); ?></span>
									<span class="hidden listId bgChItems"><?php echo htmlEscape($table['TableName']); ?></span>
									<span class="hidden id"><?php echo htmlEscape($table['TableName']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>
							</div>
						</div>
					</div>
					<div id="ccontents" class="ccontents">
						<div>
							<img src="./img/btntbn04-1.png" id="tableAddButton" alt="�E���X�g�֒ǉ�" class="verticallist">
							<img src="./img/btntbn04-2.png" id="tableDelButton" alt="�E���X�g�I���s���폜" class="verticallist">

						</div>
					</div>
					<div id="rcontents" class="rcontents Wide13">
						�I���e�[�u��
						<div id="rightListHeader" class="rightListHeader">
							<div class="listHead">
								<span class="listTableName">�e�[�u����</span>
							</div>
						</div>
						<div>
							<div id="rightList" class="rightList">
<?php
$oddFlg = -1;
if(!empty($selectedTables)){
	foreach($selectedTables as $selectedTable){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span>
									<span class="listTableName bgChItems"><?php echo htmlEscape($selectedTable); ?></span>
									<span class="hidden listId bgChItems"><?php echo htmlEscape($selectedTable); ?></span>
									<span class="hidden rightListLast id"><?php echo htmlEscape($selectedTable); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>

							</div>
						</div>
					</div>
				</div>
				<div class="transition">
					<p class="nextButton center">����</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
