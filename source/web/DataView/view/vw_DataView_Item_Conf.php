<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_ItemConf.js?var=20160613"></script>
<script type="text/javascript" src="./js/item_conf_listadd.js?var=20160620"></script>
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
				<input type="hidden" id="pgName" name="pgName" value="DataView_item_conf.php">
				<div class="navigate">
					<span class="bold">�a���ݒ�</span>
					<div class="description">
						���ږ��ɑ΂���a���̐ݒ���s���Ă��������B<br/>
					</div>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
			</div>
			<div class="contents">
				<div id="lcontents" class="lcontents itemTable">
					<span>���ڈꗗ</span>
					<div id="leftListHeader" class="leftListHeader">
						<div class="listHead">
							<span class="listTableName">�e�[�u����</span><span
							class="listItemName">���ږ�</span><span
							class="listJapaneseName">�a��</span><span
							class="listDisp">�\��</span><span
							class="listS_point">�J�n�ʒu</span><span
							class="listSize">�T�C�Y</span><span
							class="listType">�^</span>
						</div>
					</div>
					<div>
						<div id="leftList" class="leftList">
							<input type="hidden" id="page_num" name="page_num" class="on" value="1" />
<?php
$oddFlg = -1;
if(!empty($items)){
	foreach($items as $item){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listTableName bgChItems"><?php echo htmlEscape($item['TableName']); ?></span><span
									class="listItemName bgChItems"><?php echo htmlEscape($item['Label']); ?></span><span
									class="listJapaneseName bgChItems"><?php echo  htmlEscape($item['Japanese_Name']); ?></span><span
									class="listDisp bgChItems"><?php echo htmlEscape($item['NonDisp_Flg']) ?></span><span
									class="listS_point right bgChItems"><?php echo htmlEscape($item['S_Point']); ?></span><span
									class="listSize right bgChItems"><?php echo htmlEscape($item['Size']); ?></span><span
									class="listType bgChItems"><?php echo htmlEscape($item['DataType']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>
						</div>
					</div>
					<div>
						<img src="./img/btntbn06-3.png" alt="�a���X�V" class="modalOpenButton" width="" height="" />
					</div>
				</div>
				<div id="ccontents" class="ccontents">
				</div>
				<div id="rcontents" class="rcontents Wide28">
				</div>
			</div>
		</div>
	</div>
	<div id="modal">
		<div id="modalBody">
			<div id="modalHeader">&nbsp;</div>
			<form>
				<p>�a���ƕ\���ݒ����͂��Ă��������B</p>
				<div class="form_input">
					<span class="form_label">�a��</span><span class="form_sep">:</span>
					<input type="text" id="Japanese_Name" name="Japanese_Name" value="">
				</div>
				<div class="form_input">
					<span class="form_label">�\��/��\��</span><span class="form_sep">:</span>
					<input type="radio" id="NonDisp_Flg_no" name="NonDisp_Flg" value="0">
					<label for="NonDisp_Flg_no">�\��</label>
					<input type="radio" id="NonDisp_Flg_yes" name="NonDisp_Flg" value="1">
					<label for="NonDisp_Flg_yes">��\��</label>
				</div>
			</form>
			<div class="modalButtonArea">
				<p class="modalCloseButton center">����</p>
				<p class="itemUpdateButton center">�X�V</p>
				<div id="errMessageModal" class="errMessageModal"></div>
			</div>
		</div>
	</div>
</body>
</html>
