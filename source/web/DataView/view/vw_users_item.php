<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?ver=20160708"></script>
<script type="text/javascript" src="./js/DataView_UserItem.js?ver=20160708"></script>
<script type="text/javascript" src="./js/item_conf_listadd.js?ver=20160620"></script>
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
					<span class="bold">���ڐݒ�</span>
					<div class="description">
						���ڂ̐ݒ���s���Ă��������B<br/>
					</div>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
			</div>
			<div class="contents">
				<div id="lcontents" class="lcontents userItemTable">
					���ڈꗗ
					<div id="leftListHeader" class="leftListHeader">
						<div class="listHead">
							<span class="listTableName">�e�[�u����</span><span
							class="listJapaneseName">���ږ�</span><span
							class="listS_point">�J�n�ʒu</span><span
							class="listSize">�T�C�Y</span><span
							class="listType">�^</span><span
							class="listDispPri">���J�͈�</span><span
							class="User_Id">�쐬��</span>
						</div>
					</div>
					<div>
						<div id="leftList" class="leftList">
							<input type="hidden" id="page_num" name="page_num" class="on" value="1" />
<?php
$oddFlg = -1;
if(!empty($clsData->tableData)){
	foreach($clsData->tableData as $item){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listExtension hidden bgChItems"><?php echo $item->getExtension_id(); ?></span><span
									class="listTableName bgChItems"><?php echo $item->getTablename(); ?></span><span
									class="listJapaneseName bgChItems"><?php echo  $item->getItem_japanese_name(); ?></span><span
									class="listS_point right bgChItems"><?php echo $item->getS_point(); ?></span><span
									class="listSize right bgChItems"><?php echo $item->getSize(); ?></span><span
									class="listType bgChItems"><?php echo $item->getData_type(); ?></span><span
									class="listDispPri bgChItems"><?php echo $item->getDisp_flg(); ?></span><span
									class="User_Id bgChItems"><?php echo $item->getUser_id(); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>
						</div>
					</div>
					<div>
						<img src="./img/btntbn06-1.png" alt="�ǉ�" id="addButton" class="modalOpenButton modalAdd" width="" height="">
						<img src="./img/btntbn06-3.png" alt="�X�V" class="modalOpenButton modalUpdate" width="" height="">
						<img src="./img/btntbn06-2.png" alt="�I���s���폜" id="delButton"  class="delButton" width="" height="">
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
		<div id="modalBody" class="Height500">
			<div id="modalHeader">&nbsp;</div>
			<form id="modalDetail">
				<div class="form_input hidden">
					<span class="form_label">�g������ID</span><span class="form_sep">:</span>
					<input type="text" size="10" id="Extension_Id" name="Extension_Id" value="">
				</div>
				<div class="form_input">
					<span class="form_label">�e�[�u����</span><span class="form_sep">:</span>
					<input type="text" size="100" id="Table_Name" name="Table_Name" value="">
				</div>
				<div class="form_input">
					<span class="form_label">���ږ�</span><span class="form_sep">:</span>
					<input type="text" size="100" id="Item_Japanese_Name" name="Item_Japanese_Name" value="">
				</div>
				<div class="form_input">
					<span class="form_label">�J�n�ʒu</span><span class="form_sep">:</span>
					<input type="text" size="3" id="S_point" name="S_point" value="">
				</div>
				<div class="form_input">
					<span class="form_label">�T�C�Y</span><span class="form_sep">:</span>
					<input type="text" size="3" id="Size" name="Size" value="">
				</div>
				<div class="form_input">
					<span class="form_label">�^</span><span class="form_sep">:</span>
					<input type="text" size="10" id="Data_Type" name="Data_Type"
					data-text="����:9,�����t��:S9,COMP-3:C9,����:X,���{��:N" value="">
				</div>
				<div class="form_input hidden">
					<span class="form_label">�쐬��</span><span class="form_sep">:</span>
					<input type="text" size="20" id="User_Id" name="User_Id" value="">
				</div>
				<div class="form_input">
					<span class="form_label">���J�͈�</span><span class="form_sep">:</span>
					<input type="radio" id="NonDisp_Flg_none" name="NonDisp_Flg" value="0" checked="checked">
					<label>�����̂�</label>
					<input type="radio" id="NonDisp_Flg_yes" name="NonDisp_Flg" value="1">
					<label>���l��</label>
				</div>
			</form>
			<div  id="modalButton">
				<p class="modalCloseButton center">����</p>
				<p class="modeButton center">�X�V</p>
				<div id="errMessageModal" class="errMessageModal"></div>
			</div>
		</div>
	</div>
</body>
</html>
