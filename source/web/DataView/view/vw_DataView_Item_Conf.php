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
					<span class="bold">和名設定</span>
					<div class="description">
						項目名に対する和名の設定を行ってください。<br/>
					</div>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
			</div>
			<div class="contents">
				<div id="lcontents" class="lcontents itemTable">
					<span>項目一覧</span>
					<div id="leftListHeader" class="leftListHeader">
						<div class="listHead">
							<span class="listTableName">テーブル名</span><span
							class="listItemName">項目名</span><span
							class="listJapaneseName">和名</span><span
							class="listDisp">表示</span><span
							class="listS_point">開始位置</span><span
							class="listSize">サイズ</span><span
							class="listType">型</span>
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
						<img src="./img/btntbn06-3.png" alt="和名更新" class="modalOpenButton" width="" height="" />
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
				<p>和名と表示設定を入力してください。</p>
				<div class="form_input">
					<span class="form_label">和名</span><span class="form_sep">:</span>
					<input type="text" id="Japanese_Name" name="Japanese_Name" value="">
				</div>
				<div class="form_input">
					<span class="form_label">表示/非表示</span><span class="form_sep">:</span>
					<input type="radio" id="NonDisp_Flg_no" name="NonDisp_Flg" value="0">
					<label for="NonDisp_Flg_no">表示</label>
					<input type="radio" id="NonDisp_Flg_yes" name="NonDisp_Flg" value="1">
					<label for="NonDisp_Flg_yes">非表示</label>
				</div>
			</form>
			<div class="modalButtonArea">
				<p class="modalCloseButton center">閉じる</p>
				<p class="itemUpdateButton center">更新</p>
				<div id="errMessageModal" class="errMessageModal"></div>
			</div>
		</div>
	</div>
</body>
</html>
