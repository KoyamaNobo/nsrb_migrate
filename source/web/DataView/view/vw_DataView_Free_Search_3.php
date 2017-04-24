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
<script type="text/javascript" src="./js/DataView_free_search_3.js?var=20160613"></script>
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
					<span class="bold">自由検索</span>
					<div class="description">
						表示項目を選択してください。<br/>
						テーブル選択＞結合条件設定＞<span class="red">表示項目選択</span>＞抽出条件設定＞並べ替え設定＞結果表示
					</div>
				</div>
				<div class="summary">

<?php
require_once('./view/vw_Part_DataView_summaryTables.php');
require_once('./view/vw_Part_DataView_summaryBonds.php');
?>

				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
						<p class="backButton DataView_free_search_2 center">戻る</p>
						<p class="nextButton center">次へ</p>
				</div>
				<div class="contents">
					<div id="lcontents" class="lcontents lFreeSearchTable">
						<div class="leftListDetail">
							<div class="leftListDetailleft">
								<p>
									<span>テーブル</span>
									<select id="lTableName" class="selectTableName">
										<option value=""></option>
<?php
if(!empty($selectedTables)){
	foreach($selectedTables as $selectedTable){
?>
										<option value=<?php echo '"'.$selectedTable.'"' ?>><?php echo $selectedTable ?></option>
<?php
	}
}
?>
									</select>
								</p>
								<div>候補一覧</div>
							</div>
							<div id="lCandidate_Add" class="lCandidate_Add"></div>
						</div>
						<div id="leftListHeader"  class="leftListHeader">
							<div class="listHead">
								<span class="listItemName">項目名</span><span
								class="listJapaneseName">和名</span><span
								class="listS_point center">開始位置</span><span
								class="listSize center">サイズ</span>
							</div>
						</div>

						<div>
							<div id="leftList"  class="leftList">
							</div>
						</div>
						<input type=hidden id="lStartNum" value="0">
					</div>
					<div id="ccontents" class="ccontents">
						<div>
							<img src="./img/btntbn04-1.png" id="dispAddButton" alt="右リストへ追加" class="verticallist">
							<img src="./img/btntbn04-2.png" id="dispDelButton" alt="右リスト選択行を削除" class="verticallist">

						</div>
					</div>
					<div id="rcontents" class="rcontents Wide48 free_search_3">
						<span>表示項目</span>
						<div id="rightListHeader" class="rightListHeader">
							<div class="listHead">
								<span class="listTableName">テーブル名</span><span
								class="listItemName">項目名</span><span
								class="listJapaneseName">和名</span><span
								class="listS_point">開始位置</span><span
								class="listSize">サイズ</span>
							</div>
						</div>
						<div>
							<div id="rightList" class="rightList">


<?php
$oddFlg = -1;
if(!empty($selectedItemNames)){
	foreach($selectedItemNames as $selectedItemName){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listTableName bgChItems" title="型 : <?php echo htmlEscape($selectedItemName['listType']); ?>"><?php echo htmlEscape($selectedItemName['listTableName']); ?></span><span
									class="listItemName bgChItems" title="型 : <?php echo htmlEscape($selectedItemName['listType']); ?>"><?php echo htmlEscape($selectedItemName['listItemName']); ?></span><span
									class="listJapaneseName bgChItems" title="型 : <?php echo htmlEscape($selectedItemName['listType']); ?>"><?php echo htmlEscape($selectedItemName['listJapaneseName']); ?></span><span
									class="hidden listId bgChItems"><?php echo htmlEscape($selectedItemName['listId']); ?></span><span
									class="listS_point right bgChItems" title="型 : <?php echo htmlEscape($selectedItemName['listType']); ?>"><?php echo htmlEscape($selectedItemName['listS_point']); ?></span><span
									class="listSize right bgChItems" title="型 : <?php echo htmlEscape($selectedItemName['listType']); ?>"><?php echo htmlEscape($selectedItemName['listSize']); ?></span><span
									class="hidden listType bgChItems"><?php echo htmlEscape($selectedItemName['listType']); ?></span><span
									class="hidden rightChildLast id bgChItems"><?php echo htmlEscape($selectedItemName['id']); ?></span>
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
					<p class="backButton DataView_free_search_2 center">戻る</p>
					<p class="nextButton center">次へ</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
