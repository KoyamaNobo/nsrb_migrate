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
<script type="text/javascript" src="./js/DataView_free_search_6.js?var=20160613"></script>
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
						データの並びを設定してください。<br/>
						テーブル選択＞結合条件設定＞表示項目選択＞抽出条件設定＞<span class="red">並べ替え設定</span>＞結果表示
					</div>
				</div>
				<div class="summary">
<?php
echo '<input type="hidden" id="pgName" name="pgName" value="'.basename($_SERVER['PHP_SELF']).'">';
?>


<?php
require_once('./view/vw_Part_DataView_summaryTables.php');
require_once('./view/vw_Part_DataView_summaryBonds.php');
require_once('./view/vw_Part_DataView_summaryItemNames.php');
require_once('./view/vw_Part_DataView_summaryFilters.php');

?>

				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="backButton DataView_free_search_4 center">戻る</p>
					<p class="nextButton center">検索開始</p>
				</div>
				<div class="contents">
					<div id="lcontents" class="lcontents Wide48">
						候補一覧
						<div id="leftListHeader"  class="leftListHeader">
							<div class="listHead">
								<span class="listTableName">テーブル名</span><span
								class="listItemName">項目名</span><span
								class="listJapaneseName">和名</span><span
								class="listS_point">開始位置</span><span
								class="listSize">サイズ</span>
							</div>
						</div>
						<div>
							<div id="leftList"  class="leftList">
<?php
require_once('./view/vw_Part_DataView_ItemNames.php');
?>
							</div>
						</div>
						<div>
							並び順<br/>
							<select id="sort" class="sort">
								<option value="asc">昇順</option>
								<option value="desc">降順</option>
							</select><br/>
						</div>
					</div>
					<div id="ccontents" class="ccontents">
						<div>
							<br/><br/>
							<img src="./img/btntbn04-1.png" id="sortAddButton" alt="右リストへ追加" class="verticallist">
							<img src="./img/btntbn04-2.png" id="sortDelButton" alt="右リスト選択行を削除" class="verticallist">
						</div>
					</div>
					<div id="rcontents" class="rcontents Wide30">
						並べ替え設定
						<div id="sortListHeader"  class="sortListHeader">
							<div class="listHead">
								<span class="listTableName">テーブル名</span><span
								class="listItemName">項目名</span><span
								class="listSort">順</span><span
							</div>
						</div>
						<div>
							<div id="sortList"  class="sortList">


<?php
$oddFlg = -1;
if(!empty($selectedSorts)){
	foreach($selectedSorts as $selectedSort){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listTableName bgChItems"><?php echo htmlEscape($selectedSort['listTableName']); ?></span><span
									class="listItemName bgChItems" title="型 : <?php echo htmlEscape($selectedSort['listType']); ?>"><?php echo htmlEscape($selectedSort['listItemName']); ?></span><span
									class="hidden listJapansesName bgChItems"><?php echo htmlEscape($selectedSort['listJapansesName']); ?></span><span
									class="hidden listId bgChItems"><?php echo htmlEscape($selectedSort['listId']); ?></span><span
									class="listSort bgChItems" title="型 : <?php echo htmlEscape($selectedSort['listType']); ?>"><?php echo htmlEscape($selectedSort['listSort']); ?></span><span
									class="hidden listS_point right bgChItems" title="型 : <?php echo htmlEscape($selectedSort['listType']); ?>"><?php echo htmlEscape($selectedSort['listS_point']); ?></span><span
									class="hidden listSize right bgChItems" title="型 : <?php echo htmlEscape($selectedSort['listType']); ?>"><?php echo htmlEscape($selectedSort['listSize']); ?></span><span
									class="hidden listType bgChItems"><?php echo htmlEscape($selectedSort['listType']); ?></span><span
									class="hidden listNext bgChItems"><?php echo htmlEscape($selectedSort['listNext']); ?></span><span
									class="hidden sortChildLast bgChItems id"><?php echo htmlEscape($selectedSort['id']); ?></span>
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
					<p class="backButton DataView_free_search_4 center">戻る</p>
					<p class="nextButton center">検索開始</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
