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
<script type="text/javascript" src="./js/DataView_free_search_4.js?var=20160613"></script>
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
						抽出する条件を設定してください。<br/>
						テーブル選択＞結合条件設定＞表示項目選択＞<span class="red">抽出条件設定</span>＞並べ替え設定＞結果表示
					</div>
				</div>
				<div class="summary">

<?php
require_once('./view/vw_Part_DataView_summaryTables.php');
require_once('./view/vw_Part_DataView_summaryBonds.php');
require_once('./view/vw_Part_DataView_summaryItemNames.php');
?>

				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="backButton DataView_free_search_3 center">戻る</p>
					<p class="filterButton center">抽出条件追加</p>
					<p class="nextButton center">次へ</p>
				</div>
				<div class="contents">
					<div id="rcontents" class="rcontents Wide62">
						抽出条件一覧
						<div id="filterListHeader"  class="filterListHeader">
							<div class="listHead">
								<span class="listAndOr">AND/OR</span><span
								class="listTableName">テーブル名</span><span
								class="listItemName">項目名</span><span
								class="listJapaneseName">和名</span><span
								class="listOperator">比較演算子</span><span
								class="listValue">値</span>
							</div>
						</div>
						<div>
							<div id="filterList"  class="filterList">
<?php
$oddFlg = -1;
if(!empty($selectedFilters)){
	$filterCount = 0;
	foreach($selectedFilters as $selectedFilter){
	$tmpJN="";
	if($selectedFilter['JapaneseName'] == " "){
		$tmpJN = "";
	}else{
		$tmpJN = $selectedFilter['JapaneseName'];
	}
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listAndOr bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php if($filterCount != 0){ echo htmlEscape($selectedFilter['andOr']);}else{echo "&nbsp;";} ?></span><span
									class="listTableName bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['tableName']); ?></span><span
									class="listItemName bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['itemName']); ?></span><span
									class="listJapaneseName bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['JapaneseName']); ?></span><span
									class="hidden listS_point bgChItems"><?php echo htmlEscape($selectedFilter['s_point']); ?></span><span
									class="hidden listSize bgChItems"><?php echo htmlEscape($selectedFilter['size']); ?></span><span
									class="hidden listType bgChItems"><?php echo htmlEscape($selectedFilter['type']); ?></span><span
									class="listOperator bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['operator']); ?></span><span
									class="listValue bgChItems" title="開始位置 : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;サイズ ： <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;型 ： <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['value']); ?></span><span
									class="hidden bgChItems id"><?php echo htmlEscape($selectedFilter['andOr']."~".$selectedFilter['tableName']."~".$selectedFilter['itemName']."~".$tmpJN."~".$selectedFilter['s_point']."~".$selectedFilter['size']."~".$selectedFilter['type']."~".$selectedFilter['operator']."~".$selectedFilter['value']."~".$selectedFilter['id']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	$filterCount++;
	}
}
?>
							</div>
						</div>
						<img src="./img/btntbn06-2.png" id="filterDelButton" alt="上リスト選択行を削除" width="" height="">
					</div>
				</div>
				<div class="transition">
					<p class="backButton DataView_free_search_3 center">戻る</p>
					<p class="filterButton center">抽出条件追加</p>
					<p class="nextButton center">次へ</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
