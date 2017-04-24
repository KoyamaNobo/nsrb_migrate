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
<script type="text/javascript" src="./js/DataView_free_search_5.js?var=20160613"></script>
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
						結合する項目を選択してください。<br/>
						テーブル選択＞結合条件設定＞表示項目選択＞<span class="red">抽出条件設定</span>＞並べ替え設定＞結果表示
					</div>
				</div>
				<div class="summary">

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
					<p class="filterButton center">追加条件確定</p>
				</div>
				<div class="contents">
					<div id="lcontents" class="lcontents Wide55">
<?php
$count = 0;
if(!empty($selectedFilters)){
?>
						他の抽出条件との関連<br/>
						<input type="radio" id="andOr1" name="andOr" value="AND" checked><label for="andOr1">AND</label>
						<input type="radio" id="andOr2" name="andOr" value="OR"><label for="andOr2">OR</label><br/>
<?php
}else{
?>
						<input type="hidden" id="andOr1" name="andOr" value="AND">
						<input type="hidden" id="andOr2" name="andOr" value="OR">
<?php
}
?>
						項目名
						<div id="leftListHeader"  class="leftListHeader">
							<div class="listHead">
								<span class="listTableName">テーブル名</span><span
								class="listItemName">項目名</span><span
								class="listJapaneseName">和名</span><span
								class="listS_point">開始位置</span><span
								class="listSize center">サイズ</span><span
								class="listType center">型</span>
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
							比較演算子<br/>
							<select id="operator" class="operator">
								<option value=""></option>
								<option value="=">＝</option>
								<option value="<=">＜＝</option>
								<option value="<">＜</option>
								<option value=">=">＞＝</option>
								<option value=">">＞</option>
							</select><br/>
							値<br/>
							<input type="text" id="value" name="value" value="">
						</div>
					</div>
					<div id="ccontents" class="ccontents"></div>
					<div id="rcontents" class="rcontents"></div>
				</div>
				<div class="transition">
					<p class="backButton DataView_free_search_4 center">戻る</p>
					<p class="filterButton center">追加条件確定</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>
