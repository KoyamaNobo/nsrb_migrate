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
	echo '<span class="bold">条件タグ検索</span>';
}else{
	echo '<span class="bold">自由検索</span>';
}
?>
					<div class="description">
<?php
if(!empty($selectedCommandId)){
	echo '条件タグ選択＞<span class="red">結果表示</span>';
}else{
	echo 'テーブル選択＞結合条件設定＞表示項目選択＞抽出条件設定＞並べ替え設定＞<span class="red">結果表示</span>';
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
	echo '<div class=summaryTitle>選択条件タグ</div>';
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
					<p class="backButton <?php echo basename($prePgName,".php") ?> center">戻る</p>
<?php
if(!empty($getDatas)){
?>
					<p class="csvButton center">CSV作成</p>
<?php
}
?>
<?php
if(empty($selectedCommandId)){
?>
					<p class="modalOpenButton center">条件タグ保存</p><span id="massage2"></span>
<?php
}
?>
				</div>
				<div class="contents">
<?php
if(!empty($getDatas)){
?>
	<p>現在のページは「<?php echo $genzai_page;?>」です。</p>
	<p><?php echo ($start+1);?>件目から<?php echo (($start)+$hyouji_kazu);?>件目のデータを表示しています。</p>
	<div class="pagenation">
		<input type="hidden" id="Output_file" name="Output_file" value="'.$outputfile.'">
		<span class="link page=1 pageing ">[最初へ]</span>
<?php
	//前のページ表示
	for ($i = 0; $i < PAGINUM; $i++) {
		$tmp = $genzai_page - (PAGINUM - $i);
		if($tmp > 0){
			echo '<span class="link page='.$tmp.' pageing ">['.$tmp.']</span> ';
		}else{
			echo '　';
		}
	}

	echo '<span class="link page='.$genzai_page.' pageing2 ">['.$genzai_page.']</span> ';

	//次のページ表示
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
	echo '該当データなし。';
	echo '</div>';
}

?>
				</div>
				<div class="transition">
					<p class="backButton <?php echo basename($prePgName,".php") ?> center">戻る</p>
<?php
if(!empty($getDatas)){
?>
					<p class="csvButton center">CSV作成</p>
<?php
}
?>
<?php
if(empty($selectedCommandId)){
?>
					<p class="modalOpenButton center">条件タグ保存</p><span id="massage2"></span>
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
				保存する名前を入力してください。
				<input type="text" id="Command_Name" name="Command_Name" value=""><br/>
			</div>
			<div>
				<p class="modalCloseButton center">閉じる</p>
				<p class="signButton center">登録</p>
			</div>
			<div id="errMessageModal" class="errMessageModal"></div>
		</div>
	</div>
	<div id="DLmodal">
		<div id="DLmodalBody" class="Height150">
			<div id="DLmodalHeader">&nbsp;</div>
			<div>
				<div id="DLmodalInfo">CSVファイル書き込み中です。<br/>書き込み完了後にダウンロードが開始します。</div>
				<div id="DLmodalWritingCount"></div>件書き込み中
			</div>
		</div>
	</div>
</body>
</html>
