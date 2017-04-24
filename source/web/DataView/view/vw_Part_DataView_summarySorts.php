					<div class=summaryTitle>
						<span class="summarySorts">並べ替え設定</span>
						<input type="button" class="summaryDisp Sorts" value="詳細表示">
					</div>
					<div class="descriptionSorts">
<?php 
$count = 0;
if(!empty($selectedSorts)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">テーブル名</th>
								<th class="sListItemName">項目名</th>
								<th class="sListJapaneseName">和名</th>
								<th class="sListSort">順</th>
								<th class="sListS_point">開始</th>
								<th class="sListSize">サイズ</th>
								<th class="sListType">型</th>
							</tr>
<?php 
	foreach($selectedSorts as $selectedSort){

?>
							<tr>
								<?php outputSelectedSort($selectedSort) ?>
								<input type="hidden" name="selectedSorts[]" value="<?php echo $selectedSort ?>">
							</tr>
<?php 
	}
?>
						</table>
<?php 
}else{
?>
						設定なし
<?php 
}
?>
					</div>
