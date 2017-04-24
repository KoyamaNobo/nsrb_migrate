					<div class=summaryTitle>
						<span class="summaryFilters">抽出条件設定</span>
						<input type="button" class="summaryDisp Filters" value="詳細表示">
					</div>
					<div class="descriptionFilters">
<?php 
$count = 0;
if(!empty($selectedFilters)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListAndOr">関連</th>
								<th class="sListTableName">テーブル名</th>
								<th class="sListItemName">項目名</th>
								<th class="sListJapaneseName">和名</th>
								<th class="sListS_point">開始</th>
								<th class="sListSize">サイズ</th>
								<th class="sListType">型</th>
								<th class="sListOperator">比較演算子</th>
								<th class="sListValue">値</th>
							</tr>
<?php 
	$filterCount=0;
	foreach($selectedFilters as $selectedFilter){
?>
							<tr>
								<?php outputSelectedFilter($selectedFilter,$filterCount) ?>
								<input type="hidden" name="selectedFilters[]" value="<?php echo $selectedFilter ?>">
							</tr>
<?php 
		$filterCount++;
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
