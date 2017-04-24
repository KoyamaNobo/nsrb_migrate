					<div class=summaryTitle>
						<span class="summaryItemNames">表示項目選択</span>
						<input type="button" class="summaryDisp ItemNames" value="詳細表示">
					</div>
					<div class="descriptionItemNames">
<?php 

$count = 0;
if(!empty($selectedItemNames)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">テーブル名</th>
								<th class="sListItemName">項目名</th>
								<th class="sListJapaneseName">和名</th>
								<th class="sListS_point">開始</th>
								<th class="sListSize">サイズ</th>
							</tr>
<?php 
	foreach($selectedItemNames as $selectedItemName){
?>
							<tr>
								<?php outputSelectedItemName($selectedItemName) ?>
								<input type="hidden" name="selectedItemNames[]" value="<?php echo $selectedItemName ?>">
							</tr>
<?php 
	}
?>
						</table>
<?php 
}
?>
					</div>
