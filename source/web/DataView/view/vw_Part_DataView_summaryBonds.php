					<div class=summaryTitle>
						<span class="summaryBonds">結合条件設定</span>
						<input type="button" class="summaryDisp Bonds" value="詳細表示">
					</div>
					<div class="descriptionBonds">
<?php 
if(!empty($selectedBonds)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">テーブル名</th>
								<th class="sListItemName">項目名</th>
								<th class="sListJapaneseName">和名</th>
								<th class="sListS_point">開始</th>
								<th class="sListSize">サイズ</th>
								<th class="sListBond">結合方法</th>
								<th class="sListTableName">テーブル名</th>
								<th class="sListItemName">項目名</th>
								<th class="sListJapaneseName">和名</th>
								<th class="sListS_point">開始</th>
								<th class="sListSize">サイズ</th>
							</tr>
<?php 
	foreach($selectedBonds as $selectedBond){
?>
							<tr>
								<?php outputSelectedBond($selectedBond) ?>
								<?php echo '<input type="hidden" name="selectedBonds[]" value="'.$selectedBond.'">' ?>
							</tr>
<?php 
	}
?>
						</table>
<?php 
}
?>
					</div>