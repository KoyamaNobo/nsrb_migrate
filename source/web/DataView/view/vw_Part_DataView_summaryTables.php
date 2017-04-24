					<div class=summaryTitle>テーブル選択</div>
					<div class="descriptionTables">
<?php 
$count = 0;
$str   = "";
if(!empty($selectedTables)){
	foreach($selectedTables as $selectedTable){
		if($count != 0){
			echo ", ";
		}
		echo htmlEscape($selectedTable);
		echo '<input type="hidden" name="selectedTables[]" value="'.$selectedTable.'">';
		$count++;
	}
}
?>
					</div>