					<div class=summaryTitle>
						<span class="summaryFilters">���o�����ݒ�</span>
						<input type="button" class="summaryDisp Filters" value="�ڍו\��">
					</div>
					<div class="descriptionFilters">
<?php 
$count = 0;
if(!empty($selectedFilters)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListAndOr">�֘A</th>
								<th class="sListTableName">�e�[�u����</th>
								<th class="sListItemName">���ږ�</th>
								<th class="sListJapaneseName">�a��</th>
								<th class="sListS_point">�J�n</th>
								<th class="sListSize">�T�C�Y</th>
								<th class="sListType">�^</th>
								<th class="sListOperator">��r���Z�q</th>
								<th class="sListValue">�l</th>
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
						�ݒ�Ȃ�
<?php 
}
?>
					</div>
