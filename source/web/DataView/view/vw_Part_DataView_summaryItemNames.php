					<div class=summaryTitle>
						<span class="summaryItemNames">�\�����ڑI��</span>
						<input type="button" class="summaryDisp ItemNames" value="�ڍו\��">
					</div>
					<div class="descriptionItemNames">
<?php 

$count = 0;
if(!empty($selectedItemNames)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">�e�[�u����</th>
								<th class="sListItemName">���ږ�</th>
								<th class="sListJapaneseName">�a��</th>
								<th class="sListS_point">�J�n</th>
								<th class="sListSize">�T�C�Y</th>
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
