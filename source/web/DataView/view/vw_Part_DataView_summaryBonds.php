					<div class=summaryTitle>
						<span class="summaryBonds">���������ݒ�</span>
						<input type="button" class="summaryDisp Bonds" value="�ڍו\��">
					</div>
					<div class="descriptionBonds">
<?php 
if(!empty($selectedBonds)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">�e�[�u����</th>
								<th class="sListItemName">���ږ�</th>
								<th class="sListJapaneseName">�a��</th>
								<th class="sListS_point">�J�n</th>
								<th class="sListSize">�T�C�Y</th>
								<th class="sListBond">�������@</th>
								<th class="sListTableName">�e�[�u����</th>
								<th class="sListItemName">���ږ�</th>
								<th class="sListJapaneseName">�a��</th>
								<th class="sListS_point">�J�n</th>
								<th class="sListSize">�T�C�Y</th>
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