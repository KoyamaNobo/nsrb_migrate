					<div class=summaryTitle>
						<span class="summarySorts">���בւ��ݒ�</span>
						<input type="button" class="summaryDisp Sorts" value="�ڍו\��">
					</div>
					<div class="descriptionSorts">
<?php 
$count = 0;
if(!empty($selectedSorts)){
?>
						<table class="tableSummary">
							<tr>
								<th class="sListTableName">�e�[�u����</th>
								<th class="sListItemName">���ږ�</th>
								<th class="sListJapaneseName">�a��</th>
								<th class="sListSort">��</th>
								<th class="sListS_point">�J�n</th>
								<th class="sListSize">�T�C�Y</th>
								<th class="sListType">�^</th>
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
						�ݒ�Ȃ�
<?php 
}
?>
					</div>
