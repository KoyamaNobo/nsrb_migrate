<?php
$oddFlg = -1;
if(!empty($clsData->tableData)){
	foreach($clsData->tableData as $item){
		?><div class="listElem <?php if($oddFlg == 1){ echo ' odd'; }
		?>"><span class="hidden bgChItems"></span><span
		class="listExtension hidden bgChItems"><?php echo $item->getExtension_id(); ?></span><span
		class="listTableName bgChItems"><?php echo $item->getTablename(); ?></span><span
		class="listJapaneseName bgChItems"><?php echo  $item->getItem_japanese_name(); ?></span><span
		class="listS_point right bgChItems"><?php echo $item->getS_point(); ?></span><span
		class="listSize right bgChItems"><?php echo $item->getSize(); ?></span><span
		class="listType bgChItems"><?php echo $item->getData_type(); ?></span><span
		class="listDispPri bgChItems"><?php echo $item->getDisp_flg(); ?></span><span
		class="User_Id bgChItems"><?php echo $item->getUser_id(); ?></span>
</div><?php
	$oddFlg *= -1;
	}
}?>
