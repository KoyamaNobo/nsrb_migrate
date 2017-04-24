<?php 
$oddFlg = -1;
if(!empty($items)){
	foreach($items as $item){?><div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
	<span class="hidden bgChItems"></span><span 
	class="listTableName bgChItems"><?php echo htmlEscape($item['TableName']); ?></span><span 
	class="listItemName bgChItems"><?php echo htmlEscape($item['Label']); ?></span><span 
	class="listJapaneseName bgChItems"><?php echo  htmlEscape($item['Japanese_Name']); ?></span><span 
	class="listDisp bgChItems"><?php echo htmlEscape($item['NonDisp_Flg']) ?></span><span 
	class="listS_point right bgChItems"><?php echo htmlEscape($item['S_Point']); ?></span><span 
	class="listSize right bgChItems"><?php echo htmlEscape($item['Size']); ?></span><span 
	class="listType bgChItems"><?php echo htmlEscape($item['DataType']); ?></span>
</div><?php
	$oddFlg *= -1;
	}
}?>